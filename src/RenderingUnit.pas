unit RenderingUnit;

(*        AErender Launcher                                                                 *)
(*        RenderingUnit.pas                                                                 *)
(*        Lily Stilson // 2019 - 2020                                                       *)
(*        MIT License                                                                       *)
(*                                                                                          *)
(*        Copyright (c) 2019 - 2020 Alice Romanets                                          *)
(*                                                                                          *)
(*        Permission is hereby granted, free of charge, to any person obtaining a copy      *)
(*        of this software and associated documentation files (the "Software"), to deal     *)
(*        in the Software without restriction, including without limitation the rights      *)
(*        to use, copy, modify, merge, publish, distribute, sublicense, and/or sell         *)
(*        copies of the Software, and to permit persons to whom the Software is             *)
(*        furnished to do so, subject to the following conditions:                          *)
(*                                                                                          *)
(*        The above copyright notice and this permission notice shall be included in all    *)
(*        copies or substantial portions of the Software.                                   *)
(*                                                                                          *)
(*        THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR        *)
(*        IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,          *)
(*        FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE       *)
(*        AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER            *)
(*        LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,     *)
(*        OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE     *)
(*        SOFTWARE.                                                                         *)

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Diagnostics,
  System.Generics.Collections,

  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Effects,
  FMX.BufferedLayout,
  FMX.BehaviorManager,

  AErenderLauncher.AerenderParser,

  {$IFDEF MSWINDOWS}
    System.Notification, FMX.TaskBar, FMX.Platform.Win, Winapi.Windows, Winapi.TlHelp32;
  {$ENDIF MSWINDOWS}

  {$IFDEF MACOS}
  Posix.Unistd, FMX.Platform.Mac, MacApi.Foundation, MacApi.AppKit, MacApi.ObjectiveC, MacApi.CocoaTypes;
  {$ENDIF MACOS}

type
  TRenderingForm = class(TForm)
    WindowTitle: TLabel;
    RenderingStatusBar: TStatusBar;
    totalProgressBottomLabel: TLabel;
    statusBarSeparator1: TLine;
    abortRenderingButton: TButton;
    statusBarTopLayout: TLayout;
    RenderingToolBar: TToolBar;
    statusBarSeparator2: TLine;
    TotalProgressBar: TProgressBar;
    totalProgressLabel: TLabel;
    framesLabel: TLabel;
    topLayoutTitle: TLayout;
    totalProgressPercentage: TLabel;
    renderingTimer: TTimer;
    emptyLabel: TLabel;
    VertScrollBox1: TVertScrollBox;
    AErenderLayout: TLayout;
    BlurEffect1: TBlurEffect;
    FFMPEGConcatLayout: TLayout;
    Rectangle1: TRectangle;
    Label1: TLabel;
    ProgressBar1: TProgressBar;
    Label2: TLabel;
    Button1: TButton;
    timeElapsedLabel: TLabel;
    StopwatchTimer: TTimer;
    projectNameLabel: TLabel;
    totalFramesLabel: TLabel;
    procedure ShowLogButtonClick (Sender: TObject);
    procedure renderingTimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure abortRenderingButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure StopwatchTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SetLanguage(LanguageCode: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    {$IFDEF MSWINDOWS}procedure CreateHandle; override;{$ENDIF MSWINDOWS}
  end;
  TRenderGroup = record
    // Visible components
    RenderPanel: TPanel;
    CompNameLabel: TLabel;
    LogButton: TButton;   // It will be styled
    FramesLabel: TLabel;
    StatusLabel: TLabel;
    RenderingProgress: TProgressBar;
    WaitProgress: TAniIndicator;
    LogMemo: TMemo;

    // Invisible components
    Duration: TTimecode;
    FrameRate: TFrameRate;
  end;

var
  RenderingForm: TRenderingForm;
  VISIBLE: Boolean = False;

  ///  We are using TArray<> here, because TList<> generates Access Violation error
  ///  when populated with non-existent items. We need a constructor for that, but duh...
  ///  Also, we can't use TObjectList<>, since TRenderGroup is a record,
  ///  not a class - also no constructor, duh...
  ///  So, it's easier to use simple TArray<>, that we'll have to reset when render finishes
  RenderGroups: TArray<TRenderGroup>;
  CurrentTime: TDateTime;

  { TODO -oLily Stilson -cRendering Unit - macOS : Add NSApplication.requestUserAttention for macOS when Rendering is finished }

implementation

{$R *.fmx}

uses
  {$REGION '    AErenderLauncher Liraries    '}
  MainUnit,
  SettingsUnit;
  {$ENDREGION}

{$IFDEF MSWINDOWS}
procedure TRenderingForm.CreateHandle;
begin
  inherited CreateHandle;
  var hWnd: HWND := FormToHWND(Self);

  SetWindowLong(hWnd, GWL_EXSTYLE, GetWindowLong(hWnd, GWL_EXSTYLE) or WS_EX_APPWINDOW);
  SetClassLong(hWnd, GCL_STYLE, GetClassLong(hWnd, GCL_STYLE) or CS_DROPSHADOW);
end;
{$ENDIF MSWINDOWS}

function IntToCardinal (I: Integer): Cardinal;
begin
  if I < 0 then
    Result := 0
  else
    Result := I;
end;

procedure TRenderingForm.SetLanguage(LanguageCode: Integer);
begin
  RenderingForm.Caption     := Language[LanguageCode].RenderingForm.RenderingProgress;
  WindowTitle.Text          := Language[LanguageCode].RenderingForm.RenderingProgress;

  //emptyLabel.Text         := Language[LanguageCode].RenderingForm.QueueIsEmpty;
  totalProgressLabel.Text   := Language[LanguageCode].RenderingForm.TotalProgress;
  abortRenderingButton.Text := Language[LanguageCode].RenderingForm.AbortRendering;

  timeElapsedLabel.Text     := Language[LanguageCode].RenderingForm.TimeElapsed + '00:00:00';
end;

procedure TRenderingForm.abortRenderingButtonClick(Sender: TObject);
begin
  try
    begin
      // RU: Уничтожаем процесс After Effects'а
      {$IFDEF MSWINDOWS}KillProcess('AfterFX.com');{$ENDIF MSWINDOWS}
      {$IFDEF MACOS}KillProcess('aerendercore');{$ENDIF MACOS}

      // RU: Сброс состояния на панели задач
      {$IFDEF MSWINDOWS}MainTaskBar.TaskBarState := 0;{$ENDIF MSWINDOWS}

      // RU: Очищаем массив с путями к логам
      Finalize(LogFiles);

      // RU: Уничтожаем созданные в рантайме компоненты
      for var i := 0 to High(RenderGroups) do begin
        FreeAndNil(RenderGroups[i].RenderPanel);
      end;

      // RU: Сбрасываем массив с группами прогресса
      Finalize(RenderGroups);

      // RU: Отключаем ограничение на запуск рендеринга
      isRendering := False;

      // RU: Отключаем таймер и вовзращаем первые значения
      emptyLabel.Visible := True;
      emptyLabel.Enabled := True;
      renderingTimer.Enabled := False;
      StopwatchTimer.Enabled := False;
      projectNameLabel.Text := '';
      framesLabel.Text := '';
      totalProgressPercentage.Text := '0%';
      TotalProgressBar.Value := 0;
      TotalProgressBar.Max := 1;
    end
  except
    on Exception do
      ShowMessage ('Nothing to abort!')
  end;
end;

procedure TRenderingForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  VISIBLE := False;

  // RU:  При закрытии формы, если рендеринг закончен
  //      очистить окно
  if TotalProgressBar.Value = TotalProgressBar.Max then
    begin
      abortRenderingButtonClick(Sender);
      TotalProgressBar.Value := 0;
      {$IFDEF MSWINDOWS}MainTaskBar.TaskBarState := 0;{$ENDIF MSWINDOWS}
    end;
end;

procedure TRenderingForm.FormCreate(Sender: TObject);
begin
  // RU:  Сброс главного прогрессбара до нуля
  TotalProgressBar.Min := 0;
  TotalProgressBar.Max := 1;
  TotalProgressBar.Value := 0;

  // RU:  Активация плавного скроллинга списка потоков
  VertScrollBox1.AniCalculations.Animation := True;
end;

procedure TRenderingForm.FormShow(Sender: TObject);
begin
  VISIBLE := True;
  if MainUnit.RenderWindowSender = MainForm.launchButton then
    begin
      isRendering := True;
      if Length(LogFiles) = 0 then
        begin
          emptyLabel.Visible := True;
          emptyLabel.Enabled := True;
        end
      else
        begin
          emptyLabel.Visible := False;
          emptyLabel.Enabled := False;

          projectNameLabel.Text := ExtractFileName(MainForm.projectPath.Text);

          // RU:  Этот момент необходимо держать в голове
          //      Поскольку мы используем тут динамический массив TArray,
          //      то когда мы задаём ему какой-то размер, он будет сохранять
          //      все элементы внутри, даже если каждый из них будет FreeAndNil()
          SetLength(RenderGroups, Length(MainUnit.LogFiles));

          /// RU: Создание компонентов, демонстрирующих прогресс
          for var i := 0 to High(RenderGroups) do begin
              //  RU: Создание фоновой TPanel
              RenderGroups[i].RenderPanel := TPanel.Create(Self);
              RenderGroups[i].RenderPanel.Parent  := VertScrollBox1;
              RenderGroups[i].RenderPanel.Margins := TBounds.Create(TRectF.Create(8, 8, 8, 0));
              RenderGroups[i].RenderPanel.Align   := TAlignLayout.Top;
              RenderGroups[i].RenderPanel.Height  := 48;
              RenderGroups[i].RenderPanel.EnableDragHighlight := True;

              //  RU: Создание названия композиции
              RenderGroups[i].CompNameLabel := TLabel.Create(Self);
              RenderGroups[i].CompNameLabel.Parent          := RenderGroups[i].RenderPanel;
              RenderGroups[i].CompNameLabel.Align           := TAlignLayout.MostLeft;
              RenderGroups[i].CompNameLabel.Margins         := TBounds.Create(TRectF.Create(16, 8, 0, 8));
              RenderGroups[i].CompNameLabel.Text            := ExtractFileName(MainUnit.LogFiles[i]).Replace(ExtractFileExt(MainUnit.LogFiles[i]), '');
              RenderGroups[i].CompNameLabel.Font.Size       := 16;
              RenderGroups[i].CompNameLabel.StyledSettings  := [TStyledSetting.Family, TStyledSetting.Style, TStyledSetting.FontColor];
              RenderGroups[i].CompNameLabel.WordWrap        := False;
              RenderGroups[i].CompNameLabel.AutoSize        := True;
              RenderGroups[i].CompNameLabel.HitTest         := False;

              //  RU: Создание лейбла с кол-вом кадров
              RenderGroups[i].FramesLabel := TLabel.Create(Self);
              RenderGroups[i].FramesLabel.Parent      := RenderGroups[i].RenderPanel;
              RenderGroups[i].FramesLabel.Margins     := TBounds.Create(TRectF.Create(8, 0, 16, 0));
              RenderGroups[i].FramesLabel.AutoSize    := True;
              RenderGroups[i].FramesLabel.WordWrap    := False;
              RenderGroups[i].FramesLabel.Align       := TAlignLayout.MostRight;
              RenderGroups[i].FramesLabel.StyledSettings  := [TStyledSetting.Family, TStyledSetting.FontColor];
              RenderGroups[i].FramesLabel.Font.Size   := 16;
              //RenderGroups[i].FramesLabel.Font.Style  := [TFontStyle.fsBold];
              RenderGroups[i].FramesLabel.TextAlign   := TTextAlign.Trailing;
              RenderGroups[i].FramesLabel.Text        := '...';
              RenderGroups[i].FramesLabel.HitTest     := False;

              // RU:  Создание кнопки отображения логов
              RenderGroups[i].LogButton := TButton.Create(Self);
              RenderGroups[i].LogButton.Parent      := RenderGroups[i].RenderPanel;
              RenderGroups[i].LogButton.Align       := TAlignLayout.MostRight;
              RenderGroups[i].LogButton.StyleLookup := 'logbuttonstyle';
              RenderGroups[i].LogButton.Width       := 48;
              RenderGroups[i].LogButton.Position.X  := 9999;
              RenderGroups[i].LogButton.Tag         := i;
              RenderGroups[i].LogButton.OnClick     := ShowLogButtonClick;

              // RU:  Создание надписи над прогрессбаром
              RenderGroups[i].StatusLabel := TLabel.Create(Self);
              RenderGroups[i].StatusLabel.Parent          := RenderGroups[i].RenderPanel;
              RenderGroups[i].StatusLabel.Align           := TAlignLayout.Top;
              RenderGroups[i].StatusLabel.Margins         := TBounds.Create(TRectF.Create(16, 8, 0, 0));
              RenderGroups[i].StatusLabel.StyledSettings  := [TStyledSetting.Family, TStyledSetting.FontColor];
              RenderGroups[i].StatusLabel.Font.Style      := [TFontStyle.fsBold];
              RenderGroups[i].StatusLabel.Text            := Language[LANG].RenderingForm.WaitingForAerender;
              RenderGroups[i].StatusLabel.HitTest         := False;

              // RU:  Создание прогрессбара
              RenderGroups[i].RenderingProgress := TProgressBar.Create(Self);
              RenderGroups[i].RenderingProgress.Align       := TAlignLayout.Client;
              RenderGroups[i].RenderingProgress.Parent      := RenderGroups[i].RenderPanel;
              RenderGroups[i].RenderingProgress.Margins     := TBounds.Create(TRectF.Create(16, 4, 0, 16));
              RenderGroups[i].RenderingProgress.Height      := 4;
              RenderGroups[i].RenderingProgress.Value       := 0;
              RenderGroups[i].RenderingProgress.Max         := 1;
              RenderGroups[i].RenderingProgress.StyleLookup := 'progressbarministyle';
              RenderGroups[i].RenderingProgress.HitTest     := False;

              // RU:  Создание "заглушки"
              RenderGroups[i].WaitProgress := TAniIndicator.Create(Self);
              RenderGroups[i].WaitProgress.Align        := TAlignLayout.Client;
              RenderGroups[i].WaitProgress.Parent       := RenderGroups[i].RenderPanel;
              RenderGroups[i].WaitProgress.Margins      := TBounds.Create(TRectF.Create(16, 4, 0, 16));
              RenderGroups[i].WaitProgress.Height       := 4;
              RenderGroups[i].WaitProgress.StyleLookup  := 'aniindicatorhorzstyle';
              RenderGroups[i].WaitProgress.Enabled      := True;
              RenderGroups[i].WaitProgress.HitTest      := False;

              // RU:  Создание Memo для отображения логов
              RenderGroups[i].LogMemo := TMemo.Create(Self);
              RenderGroups[i].LogMemo.Parent          := RenderGroups[i].RenderPanel;
              RenderGroups[i].LogMemo.Align           := TAlignLayout.MostBottom;
              RenderGroups[i].LogMemo.Height          := 192;
              RenderGroups[i].LogMemo.Visible         := False;
              RenderGroups[i].LogMemo.StyledSettings  := [TStyledSetting.Style, TStyledSetting.FontColor];
              RenderGroups[i].LogMemo.Font.Family     := 'Consolas';
              RenderGroups[i].LogMemo.Font.Size       := 12;
              RenderGroups[i].LogMemo.ScrollAnimation := TBehaviorBoolean.True;
              RenderGroups[i].LogMemo.WordWrap        := True;
            end;
        end;

      // RU: Запуск таймера рендеринга и секундомера
      renderingTimer.Enabled := True;
      CurrentTime := Now;
      StopwatchTimer.Enabled := True;
      {$IFDEF MSWINDOWS}MainTaskBar.TaskBarState := 2;{$ENDIF MSWINDOWS}
    end;
end;

procedure TRenderingForm.renderingTimerTimer(Sender: TObject);
// RU:  Нам нужно создать какой-то тип данных, где для
//      каждого потока мы будем читать, хранить и отображать
//      то, что творится в логах.
//      TStringList является тем, где TMemo хранит весь текст,
//      посему он и будет использоваться для хранилища логов внутри программы.
type
  TRenderData = record      // Непосредственно тип
    LogFile: TStringList;   // Сюда считываем лог
    //Data: TStringList;    // Не ебу, нахуй это нужно, но почему-то я его тут обозначил. Далее в коде оно нигде не использовалось.
    Stream: TStream;        // Поток, даннве из котрого будут передаваться в переменную LogFile
    State: Integer;         // Обозначим для состояния рендеринга три цифры: -1 (ошибка), 0 (завершён), 1 (рендерится)
  end;
var
  Render: TArray<TRenderData>;
begin
  var Finished: Integer := 0;
  var Error: Integer := 0;
  var Completed: Integer := 0;

  // RU: Создаём элементы в массиве с данными логов
  SetLength(Render, Length(LogFiles));

// RU: Пока оставлю это в комментарии. Когда пойму, зачем это - восстановлю
//  for var j := 0 to High(Render) do
//    begin
//      Render[j].Data := TStringList.Create;
//      Render[j].State := '';
//    end;

  for var i := 0 to High(Render) do begin
    // RU: Создаём хранилище для содержимого логов и устанавливаем ему кодировку
    Render[i].LogFile := TStringList.Create;
    Render[i].LogFile.DefaultEncoding := TEncoding.UTF8;

    try
      // RU:  Создаём поток, из которого потом перенесём данные в хранилище содержимого логов
      //      То есть, фактически, каждый раз файл лога считывается заново,
      //      держать в памяти всегда его не получится, да и смысла нет - он постоянно обновляется
      Render[i].Stream := TFileStream.Create(LogFiles[i], fmOpenRead or fmShareDenyNone);
      try
        Render[i].LogFile.LoadFromStream(Render[i].Stream);
      finally
        Render[i].Stream.Free;
      end;

      // RU:  Если количество строк в файле лога оказалось больше, чем в LogMemo у конкретного
      //      потока, то нужно добавить эту строку в LogMemo и произвести её парсинг
      if Render[i].LogFile.Count > RenderGroups[i].LogMemo.Lines.Count then begin
        // RU: Добавляем строку и иттератор LogMemo посылаем в самый низ
        RenderGroups[i].LogMemo.Lines.Add(Render[i].LogFile[RenderGroups[i].LogMemo.Lines.Count]);
        RenderGroups[i].LogMemo.GoToTextEnd;

        // RU:  При создании прогрессбара, мы обозначили его максимум как 1
        //      Это поможет нам определить, какой поток уже узнал своё максимальное
        //      количество кадров, а какой ещё нет
        //      Тут, мы пытаемся получить какую-то базовую информацию о том,
        //      что мы сейчас будем рендерить
        if RenderGroups[i].RenderingProgress.Max = 1 then begin
            //  RU: Если строка в конце лога содержит 'Duration: ',
            //      то мы пытаемся вытащить из неё длительность области рендеринга
            if RenderGroups[i].LogMemo.Lines[RenderGroups[i].LogMemo.Lines.Count - 1].Contains('Duration: ') then
              RenderGroups[i].Duration := ParseAErenderDurationLogString(RenderGroups[i].LogMemo.Lines[RenderGroups[i].LogMemo.Lines.Count - 1]);

            //  RU: Если строка в конце лога содержит 'Frame Rate: ',
            //      то мы пытаемся вытащить из неё частоту кадров композиции
            if RenderGroups[i].LogMemo.Lines[RenderGroups[i].LogMemo.Lines.Count - 1].Contains('Frame Rate: ') then
              RenderGroups[i].FrameRate := ParseAErenderFrameRateLogString (RenderGroups[i].LogMemo.Lines[RenderGroups[i].LogMemo.Lines.Count - 1]);

            //  RU: Если у нас есть длительность и частота кадров,
            //      То нам нужно вычислить конечное значение кадров для прогрессбара
            //      И ещё, нужно скрыть WaitProgress
            if (RenderGroups[i].Duration.ToSingleString <> '0:00:00:00') and (RenderGroups[i].FrameRate <> 0) then begin
              RenderGroups[i].RenderingProgress.Max := TimecodeToFrames(RenderGroups[i].Duration, RenderGroups[i].FrameRate);
              RenderGroups[i].WaitProgress.Visible := False;
              RenderGroups[i].WaitProgress.Enabled := False;
            end;
        end;
      end;

      // RU:  Подсчитываем общее количество кадров
      //      Тип Single, потому что у прогрессбаров значения этого типа
      var TotalFrames: Single := 1;
        for var j := 0 to High(RenderGroups) do
          if RenderGroups[j].RenderingProgress.Max <> 1 then
            TotalFrames := TotalFrames + RenderGroups[j].RenderingProgress.Max;

      // RU: Начинаем отображение прогресса, только если у нас известно конечное значение кадров
      if RenderGroups[i].RenderingProgress.Max <> 1 then begin
        // RU:  Пробуем парсить последнюю строку лога
        //      Пока в строке не появятся нужные данные, функция ParseAErenderFrameLogString
        //      будет выплёвывать исключения. Если мы ловим это исключение,
        //      то прогрессбару присваиваем ЕГО ЖЕ значение. Потому что, при окончании рендеринга, может быть
        //      выкинуто исключение и из-за этого общий прогрессбар обосрётся
        //      Таким образом, мы отсеиваем все строки, до момента как aerender начинает показывать прогресс
        try
          var AERP: TAErenderFrameData := ParseAErenderFrameLogString(RenderGroups[i].LogMemo.Lines[RenderGroups[i].LogMemo.Lines.Count - 1]);
          RenderGroups[i].RenderingProgress.Value := AERP.Frame;
          RenderGroups[i].StatusLabel.Text := Language[LANG].RenderingForm.Rendering;
          RenderGroups[i].FramesLabel.Text := Format('%s / %s', [RenderGroups[i].RenderingProgress.Value.ToString, RenderGroups[i].RenderingProgress.Max.ToString]);
        except
          on Exception do
            RenderGroups[i].RenderingProgress.Value := RenderGroups[i].RenderingProgress.Value;
        end;
      end;

      // RU: Проверяем, закончен рендеринг или нет
      if RenderGroups[i].LogMemo.Text.Contains('Finished composition') then begin
        Render[i].State := 0;
        RenderGroups[i].StatusLabel.Text := Language[LANG].RenderingForm.RenderingFinished;
        RenderGroups[i].RenderingProgress.Value := RenderGroups[i].RenderingProgress.Max;
      end;
      // RU: Проверяем, была ошибка или нет
      if RenderGroups[i].LogMemo.Text.Contains('aerender ERROR') or RenderGroups[i].LogMemo.Text.Contains('aerender Error') then begin
        Render[i].State := -1;
        RenderGroups[i].WaitProgress.Visible := False;
        RenderGroups[i].WaitProgress.Enabled := False;
        RenderGroups[i].StatusLabel.Text := Language[LANG].RenderingForm.RenderingError;
        RenderGroups[i].RenderingProgress.Value := RenderGroups[i].RenderingProgress.Max;
        RenderGroups[i].RenderingProgress.StyleLookup := 'progressbarminierrorstyle'
      end;

      // RU: Общему прогрессбару устанавливаем текущее значение
      if TotalProgressBar.Max <> TotalFrames then begin
        TotalProgressBar.Max := TotalFrames;
      end else begin
        // RU: Подсчитываем общее текущее значение и присваиваем его
        var sum: Single := 0;
        for var j := 0 to High(RenderGroups) do
          if Render[j].State <> -1 then
            sum := sum + RenderGroups[j].RenderingProgress.Value;

        TotalProgressBar.Value := sum;
        totalProgressPercentage.Text := Trunc((TotalProgressBar.Value / TotalProgressBar.Max) * 100).ToString + '%';
        {$IFDEF MSWINDOWS}MainTaskBar.TaskBarProgress := Trunc((TotalProgressBar.Value / TotalProgressBar.Max) * 100);{$ENDIF MSWINDOWS}
        if (TotalProgressBar.Max = 0) or (TotalProgressBar.Max = 1) then
          framesLabel.Text := Language[LANG].RenderingForm.WaitingForAerender
        else
          framesLabel.Text := TotalProgressBar.Value.ToString + ' / ' + (TotalProgressBar.Max - 1).ToString;
      end;
    finally
      FreeAndNil(Render[i].LogFile);  // RU: Уничтожаем контейнер с логом, чтобы не было проблем
    end;
  end;

  {for var i := 0 to High(Render) do
    if Render[i].State = 0 then
      inc (Finished);

  for var i := 0 to High(Render) do
    if Render[i].State = -1 then
      inc (Error);}

  // RU:  Подсчитываем, сколько потоков завершили рендеринг
  //      Если рендеринг завершился с ошибкой, всё равно учитываем
  //      иначе таймер никогда не остановится
  for var i := 0 to High(RenderGroups) do
    if RenderGroups[i].RenderingProgress.Value = RenderGroups[i].RenderingProgress.Max then
      inc (Completed);

  // RU:  Если количество потоков, завершивших рендеринг совпадает
  //      с общим количеством потоков - всё окей, можно останавливать таймер
  if (Completed = Length(LogFiles)) then
    begin
      // RU: Останавливаем секундомер
      StopwatchTimer.Enabled := False;

      for var i := 0 to High(RenderGroups) do
        if Render[i].State <> -1 then
          RenderGroups[i].StatusLabel.Text := Language[LANG].RenderingForm.RenderingFinished;

      // RU: Полностью очищаем массив с данными логов
      Finalize(Render);

      TotalProgressBar.Value := TotalProgressBar.Max;
      totalProgressPercentage.Text := '100%';

      {$IFDEF MSWINDOWS}
      FlashWindow(ApplicationHWND, True);
      //if Error = 0 then
        MainTaskBar.TaskBarState := 0;
      {if (Error > 0) and (Error <> Length(LogFiles)) then
        MainTaskBar.TaskBarState := 4;
      if Error = Length(LogFiles) then
        MainTaskBar.TaskBarState := 3;}
      {$ENDIF MSWINDOWS}

      {$IFDEF MACOS}
      var Application: NSApplication := TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication);
      Application.requestUserAttention(NSCriticalRequest);
      {$ENDIF MACOS}

      renderingTimer.Enabled := False;
    end;
end;

procedure TRenderingForm.ShowLogButtonClick (Sender: TObject);
begin
  if RenderGroups[TButton(Sender).Tag].LogMemo.Visible = False then begin
    RenderGroups[TButton(Sender).Tag].LogMemo.Visible := True;
    RenderGroups[TButton(Sender).Tag].RenderPanel.Height := RenderGroups[TButton(Sender).Tag].RenderPanel.Height + 192;
  end else begin
    RenderGroups[TButton(Sender).Tag].LogMemo.Visible := False;
    RenderGroups[TButton(Sender).Tag].RenderPanel.Height := RenderGroups[TButton(Sender).Tag].RenderPanel.Height - 192;
  end;
end;

procedure TRenderingForm.StopwatchTimerTimer(Sender: TObject);
begin
  // This causes on macOS
  // [dccosx64 Fatal Error] RenderingUnit.pas(587): F2084 Internal Error: URW1237
  // Workaround: Get string that is already in array and pass it through an inline variable
  var TimeElapsed: String := Language[LANG].RenderingForm.TimeElapsed;
  timeElapsedLabel.Text := TimeElapsed + FormatDateTime('hh:nn:ss', Now - CurrentTime);
end;

end.


