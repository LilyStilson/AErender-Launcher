unit Unit3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.ListBox, FMX.ScrollBox, FMX.Memo, FMX.WebBrowser;

type
  TForm3 = class(TForm)
    Layout2: TLayout;
    Button1: TButton;
    Contents: TListBox;
    Layout1: TLayout;
    Title: TLabel;
    WebBrowser1: TWebBrowser;
    procedure ContentsChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  Unit1;

{$R *.fmx}

procedure TForm3.Button1Click(Sender: TObject);
begin
  Form3.Close;
end;

procedure TForm3.ContentsChange(Sender: TObject);
begin
  case Contents.ItemIndex of
    0:begin
        {$IFDEF MSWINDOWS}
        if FileExists ('C:/ProgramData/AErender/Docs/Introduction.html') then
          WebBrowser1.URL := 'file://C:/ProgramData/AErender/Docs/Introduction.html'
        else
          WebBrowser1.URL := 'file://C:/ProgramData/AErender/Docs/Error.html';
        //WebBrowser1.Reload();
        {$ENDIF MSWINDOWS}
        {$IFDEF MACOS}
        if FileExists (GetEnvironmentVariable('HOME') + '/Documents/AErender/Docs/Introduction.html') then
          WebBrowser1.URL := 'file://' + GetEnvironmentVariable('HOME') + '/Documents/AErender/Docs/Introduction.html'
        else
          WebBrowser1.URL := 'file://' + GetEnvironmentVariable('HOME') + '/Documents/AErender/Docs/Error.html';
        //WebBrowser1.Reload();
        {$ENDIF MACOS}
      end;
    1:begin
        {$IFDEF MSWINDOWS}
        if FileExists ('C:/ProgramData/AErender/Docs/AfterEffectsRenderEngine.html') then
          WebBrowser1.URL := 'file://C:/ProgramData/AErender/Docs/AfterEffectsRenderEngine.html'
        else
          WebBrowser1.URL := 'file://C:/ProgramData/AErender/Docs/Error.html';
        {$ENDIF MSWINDOWS}
        {$IFDEF MACOS}
        if FileExists (GetEnvironmentVariable('HOME') + '/Documents/AErender/Docs/AfterEffectsRenderEngine.html') then
          WebBrowser1.URL := 'file://' + GetEnvironmentVariable('HOME') + '/Documents/AErender/Docs/AfterEffectsRenderEngine.html'
        else
          WebBrowser1.URL := 'file://' + GetEnvironmentVariable('HOME') + '/Documents/AErender/Docs/Error.html';
        {$ENDIF MACOS}
      end;
    2:begin
        {$IFDEF MSWINDOWS}
        if FileExists ('C:/ProgramData/AErender/Docs/PreparingRendering.html') then
          WebBrowser1.URL := 'file://C:/ProgramData/AErender/Docs/PreparingRendering.html'
        else
          WebBrowser1.URL := 'file://C:/ProgramData/AErender/Docs/Error.html';
        {$ENDIF MSWINDOWS}
        {$IFDEF MACOS}
        if FileExists (GetEnvironmentVariable('HOME') + '/Documents/AErender/Docs/PreparingRendering.html') then
          WebBrowser1.URL := 'file://' + GetEnvironmentVariable('HOME') + '/Documents/AErender/Docs/PreparingRendering.html'
        else
          WebBrowser1.URL := 'file://' + GetEnvironmentVariable('HOME') + '/Documents/AErender/Docs/Error.html';
        {$ENDIF MACOS}
      end;
    3:begin
        {$IFDEF MSWINDOWS}
        if FileExists ('C:/ProgramData/AErender/Docs/Issues.html') then
          WebBrowser1.URL := 'file://C:/ProgramData/AErender/Docs/Issues.html'
        else
          WebBrowser1.URL := 'file://C:/ProgramData/AErender/Docs/Error.html';
        {$ENDIF MSWINDOWS}
        {$IFDEF MACOS}
        if FileExists (GetEnvironmentVariable('HOME') + '/Documents/AErender/Docs/Issues.html') then
          WebBrowser1.URL := 'file://' + GetEnvironmentVariable('HOME') + '/Documents/AErender/Docs/Issues.html'
        else
          WebBrowser1.URL := 'file://' + GetEnvironmentVariable('HOME') + '/Documents/AErender/Docs/Error.html';
        {$ENDIF MACOS}
      end;
  end;
end;

procedure TForm3.FormShow(Sender: TObject);
begin
  ContentsChange (Sender);
end;

end.
