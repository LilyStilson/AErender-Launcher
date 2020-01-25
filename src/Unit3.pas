unit Unit3;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.Layouts,
  FMX.ListBox,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.WebBrowser,
  {$IFDEF MSWINDOWS}
    FMX.Platform.Win, Winapi.Windows, Winapi.TlHelp32;
  {$ENDIF MSWINDOWS}
  {$IFDEF MACOS}
    MacApi.Foundation;
  {$ENDIF MACOS}

type
  TForm3 = class(TForm)
    Layout2: TLayout;
    Button1: TButton;
    Contents: TListBox;
    Title: TLabel;
    WebBrowser1: TWebBrowser;
    Toolbar: TToolBar;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    ListBoxGroupHeader3: TListBoxGroupHeader;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    ListBoxItem7: TListBoxItem;
    ListBoxItem8: TListBoxItem;
    ListBoxItem9: TListBoxItem;
    ListBoxItem10: TListBoxItem;
    ListBoxGroupHeader4: TListBoxGroupHeader;
    ListBoxItem11: TListBoxItem;
    ListBoxItem12: TListBoxItem;
    Splitter1: TSplitter;
    procedure ContentsChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    {$IFDEF MSWINDOWS}procedure CreateHandle; override;{$ENDIF MSWINDOWS}
  public
    { Public declarations }
  end;

var
  Form3: TForm3;
  MacDocsPaths: TArray<String> = ['', '',
                                  '', '', '', '', '', '',
                                  '', '',
                                  '', ''];

implementation

uses
  Unit1;

{$R *.fmx}

{$IFDEF MSWINDOWS}
procedure TForm3.CreateHandle;
begin
  inherited CreateHandle;

  var hWnd: HWND := FormToHWND(Self);

  SetWindowLong(hWnd, GWL_EXSTYLE, GetWindowLong(hWnd, GWL_EXSTYLE) or WS_EX_APPWINDOW);
  SetClassLong(hWnd, GCL_STYLE, GetClassLong(hWnd, GCL_STYLE) or CS_DROPSHADOW);
end;
{$ENDIF MSWINDOWS}

procedure TForm3.Button1Click(Sender: TObject);
begin
  Form3.Close;
end;

procedure TForm3.ContentsChange(Sender: TObject);
begin
  case Contents.ItemIndex of
    1:begin
        if FileExists (APPFOLDER + 'Docs/Introduction/AErender Launcher.html') then
          WebBrowser1.URL := 'file://'+ APPFOLDER + 'Docs/Introduction/AErender Launcher.html'
      end;
    2:begin
        if FileExists (APPFOLDER + 'Docs/Introduction/After Effects Render Engine.html') then
          WebBrowser1.URL := 'file://'+ APPFOLDER + 'Docs/Introduction/After Effects Render Engine.html'
      end;
    4:begin
        if FileExists (APPFOLDER + 'Docs/User Interface/Main Window.html') then
          WebBrowser1.URL := 'file://'+ APPFOLDER + 'Docs/User Interface/Main Window.html'
      end;
    11: begin
          if FileExists (APPFOLDER + 'Docs/Rendering/Preparing project.html') then
            WebBrowser1.URL := 'file://'+ APPFOLDER + 'Docs/Rendering/Preparing project.html'
        end;
    12: begin
          if FileExists (APPFOLDER + 'Docs/Rendering/Rendering with Launcher.html') then
            WebBrowser1.URL := 'file://'+ APPFOLDER + 'Docs/Rendering/Rendering with Launcher.html'
        end;
  end;
end;

procedure TForm3.FormShow(Sender: TObject);
begin
  ContentsChange (Sender);
end;

end.
