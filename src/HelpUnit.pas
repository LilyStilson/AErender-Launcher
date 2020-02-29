unit HelpUnit;

(*        AErender Launcher                                                                 *)
(*        HelpUnit.pas                                                                      *)
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
  THelpForm = class(TForm)
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
  HelpForm: THelpForm;
  MacDocsPaths: TArray<String> = ['', '',
                                  '', '', '', '', '', '',
                                  '', '',
                                  '', ''];
  WinDocsPaths: TArray<String> = [];

implementation

uses
  {$REGION '    AErenderLauncher Liraries    '}
  MainUnit;
  {$ENDREGION}

{$R *.fmx}

{$IFDEF MSWINDOWS}
procedure THelpForm.CreateHandle;
begin
  inherited CreateHandle;

  var hWnd: HWND := FormToHWND(Self);

  SetWindowLong(hWnd, GWL_EXSTYLE, GetWindowLong(hWnd, GWL_EXSTYLE) or WS_EX_APPWINDOW);
  SetClassLong(hWnd, GCL_STYLE, GetClassLong(hWnd, GCL_STYLE) or CS_DROPSHADOW);
end;
{$ENDIF MSWINDOWS}

procedure THelpForm.Button1Click(Sender: TObject);
begin
  HelpForm.Close;
end;

procedure THelpForm.ContentsChange(Sender: TObject);
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

procedure THelpForm.FormShow(Sender: TObject);
begin
  ContentsChange (Sender);
end;

end.
