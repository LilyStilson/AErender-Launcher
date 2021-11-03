program AErender_Launcher;

(*        AErender Launcher                                                                 *)
(*        AErenderLauncher.dpr                                                              *)
(*        Lily Stilson // 2019 - 2021                                                       *)
(*        MIT License                                                                       *)
(*                                                                                          *)
(*        Copyright (c) 2019 - 2021 Alice Romanets                                          *)
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

{$R *.dres}

uses
  System.StartUpCopy,
  System.SysUtils,
  FMX.Forms,
  
  /// RAD Studio 10.4 Sydney
  {$IF CompilerVersion = 34.0}
  FMX.Graphics in 'lib\FMX\DX 10.4\FMX.Graphics.pas',
  FMX.Ani in 'lib\FMX\DX 10.4\FMX.Ani.pas',
  FMX.TreeView in 'lib\FMX\DX 10.4\FMX.TreeView.pas',
  {$IFDEF MSWINDOWS}
  FMX.Platform.Win in 'lib\FMX\DX 10.4\FMX.Platform.Win.pas',
  {$ENDIF MSWINDOWS}

  /// RAD Studio 11.0 Alexandria
  ///
  /// Bug with double animation on text fields in FMX.Ani was fixed,
  /// no need to include it anymore
  {$ELSE CompilerVersion = 35.0}
  FMX.Graphics in 'lib\FMX\DX 11.0\FMX.Graphics.pas',
  FMX.TreeView in 'lib\FMX\DX 11.0\FMX.TreeView.pas',
  {$IFDEF MSWINDOWS}
  FMX.Platform.Win in 'lib\FMX\DX 11.0\FMX.Platform.Win.pas',
  {$ENDIF MSWINDOWS}

  {$ENDIF}

  FMX.CompLabel in 'lib\Components\TCompLabel\Package\FMX.CompLabel.pas',
  FMX.RenderProgress in 'lib\Components\TRenderProgress\Package\FMX.RenderProgress.pas',
  SplashScreenUnit in 'SplashScreenUnit.pas' {SplashScreenForm},
  MainUnit in 'MainUnit.pas' {MainForm},
  SettingsUnit in 'SettingsUnit.pas' {SettingsForm},
  HelpUnit in 'HelpUnit.pas' {HelpForm},
  ImportUnit in 'ImportUnit.pas' {ImportForm},
  AboutUnit in 'AboutUnit.pas' {AboutForm},
  FFMPEGUnit in 'FFMPEGUnit.pas' {FFMPEGForm},
  RenderingUnit in 'RenderingUnit.pas' {RenderingForm},
  OutputModuleEditorUnit in 'OutputModuleEditorUnit.pas' {OutputModuleEditorForm},
  {$IFDEF MACOS}
  Mac.CodeBlocks in 'lib\Mac.CodeBlocks.pas',
  MacApi.Dialogs in 'lib\MacApi.Dialogs.pas',
  {$ENDIF MACOS}
  {$IFDEF MSWINDOWS}
  FMX.Taskbar in 'lib\FMX\FMX.Taskbar.pas',
  {$ENDIF MSWINDOWS}
  AErenderLauncher.Localization in 'lib\AErenderLauncher.Localization.pas',
  AErenderLauncher.AerenderParser in 'lib\AErenderLauncher.AerenderParser.pas',
  AErenderLauncher.Math.ExpParser in 'lib\AErenderLauncher.Math.ExpParser.pas',
  AErenderLauncher.Rendering in 'lib\AErenderLauncher.Rendering.pas',
  AErenderLauncher.IO in 'lib\AErenderLauncher.IO.pas',
  AErenderLauncher.Math in 'lib\AErenderLauncher.Math.pas' {/System.Types.Nullable in 'lib\System.Types.Nullable.pas';},
  AErenderLauncher.SysUtils in 'lib\AErenderLauncher.SysUtils.pas',
  AErenderLauncher.Types in 'lib\AErenderLauncher.Types.pas';

{$R *.res}

begin
  Application.Initialize;

  SplashScreenForm := TSplashScreenForm.Create(nil);
  SplashScreenForm.Show;

  Application.ProcessMessages;

  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.CreateForm(THelpForm, HelpForm);
  Application.CreateForm(TImportForm, ImportForm);
  Application.CreateForm(TAboutForm, AboutForm);
  // Temporary disabled due to inactivity
  //Application.CreateForm(TFFMPEGForm, FFMPEGForm);
  Application.CreateForm(TRenderingForm, RenderingForm);
  Application.CreateForm(TOutputModuleEditorForm, OutputModuleEditorForm);
  Application.Run;
end.

