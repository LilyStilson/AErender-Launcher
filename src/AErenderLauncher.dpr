program AErenderLauncher;

(*        AErender Launcher                                                                 *)
(*        AErenderLauncher.dpr                                                              *)
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

{$R *.dres}

uses
  System.StartUpCopy,
  System.SysUtils,
  FMX.Forms,
  SplashScreenUnit in 'SplashScreenUnit.pas' {SplashScreenForm},
  MainUnit in 'MainUnit.pas' {MainForm},
  SettingsUnit in 'SettingsUnit.pas' {SettingsForm},
  HelpUnit in 'HelpUnit.pas' {HelpForm},
  ImportUnit in 'ImportUnit.pas' {ImportForm},
  AboutUnit in 'AboutUnit.pas' {AboutForm},
  FFMPEGUnit in 'FFMPEGUnit.pas' {FFMPEGForm},
  RenderingUnit in 'RenderingUnit.pas' {RenderingForm},
  MathExpParser in 'MathExpParser.pas',
  OutputModuleEditorUnit in 'OutputModuleEditorUnit.pas' {OutputModuleEditorForm},
  AErenderDataParser in '..\..\AErender Parser\AErenderDataParser.pas',
  Mac.CodeBlocks in 'lib\Mac.CodeBlocks.pas',
  AErenderLauncherLocalization in 'lib\AErenderLauncherLocalization.pas',
  MacApi.Dialogs in 'lib\MacApi.Dialogs.pas';

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
  Application.CreateForm(TFFMPEGForm, FFMPEGForm);
  Application.CreateForm(TRenderingForm, RenderingForm);
  Application.CreateForm(TOutputModuleEditorForm, OutputModuleEditorForm);
  Application.Run;
end.

