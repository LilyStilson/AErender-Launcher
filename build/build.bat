@ECHO off
set "msbuild_path=%WINDIR%\Microsoft.NET\Framework\v4.0.30319"
set "BDS=C:\Program Files (x86)\Embarcadero\Studio\21.0\"

::"%msbuild_path%\msbuild.exe" /t:build src\AErender_Launcher.dproj

"%msbuild_path%\msbuild.exe" /t:build AErender_Launcher.ciproj