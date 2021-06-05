# Building AErender Launcher
## Step 1
To build **Launcher** you first of all need to install a RAD Studio *(Delphi, we don't need C++Builder here).* There is three options how to do it:
### Option 1 - Legal, Free, but outdated
1) Go to https://www.embarcadero.com/
2) Go to Free Tools -> Delphi Community Ed.
3) Click on "Get community edition free"
4) Complete form, download and install Delphi Community Edition 10.3.3 with macOS/OSX dev tools

**Downsides:** You will have outdated version of Delphi. If you try to build **Launcher**, it will build, but I won't guarantee you 100% functionality, because there was a lot of bugfixes since 10.3.3 to the newest version

### Option 2 - Legal, but Trial
1) Go to https://www.embarcadero.com/
2) Go to Products -> RAD Studio
3) Click on "Start a Free Trial"
4) Complete form, download and install RAD Studio Trial 10.4.2 with macOS/OSX dev tools

### Option 3 - Illegal, but no Trial
1) Find whereever a pirated version of RAD Studio Architect 10.4.2 
2) ...
3) Install and crack RAD Studio Architect 10.4.2

*btw, I don't support this, but oh well...*

## Step 2
### If you have MSBuild installed and you want to build for Windows
To build only Windows versions of **Launcher**, go to `/build/` folder at the root of the repository and start a `build.bat` file. Both x64 and x32 versions will be built into the following folders:
```text
/src/Win32/Beta/AErender_Launcher.exe
```
```text
/src/Win64/Beta/AErender_Launcher.exe
```

### If you don't have MSBuild installed or you want to build for macOS
1) Open `/src/AErender_Launcher.dproj`
2) Change project's **Build Configurations** to "Beta" (if it's not already)
3) Select needed platform by changing it in project's **Target Platforms** (double click)
4) Compile project by `Shift+F9` or going into `Project` -> `Build AErender_Launcher`