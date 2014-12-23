; Installs Context Database Extensions Package

[Setup]
AppName=Context Database Extensions
AppVerName=Context Database Extensions v.3.37
AppCopyright=Copyright © 2003-2014, Michael Baytalsky
DefaultDirName={pf}\Context Software\DBExt3
DefaultGroupName=Context Database Extensions
;UninstallDisplayIcon={app}\
LicenseFile=license.txt
Compression=lzma/max
SolidCompression=true
;InternalCompressLevel=ultra
SourceDir=.\
OutputDir=-releases
OutputBaseFilename=ctxdbext$$$

[Types]
Name: Default; Description: "Full installation"
Name: Custom; Description: "Custom installation"; Flags: iscustom

[Components]
Name: "libd7"; Description: "Delphi 7 Library"; Types: Default
Name: "libd2005"; Description: "Delphi 2005/BDS 3.0 Library"; Types: Default
Name: "libd2006"; Description: "Delphi 2006/BDS 4.0 Library"; Types: Default
Name: "libd2007"; Description: "Delphi 2007/RAD Studio 5.0 Library"; Types: Default
Name: "libd2009"; Description: "Delphi 2009/RAD Studio 6.0 Library"; Types: Default
Name: "libd2010"; Description: "Delphi 2010/RAD Studio 7.0 Library"; Types: Default
Name: "libd2011"; Description: "Delphi XE/RAD Studio XE Library"; Types: Default
Name: "libdXE2"; Description: "Delphi XE2/RAD Studio XE2 Library"; Types: Default
Name: "libdXE3"; Description: "Delphi XE3/RAD Studio XE3 Library"; Types: Default
Name: "libdXE4"; Description: "Delphi XE4/RAD Studio XE4 Library"; Types: Default
Name: "libdXE5"; Description: "Delphi XE5/RAD Studio XE5 Library"; Types: Default
Name: "libdXE6"; Description: "Delphi XE6/RAD Studio XE6 Library"; Types: Default
Name: "libdXE7"; Description: "Delphi XE7/RAD Studio XE7 Library"; Types: Default

Name: "sources"; Description: "Source Code"; Types: Default
Name: "demos"; Description: "Demos"; Types: Default
Name: "help"; Description: "Help"; Types: Default

[Files]
Source: "source\*.*"; DestDir: "{app}\source"; Flags: recursesubdirs ignoreversion; Components: sources
Source: "packages\*.*"; DestDir: "{app}\packages"; Flags: recursesubdirs ignoreversion; Components: sources
; Copy common sources to designtime folder, just in case
Source: "..\Common\source\CtxPropView.*"; DestDir: "{app}\source\designtime\common"; Flags: ignoreversion; Components: sources
Source: "..\Common\source\CtxGridView.*"; DestDir: "{app}\source\designtime\common"; Flags: ignoreversion; Components: sources
Source: "..\Common\source\CtxProfiles.*"; DestDir: "{app}\source\designtime\common"; Flags: ignoreversion; Components: sources
Source: "..\Common\source\CtxCustomProps.*"; DestDir: "{app}\source\designtime\common"; Flags: ignoreversion; Components: sources
Source: "..\Common\source\CtxNode.*"; DestDir: "{app}\source\designtime\common"; Flags: ignoreversion; Components: sources
Source: "..\Common\source\CtxDockPanel.*"; DestDir: "{app}\source\designtime\common"; Flags: ignoreversion; Components: sources
Source: "..\Common\source\CtxCommonUtils.*"; DestDir: "{app}\source\designtime\common"; Flags: ignoreversion; Components: sources


Source: "lib\d7\*.*"; Excludes: "CtxGridView.*,CtxProfiles.*,CtxPropView.*,CtxNode.*,CtxCustomProps.*,CtxDockPanel.*,CtxCommonUtils.*"; DestDir: "{app}\libd7"; Flags: ignoreversion; Components: libd7
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libd7"; Flags: recursesubdirs ignoreversion; Components: libd7

Source: "lib\d2005\*.*"; Excludes: "CtxGridView.*,CtxProfiles.*,CtxPropView.*,CtxNode.*,CtxCustomProps.*,CtxDockPanel.*,CtxCommonUtils.*"; DestDir: "{app}\libd2005"; Flags: ignoreversion; Components: libd2005
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libd2005"; Flags: recursesubdirs ignoreversion; Components: libd2005

Source: "lib\d2006\*.*"; Excludes: "CtxGridView.*,CtxProfiles.*,CtxPropView.*,CtxNode.*,CtxCustomProps.*,CtxDockPanel.*,CtxCommonUtils.*"; DestDir: "{app}\libd2006"; Flags: ignoreversion; Components: libd2006
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libd2006"; Flags: recursesubdirs ignoreversion; Components: libd2006

Source: "lib\d2007\*.*"; Excludes: "CtxGridView.*,CtxProfiles.*,CtxPropView.*,CtxNode.*,CtxCustomProps.*,CtxDockPanel.*,CtxCommonUtils.*"; DestDir: "{app}\libd2007"; Flags: ignoreversion; Components: libd2007
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libd2007"; Flags: recursesubdirs ignoreversion; Components: libd2007

Source: "lib\d2009\*.*"; Excludes: "CtxGridView.*,CtxProfiles.*,CtxPropView.*,CtxNode.*,CtxCustomProps.*,CtxDockPanel.*,CtxCommonUtils.*"; DestDir: "{app}\libd2009"; Flags: ignoreversion; Components: libd2009
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libd2009"; Flags: recursesubdirs ignoreversion; Components: libd2009

Source: "lib\d2010\*.*"; Excludes: "CtxGridView.*,CtxProfiles.*,CtxPropView.*,CtxNode.*,CtxCustomProps.*,CtxDockPanel.*,CtxCommonUtils.*"; DestDir: "{app}\libd2010"; Flags: ignoreversion; Components: libd2010
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libd2010"; Flags: recursesubdirs ignoreversion; Components: libd2010

Source: "lib\d2011\*.*"; Excludes: "CtxGridView.*,CtxProfiles.*,CtxPropView.*,CtxNode.*,CtxCustomProps.*,CtxDockPanel.*,CtxCommonUtils.*"; DestDir: "{app}\libd2011"; Flags: ignoreversion; Components: libd2011
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libd2011"; Flags: recursesubdirs ignoreversion; Components: libd2011

Source: "lib\dXE2\*.*"; Excludes: "CtxGridView.*,CtxProfiles.*,CtxPropView.*,CtxNode.*,CtxCustomProps.*,CtxDockPanel.*,CtxCommonUtils.*"; DestDir: "{app}\libdXE2"; Flags: ignoreversion; Components: libdXE2
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libdXE2"; Flags: recursesubdirs ignoreversion; Components: libdXE2

Source: "lib\dXE3\*.*"; Excludes: "CtxGridView.*,CtxProfiles.*,CtxPropView.*,CtxNode.*,CtxCustomProps.*,CtxDockPanel.*,CtxCommonUtils.*"; DestDir: "{app}\libdXE3"; Flags: ignoreversion; Components: libdXE3
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libdXE3"; Flags: recursesubdirs ignoreversion; Components: libdXE3

Source: "lib\dXE4\*.*"; Excludes: "CtxGridView.*,CtxProfiles.*,CtxPropView.*,CtxNode.*,CtxCustomProps.*,CtxDockPanel.*,CtxCommonUtils.*"; DestDir: "{app}\libdXE4"; Flags: ignoreversion; Components: libdXE4
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libdXE4"; Flags: recursesubdirs ignoreversion; Components: libdXE4

Source: "lib\dXE5\*.*"; Excludes: "CtxGridView.*,CtxProfiles.*,CtxPropView.*,CtxNode.*,CtxCustomProps.*,CtxDockPanel.*,CtxCommonUtils.*"; DestDir: "{app}\libdXE5"; Flags: ignoreversion; Components: libdXE5
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libdXE5"; Flags: recursesubdirs ignoreversion; Components: libdXE5

Source: "lib\dXE6\*.*"; Excludes: "CtxGridView.*,CtxProfiles.*,CtxPropView.*,CtxNode.*,CtxCustomProps.*,CtxDockPanel.*,CtxCommonUtils.*"; DestDir: "{app}\libdXE6"; Flags: ignoreversion; Components: libdXE6
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libdXE6"; Flags: recursesubdirs ignoreversion; Components: libdXE6

Source: "lib\dXE7\*.*"; Excludes: "CtxGridView.*,CtxProfiles.*,CtxPropView.*,CtxNode.*,CtxCustomProps.*,CtxDockPanel.*,CtxCommonUtils.*"; DestDir: "{app}\libdXE7"; Flags: ignoreversion; Components: libdXE7
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libdXE7"; Flags: recursesubdirs ignoreversion; Components: libdXE7


;Source: "lib\c2006\*.*"; Excludes: "CtxGridView.*,CtxProfiles.*,CtxPropView.*"; DestDir: "{app}\libc2006"; Flags: ignoreversion; Components: libc2006
;Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libc2006"; Flags: recursesubdirs ignoreversion; Components: libc2006

;Source: "lib\c2009\*.*"; Excludes: "CtxGridView.*,CtxProfiles.*,CtxPropView.*"; DestDir: "{app}\libc2009"; Flags: ignoreversion; Components: libc2009
;Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libc2009"; Flags: recursesubdirs ignoreversion; Components: libc2009

Source: "demos\*.*"; DestDir: "{app}\demos"; Flags: recursesubdirs; Components: demos

Source: "..\setup\instlhlp.exe"; DestDir: "{app}\help"; Flags: ignoreversion;
Source: "help\ctxdbext.als"; DestDir: "{app}\help"; Flags: ignoreversion; Components: help
Source: "help\ctxdbext.cnt"; DestDir: "{app}\help"; Flags: ignoreversion; Components: help
Source: "help\ctxdbext.hlp"; DestDir: "{app}\help"; Flags: ignoreversion; Components: help
Source: "readme.txt"; DestDir: "{app}"; Flags: isreadme ignoreversion;
Source: "license.txt"; DestDir: "{app}"; Flags: ignoreversion;

[Icons]
Name: "{group}\Context Database Extensions Help"; Filename: "{app}\help\ctxdbext.hlp"
Name: "{group}\Read Me"; Filename: "{app}\readme.txt"
Name: "{group}\License Agreement"; Filename: "{app}\license.txt"

[Tasks]
Name: insthelp; Description: "Install Help into Delphi IDE"; Components: help;

[Registry]
Root: HKCU; Subkey: "Software\Borland\Delphi\7.0\Known Packages"; ValueType: string; ValueData: "Context Database Extensions"; ValueName: "{app}\libd7\DBExtPkgD7.bpl"; Flags: uninsdeletevalue; Components: libd7
Root: HKCU; Subkey: "Software\Borland\BDS\3.0\Known Packages"; ValueType: string; ValueData: "Context Database Extensions"; ValueName: "{app}\libd2005\DBExtPkgD2005.bpl"; Flags: uninsdeletevalue; Components: libd2005
Root: HKCU; Subkey: "Software\Borland\BDS\4.0\Known Packages"; ValueType: string; ValueData: "Context Database Extensions"; ValueName: "{app}\libd2006\DBExtPkgD2006.bpl"; Flags: uninsdeletevalue; Components: libd2006
;Root: HKCU; Subkey: "Software\Borland\BDS\4.0\Known Packages"; ValueType: string; ValueData: "Context Database Extensions (C++Builder 2006)"; ValueName: "{app}\libc2006\DBExtPkgC2006.bpl"; Flags: uninsdeletevalue; Components: libc2006
Root: HKCU; Subkey: "Software\Borland\BDS\5.0\Known Packages"; ValueType: string; ValueData: "Context Database Extensions"; ValueName: "{app}\libd2007\DBExtPkgD2007.bpl"; Flags: uninsdeletevalue; Components: libd2007
Root: HKCU; Subkey: "Software\CodeGear\BDS\6.0\Known Packages"; ValueType: string; ValueData: "Context Database Extensions"; ValueName: "{app}\libd2009\DBExtPkgD2009.bpl"; Flags: uninsdeletevalue; Components: libd2009
Root: HKCU; Subkey: "Software\CodeGear\BDS\7.0\Known Packages"; ValueType: string; ValueData: "Context Database Extensions"; ValueName: "{app}\libd2010\DBExtPkgD2010.bpl"; Flags: uninsdeletevalue; Components: libd2010
Root: HKCU; Subkey: "Software\Embarcadero\BDS\8.0\Known Packages"; ValueType: string; ValueData: "Context Database Extensions"; ValueName: "{app}\libd2011\DBExtPkgD2011.bpl"; Flags: uninsdeletevalue; Components: libd2011
Root: HKCU; Subkey: "Software\Embarcadero\BDS\9.0\Known Packages"; ValueType: string; ValueData: "Context Database Extensions"; ValueName: "{app}\libdXE2\DBExtPkgDXE2.bpl"; Flags: uninsdeletevalue; Components: libdXE2
Root: HKCU; Subkey: "Software\Embarcadero\BDS\10.0\Known Packages"; ValueType: string; ValueData: "Context Database Extensions"; ValueName: "{app}\libdXE3\DBExtPkgDXE3.bpl"; Flags: uninsdeletevalue; Components: libdXE3
Root: HKCU; Subkey: "Software\Embarcadero\BDS\11.0\Known Packages"; ValueType: string; ValueData: "Context Database Extensions"; ValueName: "{app}\libdXE4\DBExtPkgDXE4.bpl"; Flags: uninsdeletevalue; Components: libdXE4
Root: HKCU; Subkey: "Software\Embarcadero\BDS\12.0\Known Packages"; ValueType: string; ValueData: "Context Database Extensions"; ValueName: "{app}\libdXE5\DBExtPkgDXE5.bpl"; Flags: uninsdeletevalue; Components: libdXE5
Root: HKCU; Subkey: "Software\Embarcadero\BDS\14.0\Known Packages"; ValueType: string; ValueData: "Context Database Extensions"; ValueName: "{app}\libdXE6\DBExtPkgDXE6.bpl"; Flags: uninsdeletevalue; Components: libdXE6
Root: HKCU; Subkey: "Software\Embarcadero\BDS\15.0\Known Packages"; ValueType: string; ValueData: "Context Database Extensions"; ValueName: "{app}\libdXE7\DBExtPkgDXE7.bpl"; Flags: uninsdeletevalue; Components: libdXE7
;Root: HKCU; Subkey: "Software\CodeGear\BDS\6.0\Known Packages"; ValueType: string; ValueData: "Context Database Extensions (C++Builder 2009)"; ValueName: "{app}\libc2009\DBExtPkgC2009.bpl"; Flags: uninsdeletevalue; Components: libc2009

[Run]
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP borland\delphi 7.0 libd7 source"; StatusMsg: "Adding library paths (Delphi 7)..."; Components: libd7
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP borland\bds 3.0 libd2005 source"; StatusMsg: "Adding library paths (Delphi 2005)..."; Components: libd2005
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP borland\bds 4.0 libd2006 source cpp"; StatusMsg: "Adding library paths (Delphi 2006)..."; Components: libd2006
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP borland\bds 5.0 libd2007 source cpp"; StatusMsg: "Adding library paths (Delphi 2007)..."; Components: libd2007
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP codegear\bds 6.0 libd2009 source cpp"; StatusMsg: "Adding library paths (Delphi 2009)..."; Components: libd2009
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP codegear\bds 7.0 libd2010 source cpp"; StatusMsg: "Adding library paths (Delphi 2010)..."; Components: libd2010
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP embarcadero\bds 8.0 libd2011 source cpp"; StatusMsg: "Adding library paths (Delphi XE)..."; Components: libd2011
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP embarcadero\bds 9.0 libdXE2 source cpp"; StatusMsg: "Adding library paths (Delphi XE2)..."; Components: libdXE2
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP embarcadero\bds 10.0 libdXE3 source cpp"; StatusMsg: "Adding library paths (Delphi XE3)..."; Components: libdXE3
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP embarcadero\bds 11.0 libdXE4 source cpp"; StatusMsg: "Adding library paths (Delphi XE4)..."; Components: libdXE4
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP embarcadero\bds 12.0 libdXE5 source cpp"; StatusMsg: "Adding library paths (Delphi XE5)..."; Components: libdXE5
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP embarcadero\bds 14.0 libdXE6 source cpp"; StatusMsg: "Adding library paths (Delphi XE6)..."; Components: libdXE6
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP embarcadero\bds 15.0 libdXE7 source cpp"; StatusMsg: "Adding library paths (Delphi XE7)..."; Components: libdXE7

;Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP borland\bds 4.0 libc2006 source cpp"; StatusMsg: "Adding library paths (C++Builder 2006)..."; Components: libc2006
;Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP codegear\bds 6.0 libc2009 source cpp"; StatusMsg: "Adding library paths (C++Builder 2009)..."; Components: libc2009

Filename: "{app}\help\instlhlp.exe"; Parameters: "-IH CtxDBExtHelp ctxdbext.hlp borland\delphi 7.0 d7"; StatusMsg: "Installing Help (Delphi 7)..."; Check: DoInstHelp('libd7')
;Filename: "{app}\help\instlhlp.exe"; Parameters: "-IH CtxDBExtHelp ctxdbext.hlp borland\bds 3.0 d2005"; StatusMsg: "Installing Help (Delphi 2005)..."; Check: DoInstHelp('libd2005')
;Filename: "{app}\help\instlhlp.exe"; Parameters: "-IH CtxDBExtHelp ctxdbext.hlp borland\bds 4.0 d2006"; StatusMsg: "Installing Help (Delphi 2006)..."; Check: DoInstHelp('libd2006')
;Filename: "{app}\help\instlhlp.exe"; Parameters: "-IH CtxDBExtHelp ctxdbext.hlp borland\bds 5.0 d2007"; StatusMsg: "Installing Help (Delphi 2007)..."; Check: DoInstHelp('libd2007')

[UninstallRun]
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP borland\delphi 7.0 libd7 source"; StatusMsg: "Removing library paths (Delphi 7)..."; Components: libd7
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP borland\bds 3.0 libd2005 source"; StatusMsg: "Removing library paths (Delphi 2005)..."; Components: libd2005
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP borland\bds 4.0 libd2006 source cpp"; StatusMsg: "Removing library paths (Delphi 2006)..."; Components: libd2006
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP borland\bds 5.0 libd2007 source cpp"; StatusMsg: "Removing library paths (Delphi 2007)..."; Components: libd2007
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP codegear\bds 6.0 libd2009 source cpp"; StatusMsg: "Removing library paths (Delphi 2009)..."; Components: libd2009
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP codegear\bds 7.0 libd2010 source cpp"; StatusMsg: "Removing library paths (Delphi 2010)..."; Components: libd2010
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP embarcadero\bds 8.0 libd2011 source cpp"; StatusMsg: "Removing library paths (Delphi XE)..."; Components: libd2011
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP embarcadero\bds 9.0 libdXE2 source cpp"; StatusMsg: "Removing library paths (Delphi XE2)..."; Components: libdXE2
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP embarcadero\bds 10.0 libdXE3 source cpp"; StatusMsg: "Removing library paths (Delphi XE3)..."; Components: libdXE3
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP embarcadero\bds 11.0 libdXE4 source cpp"; StatusMsg: "Removing library paths (Delphi XE4)..."; Components: libdXE4
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP embarcadero\bds 12.0 libdXE5 source cpp"; StatusMsg: "Removing library paths (Delphi XE5)..."; Components: libdXE5
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP embarcadero\bds 14.0 libdXE6 source cpp"; StatusMsg: "Removing library paths (Delphi XE6)..."; Components: libdXE6
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP embarcadero\bds 15.0 libdXE7 source cpp"; StatusMsg: "Removing library paths (Delphi XE7)..."; Components: libdXE7

;Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP borland\bds 4.0 libc2006 source cpp"; StatusMsg: "Removing library paths (C++Builder 2006)..."; Components: libc2006
;Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP codegear\bds 6.0 libc2009 source cpp"; StatusMsg: "Adding library paths (C++Builder 2009)..."; Components: libc2009

Filename: "{app}\help\instlhlp.exe"; Parameters: "-UH CtxDBExtHelp ctxdbext.hlp borland\delphi 7.0 d7"; StatusMsg: "Removing Help (Delphi 7)..."; Check: DoInstHelp('libd7')
;Filename: "{app}\help\instlhlp.exe"; Parameters: "-UH CtxDBExtHelp ctxdbext.hlp borland\bds 3.0 d2005"; StatusMsg: "Removing Help (Delphi 2005)..."; Check: DoInstHelp('libd2005')
;Filename: "{app}\help\instlhlp.exe"; Parameters: "-UH CtxDBExtHelp ctxdbext.hlp borland\bds 4.0 d2006"; StatusMsg: "Removing Help (Delphi 2006)..."; Check: DoInstHelp('libd2006')
;Filename: "{app}\help\instlhlp.exe"; Parameters: "-UH CtxDBExtHelp ctxdbext.hlp borland\bds 4.0 d2006"; StatusMsg: "Removing Help (Delphi 2007)..."; Check: DoInstHelp('libd2007')

[Code]
function DoInstHelp(const CompName: String): Boolean;
begin
  Result := IsTaskSelected('insthelp') and IsComponentSelected(CompName);
end;

