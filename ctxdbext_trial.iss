; Installs Context Database Extensions Trial Package

[Setup]
AppName=Context Database Extensions
AppVerName=Context Database Extensions v.2.07 Trial
AppCopyright=Copyright © 2003-2006, Michael Baytalsky
DefaultDirName={pf}\Context Software\DBExt2
DefaultGroupName=Context Database Extensions
;UninstallDisplayIcon={app}\
LicenseFile=license.txt
Compression=lzma/max
SolidCompression=true
;InternalCompressLevel=ultra
SourceDir=.\
OutputDir=-releases
OutputBaseFilename=ctxdbext$$$t

[Types]
Name: Default; Description: "Full installation"
Name: Custom; Description: "Custom installation"; Flags: iscustom

[Components]
Name: "libd5"; Description: "Delphi 5 Units"; Types: Default
Name: "libd6"; Description: "Delphi 6 Units"; Types: Default
Name: "libd7"; Description: "Delphi 7 Units"; Types: Default
Name: "libd2005"; Description: "Delphi 2005/BDS 3.0 Units"; Types: Default
Name: "libd2006"; Description: "Delphi 2006/BDS 4.0 Units"; Types: Default

;Name: "sources"; Description: "Source Code"; Types: Default
Name: "demos"; Description: "Demos"; Types: Default
Name: "help"; Description: "Help"; Types: Default

[Files]
;Source: "source\*.*"; DestDir: "{app}\source"; Flags: recursesubdirs ignoreversion; Components: sources

Source: "libt\d5\*.*"; DestDir: "{app}\libd5"; Flags: ignoreversion; Components: libd5
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libd5"; Flags: ignoreversion; Components: libd5
;Source: "source\packages\*D5.*"; DestDir: "{app}\libd5\packages"; Flags: ignoreversion; Components: libd5

Source: "libt\d6\*.*"; DestDir: "{app}\libd6"; Flags: ignoreversion; Components: libd6
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libd6"; Flags: ignoreversion; Components: libd6
;Source: "source\packages\*D6.*"; DestDir: "{app}\libd6\packages"; Flags: ignoreversion; Components: libd6

Source: "libt\d7\*.*"; DestDir: "{app}\libd7"; Flags: ignoreversion; Components: libd7
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libd7"; Flags: ignoreversion; Components: libd7
;Source: "source\packages\*D7.*"; DestDir: "{app}\libd7\packages"; Flags: ignoreversion; Components: libd7

Source: "libt\d2005\*.*"; DestDir: "{app}\libd2005"; Flags: ignoreversion; Components: libd2005
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libd2005"; Flags: ignoreversion; Components: libd2005
;Source: "source\packages\*D2005.*"; DestDir: "{app}\libd2005\packages"; Flags: ignoreversion; Components: libd2005

Source: "libt\d2006\*.*"; DestDir: "{app}\libd2006"; Flags: ignoreversion; Components: libd2006
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libd2006"; Flags: ignoreversion; Components: libd2006
;Source: "source\packages\*D2006.*"; DestDir: "{app}\libd2006\packages"; Flags: ignoreversion; Components: libd2006

Source: "demos\*.*"; DestDir: "{app}\demos"; Flags: recursesubdirs; Components: demos

Source: "..\setup\instlhlp.exe"; DestDir: "{app}\help"; Flags: ignoreversion;
Source: "help\ctxdbext.als"; DestDir: "{app}\help"; Flags: ignoreversion; Components: help
Source: "help\ctxdbext.cnt"; DestDir: "{app}\help"; Flags: ignoreversion; Components: help
Source: "help\ctxdbext.hlp"; DestDir: "{app}\help"; Flags: ignoreversion; Components: help
Source: "readme.txt"; DestDir: "{app}"; Flags: isreadme ignoreversion;
Source: "license.txt"; DestDir: "{app}"; Flags: ignoreversion;

; Database adapters
Source: "adapters\dbisam3\*.*"; DestDir: "{app}\adapters\dbisam3"; Flags: recursesubdirs ignoreversion; Tasks: DBISAM3
Source: "adapters\dbisam4\*.*"; DestDir: "{app}\adapters\dbisam4"; Flags: recursesubdirs ignoreversion; Tasks: DBISAM4
Source: "adapters\nexus1\*.*"; DestDir: "{app}\adapters\Nexus1"; Flags: recursesubdirs ignoreversion; Tasks: Nexus1
Source: "adapters\nexus2\*.*"; DestDir: "{app}\adapters\Nexus2"; Flags: recursesubdirs ignoreversion; Tasks: Nexus2

[Icons]
Name: "{group}\Context Database Extensions Help"; Filename: "{app}\help\ctxdbext.hlp"
Name: "{group}\Read Me"; Filename: "{app}\readme.txt"
Name: "{group}\License Agreement"; Filename: "{app}\license.txt"

[Tasks]
Name: insthelp; Description: "Install Help into Delphi IDE"; Components: help;
Name: "DBISAM3"; Description: "Install DBISAM 3 Extensions";
Name: "DBISAM4"; Description: "Install DBISAM 4 Extensions";
Name: "Nexus1"; Description: "Install Nexus 1 Extensions";
Name: "Nexus2"; Description: "Install Nexus 2 Extensions";

[Registry]
Root: HKCU; Subkey: "Software\Borland\Delphi\5.0\Known Packages"; ValueType: string; ValueData: "Context Database Extensions"; ValueName: "{app}\libd5\DBExtPkgD5.bpl"; Flags: uninsdeletevalue; Components: libd5
Root: HKCU; Subkey: "Software\Borland\Delphi\6.0\Known Packages"; ValueType: string; ValueData: "Context Database Extensions"; ValueName: "{app}\libd6\DBExtPkgD6.bpl"; Flags: uninsdeletevalue; Components: libd6
Root: HKCU; Subkey: "Software\Borland\Delphi\7.0\Known Packages"; ValueType: string; ValueData: "Context Database Extensions"; ValueName: "{app}\libd7\DBExtPkgD7.bpl"; Flags: uninsdeletevalue; Components: libd7
Root: HKCU; Subkey: "Software\Borland\BDS\3.0\Known Packages"; ValueType: string; ValueData: "Context Database Extensions"; ValueName: "{app}\libd2005\DBExtPkgD2005.bpl"; Flags: uninsdeletevalue; Components: libd2005
Root: HKCU; Subkey: "Software\Borland\BDS\4.0\Known Packages"; ValueType: string; ValueData: "Context Database Extensions"; ValueName: "{app}\libd2006\DBExtPkgD2006.bpl"; Flags: uninsdeletevalue; Components: libd2006

[Run]
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP delphi 5.0 {code:GetLibPath|libd5} source"; StatusMsg: "Adding library paths (Delphi 5)..."; Components: libd5
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP delphi 6.0 {code:GetLibPath|libd6} source"; StatusMsg: "Adding library paths (Delphi 6)..."; Components: libd6
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP delphi 7.0 {code:GetLibPath|libd7} source"; StatusMsg: "Adding library paths (Delphi 7)..."; Components: libd7
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP bds 3.0 {code:GetLibPath|libd2005} source"; StatusMsg: "Adding library paths (Delphi 2005)..."; Components: libd2005
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP bds 4.0 {code:GetLibPath|libd2006} source"; StatusMsg: "Adding library paths (Delphi 2006)..."; Components: libd2006

Filename: "{app}\help\instlhlp.exe"; Parameters: "-IH CtxDBExtHelp ctxdbext.hlp delphi 5.0 delphi5"; StatusMsg: "Installing Help (Delphi 5)..."; Check: DoInstHelp('libd5')
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IH CtxDBExtHelp ctxdbext.hlp delphi 6.0 delphi6"; StatusMsg: "Installing Help (Delphi 6)..."; Check: DoInstHelp('libd6')
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IH CtxDBExtHelp ctxdbext.hlp delphi 7.0 d7"; StatusMsg: "Installing Help (Delphi 7)..."; Check: DoInstHelp('libd7')
;Filename: "{app}\help\instlhlp.exe"; Parameters: "-IH CtxDBExtHelp ctxdbext.hlp bds 3.0 d2005"; StatusMsg: "Installing Help (Delphi 2005)..."; Check: DoInstHelp('libd2005')
;Filename: "{app}\help\instlhlp.exe"; Parameters: "-IH CtxDBExtHelp ctxdbext.hlp bds 4.0 d2006"; StatusMsg: "Installing Help (Delphi 2006)..."; Check: DoInstHelp('libd2006')

[UninstallRun]
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP delphi 5.0 {code:GetLibPath|libd5} source"; StatusMsg: "Removing library paths (Delphi 5)..."; Components: libd5
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP delphi 6.0 {code:GetLibPath|libd6} source"; StatusMsg: "Removing library paths (Delphi 6)..."; Components: libd6
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP delphi 7.0 {code:GetLibPath|libd7} source"; StatusMsg: "Removing library paths (Delphi 7)..."; Components: libd7
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP bds 3.0 {code:GetLibPath|libd2005} source"; StatusMsg: "Removing library paths (Delphi 2005)..."; Components: libd2005
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP bds 4.0 {code:GetLibPath|libd2006} source"; StatusMsg: "Removing library paths (Delphi 2006)..."; Components: libd2006

Filename: "{app}\help\instlhlp.exe"; Parameters: "-UH CtxDBExtHelp ctxdbext.hlp delphi 5.0 delphi5"; StatusMsg: "Removing Help (Delphi 5)..."; Check: DoInstHelp('libd5')
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UH CtxDBExtHelp ctxdbext.hlp delphi 6.0 delphi6"; StatusMsg: "Removing Help (Delphi 6)..."; Check: DoInstHelp('libd6')
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UH CtxDBExtHelp ctxdbext.hlp delphi 7.0 d7"; StatusMsg: "Removing Help (Delphi 7)..."; Check: DoInstHelp('libd7')
;Filename: "{app}\help\instlhlp.exe"; Parameters: "-UH CtxDBExtHelp ctxdbext.hlp bds 3.0 d2005"; StatusMsg: "Removing Help (Delphi 2005)..."; Check: DoInstHelp('libd2005')
;Filename: "{app}\help\instlhlp.exe"; Parameters: "-UH CtxDBExtHelp ctxdbext.hlp bds 4.0 d2006"; StatusMsg: "Removing Help (Delphi 2006)..."; Check: DoInstHelp('libd2006')

[Code]
function GetLibPath(const InitPath: String): String;
begin
  Result := InitPath;
  if IsTaskSelected('DBISAM3') then
    Result := Result + ',adapters\dbisam3\source';
  if IsTaskSelected('DBISAM4') then
    Result := Result + ',adapters\dbisam4\source';
  if IsTaskSelected('Nexus1') then
    Result := Result + ',adapters\Nexus1\source';
  if IsTaskSelected('Nexus2') then
    Result := Result + ',adapters\Nexus2\source';
end;

function DoInstHelp(const CompName: String): Boolean;
begin
  Result := IsTaskSelected('insthelp') and IsComponentSelected(CompName);
end;

