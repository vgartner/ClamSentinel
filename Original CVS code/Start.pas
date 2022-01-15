unit Start;

{$I DelphiVersion.inc}

interface

type TScannerType = (scanner_ClamWin, none);
  
function GetIniFilePath(const sProjectFileName : string) : string;

procedure InitScanner;

var CurrentScanner : TScannerType;

implementation

uses SysUtils, IniFiles, Forms, FileCtrl, Utility, uClamWinScanner;

function GetIniFilePath(const sProjectFileName : string) : string;
var
  sPathProjectPathAppData : string;
  bDir : boolean;
  sExePath : string;
  sSystemAppData : string;
  sIniFile : string;
  IniFile : TIniFile;
  bUseLocalIniFile : boolean;

begin
  sExePath := ExtractFilePath(Application.ExeName);

  sSystemAppData := ExpandEnvironment(AddSlash(GetSystemPath('AppData')));

  sIniFile := ChangeFileExt(Application.ExeName, '.ini');

  if not(IsClamWinPortable) then
  begin
    bUseLocalIniFile := false;

    if CheckFileExists(sIniFile) then
    begin
      IniFile := TIniFile.Create(sIniFile);
      try
        bUseLocalIniFile := (IniFile.ReadInteger('Params','UseLocalIniFile',0) = 1);
      finally
        IniFile.Free;
      end;
    end;

    if not(bUseLocalIniFile) then
    begin
      sPathProjectPathAppData := sSystemAppData+sProjectFileName;

      bDir := DirectoryExists(sPathProjectPathAppData);

      if not bDir then
        bDir := CreateDir(sPathProjectPathAppData);

      if bDir then
        sIniFile := AddSlash(sPathProjectPathAppData)+sProjectFileName+'.ini';
    end;
  end;

  result := sIniFile;
end;

Procedure InitScanner;
begin
  InitClamWin;
end;

Initialization
  CurrentScanner := none;
end.
 