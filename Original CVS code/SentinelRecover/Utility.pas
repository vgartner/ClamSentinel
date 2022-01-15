unit Utility;

{$I DelphiVersion.inc}

interface

uses SysUtils, Registry, IniFiles, Windows, Forms, ShellApi, FileCtrl, ShlObj;

const
  LOCAL_PATH_CLAMWIN_PORTABLE = '..\..\..\data\settings';
  SCANNER_CONF_FILE = 'ClamWin.conf';

const CRLF = #13#10;
      CR = #13;

type
  TWindowsVersion = (cOSUnknown, cOSWin95, cOSWin98, cOSWin98SE, cOSWinME
                , cOSWinNT, cOSWin2k, cOSWinXP, cOSWin2k3, cOSVista, cOSSeven
                , cOSGenericWin9x, cOSGenericWinNT);
                
var
  sComputerName : string;
  sCurrentUserName : string;

function AddSlash(const s : string): string;
function  RemoveSlash(const s : string): string;
function LastDelimiter(const Delimiters, S: string): Integer;
function ChangeFileExt(const FileName, Extension: string): string;
function ExpandEnvironment(const strValue: string): string;
function IsClamWinPortable : boolean;
function GetIniFilePath(const sProjectFileName : string) : string;
procedure Log(const sMsg : string; const sLogFile : string;
          const bDateTime : boolean = TRUE; const bAddNewLine : boolean = FALSE);
function GetComputerName: string;
function GetCurrentUserName: string;
procedure ShowFile(const FileName : string);
procedure ShellExec(const FileName : string; Parameters : string = '');
function  GetWindowsVersion: TWindowsVersion;
function  GetSystemPath(sEnvVar : string): string;
function BrowseDialog(const Title: string; const Flag: integer): string;
function CheckFileExists(const Name : string): boolean;

implementation

uses Languages;

procedure ShowFile(const FileName : string);
begin
  if CheckFileExists(FileName) then
    ShellExec(FileName);
end;

procedure ShellExec(const FileName : string; Parameters : string = '');
begin
  ShellExecute(Application.Handle,'open',PChar(FileName),PChar(Parameters),nil, SW_SHOWNORMAL);
end;

function AddSlash(const s : string): string;
begin
  if Copy(s,Length(s),1) <> '\' then
    result := Trim(s) + '\'
  else
    result := Trim(s);
end;

function RemoveSlash(const s : string): string;
begin
  if Copy(s,Length(s),1) = '\' then
    result := Copy(s,1,Length(s)-1)
  else
    result := s;
end;

function LastDelimiter(const Delimiters, S: string): Integer;
var
  P: PChar;
begin
  Result := Length(S);
  P := PChar(Delimiters);
  while Result > 0 do
  begin
    if (S[Result] <> #0) and (StrScan(P, S[Result]) <> nil) then
      if (ByteType(S, Result) = mbTrailByte) then
        Dec(Result)
      else
        Exit;
    Dec(Result);
  end;
end;

function ChangeFileExt(const FileName, Extension: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('.\:',Filename);
  if (I = 0) or (FileName[I] <> '.') then I := MaxInt;
  Result := Copy(FileName, 1, I - 1) + Extension;
end;

function GetSystemPath(sEnvVar : string): string;
  {Desktop, Start Menu, Programs, Startup, Personal, AppData,
  Fonts, SendTo, Recent, Favorites, Cache, Cookies, History,
  NetHood, PrintHood, Templates, Local AppData, Local Settings}
begin
  result := '';
  with TRegistry.Create do
    try
      RootKey := HKEY_CURRENT_USER;
      OpenKeyReadOnly('\Software\Microsoft\Windows\CurrentVersion\' +
        'Explorer\Shell Folders');

      if ValueExists(sEnvVar) then
        Result := ReadString(sEnvVar);
    finally
      CloseKey;
      Free;
    end;
end;

function IsClamWinPortable : boolean;
var
 sExePath : string;
begin
  sExePath := ExtractFilePath(Application.ExeName);
  result := CheckFileExists(sExePath+LOCAL_PATH_CLAMWIN_PORTABLE+'\'+SCANNER_CONF_FILE);
end;

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

  sSystemAppData := AddSlash(GetSystemPath('AppData'));

  sIniFile := AddSlash(sExePath) + sProjectFileName + '.ini';

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

function ExpandEnvironment(const strValue: string): string;
var
  aResult: array[0..65535] of Char;
  wrdReturn: DWORD;
begin
  wrdReturn := ExpandEnvironmentStrings(PChar(strValue), aResult, 65536);
  if wrdReturn = 0 then
    result := strValue
  else
    result := Trim(aResult);
end;

procedure Log(const sMsg : string; const sLogFile : string;
          const bDateTime : boolean = TRUE; const bAddNewLine : boolean = FALSE);
var
  F:TextFile;
begin
  try
    AssignFile(F,sLogFile);
    try
      if CheckFileExists(sLogFile) then
        Append(F)
      else
        Rewrite(F);

      if bAddNewLine then
        Writeln(F,'');

      if bDateTime then
        Writeln(F,'##### '+FormatDateTime('dddddd tt', Now) + ' (' + sCurrentUserName + '@' + sComputerName+')');

      if sMsg <> '' then
        Writeln(F,sMsg);
    finally
      CloseFile(F);
    end;
  except
    on E : EInOutError do
      if E.ErrorCode <> 103 then
        Application.MessageBox(PChar(E.Message + ' ' + sMsgError +':' + inttostr(E.ErrorCode)),PChar(sMsgError),MB_OK);
  end;
end;

function GetComputerName: string;
var
  buffer: array[0..MAX_COMPUTERNAME_LENGTH + 1] of Char;
  Size: Cardinal;
begin
  Size := MAX_COMPUTERNAME_LENGTH + 1;
  Windows.GetComputerName(@buffer, Size);
  Result := StrPas(buffer);
end;

function GetCurrentUserName: string;
const
  cnMaxUserNameLen = 254;
var
  sUserName: string;
  dwUserNameLen: DWORD;
begin
  dwUserNameLen := cnMaxUserNameLen - 1;
  SetLength(sUserName, cnMaxUserNameLen);
  GetUserName(PChar(sUserName), dwUserNameLen);
  SetLength(sUserName, dwUserNameLen);
  Result := sUserName;
end;

function GetWindowsVersion: TWindowsVersion;
begin
  Result := cOSUnknown;

  case Win32Platform of
    VER_PLATFORM_WIN32_NT:
      begin
        Result := cOSGenericWinNT;

        case Win32MajorVersion of
          4: Result := cOSWinNT;
          5: case Win32MinorVersion of
               0: Result := cOSWin2k;
               1: Result := cOSWinXP;
               2: Result := cOSWin2k3;
             end;
          6: case Win32MinorVersion of
               0: Result := cOSVista;
               1: Result := cOSSeven;
             end;
        end;
      end;

    VER_PLATFORM_WIN32_WINDOWS:
      begin
        Result := cOSGenericWin9x;

        if Win32MajorVersion = 4 then
          case Win32MinorVersion of
            00: Result := cOSWin95;

            10: if Trim(Win32CSDVersion) = 'A' then
                  Result := cOSWin98SE
                else
                  Result := cOSWin98;

            90: Result := cOSWinME;
          end;
      end;
  end;
end;

function BrowseDialogCallBack
  (Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): 
  integer stdcall;
var
  wa, rect : TRect;
  dialogPT : TPoint;
begin
  //center in work area
  if uMsg = BFFM_INITIALIZED then
  begin
    SystemParametersInfo(SPI_GETWORKAREA, 0, @wa, 0);
    GetWindowRect(Wnd, Rect);
    dialogPT.X := ((wa.Right-wa.Left) div 2) - 
                  ((rect.Right-rect.Left) div 2);
    dialogPT.Y := ((wa.Bottom-wa.Top) div 2) - 
                  ((rect.Bottom-rect.Top) div 2);
    MoveWindow(Wnd,
               dialogPT.X,
               dialogPT.Y,
               Rect.Right - Rect.Left,
               Rect.Bottom - Rect.Top,
               True);
  end;

  Result := 0;
end; (*BrowseDialogCallBack*)

function BrowseDialog(const Title: string; const Flag: integer): string;
var
  lpItemID : PItemIDList;
  BrowseInfo : TBrowseInfo;
  DisplayName : array[0..MAX_PATH] of char;
  TempPath : array[0..MAX_PATH] of char;
begin
  Result:='';
  FillChar(BrowseInfo, sizeof(TBrowseInfo), #0);
  with BrowseInfo do begin
    hwndOwner := Application.Handle;
    pszDisplayName := @DisplayName;
    lpszTitle := PChar(Title);
    ulFlags := Flag;
    lpfn := BrowseDialogCallBack;
  end;
  lpItemID := SHBrowseForFolder(BrowseInfo);
  if lpItemId <> nil then begin
    SHGetPathFromIDList(lpItemID, TempPath);
    Result := TempPath;
    GlobalFreePtr(lpItemID);
  end;
end;

function CheckFileExists(const Name : string): boolean;
var
  Attr : DWORD;
begin
  Attr := GetFileAttributes(Pchar(Name));
  result := (Attr <> DWORD(-1)) and ((Attr and FILE_ATTRIBUTE_DIRECTORY) = 0)
end;

Initialization
  sComputerName := Trim(GetComputerName);
  sCurrentUserName := Trim(GetCurrentUserName);
end.
