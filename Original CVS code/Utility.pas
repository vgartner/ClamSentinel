unit Utility;

interface

uses Windows, Menus, SysUtils, ShellApi, Forms, Messages, Wininet
     , Registry, stdctrls, ShlObj, Classes, Languages;

const CRLF = #13#10;
      CR = #13;

const
  VKSKIP = VK_PAUSE;
  VKMOD = MOD_ALT+MOD_SHIFT; //MOD_ALT, MOD_CONTROL, MOD_SHIFT

type
  TWindowsVersion = (cOSUnknown, cOSWin95, cOSWin98, cOSWin98SE, cOSWinME
                , cOSWinNT, cOSWin2k, cOSWinXP, cOSWin2k3, cOSVista, cOSSeven
                , cOSWin8, cOSGenericWin9x, cOSGenericWinNT);

var
  sMsgCannotOpenURL : string;
  sMsgUnableInitializeWininet : string;

  sComputerName : string;
  sCurrentUserName : string;

  function  GetWindowsVersion: TWindowsVersion;

  function  ExpandEnvironment(const strValue: string): string;
  function  ConvertCommaToCommaText(const s : string) : string;
  function  AddSlash(const s : string): string;
  function  RemoveSlash(const s : string): string;

  function  TopMessage(hWnd: HWND; Text : string; Caption : string;
                   uType : Cardinal) : integer;
  procedure ShellExec(const FileName : string; Parameters : string = '');
  procedure ShowFile(const FileName : string);

  function  Get_File_Size(const sFile: string): Integer;
  procedure LimitLogSize(const sFile : string; const MaxMBSize : double);

  procedure Log(const sMsg : string; const sLogFile : string;
           const bDateTime : boolean = TRUE; const bAddNewLine : boolean = FALSE);

  procedure CheckUncheck(m : TMenuItem; var bSetting : boolean);

  procedure GetURL(const Url: string; const Agent : string; var sResult : string;
                 var sError : string);

  procedure SetKeyState(Key: Byte; bOn: Boolean);

  function IsConnectedToInternet: Boolean;

  function IsForbiddenCharForFilename(const a : char): boolean;

  function DiskInDrive(Drive: Char): Boolean;

  function GetComputerName: string;

  function GetCurrentUserName: string;

  function GetTempDir: string;

  function GetWinDir: string;

  function GetSystemDir : string;

  function GetSystemPath(sEnvVar : string): string;

  function GetCommonSystemPath(sEnvVar : string): string;

  function GetGlobalSystemPath(sEnvVar : string): string;

  function GetWOWGlobalSystemPath(sEnvVar : string): string;

  procedure ListBoxHorScrollBar(ListBox: TListBox);

  function BrowseDialog(const Title: string; const Flag: integer): string;

  function SecondsDiff(const TimeBefore : TDateTime;
                                    const TimeAfter: TDateTime): int64;

  function ConvertToComma(sl : TStrings) : string;

  function WinAPI_GetLongPathName(const ShortName: string): string;

  function GetLongPathName(lpszShortPath: PChar; lpszLongPath: PChar;
                cchBuffer: DWORD): DWORD; stdcall;

  function GetDosEnvironment(const sVal : string): string;

  function StartWith(const sStart : string; const s : string): boolean;

  function ucFirst(const s : string): string;

  procedure HideShowProcessWin9x(const bHide: boolean);

  function ExtractLongPathName(const ShortFileName: String): String;

  function ExtractLongPathNameW(const ShortFileName: String): WideString;

  function WideExtractFileName(const FileName: WideString): WideString;

  function WideExtractFilePath(const FileName: WideString): WideString;

  function StringToWide(const s : string): WideString;

  function FileExistsW(sFile: WideString): Boolean;

  function FormatW(const Msg: WideString; Params: array of WideString): WideString;

  function MatchStrings(Source, pattern: string): Boolean;

  function WideStringToString(const ws: WideString; const codePage: Word): AnsiString;

  function CheckFileExists(const Name : string): boolean;

  function GetFileTimes(const FileName: string; var Created: TDateTime;
    var Accessed: TDateTime; var Modified: TDateTime): Boolean;

  function Ansi2Dos(const s:string):string;

  function ExtensionKnown(const sExt: string): boolean;

  function DoubleExtensions(const sFile : string; var sRealExt : string;
                  var sInternalExt : string; var bManyChars : boolean): boolean;

  function WideFileGetAttr(const FileName: WideString): Cardinal;

  function WideFileSetAttr(const FileName: WideString; Attr: Integer): Boolean;

  function HasNumbers(const s : string): boolean;

  function HasOnlyNumbersOrDots_NotBadValue(const s : string): boolean;

  function IsInList(const l : TStrings; const s : string): integer;

  function  IsExtInList(const sFile : string; const lst : TStrings): boolean;
  
implementation

uses ActiveX;

function IsExtInList(const sFile : string; const lst : TStrings): boolean;
  var
   sExt : string;
begin
  sExt := UpperCase(Trim(ExtractFileExt(sFile)));

  Result := false;

  if sExt <> '' then
  begin
    if lst.IndexOf(sExt)>=0 then
      Result := true;
  end;
end;

function ucFirst(const s : string): string;
begin
  result := '';

  if length(s)>0 then
    result := UpperCase(Copy(s,1,1)) + Copy(s,2,MAXINT);
end;

function StartWith(const sStart : string; const s : string): boolean;
begin
  result := (Copy(s,1,Length(sStart)) = sStart);
end;

function GetDosEnvironment(const sVal : string): string;
var
  buffer: array [0..4096] of Char;
begin
  buffer := '';
  GetEnvironmentVariable(PChar(sVal), buffer, SizeOf(buffer));
  result := Trim(buffer);
end;

function GetLongPathName; external kernel32 Name 'GetLongPathNameA';

function WinAPI_GetLongPathName(const ShortName: string): string;
var
  iLen : integer;
begin
  SetLength(Result, MAX_PATH);

  iLen := GetLongPathName(PChar(ShortName), PChar(Result), MAX_PATH);

  if ((iLen < 0) or (iLen > MAX_PATH)) then
    SetLength(Result, 0)
  else
    SetLength(Result, iLen);
end;

function IsKeyDown(const keys : TKeyboardState; const k : Byte): boolean;
begin
  result := ((keys[k] and 128) <> 0)
end;

function ConvertToComma(sl : TStrings) : string;
var
  s : string;
  i : integer;
begin
  s := '';
  for i := 0 to sl.Count-1 do
  begin
    if s <> '' then
      s := s + ',';

    s := s + sl[i];
  end;
  result := s;
end;

procedure ListBoxHorScrollBar(ListBox: TListBox);
var
  i, w, MaxWidth: Integer;
begin
  { get largest item }
  MaxWidth := 0;
  for i := 0 to ListBox.Items.Count - 1 do
    with ListBox do
    begin
      w := Canvas.TextWidth(Items[i]);
      if w > MaxWidth then
        MaxWidth := w;
    end;

  SendMessage(ListBox.Handle, LB_SETHORIZONTALEXTENT,
    MaxWidth + GetSystemMetrics(SM_CXFRAME), 0);
end;

function GetCommonSystemPath(sEnvVar : string): string;
  {Common Desktop, Common Start Menu, Common Programs, Common Startup, Personal
  , Common AppData, Common Favorites, Common Documents}
begin
  Result := '';

  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      OpenKeyReadOnly('\Software\Microsoft\Windows\CurrentVersion\' +
        'Explorer\Shell Folders');

      if ValueExists(sEnvVar) then
        Result := ReadString(sEnvVar);
    finally
      CloseKey;
      Free;
    end;
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

function GetGlobalSystemPath(sEnvVar : string): string;
  {ProgramFilesDir, CommonFilesDir}
begin
  result := '';
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      OpenKeyReadOnly('\Software\Microsoft\Windows\CurrentVersion');

      if ValueExists(sEnvVar) then
        Result := ReadString(sEnvVar);
    finally
      CloseKey;
      Free;
    end;
end;

function GetWOWGlobalSystemPath(sEnvVar : string): string;
  {ProgramFilesDir, CommonFilesDir}
begin
  result := '';
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      OpenKeyReadOnly('\Software\Wow6432Node\Microsoft\Windows\CurrentVersion');

      if ValueExists(sEnvVar) then
        result := ReadString(sEnvVar);
    finally
      CloseKey;
      Free;
    end;
end;

function GetTempDir: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  GetTempPath(SizeOf(Buffer) - 1, Buffer);
  Result := WinAPI_GetLongPathName(StrPas(Buffer));
end;

function GetWinDir: string;
var
  dir: array [0..MAX_PATH] of Char;
begin
  GetWindowsDirectory(dir, MAX_PATH);
  Result := WinAPI_GetLongPathName(StrPas(dir));
end;

function GetSystemDir: string;
var
  dir: array [0..MAX_PATH] of Char;
begin
  GetSystemDirectory(dir, MAX_PATH);
  Result := WinAPI_GetLongPathName(StrPas(dir));
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

function ConvertCommaToCommaText(const s : string) : string;
begin
  Result := '"' + StringReplace(trim(s), ',', '","'
               , [rfReplaceAll, rfIgnoreCase]) + '"';
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

function TopMessage(hWnd: HWND; Text : string; Caption : string; uType : Cardinal) : integer;
begin
  PostMessage(hWnd, WM_USER + 1024, 0, 0);
  result := Windows.MessageBox(
    hWnd,
    PAnsiChar(Text),
    PAnsiChar(Caption),
    MB_SYSTEMMODAL or MB_SETFOREGROUND or MB_TOPMOST or uType);
end;

procedure ShellExec(const FileName : string; Parameters : string = '');
begin
  ShellExecute(Application.Handle,'open',PChar(FileName),PChar(Parameters),nil, SW_SHOWNORMAL);
end;

procedure ShowFile(const FileName : string);
begin
  if CheckFileExists(FileName) then
    ShellExec(FileName);
end;

function Get_File_Size(const sFile: string): Integer;
var
  SearchRec: TSearchRec;
begin
  Result := -1;
  try
    if FindFirst(ExpandFileName(sFile), faAnyFile, SearchRec) = 0 then
      Result := SearchRec.Size; //in bytes
  finally
    FindClose(SearchRec);
  end;
end;

procedure LimitLogSize(const sFile : string; const MaxMBSize : double);
begin
  try
    if CheckFileExists(sFile) then
      if Get_File_Size(sFile)/(1024*1024) > MaxMBSize then
        DeleteFile(sFile);
  except
  end;
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

procedure CheckUncheck(m : TMenuItem; var bSetting : boolean);
var
 b : boolean;
begin
  b := not(m.Checked);

  bSetting := b;
  m.Checked := b;
end;

procedure GetURL(const Url: string; const Agent : string; var sResult : string; var sError : string);
var
  NetHandle: HINTERNET;
  UrlHandle: HINTERNET;
  Buffer: array[0..1024] of Char;
  BytesRead: dWord;
begin
  sResult := '';
  sError := '';

  NetHandle := InternetOpen(PChar(Agent), // Agent
                     INTERNET_OPEN_TYPE_PRECONFIG, // AccessType
                     nil, // ProxyName
                     nil, // ProxyBypass
                     0);

  if Assigned(NetHandle) then
  begin
    try
      UrlHandle := InternetOpenUrl(NetHandle, PChar(Url), nil, 0
                     , INTERNET_FLAG_RELOAD + INTERNET_FLAG_NO_CACHE_WRITE, 0);

      if Assigned(UrlHandle) then
      begin
        FillChar(Buffer, SizeOf(Buffer), 0);

        try
          repeat
            sResult := sResult + Buffer;
            FillChar(Buffer, SizeOf(Buffer), 0);
            InternetReadFile(UrlHandle, @Buffer, SizeOf(Buffer), BytesRead);
          until BytesRead = 0;
        finally
          InternetCloseHandle(UrlHandle);
        end;
      end
      else
        sError := Format(sMsgCannotOpenURL, [Url]);
    finally
      InternetCloseHandle(NetHandle);
    end;
  end
  else
    sError := sMsgUnableInitializeWininet;
end;

function IsConnectedToInternet: Boolean;
var
  dwConnectionTypes: DWORD;
begin
  result := false;

  dwConnectionTypes :=
    INTERNET_CONNECTION_MODEM +
    INTERNET_CONNECTION_LAN +
    INTERNET_CONNECTION_PROXY;
  try
    Result := InternetGetConnectedState(@dwConnectionTypes, 0);
  except
  end;
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
               2: Result := cOSWin8;
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

function IsForbiddenCharForFilename(const a : char): boolean;
  { for short 8.3 file names }
  const aShortForbiddenChars = [';', '=', '+', '<', '>', '|', '"', '[', ']', '\', '/', ''''];
  { for long file names }
  const aLongForbiddenChars = ['<', '>', '|', '"', '\', '/', ':', '*', '?'];
begin
  result := (a in aShortForbiddenChars) or (a in aLongForbiddenChars);
end;

function DiskInDrive(Drive: Char): Boolean;
  // Disk can be a floppy, CD-ROM,...
var
  ErrorMode: Word;
begin
  result := false;

  { make it upper case }
  if Drive in ['a'..'z'] then Dec(Drive, $20);
  { make sure it's a letter }
  if not (Drive in ['A'..'Z']) then
    exit;
    //raise EConvertError.Create('Not a valid drive ID');
  { turn off critical errors }
  ErrorMode := SetErrorMode(SEM_FailCriticalErrors or SEM_NOGPFAULTERRORBOX or SEM_NOOPENFILEERRORBOX);
  try
    { drive 1 = a, 2 = b, 3 = c, etc. }
    if DiskSize(Ord(Drive) - $40) = -1 then
      Result := False
    else
      Result := True;
  finally
    { Restore old error mode }
    SetErrorMode(ErrorMode);
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

function SecondsDiff(const TimeBefore : TDateTime; const TimeAfter: TDateTime): int64;
var
  FileTimeBefore : TFileTime;
  FileTimeAfter : TFileTime;
  SystemTime: TSystemTime;
begin
  DateTimeToSystemTime(TimeBefore,SystemTime);
  SystemTimeToFileTime(SystemTime,FileTimeBefore);
  DateTimeToSystemTime(TimeAfter,SystemTime);
  SystemTimeToFileTime(SystemTime,FileTimeAfter);
  result := abs((int64(FileTimeAfter) - int64(FileTimeBefore)) div int64(10000000));
end;

procedure SetKeyState(Key: Byte; bOn: Boolean);
var
  KBState: TKeyboardState;
begin
  GetKeyboardState(KBState);

  if (Win32Platform = VER_PLATFORM_WIN32_NT) then
  begin
    if Boolean(KBState[Key]) <> bOn then
    begin
      keybd_event(Key,
                  MapVirtualKey(Key, 0),
                  KEYEVENTF_EXTENDEDKEY,
                  0);

      keybd_event(Key,
                  MapVirtualKey(Key, 0),
                  KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP,
                  0);
    end;
  end
  else
  begin
    KBState[Key] := Ord(bOn);
    SetKeyboardState(KBState);
  end;
end;

procedure HideShowProcessWin9x(const bHide: boolean);
  const
    RSPSIMPLESERVICE     = 1;
    RSPUNREGISTERSERVICE = 0;

  type
    TRegisterServiceProcessFunc = function (dwProcessID, dwType: DWord) : DWord; stdcall;

  var
    dwTypeOp: DWord;
    dllHandle : cardinal;
    RegisterServiceProcessFunc : TRegisterServiceProcessFunc;
begin
  if bHide then
    dwTypeOp := RSPSIMPLESERVICE
  else
    dwTypeOp := RSPUNREGISTERSERVICE;

  dllHandle := LoadLibrary('KERNEL32.DLL') ;

  if dllHandle <> 0 then
  begin
    @RegisterServiceProcessFunc := GetProcAddress(dllHandle, 'RegisterServiceProcess');
    if Assigned(RegisterServiceProcessFunc) then
      RegisterServiceProcessFunc(GetCurrentProcessID, dwTypeOp)
    else
      FreeLibrary(dllHandle);
  end;
end;

function ExtractLongPathName(const ShortFileName: String): String;
var
  pDesktop: IShellFolder;
  sFile: WideString;
  iEaten: Cardinal;
  pItemList: PItemIDList;
  iAttributes: Cardinal;
  szFile: array[0..MAX_PATH] of Char;
  pMalloc: IMalloc;
begin

  Result := '';
  if Succeeded(SHGetDesktopFolder(pDesktop)) then
  begin
    sFile := ShortFileName;
    iAttributes := 0;
    if Succeeded(pDesktop.ParseDisplayName(0, nil, POLESTR(sFile), iEaten,
         pItemList, iAttributes)) then
    begin
      SHGetPathFromIDList(pItemList, szFile);
      Result := szFile;
      // release ItemIdList
      SHGetMalloc(pMalloc);
      pMalloc.Free(pItemList);
    end;
  end;
end;

function WStrScan(const Str: PWideChar; Chr: WideChar): PWideChar;
begin
  Result := Str;
  while Result^ <> Chr do
  begin
    if Result^ = #0 then
    begin
      Result := nil;
      Exit;
    end;
    Inc(Result);
  end;
end;

function WideLastDelimiter(const Delimiters, S: WideString): Integer;
var
  P: PWideChar;
begin
  Result := Length(S);
  P := PWideChar(Delimiters);
  while Result > 0 do
  begin
    if (S[Result] <> #0) and (WStrScan(P, S[Result]) <> nil) then
      Exit;
    Dec(Result);
  end;
end;

function WideExtractFileName(const FileName: WideString): WideString;
var
  I: Integer;
begin
  I := WideLastDelimiter('\:', FileName);
  Result := Copy(FileName, I + 1, MaxInt);
end;

function WideExtractFilePath(const FileName: WideString): WideString;
var
  I: Integer;
begin
  I := WideLastDelimiter('\:', FileName);
  Result := Copy(FileName, 1, I);
end;

function ExtractLongPathNameW(const ShortFileName: String): WideString;
var
  pDesktop: IShellFolder;
  wFile: WideString;
  iEaten: Cardinal;
  pItemList: PItemIDList;
  iAttributes: Cardinal;
  szFile: array[0..MAX_PATH] of WideChar;
  pMalloc: IMalloc;
begin

  Result := '';
  if Succeeded(SHGetDesktopFolder(pDesktop)) then
  begin
    wFile := ShortFileName;
    iAttributes := 0;
    if Succeeded(pDesktop.ParseDisplayName(0, nil, POLESTR(wFile), iEaten,
         pItemList, iAttributes)) then
    begin
      SHGetPathFromIDListW(pItemList, szFile);
      Result := WideString(szFile);
      // release ItemIdList
      SHGetMalloc(pMalloc);
      pMalloc.Free(pItemList);
    end;
  end;
end;

function StringToWide(const s : string): WideString;
const
  SIZE = 4096;
var
  Buffer: array[0..SIZE-1] of WideChar;
begin
  FillChar(Buffer, SizeOf(Buffer), 0);
  result := WideString(StringToWideChar(Copy(s,1,SIZE-1), Buffer, SizeOf(Buffer)));
end;

function FileExistsW(sFile: WideString): Boolean;
var
  li_Attr: DWORD;
begin
  li_Attr := GetFileAttributesW(PWideChar(sFile));
  Result := (li_Attr <> $FFFFFFFF) and ((li_Attr and FILE_ATTRIBUTE_DIRECTORY) = 0);
end;

procedure SReplaceW(var S: WideString; const SFrom, STo: WideString);
var
  i: integer;
begin
  i:= Pos(SFrom, S);
  if i>0 then
    begin
    Delete(S, i, Length(SFrom));
    Insert(STo, S, i);
    end;
end;

function FormatW(const Msg: WideString; Params: array of WideString): WideString;
var
  i: integer;
begin
  Result:= Msg;
  for i:= Low(Params) to High(Params) do
    SReplaceW(Result, '%s', Params[i]);
end;

function MatchStrings(Source, pattern: string): Boolean;
var
  pSource: array [0..4096] of Char;
  pPattern: array [0..4096] of Char;

  function MatchPattern(element, pattern: PChar): Boolean;

    function IsPatternWild(pattern: PChar): Boolean;
    begin
      Result := StrScan(pattern, '*') <> nil;
      if not Result then Result := StrScan(pattern, '?') <> nil;
    end;
  begin
    if 0 = StrComp(pattern, '*') then
      Result := True
    else if (element^ = Chr(0)) and (pattern^ <> Chr(0)) then
      Result := False
    else if element^ = Chr(0) then
      Result := True
    else
    begin
      case pattern^ of
        '*': if MatchPattern(element, @pattern[1]) then
            Result := True
          else
            Result := MatchPattern(@element[1], pattern);
          '?': Result := MatchPattern(@element[1], @pattern[1]);
        else
          if element^ = pattern^ then
            Result := MatchPattern(@element[1], @pattern[1])
          else
            Result := False;
      end;
    end;
  end;
begin
  StrPCopy(pSource, Source);
  StrPCopy(pPattern, pattern);
  Result := MatchPattern(pSource, pPattern);
end;

function WideStringToString(const ws: WideString; const codePage: Word): AnsiString;
var
  l: integer;
begin
  if ws = '' then
    Result := ''
  else
  begin
    l := WideCharToMultiByte(codePage,
      WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
      @ws[1], - 1, nil, 0, nil, nil);
    SetLength(Result, l - 1);
    if l > 1 then
      WideCharToMultiByte(codePage,
        WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
        @ws[1], - 1, @Result[1], l - 1, nil, nil);
  end;
end; { WideStringToString }

function CheckFileExists(const Name : string): boolean;
var
  Attr : DWORD;
begin
  Attr := GetFileAttributes(Pchar(Name));
  result := (Attr <> DWORD(-1)) and ((Attr and FILE_ATTRIBUTE_DIRECTORY) = 0)
end;

function GetFileTimes(const FileName: string; var Created: TDateTime;
var Accessed: TDateTime; var Modified: TDateTime): Boolean;
var
  h: THandle;
  Info1, Info2, Info3: TFileTime;
  SysTimeStruct: SYSTEMTIME;
  TimeZoneInfo: TTimeZoneInformation;
  Bias: Double;
begin
  Result := False;

  try
    h      := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
    if h > 0 then
    begin
      try
        if GetTimeZoneInformation(TimeZoneInfo) = TIME_ZONE_ID_DAYLIGHT then
          Bias := (TimeZoneInfo.Bias+ TimeZoneInfo.DaylightBias) / 1440 // 60x24
        else
          Bias := TimeZoneInfo.Bias / 1440; // 60x24

        GetFileTime(h, @Info1, @Info2, @Info3);
        if FileTimeToSystemTime(Info1, SysTimeStruct) then
          Created := SystemTimeToDateTime(SysTimeStruct) - Bias;
        if FileTimeToSystemTime(Info2, SysTimeStruct) then
          Accessed := SystemTimeToDateTime(SysTimeStruct) - Bias;
        if FileTimeToSystemTime(Info3, SysTimeStruct) then
          Modified := SystemTimeToDateTime(SysTimeStruct) - Bias;
        Result := True;
      finally
        FileClose(h);
      end;
    end;
  except
  end;
end;

function Ansi2Dos(const s:string):string;
begin
  SetLength(Result,Length(s));
  CharToOEMBuff(@s[1],@Result[1],Length(s));
end;

function ExtensionKnown(const sExt: string): boolean;
begin
  result := false;

  try
    if strtoint(copy(sExt,2,MAXINT)) <> 123 then
    exit;
  except
   ;
  end;

  with TRegistry.Create do
    try
      RootKey := HKEY_CLASSES_ROOT;
      //Result := OpenKeyReadOnly(sExt);

      if OpenKeyReadOnly(sExt) then
        result := (Trim(ReadString('')) <> '');
    finally
      CloseKey;
      Free;
    end;
end;

function DoubleExtensions(const sFile : string; var sRealExt : string;
                 var sInternalExt : string; var bManyChars : boolean): boolean;

  Const LIMIT_MORE_CHARS = 7;

  var
    sFileName : string;

  procedure StripChars(var s : string; const sSearch : string;
                             const sReplace : string; var bManyChars : boolean);
    var iLen : integer;
  begin
    iLen := Length(s);

    while Pos(sSearch,s)>0 do
      sFileName := StringReplace(s ,sSearch, sReplace ,[rfReplaceAll, rfIgnoreCase]);

    bManyChars := bManyChars or ((iLen - Length(s)) > LIMIT_MORE_CHARS);
  end;

begin
  bManyChars := false;

  sFileName := ExtractFileName(sFile);

  StripChars(sFileName, ' ', '', bManyChars);
  StripChars(sFileName, '..', '.', bManyChars);
  StripChars(sFileName, '__', '_', bManyChars);

  sRealExt := ExtractFileExt(sFileName);

  sInternalExt := ExtractFileExt(Copy(sFileName, 1, Length(sFileName) - Length(sRealExt)));

  result := (sRealExt<>'') and (sInternalExt<>'');
end;

function WideFileGetAttr(const FileName: WideString): Cardinal;
begin
  Result := GetFileAttributesW(PWideChar(FileName));
end;

function WideFileSetAttr(const FileName: WideString; Attr: Integer): Boolean;
begin
  Result := SetFileAttributesW(PWideChar(FileName), Attr)
end;

function HasNumbers(const s : string): boolean;
  var i : integer;
      bFound : boolean;
begin
  i := 1;
  bFound := false;

  while not ((i>Length(s)) or bFound) do
  begin
    if s[i] in ['0'..'9'] then
      bFound := true
    else
      Inc(i);
  end;

  result := bFound;
end;

function HasOnlyNumbersOrDots_NotBadValue(const s : string): boolean;
  var i : integer;
      bOnlyNumbers : boolean;
      sNumber : string;
      bSameNumbers : boolean;
      iCount : integer;
      bIsSeparator : boolean;
begin
  i := 1;
  bOnlyNumbers := true;
  sNumber := '';
  bSameNumbers := true;
  iCount := 0;

  while not ((i>Length(s)) or not(bOnlyNumbers)) do
  begin
    bIsSeparator := s[i] in ['.',',',' '];

    if not((s[i] in ['0'..'9']) or bIsSeparator) then
      bOnlyNumbers := false
    else
      begin
        if not bIsSeparator then
        begin
          if sNumber='' then
            begin
              sNumber := s[i];
              Inc(iCount);
            end
          else
            begin
              if sNumber <> s[i] then
                bSameNumbers := false
              else
                Inc(iCount);
            end
        end;

        Inc(i);
      end;
  end;

  result := bOnlyNumbers and not(bSameNumbers and (iCount>=8));
end;

function IsInList(const l : TStrings; const s : string): integer;
  var
    i : integer;
    bFound : boolean;
    ss : string;
begin
  i := 0;
  bFound := false;
  result := -1;

  ss := AddSlash(Uppercase(s));
  
  while not(bFound) and (i<l.Count) do
  begin
    if StartWith(ss, AddSlash(Uppercase(l[i]))) then
      bFound := true
    else
      Inc(i);
  end;

  if bFound then
    result := i;
end;

Initialization
  SetLanguage_Utility;

  sComputerName := Trim(GetComputerName);
  sCurrentUserName := Trim(GetCurrentUserName);
end.
