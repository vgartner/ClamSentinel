
{********************************************************}
{                                                        }
{    Firesoft Utils Package                              }
{    ShellNotify Component                               }
{                                                        }
{    Copyright (c) Federico Firenze                      }
{    Buenos Aires, Argentina                             }
{                                                        }
{    Fixed and extended by Andrea Russo - Italy          }
{********************************************************}

unit ShellNotify;

{$IFNDEF VER110}
  {$IFNDEF VER120}
    {$IFNDEF VER130}
      {$WARN SYMBOL_PLATFORM OFF}
      {$DEFINE DELPHICLX}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

interface

uses
  Windows, Messages, Classes, ShlObj;

const
  SHCNF_ACCEPT_INTERRUPTS = $0001;
  SHCNF_ACCEPT_NON_INTERRUPTS = $0002;

  { Mensaje de Notificación de Cambios }
  WM_SHELLNOTIFY = WM_USER + $5000;

type
  SHChangeNotifyEntry = record
    pidl: PItemIDList;
    fRecursive: Boolean;
  end;
  PSHChangeNotifyEntry = ^SHChangeNotifyEntry;

  TItemIDArray = record
    pidl: array[0..1] of PItemIDList
  end;
  PItemIDArray = ^TItemIDArray;

{$EXTERNALSYM SHChangeNotifyRegister}
function SHChangeNotifyRegister(hWnd: HWND; fSources: DWORD; fEvents: cardinal; wMsg: UINT; cEntries: UINT; pfsne: PSHChangeNotifyEntry): THandle; stdcall;
{$EXTERNALSYM SHChangeNotifyDeregister}
function SHChangeNotifyDeregister(ulID: THandle): Boolean; stdcall;
{$EXTERNALSYM SHILCreateFromPath}
function SHILCreateFromPath(pszPath: PChar; ppidl: PSHChangeNotifyEntry; var rgflnOut: PDWORD): HRESULT; stdcall;
{$EXTERNALSYM SHILCreateFromPath}
function SHILCreateFromPathNT(pszPath: LPCWSTR; ppidl: PSHChangeNotifyEntry; var rgflnOut: PDWORD): HRESULT; stdcall;


type
  TShellNotifyEvent = (neAssocChanged, neAttributes, neCreate, neDelete, neDriveAdd, neDriveAddGUI, neDriveRemoved,
                       neMediaInserted, neMediaRemoved, neMkDir, neNetShare, neNetUnshare, neRenameFolder, neRenameItem,
                       neRmDir, neServerDisconnect, neUpdateDir, neUpdateImage, neUpdateItem, neOther, neInterrupt, neFreeSpace);
  TShellNotifyEvents = set of TShellNotifyEvent;
  TSHNotifyEvent = procedure(Sender: TObject; Event: TShellNotifyEvent; Path1, Path2: string) of object;

  TShellNotify = class(TComponent)
  private
    FActive: Boolean;
    FHandle: THandle;
    FNotifyEvents: TShellNotifyEvents;
    FPathList: TStrings;
    FOnNotify: TSHNotifyEvent;
    procedure SetActive(const Value: Boolean);
    procedure SetShellNotifyEvents(const Value: TShellNotifyEvents);
    procedure SetPathList(const Value: TStrings);
  protected
    hNotify: ULONG; { Handle }
    procedure WndProc(var Message: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Open;
    procedure Close;

    property Handle: THandle read FHandle;
  published
    property NotifyEvents: TShellNotifyEvents read FNotifyEvents write SetShellNotifyEvents default [neCreate, neDelete, neMkDir, neRenameFolder, neRenameItem, neRmDir];
    property PathList: TStrings read FPathList write SetPathList;
    property Active: Boolean read FActive write SetActive default False; { Tiene que estar por debajo de NotifyEvents y PathList }
    property OnNotify: TSHNotifyEvent read FOnNotify write FOnNotify;
  end;

{$IFNDEF FSUTILS}
procedure Register;
{$ENDIF}

implementation

uses
  SysUtils, ShellApi, Consts{$IFNDEF DELPHICLX}, Forms{$ENDIF};

{$IFNDEF FSUTILS}
procedure Register;
begin
  RegisterComponents('FireSoft', [TShellNotify]);
end;
{$ENDIF}

function SHChangeNotifyRegister;  external shell32 index 2;
function SHChangeNotifyDeregister; external shell32 index 4;
function SHILCreateFromPathNT; external shell32 index 28;
function SHILCreateFromPath; external shell32 index 28;


function NotifyEventsToCardinal(ANotifyEvents: TShellNotifyEvents): Cardinal;
begin
  Result := 0;

  if neAssocChanged in ANotifyEvents Then
    Result := Result or SHCNE_ASSOCCHANGED;

  if neAttributes in ANotifyEvents Then
    Result := Result or SHCNE_ATTRIBUTES;

  if neCreate in ANotifyEvents Then
    Result := Result or SHCNE_CREATE;

  if neDelete in ANotifyEvents Then
    Result := Result or SHCNE_DELETE;

  if neDriveAdd in ANotifyEvents Then
    Result := Result or SHCNE_DRIVEADD;

  if neDriveAddGUI in ANotifyEvents Then
    Result := Result or SHCNE_DRIVEADDGUI;

  if neDriveRemoved in ANotifyEvents Then
    Result := Result or SHCNE_DRIVEREMOVED;

  if neMediaInserted in ANotifyEvents Then
    Result := Result or SHCNE_MEDIAINSERTED;

  if neMediaRemoved in ANotifyEvents Then
    Result := Result or SHCNE_MEDIAREMOVED;

  if neMkDir in ANotifyEvents Then
    Result := Result or SHCNE_MKDIR;

  if neNetShare in ANotifyEvents Then
    Result := Result or SHCNE_NETSHARE;

  if neNetUnshare in ANotifyEvents Then
    Result := Result or SHCNE_NETUNSHARE;

  if neRenameFolder in ANotifyEvents Then
    Result := Result or SHCNE_RENAMEFOLDER;

  if neRenameItem in ANotifyEvents Then
    Result := Result or SHCNE_RENAMEITEM;

  if neRmDir in ANotifyEvents Then
    Result := Result or SHCNE_RMDIR;

  if neServerDisconnect in ANotifyEvents Then
    Result := Result or SHCNE_SERVERDISCONNECT;

  if neUpdateDir in ANotifyEvents Then
    Result := Result or SHCNE_UPDATEDIR;

  if neUpdateImage in ANotifyEvents Then
    Result := Result or SHCNE_UPDATEIMAGE;

  if neUpdateItem in ANotifyEvents Then
    Result := Result or SHCNE_UPDATEITEM;

  if neInterrupt in ANotifyEvents Then
    Result := Result or SHCNE_INTERRUPT;

  if neFreeSpace in ANotifyEvents Then
    Result := Result or SHCNE_FREESPACE;
end;

function CardinalToNotifyEvent(AValue: Cardinal): TShellNotifyEvent;
begin
  case AValue of
    SHCNE_ASSOCCHANGED:
      {A file type association has changed}
      Result := neAssocChanged;
    SHCNE_ATTRIBUTES:
      {The attributes of an item or folder have changed}
      Result := neAttributes;
    SHCNE_CREATE:
      {A non-folder item has been created}
      Result := neCreate;
    SHCNE_DELETE:
      {A non-folder item has been deleted}
      Result := neDelete;
    SHCNE_DRIVEADD:
      {A drive has been added}
      Result := neDriveAdd;
    SHCNE_DRIVEADDGUI:
      {A drive has been added and the shell should create a new window for the drive}
      Result := neDriveAddGui;
    SHCNE_DRIVEREMOVED:
      {A drive has been removed.}
      Result := neDriveRemoved;
    SHCNE_MEDIAINSERTED:
      {Storage media has been inserted into a drive}
      Result := neMediaInserted;
    SHCNE_MEDIAREMOVED:
      {Storage media has been removed from a drive}
      Result := neMediaRemoved;
    SHCNE_MKDIR:
      {A folder item has been created.}
      Result := neMkDir;
    SHCNE_NETSHARE:
      {A folder on the local computer is being shared via the network}
      Result := neNetShare;
    SHCNE_NETUNSHARE:
      {A folder on the local computer is no longer being shared via the network.}
      Result := neNetUnShare;
    SHCNE_RENAMEFOLDER:
      {The name of a folder has changed.}
      Result := neRenameFolder;
    SHCNE_RENAMEITEM:
      {A non-folder item has been renamed.}
      Result := neRenameItem;
    SHCNE_RMDIR:
      {A folder item has been removed.}
      Result := neRmDir;
    SHCNE_SERVERDISCONNECT:
      {The computer has disconnected from a server.}
      Result := neServerDisconnect;
    SHCNE_UPDATEDIR:
      {The contents of an existing folder have changed}
      {   , but the folder still exists and has not been renamed.}
      Result := neUpdateDir;
    SHCNE_UPDATEIMAGE:
      {An image in the system image list has changed.}
      Result := neUpdateImage;
    SHCNE_UPDATEITEM:
      {An existing non-folder item has changed, but the}
      {    item still exists and has not been renamed.}
      Result := neUpdateItem;
    SHCNE_INTERRUPT:
      {The specified event occurred as a result of a system interrupt.}
      Result := neInterrupt;
    SHCNE_FREESPACE:
      {The amount of free space on a drive has changed.}
      Result := neFreeSpace;
  else
    Result := neOther;
  end;
end;

function StringToWide(const s : string): WideString;
  var
    Buffer: array[0..MAX_PATH - 1] of WideChar;
begin
  FillChar(Buffer, SizeOf(Buffer), 0);
  result := WideString(StringToWideChar(s, Buffer, MAX_PATH));
end;

function WideExtractShortPathName(const FileName: WideString): String;
  var
    Buffer: array[0..MAX_PATH - 1] of WideChar;
begin
  Result := '';
  FillChar(Buffer, SizeOf(Buffer), 0);
  SetString(Result, Buffer, GetShortPathNameW(PWideChar(FileName)
    , Buffer, MAX_PATH));
end;

function PidlToStr(pidl: PItemIDList): string;
var
  PResult: PChar;
  szFile: array[0..MAX_PATH] of WideChar;
  wFilename : WideString;
  sResult : string;
  sTmp : string;
begin
  PResult := StrAlloc(MAX_PATH);
  try
    if not SHGetPathFromIDList(pidl, PResult) then
      sResult := ''
    else
      sResult := StrPas(PResult);
  finally
    StrDispose(PResult);
  end;

  if Win32Platform <> VER_PLATFORM_WIN32_WINDOWS Then
  begin
    SHGetPathFromIDListW(pidl, szFile);
    wFilename := WideString(szFile);

    //if the filename has a different codepage return the short filename 8.3
    if (Pos('?',sResult)>0)
       or (WideString(StringToWide(sResult))
              <> wFilename) then
    begin
      sTmp := Trim(WideExtractShortPathName(wFilename));
      if sTmp <> '' then
        sResult := sTmp;
    end;
  end;
  result := sResult;
end;

{ TShellNotify }

procedure TShellNotify.Close;
begin
  if hNotify > 0 Then
    SHChangeNotifyDeregister(hNotify);

  if FHandle > 0 Then
    DeallocateHWnd(FHandle);
end;

constructor TShellNotify.Create(AOwner: TComponent);
begin
  inherited;
  FPathList := TStringList.Create;
  FActive := False;
  FNotifyEvents := [neCreate, neDelete, neMkDir, neRenameFolder, neRenameItem, neRmDir];
end;

destructor TShellNotify.Destroy;
begin
  Active := False;
  FPathList.Free;
  inherited;
end;

procedure TShellNotify.Open;
var
  BuffPath: array[0..MAX_PATH] of WideChar;
  iPath,
  cEntries: Integer;
  fEvents: Cardinal;
  Attr: PDWORD;
  NotifyEntrys: array[0..1023] of SHChangeNotifyEntry; {Si uso un Array dinámico Falla}

  {$IFNDEF DELPHICLX}
  procedure RaiseLastOsError;
  begin
    RaiseLastWin32Error;
  end;
  {$ENDIF}
begin
  if FNotifyEvents = [] Then
    fEvents := SHCNE_ALLEVENTS or SHCNE_INTERRUPT
  else
    fEvents := NotifyEventsToCardinal(FNotifyEvents);


  FHandle := AllocateHWnd(WndProc);
  try
    if FPathList.Count = 0 Then
    begin
      { Notifica cambios de todo el FileSystem }
      cEntries := 1;
      with NotifyEntrys[0] do
      begin
        pidl := nil;
        fRecursive := True;
      end;
    end
    else
    begin
      cEntries := FPathList.Count;
      for iPath := 0 to cEntries-1 do
      begin
        if Win32Platform <> VER_PLATFORM_WIN32_WINDOWS Then
        begin
          StringToWideChar(FPathList[iPath], BuffPath, SizeOf(BuffPath));
          if SHILCreateFromPathNT(BuffPath, @NotifyEntrys[iPath], Attr) <> S_OK Then
            RaiseLastOsError;
        end else
        begin
          if SHILCreateFromPath(PChar(FPathList[iPath]), @NotifyEntrys[iPath], Attr) <> S_OK Then
            RaiseLastOsError;
        end;

        NotifyEntrys[iPath].fRecursive := True;
      end;
    end;

    try
      hNotify := SHChangeNotifyRegister(FHandle, SHCNF_ACCEPT_INTERRUPTS + SHCNF_ACCEPT_NON_INTERRUPTS,
                                      fEvents , WM_SHELLNOTIFY, cEntries, @NotifyEntrys);

      if hNotify = 0 then
        RaiseLastOsError;

    except
      if FHandle > 0 then
        DeallocateHWnd(FHandle);
        raise;
    end;
  except
    Close;
    raise;
  end;
end;

procedure TShellNotify.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    if not (csDesigning in ComponentState) then
    begin
      if Value then Open else Close;
    end;
    FActive := Value;
  end;
end;

procedure TShellNotify.SetShellNotifyEvents(const Value: TShellNotifyEvents);
begin
  if FNotifyEvents <> Value Then
  begin
    Active := False; { TODO : Falta Código }
    FNotifyEvents := Value;
  end;
end;

procedure TShellNotify.SetPathList(const Value: TStrings);
begin
  Active := False; { TODO : Falta Código }
  FPathList.Assign(Value);
end;

procedure TShellNotify.WndProc(var Message: TMessage);
var
  NotifyEvent: TShellNotifyEvent;
  pItem: PItemIDArray;
  Path1,
  Path2 : string;
begin
  if Message.Msg = WM_SHELLNOTIFY Then
  begin
    NotifyEvent := CardinalToNotifyEvent(Message.LParam and SHCNE_ALLEVENTS);
    pItem := PItemIDArray(Message.wParam);
    Path1 := PidlToStr(pItem^.pidl[0]);

    if NotifyEvent in [neRenameFolder, neRenameItem, neUpdateImage] Then
      Path2 := PidlToStr(pItem^.pidl[1])
    else
      Path2 := '';

    if Assigned(FOnNotify) Then
      FOnNotify(Self, NotifyEvent, Path1, Path2);
  end
  else
    Message.Result := DefWindowProc(FHandle, Message.Msg, Message.wParam, Message.lParam);
end;

end.
