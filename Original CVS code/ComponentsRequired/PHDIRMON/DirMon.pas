//==============================================================================
// Modul - Name   : DirMon
// Version        : 1.2
// Description    : Directory Changes Monitoring Component.
//                  The component notifies by changes in a diretory such as:
//                  File/Directory: Create/Delete/Modify/Rename
//                  We have 4 Events:
//                    OnCreated: TFileChangedEvent
//                    OnDeleted: TFileChangedEvent
//                    OnModified: TFileChangedEvent
//                    OnRenamed: TFileRenamedEvent
//                  We are using here only the Idea's from Damien Thouvenin - but it is
//                  is easyer for us to use a Component instead the source of his Demo.
//                  if you find it good so send your thanks also to damien@thouvenin.net
//                  but your BUGS you can sent to support@phaeteon.de
//------------------------------------------------------------------------------
// Compatible: Delphi 4 and Delphi 5 and Delphi 6 and Lazarus
//------------------------------------------------------------------------------
// Tested Platforms : Windows NT 4.0, Windows 2000 Prof/Server, XP, Vista
//                    not Compatible to Windows'98
//------------------------------------------------------------------------------
// Histoy:
// When        Who   What
// 06.11.2000  UR    Created by phaeteon(www.phaeteon.de) and LoLa (www.lolasoft.de)
// 09.09.2009  Andrea Russo - Italy Fixed problem about the extraction of the filename
// 02.23.2010  Andrea Russo - Italy Remove PostQueuedCompletionStatus that cause problems
//                            on Vista so now GetQueuedCompletionStatus has a MAXINT timeout
// 09.22.2010  Andrea Russo - Italy Some changes about unicode filenames
// 11.12.2013  Andrea Russo - Italy Added Lazarus support
//
//==============================================================================


// This is the "readme" from Damien Thouvenin - great thanks for this routines!
// His original files are in "DirMon.zip" you can found at torry or DSP
{ ========================== DIR EVENTS MONITORING =============================

  == Overview ==

  This form demonstrates the use of ReadDirectoryChangesW API function

  The function can be used either in synchronous or asynchronous mode.
  Here I implemented the asynchronous mode (ie: function returns immediatly
  and you have to watch for events). There are 3 ways to get results:
  a. call the function with a callback proc that the system calls when an event occurs
  b. associate the directory with an IOCompletionPort that "traps" IO events (that's what I did)
  c. create an Event, wait for it and call GetOverlapped Result

  For more information on synchronous calls or on the 2 other asynchronous implementations refer to MSDN

  == Implementation notes ==

  I assume anyone willing to use this code will have sufficient knowledge of
  basic API Calls so I won't comment on threads, API Structures etc...

  I implemented a very basic call to SHBrowseForFolder. If you're interested
  refer to MSDN or download Brad Stower's components at www.delphifreestuff.com

  OK, Now we get to the bottom of things.
  Like much of the APIs, Monitoring is quite simple once you know how to get it to work !

  First you have to open the directory you want to monitor. Use CreateFile in
  FILE_LIST_DIRECTORY mode and with FILE_FLAG_BACKUP_SEMANTICS privilege.
  Note that you have to add FILE_FLAG_OVERLAPPED for asynchronous operations.

  Then create an IOCompletionPort with the directory handle.
  If you open multiple directories, you can reuse the same port, simply
  specify a different cookie for each dir.

  Third Call ReadDirectoryChangesW with an empty Overlapped struct and no
  callback proc (asynchronous b method, see overview)

  Then wait for events using GetQueuedCompletionStatus. Upon event fire ReadDirectoryChangesW
  again and loop.

  Here you have mulmtiple implementation choices. Either you give a TimeOut to GetQueuedCompletionStatus
  and check whether it returned sth or (what I did) you call it in a thread with INFINITE wait time
  In this alternative, post an empty completion status to stop the thread; see PostQueuedCompletionStatus
  call in bStopClick method

  When you are finished, release all dir handles and IOCompletionPort with CloseHandle

  Events are written as continous TFileNotifyInformation records in a buffer you provide.

  >>Important Note<<
  FBytesWritten is not updated by asynchronous calls to ReadDirectoryChangesW
  Thus, don't rely on it for buffer parsing. Rather use the NextEntryOffset which
  is set to 0 for the last record.

  == Release Notes ==

  This code has been tested with delphi 3.02 running on Windows NT4 SP6
  It should work on all Windows NT platforms, though I haven't tested it under
  Windows NT 3.51 or Windows 2000.

  I don't known whether it works under Win9x or not.
  Eventually, it may be kind of you to let me know if you run some tests

  You shouldn't have much trouble compiling it with Delphi 2/4/5+ and C++ port is quite easy

  Damien Thouvenin  (mailto:damien@thouvenin.net)
}

unit DirMon;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

//==============================================================================
interface
//==============================================================================

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms
  {$IFDEF FPC}
    ,FileUtil
  {$ELSE}
    ,Messages
  {$ENDIF}
  , Dialogs;

type
  // Exception's
  EDirMonError = class(Exception);

  // Changes in Files/Directories
  TFileChangedEvent = procedure( Sender: TObject; FileName: String) of Object;
  // Files/Directory - Renamed
  TFileRenamedEvent = procedure( Sender: TObject; fromFileName: String; toFileName: String) of Object;

  // watch filters
  TWatchFilter = (nfFILE_NAME,
                  nfDIR_NAME,
                  nfATTRIBUTES,
                  nfSIZE,
                  nfLAST_WRITE,
                  nfLAST_ACCESS,
                  nfCREATION,
                  nfSECURITY);
  TWatchFilters = set of TWatchFilter;

  // The Directory Monitor
  TDirMon = class(TComponent)
  private
    { Private-Deklarationen }
    FDirectoryHandle: THandle;
    FNotificationBuffer: array[0..4096] of Byte;
    FWatchThread: TThread;
    FWatchFilters: TWatchFilters;
    FNotifyFilter: DWord;
    FOverlapped: TOverlapped;
    {$IFDEF FPC}
        FPOverlapped: LPOverlapped;
    {$ELSE}
        FPOverlapped: POverlapped;
    {$ENDIF}
    FBytesWritten: DWORD;
    FCompletionPort: THandle;
    FPath: String;
    FActive: Boolean;
    FOnCreated: TFileChangedEvent;
    FOnDeleted: TFileChangedEvent;
    FOnModified: TFileChangedEvent;
    FOnRenamed: TFileRenamedEvent;
    FWatchSubTree: Boolean;

    procedure SetActive( AActive: Boolean);
    procedure SetPath(aPath: String);
    procedure cmdCreated( Sender: TObject; FileName: String);
    procedure cmdDeleted( Sender: TObject; FileName: String);
    procedure cmdModified( Sender: TObject; FileName: String);
    procedure cmdRenamed( Sender: TObject; fromFileName: String; toFileName: String);
  protected

    procedure Start;
    procedure Stop;
  public
    { Public-Deklarationen }
    { Protected-Deklarationen }
    constructor Create( AOwner: TComponent); override;
    destructor destroy; override;
  published
    { Published-Deklarationen }

    property Active: Boolean read FActive write SetActive;
    property Path: String read FPath write SetPath;
    property OnCreated: TFileChangedEvent read FOnCreated write FOnCreated;
    property OnDeleted: TFileChangedEvent read FOnDeleted write FOnDeleted;
    property OnModified: TFileChangedEvent read FOnModified write FOnModified;
    property OnRenamed: TFileRenamedEvent read FOnRenamed write FOnRenamed;
    property WatchSubtree: Boolean read FWatchSubTree write FWatchSubtree;
    property WatchFilters: TWatchfilters read FWatchFilters write FWatchFilters;

  end;

{$IFDEF FPC}
  function ReadDirectoryChangesW(hDirectory: HANDLE; lpBuffer: LPVOID;
    nBufferLength: DWORD; bWatchSubtree: BOOL; dwNotifyFilter: DWORD;
    lpBytesReturned: LPDWORD; lpOverlapped: LPOVERLAPPED;
    lpCompletionRoutine: LPOVERLAPPED_COMPLETION_ROUTINE): BOOL; stdcall; external 'kernel32' name 'ReadDirectoryChangesW';
  {$EXTERNALSYM ReadDirectoryChangesW}

  function CreateFile(lpFileName: PChar; dwDesiredAccess, dwShareMode: DWORD;
    lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
    hTemplateFile: THandle): THandle; stdcall; external 'kernel32' name 'CreateFileA';
{$ENDIF}

procedure Register;

//==============================================================================
implementation
//==============================================================================

uses
  {$IFNDEF FPC}
    ShlObj, FileCtrl,
  {$ENDIF}
  Math;

type
  // see windows API help
  PFileNotifyInformation = ^TFileNotifyInformation;
  TFileNotifyInformation = packed record
    NextEntryOffset: DWORD;
    Action: DWORD;
    FileNameLength: DWORD;
    FileName: array[0..0] of WideChar;
  end;

const
  FILE_LIST_DIRECTORY   = $0001;
  FILE_NOTIFY_CHANGE_CREATION         = $00000040;
  FILE_NOTIFY_CHANGE_LAST_ACCESS      = $00000020;

type
  TWaitThread = class(TThread)
  private
    FParent: TDirMon;
    FRenamedFrom: String;
    procedure HandleEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(AParent: TDirMon);
  end;

//------------------------------------------------------------------------------
// TWatchThread
//------------------------------------------------------------------------------
constructor TWaitThread.Create(AParent: TDirMon);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FParent := AParent;
end;

procedure TWaitThread.HandleEvent;
var
  FileOpNotification: PFileNotifyInformation;
  Offset: Longint;

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

  function GetFileName(const FileName: PWideChar; const FileNameLength: DWORD):string;
    var
      sResult : string;
      sFile   : string;
      iLen : integer;
      i : integer;
      wFilename : WideString;
  begin
    iLen := Floor(FileNameLength / SizeOf(WideChar));

    wFilename := StringToWide(FParent.FPath);

    for i := 0 to iLen-1 do
      wFilename := wFilename + WideChar(Filename[i]);

    sFile := Trim(FParent.FPath + WideCharLenToString(FileName, iLen));
    sResult := sFile;

    //if the filename has a different codepage return the short filename 8.3
    if (Pos('?',sResult)>0)
       or (WideString(StringToWide(sResult))
              <> wFilename) then
    begin
      sResult := Trim(WideExtractShortPathName(wFilename));

      if sResult = '' then
        sResult := sFile;
    end;

    result := sResult;
  end;
begin
  if Terminated then exit;

  with FParent do
  begin
    Pointer(FileOpNotification) := @FNotificationBuffer[0];
    repeat
      Offset := FileOpNotification^.NextEntryOffset;
      Case FileOpNotification^.Action of
        1: cmdCreated( FParent, GetFileName(@(FileOpNotification^.FileName[0]), FileOpNotification^.FileNameLength));
        2: cmdDeleted( FParent, GetFileName(@(FileOpNotification^.FileName[0]), FileOpNotification^.FileNameLength));
        3: cmdModified( FParent, GetFileName(@(FileOpNotification^.FileName[0]), FileOpNotification^.FileNameLength));
        4: FRenamedFrom := GetFileName(@(FileOpNotification^.FileName[0]), FileOpNotification^.FileNameLength); // Ausnahme
        5: cmdRenamed( FParent, FRenamedFrom, GetFileName(@(FileOpNotification^.FileName[0]), FileOpNotification^.FileNameLength));
      end;
      PChar(FileOpNotification) := PChar(FileOpNotification)+Offset;
    until Offset=0;
  end;
end;

procedure TWaitThread.Execute;
var
  numBytes: DWORD;
  CompletionKey: DWORD;
begin
  numBytes := 0;
  CompletionKey := 0;
  while not Terminated do
  begin
    if GetQueuedCompletionStatus(FParent.FCompletionPort, numBytes, CompletionKey, FParent.FPOverlapped, MAXINT) then
    begin
      if (CompletionKey <> 0) and not(Terminated) then
      begin
        Synchronize(HandleEvent);
        with FParent do
        begin
          FBytesWritten := 0;
          ZeroMemory(@FNotificationBuffer, SizeOf(FNotificationBuffer));
          ReadDirectoryChangesW(FDirectoryHandle, @FNotificationBuffer, SizeOf(FNotificationBuffer), FParent.WatchSubtree , FNotifyFilter, @FBytesWritten, @FOverlapped, nil);
        end;
      end
    end;
  end;
end;

//------------------------------------------------------------------------------
// TDirMon
//------------------------------------------------------------------------------
constructor TDirMon.Create( AOwner: TComponent);
begin
  inherited;
  FCompletionPort := 0;
  FDirectoryHandle := 0;
  FPOverlapped := @FOverlapped;
  ZeroMemory(@FOverlapped, SizeOf(FOverlapped));
  FWatchFilters:=[nfFILE_NAME,nfDIR_NAME,nfLAST_WRITE,nfCREATION];
end;

destructor TDirMon.destroy;
begin
  if FActive then Stop;
  inherited;
end;
procedure TDirMon.SetActive( AActive: Boolean);
begin
  if csDesigning in ComponentState then Exit;  // Don't start it in DesignerMode
  If AActive Then
    Start
  else
    Stop;
end;

procedure TDirMon.Start;
begin
  if FActive then Exit; // Don't start it again
  FNotifyFilter := 0;   // Set MyFilterArray->DWord-Filter in ReadDirectoryChanges
  if (nfFILE_NAME in FWatchFilters)
    then FNotifyFilter:=FNotifyFilter or FILE_NOTIFY_CHANGE_FILE_NAME;
  if (nfDIR_NAME in FWatchFilters)
    then FNotifyFilter:=FNotifyFilter or FILE_NOTIFY_CHANGE_DIR_NAME;
  if (nfATTRIBUTES in FWatchFilters)
    then FNotifyFilter:=FNotifyFilter or FILE_NOTIFY_CHANGE_ATTRIBUTES;
  if (nfSIZE in FWatchFilters)
    then FNotifyFilter:=FNotifyFilter or FILE_NOTIFY_CHANGE_SIZE;
  if (nfLAST_WRITE in FWatchFilters)
    then FNotifyFilter:=FNotifyFilter or FILE_NOTIFY_CHANGE_LAST_WRITE;
  if (nfLAST_ACCESS in FWatchFilters)
    then FNotifyFilter:=FNotifyFilter or FILE_NOTIFY_CHANGE_LAST_ACCESS;
  if (nfCREATION in FWatchFilters)
    then FNotifyFilter:=FNotifyFilter or FILE_NOTIFY_CHANGE_CREATION;
  if (nfSECURITY in FWatchFilters)
    then FNotifyFilter:=FNotifyFilter or FILE_NOTIFY_CHANGE_SECURITY;
  if FNotifyFilter = 0 then
     exit;

  FDirectoryHandle := CreateFile(PChar(FPath),
    FILE_LIST_DIRECTORY,
    FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
    nil,
    OPEN_EXISTING,
    FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED,
    0);

  if FDirectoryHandle = INVALID_HANDLE_VALUE then
  begin
    FDirectoryHandle := 0;
    raise EDirMonError.Create(SysErrorMessage(GetLastError));
    exit;
  end;
  FCompletionPort := CreateIoCompletionPort(FDirectoryHandle, 0, Longint(pointer(self)), 0);
  ZeroMemory(@FNotificationBuffer, SizeOf(FNotificationBuffer));
  FBytesWritten := 0;

  if not ReadDirectoryChangesW(FDirectoryHandle, @FNotificationBuffer, SizeOf(FNotificationBuffer), FWatchSubTree, FNotifyFilter, @FBytesWritten, @FOverlapped, nil) then
  begin
    {$IFDEF FPC}
      FileClose(FDirectoryHandle);
      FileClose(FCompletionPort);
    {$ELSE}
      CloseHandle(FDirectoryHandle);
      CloseHandle(FCompletionPort);
    {$ENDIF}
    FDirectoryHandle := 0;
    FCompletionPort := 0;

    raise EDirMonError.Create(SysErrorMessage(GetLastError));
    exit;
  end;
  FWatchThread := TWaitThread.Create(self); // The Thread is the Monitorig Thred
  TWaitThread(FWatchThread).Resume;

  FActive := True;
end;

procedure TDirMon.Stop;
begin
  if not FActive then Exit;
  if FCompletionPort = 0 then
    exit;
  FWatchThread.Suspend;
  FWatchThread.Terminate;
  {$IFDEF FPC}
    FileClose(FDirectoryHandle);
    FileClose(FCompletionPort);
  {$ELSE}
    CloseHandle(FDirectoryHandle);
    CloseHandle(FCompletionPort);
  {$ENDIF}
  FDirectoryHandle := 0;
  FCompletionPort := 0;
  FActive := False;
  //FWatchThread.Free;
end;

procedure TDirMon.cmdCreated( Sender: TObject; FileName: String);
begin
  if Assigned(FOnCreated) then FOnCreated(Sender,FileName);
end;

procedure TDirMon.cmdDeleted( Sender: TObject; FileName: String);
begin
  if Assigned(FOnDeleted) then FOnDeleted(Sender,FileName);
end;

procedure TDirMon.cmdModified( Sender: TObject; FileName: String);
begin
  if Assigned(FOnModified) then FOnModified(Sender,FileName);
end;

procedure TDirMon.cmdRenamed( Sender: TObject; fromFileName: String; toFileName: String);
begin
  if Assigned(FOnRenamed) then FOnRenamed(Sender,fromFileName,toFileName);
end;

procedure TDirMon.SetPath(aPath: String);
{$IFNDEF VER130}
function IncludeTrailingBackslash(const S: string): string;
begin
  if S[length(S)]='\' then result:=S else result:=S+'\';
end;
{$ENDIF}
begin
  {$IFDEF FPC}
     if DirectoryExistsUTF8(aPath) then
  {$ELSE}
     if DirectoryExists(aPath) then
  {$ENDIF}
    FPath:=IncludeTrailingBackslash(aPath);
  if FActive then // When You do this at RunTime - We stop and start the Monitoring Process
  begin
    Stop;
    start;
  end;
end;

//------------------------------------------------------------------------------
// registering the component TDirMon
//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('Win32', [TDirMon]);
end;

end.
