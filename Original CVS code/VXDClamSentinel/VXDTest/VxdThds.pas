//Interface for the VXD driver Sentinel.vxd used only if the operative system is Win98
unit VxdThds;

interface

uses
  Classes, Windows, Dialogs, SysUtils;

const
INSTALL_HOOK = 1;
GET_OUTPUT =   2;

type
  TVxdThread = class(TThread)
  private
    { Private declarations }

    FFilename : string;
    FSleepTime : DWord;

    ghDevice: tHandle;
    nb: Cardinal;

    procedure GetInfo;
    procedure GetLoop;

  protected
    procedure Execute; override;

  public
    property Filename : string read FFilename;
    property SleepTime : DWORD write FSleepTime;

    constructor Create(const SleepTime : DWORD);
    destructor  Destroy; Override;

    procedure InstallHook;
  end;

implementation

{ TVxdThread }

constructor TVxdThread.Create(const SleepTime : DWORD);
begin
  FFilename := '';
  FSleepTime := SleepTime;

  ghDevice := INVALID_HANDLE_VALUE;

  FreeOnTerminate := True;

  inherited Create(false);
end;

procedure TVxdThread.Execute;
begin
  { Place thread code here }

  ghDevice := INVALID_HANDLE_VALUE;
  FFilename := '';

  ghDevice:=CreateFile('\\.\SENTINEL.VXD', 0, 0, nil, 0,
    FILE_FLAG_OVERLAPPED or FILE_FLAG_DELETE_ON_CLOSE, 0);

  if ghDevice = INVALID_HANDLE_VALUE then
  begin
    MessageBox(0, 'Error loading VXD file', 'Error', mb_IconExclamation + mb_ok);
    Terminate;
    exit;
  end;

  // Tell driver to start install the hook and start the loop
  InstallHook;
end;

destructor TVxdThread.Destroy;
begin
  FFilename := '';

  if (ghDevice <> INVALID_HANDLE_VALUE) then
  begin
    CloseHandle(ghDevice);
    ghDevice := INVALID_HANDLE_VALUE;
  end;

  inherited Destroy;
end;

procedure TVxdThread.InstallHook;
begin
  // Tell driver to start install the hook
  if (ghDevice <> INVALID_HANDLE_VALUE) then
  begin
    if not DeviceIoControl(ghDevice, INSTALL_HOOK, nil, 0, nil, 0, nb, nil) then
    begin
      MessageBox(0,'Couldn''t access device driver', 'Error', mb_IconExclamation + mb_ok);
      Terminate;
      exit;
    end;

    //Start the loop
    GetLoop;
  end;
end;

procedure TVxdThread.GetLoop;
begin
  repeat
    GetInfo;
  until Terminated or (ghDevice = INVALID_HANDLE_VALUE);
end;

procedure TVxdThread.GetInfo;
var
 Stats : array[0..256*MAX_PATH-1] of char;
 StatLen : Cardinal;
begin

  FillChar(Stats, SizeOf(Stats), #0);
  StatLen := 0;
  FFilename := '';

  StatLen:=0;

  if not(Terminated) and (ghDevice <> INVALID_HANDLE_VALUE) then
  begin
    if not DeviceIoControl(ghDevice, GET_OUTPUT
                           , nil, 0, @Stats, SizeOf(Stats), StatLen, nil) then
    begin
      MessageBox(0,'Couldn''t access device driver', 'Error', mb_IconExclamation + mb_ok);
      Terminate;
      exit;
    end;

    if (StatLen > 0) then
    begin
      FFilename := PChar(@Stats[0]);
      DoTerminate;
    end;

    Sleep(FSleepTime);
  end;
end;

end.
