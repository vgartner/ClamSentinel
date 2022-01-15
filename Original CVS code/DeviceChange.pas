unit DeviceChange;
//By Andrea Russo - Italy

{$I DelphiVersion.inc}

interface

uses
  {$IFDEF DELPHI_6_UP}
    Classes,
  {$ENDIF}
    Windows, Messages, SysUtils, Forms;

type
  TDeviceChangedEvent = procedure( Sender: TObject; sUnit: String) of Object;

  TDeviceChange = class(TObject)
  private
    FDirectoryHandle: THandle;
    FNotificationPointer : Pointer;
    FUnitPath : string;
    FWindowHandle : HWND;

    FOnQueryRemove: TDeviceChangedEvent;

    procedure WndProc(var Msg: TMessage);

    procedure RegisterForDeviceChange(const Handle : THandle;
                  const sUnit : string; var pNotify : Pointer;
                  var hDir : THandle);

    procedure UnregisterForDeviceChange(var r : Pointer);

  protected
    procedure WMDeviceChange(var Msg: TMessage);

  public
    constructor Create(const sDrivePath : string);
    destructor  Destroy; override;

  published
    property UnitPath: String read FUnitPath;
    property OnQueryRemove: TDeviceChangedEvent read FOnQueryRemove write FOnQueryRemove;
  end;

implementation

const DBT_DEVICEARRIVAL           = $8000; // system detected a new device
      DBT_DEVICEQUERYREMOVE       = $8001; // wants to remove, may fail
      DBT_DEVICEQUERYREMOVEFAILED = $8002; // removal aborted
      DBT_DEVICEREMOVEPENDING     = $8003; // about to remove, still avail
      DBT_DEVICEREMOVECOMPLETE    = $8004; // device is gone
      DBT_DEVICETYPESPECIFIC      = $8005; // type specific event
      DBT_DEVTYP_VOLUME           = $0002; // Logical volume
      DBTF_MEDIA                  = $0001; // change affects media in drive
      DBTF_NET                    = $0002; // logical volume is network volume

      DBT_DEVTYP_HANDLE = $00000006;

type PDevBroadcastHdr = ^TDevBroadcastHdr;
     TDevBroadcastHdr = packed record
       dbcd_size       : DWord;
       dbcd_devicetype : DWord;
       dbcd_reserved   : DWord;
     end;

     PDEV_BROADCAST_HANDLE = ^DEV_BROADCAST_HANDLE;
      _DEV_BROADCAST_HANDLE = record
       dbch_size: DWORD;
       dbch_devicetype: DWORD;
       dbch_reserved: DWORD;
       dbch_handle: THandle;
       dbch_hdevnotify: Pointer;
       dbch_eventguid: TGUID;
       dbch_nameoffset: Longint;
       dbch_data: array [0..0] of BYTE;
     end;
     DEV_BROADCAST_HANDLE = _DEV_BROADCAST_HANDLE;

//------------------------------------------------------------------------------
// TDeviceChange
//------------------------------------------------------------------------------
constructor TDeviceChange.Create(const sDrivePath : string);
begin
  inherited Create;

  {$IFDEF DELPHI_6_UP}
    FWindowHandle := Classes.AllocateHWnd(WndProc);
  {$ELSE}
    FWindowHandle := AllocateHWnd(WndProc);
  {$ENDIF}

  FUnitPath := sDrivePath;
  FDirectoryHandle := 0;
  RegisterForDeviceChange(FWindowHandle,FUnitPath,FNotificationPointer,FDirectoryHandle);
end;

destructor TDeviceChange.Destroy;
begin
  if FDirectoryHandle <> 0 then
    CloseHandle(FDirectoryHandle);

  FDirectoryHandle := 0;

  if Assigned(FNotificationPointer) then
    UnregisterForDeviceChange(FNotificationPointer);

  {$IFDEF DELPHI_6_UP}
    Classes.DeallocateHWnd(FWindowHandle);
  {$ELSE}
    DeallocateHWnd(FWindowHandle);
  {$ENDIF}

  inherited Destroy;
end;

procedure TDeviceChange.RegisterForDeviceChange(const Handle : THandle;
                  const sUnit : string; var pNotify : Pointer;
                  var hDir : THandle);
var
  dbi: DEV_BROADCAST_HANDLE;
  Size: Integer;
begin
  pNotify := nil;

  Size := SizeOf(DEV_BROADCAST_HANDLE);
  ZeroMemory(@dbi, Size);
  dbi.dbch_size := Size;
  dbi.dbch_devicetype := DBT_DEVTYP_HANDLE;
  dbi.dbch_reserved := 0;

  hDir := CreateFile(PChar(sUnit),GENERIC_READ,FILE_SHARE_READ or FILE_SHARE_WRITE
     or FILE_SHARE_DELETE,nil,OPEN_EXISTING,FILE_FLAG_BACKUP_SEMANTICS,0);

  if hDir = INVALID_HANDLE_VALUE then
  begin
    hDir := 0;
    exit;
  end;

  dbi.dbch_handle := hDir;

  pNotify := RegisterDeviceNotification(Handle, @dbi,
    DEVICE_NOTIFY_WINDOW_HANDLE);
end;

procedure TDeviceChange.UnregisterForDeviceChange(var r : Pointer);
begin
  UnregisterDeviceNotification(r);
  r := nil;
end;

procedure TDeviceChange.WndProc(var Msg: TMessage);
begin
  if (Msg.Msg = WM_DEVICECHANGE) then
  begin
    try
      WMDeviceChange(Msg);
    except
      Application.HandleException(Self);
    end;
  end else
    Msg.Result := DefWindowProc(FWindowHandle, Msg.Msg, Msg.wParam, Msg.lParam);
end;

procedure TDeviceChange.WMDeviceChange(var Msg: TMessage);
begin
  if Msg.lParam <> 0 then
  if PDevBroadcastHdr(Msg.lParam)^.dbcd_devicetype in [DBT_DEVTYP_HANDLE] then
  begin
    case Msg.wParam of
      DBT_DEVICEARRIVAL:
        ;
      DBT_DEVICEREMOVECOMPLETE:
        ;
      DBT_DEVICEQUERYREMOVE:
        if Assigned(FOnQueryRemove) then FOnQueryRemove(Self,FUnitPath);
      DBT_DEVICEQUERYREMOVEFAILED:
        ;
      DBT_DEVICEREMOVEPENDING:
        ;
    end;
  end;
end;

end.
