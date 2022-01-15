unit UDrives;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, ExtCtrls;

const SEP : string = ' -> ';

type
  TFDrives = class(TForm)
    PanelBase: TPanel;
    chkListDrives: TCheckListBox;
    PanelBottom: TPanel;
    btnConfirm: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnConfirmClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure chkListDrivesClickCheck(Sender: TObject);
    procedure chkListDrivesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  private
    { Private declarations }
    procedure List_Drives(var lDrives : TStrings);
    function  CheckDrive(const sDrv : string): boolean;
    function  ExtractDrive(const sDrv : string): string;
    function  GetDriveDescription(const sDrv : string): string;
    procedure CheckAllFixedDrives;
    function  IsSystemDrive(const sDrv : string): boolean;
  public
    { Public declarations }
    sMsgRemovableDevice : string;
    sMsgFixedDevice : string;
    sMsgRemoteDevice : string;
    sMsgAbsentDevice : string;
    sMsgNotReadyDevice : string;
    
    bAddAllFixedDrives : boolean;
  end;

var
  FDrives: TFDrives;

implementation

uses Sentinel, Utility, Languages, Start, UVersion;

{$R *.DFM}

function TFDrives.IsSystemDrive(const sDrv : string): boolean;
begin
  result := AddSlash(UpperCase(sDrv)) = UpperCase(FClamSentinel.sSystemDrive)
end;

procedure TFDrives.CheckAllFixedDrives;
var
 i : integer;
 pDrive : PChar;
begin
  for i := 0 to (ChkListDrives.Items.Count - 1) do
  begin
    pDrive := PChar(UpperCase(ExtractDrive(ChkListDrives.Items[i])));

    if GetDriveType(pDrive) = DRIVE_FIXED then
      chkListDrives.Checked[i] := true;
  end;
end;

procedure TFDrives.List_Drives(var lDrives : TStrings);
const
  DRIVE_UNKNOWN = 0;
  DRIVE_ABSENT = 1;
  DRIVE_REMOVABLE = 2;
  DRIVE_FIXED = 3;
  DRIVE_REMOTE = 4;
  DRIVE_CDROM = 5;
  DRIVE_RAMDISK = 6;
var
  r: LongWord;
  Drives: array[0..128] of char;
  pDrive: PChar;
  sDesc : string;
begin
  lDrives.Clear;

  r := GetLogicalDriveStrings(SizeOf(Drives), Drives);
  if r = 0 then Exit;
  if r > SizeOf(Drives) then
    raise Exception.Create(SysErrorMessage(ERROR_OUTOFMEMORY));
  pDrive := Drives;
  while pDrive^ <> #0 do
  begin
    if UpperCase(pDrive) <> 'A:\' then
    begin
      if GetDriveType(pDrive) in [DRIVE_REMOVABLE, DRIVE_FIXED, DRIVE_REMOTE] then
      begin
        sDesc := GetDriveDescription(pDrive);

        if (sDesc <> '') then
          lDrives.Add(RemoveSlash(pDrive) + SEP + sDesc);
      end;
    end;
    Inc(pDrive, 4);
  end;
end;

function TFDrives.GetDriveDescription(const sDrv : string): string;
  var
    bNetShare : boolean;
begin
  result := '';
  bNetShare := false;

  if sDrv <> '' then
  begin
    case GetDriveType(PChar(sDrv)) of
      DRIVE_REMOVABLE: result := sMsgRemovableDevice;
      DRIVE_FIXED:     result := sMsgFixedDevice;
      DRIVE_REMOTE:    result := sMsgRemoteDevice;
      DRIVE_NO_ROOT_DIR:
        begin
         if copy(sDrv,1,2)='\\' then
         begin
           bNetShare := true;
           result := sMsgRemoteDevice;
         end;
        end;
    end;

    if not(DiskInDrive(sDrv[1])) and not(bNetShare) then
      result := result + '(' + sMsgNotReadyDevice + ')';
  end;
end;

function TFDrives.ExtractDrive(const sDrv : string): string;
var
  iPos : integer;
begin
  if Copy(sDrv,1,2)='\\' then
    begin
      iPos := Pos(SEP,sDrv);
      if iPos>0 then
        result := Copy(sDrv,1,iPos-1)      
      else
        result := sDrv;
    end
  else
    result := AddSlash(Trim(Copy(sDrv,1,Pos(':',sDrv))));
end;

function TFDrives.CheckDrive(const sDrv : string): boolean;
var
 i : integer;
 bFound : boolean;
begin
  i := 0;
  bFound := false;

  while not(bFound) and (i < ChkListDrives.Items.Count) do
  begin
    if UpperCase(sDrv) = UpperCase(ExtractDrive(ChkListDrives.Items[i])) then
      begin
        bFound := true;
        chkListDrives.Checked[i] := true;

        if IsSystemDrive(sDrv) then
          chkListDrives.State[i] := cbGrayed;
      end
    else
      inc(i);
  end;
  result := bFound;
end;

procedure TFDrives.FormCreate(Sender: TObject);
begin
  // DPI fix begin
  ClientWidth := PanelBase.ClientWidth;
  ClientHeight := PanelBase.ClientHeight;
  PanelBase.Align := alClient;
  // DPI fix end

  bAddAllFixedDrives := false;
  Caption := cProjectName + ' - Fixed disks monitored';
end;

procedure TFDrives.btnConfirmClick(Sender: TObject);
var
 i : integer;
 sDrive : string;
 iIndex : integer;
 lst : TStrings;
begin
  for i := 0 to (chkListDrives.Items.Count - 1) do
  begin
    sDrive := ExtractDrive(ChkListDrives.Items[i]);
    iIndex := IsInList(FClamSentinel.FixedDrivesToScan,sDrive);

    if chkListDrives.Checked[i] or (chkListDrives.State[i] = cbGrayed) then
      begin
        if iIndex < 0 then
          begin
            FClamSentinel.lstFixedDrivesToScan.Add(sDrive);
            FClamSentinel.FixedDrivesToScan.Add(sDrive);
          end
      end
    else
      begin
        if iIndex >= 0 then
        begin
          FClamSentinel.FixedDrivesToScan.Delete(iIndex);

          lst := TStringList.Create;
          try
            lst.Clear;
            lst.CommaText := ExpandEnvironment(FClamSentinel.lstFixedDrivesToScan.CommaText);

            iIndex := IsInList(lst,sDrive);

            if iIndex >= 0 then
            begin
              FClamSentinel.lstFixedDrivesToScan.Delete(iIndex);
            end;
          finally
            lst.Free;
          end;
        end;
      end;
  end;

  FClamSentinel.ModalResult := mrOk;
  Close;
end;

procedure TFDrives.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FDrives := nil;
  action := caFree;
end;

procedure TFDrives.btnCancelClick(Sender: TObject);
begin
  FClamSentinel.ModalResult := mrCancel;
  Close;
end;

procedure TFDrives.FormShow(Sender: TObject);
var
 lDrives : TStrings;
 i : integer;
 sDrive : string;
 sDesc : string;
begin
  SetLanguage_FDrives(FDrives);

  lDrives := TStringList.Create;
  try
    ChkListDrives.Clear;
    List_Drives(lDrives);
    ChkListDrives.Items.AddStrings(lDrives);
  finally
    lDrives.Free;
  end;

  FDrives.btnCancel.Enabled := not(bAddAllFixedDrives);

  if FClamSentinel.FixedDrivesToScan.Count > 0 then
    begin
      for i := 0 to (FClamSentinel.FixedDrivesToScan.Count - 1) do
      begin
        sDrive := FClamSentinel.FixedDrivesToScan[i];
        if sDrive<>'' then
        begin
          if not CheckDrive(sDrive) then
          begin
            sDesc := GetDriveDescription(sDrive);

            if sDesc = '' then
              sDesc := sMsgAbsentDevice;

            ChkListDrives.items.Add(RemoveSlash(sDrive) + SEP + sDesc);
            CheckDrive(sDrive);
          end;
        end;
      end;
    end
  else
    CheckDrive(FClamSentinel.sSystemDrive);

  if bAddAllFixedDrives then
    CheckAllFixedDrives;
end;

procedure TFDrives.chkListDrivesClickCheck(Sender: TObject);
var
  sDrv : string;
  i : integer;
begin
  i := chkListDrives.ItemIndex;
  sDrv := ExtractDrive(chkListDrives.Items[i]);

  if IsSystemDrive(sDrv) then
    CheckDrive(sDrv);
end;

procedure TFDrives.chkListDrivesDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with chkListDrives.Canvas do
  begin
    if FClamSentinel.NewDrives.IndexOf(ExtractDrive(chkListDrives.Items[Index]))>=0 then
      Font.Color:=clGreen;

    FillRect(Rect);
    TextOut(Rect.Left, Rect.Top, chkListDrives.Items[Index]);
  end;
end;

end.
