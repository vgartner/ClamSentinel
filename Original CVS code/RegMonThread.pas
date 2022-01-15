unit RegMonThread;

interface

uses
  Classes, Windows, Dialogs, SysUtils, Registry;

const
  RegistryMonitorFilter = REG_NOTIFY_CHANGE_NAME or
          REG_NOTIFY_CHANGE_ATTRIBUTES or
          REG_NOTIFY_CHANGE_LAST_SET   or
          REG_NOTIFY_CHANGE_SECURITY;

type TDiffList = packed record
       sKey : string;
       sVal : string;
     end;

type TArrayOfDiffList = array of TDiffList;

type
  TRegMonThread = class(TThread)
  private
    { Private declarations }

    FReg     : TRegistry;
    FEvent   : Integer;
    FKey     : string;
    FRootKey : HKey;
    FRootKeyString : string;
    FIdx : integer;

    aOriginalValues : TArrayOfDiffList;
    FListOriginalKeyNames : TStringList;

    procedure InitThread;
    procedure GetSnapshot(var list : TArrayOfDiffList; var keylist : TStringList);
    
  protected
    procedure Execute; override;

  public
    property Key: string   read FKey      write FKey;
    property RootKey: HKey read FRootKey  write FRootKey;
    property RootKeyString : string read FRootKeyString;
    property Idx : integer read FIdx write FIdx;

    procedure   DoSnapshot;
    function    FindDifferences : string;
    constructor Create;
    destructor  Destroy; Override;
  end;

implementation

function RootKeyToString(const RootKey : HKey) : string;
begin
  case RootKey of
     HKEY_CLASSES_ROOT     : Result := 'HKEY_CLASSES_ROOT';
     HKEY_CURRENT_USER     : Result := 'HKEY_CURRENT_USER';
     HKEY_LOCAL_MACHINE    : Result := 'HKEY_LOCAL_MACHINE';
     HKEY_USERS            : Result := 'HKEY_USERS';
     HKEY_PERFORMANCE_DATA : Result := 'HKEY_PERFORMANCE_DATA';
     HKEY_CURRENT_CONFIG   : Result := 'HKEY_CURRENT_CONFIG';
     HKEY_DYN_DATA         : Result := 'HKEY_DYN_DATA';
    else
      Result := 'UNKNOWN';
  end
end;

{ TRegMonThread }

constructor TRegMonThread.Create;
begin
  inherited Create(True); //Execute called after a resume

  FIdx := -1;
  FKey := '';
  FEvent := -1;
  FRootKeyString := '';
  FReg := TRegistry.Create;
  SetLength(aOriginalValues,0);
  FListOriginalKeyNames := TStringList.Create;
  
  FreeOnTerminate := true;
end;

destructor TRegMonThread.Destroy;
begin
  FReg.CloseKey;
  FReg.Free;
  SetLength(aOriginalValues,0);
  FListOriginalKeyNames.Free;

  inherited Destroy;
end;

procedure TRegMonThread.InitThread;
begin
  FReg.RootKey := FRootKey;
  FRootKeyString := RootKeyToString(FRootKey);

  if not FReg.OpenKeyReadOnly(FKey) then
  begin
    Terminate;
    exit;
  end;

  DoSnapshot;

  FEvent := CreateEvent(nil, True, False, PChar('RegMonitorChange'+inttostr(FIdx)));
  RegNotifyChangeKeyValue(FReg.CurrentKey, True, RegistryMonitorFilter, FEvent, True);
end;

procedure TRegMonThread.Execute;
begin
  InitThread;

  while not Terminated do
  begin
    if WaitForSingleObject(FEvent, INFINITE) = WAIT_OBJECT_0 then
    begin
      DoTerminate;

      if not Terminated then
      begin
        ResetEvent(FEvent);
        RegNotifyChangeKeyValue(FReg.CurrentKey, True, RegistryMonitorFilter, FEvent, True);
      end;
    end;
  end;
end;

procedure TRegMonThread.DoSnapshot;
begin
  GetSnapshot(aOriginalValues, FListOriginalKeyNames);
end;

procedure TRegMonThread.GetSnapshot(var list : TArrayOfDiffList; var keylist : TStringList);
var
  ListValues : TStringList;
  i : integer;
  sCurrValue : string;
  k : integer;
  j : integer;
  p : PChar;
  h : integer;
begin
  SetLength(list,0);
  keylist.Clear;

  FReg.GetKeyNames(keylist);

  ListValues := TStringList.Create;
  try
    FReg.GetValueNames(ListValues);

    for i := 0 to ListValues.Count-1 do
    begin
      sCurrValue := ListValues.Strings[i];

      if FReg.ValueExists(sCurrValue) then
      begin
        k := High(list)+1;
        SetLength(list,k+1);

        with list[k] do
        begin
          sKey := sCurrValue;

          case FReg.Getdatatype(sCurrValue) of
            rdString, rdExpandString:
              sVal := FReg.ReadString(sCurrValue);

            rdInteger:
              sVal := IntToHex(FReg.ReadInteger(sCurrValue),8);

            rdBinary:
            begin
              j := FReg.GetDataSize(sCurrValue);
              GetMem(p, j);
              FReg.ReadBinaryData(sCurrValue, p^, J);

              for h := 0 to j - 1 do
                sVal := sVal + IntToHex(Byte(p[h]), 2);

              FreeMem(p, j);
            end;
          end;
        end;
      end;
    end;
  finally
    ListValues.Free;
  end;
end;

function TRegMonThread.FindDifferences : string;
Const
  CRLF = #13#10;
  SEP : string = ' -> ';

var
  aNewValues : TArrayOfDiffList;
  ListNewKeyNames : TStringList;
  i : integer;
  j : integer;
  bFound : boolean;

  sModified : string;
  sAdded : string;
  sDeleted : string;
  sKeyDeleted : string;
  sKeyAdded : string;
  sResult : string;
  sKeyName : string;

  procedure AddString(var s : string; const stext : string);
  begin
    if s <> '' then
      s := s + CRLF;

    s := s + stext;
  end;

  procedure AddMsg(var smsg : string; const s : string);
  begin
    if s<>'' then
    begin
      if smsg<>'' then
        smsg := smsg + CRLF;

      smsg := smsg + s;
    end;
  end;
begin
  sResult := '';
  sModified := '';
  sAdded := '';
  sDeleted := '';
  sKeyDeleted := '';
  sKeyAdded := '';

  ListNewKeyNames := TStringList.Create;
  try
    GetSnapshot(aNewValues, ListNewKeyNames);
    //Keys added
    for i := 0 to ListNewKeyNames.Count-1 do
    begin
      sKeyName := ListNewKeyNames.Strings[i];

      if FListOriginalKeyNames.IndexOf(sKeyName) < 0 then
        AddString(sKeyAdded,'[+] ' + sKeyName);
    end;
    
    // keys deleted
    for i := 0 to FListOriginalKeyNames.Count-1 do
    begin
      sKeyName := FListOriginalKeyNames.Strings[i];

      if ListNewKeyNames.IndexOf(sKeyName) < 0 then
        AddString(sKeyDeleted,'[-] ' + sKeyName);
    end;

  finally
    ListNewKeyNames.Free;
  end;

  //Values added or modified
  for i := Low(aNewValues) to High(aNewValues) do
  begin
    bFound := false;

    with aNewValues[i] do
    begin
      j := Low(aOriginalValues);

      while not(bFound) and (j <= High(aOriginalValues)) do
      begin
        if sKey = aOriginalValues[j].sKey then
          begin
            bFound := true;

            if sVal <> aOriginalValues[j].sVal then
              AddString(sModified, '[<>] ' + sKey + SEP + sVal + '  [' + aOriginalValues[j].sVal + ']');
          end
        else
          Inc(j);
      end;

      if not bFound then
        AddString(sAdded,'[+] ' + sKey + SEP + sVal);
    end;
  end;

  //Values deleted
  for i := Low(aOriginalValues) to High(aOriginalValues) do
  begin
    bFound := false;

    with aOriginalValues[i] do
    begin
      j := Low(aNewValues);

      while not(bFound) and (j <= High(aNewValues)) do
      begin
        if sKey = aNewValues[j].sKey then
          bFound := true
        else
          Inc(j);
      end;

      if not bFound then
        AddString(sDeleted,'[-] ' + sKey + SEP + sVal);
    end;
  end;

  sResult := sModified;

  AddMsg(sResult, sAdded);
  AddMsg(sResult, sDeleted);

  AddMsg(sResult, sKeyAdded);
  AddMsg(sResult, sKeyDeleted);

  result := sResult;

  SetLength(aNewValues,1);
end;

end.
