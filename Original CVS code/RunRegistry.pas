unit RunRegistry;

interface

uses Windows, Dialogs, Registry;

const
 KEY_RUN = 'Software\Microsoft\Windows\CurrentVersion\Run';
 HKCU = HKEY_CURRENT_USER;
 HKLM = HKEY_LOCAL_MACHINE;

function  RunKeyFound(ApTitle : string; bHKCU : boolean = TRUE): boolean;
function  GetRunKey(ApTitle : string; bHKCU : boolean = TRUE): string;
procedure RemoveFromRunKey(ApTitle: string; bHKCU : boolean = TRUE);
procedure RunOnWinStart(ApTitle: string; ApPathFile: string; RunOnce: Boolean
               ; bHKCU : boolean = TRUE);

implementation

function GetRunKey(ApTitle: string; bHKCU : boolean = TRUE): string;
var
  Reg: TRegistry;
begin
  result := '';

  Reg := TRegistry.Create;

  if bHKCU then
    Reg.RootKey := HKCU
  else
    Reg.RootKey := HKLM;

  if Reg.OpenKeyReadOnly(KEY_RUN) then
  begin
    if Reg.ValueExists(ApTitle) then
      result := Reg.ReadString(ApTitle);
  end;

  Reg.CloseKey;
  Reg.Free;
end;

function RunKeyFound(ApTitle: string; bHKCU : boolean = TRUE): boolean;
var
  Reg: TRegistry;
begin
  result := false;

  Reg := TRegistry.Create;

  if bHKCU then
    Reg.RootKey := HKCU
  else
    Reg.RootKey := HKLM;

  if Reg.OpenKeyReadOnly(KEY_RUN) then
    result := Reg.ValueExists(ApTitle);

  Reg.CloseKey;
  Reg.Free;
end;

procedure RunOnWinStart(ApTitle : string; ApPathFile: string;
  RunOnce: Boolean; bHKCU : boolean = TRUE);
var
  Reg: TRegistry;
  TheKey: string;
begin
  Reg := TRegistry.Create;

  if bHKCU then
    Reg.RootKey := HKCU
  else
    Reg.RootKey := HKLM;

  TheKey := KEY_RUN;
  if RunOnce then TheKey := TheKey + 'Once';
  // Open key, or create it if it doesn't exist
  Reg.OpenKey(TheKey, True);
  Reg.WriteString(ApTitle, ApPathFile);
  Reg.CloseKey;
  Reg.Free;
  //ShowMessage(ApTitle + ' has been set to run on system startup');
end;

procedure RemoveFromRunKey(ApTitle: string; bHKCU : boolean = TRUE);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;

  if bHKCU then
    Reg.RootKey := HKCU
  else
    Reg.RootKey := HKLM;

  // Check if key exist...
  // ...if yes, try to delete the entry for ApTitle
  if not Reg.OpenKey(KEY_RUN, False) then
    ShowMessage('Key not found')
  else begin
    if not Reg.DeleteValue(ApTitle) then
    //  ShowMessage(ApTitle + ' will no longer run on system startup')
    //else
      ShowMessage('Not found: ' + ApTitle);
  end;
  Reg.CloseKey;
  Reg.Free;
end;

end.
