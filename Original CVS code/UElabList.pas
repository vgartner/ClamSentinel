unit UElabList;

interface

uses SysUtils;

type
  FileElabInfo = packed record
    sFileElab : string;
    TimeElab : TDateTime;
  end;

 TFileElabInfo = record
   bWait : boolean;
   aElab : array of FileElabInfo;
 end;

    function  IsIntoElabList(var ElabList : TFileElabInfo; const sFile : string): boolean;
    procedure CleanElabList(var ElabList : TFileElabInfo);
    procedure AddIntoElabList(var ElabList : TFileElabInfo; const sFile : string);
    procedure WaitIfInUseElabList(var ElabList : TFileElabInfo);
    procedure ResetElabList(var ElabList : TFileElabInfo);
    procedure RemoveFromElabList(var ElabList : TFileElabInfo; const sFile : string);

implementation

uses Utility, Sentinel, Windows, Forms;

procedure AddIntoElabList(var ElabList : TFileElabInfo;
                                               const sFile : string);
var
 i : Integer;
begin
  i := Length(ElabList.aElab);

  SetLength(ElabList.aElab,i+1);

  with ElabList.aElab[i] do
  begin
    sFileElab := UpperCase(sFile);
    TimeElab := Now;
  end;
end;

procedure CleanElabList(var ElabList : TFileElabInfo);
var
  i : integer;
  iNew : integer;
  TempElabList : TFileElabInfo;
begin

WaitIfInUseElabList(ElabList);

ElabList.bWait := true;

try
  SetLength(TempElabList.aElab,0);

  i := Low(ElabList.aElab);

  while i<=High(ElabList.aElab) do
  begin
    if SecondsDiff(ElabList.aElab[i].TimeElab,now) <= MAX_SECONDS_NO_RESCAN then
    begin
      iNew := Length(TempElabList.aElab);

      SetLength(TempElabList.aElab,iNew+1);

      TempElabList.aElab[iNew] := ElabList.aElab[i];
    end;

    Inc(i);
  end;

  SetLength(ElabList.aElab,0);
  SetLength(ElabList.aElab,High(TempElabList.aElab)+1);

  for i := Low(TempElabList.aElab) to High(TempElabList.aElab) do
    ElabList.aElab[i] := TempElabList.aElab[i];

finally
  ElabList.bWait := false;
end;
end;

function IsIntoElabList(var ElabList : TFileElabInfo;
                                                const sFile : string): boolean;
var
  bFound : boolean;
  i : integer;
begin

CleanElabList(ElabList);

WaitIfInUseElabList(ElabList);

ElabList.bWait := true;
bFound := false;

try
  i := Low(ElabList.aElab);

  while not(bFound) and (i<=High(ElabList.aElab)) do
  begin
    if ElabList.aElab[i].sFileElab = UpperCase(sFile) then
      bFound := true
    else
      Inc(i);
  end;

finally
  ElabList.bWait := false;
end;

  Result := bFound;
end;

procedure WaitIfInUseElabList(var ElabList : TFileElabInfo);
begin
  while ElabList.bWait do
  begin
    sleep(5);
    Application.ProcessMessages;
  end;
end;

procedure ResetElabList(var ElabList : TFileElabInfo);
begin
  SetLength(ElabList.aElab,0);
end;

procedure RemoveFromElabList(var ElabList : TFileElabInfo;
                                                const sFile : string);
var
  bFound : boolean;
  i : integer;
begin

CleanElabList(ElabList);

WaitIfInUseElabList(ElabList);

ElabList.bWait := true;
bFound := false;

try
  i := Low(ElabList.aElab);

  while not(bFound) and (i<=High(ElabList.aElab)) do
  begin
    if ElabList.aElab[i].sFileElab = UpperCase(sFile) then
      begin
        bFound := true;
        ElabList.aElab[i].sFileElab := '';
      end
    else
      Inc(i);
  end;

finally
  ElabList.bWait := false;
end;
end;

end.
