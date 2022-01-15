unit LoadVxd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, VxdThds;

const VXD_INTERVAL = 500;

type
  TFTest = class(TForm)
    Memo: TMemo;
    PanelTop: TPanel;
    ButtonClose: TButton;
    ButtonOpen: TButton;
    ButtonClear: TButton;
    chkLongPath: TCheckBox;
    procedure ButtonOpenClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    VxdThread : TVxdThread;

    procedure ThreadVxdDone(Sender: TObject);
    procedure AddLine(const s : string);
  public
    { Public declarations }
  end;

var
  FTest: TFTest;

function GetLongPathName(lpszShortPath: PChar; lpszLongPath: PChar;
                cchBuffer: DWORD): DWORD; stdcall;

implementation

{$R *.DFM}

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

procedure TFTest.AddLine(const s : string);
begin
  if Memo.Lines.Count > 515 then
    Memo.Lines.Delete(Memo.Lines.Count-1);

  Memo.Lines.Insert(0,s);
end;

procedure TFTest.ThreadVxdDone(Sender: TObject);
    function Explode(var s : string): string;
      var
        iPos : integer;
    begin
      iPos := Pos('|',s);
     if iPos > 0 then
        begin
          Result := Copy(s,1,iPos-1);
          s := Copy(s,iPos+1,MAXINT);
        end
      else
        begin
          Result := s;
          s := '';
        end;
    end; //end Explode
var
 sNext : string;
 sFile : string;
 sFileLong : string;
begin
  if VxdThread = nil then
    exit;

  sNext := TVxdThread(Sender).Filename;

  While sNext <> '' do
  begin
    sFile := Explode(sNext);

    if sFile <> '' then
    begin
      if chkLongPath.Checked then
        sFileLong := WinAPI_GetLongPathName(sFile);

      if sFileLong <> '' then
        sFile := sFileLong;

      AddLine(sFile);
    end;
  end;
end;

procedure TFTest.ButtonOpenClick(Sender: TObject);
begin
  Memo.Clear;
  ButtonOpen.Enabled := false;
  ButtonClose.Enabled := true;

  AddLine('Open');
  SetCurrentDir(ExtractFilePath(Application.ExeName));
  VxdThread := TVxdThread.Create(VXD_INTERVAL);
  VxdThread.OnTerminate := ThreadVxdDone;
end;

procedure TFTest.ButtonCloseClick(Sender: TObject);
begin
 if (VxdThread <> nil) then
 begin
    AddLine('Close');
    ButtonClose.Enabled := false;

    VxdThread.OnTerminate := nil;
    VxdThread.Terminate;
    AddLine('Terminated');

    VxdThread := nil;

    ButtonOpen.Enabled := true;
 end;
end;

procedure TFTest.ButtonClearClick(Sender: TObject);
begin
  Memo.Clear;
end;

procedure TFTest.FormCreate(Sender: TObject);
begin
  VxdThread := nil;
end;

procedure TFTest.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ButtonCloseClick(Sender);
  Sleep(VXD_INTERVAL);
  Application.ProcessMessages;
end;

end.
