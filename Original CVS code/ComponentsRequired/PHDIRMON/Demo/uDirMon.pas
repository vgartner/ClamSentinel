unit uDirMon;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DirMon, ExtCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Memo1: TMemo;
    btnStartStop: TButton;
    edDirectory: TEdit;
    Label1: TLabel;
    chkIncludeSubTree: TCheckBox;
    DirMon1: TDirMon;
    procedure DirMon1Created(Sender: TObject; FileName: String);
    procedure DirMon1Deleted(Sender: TObject; FileName: String);
    procedure DirMon1Modified(Sender: TObject; FileName: String);
    procedure DirMon1Renamed(Sender: TObject; fromFileName,
      toFileName: String);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnStartStopClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.DirMon1Created(Sender: TObject; FileName: String);
begin
  Memo1.Lines.Add('CREATED '+FileName);
end;

procedure TForm1.DirMon1Deleted(Sender: TObject; FileName: String);
begin
  Memo1.Lines.Add('Deleted '+DirMon1.Path + FileName);
end;

procedure TForm1.DirMon1Modified(Sender: TObject; FileName: String);
begin
  Memo1.Lines.Add('Modified '+FileName);
end;

procedure TForm1.DirMon1Renamed(Sender: TObject; fromFileName,
  toFileName: String);
begin
  Memo1.Lines.Add('Renamed von '+fromFileName+' in '+ToFileName);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  dirMon1.Active:=False;
end;

procedure TForm1.btnStartStopClick(Sender: TObject);
begin
  if DirMon1.active then
  begin
    btnStartStop.Caption:='&Start';
    DirMon1.active:=false;
  end else
  begin
    btnStartStop.Caption:='&Stop';
    dirMon1.WatchSubtree:=chkIncludeSubTree.Checked;
    dirMon1.Path:=edDirectory.text;
    dirMon1.active:=true;
  end;
end;

end.
  