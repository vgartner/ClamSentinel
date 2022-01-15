program PDirMon;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFNDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  uDirMon in 'uDirMon.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
