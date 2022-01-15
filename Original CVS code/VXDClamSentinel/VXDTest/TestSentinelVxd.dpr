program TestSentinelVxd;

uses
  Forms,
  LoadVxd in 'LoadVxd.pas' {FTest},
  VxdThds in 'VxdThds.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFTest, FTest);
  Application.Run;
end.
