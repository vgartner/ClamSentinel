program SentinelRecover;

uses
  Forms,
  URecover in 'URecover.pas' {FRecover},
  Utility in 'Utility.pas',
  Languages in 'Languages.pas',
  Sign in '..\Sign.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFRecover, FRecover);
  Application.Run;
end.
