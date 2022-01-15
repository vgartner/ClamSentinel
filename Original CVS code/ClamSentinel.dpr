program ClamSentinel;

uses
  windows,
  Forms,
  ShellApi,
  IniFiles,
  Sentinel in 'Sentinel.pas' {FClamSentinel},
  ScanThds in 'ScanThds.pas',
  TrayIcon in 'TrayIcon.pas',
  RunRegistry in 'RunRegistry.pas',
  Utility in 'Utility.pas',
  DeviceChange in 'DeviceChange.pas',
  VxdThds in 'VxdThds.pas',
  NewIniFile in 'NewIniFile.pas',
  UDrives in 'UDrives.pas' {FDrives},
  UChangeList in 'UChangeList.pas' {FChangeList},
  Languages in 'Languages.pas',
  UClamWinScanner in 'UClamWinScanner.pas',
  UElabList in 'UElabList.pas',
  RegMonThread in 'RegMonThread.pas',
  CheckMalwareThds in 'CheckMalwareThds.pas',
  Sign in 'Sign.pas',
  Start in 'Start.pas',
  UVersion in 'UVersion.pas',
  VerifyPEFormat in 'VerifyPEFormat.pas';

var
  bTerminate : boolean;
  NS : integer = 0;
  IniFile : TIniFile;

{$R *.RES}

begin
  bTerminate := true;

  Application.Initialize;
  Application.Title := 'Clam Sentinel';

  if (FindWindow('TFClamSentinel',nil)=0)
        or (FindWindow('TAppBuilder', nil)>0) then
  begin
    IniFile := TIniFile.Create(GetIniFilePath(cProjectFileName));
    try
      NS := IniFile.ReadInteger('Params', 'NS', 0);
    finally
      IniFile.Free;
    end;

    if not(NS=2) then
      InitScanner;

    Application.CreateForm(TFClamSentinel, FClamSentinel);
  if FClamSentinel.bStartApplication then
      begin
        if not(NS=0) then
        begin
          FClamSentinel.bSkipOn := true;
          FClamSentinel.StartStopIcon(FClamSentinel.FSDetectionActive);
        end;

        Application.ShowMainForm := false;
        Application.Run;
        bTerminate := false;
      end;
  end;

  if bTerminate then
  begin
    DeleteIcon;
    Application.Terminate;
  end;
end.
