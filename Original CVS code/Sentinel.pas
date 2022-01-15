unit Sentinel;

{$I DelphiVersion.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, Buttons, ShellApi,
  DirMon, TrayIcon, ShellNotify, ExtCtrls, DeviceChange, CheckMalwareThds,
  VxdThds, UElabList, RegMonThread, Start, UVersion, Sign;

const
  LOGFILE_DRIVE_SCAN =    cProjectFileName+'_DriveAddLog.txt';
  LOGFILE_MEMORY_SCAN =   cProjectFileName+'_MemoryScanLog.txt';
  LOGFILE_REALTIME_SCAN = cProjectFileName+'_RealTimeLog.txt';
  LOGFILE_MESSAGES =      cProjectFileName+'_MessagesLog.txt';
  LOGFILE_QUARANTINE =    cProjectFileName+'_QuarantineLog.txt';

  BAT_MEMORY_SCAN = 'MemoryScan.bat';
  BAT_DRIVE_SCAN = 'DriveScan.bat';

  SCANNER_CONF_FILE : string = '';

  MAX_CLAMSCAN_FILES = 10;

  MAX_SECONDS_NO_RESCAN = 3;

  MAX_LOG_SIZE_MB = 5;

  VXD_INTERVAL = 500; //only if OS is Win98

  TIMER_LONG_INTERVAL = 86400000; //24 hours
  TIMER_SHORT_INTERVAL = 900000;  //15 minutes
  TIMER_FIRST_INTERVAL = 300000;  //5 minutes

  LANG_AZERI                        = $2c;
  LANG_TURKISH                      = $1F;
  LANG_UZBEK                        = $43;
  
type LANGANDCODEPAGE = packed record
      wLanguage :Word;
      wCodePage :Word;
     end;

type
  TFileVersionInfo = packed record
    bVersionInfo : boolean;
    CompanyName,
    FileDescription,
    FileVersion,
    InternalName,
    LegalCopyRight,
    LegalTradeMarks,
    OriginalFileName,
    ProductName,
    ProductVersion,
    Comments : string;
    CompiledScript : string;
    PrivateBuild : string;
    SpecialBuild : string;
    Language : Word;
    CodePage : Word;
    CodePagePossible : Word;
  end;
    
type TDetectType = (detect_None, detect_Virus, detect_FalsePositive);

type TInfectedFileOp = (opMoveToQuarantine, opReportOnly);

type
  TFClamSentinel = class(TForm)
    ShellNotify: TShellNotify;
    PopupMenuTrayIcon: TPopupMenu;
    mnuSettings_RunOnStartup: TMenuItem;
    mnuSettings: TMenuItem;
    N5: TMenuItem;
    mnuMemoryScan: TMenuItem;
    SepMemoryScan: TMenuItem;
    mnuQuarantine: TMenuItem;
    mnuOpen: TMenuItem;
    mnuRealTimeLog: TMenuItem;
    mnuMemoryLog: TMenuItem;
    mnuDriveScanLog: TMenuItem;
    N3: TMenuItem;
    mnuStart: TMenuItem;
    mnuStop: TMenuItem;
    N4: TMenuItem;
    mnuAbout: TMenuItem;
    N1: TMenuItem;
    mnuExit: TMenuItem;
    mnuClamSentinelWebsite: TMenuItem;
    mnuSettings_DetectNewDrives: TMenuItem;
    mnuSettings_MemoryScanAtStartup: TMenuItem;
    mnuSettings_Log: TMenuItem;
    mnuSettings_SepStartup: TMenuItem;
    mnuSettings_AskForScanNewDrives: TMenuItem;
    mnuCheckVersion: TMenuItem;
    mnuAdvancedSettings_UseVxdOnWin98: TMenuItem;
    mnuAdvancedSettings_Drives: TMenuItem;
    mnuAdvancedSettings_NoScan: TMenuItem;
    mnuAdvancedSettings_Extensions: TMenuItem;
    mnuAdvancedSettings_LogMaxSize: TMenuItem;
    mnuSettings_AdvancedSettings: TMenuItem;
    mnuAdvancedSettings_MaxActiveScan: TMenuItem;
    mnuSettings_InfectedFiles: TMenuItem;
    mnuInfectedFiles_ReportOnly: TMenuItem;
    mnuInfectedFiles_MoveToQuarantine: TMenuItem;
    mnuMessagesLog: TMenuItem;
    TimerCheckVersion: TTimer;
    mnuAdvancedSettings_FullScan: TMenuItem;
    mnuSettings_MonitorSystemChanges: TMenuItem;
    mnuSettings_NotifyNewVersion: TMenuItem;
    mnuSettings_MonitorSystemChanges_DetectionOnly: TMenuItem;
    mnuSettings_MonitorSystemChanges_DetectionWithWarnings: TMenuItem;
    mnuSettings_MonitorSystemChanges_NoDetection: TMenuItem;
    mnuQuarantineLog: TMenuItem;
    mnuQuarantineTools: TMenuItem;
    mnuOpenRecover: TMenuItem;
    N2: TMenuItem;
    mnuSettings_MonitorSystemChanges_SkipFilesWithValidSignature: TMenuItem;
    mnuDownloadDBUpdate: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ShellNotifyNotify(Sender: TObject; Event: TShellNotifyEvent;
      Path1, Path2: String);
    procedure mnuStartClick(Sender: TObject);
    procedure mnuStopClick(Sender: TObject);
    procedure mnuMemoryScanClick(Sender: TObject);
    procedure PopupMenuTrayIconPopup(Sender: TObject);
    procedure DirMonCreated(Sender: TObject; FileName: String);
    procedure DirMonModified(Sender: TObject; FileName: String);
    procedure DirMonRenamed(Sender: TObject; fromFileName,
      toFileName: String);

    procedure OnDeviceQueryRemove(Sender: TObject; sUnit: String);
    procedure mnuQuarantineClick(Sender: TObject);
    procedure mnuMemoryLogClick(Sender: TObject);
    procedure mnuRealTimeLogClick(Sender: TObject);
    procedure mnuDriveScanLogClick(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure mnuSettings_RunOnStartupClick(Sender: TObject);
    procedure mnuClamSentinelWebsiteClick(Sender: TObject);
    procedure mnuSettings_DetectNewDrivesClick(Sender: TObject);
    procedure mnuSettings_MemoryScanAtStartupClick(Sender: TObject);
    procedure mnuSettings_LogClick(Sender: TObject);
    procedure mnuSettings_AskForScanNewDrivesClick(Sender: TObject);
    procedure mnuCheckVersionClick(Sender: TObject);
    procedure mnuAdvancedSettings_UseVxdOnWin98Click(Sender: TObject);
    procedure mnuAdvancedSettings_DrivesClick(Sender: TObject);
    procedure mnuAdvancedSettings_NoScanClick(Sender: TObject);
    procedure mnuAdvancedSettings_ExtensionsClick(Sender: TObject);
    procedure mnuAdvancedSettings_LogMaxSizeClick(Sender: TObject);
    procedure mnuAdvancedSettings_MaxActiveScanClick(Sender: TObject);
    procedure mnuInfectedFiles_MoveToQuarantineClick(Sender: TObject);
    procedure mnuInfectedFiles_ReportOnlyClick(Sender: TObject);
    procedure mnuMessagesLogClick(Sender: TObject);
    procedure TimerCheckVersionTimer(Sender: TObject);
    procedure mnuAdvancedSettings_FullScanClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure mnuSettings_NotifyNewVersionClick(Sender: TObject);
    procedure mnuSettings_MonitorSystemChanges_DetectionWithWarningsClick(
      Sender: TObject);
    procedure mnuSettings_MonitorSystemChanges_DetectionOnlyClick(
      Sender: TObject);
    procedure mnuSettings_MonitorSystemChanges_NoDetectionClick(
      Sender: TObject);
    procedure mnuQuarantineLogClick(Sender: TObject);
    procedure mnuOpenRecoverClick(Sender: TObject);
    procedure mnuSettings_MonitorSystemChanges_SkipFilesWithValidSignatureClick(
      Sender: TObject);
    procedure mnuDownloadDBUpdateClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    { Private declarations }

    //Settings
    ExtToScan : TStrings;

    ListNoScan : TStrings;
    lstNoScan : TStrings;

    ListFullScan : TStrings;
    lstFullScan : TStrings;

    dblMaxLogSizeMB : double;
    bMemoryScanAtStartup : boolean;

    bDetectNewDrives : boolean;
    bAskForScanNewDrives : boolean;
    bNotifyNewVersion : boolean;
    bUseLocalIniFile : boolean;
    bMonitorSystemChanges : boolean;
    sMyRegistryChange : string;
    iMaxNumActiveScan : integer;
    bSkipFilesWithValidSignature : boolean;

    bUpdateClamDB : boolean;

    bUseVxdOnWin98 : boolean;
    //end Settings

    VxdThread : TVxdThread;

    lstDirsToMonitor : TStrings;
    lstDirsToMonitorAllSubfolders : TStrings;
    lstNewFoldersMonitored : TStrings;
    lstExtToMonitorNoTempDir : TStrings;

    FilesScannedRecently : TFileElabInfo;
    FilesMonitoredRecently : TFileElabInfo;
    FilesNewFilesDetected :  TFileElabInfo;

    aDirMonitors : array of TDirMon;
    aDeviceChange : array of TDeviceChange;
    idSkip: Integer;

    ScanQueue : TStringList;

    bCanClose : boolean;
    bRestartOnResume : boolean;

    aRegMonThds : array of TRegMonThread;

    iSEC_DURATION_VIRUS_ALERT : Cardinal;
    iSEC_DURATION_REGISTRY_ALERT : Cardinal;

    procedure DeleteRegMonitors;
    procedure LoadRegMonitors;
    function  CreateNewRegMonitor(const hkRootKey : HKey; const sKey : string) : integer;

    procedure ThreadRegMonDone(Sender: TObject);

    procedure CheckForScan(const sFile : string);
    function  IsRecycleFile(const sFile : string): boolean;
    function  CheckIsDirectoryMonitored(const sFile : string) : boolean;

    procedure StartMemoryScan(const bPause : boolean);
    function  ScanIt(const sFile : string): boolean;

    function  IsPrgTemp(const sFile : string): boolean;
    function  IsPrgLog(const sFile : string): boolean;
    function  IsServicingStack(const sFile : string): boolean;

    function  IsFileInList(const sFile : string; const lst : TStrings;
                                       bAlsoSubFolders : boolean = true): boolean;

    function  IsNoScan(const sFile : string): boolean;

    function  InizializeParams : boolean;

    function  CreateNewDeviceChange(const sUnitPath : string) : integer;
    procedure DeleteDeviceChange(const sUnitPath : string = 'all');

    function  CreateNewDirMonitor(const sPath : string) : integer;
    procedure DeleteDirMonitor(const sPath : string = 'all');

    procedure SaveSettings(bReadOnlyException : boolean = true);
    procedure ReloadIniFile;

    function  IMustToScanIt(const sFile : string) : boolean;
    function  IMustToScanItMonitor(const sFile : string) : boolean;

    function  NotSkipFile(const sFile : string) : boolean;

    function  IsDriveMonitored(const sFile : string): boolean;

    procedure ThreadVxdDone(Sender: TObject);
    procedure StartStopVxd(const b : boolean);

    function  notExcluded(const sFile : string): boolean;

    procedure pmChangeMessageBox(var Msg: TMessage); message WM_USER + 1024;

    procedure CheckVersion(bAutomatic : boolean = false);

    procedure MonitorNewFolders(const sName : string);
    procedure CallMonitorOrScanner(const sFile: string);
    function  IsDir(const s : string): boolean;

    procedure WMHotKey(var Msg: TWMHotKey); message WM_HOTKEY;
    procedure WMQUERYENDSESSION(var Msg: TMessage); message WM_QUERYENDSESSION;
    procedure WMPOWERBROADCAST(var msg: TMessage); message WM_POWERBROADCAST;

    procedure ResolveFileName(const sFile : string; var sLongFilename : string;
      var bWide : boolean; var wOriginalFile : WideString);

    function FileVersionInfo(const sAppNamePath: TFileName): TFileVersionInfo;
    function GoodFileInfo(const sFile : string; const iSize : Cardinal; sExt : string) : boolean;
    function DetectLangCodPage(const iLang : Word): Word;
    procedure StopVxdSleep;

    procedure CallScanner_DetectNewExeFiles(const bIsProbablyMalware : boolean;
       const bIsNewFile : boolean; const sFilenameFullPath : string;
       const TypeOfDetection : TDetectionsType; const bMoveToQuarantine : boolean;
       const bBadFileInfo : boolean; const FileType : TFileTypeDetected;
       const bIsHidden : boolean; const bHasValidSignature : boolean;
       const bHasPEFormat : boolean);

    function IsSpecialFileMonitoredForChanges(const sUcaseFile : string; const sPath : string): boolean;

  public
    { Public declarations }
    sPathQuarantine : string;

    WhenInfectedFileIsFound : TInfectedFileOp;
    bLog : boolean;
    sPathLog : string;
    iNumActiveScan : integer;
    ShowCmdScan : integer;
    sTitle : string;
    bSkipOn : boolean;
    bMonitorWarnings : boolean;

    sMsgStopped : string;
    sMsgScanning : string;
    sMsgRunOnSystemStartup : string;
    sMsgClamWinConfNotFound : string;
    sMsgFileNotFound : string;
    sMsgDriveHasBeenInserted : string;
    sMsgCaptionDetected : string;
    sMsgVirusMovedToQuarantine : string;
    sMsgVirusNotMovedToQuarantine : string;
    sMsgFalsePositive : string;
    sMsgVersionInfo : string;
    sMsgAboutInfo : string;
    sMsgFileReadOnly : string;
    sMsgHaveLatestVersion : string;
    sMsgNewVersionAvailable : string;
    sMsgCheckLatestVersion : string;
    sMsgMaxFilesizeForLogFile : string;
    sMsgValueNotAccepted : string;
    sMsgMaxNumActiveScans : string;
    sMsgInformation : string;
    sMsgMemoryScanning : string;
    sMsgDriveScanning : string;
    sMsgModifiedFolder : string;
    sMsgFileChanged : string;
    sMsgNewFileCreated : string;
    sMsgSuspiciousFile : string;
    sMsgSuspiciousOriginFile : string;
    sMsgObfuscatedFile : string;
    sMsgBadPEHeader : string;
    sMsgCryptedFile : string;
    sMsgBadSignatureFile : string;
    sMsgVerifySuspiciousFile : string;
    sMsgFolderChanged : string;
    sMsgSignExpired : string;

    sMsgRegistryChanged : string;
    sMsgModifiedRegistry : string;

    sExePath : string;
    sSystemTempDir : string;
    sSystemAppData : string;
    sSytemLocalAppData : string;
    sSytemCommonAppData : string;
    sEnvironAllUsersProfile : string;
    sSystemDrive : string;
    sWindowsDir : string;
    sSystem32Dir : string;
    sSystemWOW32Dir : string;
    sSytemTempDir : string;
    sSystemRecycleBin : string;
    sSystemProgramFilesDir : string;
    sEnvironProgramFiles : string;
    sEnvironPublic : string;
    sEnvironProgramW6432 : string;
    sSystemWOWProgramFilesDir : string;
    sSystemCommonFilesDir : string;
    sSystemWOWCommonFilesDir : string;
    sEnvironCommonProgramFiles : string;
    sEnvironCommonProgramW6432 : string;

    sIniFile : string;
    FixedDrivesToScan : TStrings;
    lstFixedDrivesToScan : TStrings;
    NewDrives : TStringList;

    bStartApplication : boolean;
    NotWin9x : boolean;
    IsWin9x : boolean;

    bUseShellNotifyMonitorForFiles : boolean;
    lstExtToMonitor : TStrings;

    procedure ThreadScanDone(Sender: TObject);
    procedure ThreadOptionScanDone(Sender: TObject);
    procedure ThreadCheckMalwareDone(Sender: TObject);
    procedure WndProc(var Msg : TMessage); override;
    procedure MonitorActive(const b : boolean);
    procedure RunRealtimeScan(const sFiles : string);
    procedure SaveRestart;
    function  FSDetectionActive: boolean;
    procedure StartStopIcon(const b : boolean);
    procedure ActionIsMalware(const sFilenameFullPath : string;
        const TypeOfDetection : TDetectionsType; const bMoveToQuarantine : boolean;
        const bBadFileInfo : boolean; var bBeScanned : boolean;
        const bIsHidden : boolean);
  end;

var
  FClamSentinel: TFClamSentinel;

implementation

{$R *.DFM}
{$R ScanIcons.res}

uses ScanThds, IniFiles, FileCtrl, RunRegistry, Utility, NewIniFile,
  UDrives, UChangeList, Languages, UClamWinScanner, Registry
  , VerifyPEFormat;

var
  WinVer: TWindowsVersion;

procedure TFClamSentinel.pmChangeMessageBox(var Msg: TMessage );
var MsgboxHwnd: HWND;
    hParent : HWND;
begin
  MsgboxHwnd := FindWindow(MAKEINTRESOURCE(WC_DIALOG), nil);

  if (MsgboxHwnd <> 0) then
  begin
    hParent := GetParent(MsgboxHwnd);
    if hParent = Handle then
     SendMessage(Msgboxhwnd, WM_SETICON, ICON_SMALL, FClamSentinel.Icon.Handle);
  end;
end;

procedure TFClamSentinel.WndProc(var Msg : TMessage);
var
  p : TPoint;
begin
  case Msg.Msg of
    WM_USER + 1:
    case Msg.lParam of
      WM_RBUTTONDOWN, WM_LBUTTONDOWN:
        begin
          if SetForegroundWindow(Handle) then
          begin
            GetCursorPos(p);
            PopupMenuTrayIcon.Popup(p.x, p.y);
            PostMessage(Handle, WM_NULL, 0, 0);
          end;
        end;
      NIN_BALLOONSHOW:
        {Sent when the balloon is shown}
        ;
      NIN_BALLOONHIDE:
       {Sent when the balloon disappears?Rwhen the icon is deleted,
        for example. This message is not sent if the balloon is dismissed because of
        a timeout or mouse click by the user. }
        DeleteBalloonTips;
      NIN_BALLOONTIMEOUT:
       {Sent when the balloon is dismissed because of a timeout.}
       DeleteBalloonTips;
      NIN_BALLOONUSERCLICK:
       {Sent when the balloon is dismissed because the user clicked the mouse.
        Note: in XP there's Close button on he balloon tips, when click the button,
        send NIN_BALLOONTIMEOUT message actually.}
        DeleteBalloonTips;
    end;
  end;
  inherited;
end;

procedure TFClamSentinel.MonitorActive(const b : boolean);
var
 i : integer;
begin
  WaitIfInUseElabList(FilesScannedRecently);
  ResetElabList(FilesScannedRecently);
  WaitIfInUseElabList(FilesMonitoredRecently);
  ResetElabList(FilesMonitoredRecently);
  WaitIfInUseElabList(FilesNewFilesDetected);
  ResetElabList(FilesNewFilesDetected);

  ShellNotify.Active := b;

  if NotWin9x then
  begin
    for i := Low(aDirMonitors) to High(aDirMonitors) do
      if aDirMonitors[i] <> nil then
        aDirMonitors[i].Active := b;
  end;

  if bUseVxdOnWin98 then
    StartStopVxd(b);

  StartStopIcon(b);
end;

procedure TFClamSentinel.StartStopIcon(const b : boolean);
var
 bNotSkip : boolean;
 s : string;
begin
  bNotSkip := not(bSkipOn);
  s := '';

  if b and bNotSkip then
    begin
       if bMonitorSystemChanges then
        NewIcon(LoadIcon(HInstance, 'SCAN_ENABLED'))
      else
        NewIcon(LoadIcon(HInstance, 'SCAN_NO_MONITOR'));

      ChangeTooltip(sTitle);
    end
  else
    begin
      s := sTitle + ' - ';

      if bNotSkip then
        begin
          NewIcon(LoadIcon(HInstance, 'SCAN_DISABLED'));
          s := s + sMsgStopped;
        end
      else
        begin
          NewIcon(LoadIcon(HInstance, 'SCAN_DISABLED_KEYB'));

          if not b then
            s := s + sMsgStopped + ' ';

          s := s + '(no scanner)';

        end;

      ChangeTooltip(s);
    end;
end;

procedure TFClamSentinel.StartStopVxd(const b : boolean);
begin
 if IsWin9x then
 begin
    if b then
      begin
        if VxdThread = nil then
        begin
          if CheckFileExists(sExePath+'Sentinel.vxd') then
          begin
            SetCurrentDir(sExePath);
            VxdThread := TVxdThread.Create(VXD_INTERVAL);
            VxdThread.OnTerminate := ThreadVxdDone;
          end;
        end;
      end
    else
      begin
        if VxdThread <> nil then
        begin
          VxdThread.OnTerminate := nil;
          VxdThread.Terminate;
          VxdThread := nil;
        end;
      end;
 end;
end;

procedure TFClamSentinel.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if not bCanClose then
  begin
    Action := caNone;
    exit;
  end;

  SaveSettings(false);

  TimerCheckVersion.enabled := false;

  DeleteIcon;
  DeleteRegMonitors;
  Application.Terminate;
end;

procedure TFClamSentinel.FormCreate(Sender: TObject);
var
  bMemoryScan : boolean;
  sLocalIniFile : string;
  sSystemDir : string;
  bRemoveKey : boolean;
  bAutostartForAllUser : boolean;
begin
  bSkipOn := false;
  bCanClose := false;
  bRemoveKey := false;
  bRestartOnResume := false;
  bAutostartForAllUser := false;

  SetLength(aRegMonThds,1);
  aRegMonThds[0]:=nil;

  VxdThread := nil;

  sExePath := ExtractFilePath(Application.ExeName);

  sSystemAppData := ExpandEnvironment(AddSlash(GetSystemPath('AppData')));
  sSytemLocalAppData := ExpandEnvironment(AddSlash(GetSystemPath('Local AppData')));
  sSytemCommonAppData := ExpandEnvironment(AddSlash(GetCommonSystemPath('Common AppData')));
  sEnvironAllUsersProfile := AddSlash(ExpandEnvironment('%ALLUSERSPROFILE%'));
  sEnvironPublic := AddSlash(ExpandEnvironment('%PUBLIC%'));

  sSystemTempDir := AddSlash(GetTempDir);
  sWindowsDir := AddSlash(GetWinDir);
  sSystemDrive := AddSlash(ExtractFileDrive(sWindowsDir));
  sSystemDir := AddSlash(GetSystemDir);
  sSystemProgramFilesDir := ExpandEnvironment(AddSlash(GetGlobalSystemPath('ProgramFilesDir')));
  sSystemWOWProgramFilesDir := ExpandEnvironment(AddSlash(GetWOWGlobalSystemPath('ProgramFilesDir')));
  sSystemCommonFilesDir := ExpandEnvironment(AddSlash(GetGlobalSystemPath('CommonFilesDir')));
  sSystemWOWCommonFilesDir := ExpandEnvironment(AddSlash(GetWOWGlobalSystemPath('CommonFilesDir')));
  sEnvironCommonProgramFiles := AddSlash(ExpandEnvironment('%CommonProgramFiles%'));
  sEnvironCommonProgramW6432 := AddSlash(ExpandEnvironment('%CommonProgramW6432%'));
  sEnvironProgramFiles := AddSlash(ExpandEnvironment('%ProgramFiles%'));
  sEnvironProgramW6432 := AddSlash(ExpandEnvironment('%ProgramW6432%'));

  ScanQueue := TStringList.Create;
  NewDrives := TStringList.Create;
  FixedDrivesToScan := TStringList.Create;
  lstFixedDrivesToScan := TStringList.Create;

  lstDirsToMonitor := TStringList.Create;
  lstDirsToMonitorAllSubfolders := TStringList.Create;
  lstNewFoldersMonitored := TStringList.Create;
  lstExtToMonitor := TStringList.Create;
  lstExtToMonitorNoTempDir := TStringList.Create;

  NewDrives.Clear;
  ScanQueue.Clear;
  FixedDrivesToScan.Clear;
  lstFixedDrivesToScan.Clear;

  lstDirsToMonitor.Clear;
  lstDirsToMonitorAllSubfolders.Clear;
  lstNewFoldersMonitored.Clear;
  lstExtToMonitor.Clear;
  lstExtToMonitorNoTempDir.Clear;

  NotWin9x := not(Win32Platform = VER_PLATFORM_WIN32_WINDOWS);
  IsWin9x := not(NotWin9x);

  bUseShellNotifyMonitorForFiles := true;

  WinVer := GetWindowsVersion;

  if WinVer in [cOSUnknown, cOSWinNT, cOSWin2k, cOSWin2k3, cOSGenericWinNT] then
    begin
      iSEC_DURATION_VIRUS_ALERT := SEC_DURATION_ALERTS_WIN2000;
      iSEC_DURATION_REGISTRY_ALERT := SEC_DURATION_ALERTS_WIN2000;
    end
  else
    begin
      iSEC_DURATION_VIRUS_ALERT := SEC_DURATION_VIRUS_ALERT;
      iSEC_DURATION_REGISTRY_ALERT := SEC_DURATION_REGISTRY_ALERT;
    end;

  if IsWin9x then
    sSystemRecycleBin := sSystemDrive+'Recycled\'
  else
    begin
      sSystemRecycleBin := sSystemDrive+'Recycler\';

      if WinVer in [cOSVista,cOSSeven,cOSWin8] then
        sSystemRecycleBin := sSystemDrive+'$Recycle.Bin\'
    end;

  lstDirsToMonitor.Add(sSystemDrive);
  lstDirsToMonitor.Add(sWindowsDir);

  lstDirsToMonitor.Add(ExpandEnvironment(AddSlash(GetSystemPath('Startup'))));
  lstDirsToMonitor.Add(ExpandEnvironment(AddSlash(GetSystemPath('SendTo'))));

  lstDirsToMonitor.Add(ExpandEnvironment(AddSlash(GetCommonSystemPath('Common Startup'))));

  lstDirsToMonitor.Add(sWindowsDir+'ShellNew\');
  lstDirsToMonitor.Add(sWindowsDir+'SendTo\');
  lstDirsToMonitor.Add(sWindowsDir+'inf\');
  lstDirsToMonitor.Add(sWindowsDir+'Tasks\');

  sSystem32Dir := sWindowsDir+'System32\';
  sSystemWOW32Dir := sWindowsDir+'SysWOW64\';

  lstDirsToMonitorAllSubfolders.Add(sSystem32Dir);
  lstDirsToMonitorAllSubfolders.Add(sSystemWOW32Dir);

  lstDirsToMonitorAllSubfolders.Add(sWindowsDir+'System\');

  if lstDirsToMonitorAllSubfolders.IndexOf(sSystemDir)<0 then
    lstDirsToMonitorAllSubfolders.Add(sSystemDir); //for wow

  if lstDirsToMonitorAllSubfolders.IndexOf(sWindowsDir+'temp\')<0 then
    lstDirsToMonitorAllSubfolders.Add(sWindowsDir+'temp\');

  lstDirsToMonitorAllSubfolders.Add(sSytemLocalAppData);
  lstDirsToMonitorAllSubfolders.Add(sSytemCommonAppData);
  lstDirsToMonitorAllSubfolders.Add(sEnvironAllUsersProfile);
  lstDirsToMonitorAllSubfolders.Add(sEnvironPublic);

  lstDirsToMonitorAllSubfolders.Add(sSystemAppData);
  lstDirsToMonitorAllSubfolders.Add(sSystemTempDir);

  lstDirsToMonitorAllSubfolders.Add(sSystemDrive+'Recycler\');
  lstDirsToMonitorAllSubfolders.Add(sSystemDrive+'Recycled\');
  lstDirsToMonitorAllSubfolders.Add(sSystemDrive+'$Recycle.Bin\');

  lstDirsToMonitorAllSubfolders.Add(ExpandEnvironment(AddSlash(GetCommonSystemPath('Common Desktop'))));
  lstDirsToMonitorAllSubfolders.Add(ExpandEnvironment(AddSlash(GetSystemPath('Desktop'))));

  lstDirsToMonitorAllSubfolders.Add(sSystemCommonFilesDir);
  lstDirsToMonitorAllSubfolders.Add(sSystemWOWCommonFilesDir);

  lstDirsToMonitorAllSubfolders.Add(sEnvironCommonProgramFiles);
  lstDirsToMonitorAllSubfolders.Add(sEnvironCommonProgramW6432);

  lstNewFoldersMonitored.Add('Recycle');

  lstExtToMonitor.Add('.dll');
  lstExtToMonitor.Add('.exe');
  lstExtToMonitor.Add('.sys');
  lstExtToMonitor.Add('.scr');
  lstExtToMonitor.Add('.pif');
  lstExtToMonitor.Add('.inf');
  lstExtToMonitor.Add('.com');
  lstExtToMonitor.Add('.bat');
  lstExtToMonitor.Add('.cmd');
  lstExtToMonitor.Add('.vbs');
  lstExtToMonitor.Add('.vbe');
  lstExtToMonitor.Add('.jse');
  lstExtToMonitor.Add('.wsf');
  lstExtToMonitor.Add('.wsh');
  lstExtToMonitor.Add('.msc');
  lstExtToMonitor.Add('.msi');
  lstExtToMonitor.Add('.cpl');
  lstExtToMonitor.Add('.crt');
  lstExtToMonitor.Add('.ocx');
  lstExtToMonitor.Add('.lnk');
  lstExtToMonitor.Add('.job');
  lstExtToMonitor.Add('.nt');
  lstExtToMonitor.Add('.xpd');
  lstExtToMonitor.Add('.checkvirus');
  lstExtToMonitor.Add('.bin');

  lstExtToMonitorNoTempDir.Add('.js');
  lstExtToMonitorNoTempDir.Add('.tmp');
  lstExtToMonitorNoTempDir.Add('.jpg');
  lstExtToMonitorNoTempDir.Add('.jpeg');
  lstExtToMonitorNoTempDir.Add('.asp');
  lstExtToMonitorNoTempDir.Add('.aspx');

  lstExtToMonitor.AddStrings(lstExtToMonitorNoTempDir);

  SetLength(aDeviceChange,1);
  aDeviceChange[0]:=nil;

  iNumActiveScan := 0;

  bStartApplication := false;

  sIniFile := GetIniFilePath(cProjectFileName);

  SetLanguage_FSentinel(FClamSentinel);

  sLocalIniFile := ChangeFileExt(Application.ExeName, '.ini');

  if not CheckFileExists(sIniFile) then
  begin
    sLocalIniFile := ChangeFileExt(Application.ExeName, '.ini');

    if not CheckFileExists(sLocalIniFile) then
      begin
        if FDrives = nil then
        begin
          FDrives := TFDrives.create(self);
          FDrives.bAddAllFixedDrives := true;
          FDrives.ShowModal;
        end;
        WriteNewIniFile(sIniFile);

        if not(IsClamWinPortable)
            and not ((UpperCase(GetRunKey(cProjectName))=UpperCase(Application.ExeName))
                     or (UpperCase(GetRunKey(cProjectName,false))=UpperCase(Application.ExeName))) then
        begin
          if TopMessage(Handle,sMsgRunOnSystemStartup,cProjectName, MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON1) = IDYES then
            RunOnWinStart(cProjectName, PChar(Application.Exename), false)
          else
            bRemoveKey := true;
        end
      end
    else
      CopyFile(PChar(sLocalIniFile),PChar(sIniFile),true);
  end;

  PatchIniFile;

  if RunKeyFound(cProjectName) then
  begin
    if UpperCase(GetRunKey(cProjectName,false))=UpperCase(Application.ExeName) then
    begin
      bRemoveKey := true;
      bAutostartForAllUser := true;
    end;

    if bRemoveKey then
      RemoveFromRunKey(cProjectName);

    if bAutostartForAllUser then
      mnuExitClick(self);
  end;

  ResetElabList(FilesScannedRecently);
  ResetElabList(FilesMonitoredRecently);
  ResetElabList(FilesNewFilesDetected);

  Screen.cursor := crHourGlass;
  //Application.ShowMainForm := false;

  AddIcon(Self);
  GoTray(Self);

  sTitle := cProjectName + ' ' + VERSION;

  ChangeToolTip(sTitle);

  if InizializeParams then
  begin
    bStartApplication := true;

    if bMonitorSystemChanges then
      LoadRegMonitors;

    if dblMaxLogSizeMB > 0 then
    begin
      //Delete log files if are more of the max filesize
      LimitLogSize(sPathLog + LOGFILE_DRIVE_SCAN, dblMaxLogSizeMB);
      LimitLogSize(sPathLog + LOGFILE_MEMORY_SCAN, dblMaxLogSizeMB);
      LimitLogSize(sPathLog + LOGFILE_REALTIME_SCAN, dblMaxLogSizeMB);
      LimitLogSize(sPathLog + LOGFILE_MESSAGES, dblMaxLogSizeMB);
    end;

    Screen.cursor := crDefault;

    bMemoryScan := false;
    //With the command parameter --memory start memory scan on startup
    if ParamCount>0 then
      if LowerCase(trim(ParamStr(1))) = '--memory' then
        bMemoryScan := true;

    if bMemoryScanAtStartup or bMemoryScan then
    begin
      if CurrentScanner = scanner_ClamWin then
        StartMemoryScan(false);
    end;

    idSkip := GlobalAddAtom('Skip key');
    RegisterHotKey(Handle, idSkip, VKMOD, VKSKIP);

    if bNotifyNewVersion then
    begin
      TimerCheckVersion.Interval := TIMER_FIRST_INTERVAL;
      TimerCheckVersion.enabled := true;
    end;
  end;

  // Hide from task list (Ctrl-Alt-Del list)
  if IsWin9x then
    HideShowProcessWin9x(true);
end;

function TFClamSentinel.InizializeParams : boolean;
const
  DRIVE_REMOVABLE = 2;

var
  IniFile : TIniFile;
  DirToScanOK : TStrings;
  DirToScan : TStrings;
  sPath : string;
  i : integer;
  sNoScanCommaText : string;
  sFullScanCommaText : string;
  sCurrentDrive : string;
  sPathClamWinConf : string;
  iIndex : integer;
  sDrives : string;
begin
 sPathClamWinConf := '';

 MonitorActive(false);

 if NotWin9x then
 begin
   DeleteDirMonitor('all');
   SetLength(aDirMonitors,1);
   aDirMonitors[0]:=nil;
 end;

 ShellNotify.NotifyEvents := [];

 if bUseShellNotifyMonitorForFiles then
   ShellNotify.NotifyEvents := ShellNotify.NotifyEvents
                 + [neCreate,neRenameItem,neUpdateItem];

 ExtToScan := TStringList.Create;
 ListNoScan := TStringList.Create;
 lstNoScan := TStringList.Create;

 ListFullScan := TStringList.Create;
 lstFullScan := TStringList.Create;

 if CheckFileExists(sIniFile) then
 begin
   IniFile  := TIniFile.Create(sIniFile);

   try
     //Monitor system for new malware
     bMonitorSystemChanges := (IniFile.ReadInteger('Params', 'MonitorSystemChanges', 0) = 1);
     bMonitorWarnings := (IniFile.ReadInteger('Params', 'MonitorWarnings', 0) = 1);

     DirToScanOK := TStringList.Create;
     DirToScan := TStringList.Create;

     try
       sDrives := UpperCase(ConvertCommaToCommaText(IniFile.ReadString('Params', 'DirToScan',  sSystemDrive)));

       if sDrives <> '""' then
         begin
           lstFixedDrivesToScan.CommaText := sDrives;
           FixedDrivesToScan.CommaText := ExpandEnvironment(sDrives);
         end
       else
         FixedDrivesToScan.Add(ExpandEnvironment(AddSlash(sSystemDrive)));

       DirToScan.AddStrings(FixedDrivesToScan);

       sCurrentDrive := AddSlash(ExtractFileDrive(sExePath));

       if GetDriveType(PChar(sCurrentDrive)) = DRIVE_REMOVABLE then
       begin
         if (DirToScan.IndexOf(sCurrentDrive) < 0)
              and (NewDrives.IndexOf(sCurrentDrive) < 0) then
           NewDrives.Add(sCurrentDrive);
       end;

       if NewDrives.Count > 0 then
         DirToScan.AddStrings(NewDrives);

       for i := 0 to DirToScan.Count-1 do
       begin
         sPath := AddSlash(DirToScan[i]);

         //if (sPath <> '\') and DiskInDrive(sPath[1]) and DirectoryExists(sPath) then
         if (sPath <> '\') and DirectoryExists(sPath) then
         begin
           DirToScanOK.Add(sPath);

           if NotWin9x then
           begin
             CreateNewDirMonitor(sPath);

             if IsInList(FixedDrivesToScan,DirToScan[i])>=0 then
             begin
               if GetDriveType(PChar(sPath)) = DRIVE_REMOVABLE then
                 CreateNewDeviceChange(sPath);
             end;
           end;
         end;
       end;

       ShellNotify.PathList.Clear;
       if bUseShellNotifyMonitorForFiles then
           ShellNotify.PathList.AddStrings(DirToScanOK);

     finally
       DirToScanOK.free;
       DirToScan.Free;
     end;

     ExtToScan.CommaText := IniFile.ReadString('Params', 'ExtToScan',  ' ');

     iIndex := ExtToScan.IndexOf('.CLAMTMP');
     if iIndex>=0 then
       ExtToScan.Delete(iIndex);

     bLog := (IniFile.ReadInteger('Params', 'Log', 0) = 1);

     //Skip files with a valid digital signature
     bSkipFilesWithValidSignature := (IniFile.ReadInteger('Params', 'SkipValidSignatures', 0) = 1);

     //If you want to detect new drives added
     bDetectNewDrives := (IniFile.ReadInteger('Params', 'Detect_NewDrives', 1) = 1);

     //If you want to update the virus database via ClamSentinel
     bUpdateClamDB := (IniFile.ReadInteger('Params', 'UpdateClamDB', 0) = 1);

     if bDetectNewDrives then
       ShellNotify.NotifyEvents := ShellNotify.NotifyEvents + [neDriveAdd,neDriveRemoved];

     //If you want to ask for to scan new drives added
     bAskForScanNewDrives := (IniFile.ReadInteger('Params', 'AskForScan_NewDrives', 1) = 1);

     bNotifyNewVersion := (IniFile.ReadInteger('Params', 'NotifyNewVersion', 1) = 1);

     bUseLocalIniFile := (IniFile.ReadInteger('Params', 'UseLocalIniFile', 0) = 1);

     bMemoryScanAtStartup := (IniFile.ReadInteger('Params', 'MemoryScanAtStartup', 0) = 1);

     if bMonitorSystemChanges then
       ShellNotify.NotifyEvents := ShellNotify.NotifyEvents + [neMkDir,neRenameFolder];

     lstNoScan.Clear;
     ListNoScan.Clear;
     
     sNoScanCommaText := ConvertCommaToCommaText(IniFile.ReadString('Params', 'NoScan',  ''));

     if sNoScanCommaText<>'""' then
     begin
       lstNoScan.CommaText := sNoScanCommaText;

       ListNoScan.CommaText := ExpandEnvironment(sNoScanCommaText);
     end;

     lstFullScan.Clear;
     ListFullScan.Clear;

     sFullScanCommaText := ConvertCommaToCommaText(IniFile.ReadString('Params', 'FullScan',  ''));

     if sFullScanCommaText<>'""' then
     begin
       lstFullScan.CommaText := sFullScanCommaText;

       ListFullScan.CommaText := ExpandEnvironment(sFullScanCommaText);
     end;

     sPathLog := AddSlash(ExpandEnvironment(IniFile.ReadString('Params', 'PathLog',  ' ')));

     sPathClamWinConf := AddSlash(ExpandEnvironment(
                            IniFile.ReadString('Params','PathClamWin',' ')));

     dblMaxLogSizeMB := IniFile.ReadFloat('Params','MaxLogSizeMB', MAX_LOG_SIZE_MB);

     //Maximum number of simultaneously active scans (1..10)
     iMaxNumActiveScan := IniFile.ReadInteger('Params','MaxNumActiveScan', 1);

     WhenInfectedFileIsFound := opMoveToQuarantine;

     //What to do when an infected file has been found
     if IniFile.ReadInteger('Params', 'WhenInfectedFileIsFound', Ord(opMoveToQuarantine)) = ord(opReportOnly) then
       WhenInfectedFileIsFound := opReportOnly;

     //ShowCmd for createprocess as integer (to see SW_HIDE, SW_SHOWNORMAL, etc.)
     ShowCmdScan := IniFile.ReadInteger('Params', 'ShowCmdScan', SW_HIDE);

     //If you want to use vxd driver Sentinel.vxd on Win98
     bUseVxdOnWin98 := (IniFile.ReadInteger('Params', 'UseVxdOnWin98', 1) = 1);

   finally
     IniFile.Free;
   end;

   if CurrentScanner = scanner_ClamWin then
     result := ReadClamWinConfiguration(sPathClamWinConf)
   else
     begin
       sPathQuarantine := ExtractFilePath(sIniFile) + 'quarantine';

       if not DirectoryExists(sPathQuarantine) then
         CreateDir(sPathQuarantine);
         
       result := true;
     end;
 end
 else
 begin
   TopMessage(Handle,Format(sMsgFileNotFound,[sIniFile]),cProjectName,MB_ICONERROR);
   result := false;
 end;

 if RemoveSlash(sPathLog) = '' then
   sPathLog := ExtractFilePath(sIniFile);

 MonitorActive(result);
end;

procedure TFClamSentinel.mnuExitClick(Sender: TObject);
begin
  bCanClose := true;
  StopVxdSleep;
  Close;
end;

function TFClamSentinel.IsPrgTemp(const sFile : string): boolean;
  var
    sUCaseFile : string;
begin
  sUCaseFile := UpperCase(sFile);

  result := (sUCaseFile = UpperCase(sSystemTempDir+BAT_MEMORY_SCAN))
    or (sUCaseFile = UpperCase(sSystemTempDir+BAT_DRIVE_SCAN));

  if not result then
    result := MatchStrings(ExtractFileName(sUCaseFile), UpperCase(cProjectFileName) + '*.EXE');
end;

function TFClamSentinel.IsServicingStack(const sFile : string): boolean;
begin
  IsServicingStack := (Pos('microsoft-windows-servicingstack', LowerCase(ExtractFilePath(sFile)))>0);
end;

function TFClamSentinel.IsPrgLog(const sFile : string): boolean;
  var
    sUCaseFile : string;
begin
  sUCaseFile := UpperCase(sFile);

  result := (sUCaseFile = UpperCase(sPathLog + LOGFILE_DRIVE_SCAN))
    or (sUCaseFile = UpperCase(sPathLog + LOGFILE_MEMORY_SCAN))
    or (sUCaseFile = UpperCase(sPathLog + LOGFILE_REALTIME_SCAN))
    or (sUCaseFile = UpperCase(sPathLog + LOGFILE_MESSAGES))
    or (sUCaseFile = UpperCase(sPathLog + LOGFILE_QUARANTINE));
end;

function TFClamSentinel.IsFileInList(const sFile : string; const lst : TStrings;
                                  bAlsoSubFolders : boolean = true): boolean;
var
  i : integer;
  bFound : boolean;
  sLowCasePathFile : string;
  sLowCaseListFile : string;
begin
  bFound := false;
  i := 0;

  while not(bFound) and (i<lst.Count) do
  begin
    sLowCasePathFile := LowerCase(ExtractFilePath(sFile));
    sLowCaseListFile := AddSlash(LowerCase(lst[i]));

    if bAlsoSubFolders then
      bFound := StartWith(sLowCaseListFile,sLowCasePathFile)
    else
      bFound := sLowCasePathFile = sLowCaseListFile;

    if not bFound then
      inc(i);
  end;
  result := bFound;
end;

function TFClamSentinel.IsNoScan(const sFile : string): boolean;
var
  i : integer;
  bFound : boolean;
  sLowCaseFile : string;
  sLowCaseNoScan : string;
  sFileNameNoScan : string;
  sFileName : string;
begin
  bFound := false;
  i := 0;

  while not(bFound) and (i<ListNoScan.Count) do
  begin
    sLowCaseFile := LowerCase(sFile);
    sLowCaseNoScan := LowerCase(ListNoScan[i]);
    sFileNameNoScan := ExtractFileName(sLowCaseNoScan);
    sFileName := ExtractFileName(sLowCaseFile);

    bFound := StartWith(sLowCaseNoScan,sLowCaseFile);

    if not(bFound) and (sFileNameNoScan = sLowCaseNoScan) then
      bFound := (sFileNameNoScan = sFileName);

    if not(bFound) and (ExtractFilePath(sLowCaseNoScan)='')
         and ((Pos('*',sFileNameNoScan)>0) or (Pos('?',sFileNameNoScan)>0)) then
      bFound := MatchStrings(sFileName, sFileNameNoScan);
      
    if not(bFound)
         and ((Pos('*',sLowCaseNoScan)>0) or (Pos('?',sLowCaseNoScan)>0)) then
      bFound := MatchStrings(sLowCaseFile, sLowCaseNoScan);

    if not bFound then
      inc(i);
  end;
  result := bFound;
end;

function TFClamSentinel.ScanIt(const sFile : string): boolean;
begin
  result := (IsExtInList(sFile,ExtToScan) or IsFileInList(sFile,ListFullScan))
     and not(IsNoScan(sFile));
end;

procedure TFClamSentinel.StartMemoryScan(const bPause : boolean);
var
 sParams : string;
begin
  sParams := '';

  //for write date time and to separate each scan
  if bLog then
    Log('',sPathLog + LOGFILE_MEMORY_SCAN,true,true);

  if CurrentScanner = scanner_ClamWin then
  begin
    sParams := '--show-progress --kill --memory';

    StartScanner('',LOGFILE_MEMORY_SCAN, sParams
        , SW_SHOW, false, bPause, opMemoryScan, sSystemTempDir + BAT_MEMORY_SCAN);
  end;
end;

procedure TFClamSentinel.FormDestroy(Sender: TObject);
begin
  if NotWin9x then
  begin
    DeleteDirMonitor('all');
    SetLength(aDirMonitors,1);
    aDirMonitors[0]:=nil;
    DeleteDeviceChange('all');
  end;

  UnRegisterHotKey(Handle, idSkip);
  GlobalDeleteAtom(idSkip);

  ExtToScan.Free;
  ListNoScan.Free;
  lstNoScan.Free;
  lstFullScan.Free;
  ListFullScan.Free;
  ScanQueue.Free;
  NewDrives.Free;
  FixedDrivesToScan.Free;

  lstDirsToMonitor.Free;
  lstDirsToMonitorAllSubfolders.Free;
  lstNewFoldersMonitored.Free;
  lstExtToMonitor.Free;
  lstExtToMonitorNoTempDir.Free;
  lstFixedDrivesToScan.Free;

  ResetElabList(FilesScannedRecently);
  ResetElabList(FilesMonitoredRecently);
  ResetElabList(FilesNewFilesDetected);
end;

procedure TFClamSentinel.ShellNotifyNotify(Sender: TObject;
  Event: TShellNotifyEvent; Path1, Path2: String);
var
  iIndex : integer;
  iIndexToScan : integer;
  sMsg : string;
  sLetter : string;
  sParams : string;
begin
  case Event of
    neCreate, neUpdateItem:
      CheckForScan(Path1);

    neRenameItem:
      CheckForScan(Path2);

    neDriveAdd:
      begin
        if NewDrives.IndexOf(Path1) < 0 then
        begin
          if (Path1<>'') and (DiskInDrive(Path1[1])) then
          begin
            NewDrives.Add(Path1);

            sLetter := UpperCase(StringReplace(Path1, ':\', '', [rfReplaceAll, rfIgnoreCase]));

            //sMsg := 'Do you want  drive ' + sLetter + ' to be monitored?';
            //if TopMessage(Handle,sMsg,'Confirm',MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON1) = IDYES then
            //begin

              if bUseShellNotifyMonitorForFiles then
              begin
                ShellNotify.PathList.Add(Path1);
                ShellNotify.Active:=false;
                ShellNotify.Active:=true;
              end;

              if NotWin9x then
              begin
                aDirMonitors[CreateNewDirMonitor(Path1)].active := true;
                CreateNewDeviceChange(Path1);
              end;
            //end;

            if bAskForScanNewDrives and not(CurrentScanner = none) then
            begin
              sMsg := Format(sMsgDriveHasBeenInserted,[sLetter]);

              if TopMessage(Handle,sMsg,Format(sMsgCaptionDetected,[sLetter]), MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON1) = IDYES then
              begin
                //for write date time and to separate each scan
                if bLog then
                  Log('',sPathLog + LOGFILE_DRIVE_SCAN,true,true); //for write date time

                sParams := '';

                if CurrentScanner = scanner_ClamWin then
                begin
                  sParams := '--show-progress --recursive';

                  StartScanner(Path1,LOGFILE_DRIVE_SCAN,
                      sParams , SW_SHOW, false, true
                      , opDriveScan, sSystemTempDir + BAT_DRIVE_SCAN);
                end;
              end;
            end;
          end;
        end;
      end;
    neDriveRemoved:
      begin
        iIndex := NewDrives.IndexOf(Path1);

        iIndexToScan := IsInList(FixedDrivesToScan,Path1);

        if (iIndex >= 0) or (iIndexToScan >= 0) then
        begin
          if iIndex >= 0 then
            NewDrives.Delete(iIndex);

          if NotWin9x then
          begin
            DeleteDirMonitor(Path1);
            DeleteDeviceChange(Path1);
          end;

          if bUseShellNotifyMonitorForFiles then
          begin
            iIndex := ShellNotify.PathList.IndexOf(Path1);

            if iIndex >= 0 then
            begin
              ShellNotify.PathList.Delete(iIndex);
              ShellNotify.Active:=false;
              ShellNotify.Active:=true;
            end;
          end;
        end;
      end;
    neMkDir:
        MonitorNewFolders(Path1);

    neRenameFolder:
        MonitorNewFolders(Path2);
  end;
end;

procedure TFClamSentinel.MonitorNewFolders(const sName : string);
  function IsInNewFolderList(const s : string; const lst : TStrings): boolean;
  var
    i : integer;
    bFound : boolean;
  begin
    bFound := false;
    i := 0;

    while not(bFound) and (i<lst.Count) do
    begin
      bFound := (Pos(LowerCase(lst[i]),LowerCase(s)) > 0);

      if not bFound then
        inc(i);
    end;
    result := bFound;
  end;

var
  sUCaseName : string;
  sMsg : string; 
  sSlashName : string;
begin

if bMonitorSystemChanges and not(IsRecycleFile(sName)) then
begin
  sSlashName := AddSlash(sName);
  sUCaseName := UpperCase(sSlashName);

  if IsInNewFolderList(sUCaseName,lstNewFoldersMonitored) and not(IsNoScan(sUcaseName)) then
  begin
    if not(IsIntoElabList(FilesMonitoredRecently,sUCaseName)) then
    begin
      if DirectoryExists(sName) then
      begin
        AddIntoElabList(FilesMonitoredRecently,sUCaseName);

        sMsg := Format(sMsgFolderChanged,[sSlashName]);

        if bMonitorWarnings then
          ShowBalloonTips(sMsgModifiedFolder,sMsg,NIIF_Info, SEC_DURATION_MODIFIED_FOLDER_ALERT);

        if bLog then
          Log(sMsgModifiedFolder + CRLF + sMsg,sPathLog + LOGFILE_MESSAGES,true,true);
      end;
    end;
  end;
end;
end;

procedure TFClamSentinel.RunRealtimeScan(const sFiles : string);
var
 sParams : string;
begin
  //realtime scanner always use threads
  sParams := '';

  if CurrentScanner = scanner_ClamWin then
  begin
    sParams := '--verbose --no-summary';

    StartScanner(sFiles, LOGFILE_REALTIME_SCAN, sParams
      , SW_SHOWMINNOACTIVE, true, false, opFileScan, '');
  end;
end;

function TFClamSentinel.IsDriveMonitored(const sFile : string): boolean;
var
  sDrive : string;
begin
  Result := false;

  if copy(sFile,1,2)<>'\\' then
    sDrive := AddSlash(ExtractFileDrive(sFile))
  else
    sDrive := AddSlash(ExtractFilePath(sFile));

  if sDrive <> '\' then
  begin
    result := (NewDrives.IndexOf(sDrive)>=0) or (IsInList(FixedDrivesToScan,sDrive)>=0);
  end;
end;

function TFClamSentinel.NotSkipFile(const sFile : string) : boolean;
begin
  //require an uppercase filename

  result := (ExtractFileName(sFile) <> '')
             and (UpperCase(ExtractFilePath(sFile)) <> AddSlash(Uppercase(sPathQuarantine)))
             and (Uppercase(sFile) <> Uppercase(sIniFile))
             and CheckFileExists(sFile)
             and not(IsScannerTemp(sFile))
             and not(IsPrgTemp(sFile))
             and not(IsPrgLog(sFile))
             and not(IsServicingStack(sFile))
             and not((ExtractFileExt(sFile) = '.JOB')
                  and StartWith('USER_FEED_SYNCHRONIZATION-{',ExtractFileName(sFile)))
             and notExcluded(sFile)
             and IsDriveMonitored(sFile);
end;

function TFClamSentinel.IMustToScanIt(const sFile : string) : boolean;
begin
  result := IMustToScanItMonitor(sFile) and not(bSkipOn);
end;

function TFClamSentinel.IMustToScanItMonitor(const sFile : string) : boolean;
begin
  //require an uppercase filename
  Result := false;

  if IsDriveMonitored(sFile)
         and (ScanQueue.IndexOf(sFile) < 0)
         and not(IsIntoElabList(FilesScannedRecently,sFile))
         and ScanIt(sFile)
         and CheckFileExists(sFile) then
    Result := true;
end;

function TFClamSentinel.IsRecycleFile(const sFile : string): boolean;

  function IsSystemRecycleFolder(const sFile : string): boolean;
  var
    sPath : string;
    sRecycle : string;
  begin
    sPath := LowerCase(ExtractFilePath(sFile));
    sRecycle := LowerCase(sSystemRecycleBin);

    result := StartWith(sRecycle,sPath);
  end;

  function OnlyNumbers(const s : string): boolean;
    var
      i : integer;
  begin
    result := true;
    for i := 1 to Length(s) do
      result := result and (s[i] in ['0'..'9']);
  end;

  var
    sFileName : string;
    sExt : string;
    sHead : string;
    sTail : string;
    WinVer: TWindowsVersion;
begin
  result := false;

  if IsSystemRecycleFolder(sFile) then
  begin
    WinVer := GetWindowsVersion;
    sFileName := UpperCase(ExtractFileName(sFile));
    sExt := ExtractFileExt(sFile);
    sFileName := copy(sFilename,1,Length(sFilename)-Length(sExt));
    sHead := copy(sFileName,1,2);
    sTail := Copy(sFileName,3,MAXINT);

    if WinVer in [cOSVista,cOSSeven,cOSWin8] then
      begin
        if (sHead = '$I') or (sHead = '$R') then
          result := true;
      end
    else
      begin
        if sHead = 'D' + UpperCase(Copy(sSystemDrive,1,1)) then
          result := OnlyNumbers(sTail);
      end;
    end;
end;

procedure TFClamSentinel.ResolveFileName(const sFile : string; var sLongFilename : string;
   var bWide : boolean; var wOriginalFile : WideString);
begin
  sLongFilename := sFile;

  if NotWin9x and (Length(Trim(ExtractFileName(sLongFilename))) <= 12) then //Possible 8.3 format
    sLongFilename := ExtractLongPathName(sFile);

  sLongFilename := Trim(sLongFilename);

  bWide := (NotWin9x and (sLongFilename<>'') and (sFile <> sLongFilename));

  if bWide then
    wOriginalFile := ExtractLongPathNameW(sFile)
  else
    wOriginalFile := StringToWide(sFile);
end;

function TFClamSentinel.IsSpecialFileMonitoredForChanges(const sUCaseFile : string; const sPath : string): boolean;
begin
  result := (UpperCase(sPath) = UpperCase(sSystem32Dir + 'drivers\etc\'))
             or (UpperCase(sPath) = UpperCase(sSystemWOW32Dir + 'drivers\etc\'))
             or (sUCaseFile = UpperCase(sWindowsDir + 'hosts'))
             or (sUCaseFile = UpperCase(sWindowsDir + 'win.ini'))
             or (sUCaseFile = UpperCase(sWindowsDir + 'system.ini'));
end;

function TFClamSentinel.CheckIsDirectoryMonitored(const sFile : string) : boolean;
var
  sMsg : string;
  sUCaseFile : string;
  bIsSystemTempDir : boolean;
  sPath : string;
  sLongFilename : string;
  sFileMsg : string;
  bWide : boolean;
  wOriginalFullFilenamePath : WideString;
  wMsg : WideString;
  sDosFilename : string;
  bMonitored : boolean;
  bNotSkip : boolean;
  bIMustToScanItMonitor : boolean;
  iSecDuration : Cardinal;
  bIsSpecialFileMonitoredForChanges : boolean;
  bIsExToMonitor : boolean;
  bIsExecutable : boolean;

  procedure ShowMsg(const sMessage : string; const iSecDuration : Cardinal);
  begin
    sMsg := Format(sMessage,[sPath,sFileMsg + sDosFilename]);

    if NotWin9x then
      begin
        wMsg := FormatW(StringToWide(sMessage),
             [WideExtractFilePath(wOriginalFullFilenamePath)
                ,WideExtractFileName(wOriginalFullFilenamePath)
                                       + StringToWide(sDosFilename)]);

        if bMonitorWarnings then
          ShowBalloonTipsW(StringtoWide(sMsgModifiedFolder), wMsg
                    , NIIF_WARNING, iSecDuration);
      end
    else
      begin
        if bMonitorWarnings then
           ShowBalloonTips(sMsgModifiedFolder,sMsg,NIIF_Info
                                    , iSecDuration);
      end;

     if bLog then
       Log(sMsgModifiedFolder + CRLF + sMsg,sPathLog + LOGFILE_MESSAGES,true,true);
  end;

begin
  result := false;
  sDosFilename := '';
  bIsExecutable := false;

  sUCaseFile := UpperCase(sFile);
  sPath := ExtractFilePath(sFile);

  bNotSkip :=
     not(IsIntoElabList(FilesMonitoredRecently,sUCaseFile))
     and not(IsNoScan(sFile))
     and not(IsRecycleFile(sFile))
     and CheckFileExists(sFile);

  if bNotSkip then
  begin
    bIsSystemTempDir := StartWith(UpperCase(sSystemTempDir),AddSlash(UpperCase(sPath)))
       or StartWith(UpperCase(sSystemAppData),AddSlash(UpperCase(sPath)))
       or StartWith(UpperCase(sSytemLocalAppData),AddSlash(UpperCase(sPath)))
       or StartWith(UpperCase(sSytemCommonAppData),AddSlash(UpperCase(sPath)));

    bIsExToMonitor := IsExtInList(sUCaseFile,lstExtToMonitor);

    if not bIsExToMonitor then
      bIsExecutable := (IsExecutable(sFile) in [IsExe, IsDll]);

    bMonitored := (IsFileInList(sUCaseFile,lstDirsToMonitorAllSubfolders,true)
       or IsFileInList(sUCaseFile,lstDirsToMonitor,false))
       and (bIsExToMonitor or bIsExecutable)
       and not(bIsSystemTempDir and IsExtInList(sUCaseFile,lstExtToMonitorNoTempDir))
       and bNotSkip;

    bIMustToScanItMonitor := (IMustToScanItMonitor(sUCaseFile) and bIsExToMonitor) or bIsExecutable;

    bIsSpecialFileMonitoredForChanges := IsSpecialFileMonitoredForChanges(sUcaseFile,sPath);

    if bMonitored or bIsSpecialFileMonitoredForChanges then
    begin
      ResolveFileName(sFile, sLongFilename, bWide, wOriginalFullFilenamePath);

      sFileMsg := Trim(ExtractFileName(sLongFilename));
      sPath := Trim(ExtractFilePath(sLongFilename));

      if bWide then
        sDosFilename := ' (' + Trim(ExtractFileName(sFile)) + ')';

      if bIsSpecialFileMonitoredForChanges then
        iSecDuration := iSEC_DURATION_REGISTRY_ALERT
      else
        iSecDuration := SEC_DURATION_MODIFIED_FOLDER_ALERT;

      ShowMsg(sMsgFileChanged, iSecDuration);
    end;

    if bMonitored or bIMustToScanItMonitor then
    begin
      result := true;
      AddIntoElabList(FilesMonitoredRecently,sUCaseFile);

      if bMonitored then
        begin
          with TCheckMalwareThread.Create(PChar(sFile), false, lstExtToMonitor) do
             OnTerminate := FClamSentinel.ThreadCheckMalwareDone;
        end
      else
        begin
          //starts the monitor for all files with a monitored extension outside monitored folders
          if bIMustToScanItMonitor then
          begin
            with TCheckMalwareThread.Create(PChar(sFile), true, lstExtToMonitor) do
               OnTerminate := FClamSentinel.ThreadCheckMalwareDone;
          end;
        end;
    end;
  end;
end;

procedure TFClamSentinel.CheckForScan(const sFile : string);
var
 sUCaseFile : string;
 bMonitored : boolean;
begin
  bMonitored := false;
  sUCaseFile := UpperCase(sFile);

  if NotSkipFile(sUCaseFile) then
  begin
    if bMonitorSystemChanges then
      bMonitored := CheckIsDirectoryMonitored(sFile);

    if not(bMonitored)
       and not(IsIntoElabList(FilesMonitoredRecently,sUCaseFile))
       and IMustToScanIt(sUCaseFile) then
    begin
      if iNumActiveScan < iMaxNumActiveScan then
        begin
          RunRealtimeScan('"'+ sUCaseFile +'"'); //start immediatly
          AddIntoElabList(FilesScannedRecently,sUCaseFile);
        end
      else
        ScanQueue.Add(sUCaseFile); //Add into the queue
    end;
  end;
end;

procedure TFClamSentinel.mnuStartClick(Sender: TObject);
begin
  ReloadIniFile;
end;

procedure TFClamSentinel.mnuStopClick(Sender: TObject);
begin
  MonitorActive(false);
end;

procedure TFClamSentinel.mnuMemoryScanClick(Sender: TObject);
begin
  StartMemoryScan(true);
end;

function TFClamSentinel.FSDetectionActive: boolean;
var
 bFSDetectionActive : boolean;
begin

  bFSDetectionActive := ShellNotify.Active;

  if NotWin9x then
  begin
    if low(aDirMonitors)=0 then
      if aDirMonitors[0] <> nil then
        bFSDetectionActive := bFSDetectionActive or aDirMonitors[0].Active;
  end;
  Result := bFSDetectionActive;
end;

procedure TFClamSentinel.PopupMenuTrayIconPopup(Sender: TObject);
var
 bFSDetectionActive : boolean;
begin
  bFSDetectionActive := FSDetectionActive;

  mnuStart.enabled := not(bFSDetectionActive);
  mnuStop.enabled := (bFSDetectionActive);

  mnuQuarantine.enabled := (DirectoryExists(sPathQuarantine));
  mnuRealTimeLog.enabled := (CheckFileExists(sPathLog + LOGFILE_REALTIME_SCAN));
  mnuDriveScanLog.enabled := (CheckFileExists(sPathLog + LOGFILE_DRIVE_SCAN));
  mnuMessagesLog.enabled := (CheckFileExists(sPathLog + LOGFILE_MESSAGES));
  mnuQuarantineLog.enabled := (CheckFileExists(sPathLog + LOGFILE_QUARANTINE));

  mnuMemoryScan.Visible := (CurrentScanner = scanner_ClamWin);
  SepMemoryScan.Visible := mnuMemoryScan.Visible;
  
  mnuMemoryLog.Visible := (CurrentScanner = scanner_ClamWin);
  mnuDownloadDBUpdate.Visible := (CurrentScanner = scanner_ClamWin) and bUpdateClamDB;

  mnuSettings_MemoryScanAtStartup.Visible := (CurrentScanner = scanner_ClamWin);

  mnuMemoryLog.enabled := (CheckFileExists(sPathLog + LOGFILE_MEMORY_SCAN)) and (mnuMemoryLog.Visible);

  mnuSettings_RunOnStartup.Visible := not(UpperCase(GetRunKey(cProjectName,false))=UpperCase(Application.Exename));
  mnuSettings_SepStartup.Visible := mnuSettings_RunOnStartup.Visible;
  mnuSettings_RunOnStartup.Checked := (UpperCase(GetRunKey(cProjectName))=UpperCase(Application.Exename));

  mnuSettings_DetectNewDrives.Checked := bDetectNewDrives;
  mnuSettings_AskForScanNewDrives.Checked := bAskForScanNewDrives;
  mnuSettings_MemoryScanAtStartup.Checked := bMemoryScanAtStartup;
  mnuSettings_Log.Checked := bLog;
  mnuSettings_NotifyNewVersion.Checked := bNotifyNewVersion;
  mnuAdvancedSettings_UseVxdOnWin98.Checked := bUseVxdOnWin98;

  mnuInfectedFiles_MoveToQuarantine.Checked := (WhenInfectedFileIsFound = opMoveToQuarantine);
  mnuInfectedFiles_ReportOnly.Checked := (WhenInfectedFileIsFound = opReportOnly);

  mnuAdvancedSettings_UseVxdOnWin98.Visible := IsWin9x;

  mnuSettings_MonitorSystemChanges_NoDetection.Checked := not(bMonitorSystemChanges);  
  mnuSettings_MonitorSystemChanges_DetectionWithWarnings.Checked := bMonitorSystemChanges and bMonitorWarnings;
  mnuSettings_MonitorSystemChanges_DetectionOnly.Checked := bMonitorSystemChanges and not(bMonitorWarnings);
  mnuSettings_MonitorSystemChanges_SkipFilesWithValidSignature.Checked := bSkipFilesWithValidSignature;
  mnuSettings_MonitorSystemChanges_SkipFilesWithValidSignature.Enabled := bMonitorSystemChanges;

end;

procedure TFClamSentinel.OnDeviceQueryRemove(Sender: TObject; sUnit: String);
  var iIndex : integer;
begin
  if NotWin9x then
  begin
    //A request for unplug the device has been detected
    //Delete the monitor that has an handle opened to the device
    DeleteDirMonitor(sUnit);
    DeleteDeviceChange(sUnit);

    if bUseShellNotifyMonitorForFiles then
    begin
      iIndex := ShellNotify.PathList.IndexOf(sUnit);

      if iIndex >= 0 then
      begin
        ShellNotify.PathList.Delete(iIndex);
        ShellNotify.Active:=false;
        ShellNotify.Active:=true;
      end;
    end;
  end;
end;

function TFClamSentinel.IsDir(const s : string): boolean;
begin
  if ExtractFileExt(s) = '' then
    result := DirectoryExists(s)
  else
    result := false;
end;

procedure TFClamSentinel.CallMonitorOrScanner(const sFile: string);
begin
  if bMonitorSystemChanges and IsDir(sFile) then
    MonitorNewFolders(sFile)
  else
    CheckForScan(sFile);
end;

procedure TFClamSentinel.DirMonCreated(Sender: TObject; FileName: String);
begin
  CallMonitorOrScanner(FileName);
end;

procedure TFClamSentinel.DirMonModified(Sender: TObject; FileName: String);
begin
  CheckForScan(FileName);
end;

procedure TFClamSentinel.DirMonRenamed(Sender: TObject; fromFileName,
  toFileName: String);
begin
  CallMonitorOrScanner(toFileName);
end;

procedure TFClamSentinel.DeleteDirMonitor(const sPath : string = 'all');
var
  i : integer;
begin
  for i := Low(aDirMonitors) to High(aDirMonitors) do
  begin
    if aDirMonitors[i] <> nil then
    begin
      with aDirMonitors[i] do
      begin
        if (Path = sPath) or (sPath='all') then
        begin
          Active := false;
          Free;
          aDirMonitors[i] := nil;
        end;
      end;
    end;
  end;
end;

function TFClamSentinel.CreateNewDirMonitor(const sPath : string) : integer;
var
 i : integer;
begin
  if aDirMonitors[0]=nil then
    i:=0
  else
    i := High(aDirMonitors)+1;

  SetLength(aDirMonitors,i+1);

  aDirMonitors[i] := TDirMon.Create(Self);

  with aDirMonitors[i] do
  begin
    WatchFilters := [nfFile_NAME,nfCREATION,nfLast_WRITE];

    if bMonitorSystemChanges then
      WatchFilters := WatchFilters + [nfDIR_NAME];

    WatchSubtree := true;
    Path := sPath;

    OnCreated := DirMonCreated;
    OnModified := DirMonModified;
    OnRenamed := DirMonRenamed;
  end;
  result := i;
end;

procedure TFClamSentinel.DeleteDeviceChange(const sUnitPath : string = 'all');
var
  i : integer;
begin
  for i := Low(aDeviceChange) to High(aDeviceChange) do
  begin
    if aDeviceChange[i] <> nil then
    begin
      with aDeviceChange[i] do
      begin
        if (UnitPath = sUnitPath) or (sUnitPath='all') then
        begin
          Free;
          aDeviceChange[i] := nil;
        end;
      end;
    end;
  end;
end;

function TFClamSentinel.CreateNewDeviceChange(const sUnitPath : string) : integer;
var
 i : integer;
begin
  if aDeviceChange[0]=nil then
    i:=0
  else
    i := High(aDeviceChange)+1;

  SetLength(aDeviceChange,i+1);

  aDeviceChange[i] := TDeviceChange.Create(sUnitPath);

  aDeviceChange[i].OnQueryRemove := OnDeviceQueryRemove;

  result := i;
end;

procedure TFClamSentinel.ThreadScanDone(Sender: TObject);

  function StripLoadingDBMessage(const sLog : string): string;
    const
      cLOADINGDATABASE = 'Loading virus signature database, please wait';
      CRLF = #13#10;
    var
      iLenLoadingDatabase : integer;
      s : string;
      sMsg : string;
      iPosStop : integer;
      sLine : string;
  begin
    iLenLoadingDatabase := Length(cLOADINGDATABASE);

    s := sLog + CRLF;
    sMsg := '';

    iPosStop := Pos(CRLF,s);

    while (iPosStop > 0) do
    begin
      sLine := copy(s,0,iPosStop-1);

      if not (Copy(sLine,1,iLenLoadingDatabase) = cLOADINGDATABASE) then
      begin
        if sMsg <> '' then
          sMsg := sMsg + CRLF;

        sMsg := sMsg + sLine;

        if Copy(sMsg,Length(sMsg)-1,2)=CRLF then
          sMsg := Copy(sMsg,1,Length(sMsg)-2);
      end;
      s := copy(s,iPosStop+2,MAXINT);
      iPosStop := Pos(CRLF,s);
    end;

    result := sMsg;
  end;

var
 sOut : string;
 sErr : string;

 iIndex : integer;
 iFiles : integer;
 sFiles : string;
 sCurrentFile : string;
 sAlert : string;
 sLog : string;
 sMsg : string;
 DetectType : TDetectType;
 sDetect : string;
begin
  sDetect := '';
  DetectType := detect_None;
  StartStopIcon(FSDetectionActive);

  Dec(iNumActiveScan);

  if iNumActiveScan<0 then
    iNumActiveScan := 0;

  if iNumActiveScan < iMaxNumActiveScan then
  begin
    if (ScanQueue.Count > 0) and (FSDetectionActive) then
    begin
      iFiles := 0;
      sFiles := '';

      while (ScanQueue.Count > 0) and (iFiles < MAX_CLAMSCAN_FILES) do
      begin
        iIndex := ScanQueue.Count-1;

        sCurrentFile := ScanQueue[iIndex];

        if CheckFileExists(sCurrentFile) and not(IsIntoElabList(FilesScannedRecently,sCurrentFile)) then
        begin
          sFiles := sFiles + '"' + sCurrentFile + '" ';
          AddIntoElabList(FilesScannedRecently,UpperCase(sCurrentFile));

          Inc(iFiles);
        end;

        ScanQueue.Delete(iIndex);
      end;

      if sFiles<>'' then
        RunRealtimeScan(sFiles);
    end;
  end;

  sOut := Trim(TScanThread(Sender).Output);
  sErr := Trim(TScanThread(Sender).Error);

  sLog := sOut;

  if (sLog<>'') and (sErr<>'') then
    sLog := sLog + CRLF;

  sLog := sLog + sErr;

  if (sLog<>'') and (bLog) then
  begin
    Log(StripLoadingDBMessage(sLog),TScanThread(Sender).LogFile);
  end;

  ExtractVirusAlert(sOut,sAlert,DetectType);

  if sAlert <> '' then
  begin
    case DetectType of
      detect_Virus:         sDetect := 'virus';
      detect_FalsePositive: sDetect := sMsgFalsePositive;
    end;

    if (WhenInfectedFileIsFound = opMoveToQuarantine)
                       and (DetectType <> detect_FalsePositive) then
      sMsg := Format(sMsgVirusMovedToQuarantine,[sDetect])
    else
      sMsg := Format(sMsgVirusNotMovedToQuarantine,[sDetect]);

    ShowBalloonTips(sMsg, sAlert, NIIF_WARNING, iSEC_DURATION_VIRUS_ALERT);

    if bLog then
    begin
      Log(sMsg + CRLF + sAlert,sPathLog + LOGFILE_MESSAGES,true,true);
      Log(sLog + CRLF + CRLF + sMsg + CRLF + sAlert,sPathLog + LOGFILE_QUARANTINE,true,true);
    end;
  end
end;

function TFClamSentinel.FileVersionInfo(const sAppNamePath: TFileName): TFileVersionInfo;

const NOVAL = -1;

var
  VerSize: Integer;
  VerBuf: PChar;
  pBuf : Pointer;
  lpTranslate  :^LANGANDCODEPAGE;
  aTranslateInfoDetected  : array of LANGANDCODEPAGE;
  GoodTranslateInfo : LANGANDCODEPAGE;
  VerHandle: Cardinal;
  VerBufLen: Cardinal;
  VerKey: string;
  cp : Word;
  k : integer;
  iSize : integer;
  bWide : boolean;
  fvi : TFileVersionInfo;

  iIndex_1250 : integer;
  iIndex_1251 : integer;
  iIndex_1252 : integer;
  iIndex_1254 : integer;
  iIndex_1200_1250 : integer;
  iIndex_1200_1251 : integer;
  iIndex_1200_1252 : integer;
  iIndex_1200_1254 : integer;
  bZeroInfo : boolean;

  function QueryValueANSI(const aKey: string): string;
    Var vStr : PChar;
  begin
    Result := '';

    if VerQueryValue(VerBuf, PChar(VerKey + aKey), Pointer(vStr), VerBufLen) then
      Result := Trim(StrPas(vStr));
  end;

  function QueryValueWide(const aKey: string): string;
    Var vStr : PWideChar;
  begin
    Result := '';

    if VerQueryValueW(VerBuf, PWideChar(StringToWide(VerKey + aKey)), Pointer(vStr), VerBufLen) then
      Result := WideStringToString(vStr,cp)
  end;

  function QueryValue(const aKey: string): string;
    var sVal : string;
  begin

    if not(bWide) or (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) then
      result := QueryValueANSI(aKey)
    else
      begin
        sVal := QueryValueWide(aKey);
        if sVal = '' then
          result := QueryValueANSI(aKey)
        else
          result := sVal;
      end;
  end;

  function ExtractStructureFileInfo : TFileVersionInfo;
  begin
    with Result do
    begin
      bVersionInfo     := true;
      CodePage         := GoodTranslateInfo.wCodePage;
      Language         := GoodTranslateInfo.wLanguage;
      CompanyName      := QueryValue('CompanyName');
      FileDescription  := QueryValue('FileDescription');
      FileVersion      := QueryValue('FileVersion');
      InternalName     := QueryValue('InternalName');
      LegalCopyRight   := QueryValue('LegalCopyright');
      LegalTradeMarks  := QueryValue('LegalTrademarks');
      OriginalFileName := QueryValue('OriginalFilename');
      ProductName      := QueryValue('ProductName');
      ProductVersion   := QueryValue('ProductVersion');
      Comments         := QueryValue('Comments');
      CompiledScript   := QueryValue('CompiledScript');
      PrivateBuild     := QueryValue('PrivateBuild');
      SpecialBuild     := QueryValue('SpecialBuild');
      CodePagePossible := cp;
    end;
  end;
begin
  with fvi do
  begin
    bVersionInfo     := false;
    CompanyName      := '';
    FileDescription  := '';
    FileVersion      := '';
    InternalName     := '';
    LegalCopyRight   := '';
    LegalTradeMarks  := '';
    OriginalFileName := '';
    ProductName      := '';
    ProductVersion   := '';
    Comments         := '';
    CompiledScript   := '';
    PrivateBuild     := '';
    SpecialBuild     := '';
    Language         := 0;
    CodePage         := 0;
    CodePagePossible := 0;
  end;

  iIndex_1250 := NOVAL;
  iIndex_1251 := NOVAL;
  iIndex_1252 := NOVAL;
  iIndex_1254 := NOVAL;
  iIndex_1200_1250 := NOVAL;
  iIndex_1200_1251 := NOVAL;
  iIndex_1200_1252 := NOVAL;
  iIndex_1200_1254 := NOVAL;  
  bZeroInfo := false;

  GoodTranslateInfo.wLanguage := 0;
  GoodTranslateInfo.wCodePage := 0;
  bWide := false;

  VerSize := GetFileVersionInfoSize(PChar(sAppNamePath), VerHandle);
  if VerSize > 0 then
  begin
    VerBuf := AllocMem(VerSize);
    try
      if GetFileVersionInfo(PChar(sAppNamePath), VerHandle, VerSize, VerBuf) then
      begin
        VerBufLen := VerSize;
        if VerQueryValue(VerBuf, '\VarFileInfo\Translation', pBuf, VerBufLen) then
        begin
          if VerBufLen > 0 then
          begin
            bWide := true;
            iSize := VerBufLen div SizeOf(LANGANDCODEPAGE);

            SetLength(aTranslateInfoDetected,iSize);
            lpTranslate := pBuf;

            for k := low(aTranslateInfoDetected) to High(aTranslateInfoDetected) do
            begin
              aTranslateInfoDetected[k].wLanguage := lpTranslate.wLanguage;
              aTranslateInfoDetected[k].wCodePage := lpTranslate.wCodePage;

              if (lpTranslate.wLanguage = 0) and (lpTranslate.wCodePage = 0) then
                bZeroInfo := true
              else if lpTranslate.wCodePage = 1250 then
                     iIndex_1250 := k
                   else if lpTranslate.wCodePage = 1251 then
                          iIndex_1251 := k
                        else if lpTranslate.wCodePage = 1252 then
                               iIndex_1252 := k
                             else if lpTranslate.wCodePage = 1254 then
                                    iIndex_1254 := k
                             else if lpTranslate.wCodePage = 1200 then
                                  begin
                                    cp := DetectLangCodPage(lpTranslate.wLanguage);

                                    if cp > 0 then
                                    begin
                                      if cp = 1250 then
                                        iIndex_1200_1250 := k
                                      else if cp = 1251 then
                                             iIndex_1200_1251 := k
                                           else if cp = 1252 then
                                                  iIndex_1200_1252 := k
                                             else if cp = 1254 then
                                                    iIndex_1200_1254 := k
                                    end;
                                  end;
              Inc(lpTranslate);
            end;

            k := NOVAL;

            if iIndex_1250 > NOVAL then
              begin
                k := iIndex_1250;
                cp := 1250;
              end
            else if iIndex_1252 > NOVAL then
                   begin
                     k := iIndex_1252;
                     cp := 1252;
                   end
              else if iIndex_1254 > NOVAL then
                   begin
                     k := iIndex_1254;
                     cp := 1254;
                   end
                 else if iIndex_1200_1250 > NOVAL then
                        begin
                          k := iIndex_1200_1250;
                          cp := 1250;
                        end
                      else if iIndex_1200_1252 > NOVAL then
                             begin
                               k := iIndex_1200_1252;
                               cp := 1252;
                             end
                           else if iIndex_1200_1254 > NOVAL then
                             begin
                               k := iIndex_1200_1254;
                               cp := 1254;
                             end
                           else if iIndex_1251 > NOVAL then
                                  begin
                                    k := iIndex_1251;
                                    cp := 1251;
                                  end
                                else if iIndex_1200_1251 > NOVAL then
                                       begin
                                         k := iIndex_1200_1251;
                                         cp := 1251;
                                       end;

            if k > NOVAL then
              begin
                GoodTranslateInfo := aTranslateInfoDetected[k];
                bZeroInfo := false;
              end
            else
              begin
               if High(aTranslateInfoDetected)=0 then
               begin
                 GoodTranslateInfo := aTranslateInfoDetected[0];
                 cp := GoodTranslateInfo.wCodePage;
               end;
              end;
          end;
        end
        else
          bZeroInfo := true;

        if bZeroInfo then
        begin
          GoodTranslateInfo.wLanguage := $0409;
          GoodTranslateInfo.wCodePage := $04E4;
          cp := GoodTranslateInfo.wCodePage;
        end;

        if GoodTranslateInfo.wCodePage>0 then
        begin
          VerKey := Format('\StringFileInfo\%.4x%.4x\'
             , [GoodTranslateInfo.wLanguage, GoodTranslateInfo.wCodePage]);

          fvi := ExtractStructureFileInfo;

          with fvi do
          begin
            if (CompanyName='') and (FileDescription='') and (FileVersion='')
              and (InternalName='') and (LegalCopyRight='') and (LegalTradeMarks='')
              and (OriginalFileName='') and (ProductName='') and (ProductVersion='')
              and (Comments='') then
            begin
              GoodTranslateInfo.wLanguage := $0409;
              GoodTranslateInfo.wCodePage := $04E4;
              cp := GoodTranslateInfo.wCodePage;

              VerKey := Format('\StringFileInfo\%.4x%.4x\'
                , [GoodTranslateInfo.wLanguage, GoodTranslateInfo.wCodePage]);

              fvi := ExtractStructureFileInfo;
            end;
            result := fvi;
          end;
        end;
    end;
    finally
      FreeMem(VerBuf, VerSize);
    end;
  end;
end;

function CheckVowels(const s : string; const cp : Word; var bFirstCharIsVowelorConsonant : boolean) : boolean;

  Const MAX_WORD_LENGTH = 17;
        MAX_SEQ_NO_VOWELS = 5;

  var sl : string;
      i : integer;
      c : char;
      iTotVowels : Cardinal;
      iTotChars : Cardinal;

      iTotWords : Cardinal;
      iTotWordsNoVowels : Cardinal;

      iVowels : Cardinal;
      iChars : Cardinal;

      iCountNoVowels : Cardinal;

      bCheck : boolean;
      bVowel : boolean;
      bConsonant : boolean;
begin
  //In nearly all languages, words must contain at least one vowel.

  iTotVowels := 0;
  iTotChars := 0;

  iTotWords := 0;
  iTotWordsNoVowels := 0;

  iVowels := 0;
  iChars := 0;

  iCountNoVowels := 0;

  sl := Trim(LowerCase(s));

  sl := StringReplace(sl,'(C)','',[rfReplaceAll, rfIgnoreCase]);
  sl := StringReplace(sl,'(R)','',[rfReplaceAll, rfIgnoreCase]);
  sl := StringReplace(sl,'HTML','',[rfReplaceAll, rfIgnoreCase]);
  sl := StringReplace(sl,'DLL','',[rfReplaceAll, rfIgnoreCase]);
  sl := trim(sl);
  
  bCheck := true;
  i := 1;

  while bCheck and (i<=Length(sl)) do
  begin
    c := sl[i];

    bVowel := (c in ['a','e','i','o','u','j','y']);
    bConsonant := false;
    
    if not(bVowel) and (cp = 1250) then
    begin
      //Codepage 1250 vowels
      bVowel := (Ord(c) in [165,185,193,194,195,196,201,202,203,204,205,206,211
        ,212,213,214,217,218,219,220,221,223,225,226,227,228,229,233,234,235
        ,236,237,238,243,244,245,246,249,250,251,252,253]);
    end;

    if not(bVowel) and (cp = 1252) then
    begin
      //Codepage 1252 vowels
      bVowel := (Ord(c) in [192,193,194,195,196,197,198,200,201,202,203,204,205
         ,206,207,210,211,212,213,214,216,217,218,219,220,221,223,224,225,226
         ,227,228,229,230,232,233,234,235,236,237,238,239,240,242,243,244,245
         ,246,248,249,250,251,252,253,255]);
    end;

    if not(bVowel) and (cp = 1251) then
    begin
      //Codepage 1251 vowels
      bVowel := (Ord(c) in [161,162,163,168,175,178,179,184,188,191,192,224,197
        ,229,199,231,200,232,201,233,206,238,211,243,219,251,221,253,222,254,223
        ,255]);
    end;

    if not(bVowel) and (cp = 1254) then
    begin
      //Codepage 1254 vowels
      bVowel := (Ord(c) in [140,156,159,192,193,194,195,196,197,198,200,201,202
        ,203,204,205,206,207,210,211,212,213,214,216,217,218,219,220,221,223,224
        ,225,226,227,228,229,230,232,233,234,235,236,237,238,239,242,243,244,245
        ,246,248,249,250,251,252,253,255]);
    end;

    if bVowel then
      begin
        Inc(iTotChars);
        Inc(iChars);

        Inc(iTotVowels);
        Inc(iVowels);

        iCountNoVowels := 0;
      end
    else
      begin
        bConsonant := c in ['a'..'z'];

        if not(bConsonant) and (cp = 1250) then
        begin
          //Codepage 1250 consonants
          bConsonant := (Ord(c) in [138,140,141,142,143,154,156,157,158,159,163
             ,167,170,175,179,181,186,188,190,191,192,197,198,199,200,207,208
             ,209,210,216,222,224,230,231,232,239,240,241,242,248,254]);
        end;

        if not(bConsonant) and (cp = 1252) then
        begin
          //Codepage 1252 consonants
          bConsonant := (Ord(c) in [181,199,208,209,222,231,241,254]);
        end;

        if not(bConsonant) and (cp = 1251) then
        begin
          //Codepage 1251 consonants
          bConsonant := (Ord(c) in [128,129,131,138,140,141,142,143,144,154,156
            ,157,158,159,165,180,181,189,190,193,225,194,226,195,227,196,228,198
            ,230,202,234,203,235,204,236,205,237,207,239,208,240,209,241,210,242
            ,212,244,213,245,214,246,215,247,216,248,217,249,218,250,220,252]);
        end;

        if not(bConsonant) and (cp = 1254) then
        begin
          //Codepage 1254 consonants
          bConsonant := (Ord(c) in [138,131,154,199,208,209,222,231,240,241,254]);
        end;
        
        if bConsonant then
          begin
            Inc(iTotChars);
            Inc(iChars);

            Inc(iCountNoVowels);

            if iCountNoVowels > MAX_SEQ_NO_VOWELS then
              bCheck := false;
          end;
      end;

    if iChars > MAX_WORD_LENGTH then
      bCheck := false
    else
      begin
        if not(bVowel or bConsonant) and not (c in ['0'..'9']) then
        begin
          if iChars > 0 then
          begin
            Inc(iTotWords);

            if iVowels = 0 then
              Inc(iTotWordsNoVowels);
          end;

          iVowels := 0;
          iChars := 0;
          iCountNoVowels := 0;
        end;

        if (i=1) and (bVowel or bConsonant) then
          bFirstCharIsVowelorConsonant := true;

        Inc(i);
      end;
  end;

  if bCheck and (iChars > 0) then
  begin
    Inc(iTotWords);

    if iVowels = 0 then
      Inc(iTotWordsNoVowels);
  end;

  if bCheck then
    bCheck := iTotVowels >= Round(iTotChars / 5); //1 vowel every 5 chars;

  if bCheck then
    bCheck := iTotWordsNoVowels <= Round(iTotWords / 2);

  result := bCheck;
end;

function TFClamSentinel.DetectLangCodPage(const iLang : Word): Word;
var
  IDLanguage : Word;

begin
  //CodePage 1250 - Central and East European Latin
  //CodePage 1251 - Cyrillic
  //CodePage 1252 - West European Latin
  //CodePage 1254 - Azeri (Latin), Turkish, Uzbek (Latin)

  result := 0;
  IDLanguage := iLang and $3FF;

  case IDLanguage of
    //1252
    LANG_NEUTRAL:       result := 1252;
    LANG_AFRIKAANS:     result := 1252;
    LANG_BASQUE:        result := 1252;
    LANG_CATALAN:       result := 1252;
    LANG_DANISH:        result := 1252;
    LANG_DUTCH:         result := 1252;
    LANG_ENGLISH:       result := 1252;
    LANG_FAEROESE:      result := 1252;
    LANG_FINNISH:       result := 1252;
    LANG_FRENCH:        result := 1252;
    LANG_GERMAN:        result := 1252;
    //LANG_ICELANDIC:   result := 1252;
    LANG_ITALIAN:       result := 1252;
    LANG_NORWEGIAN:     result := 1252;
    LANG_PORTUGUESE:    result := 1252;
    LANG_SPANISH:       result := 1252;
    LANG_SWEDISH:       result := 1252;
    LANG_GALICIAN:      result := 1252;
    
    //1250
    LANG_ROMANIAN:      result := 1250;
    LANG_ALBANIAN:      result := 1250;
    LANG_POLISH:        result := 1250;
    LANG_HUNGARIAN:     result := 1250;
    LANG_SLOVENIAN:     result := 1250;
    LANG_CZECH:         result := 1250;
    LANG_CROATIAN:      result := 1250;
    LANG_SLOVAK:        result := 1250;

    //1251
    LANG_RUSSIAN:       result := 1251;
    LANG_BULGARIAN:     result := 1251;
    LANG_UKRAINIAN:     result := 1251;
    LANG_BELARUSIAN:    result := 1251;

    //1254
    LANG_AZERI:         result := 1254;
    LANG_TURKISH:       result := 1254;
    LANG_UZBEK:         result := 1254;
  end;
end;

function TFClamSentinel.GoodFileInfo(const sFile : string; const iSize : Cardinal; sExt : string) : boolean;

  function RemoveFileName(const sInfo : string) : string;
    var s : string;
        s1 : string;
  begin
    s := ExtractFileName(sFile);
    s1 := StringReplace(sInfo,s,' ',[rfReplaceAll, rfIgnoreCase]);
    s := StringReplace(s,ExtractFileExt(s),'',[rfReplaceAll, rfIgnoreCase]);
    result := StringReplace(s1,s,' ',[rfReplaceAll, rfIgnoreCase]);
  end;

  var
    FvI: TFileVersionInfo;
    bBadFileInfo : boolean;

    sRealExt : string;
    sInternalExt : string;
    bManyChars : boolean;

    IconHandle  : HIcon;

    bFirstCharIsVowelorConsonant : boolean;
begin

  FvI.bVersionInfo := false;
  bBadFileInfo := false;

  FvI := FileVersionInfo(sFile);

  with FvI do
  begin
    if iSize > SIZE_LIMIT_MALWARE then
    begin
      if not(bVersionInfo) then
        begin
          result := false;
          exit;
        end
      else
        bBadFileInfo := ((ProductVersion<>'') and (FileVersion<>'')
          and (ProductName='') and (FileDescription=''));
    end;

    if bVersionInfo then
    begin
     if not(((ProductName='Microsoft® .NET Framework')
              or (ProductName='Microsoft (R) Windows (R) Operating System')
              or (ProductName='.NET Compact Framework')
              or (ProductName='Microsoft® DirectX for Windows®')
              or (ProductName='Microsoft® Silverlight')
              or (ProductName='Microsoft (R) Visual C++')
              or (ProductName='Microsoft SQL Server')
              or StartWith('Microsoft® Visual Studio®',ProductName))
           and (StartWith('© Microsoft Corporation.', LegalCopyRight)
              or StartWith('Copyright (c) Microsoft Corporation.', LegalCopyRight)
              or StartWith('Copyright © Microsoft Corporation.', LegalCopyRight))
           and (CompanyName='Microsoft Corporation')) then
     begin
      //Copyright information with no Product Name information.
      //Product Name and Product Version filled out with no Copyright information.

      //0
      if not(bBadFileInfo) then
      begin
        bBadFileInfo :=
           ( (ProductName='') and (ProductVersion='') and (FileVersion='')
            and (FileDescription='')
            and ((LegalCopyRight<>'') or (LegalTradeMarks<>'')) );
      end;

      //1
      if not(bBadFileInfo) then
      begin
        bBadFileInfo :=
             ((ProductName<>'') and (ProductVersion<>'') and (LegalCopyRight='')
                and (LegalTradeMarks='') and (CompanyName=''));
      end;

      //if not(bBadFileInfo) then
      //begin
      //2bBadFileInfo :=
      //2     (((LegalCopyRight<>'') or (LegalTradeMarks<>''))
      //2           and ((ProductName='') and (FileDescription='')));
      //end;

      //3
      //if not(bBadFileInfo) then
      //begin
      //3bBadFileInfo := bBadFileInfo or
      //3     ((ProductVersion<>'') and (FileVersion<>'') and (ProductName=''));
      //end;

      if not(bBadFileInfo) then
      begin
        bBadFileInfo := bBadFileInfo or
           ((ProductVersion<>'') and (FileVersion<>'') and (ProductName='')
           and (LegalCopyRight='') and (LegalTradeMarks='')
           and (FileDescription=''));
      end;

      if not(bBadFileInfo) and (FileDescription<>'') then
      begin
        if LegalCopyRight<>'' then
          bBadFileInfo := (FileDescription = LegalCopyRight);

        if not(bBadFileInfo) and (LegalTradeMarks<>'') then
          bBadFileInfo := (FileDescription = LegalTradeMarks);
      end;

      if not(bBadFileInfo) then
      begin
        bBadFileInfo := (FileDescription='') and (LegalTradeMarks='')
           and (LegalCopyRight='');
      end;

      if not(bBadFileInfo) then
      begin
        bBadFileInfo := ((FileDescription<>'') and (Length(FileDescription)<3))
           or ((ProductName<>'') and (Length(ProductName)<3));
      end;

      if not(bBadFileInfo) then
      begin
        bBadFileInfo := (ProductVersion='0.0.0.0') or (FileVersion='0.0.0.0');
      end;

      if not(bBadFileInfo) then
      begin
        bBadFileInfo := (LegalCopyRight<>'') and (ProductName='')
      end;

      //if not(bBadFileInfo) then
      //begin
      //  bBadFileInfo := (FileDescription<>'')
      //     and ((LegalTradeMarks<>'') or (LegalCopyRight<>'')) and (CompanyName='');
      //end;

      //4
      if not(bBadFileInfo) then
      begin
         if (CodePage = 1250) or (CodePage = 1252) or (CodePage = 1251)
                 or (CodePage = 1254)
                 or ((CodePage = 1200) and (CodePagePossible>0)) then
        begin
          if FileDescription<>'' then
          begin
            bFirstCharIsVowelorConsonant := false;
            
            bBadFileInfo := not CheckVowels(RemoveFileName(FileDescription), CodePagePossible, bFirstCharIsVowelorConsonant);

            bBadFileInfo := bBadFileInfo or not(bFirstCharIsVowelorConsonant);
          end;

          if not(bBadFileInfo) and (ProductName<>'') then
             bBadFileInfo := not CheckVowels(RemoveFileName(ProductName), CodePagePossible, bFirstCharIsVowelorConsonant);
        end;
      end;

      if not(bBadFileInfo) then
      begin
        if OriginalFileName<>'' then
        begin
          sRealExt := '';
          sInternalExt := '';

          if DoubleExtensions(OriginalFileName, sRealExt, sInternalExt, bManyChars) then
            bBadFileInfo := (bManyChars or ExtensionKnown(sInternalExt)) and not(UpperCase(sRealExt) = '.MUI')
                               and (lstExtToMonitor.IndexOf(sRealExt)>=0);
        end;
      end;

      if not(bBadFileInfo) then
      begin
        if (ProductName='') and (ProductVersion='') then
        begin
          if sExt <> '.DLL' then
          begin
            IconHandle := ExtractIcon(Application.Handle, PChar(sFile), 0) ;
            bBadFileInfo := (IconHandle>0);
          end;
        end;
      end;

      if not(bBadFileInfo) then
      begin
        bBadFileInfo := ((ProductVersion<>'') and not(HasOnlyNumbersOrDots_NotBadValue(ProductVersion)));
      end;

      if not(bBadFileInfo) then
      begin
        bBadFileInfo := ((FileVersion<>'') and not(HasNumbers(FileVersion)));
      end;

     end;
    end;
  end;
  result := not(bBadFileInfo);
end;

procedure TFClamSentinel.ActionIsMalware(const sFilenameFullPath : string;
     const TypeOfDetection : TDetectionsType; const bMoveToQuarantine : boolean;
     const bBadFileInfo : boolean; var bBeScanned : boolean;
     const bIsHidden : boolean);
var
  sLongFilename : string;
  sFilePath : string;
  bWide : boolean;
  wFileName : WideString;
  wQuarantineOriginalFilename : WideString;
  wQuarantineFilename : WideString;
  wOriginalFullFilenamePath : WideString;
  wMsg : WideString;
  sFileMsg : string;
  sDosFilename : string;
  sMsgDetectionType : string;
  sQuarantineExt : string;
  k : integer;
  bMoved : boolean;
  sQuarantineOriginalFilename : string;
  sQuarantineFilename : string;
  sMsg : string;
  sAlert : string;
begin
  bMoved := false;
  sDosFilename := '';
  k := 0;
     
     if CheckFileExists(sFilenameFullPath) then
     begin
       ResolveFileName(sFilenameFullPath, sLongFilename, bWide, wOriginalFullFilenamePath);

       if bBadFileInfo then
         begin
           sMsgDetectionType := sMsgSuspiciousOriginFile;
           sQuarantineExt := '.suspiciousorigin';
         end
       else
         case TypeOfDetection of
           Infected:
             begin
               sMsgDetectionType := 'virus';
               sQuarantineExt := '.infected';
             end;
           Suspicious:
             begin
               sMsgDetectionType := sMsgSuspiciousFile;
               sQuarantineExt := '.suspicious';
             end;
           Obfuscated:
             begin
               sMsgDetectionType := sMsgObfuscatedFile;
               sQuarantineExt := '.obfuscated';
             end;
           Crypted:
             begin
               sMsgDetectionType := sMsgCryptedFile;
               sQuarantineExt := '.crypted';
             end;
           badSignature:
             begin
               sMsgDetectionType := sMsgBadSignatureFile;
               sQuarantineExt := '.badsignature';
             end;
           SignatureExpired:
             begin
               sMsgDetectionType := sMsgSignExpired;
               sQuarantineExt := '.signatureexpired';
             end;
           badPEHeader:
             begin
               sMsgDetectionType := sMsgBadPEHeader;
               sQuarantineExt := '.badpeheader';
             end;
           SuspiciousExt:
             begin
               sMsgDetectionType := sMsgSuspiciousFile;
               sQuarantineExt := '.suspiciousext';
             end;
           else
             begin
               sMsgDetectionType := sMsgSuspiciousFile;
               sQuarantineExt := '.suspiciouselse';
             end;
         end;

       sFilePath := Trim(ExtractFilePath(sLongFileName));
       sLongFileName := Trim(ExtractFileName(sLongFileName));
       sFileMsg := sLongFilename;

       if bMoveToQuarantine then
       begin
         if (WhenInfectedFileIsFound = opMoveToQuarantine) and DirectoryExists(sPathQuarantine) then
         begin
           if bWide then
             begin
               wFileName := StringToWide(sFilenameFullPath);

               wQuarantineOriginalFilename := StringToWide(AddSlash(sPathQuarantine))
                  + WideExtractFileName(wOriginalFullFilenamePath)
                  + '_UseManualRestore'
                  + sQuarantineExt;

               wQuarantineFilename := wQuarantineOriginalFilename;

               while FileExistsW(wQuarantineFilename) do
               begin
                 wQuarantineFilename := wQuarantineOriginalFilename + IntToStr(k);
                 Inc(k);
               end;

               bMoved := CopyFileW(PWideChar(wFileName)
                                       ,PWideChar(wQuarantineFilename),false);


               if bMoved then
                 bMoved := FileExistsW(wQuarantineFilename);

               if bMoved and bIsHidden then
                 WideFileSetAttr(PWideChar(wQuarantineFilename), WideFileGetAttr(PWideChar(wQuarantineFilename)) xor faHidden xor faSysFile);

             end
           else
             begin
               sQuarantineOriginalFilename := AddSlash(sPathQuarantine)
                               + sLongFilename
                               + sQuarantineExt;
               sQuarantineFileName := sQuarantineOriginalFilename;

               while CheckFileExists(sQuarantineFileName) do
               begin
                 sQuarantineFileName := sQuarantineOriginalFilename + IntToStr(k);
                 Inc(k);
               end;

               bMoved := CopyFile(PChar(sFilenameFullPath),PChar(sQuarantineFileName),false);

               if bMoved then
                 bMoved := CheckFileExists(sQuarantineFileName);

               if bMoved and bIsHidden then
                 FileSetAttr(PChar(sQuarantineFileName), FileGetAttr(PChar(sQuarantineFileName)) xor faHidden xor faSysFile);
             end;

           if bMoved then
           begin
             FileSetAttr(sFilenameFullPath, faArchive);
             bMoved := DeleteFile(sFilenameFullPath);

             if bMoved then
               bMoved := not CheckFileExists(sFilenameFullPath);

             if not(bWide) then
             begin
               if bMoved then
                 CreateRecoverFile(sFilenameFullPath, sQuarantineFileName)
               else
                 begin
                   if CheckFileExists(sQuarantineFileName) then
                     DeleteFile(sQuarantineFileName);
                 end;
             end
             else
             begin
               if not(bMoved) and FileExistsW(wQuarantineFilename) then
                 DeleteFileW(PWideChar(wQuarantineFilename));
             end;
           end;

           sFileMsg := sLongFilename;

           if bWide then
             sDosFilename := ' (' + Trim(ExtractFileName(sFilenameFullPath)) + ') UseManualRestore';

           if bMoved then
           begin
             bBeScanned := false;

             sMsg := Format(sMsgFileChanged,
                              [sFilePath,sFileMsg + sDosFilename]);

             sAlert := Format(sMsgVirusMovedToQuarantine,[sMsgDetectionType]);

             if NotWin9x then
               begin
                 wMsg := FormatW(StringToWide(sMsgFileChanged),
                          [WideExtractFilePath(wOriginalFullFilenamePath)
                             ,WideExtractFileName(wOriginalFullFilenamePath)
                                              + StringToWide(sDosFilename)]);

                 ShowBalloonTipsW(StringtoWide(sAlert), wMsg, NIIF_WARNING
                                                    , iSEC_DURATION_VIRUS_ALERT);
               end
             else
               ShowBalloonTips(sAlert, sMsg, NIIF_WARNING
                                                   , iSEC_DURATION_VIRUS_ALERT);
             if bLog then
             begin
               Log(sAlert + CRLF + sMsg,sPathLog + LOGFILE_MESSAGES,true,true);
               Log(sAlert + CRLF + sMsg,sPathLog + LOGFILE_QUARANTINE,true,true);
             end;
           end;
         end;

         RemoveFromElabList(FilesMonitoredRecently,sFileNameFullPath);
         RemoveFromElabList(FilesNewFilesDetected,sFileNameFullPath);
       end;

       if not(bMoved) then
       begin
         sMsg := sMsgVerifySuspiciousFile + CRLF + CRLF
                    + Format(sMsgFileChanged,
                              [sFilePath,sFileMsg + sDosFilename]);

         if NotWin9x then
           begin
             wMsg := StringToWide(sMsgVerifySuspiciousFile + CRLF + CRLF)
                     + FormatW(StringToWide(sMsgFileChanged),
                         [WideExtractFilePath(wOriginalFullFilenamePath)
                             ,WideExtractFileName(wOriginalFullFilenamePath)
                               + StringToWide(sDosFilename)]);

             ShowBalloonTipsW(StringtoWide(ucFirst(sMsgDetectionType)), wMsg
                                      , NIIF_WARNING, iSEC_DURATION_VIRUS_ALERT);
           end
         else
             ShowBalloonTips(ucFirst(sMsgDetectionType),sMsg,NIIF_Info
                                                    , iSEC_DURATION_VIRUS_ALERT);
         if bLog then
           Log(ucFirst(sMsgDetectionType) + '.' + CRLF + sMsg,sPathLog + LOGFILE_MESSAGES,true,true);

         if bMoveToQuarantine and bLog then
           Log(ucFirst(sMsgDetectionType) + '.' + CRLF + sMsg,sPathLog + LOGFILE_QUARANTINE,true,true);
       end;
     end
       else  //file not exists
         bBeScanned := false;
end;

procedure TFClamSentinel.ThreadCheckMalwareDone(Sender: TObject);
var
  sFilenameFullPath : string;
  sUcaseFile : string;
  bIsProbablyMalware : boolean;
  bMoveToQuarantine : boolean;
  bBadFileInfo : boolean;
  resultSign : TVerifyResult;
  bSignVerified : boolean;
  iSize : Cardinal;
  TypeOfDetection : TDetectionsType;
  bHasPEFormat : boolean;
  bIsNewFile : boolean;
  bHasEmbeddedSignature : boolean;
  bIsHidden : boolean;
  FileType : TFileTypeDetected;
  iPointsForAllFiles : Cardinal;
  bIsOutsideMonitor : boolean;
  sExt : string;
//  sDrive : string;
  sFilePath : string;
  bHasValidSignature : boolean;
  //todo DateModify : TDateTime;
begin
  bBadFileInfo := false;

  sFilenameFullPath := TCheckMalwareThread(Sender).Filename;
  sUcaseFile := UpperCase(sFilenameFullPath);

  bIsProbablyMalware := TCheckMalwareThread(Sender).IsProbableMalware;
  bMoveToQuarantine := TCheckMalwareThread(Sender).MoveToQuarantine;
  bSignVerified := TCheckMalwareThread(Sender).SignatureVerified;
  resultSign := TCheckMalwareThread(Sender).SignatureVerifyResult;
  iSize := TCheckMalwareThread(Sender).Size;
  TypeOfDetection := TCheckMalwareThread(Sender).TypeOfDetection;
  bHasPEFormat := TCheckMalwareThread(Sender).HasPEFormat;
  bIsNewFile := TCheckMalwareThread(Sender).IsNewFile;
  bHasEmbeddedSignature := TCheckMalwareThread(Sender).HasEmbeddedSignature;
  bIsHidden := TCheckMalwareThread(Sender).IsHidden;
  FileType := TCheckMalwareThread(Sender).FileType;
  iPointsForAllFiles := TCheckMalwareThread(Sender).PointsForAllFiles;
  bIsOutsideMonitor := TCheckMalwareThread(Sender).IsOutsideMonitor;
  //todo DateModify := TCheckMalwareThread(Sender).DateModify;

  bHasValidSignature := (bSignVerified and (resultSign = SignOK));

  if (bHasEmbeddedSignature or bIsProbablyMalware) and not(bSignVerified) then
  begin
    resultSign := CheckFileTrust(sFilenameFullPath);
    bSignVerified := true;

    if resultSign = SignOK then
    begin
      bHasValidSignature := true;

      bIsProbablyMalware := false;
      bMoveToQuarantine := false;
    end;

    if (resultSign in [SignInvalid]) and not((WinVer = cOsWin8) and not(bHasEmbeddedSignature)) then
    begin
      bIsProbablyMalware := true;
      bMoveToQuarantine := true;

      iPointsForAllFiles := INFINITY_POINTS;
      TypeOfDetection := badSignature;
    end;

    if resultSign = SignExpired then
    begin
      bIsProbablyMalware := true;
      bMoveToQuarantine := false;

      TypeOfDetection := SignatureExpired;
    end;
  end;

  sExt := UpperCase(ExtractFileExt(sFilenameFullPath));

  if not(bSignVerified and (resultSign = SignOK)) and not(bIsProbablyMalware)
     and not(iSize > SIZE_LIMIT_IMPROBABLE)
     and (bHasPEFormat or (sExt = '.EXE') or (sExt = '.CHECKVIRUS')) then
  begin
    bBadFileInfo := not(GoodFileInfo(sFilenameFullPath, iSize, sExt));

    if bBadFileInfo then
    begin
      bIsProbablyMalware := true;
      bMoveToQuarantine := true;
      Inc(iPointsForAllFiles);

      if not(bSignVerified) then
        resultSign := CheckFileTrust(sFilenameFullPath);

      if resultSign = SignOK then
      begin
        bHasValidSignature := true;
        bIsProbablyMalware := false;
        bMoveToQuarantine := false;
      end;

      if (resultSign in [SignInvalid]) and not((WinVer = cOsWin8) and not(bHasEmbeddedSignature)) then
      begin
        bIsProbablyMalware := true;
        bMoveToQuarantine := true;

        iPointsForAllFiles := INFINITY_POINTS;
        TypeOfDetection := badSignature;
      end;

      if resultSign = SignExpired then
      begin
        bIsProbablyMalware := true;
        bMoveToQuarantine := false;

        TypeOfDetection := SignatureExpired;
      end;
    end;
  end;

  if bIsOutsideMonitor and bIsProbablyMalware and (iPointsForAllFiles < INFINITY_POINTS) then
  begin
    sFilePath := ExtractFilePath(sUcaseFile);

    if iSize < SIZE_LIMIT_ALL_FILES then
      Inc(iPointsForAllFiles);

    if StartWith(UpperCase(sSystemProgramFilesDir), sFilePath)
         or StartWith(UpperCase(sSystemWOWProgramFilesDir), sFilePath)
         or StartWith(UpperCase(sEnvironProgramFiles), sFilePath)
         or StartWith(UpperCase(sEnvironProgramW6432), sFilePath) then
      Inc(iPointsForAllFiles);

    //sDrive := AddSlash(ExtractFileDrive(sFilePath));

    //if (UpperCase(sSystemDrive) = sDrive) then
    //begin
    //  if Pos('\',Copy(RemoveSlash(sFilePath),length(sDrive)+1,MAXINT))=0 then
    //    Inc(iPointsForAllFiles);
    //end;

    if (FileType = IsExe) or (sExt = '.EXE') or (sExt = '.CHECKVIRUS') then
      Inc(iPointsForAllFiles);

    if TypeOfDetection = Crypted then
      Dec(iPointsForAllFiles);

    if TypeOfDetection = Obfuscated then
      Inc(iPointsForAllFiles);

    //if (TypeOfDetection = Suspicious) and (FileType = IsDll) then
    //begin
    //  if SecondsDiff(DateModify,now) > SecsPerDay*180 then //180 = 6 months
    //    Dec(iPointsForAllFiles);
    //end;

    if (TypeOfDetection in [Suspicious, badPEHeader, SuspiciousExt]) and (FileType = IsDll) then
      Dec(iPointsForAllFiles);

    if (iPointsForAllFiles < LIMIT_FOR_ALL_FILES) then
    begin
      bIsProbablyMalware := false;
      bMoveToQuarantine := false;
      TypeOfDetection := good;
    end;
  end;

  CallScanner_DetectNewExeFiles(bIsProbablyMalware, bIsNewFile, sFilenameFullPath,
     TypeOfDetection, bMoveToQuarantine, bBadFileInfo, FileType, bIsHidden, bHasValidSignature,
     bHasPEFormat);
end;

procedure TFClamSentinel.CallScanner_DetectNewExeFiles(const bIsProbablyMalware : boolean;
   const bIsNewFile : boolean; const sFilenameFullPath : string;
   const TypeOfDetection : TDetectionsType; const bMoveToQuarantine : boolean;
   const bBadFileInfo : boolean; const FileType : TFileTypeDetected;
   const bIsHidden : boolean; const bHasValidSignature : boolean;
   const bHasPEFormat : boolean);
var
  bBeScanned : boolean;

  sLongFilename : string;
  bWide : boolean;
  wOriginalFullFilenamePath : WideString;
  sFileMsg : string;
  sFilePath : string;
  sDosFilename : string;
  wMsg : WideString;

  sUcaseFile : string;
  sMsg : string;

  sExt : string;
begin
  //bBeScanned := true;
  bBeScanned := not(bHasValidSignature and bSkipFilesWithValidSignature);

  sUcaseFile := UpperCase(sFilenameFullPath);

  sExt := ExtractFileExt(sUCaseFile);

  if bIsProbablyMalware then
    ActionIsMalware(sFilenameFullPath, TypeOfDetection, bMoveToQuarantine, bBadFileInfo, bBeScanned, bIsHidden);

  if not(bIsProbablyMalware) and bIsNewFile
      and (bHasPEFormat or (FileType = IsExe) or (FileType = IsDll)) then
  begin
      if not(IsIntoElabList(FilesNewFilesDetected,sUCaseFile)) then
      begin
        ResolveFileName(sFilenameFullPath, sLongFilename, bWide, wOriginalFullFilenamePath);

        sFileMsg := Trim(ExtractFileName(sLongFilename));
        sFilePath := Trim(ExtractFilePath(sLongFilename));

        if bWide then
          sDosFilename := ' (' + Trim(ExtractFileName(sFilenameFullPath)) + ')';

        AddIntoElabList(FilesNewFilesDetected,sUCaseFile);

        sMsg := Format(sMsgNewFileCreated,[sFilePath,sFileMsg + sDosFilename]);

        if NotWin9x then
          begin
            wMsg := FormatW(StringToWide(sMsgNewFileCreated),
                 [WideExtractFilePath(wOriginalFullFilenamePath)
                    ,WideExtractFileName(wOriginalFullFilenamePath)
                                           + StringToWide(sDosFilename)]);

            if bMonitorWarnings then
              ShowBalloonTipsW(StringtoWide(sMsgModifiedFolder), wMsg
                        , NIIF_WARNING, SEC_DURATION_NEW_FILE_CREATED);
          end
        else
          begin
            if bMonitorWarnings then
               ShowBalloonTips(sMsgModifiedFolder,sMsg,NIIF_Info
                                        , SEC_DURATION_NEW_FILE_CREATED);
          end;

         if bLog then
           Log(sMsgModifiedFolder + CRLF + sMsg,sPathLog + LOGFILE_MESSAGES,true,true);
      end;
  end;

  if bBeScanned then
  begin
    if FSDetectionActive and NotSkipFile(sUCaseFile)
         and (IMustToScanIt(sUCaseFile)
              or (bHasPEFormat and not(IsExtInList(sUCaseFile,ExtToScan)))) then
    begin
      if iNumActiveScan < iMaxNumActiveScan then
        begin
          RunRealtimeScan('"'+ sUCaseFile +'"'); //start immediatly
          AddIntoElabList(FilesScannedRecently,sUCaseFile);
        end
      else
        ScanQueue.Add(sUCaseFile); //Add into the queue
    end;
  end;
end;

procedure TFClamSentinel.mnuQuarantineClick(Sender: TObject);
begin
  if DirectoryExists(sPathQuarantine) then
    ShellExec(sPathQuarantine);
end;

procedure TFClamSentinel.mnuMemoryLogClick(Sender: TObject);
begin
  ShowFile(sPathLog + LOGFILE_MEMORY_SCAN);
end;

procedure TFClamSentinel.mnuRealTimeLogClick(Sender: TObject);
begin
  ShowFile(sPathLog + LOGFILE_REALTIME_SCAN);
end;

procedure TFClamSentinel.mnuDriveScanLogClick(Sender: TObject);
begin
  ShowFile(sPathLog + LOGFILE_DRIVE_SCAN);
end;

procedure TFClamSentinel.mnuAboutClick(Sender: TObject);
begin
  if (Length(sMsgAboutInfo)>255) then
    ShowBalloonWin9x(sMsgVersionInfo,sMsgAboutInfo,NIIF_Info)
  else
    ShowBalloonTips(sMsgVersionInfo,sMsgAboutInfo,NIIF_Info, SEC_DURATION_ABOUT)
end;

procedure TFClamSentinel.mnuSettings_RunOnStartupClick(Sender: TObject);
begin
 sMyRegistryChange := 'S';

 if mnuSettings_RunOnStartup.Checked = false then
   RunOnWinStart(cProjectName, PChar(Application.Exename), false)
 else
   RemoveFromRunKey(cProjectName);

 mnuSettings_RunOnStartup.Checked := not(mnuSettings_RunOnStartup.Checked);
end;

procedure TFClamSentinel.ReloadIniFile;
begin
  InizializeParams;
end;

procedure TFClamSentinel.mnuClamSentinelWebsiteClick(Sender: TObject);
begin
  ShellExec(cURL);
end;

procedure TFClamSentinel.mnuSettings_DetectNewDrivesClick(Sender: TObject);
begin
  CheckUncheck(mnuSettings_DetectNewDrives, bDetectNewDrives);
  SaveSettings;
end;

procedure TFClamSentinel.mnuSettings_MemoryScanAtStartupClick(
  Sender: TObject);
begin
  CheckUncheck(mnuSettings_MemoryScanAtStartup, bMemoryScanAtStartup);
  SaveSettings;
end;

procedure TFClamSentinel.mnuSettings_LogClick(Sender: TObject);
begin
  CheckUncheck(mnuSettings_Log, bLog);
  SaveSettings;
end;

procedure TFClamSentinel.WMHotKey(var Msg: TWMHotKey);
begin
  if Msg.HotKey = idSkip then
  begin
    bSkipOn := not(bSkipOn);
    StartStopIcon(FSDetectionActive);
  end;
end;

procedure TFClamSentinel.mnuSettings_AskForScanNewDrivesClick(
  Sender: TObject);
begin
  CheckUncheck(mnuSettings_AskForScanNewDrives, bAskForScanNewDrives);
  SaveSettings;
end;

procedure TFClamSentinel.SaveSettings(bReadOnlyException : boolean = true);
var
  IniFile : TIniFile;
  cur : TCursor;
begin

cur := Screen.Cursor;

Screen.Cursor := crHourglass;

try
  IniFile := TIniFile.Create(sIniFile);
  try
    try
      IniFile.WriteInteger('Params', 'Detect_NewDrives', Ord(bDetectNewDrives));
      IniFile.WriteInteger('Params', 'AskForScan_NewDrives', Ord(bAskForScanNewDrives));
      IniFile.WriteInteger('Params', 'NotifyNewVersion', Ord(bNotifyNewVersion));
      IniFile.WriteInteger('Params', 'UseLocalIniFile', Ord(bUseLocalIniFile));
      IniFile.WriteInteger('Params', 'MemoryScanAtStartup', Ord(bMemoryScanAtStartup));
      IniFile.WriteInteger('Params', 'MonitorSystemChanges', Ord(bMonitorSystemChanges));
      IniFile.WriteInteger('Params', 'MonitorWarnings', Ord(bMonitorWarnings));
      IniFile.WriteInteger('Params', 'Log', Ord(bLog));

      IniFile.WriteInteger('Params', 'UseVxdOnWin98', Ord(bUseVxdOnWin98));
      IniFile.WriteInteger('Params', 'UseLocalIniFile', Ord(bUseLocalIniFile));

      IniFile.WriteString('Params', 'DirToScan', ConvertToComma(lstFixedDrivesToScan));
      IniFile.WriteString('Params', 'NoScan', ConvertToComma(lstNoScan));
      IniFile.WriteString('Params', 'FullScan', ConvertToComma(lstFullScan));
      IniFile.WriteString('Params', 'ExtToScan', ConvertToComma(ExtToScan));

      IniFile.WriteFloat('Params', 'MaxLogSizeMB', dblMaxLogSizeMB);

      IniFile.WriteInteger('Params', 'MaxNumActiveScan', iMaxNumActiveScan);

      IniFile.WriteInteger('Params', 'WhenInfectedFileIsFound', Ord(WhenInfectedFileIsFound));

      IniFile.WriteInteger('Params', 'SkipValidSignatures', Ord(bSkipFilesWithValidSignature));

    except
      {$IFNDEF VER120}
      on E : EIniFileException do
          if bReadOnlyException then TopMessage(Handle,Format(sMsgFileReadOnly
                              ,[sIniFile]),cProjectName,MB_ICONERROR);
      {$ENDIF}
      on E : Exception do
          if bReadOnlyException then TopMessage(Handle,E.Message,cProjectName,MB_ICONERROR);
    end;
  finally
    IniFile.Free;
  end;
except
    on E : Exception do TopMessage(Handle,E.Message,cProjectName,MB_ICONERROR);
end;
Screen.Cursor := Cur;
end;

procedure TFClamSentinel.CheckVersion(bAutomatic : boolean = false);
var
  sResult : string;
  sError : string;
  sMsg : string;
  bNewVersionAvailable : boolean;
  sAgent : string;
  sVer : string;

begin
  TimerCheckVersion.Enabled := false;

  bNewVersionAvailable := false;
  sVer := '';

  sAgent := cProjectName + ' ' + VERSION;

  case WinVer of
    cOSUnknown:       sVer := 'OSUnknown';
    cOSWin95:         sVer := 'OSWin95';
    cOSWin98:         sVer := 'OSWin98';
    cOSWin98SE:       sVer := 'OSWin98SE';
    cOSWinME:         sVer := 'OSWinME';
    cOSWinNT:         sVer := 'OSWinNT';
    cOSWin2k:         sVer := 'OSWin2k';
    cOSWinXP:         sVer := 'OSWinXP';
    cOSWin2k3:        sVer := 'OSWin2k3';
    cOSVista:         sVer := 'OSVista';
    cOSSeven:         sVer := 'OSSeven';
    cOSWin8:          sVer := 'OSWin8';
    cOSGenericWin9x:  sVer := 'OSGenericWin9x';
    cOSGenericWinNT:  sVer := 'OSGenericWinNT';
  end;

  sAgent := sAgent + ' ' + sVer;
  GetURL(cURL + 'Version.php', sAgent, sResult, sError);

  if sError = '' then
    begin
      sResult := trim(sResult);
      if sResult = VERSION then
        sMsg := sMsgHaveLatestVersion
      else
        begin
          sMsg := Format(sMsgNewVersionAvailable,[sResult]);
          bNewVersionAvailable := true;
        end;
    end
  else
    sMsg := sError;

  if not(bAutomatic) or (bAutomatic and bNewVersionAvailable) then
  begin
    if bAutomatic then
      sMsg := cProjectName + CRLF + CRLF + sMsg;

    ShowBalloonTips(sMsgCheckLatestVersion, sMsg, NIIF_Info, SEC_DURATION_LATEST_VERSION);

    if bAutomatic and bLog then
    begin
      Log(sMsgCheckLatestVersion + CRLF + sMsg,sPathLog + LOGFILE_MESSAGES, true, true);
    end;
  end;

  if bNotifyNewVersion then
  begin
    TimerCheckVersion.Interval := TIMER_LONG_INTERVAL;
    TimerCheckVersion.Enabled := true;
  end;
end;

procedure TFClamSentinel.mnuCheckVersionClick(Sender: TObject);
begin
  CheckVersion;
end;

function TFClamSentinel.notExcluded(const sFile : string): boolean;
  const
    aNoWinDirScan: array[0..2] of string = ('SYSTEM.INI','POWERPNT.INI','WAVEMIX.INI');
    aNoFileScan: array[0..0] of string = ('DESKTOP.INI');
  var
    i : integer;
begin
  result := true;

  i := Low(aNoWinDirScan);

  while (i<=High(aNoWinDirScan)) and result do
  begin
    if Pos(sWindowsDir + aNoWinDirScan[i], sFile)>0 then
      result := false;

    inc(i);
  end;

  i := Low(aNoFileScan);

  while (i<=High(aNoFileScan)) and result do
  begin
    if Pos('\' + aNoFileScan[i], sFile)>0 then
      result := false;

    inc(i);
  end;
end;

procedure TFClamSentinel.ThreadVxdDone(Sender: TObject);
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
  bStartImmediatly : boolean;
  sCurrentFile : string;
  sFileLong : string;
  sFiles : string;
  iFiles : integer;
  bMonitored : boolean;
begin
  if VxdThread = nil then
    exit;

  if FSDetectionActive then
  begin
    iFiles := 0;
    sFiles := '';

    sNext := TVxdThread(Sender).Filename;

    bStartImmediatly := (iNumActiveScan < iMaxNumActiveScan);

    While sNext <> '' do
    begin
       bMonitored := false;

       sCurrentFile := Trim(Explode(sNext));

       if sCurrentFile <> '' then
       begin
         sFileLong := WinAPI_GetLongPathName(sCurrentFile);

         if sFileLong <> '' then
           sCurrentFile := sFileLong;

         sCurrentFile := UpperCase(sCurrentFile);

         if not(bMonitorSystemChanges and IsDir(sCurrentFile)) then
           begin
             if NotSkipFile(sCurrentFile) then
             begin
               if bMonitorSystemChanges then
                 bMonitored := CheckIsDirectoryMonitored(sCurrentFile);

               if not(bMonitored) and IMustToScanIt(sCurrentFile) then
               begin
                 if bStartImmediatly and (iFiles < MAX_CLAMSCAN_FILES) then
                   begin
                     sFiles := sFiles + '"' + sCurrentFile + '" ';
                     AddIntoElabList(FilesScannedRecently,UpperCase(sCurrentFile));
                     Inc(iFiles);
                   end
                 else
                   ScanQueue.Add(sCurrentFile); //Add into the queue
               end;
             end;
           end
         else
           MonitorNewFolders(sCurrentFile);
       end;
    end;

    if bStartImmediatly and (sFiles<>'') then
      RunRealtimeScan(sFiles);
  end;
end;

procedure TFClamSentinel.StopVxdSleep;
begin
  MonitorActive(false);
  
  if IsWin9x then
  begin
    //Application.ProcessMessages;
    Sleep(VXD_INTERVAL);
    Application.ProcessMessages;
  end;
end;

procedure TFClamSentinel.mnuAdvancedSettings_UseVxdOnWin98Click(Sender: TObject);
begin
  CheckUncheck(mnuAdvancedSettings_UseVxdOnWin98, bUseVxdOnWin98);
  SaveSettings;

  StartStopVxd(bUseVxdOnWin98);
end;

procedure TFClamSentinel.SaveRestart;
var
 cur : TCursor;
begin
  cur := Screen.Cursor;
  Screen.Cursor := crHourglass;

  SaveSettings;

  if FSDetectionActive then
    MonitorActive(false);

  Application.ProcessMessages;
  Sleep(1000);

  ReloadIniFile;
    
  Screen.Cursor := cur;
end;

procedure TFClamSentinel.mnuAdvancedSettings_DrivesClick(Sender: TObject);
begin
  If FDrives = nil then
  begin
     ModalResult := mrNone;

     FDrives := TFDrives.create(self);
     FDrives.ShowModal;

     if ModalResult = mrOk then
       SaveRestart;

     ModalResult := mrNone;
  end;
end;

procedure TFClamSentinel.mnuAdvancedSettings_NoScanClick(Sender: TObject);
begin
  if FChangeList = nil then
  begin
     ModalResult := mrNone;

     FChangeList := TFChangeList.create(self);
     FChangeList.Settings(lstNoScan, ScanPaths);
     FChangeList.ShowModal;

     if ModalResult = mrOk then
       SaveRestart;

      ModalResult := mrNone;
  end;
end;

procedure TFClamSentinel.mnuAdvancedSettings_ExtensionsClick(Sender: TObject);
begin
  if FChangeList = nil then
  begin
     ModalResult := mrNone;

     FChangeList := TFChangeList.create(self);
     FChangeList.Settings(ExtToScan, Extensions);
     FChangeList.ShowModal;

     if ModalResult = mrOk then
       SaveRestart;

     ModalResult := mrNone;
  end;
end;

procedure TFClamSentinel.mnuAdvancedSettings_LogMaxSizeClick(Sender: TObject);
var
 sVal : string;
begin

 sVal := FloatToStr(dblMaxLogSizeMB);

 if InputQuery(cProjectName, sMsgMaxFilesizeForLogFile, sVal) then
   try
     dblMaxLogSizeMB := strtofloat(sVal);
     SaveSettings;

   except
     on e: EConvertError do
       TopMessage(Handle,sMsgValueNotAccepted,cProjectName,MB_ICONERROR);
   end;
end;

procedure TFClamSentinel.mnuAdvancedSettings_MaxActiveScanClick(
  Sender: TObject);
var
 sVal : string;
 iVal : Cardinal;
begin

 sVal := IntToStr(iMaxNumActiveScan);

 if InputQuery(cProjectName, sMsgMaxNumActiveScans, sVal) then
   try
     iVal := strtoint(sVal);

     if (iVal > 0) and (iVal < 11) then
       begin
         iMaxNumActiveScan := iVal;
         SaveSettings;
       end
     else
       TopMessage(Handle,sMsgValueNotAccepted,cProjectName,MB_ICONERROR);
       
   except
     on e: EConvertError do
       TopMessage(Handle,sMsgValueNotAccepted,cProjectName,MB_ICONERROR);
   end;
end;

procedure TFClamSentinel.mnuInfectedFiles_MoveToQuarantineClick(
  Sender: TObject);
begin
  WhenInfectedFileIsFound := opMoveToQuarantine;

  SaveSettings;
end;

procedure TFClamSentinel.mnuInfectedFiles_ReportOnlyClick(Sender: TObject);
begin
  WhenInfectedFileIsFound := opReportOnly;

  SaveSettings;
end;

procedure TFClamSentinel.mnuMessagesLogClick(Sender: TObject);
begin
  ShowFile(sPathLog + LOGFILE_MESSAGES);
end;

procedure TFClamSentinel.TimerCheckVersionTimer(Sender: TObject);
begin
  if IsConnectedToInternet then
    CheckVersion(true)
  else
    begin
      if TimerCheckVersion.Interval = TIMER_LONG_INTERVAL then
        TimerCheckVersion.Interval := TIMER_SHORT_INTERVAL;
    end;
end;

procedure TFClamSentinel.mnuAdvancedSettings_FullScanClick(
  Sender: TObject);
begin
  if FChangeList = nil then
  begin
     ModalResult := mrNone;

     FChangeList := TFChangeList.create(self);
     FChangeList.Settings(lstFullScan, FullScanFolders);
     FChangeList.ShowModal;

     if ModalResult = mrOk then
       SaveRestart;

      ModalResult := mrNone;
  end;
end;

procedure TFClamSentinel.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := bCanClose;
end;

procedure TFClamSentinel.WMPOWERBROADCAST(var Msg: TMessage); //message WM_POWERBROADCAST;
const
  PBT_APMSUSPEND = $0004;
  PBT_APMRESUMEAUTOMATIC = $0012;
begin
  case msg.WParam of
    PBT_APMSUSPEND:
      begin
        bRestartOnResume := FSDetectionActive;

        if bRestartOnResume then
          MonitorActive(false);
      end;
    PBT_APMRESUMEAUTOMATIC:
      begin
        if bRestartOnResume then
          ReloadIniFile;
      end;
  end;

  inherited;
end;

procedure TFClamSentinel.WMQUERYENDSESSION(var Msg: TMessage); //message WM_QUERYENDSESSION;
begin
  bCanClose := true;
  inherited;
  StopVxdSleep;
  Close;
end;

procedure TFClamSentinel.mnuSettings_NotifyNewVersionClick(
  Sender: TObject);
begin
  CheckUncheck(mnuSettings_NotifyNewVersion, bNotifyNewVersion);

  if not(mnuSettings_NotifyNewVersion.Checked) then
    TimerCheckVersion.Enabled := false;

  SaveSettings;
end;

procedure TFClamSentinel.LoadRegMonitors;
begin
  CreateNewRegMonitor(HKEY_LOCAL_MACHINE, 'Software\Microsoft\Windows NT\CurrentVersion\Winlogon\Userinit');
  CreateNewRegMonitor(HKEY_LOCAL_MACHINE, 'Software\Microsoft\Windows NT\CurrentVersion\Winlogon\Shell');

  CreateNewRegMonitor(HKEY_LOCAL_MACHINE, 'Software\Microsoft\Active Setup\Installed Components');

  CreateNewRegMonitor(HKEY_CURRENT_USER, 'Software\Microsoft\Windows NT\CurrentVersion\Windows\Run');
  CreateNewRegMonitor(HKEY_CURRENT_USER, 'Software\Microsoft\Windows NT\CurrentVersion\Windows\Load');

  CreateNewRegMonitor(HKEY_LOCAL_MACHINE, 'Software\Microsoft\Windows\CurrentVersion\RunOnce');
  CreateNewRegMonitor(HKEY_LOCAL_MACHINE, 'Software\Microsoft\Windows\CurrentVersion\RunOnceEx');

  CreateNewRegMonitor(HKEY_CURRENT_USER, 'Software\Microsoft\Windows\CurrentVersion\RunOnce');
  CreateNewRegMonitor(HKEY_CURRENT_USER, 'Software\Microsoft\Windows\CurrentVersion\RunOnceEx');

  CreateNewRegMonitor(HKEY_LOCAL_MACHINE, 'Software\Microsoft\Windows\CurrentVersion\Run');
  CreateNewRegMonitor(HKEY_CURRENT_USER, 'Software\Microsoft\Windows\CurrentVersion\Run');

  CreateNewRegMonitor(HKEY_CURRENT_USER, 'Software\Microsoft\Windows\CurrentVersion\RunServices');

  CreateNewRegMonitor(HKEY_CURRENT_USER, 'Software\Microsoft\Windows\CurrentVersion\RunServicesOnce');

  CreateNewRegMonitor(HKEY_LOCAL_MACHINE, 'Software\Microsoft\Windows\CurrentVersion\ShellServiceObjectDelayLoad');

  CreateNewRegMonitor(HKEY_LOCAL_MACHINE, 'Software\Policies\Microsoft\Windows\System\Scripts');
  CreateNewRegMonitor(HKEY_CURRENT_USER, 'Software\Policies\Microsoft\Windows\System\Scripts');

  CreateNewRegMonitor(HKEY_LOCAL_MACHINE, 'Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\Run');
  CreateNewRegMonitor(HKEY_CURRENT_USER, 'Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\Run');

  CreateNewRegMonitor(HKEY_LOCAL_MACHINE, 'Software\Microsoft\Windows NT\CurrentVersion\Image File Execution Options');

  if not (WinVer = cOsWinME) then
    CreateNewRegMonitor(HKEY_CURRENT_USER, 'Software\Microsoft\Windows\CurrentVersion\Explorer\MountPoints');

  CreateNewRegMonitor(HKEY_LOCAL_MACHINE, 'Software\Microsoft\Windows\CurrentVersion\RunServices');
  CreateNewRegMonitor(HKEY_LOCAL_MACHINE, 'Software\Microsoft\Windows\CurrentVersion\RunServicesOnce');
  CreateNewRegMonitor(HKEY_LOCAL_MACHINE, 'System\CurrentControlSet\Control\MPRServices');
  CreateNewRegMonitor(HKEY_LOCAL_MACHINE, 'System\CurrentControlSet\services\VxD');
end;

procedure TFClamSentinel.DeleteRegMonitors;
var
  i : integer;
begin
  for i := Low(aRegMonThds) to High(aRegMonThds) do
  begin
    if aRegMonThds[i] <> nil then
    begin
      with aRegMonThds[i] do
      begin
        OnTerminate := nil;
        Terminate;
        aRegMonThds[i] := nil;
      end;
    end;
  end;
  sMyRegistryChange := '';
end;

function TFClamSentinel.CreateNewRegMonitor(const hkRootKey : HKey; const sKey : string) : integer;
var
  i : integer;
  FRegMon : TRegistry;
  bExists : boolean;
begin
  result := -1;

  FRegMon := TRegistry.Create;

  try
    FRegMon.RootKey := hkRootKey;

    bExists := FRegMon.OpenKeyReadOnly(sKey);
  finally
    FRegMon.CloseKey;
    FRegMon.Free;
  end;

  if not(bExists) then
    exit;

  if aRegMonThds[0]=nil then
    i:=0
  else
    i := High(aRegMonThds)+1;

  SetLength(aRegMonThds,i+1);

  aRegMonThds[i] := TRegMonThread.Create;

  with aRegMonThds[i] do
  begin
    OnTerminate := ThreadRegMonDone;
    Key := sKey;
    RootKey := hkRootKey;
    Idx := i;
    Resume;
  end;

  result := i;
end;

procedure TFClamSentinel.ThreadRegMonDone(Sender: TObject);
var
  sKey : string;
  sMsg : string;
  sDiff : string;
begin

  if not bMonitorSystemChanges then
    exit;

  with TRegMonThread(Sender) do
  begin
    if aRegMonThds[idx] <> nil then
    begin
      if sMyRegistryChange='' then
        begin
          sKey := RootKeyString + '\' + Key;

          sMsg := Format(sMsgRegistryChanged,[sKey]);

          sDiff := FindDifferences;

          if sDiff <> '' then
          begin
            DoSnapshot;
            sMsg := sMsg + CRLF + sDiff;

            if bMonitorWarnings then
              ShowBalloonTips(sMsgModifiedRegistry,sMsg,NIIF_Info, iSEC_DURATION_REGISTRY_ALERT);

            if bLog then
              Log(sMsgModifiedRegistry + CRLF + sMsg,sPathLog + LOGFILE_MESSAGES,true,true);
          end;
        end
      else
        sMyRegistryChange := '';
    end;
  end;
end;

procedure TFClamSentinel.mnuSettings_MonitorSystemChanges_DetectionWithWarningsClick(
  Sender: TObject);
begin
  bMonitorSystemChanges := true;
  bMonitorWarnings := true;

  SaveSettings;
  StartStopIcon(FSDetectionActive);

  if aRegMonThds[0] = nil then
    LoadRegMonitors;
end;

procedure TFClamSentinel.mnuSettings_MonitorSystemChanges_DetectionOnlyClick(
  Sender: TObject);
begin
  bMonitorSystemChanges := true;
  bMonitorWarnings := false;

  SaveSettings;
  StartStopIcon(FSDetectionActive);

  if aRegMonThds[0] = nil then
    LoadRegMonitors;
end;

procedure TFClamSentinel.mnuSettings_MonitorSystemChanges_NoDetectionClick(
  Sender: TObject);
begin
  bMonitorSystemChanges := false;
  bMonitorWarnings := false;
  SaveSettings;
  StartStopIcon(FSDetectionActive);
end;

procedure TFClamSentinel.mnuQuarantineLogClick(Sender: TObject);
begin
  ShowFile(sPathLog + LOGFILE_QUARANTINE);
end;

procedure TFClamSentinel.mnuOpenRecoverClick(Sender: TObject);
begin
  ShellExec(sExePath+'SentinelRecover.exe');
end;

procedure TFClamSentinel.mnuSettings_MonitorSystemChanges_SkipFilesWithValidSignatureClick(
  Sender: TObject);
begin
  CheckUncheck(mnuSettings_MonitorSystemChanges_SkipFilesWithValidSignature, bSkipFilesWithValidSignature);
  SaveSettings;
end;

procedure TFClamSentinel.mnuDownloadDBUpdateClick(Sender: TObject);
begin
  ShellExec(ExtractFilePath(sScannerLocation)+'clamwin.exe',' --mode=update');
end;

procedure TFClamSentinel.FormPaint(Sender: TObject);
begin
  Hide;
end;

procedure TFClamSentinel.ThreadOptionScanDone(Sender: TObject);
var
 sOut : string;
 sErr : string;
begin
  sOut := Trim(TScanThread(Sender).Output);
  sErr := Trim(TScanThread(Sender).Error);

  if Pos('--archive-verbose',sOut)>0 then
    bArchiveVerbose := true;
end;

end.
