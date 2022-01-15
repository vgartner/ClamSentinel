unit UClamWinScanner;

interface

uses SysUtils, ShellApi, Windows, IniFiles, Utility, ScanThds, TrayIcon, Forms
     , Sentinel, Sign;

  const
    LOCAL_PATH_CLAMWIN_PORTABLE = '..\..\..\data\settings';
    FILE_HEAD = '\\?\';

  type TScanOp = (opFileScan, opMemoryScan, opDriveScan);

  procedure StartScanner(const sFiles : string;
                const sLogFile : string;
                const sExtraParams : string;
                const ShowCmd : integer;
                const bUseThread : boolean = false;
                const bPause : boolean = false;
                const opScan : TScanOp = opFileScan;
                const sBatchFile : string = '');

  procedure ExtractVirusAlert(const sIn : string; var sAlert : string;
                                           var DetectType : TDetectType);

  function ReadClamWinConfiguration(sPathClamWinConf : string) : boolean;

  function IsScannerTemp(const sFile : string): boolean;

  function IsClamWinPortable : boolean;

  procedure InitClamWin;

  procedure CreateRecoverFile(const sOrigFile : string; const sQuarFile : string);

  procedure GetScannerOptions;
var
  sPathDB : string;
  sScannerLocation : string;
  ClamScan_MaxFileSize : Cardinal;
  ClamScan_MaxScanSize : Cardinal;
  ClamScan_MaxFiles : Cardinal;
  ClamScan_MaxRecursion : Cardinal;
  sScannerPriority : string;
  bArchiveVerbose : boolean;

implementation

uses Start, UVersion;

procedure InitClamWin;
begin
  SCANNER_CONF_FILE := 'ClamWin.conf';
  CurrentScanner := scanner_ClamWin;
  bArchiveVerbose := false;
end;

procedure StartScanner(const sFiles : string;
             const sLogFile : string;
             const sExtraParams : string;
             const ShowCmd : integer;
             const bUseThread : boolean = false;
             const bPause : boolean = false;
             const opScan : TScanOp = opFileScan;
             const sBatchFile : string = '');
var
  sOptions : string;

  FBatch : TextFile;
  bExecute : boolean;
  sBatchTitle : string;
begin

  sOptions := '--database="';

  sOptions := sOptions + sPathDB; //otherwise use the db dir

  sOptions := sOptions + '"';

  if FClamSentinel.WhenInfectedFileIsFound = opMoveToQuarantine then
    sOptions := sOptions + ' --move="'+ FClamSentinel.sPathQuarantine +'"';

  if FClamSentinel.bLog and not(bUseThread) then
    sOptions := sOptions + ' --log="'+ FClamSentinel.sPathLog + sLogFile +'"';

  if ClamScan_MaxFileSize>0 then
    sOptions := sOptions + ' --max-filesize='+inttostr(ClamScan_MaxFileSize)+'M';

  if ClamScan_MaxScanSize>0 then
    sOptions := sOptions + ' --max-scansize='+inttostr(ClamScan_MaxScanSize)+'M';

  if ClamScan_MaxFiles>0 then
    sOptions := sOptions + ' --max-files='+inttostr(ClamScan_MaxFiles);

  if ClamScan_MaxRecursion>0 then
    sOptions := sOptions + ' --max-recursion='+inttostr(ClamScan_MaxRecursion);

  if bArchiveVerbose then
    sOptions := sOptions + ' --archive-verbose';

  sOptions := sOptions + ' ' + sExtraParams;

  if sFiles <> '' then
    sOptions := sOptions + ' ' + sFiles;

  if (bPause or (opScan in [opMemoryScan,opDriveScan])) and not(bUseThread) then
    begin
      AssignFile(FBatch,sBatchFile);
      try
        Rewrite(FBatch);
        WriteLn(FBatch,'@ECHO OFF');

        sBatchTitle := cProjectName;

        case opScan of
          opMemoryScan:
            sBatchTitle := cProjectName + ' - ' + FClamSentinel.sMsgMemoryScanning;

          opDriveScan:
            sBatchTitle := cProjectName + ' - ' + Format(FClamSentinel.sMsgDriveScanning
                     ,[StringReplace(sFiles,':\','',[rfReplaceAll])]);
        end;
        WriteLn(FBatch,'ECHO '+ Ansi2Dos(sBatchTitle));

        if FClamSentinel.NotWin9x then
        begin
          WriteLn(FBatch,'@TITLE '+ Ansi2Dos(sBatchTitle));
          WriteLn(FBatch,'COLOR 70');
        end;

        WriteLn(FBatch,'CALL "' + sScannerLocation + '" ' + sOptions);

        if bPause then
          WriteLn(FBatch,'PAUSE');

        WriteLn(FBatch,'CLS');
        WriteLn(FBatch,'EXIT');

        bExecute := true;
      finally
        CloseFile(FBatch);
      end;

      if bExecute then
      begin
        //if NotWin9x then
        //  ShellExecuteW(FClamSentinel.Handle, 'open', PWideChar(StringToWide(sBatchFile)), nil, nil, ShowCmd)
        //else
          ShellExecute(FClamSentinel.Handle, 'open', PChar(sBatchFile), nil, nil, ShowCmd);
      end;
    end
  else
    if bUseThread then
      begin
        if not(FClamSentinel.bSkipOn) then
        begin
          Inc(FClamSentinel.iNumActiveScan);

          with TScanThread.Create(PChar(sScannerLocation),PChar(' ' + sOptions)
                              ,FClamSentinel.sPathLog + sLogFile,FClamSentinel.ShowCmdScan
                              ,sScannerPriority, sFiles) do
             OnTerminate := FClamSentinel.ThreadScanDone;

          NewIcon(LoadIcon(HInstance,'SCAN_ACTIVE'));
          ChangeToolTip(FClamSentinel.sTitle + ' - ' + FClamSentinel.sMsgScanning);
        end;
      end
    else
      ShellExecute(FClamSentinel.Handle, 'open', PChar(sScannerLocation)
         , PChar(sOptions), nil, ShowCmd)
end;

procedure ExtractVirusAlert(const sIn : string; var sAlert : string; var DetectType : TDetectType);
const
  cFOUND = ' FOUND';
  cWARNING = 'LibClamAV Warning: ';
  cCANNOTBEMOVED = ' cannot be moved';
  CRLF = #13#10;

var
 iPosStop : integer;
 iPos : integer;
 sLine : string;
 bFound : boolean;
 sMsg : string;
 s : string;
 iLenFound : integer;
 iLenWarning : integer;
 iLenFileHead : integer;

 sFile : string;
 bDummy : boolean;
begin

DetectType := detect_none;

iLenFound := Length(cFOUND);

s := sIn + CRLF;

sMsg := '';

iPosStop := Pos(CRLF,s);

while (iPosStop > 0) do
begin
  sLine := copy(s,0,iPosStop-1);

  if Copy(sLine,Length(sLine)-iLenFound+1,iLenFound) = cFOUND then
    bFound := true
  else
    bFound := false;

  if not bFound then
  begin
    iLenWarning := Length(cWARNING);

    if Copy(sLine,1,iLenWarning) = cWARNING then
      begin
        iPos := Pos(cCANNOTBEMOVED, sLine);

        if iPos > 0 then
        begin
          sFile := Copy(Copy(sLine,1,iPos-1),iLenWarning+1,MAXINT);

          iLenFileHead := Length(FILE_HEAD);

          if Copy(sFile,1,iLenFileHead) = FILE_HEAD then
            sFile := Copy(sFile,iLenFileHead+1,MAXINT);

          FClamSentinel.ActionIsMalware(sFile,Infected,True,False,bDummy,True);
        end;
      end
  end;

  if bFound then
  begin
    if bFound and (DetectType<>detect_Virus) then
    begin
      if Pos('FALSE POSITIVE FOUND',sLine)>0 then
        DetectType := detect_FalsePositive
      else
        DetectType := detect_Virus;
    end;

    if sMsg <> '' then
      sMsg := sMsg + CRLF;

    sMsg := sMsg + sLine;

    if Copy(sMsg,Length(sMsg)-1,2)=CRLF then
      sMsg := Copy(sMsg,1,Length(sMsg)-2);
  end;

  s := copy(s,iPosStop+2,MAXINT);
  iPosStop := Pos(CRLF,s);
end;

sAlert := sMsg;

end;

function ReadClamWinConfiguration(sPathClamWinConf : string) : boolean;
var
 IniFileClamWinConfig : TIniFile;
 sFile : string;
begin
   result := true;

   sFile := sPathClamWinConf + SCANNER_CONF_FILE;

   if CheckFileExists(sFile) then
     begin
       IniFileClamWinConfig  := TIniFile.Create(sFile);

       try
         sScannerLocation :=
              ExpandEnvironment(IniFileClamWinConfig.ReadString('ClamAV', 'clamscan',  ' '));

         sPathDB :=
              ExpandEnvironment(IniFileClamWinConfig.ReadString('ClamAV', 'database',  ' '));

         FClamSentinel.sPathQuarantine :=
              ExpandEnvironment(IniFileClamWinConfig.ReadString('ClamAV', 'quarantinedir',  ' '));

         if trim(RemoveSlash(FClamSentinel.sPathLog))='' then
           FClamSentinel.sPathLog :=
                AddSlash(ExpandEnvironment(ExtractFilePath(IniFileClamWinConfig.ReadString('ClamAV','logfile',' '))))
         else
           FClamSentinel.sPathLog := AddSlash(FClamSentinel.sPathLog);

         ClamScan_MaxFileSize := IniFileClamWinConfig.ReadInteger('ClamAV','maxfilesize',0);
         ClamScan_MaxScanSize := IniFileClamWinConfig.ReadInteger('ClamAV','maxscansize',0);
         ClamScan_MaxFiles := IniFileClamWinConfig.ReadInteger('ClamAV','maxfiles',0);
         ClamScan_MaxRecursion := IniFileClamWinConfig.ReadInteger('ClamAV','maxrecursion',0);

         sScannerPriority := UpperCase(IniFileClamWinConfig.ReadString('ClamAV', 'priority',  'Low'));

         GetScannerOptions;
       finally
         IniFileClamWinConfig.Free;
       end;
     end
   else
     begin
       TopMessage(FClamSentinel.Handle,FClamSentinel.sMsgClamWinConfNotFound + CRLF + CRLF + FClamSentinel.sIniFile
                    ,cProjectName,MB_ICONERROR);
       ShowFile(FClamSentinel.sIniFile);
       result := false;
     end;
end;

function IsScannerTemp(const sFile : string): boolean;
  const CLAM_PREFIX = '.clamtmp';
  var
    bClamScanTmp : boolean;
    iLen : integer;
    sPath : string;
begin
  bClamScanTmp := (Pos('clamav-',Lowercase(sFile))>0);

  if bClamScanTmp then
  begin
    if not(Lowercase(ExtractFileExt(sFile)) = CLAM_PREFIX) then
    begin
      sPath := LowerCase(ExtractFilePath(sFile));
      iLen := Length(CLAM_PREFIX)+1;
      bClamScanTmp := (LowerCase(Copy(sPath,Length(sPath)-iLen+1,iLen)) = CLAM_PREFIX + '\');
    end;
  end;

  Result := bClamScanTmp;
end;

function IsClamWinPortable : boolean;
var
 sExePath : string;
begin
  sExePath := ExtractFilePath(Application.ExeName);
  result := CheckFileExists(sExePath+LOCAL_PATH_CLAMWIN_PORTABLE+'\'+SCANNER_CONF_FILE);
end;

procedure CreateRecoverFile(const sOrigFile : string; const sQuarFile : string);
  var
   F : TextFile;
   sRecoverFile : string;
begin
  if sQuarFile <> '' then
  begin
    sRecoverFile := sQuarFile + '.txt';

    try
      AssignFile(F,sRecoverFile);
      try
        Rewrite(F);
        Write(F, FILE_HEAD + sOrigFile + #9 + FILE_HEAD + sQuarFile);
      finally
        CloseFile(F);
      end;
    except
    end;
  end;
end;

procedure GetScannerOptions;
begin
  with TScanThread.Create(PChar(sScannerLocation),PChar(' -h')
                          ,'',FClamSentinel.ShowCmdScan
                          ,sScannerPriority, '') do
  OnTerminate := FClamSentinel.ThreadOptionScanDone;
end;

end.
