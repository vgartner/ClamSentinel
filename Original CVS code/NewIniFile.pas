unit NewIniFile;

interface

const LAST_PATCH = 3;

  procedure WriteNewIniFile(const sFile : string);
  procedure PatchIniFile;

implementation

uses Utility, SysUtils, UClamWinScanner, Sentinel, Start, UVersion, Classes, IniFiles;

procedure WriteNewIniFile(const sFile : string);
var
 FOut : TextFile;
 WinVer: TWindowsVersion;

 sPathClamWin9x, sPathClamWinNT, sPathClamWinPortable : string;

 sNoScan9x,sNoScan2000,sNoScanVista : string;
begin
  WinVer := GetWindowsVersion;

  if WinVer in [cOSWin95, cOSWin98, cOSWin98SE, cOSWinME, cOSGenericWin9x] then
    AssignFile(Fout, LowerCase(sFile))
  else
    AssignFile(Fout, sFile);

  try
    ReWrite(FOut);

    WriteLn(FOut, ';==========================================================================================================');
    WriteLn(FOut, ';If you edit this file manually you MUST restart (Stop-Start) ' + cProjectName + ' to have the new settings');
    WriteLn(FOut, ';==========================================================================================================');
    WriteLn(FOut, ';The semicolons in the ini file just precede the instructions/comments.');
    WriteLn(FOut, ';The configuration is done on the lines that don''t have the semicolons.');
    WriteLn(FOut, '');
    WriteLn(FOut, '[Params]');
    WriteLn(FOut, '');
    WriteLn(FOut, 'Patch='+inttostr(LAST_PATCH));
    WriteLn(FOut, '');
    WriteLn(FOut, ';### Use local ini file (1=yes; 0=no)');
    WriteLn(FOut, 'UseLocalIniFile=0');
    WriteLn(FOut, '');
    WriteLn(FOut, ';===========Paths Settings ========================================================');
    WriteLn(FOut, '');
    WriteLn(FOut, ';### Into the pathname all environment variables are replaced with the current value');
    WriteLn(FOut, '');
    WriteLn(FOut, ';### Path of the directory that contains the file '+SCANNER_CONF_FILE);
    WriteLn(FOut, '');

    sPathClamWin9x := 'PathClamWin=' + FClamSentinel.sSystemAppData + '.clamwin\';
    sPathClamWinNT := 'PathClamWin=%APPDATA%\.clamwin\';

    sPathClamWinPortable := 'PathClamWin=' + LOCAL_PATH_CLAMWIN_PORTABLE;

    if IsClamWinPortable then
      begin
        sPathClamWinNT := ';' + sPathClamWinNT;
        sPathClamWin9x := ';' + sPathClamWin9x;
      end
    else
      begin
        sPathClamWinPortable := ';' + sPathClamWinPortable;

        case WinVer of
          cOSWin95, cOSWin98, cOSWin98SE, cOSWinME, cOSGenericWin9x:
            sPathClamWinNT := ';' + sPathClamWinNT;

          cOSWinNT, cOSWin2k, cOSWinXP, cOSWin2k3, cOSVista, cOSSeven, cOsWin8, cOSGenericWinNT:
            sPathClamWin9x := ';' + sPathClamWin9x;
        end;
    end;

    WriteLn(FOut, ';***** on Windows 98/ME');
    WriteLn(FOut, sPathClamWin9x);
    WriteLn(FOut, '');
    WriteLn(FOut, ';***** on Windows NT/XP/2000, Vista, Windows 7');
    WriteLn(FOut, sPathClamWinNT);
    WriteLn(FOut, '');
    WriteLn(FOut, ';***** on ClamWin Portable');
    WriteLn(FOut, sPathClamWinPortable);
    WriteLn(FOut, '');
    WriteLn(FOut, '');
    
    WriteLn(FOut, ';### Drives that you want to monitor');
    WriteLn(FOut, ';### note: the program monitor all subfolders');
    WriteLn(FOut, ';### note: separate the values with a comma (without ") for example: DirToScan=C:\,D:\,E:\,F:\,\\mynetmachine\myfolder,\\mynetmachine2\%USERNAME%');
    WriteLn(FOut, 'DirToScan=' + ConvertToComma(FClamSentinel.FixedDrivesToScan));
    WriteLn(FOut, '');
    WriteLn(FOut, ';### Directories or files that you don''t want to monitor');
    WriteLn(FOut, ';### For example you don''t want to scan the recent folder');
    WriteLn(FOut, ';### that change dinamically very often (case insensitive)');
    WriteLn(FOut, ';### note: the program don''t scans all subfolders');
    WriteLn(FOut, ';### note: separate the values with a comma (without ").');

    sNoScan9x    := 'NoScan=' + ExpandEnvironment(AddSlash(GetSystemPath('Recent')));
    sNoScan2000  := 'NoScan=%USERPROFILE%\Recent\';
    sNoScanVista := 'NoScan=%APPDATA%\Microsoft\Windows\Recent,*.RBF,*.RBS';

    WriteLn(FOut, '');
    case WinVer of
      cOSWin95, cOSWin98, cOSWin98SE, cOSWinME, cOSGenericWin9x:
        begin
          WriteLn(FOut, ';***** on Windows 98/ME');
          WriteLn(FOut, sNoScan9x);
          WriteLn(FOut, '');
          WriteLn(FOut, ';***** on Windows NT/XP/2000');
          WriteLn(FOut, ';' + sNoScan2000);
          WriteLn(FOut, '');
          WriteLn(FOut, ';***** on Vista, Windows 7');
          WriteLn(FOut, ';' + sNoScanVista);
        end;

      cOSWinNT, cOSWin2k, cOSWinXP, cOSWin2k3:
        begin
          WriteLn(FOut, ';***** on Windows 98/ME');
          WriteLn(FOut, ';' + sNoScan9x);
          WriteLn(FOut, '');
          WriteLn(FOut, ';***** on Windows NT/XP/2000');
          WriteLn(FOut, sNoScan2000);
          WriteLn(FOut, '');
          WriteLn(FOut, ';***** on Vista, Windows 7');
          WriteLn(FOut, ';' + sNoScanVista);
        end;

      cOSVista, cOSSeven, cOsWin8, cOSGenericWinNT:
        begin
          WriteLn(FOut, ';***** on Windows 98/ME');
          WriteLn(FOut, ';' + sNoScan9x);

          WriteLn(FOut, ';***** on Windows NT/XP/2000');
          WriteLn(FOut, ';' + sNoScan2000);

          WriteLn(FOut, ';***** on Vista, Windows 7');
          WriteLn(FOut, sNoScanVista);
        end;
    end;
    WriteLn(FOut, '');
    WriteLn(FOut, ';### Directories where to scan any file');
    WriteLn(FOut, ';### note: the program scans all subfolders');
    WriteLn(FOut, ';### note: separate the values with a comma (without ").');
    WriteLn(FOut, 'FullScan=');

    WriteLn(FOut, '');

    WriteLn(FOut, ';===========Scan Settings ========================================================');
    WriteLn(FOut, '');
    WriteLn(FOut, ';### File extensions that you want to scan');
    Write(FOut, 'ExtToScan=.ACE,.ACM,.ACV,.ARC,.ARJ,.ASD,.ADD,.APP,.ASP,.AVB,.AX,.BAT,.BIN,.BOO,.BTM,.CAB');
    Write(FOut, ',.CHM,.CLA,.CLASS,.CDR,.CNV,.CMD,.COM,.CPL,.CPT,.CRT,.CSC,.CSH,.CTL,.DLL,.DOC,.DOT');
    Write(FOut, ',.DRV,.DVB,.DWG,.DMD,.EMAIL,.EML,.EXE,.FON,.FLT,.FOT,.GMS,.GVB,.HLP,.HTT,.HTA,.INF,.INI');
    Write(FOut, ',.INS,.ISP,.IST,.JSE,.JSP,.KSH,.LIB,.LNK,.MHT,.MHTM,.MHTML,.MSC,.MSI,.MSO,.MSP,.MST,.OBJ');
    Write(FOut, ',.OCX,.OVL,.OV?,.PCI,.PCD,.PGM,.PIF,.PI,.PH,.PHTM,.PL,.PLX,.PM,.PWZ,.PRG,.REG,.SCR,.SCF');
    Write(FOut, ',.SPL,.SH,.SHB,.SHS,.SCT,.SHM,.SMM,.SYS,.SWF,.URL,.VB,.VBA,.VBE,.VBS,.VBX,.VXD,.VS');
    Write(FOut, ',.TMP,.ZIP,.RAR,.JAR,.GZ,.RTF,.PDF,.AVI,.JS');
    Write(FOut, ',.WSC,.WS,.WSH,.WIZ,.WSF,.386,.3D,.3GR,.JOB,.ASPX');
    Write(FOut, ',.HTM,.HTML,.PPT,.XLS,.DOCX,.XLSX,.PPTX,.MWT,.EVT,.XPD,.CRX,.DEX,.7Z,.ASF,.JNT');
    WriteLn(FOut, '');
    WriteLn(FOut, ';### If you want to detect new drives added (1=yes; 0=no)');
    WriteLn(FOut, 'Detect_NewDrives=1');
    WriteLn(FOut, '');
    WriteLn(FOut, ';### If you want that ' + cProjectName + ' asks for to scan new drives added (1=yes; 0=no)');
    WriteLn(FOut, 'AskForScan_NewDrives=1');
    WriteLn(FOut, '');
    WriteLn(FOut, ';### Notify of new versions (1=yes; 0=no)');
    WriteLn(FOut, 'NotifyNewVersion=1');
    WriteLn(FOut, '');
    WriteLn(FOut, ';### Monitor system for new malware (1=yes; 0=no)');
    WriteLn(FOut, 'MonitorSystemChanges=1');
    WriteLn(FOut, '');
    WriteLn(FOut, ';### If the malware monitor is active show warnings about system changes detected (1=yes; 0=no)');
    WriteLn(FOut, 'MonitorWarnings=0');
    WriteLn(FOut, '');
    WriteLn(FOut, ';### If you want to run a memory scan when the program starts (1=yes; 0=no)');
    WriteLn(FOut, 'MemoryScanAtStartup=0');
    WriteLn(FOut, '');
    WriteLn(FOut, ';### Maximum number of simultaneously active scans (1..10)');
    WriteLn(FOut, 'MaxNumActiveScan=1');
    WriteLn(FOut, '');
    WriteLn(FOut, ';### What to do when an infected file has been found (0=MoveToQuarantine; 1=ReportOnly)');
    WriteLn(FOut, 'WhenInfectedFileIsFound='+inttostr(Ord(opMoveToQuarantine)));
    WriteLn(FOut, '');
    WriteLn(FOut, ';### Skip files with a valid signature if the monitor is active (0=not skip; 1=skip)');
    WriteLn(FOut, 'SkipValidSignatures=0');

//    WriteLn(FOut, '');
//    WriteLn(FOut, 'ShowCmdScan=0');
    WriteLn(FOut, '');
    WriteLn(FOut, ';### Use also the vxd driver on Win98/Me for detects filesystem changes (1=yes; 0=no)');
    WriteLn(FOut, 'UseVxdOnWin98=1');
    WriteLn(FOut, '');

    WriteLn(FOut, ';===========Log Settings ========================================================');
    WriteLn(FOut, '');
    WriteLn(FOut, ';### If you want to write a log (1=yes; 0=no)');
    WriteLn(FOut, 'Log=1');
    WriteLn(FOut, '');
    WriteLn(FOut, ';### Path for logs files. If empty is used the logfile path defined into '+SCANNER_CONF_FILE);
    WriteLn(FOut, 'PathLog=');
    WriteLn(FOut, '');
    WriteLn(FOut, ';### Max filesize for log files.');
    WriteLn(FOut, ';### When the program starts if a log file is more of this size it''s deleted.');
    WriteLn(FOut, 'MaxLogSizeMB='+inttostr(MAX_LOG_SIZE_MB));

    WriteLn(FOut, '');
    WriteLn(FOut, ';### GUI Language (normally detected by OS settings)');
    WriteLn(FOut, ';### Possible values: English, Italian, French, German, Spanish, Japanese');
    WriteLn(FOut, ';### Polish, Russian, Portuguese, Bulgarian, Indonesian, Azeri, Dutch, Hebrew, Galician');
    WriteLn(FOut, 'Language =');

  finally
    CloseFile(Fout);
  end;
end;

procedure PatchIniFile;
var
  IniFile : TIniFile;
  CurrentPatch: integer;
  iPatch: integer;

  procedure applyPatch(iPatch: integer);
  var lst : TStrings;
    procedure AddItem(s : string);
    begin
      if lst.IndexOf(s)<0 then
        lst.add(s);
    end;
  begin
     case iPatch of
        1:
          begin
            lst := TStringList.Create;
            try
              lst.CommaText := IniFile.ReadString('Params', 'ExtToScan',  ' ');
              AddItem('.DOCX');
              AddItem('.XLSX');
              AddItem('.PPTX');
              AddItem('.MWT');
              lst.add('.EVT');
              AddItem('.XPD');
              AddItem('.CRX');
              AddItem('.DEX');
              AddItem('.7Z');
              AddItem('.ASF');
              IniFile.WriteString('Params', 'ExtToScan', ConvertToComma(lst));
            finally
              lst.free;
            end;
          end;
        2:
          begin
            lst := TStringList.Create;
            try
              lst.CommaText := IniFile.ReadString('Params', 'NoScan',  ' ');
              AddItem('*.RBF');
              AddItem('*.RBS');
              IniFile.WriteString('Params', 'NoScan', ConvertToComma(lst));
            finally
              lst.free;
            end;
          end;
        3:
          begin
            lst := TStringList.Create;
            try
              lst.CommaText := IniFile.ReadString('Params', 'ExtToScan',  ' ');
              AddItem('.JNT');
              IniFile.WriteString('Params', 'ExtToScan', ConvertToComma(lst));
            finally
              lst.free;
            end;
          end;
     end;
  end;
begin
  IniFile := TIniFile.Create(GetIniFilePath(cProjectFileName));
  CurrentPatch := IniFile.ReadInteger('Params', 'Patch', 0);

  iPatch := CurrentPatch + 1;
  while iPatch <= LAST_PATCH do begin
     try
        applyPatch(iPatch);
     except
        IniFile.Free;
        raise;
     end;
     IniFile.WriteInteger('Params', 'Patch', iPatch);
     inc(iPatch);
  end;
  IniFile.Free;
end;

end.
