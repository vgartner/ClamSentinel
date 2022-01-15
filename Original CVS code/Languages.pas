unit Languages;

interface

uses Classes,Forms,Windows,SysUtils,Sentinel,UChangeList,UDrives,UVersion;

const
  LANG_GALICIAN = $56;

type Lang = (English, Italian, French, German, Spanish, Japanese, Polish, Russian, Portuguese, Bulgarian, Indonesian, Azeri, Dutch, Hebrew, Galician);

var CurLang : Lang;
    sMsgError : string;
    sMsgConfirm : string;
    sMsgCancel : string;
    SMsgYes : string;
    SMsgNo : string;

function  StartLanguage : Lang;

procedure SetLanguage_FSentinel(F : TFClamSentinel);
procedure SetLanguage_FDrives(F : TFDrives);
procedure SetLanguage_FChangeList(F : TFChangeList);
procedure SetLanguage_Utility;
procedure SetLanguage_VxdThds;

implementation

uses IniFiles,VxdThds,Utility,Consts,Start;

procedure HookResourceString(rs: PResStringRec; newStr: string);
var
  oldprotect: DWORD;
begin
  VirtualProtect(rs, SizeOf(rs^), PAGE_EXECUTE_READWRITE, @oldProtect);
  rs^.Identifier := Integer(PChar(newStr));
  VirtualProtect(rs, SizeOf(rs^), oldProtect, @oldProtect);
end;

function StartLanguage : Lang;
var
  IDLanguage : word;
  IniFile : TIniFile;
  sLang : string;
begin
  Result := English;
  sLang := '';

  IniFile := TIniFile.Create(GetIniFilePath(cProjectFileName));
  try
    sLang := trim(IniFile.ReadString('Params','Language',''));

    if sLang = 'English'    then Result := English;
    if sLang = 'Italian'    then Result := Italian;
    if sLang = 'French'     then Result := French;
    if sLang = 'German'     then Result := German;
    if sLang = 'Spanish'    then Result := Spanish;
    if sLang = 'Japanese'   then Result := Japanese;
    if sLang = 'Polish'     then Result := Polish;
    if sLang = 'Russian'    then Result := Russian;
    if sLang = 'Portuguese' then Result := Portuguese;
    if sLang = 'Bulgarian'  then Result := Bulgarian;
    if sLang = 'Indonesian' then Result := Indonesian;
    if sLang = 'Azeri'      then Result := Azeri;
    if sLang = 'Dutch'      then Result := Dutch;
    if sLang = 'Hebrew'     then Result := Hebrew;
    if sLang = 'Galician'   then Result := Galician;
  finally
    IniFile.Free;
  end;

  if sLang='' then
  begin
    IDLanguage := windows.GetUserDefaultLangID() and $3FF; // low 9-bits
    case IDLanguage of
      LANG_ENGLISH:    Result := English;
      LANG_ITALIAN:    Result := Italian;
      LANG_FRENCH:     Result := French;
      LANG_GERMAN:     Result := German;
      LANG_SPANISH:    Result := Spanish;
      LANG_JAPANESE:   Result := Japanese;   //SHIFT-JIS
      LANG_POLISH:     Result := Polish;     //Windows 1250 + Special chars
      LANG_RUSSIAN:    Result := Russian;    //Windows 1251
      LANG_PORTUGUESE: Result := Portuguese;
      LANG_BULGARIAN:  Result := Bulgarian;  //Windows 1251
      LANG_INDONESIAN: Result := Indonesian;
      LANG_AZERI:      Result := Azeri;
      LANG_DUTCH:      Result := Dutch;
      LANG_GALICIAN:   Result := Galician;
      //todo LANG_HEBREW:     Result := Hebrew; //Windows 1255
    end;
  end;
end;

procedure SetLanguage_FSentinel(F : TFClamSentinel);
  var sCredits : string;
begin
  sCredits := 'laclasse, Francis Chabot, OHTSUKA Fuminoli,'
              + CR + 'Michele Colagrossi, Ariel Santangelo, Szymon Huszno,'
              + CR + 'Mike Bulatov, Evandro Tonezer, Sйbastien Caillat,'
              + CR + 'Miroslav Mihov, Toffan Effendi, Sebuhi Abasov,'
              + CR + 'Robert van Drie, Raъl F.'; //todo , Moshe Flam';
with F do
begin
  Case CurLang of
    Italian:
    begin
      sMsgStopped := 'Fermato';
      sMsgRunOnSystemStartup := 'Eseguire ' + cProjectName + ' all''avvio di Windows?';
      sMsgClamWinConfNotFound := 'Il file '+SCANNER_CONF_FILE+' non и stato trovato.'
                 +CRLF+CRLF+'Prego editare i valori corretti nel file: ';
      sMsgFileNotFound := 'Il file %s non и stato trovato.';
      sMsgScanning := 'Scansione...';
      sMsgDriveHasBeenInserted := 'Il disco %s и stato inserito.'
                    + CRLF + CRLF + 'Effettuare la scansione antivirus?';
      sMsgCaptionDetected := 'Conferma: Disco %s rilevato';

      sMsgVirusMovedToQuarantine := 'Un %s и stato messo in quarantena!';
      sMsgVirusNotMovedToQuarantine := 'Un %s и stato trovato e NON spostato in quarantena!';

      sMsgFalsePositive := 'falso positivo';
      sMsgVersionInfo := cProjectName + ' Versione ' + VERSION;
      sMsgAboutInfo :=  'Uno scanner residente per l''antivirus gratuito ClamWin.'
       +CR+CR + cURL
       +CR+CR + 'Sviluppato da Andrea Russo Italia'
       +CR+CR + 'Un ringraziamento speciale a Robert Scroggins.'
       +CR+CR + 'Ringraziamenti: ' + sCredits;
      sMsgFileReadOnly := 'Impossibile aprire il file %s in scrittura.';
      sMsgHaveLatestVersion := 'Nessun aggiornamento disponibile.';
      sMsgNewVersionAvailable := 'La nuova versione %s и disponibile.';
      sMsgCheckLatestVersion := 'Verifica degli aggiornamenti';
      sMsgMaxFilesizeForLogFile := 'Dimensione massima dei file di log MB:';
      sMsgValueNotAccepted := 'Valore non accettato.';
      sMsgMaxNumActiveScans := 'Massimo numero di scansioni attive contemporaneamente (1..10): ';
      sMsgInformation := 'Informazione';
      sMsgMemoryScanning := 'Scansione della memoria';
      sMsgDriveScanning := 'Scansione del disco %s';
      sMsgModifiedFolder := 'Cartella modificata';
      sMsgFileChanged := 'Cartella: %s' + CRLF + 'File: %s';
      sMsgFolderChanged := 'И stata rilevata la creazione di una cartella.'
               + CRLF + 'Cartella: %s';
      sMsgNewFileCreated := 'И stata rilevata la creazione di un nuovo file.'
               + CRLF + 'Cartella: %s' + CRLF + 'File: %s';
      sMsgSuspiciousFile := 'file sospetto';
      sMsgObfuscatedFile := 'file offuscato';
      sMsgbadPEHeader := 'file con formato PE non valido';
      sMsgCryptedFile := 'file criptato';
      sMsgSuspiciousOriginFile := 'file di origine sospetta';
      sMsgBadSignatureFile := 'file con firma non valida';
      sMsgSignExpired := 'file con firma scaduta';

      sMsgVerifySuspiciousFile := 'Verificare il seguente file:';

      sMsgModifiedRegistry := 'Registro modificato';
      sMsgRegistryChanged := 'И stata rilevata la modifica della chiave di registro:'
        + CRLF + '%s';

      mnuSettings_AdvancedSettings.Caption := 'Impostazioni avanzate';
      mnuAdvancedSettings_Drives.Caption := 'Scelta dei dischi da monitorare';
      mnuAdvancedSettings_Extensions.Caption := 'Estensioni controllate';
      mnuAdvancedSettings_NoScan.Caption := 'Cartelle o file da escludere';
      mnuAdvancedSettings_FullScan.Caption := 'Cartelle in cui controllare qualsiasi file';
      mnuAdvancedSettings_MaxActiveScan.Caption := 'Numero massimo di scansioni attive contemporaneamente';
      mnuAdvancedSettings_LogMaxSize.Caption := 'Dimensione massima dei file di log';
      mnuAdvancedSettings_UseVxdOnWin98.Caption := 'Usa il vxd per rilevare le modifiche al filesystem';
      mnuSettings.Caption := 'Impostazioni';
      mnuSettings_RunOnStartup.Caption := 'Attiva '+cProjectName+' all''avvio di Windows';
      mnuSettings_MemoryScanAtStartup.Caption := 'Effettua la scansione della memoria all''avvio';
      mnuSettings_Log.Caption := 'Attiva la scrittura del log su file';
      mnuSettings_DetectNewDrives.Caption := 'Rileva l''inserimento di dischi rimovibili';
      mnuSettings_AskForScanNewDrives.Caption := 'Chiedi se fare la scansione dei dischi rimovibili';

      mnuSettings_MonitorSystemChanges.Caption := 'Individuazione di nuovi malware';
      mnuSettings_MonitorSystemChanges_DetectionWithWarnings.Caption := 'Rileva i file sospetti ed informa sulle modifiche al sistema';
      mnuSettings_MonitorSystemChanges_DetectionOnly.Caption := 'Rileva i file sospetti senza informare sulle modifiche al sistema';
      mnuSettings_MonitorSystemChanges_NoDetection.Caption := 'Non attiva';
      mnuSettings_MonitorSystemChanges_SkipFilesWithValidSignature.Caption := 'Salta i file con una firma digitale valida (riduce la sicurezza)';

      mnuSettings_InfectedFiles.Caption := 'Cosa fare quando un file infetto viene trovato';
      mnuSettings_NotifyNewVersion.Caption := 'Notifica la disponibilitа di nuove versioni';

      mnuInfectedFiles_MoveToQuarantine.Caption := 'Sposta in quarantena';
      mnuInfectedFiles_ReportOnly.Caption := 'Informa soltanto';
      mnuMemoryScan.Caption := 'Avvia una scansione della memoria';
      mnuQuarantine.Caption := 'Cartella di quarantena';
      mnuQuarantineTools.Caption := 'Quarantena';
      mnuOpen.Caption := 'File di log';
      mnuRealTimeLog.Caption := 'Scansione in tempo reale';
      mnuMessagesLog.Caption := 'Messaggi';
      mnuQuarantineLog.Caption := 'Quarantena';
      mnuMemoryLog.Caption := 'Scansione della memoria';
      mnuDriveScanLog.Caption := 'Scansione dei dischi rimovibili';
      mnuStart.Caption := 'Avvia';
      mnuStop.Caption := 'Ferma';
      mnuCheckVersion.Caption := 'Verifica degli aggiornamenti';
      mnuClamSentinelWebsite.Caption := 'Visita il sito web di '+cProjectName;
      mnuAbout.Caption := 'Informazioni';
      mnuExit.Caption := 'Chiudi';
    end;

    English:
    begin
      sMsgStopped := 'Stopped';
      sMsgRunOnSystemStartup := 'Do you want to Run ' + cProjectName + ' on system startup?';
      sMsgClamWinConfNotFound := 'File '+SCANNER_CONF_FILE+' not found.'
                 +CRLF+CRLF+'Please edit correct values into the file:';
      sMsgFileNotFound := 'File %s not found.';
      sMsgScanning := 'Scanning...';
      sMsgDriveHasBeenInserted := 'Drive %s has been inserted.'
                    + CRLF + CRLF + 'Do you want to scan it now for viruses?';
      sMsgCaptionDetected := 'Confirm: Drive %s detected';
      sMsgVirusMovedToQuarantine := 'A %s was moved to quarantine!';
      sMsgVirusNotMovedToQuarantine := 'A %s was found but NOT moved to quarantine (report only)!';
      sMsgFalsePositive := 'false positive';
      sMsgVersionInfo := cProjectName + ' Version ' + VERSION;
      sMsgAboutInfo := 'A resident scanner for the free antivirus ClamWin.'
       +CR+CR + cURL + CR
       +CR+CR + 'Developed by Andrea Russo Italy'
       +CR+CR + 'A special thanks to Robert Scroggins.'
       +CR+CR + 'Credits: ' + sCredits;
      sMsgFileReadOnly := 'Can not open file %s for writing.';
      sMsgHaveLatestVersion := 'You have the latest version.';
      sMsgNewVersionAvailable := 'The new version %s is available.';
      sMsgCheckLatestVersion := 'Check Latest Version';
      sMsgMaxFilesizeForLogFile := 'Max filesize for log files MB:';
      sMsgValueNotAccepted := 'Value not accepted.';
      sMsgMaxNumActiveScans := 'Maximum number of simultaneously active scans (1..10): ';
      sMsgInformation := 'Information';
      sMsgMemoryScanning := 'Memory scan';
      sMsgDriveScanning := 'Scanning the drive %s';
      sMsgModifiedFolder := 'Modified folder';
      sMsgFileChanged := 'Folder: %s' + CRLF + 'File: %s';
      sMsgFolderChanged := 'A new folder has been identified.'
               + CRLF + 'Folder: %s';
      sMsgNewFileCreated := 'A new file has been identified.'
               + CRLF + 'Folder: %s' + CRLF + 'File: %s';
      sMsgSuspiciousFile := 'suspicious file';
      sMsgObfuscatedFile := 'obfuscated file';
      sMsgbadPEHeader := 'file with invalid PE format';
      sMsgCryptedFile := 'crypted file';
      sMsgSuspiciousOriginFile := 'suspicious origin file';
      sMsgBadSignatureFile := 'file with invalid signature';
      sMsgSignExpired := 'file with expired signature';

      sMsgVerifySuspiciousFile := 'Please verify this suspicious file:';

      sMsgModifiedRegistry := 'Registry modified';
      sMsgRegistryChanged := 'It has been found the modification of the registry key:'
        + CRLF + '%s';

      mnuSettings_RunOnStartup.Caption := 'Run '+cProjectName+' on startup';
      mnuClamSentinelWebsite.Caption := 'Visit '+cProjectName+' Website';

      (*
      mnuSettings_AdvancedSettings.Caption := 'Advanced settings';
      mnuAdvancedSettings_Drives.Caption := 'Choose disks to monitor';
      mnuAdvancedSettings_Extensions.Caption := 'Extensions scanned';
      mnuAdvancedSettings_NoScan.Caption := 'Paths or files not scanned';
      mnuAdvancedSettings_FullScan.Caption := 'Paths where all files will be scanned';
      mnuAdvancedSettings_MaxActiveScan.Caption := 'Maximum number of simultaneously active scans';
      mnuAdvancedSettings_LogMaxSize.Caption := 'Max filesize for log files';
      mnuAdvancedSettings_UseVxdOnWin98.Caption := 'Use vxd for detect filesystem changes';
      mnuSettings.Caption := 'Settings';
      mnuSettings_MemoryScanAtStartup.Caption := 'Scan the memory when the program starts';
      mnuSettings_Log.Caption := 'Write scan activity to the log';
      mnuSettings_DetectNewDrives.Caption := 'Detect and monitor new drives';
      mnuSettings_AskForScanNewDrives.Caption := 'Ask to scan new drives';

      mnuSettings_MonitorSystemChanges.Caption := 'Monitor system for new malware';
      mnuSettings_MonitorSystemChanges_DetectionWithWarnings.Caption := 'Detect suspicious files and warn about system changes';
      mnuSettings_MonitorSystemChanges_DetectionOnly.Caption := 'Detect suspicious files only';
      mnuSettings_MonitorSystemChanges_NoDetection.Caption := 'Disabled';
      mnuSettings_MonitorSystemChanges_SkipFilesWithValidSignature.Caption := 'Skip files with a valid digital signature (reduces security)';

      mnuSettings_InfectedFiles.Caption := 'What to do when an infected file has been found';

      mnuInfectedFiles_MoveToQuarantine.Caption := 'Move to quarantine folder';
      mnuInfectedFiles_ReportOnly.Caption := 'Report only';

      mnuSettings_NotifyNewVersion.Caption := 'Notify of new versions';

      mnuMemoryScan.Caption := 'Memory scan';
      mnuQuarantine.Caption := 'Quarantine Folder';
      mnuQuarantineTools.Caption := 'Quarantine';
      mnuOpen.Caption := 'Logs';
      mnuRealTimeLog.Caption := 'Real Time Scan';
      mnuMessagesLog.Caption := 'Messages';
      mnuQuarantineLog.Caption := 'Quarantine';
      mnuMemoryLog.Caption := 'Memory Scan';
      mnuDriveScanLog.Caption := 'Drive Scan';
      mnuStart.Caption := 'Start';
      mnuStop.Caption := 'Stop';
      mnuCheckVersion.Caption := 'Check Latest Version';
      mnuAbout.Caption := 'About';
      mnuExit.Caption := 'Exit';
      *)
    end;

    French:
    begin
      sMsgStopped := 'Arrкtй';
      sMsgRunOnSystemStartup := 'Exйcuter ' + cProjectName + ' au dйmarrage de Windows ?';
      sMsgClamWinConfNotFound := 'Le fichier '+SCANNER_CONF_FILE+' n''a pas йtй trouvй.'
                 +CRLF+CRLF+'йditer les valeurs correctes dans le fichier : ';
      sMsgFileNotFound := 'Le fichier %s n''a pas йtй trouvй.';
      sMsgScanning := 'Scan...';
      sMsgDriveHasBeenInserted := 'Le disque %s a йtй insйrй.'
                    + CRLF + CRLF + 'Effectuer un scan antivirus ?';
      sMsgCaptionDetected := 'Confirmation : Disque %s relevй';
      sMsgVirusMovedToQuarantine := 'Un %s a йtй mis en quarantaine!';
      sMsgVirusNotMovedToQuarantine := 'Un %s a йtй trouvй et il n''a pas йtй mis en quarantaine !';
      sMsgFalsePositive := 'de faux positifs';
      sMsgVersionInfo := cProjectName + ' Version ' + VERSION;
      sMsgAboutInfo :=  'Scanner rйsident pour l''antivirus libre ClamWin.'
       +CR+CR + cURL + CR
       +CR+CR + 'Dйveloppй par Andrea Russo Italie'
       +CR+CR + 'Remerciement spйcial а Robert Scroggins.'
       +CR+CR + 'Remerciements : ' + sCredits;
      sMsgFileReadOnly := 'Impossible d''ouvrir le fichier %s en йcriture.';
      sMsgHaveLatestVersion := 'Aucune nouvelle version n''est disponible.';
      sMsgNewVersionAvailable := 'La nouvelle version %s est disponible.';
      sMsgCheckLatestVersion := 'Rechercher des mises а jour';
      sMsgMaxFilesizeForLogFile := 'Taille maximum du fichier log MB :';
      sMsgValueNotAccepted := 'Valeur incorrecte.';
      sMsgMaxNumActiveScans := 'Nombre maximum de scans actifs en mкme temps (1..10) : ';
      sMsgInformation := 'Information';
      sMsgMemoryScanning := 'Scanner de la mйmoire';
      sMsgDriveScanning := 'Scanner du disque %s';
      sMsgModifiedFolder := 'Mise а jour du dossier';
      sMsgFileChanged := 'Dossier : %s' + CRLF + 'Fichier : %s';
      sMsgFolderChanged := 'La crйation d''un dossier a йtй detectйe.'
               + CRLF + 'Dossier : %s';
      sMsgNewFileCreated := 'La crйation d''un fichier a йtй detectйe.'
               + CRLF + 'Dossier : %s' + CRLF + 'Fichier : %s';
      sMsgSuspiciousFile := 'fichier suspect';
      sMsgObfuscatedFile := 'fichier masquйe';
      sMsgbadPEHeader := 'fichier avec format PE invalide';
      sMsgCryptedFile := 'fichier cryptй';
      sMsgSuspiciousOriginFile := 'fichier d''origine suspecte';
      sMsgBadSignatureFile := 'fichier avec signature invalide';
      sMsgSignExpired := 'fichier avec signature expirй';
      sMsgVerifySuspiciousFile := 'Vйrifier le fichier suivant :';

      sMsgModifiedRegistry := 'Registre modifiй';
      sMsgRegistryChanged := 'Clй du registre modifiйe :'
        + CRLF + '%s';

      mnuSettings_AdvancedSettings.Caption := 'Configuration avancйe';
      mnuAdvancedSettings_Drives.Caption := 'Choix des disques а surveiller';
      mnuAdvancedSettings_Extensions.Caption := 'Extensions surveillйes';
      mnuAdvancedSettings_NoScan.Caption := 'Dossiers ou fichiers non surveillйs';
      mnuAdvancedSettings_FullScan.Caption := 'Surveiller tous les fichiers du rйpertoire';
      mnuAdvancedSettings_MaxActiveScan.Caption := 'Nombre maximum de scan actif en mкme temps';
      mnuAdvancedSettings_LogMaxSize.Caption := 'Taille maximum du fichier log';
      mnuAdvancedSettings_UseVxdOnWin98.Caption := 'Utiliser le vxd pour surveiller les modifications des fichiers systиmes';
      mnuSettings.Caption := 'Configuration';
      mnuSettings_RunOnStartup.Caption := 'Lancer '+cProjectName+' au dйmarrage de Windows';
      mnuSettings_MemoryScanAtStartup.Caption := 'Effectuer le scan de la mйmoire au dйmarrage';
      mnuSettings_Log.Caption := 'Activer l''йcriture dans un fichier log';
      mnuSettings_DetectNewDrives.Caption := 'Surveiller l''insertion des disques amovibles';
      mnuSettings_AskForScanNewDrives.Caption := 'Demander si l''on doit scanner des disques amovibles';
      mnuSettings_MonitorSystemChanges.Caption := 'Systиme de surveillance pour les nouveaux logiciels malveillants';

      mnuSettings_MonitorSystemChanges_DetectionWithWarnings.Caption := 'Dйtection des fichiers suspects et informe des modifications au systиme';
      mnuSettings_MonitorSystemChanges_DetectionOnly.Caption := 'Dйtection des fichiers suspects uniquement';
      mnuSettings_MonitorSystemChanges_NoDetection.Caption := 'Inactifs';
      mnuSettings_MonitorSystemChanges_SkipFilesWithValidSignature.Caption := 'Passer des fichiers avec une signature numйrique valide';

      mnuSettings_InfectedFiles.Caption := 'Que faire quand un fichier infectй est trouvй';
      mnuSettings_NotifyNewVersion.Caption := 'Informer de la disponibilitй de nouvelles versions';
      mnuInfectedFiles_MoveToQuarantine.Caption := 'Dйplacer en quarantaine';
      mnuInfectedFiles_ReportOnly.Caption := 'Informer seulement';
      mnuMemoryScan.Caption := 'Activer le scan de la mйmoire';
      mnuQuarantine.Caption := 'Dossier de quarantaine';
      mnuQuarantineTools.Caption := 'Quarantaine';
      mnuOpen.Caption := 'Fichier log';
      mnuRealTimeLog.Caption := 'Scanner en temps rйel';
      mnuMessagesLog.Caption := 'Messages';
      mnuQuarantineLog.Caption := 'Quarantaine';
      mnuMemoryLog.Caption := 'Scanner de la mйmoire';
      mnuDriveScanLog.Caption := 'Scanner des disques amovibles';
      mnuStart.Caption := 'Dйmarrer';
      mnuStop.Caption := 'Arrкter';
      mnuCheckVersion.Caption := 'Rechercher des mises а jour';
      mnuClamSentinelWebsite.Caption := 'Visiter le site web de '+cProjectName;
      mnuAbout.Caption := 'А propos';
      mnuExit.Caption := 'Quitter';
    end;

    Japanese:
    begin
      sMsgStopped := '’вЋ~‚µ‚Ь‚µ‚Ѕ';
      sMsgRunOnSystemStartup := 'ѓVѓXѓeѓЂ‹N“®Ћћ‚Й' + cProjectName + '‚р‹N“®‚µ‚Ь‚·‚©ЃH';
      sMsgClamWinConfNotFound := 'ѓtѓ@ѓCѓ‹'+SCANNER_CONF_FILE+'‚НЊ©‚В‚©‚и‚Ь‚№‚с'
         +CRLF+CRLF+'ђі‚µ‚ў’l‚рѓtѓ@ѓCѓ‹‚Й“ь‚к‚Д‚­‚ѕ‚і‚ў:';
      sMsgFileNotFound := 'ѓtѓ@ѓCѓ‹ %s ‚НЊ©‚В‚©‚и‚Ь‚№‚с';
      sMsgScanning := 'ѓXѓLѓѓѓ“’†...';
      sMsgDriveHasBeenInserted := 'ѓhѓ‰ѓCѓu %s ‚Є‘}“ь‚і‚к‚Ь‚µ‚Ѕ'
         + CRLF + CRLF + 'ЌЎѓEѓBѓ‹ѓXѓXѓLѓѓѓ“‚µ‚Ь‚·‚©ЃH';
      sMsgCaptionDetected := 'Љm”F: ѓhѓ‰ѓCѓu %s ‚ЄЊџЏo‚і‚к‚Ь‚µ‚Ѕ';
      sMsgVirusMovedToQuarantine := '%s ‚НЉu—Ј‚і‚к‚Ь‚µ‚Ѕ';
      sMsgVirusNotMovedToQuarantine := '%s ‚НЊ©‚В‚©‚и‚Ь‚µ‚Ѕ‚ЄЉu—Ј‚µ‚Д‚ў‚Ь‚№‚сЃi•сЌђ‚М‚ЭЃjЃI';
      sMsgFalsePositive := '‹^‚ў‚ ‚и';
      sMsgVersionInfo := cProjectName + 'ѓoЃ[ѓWѓ‡ѓ“' + VERSION;
      sMsgAboutInfo := 'ѓtѓЉЃ[‚МѓAѓ“ѓ`ѓEѓBѓ‹ѓXclamwin—p‚МЏн’“ѓ^ѓCѓv‚МѓEѓBѓ‹ѓXѓXѓLѓѓѓiЃ['
       +CR+CR + cURL + CR
       +CR+CR + 'Developed by Andrea Russo Italy'
       +CR+CR + 'A special thanks to Robert Scroggins.'
       +CR+CR + 'Credits: ' + sCredits;

      sMsgFileReadOnly := 'ѓtѓ@ѓCѓ‹ %s ‚НЏ‘‚«Ќћ‚Э’†‚Й‚В‚«ЉJ‚Ї‚Ь‚№‚с';
      sMsgHaveLatestVersion := 'ЌЕђV‚МѓoЃ[ѓWѓ‡ѓ“‚Е‚·';
      sMsgNewVersionAvailable := 'ђV‚µ‚ўѓoЃ[ѓWѓ‡ѓ“ %s ‚Є——p‚Е‚«‚Ь‚·';
      sMsgCheckLatestVersion := 'ЌЕђV‚МѓoЃ[ѓWѓ‡ѓ“‚р’І‚Ч‚й';
      sMsgMaxFilesizeForLogFile := 'ѓЌѓOѓtѓ@ѓCѓ‹‚МЌЕ‘еѓtѓ@ѓCѓ‹ѓTѓCѓY MB:';
      sMsgValueNotAccepted := '‚»‚М’l‚НЋу‚Ї“ь‚к‚з‚к‚Ь‚№‚с';
      sMsgMaxNumActiveScans := '“ЇЋћ‚ЙѓAѓNѓeѓBѓuѓXѓLѓѓѓ“‚рЌs‚¤ЌЕ‘еђ” (1..10): ';
      sMsgInformation := 'Џо•с';
      sMsgMemoryScanning := 'ѓЃѓ‚ѓЉ‚рѓXѓLѓѓѓ“‚·‚й';
      sMsgDriveScanning := 'ѓhѓ‰ѓCѓu %s ‚рѓXѓLѓѓѓ“’†';
      sMsgModifiedFolder := '•ПЌX‚і‚к‚ЅѓtѓHѓ‹ѓ_';
      sMsgFileChanged := 'ѓtѓHѓ‹ѓ_: %s' + CRLF + 'ѓtѓ@ѓCѓ‹: %s';
      sMsgFolderChanged := 'ђV‚µ‚ўѓtѓHѓ‹ѓ_‚ЄЉm”F‚і‚к‚Ь‚µ‚Ѕ'
        + CRLF + 'ѓtѓHѓ‹ѓ_: %s';
      sMsgNewFileCreated := 'ђV‚µ‚ўѓtѓ@ѓCѓ‹‚НЋЇ•К‚і‚к‚Ѕ'
               + CRLF + 'ѓtѓHѓ‹ѓ_: %s' + CRLF + 'ѓtѓ@ѓCѓ‹: %s';
      sMsgSuspiciousFile := '‹^‚н‚µ‚ўѓtѓ@ѓCѓ‹';
      sMsgObfuscatedFile := '•s–ѕ—Д‚Иѓtѓ@ѓCѓ‹';
      sMsgbadPEHeader := 'PEѓtѓ@ѓCѓ‹Њ`Ћ®‚Є–іЊш‚Е‚·';
      sMsgCryptedFile := '€ГЌ†‰»‚і‚к‚Ѕѓtѓ@ѓCѓ‹';
      sMsgSuspiciousOriginFile := '‹^‚н‚µ‚ўѓIѓЉѓWѓiѓ‹‚Мѓtѓ@ѓCѓ‹';
      sMsgBadSignatureFile := '–іЊш‚ИЏђ–ј‚Є‚В‚ў‚Ѕѓtѓ@ѓCѓ‹';
      sMsgSignExpired := '–іЊш‚ИЏђ–ј‚Є‚В‚ў‚Ѕѓtѓ@ѓCѓ‹ (expired)';
      sMsgVerifySuspiciousFile := '‚±‚М‹^‚н‚µ‚ўѓtѓ@ѓCѓ‹‚рЊџЏШ‚µ‚Д‚­‚ѕ‚і‚ў:';
      sMsgModifiedRegistry := 'ѓЊѓWѓXѓgѓЉ‚Є•ПЌX‚і‚к‚Ь‚µ‚Ѕ';
      sMsgRegistryChanged := 'ѓЊѓWѓXѓgѓЉѓLЃ[‚М•ПЌX‚ЄЊ©‚В‚©‚и‚Ь‚µ‚Ѕ:'+ CRLF + '%s';
      mnuSettings_RunOnStartup.Caption := 'ѓVѓXѓeѓЂ‹N“®Ћћ‚Й'+cProjectName+'‚р‹N“®';
      mnuClamSentinelWebsite.Caption := cProjectName+'‚МWebsite‚ЦЌs‚­';
      mnuSettings_AdvancedSettings.Caption := 'ЏЪЌЧђЭ’и';
      mnuAdvancedSettings_Drives.Caption := 'ѓ‚ѓjѓ^‚·‚йѓfѓBѓXѓN‚р‘I‘р';
      mnuAdvancedSettings_Extensions.Caption := 'Љg’ЈѓXѓLѓѓѓ“';
      mnuAdvancedSettings_NoScan.Caption := 'ѓXѓLѓѓѓ“‚і‚к‚И‚ўѓpѓX‚Ь‚Ѕ‚Нѓtѓ@ѓCѓ‹‚рЋw’и';
      mnuAdvancedSettings_FullScan.Caption := 'ѓXѓLѓѓѓ“‚і‚к‚йѓpѓX‚рЋw’и';
      mnuAdvancedSettings_MaxActiveScan.Caption := '“ЇЋћ‚ЙЌs‚¤ѓAѓNѓeѓBѓuѓXѓLѓѓѓ“‚МЌЕ‘еђ”';
      mnuAdvancedSettings_LogMaxSize.Caption := 'ѓЌѓOѓtѓ@ѓCѓ‹‚МЌЕ‘еѓtѓ@ѓCѓ‹ѓTѓCѓY';
      mnuAdvancedSettings_UseVxdOnWin98.Caption := 'ѓtѓ@ѓCѓ‹ѓVѓXѓeѓЂ‚М•ПЌX‚Йvxd‚рЋg‚¤';
      mnuSettings.Caption := 'ђЭ’и';
      mnuSettings_MemoryScanAtStartup.Caption := 'ѓvѓЌѓOѓ‰ѓЂ‹N“®Ћћ‚ЙѓЃѓ‚ѓЉ‚рѓXѓLѓѓѓ“‚·‚й';
      mnuSettings_Log.Caption := 'ѓЌѓOѓtѓ@ѓCѓ‹‚ЙЏ‘‚«Ќћ‚Ю';
      mnuSettings_DetectNewDrives.Caption := 'ђV‚µ‚ўѓhѓ‰ѓCѓu‚рЊџЏo‚·‚й';
      mnuSettings_AskForScanNewDrives.Caption := 'ђV‚µ‚ўѓhѓ‰ѓCѓu‚рѓXѓLѓѓѓ“‚·‚й‚©ђq‚Л‚й';
      mnuSettings_MonitorSystemChanges.Caption := 'ђV‚µ‚ўѓ}ѓ‹ѓEѓFѓA‚Й‘О‚µ‚ДѓVѓXѓeѓЂ‚рѓ‚ѓjѓ^‚·‚й';
      mnuSettings_MonitorSystemChanges_DetectionWithWarnings.Caption := 'ѓVѓXѓeѓЂ•ПЌX‚Й‘О‚µ‚Д‹^‚н‚µ‚«ѓtѓ@ѓCѓ‹‚рЊџЏo‚µ‚ДЊxЌђ‚·‚й';
      mnuSettings_MonitorSystemChanges_DetectionOnly.Caption := '‹^‚н‚µ‚«ѓtѓ@ѓCѓ‹‚М‚ЭЊџЏo';
      mnuSettings_MonitorSystemChanges_NoDetection.Caption := '–іЊш';
      mnuSettings_MonitorSystemChanges_SkipFilesWithValidSignature.Caption := '—LЊш‚ИѓfѓWѓ^ѓ‹Џђ–ј‚рЋќ‚Вѓtѓ@ѓCѓ‹‚рѓXѓLѓbѓv‚·‚й';

      mnuSettings_InfectedFiles.Caption := 'Љґђх‚µ‚Ѕѓtѓ@ѓCѓ‹‚ЄЊ©‚В‚©‚Б‚Ѕ‚з‚З‚¤‚·‚й‚©';
      mnuInfectedFiles_MoveToQuarantine.Caption := 'Љu—ЈѓtѓHѓ‹ѓ_‚Й€Ъ“®‚·‚й';
      mnuInfectedFiles_ReportOnly.Caption := '•сЌђ‚М‚Э';
      mnuSettings_NotifyNewVersion.Caption := 'ђV‚µ‚ўѓoЃ[ѓWѓ‡ѓ“‚М‚Ё’m‚з‚№';
      mnuMemoryScan.Caption := 'ѓЃѓ‚ѓЉ‚рѓXѓLѓѓѓ“‚·‚й';
      mnuQuarantine.Caption := 'Њџ‰uѓtѓHѓ‹ѓ_';
      mnuQuarantineTools.Caption := 'Љu—ЈѓtѓHѓ‹ѓ_';
      mnuOpen.Caption := 'Logs';
      mnuRealTimeLog.Caption := 'ѓЉѓAѓ‹ѓ^ѓCѓЂѓXѓLѓѓѓ“';
      mnuMessagesLog.Caption := 'ѓЃѓbѓZЃ[ѓW';
      mnuQuarantineLog.Caption := 'Њџ‰u';
      mnuMemoryLog.Caption := 'ѓЃѓ‚ѓЉ‚рѓXѓLѓѓѓ“‚·‚й';
      mnuDriveScanLog.Caption := 'ѓhѓ‰ѓCѓu‚рѓXѓLѓѓѓ“‚·‚й';
      mnuStart.Caption := 'ЉJЋn';
      mnuStop.Caption := '’вЋ~';
      mnuCheckVersion.Caption := 'ЌЕђV‚МѓoЃ[ѓWѓ‡ѓ“‚рѓ`ѓFѓbѓN‚·‚й';
      mnuAbout.Caption := 'ѓoЃ[ѓWѓ‡ѓ“Џо•с';
      mnuExit.Caption := 'ЏI—№';
    end;

    German:
    begin
      sMsgStopped := 'Gestoppt';
      sMsgRunOnSystemStartup := 'Wollen Sie das ' + cProjectName + ' beim Systemstart geladen wird?';
      sMsgClamWinConfNotFound := 'Datei '+SCANNER_CONF_FILE+' wurde nicht gefunden.'
                 +CRLF+CRLF+'Bitte editieren Sie die Werte in der Datei:';
      sMsgFileNotFound := 'Datei %s nicht gefunden.';
      sMsgScanning := 'Ьberprьfe...';
      sMsgDriveHasBeenInserted := 'Laufwerk %s  wurde angeschlossen.'
                    + CRLF + CRLF + 'Wollen Sie dieses Laufwerk nach Viren untersuchen?';
      sMsgCaptionDetected := 'Bestдtige: Laufwerk %s erkannt';
      sMsgVirusMovedToQuarantine := 'Ein %s wurde in die Quarantдne verschoben!';
      sMsgVirusNotMovedToQuarantine := 'Ein %s wurde gefunden aber NCHT in die Quarantдne verschoben (Nur Bericht)!';
      sMsgFalsePositive := 'falsch positive';
      sMsgVersionInfo := cProjectName + ' Version ' + VERSION;
      sMsgAboutInfo := 'Ein Echtzeitschutz fьr den freien ClamWin Antivirus.'
       +CR+CR + cURL + CR
       +CR+CR + 'Entwickelt von Andrea Russo Italy'
       +CR+CR + 'Speziellen dank an Robert Scroggins.'
       +CR+CR + 'Credits: ' + sCredits;
      sMsgFileReadOnly := 'Kann Datei %s  nicht zum schreiben цffnen.';
      sMsgHaveLatestVersion := 'Sie haben die aktuellste Version.';
      sMsgNewVersionAvailable := 'Es ist Version %s verfьgbar.';
      sMsgCheckLatestVersion := 'Ьberprьfe aktuellste Version.';
      sMsgMaxFilesizeForLogFile := 'Maximale grцЯe der Protokolldatei in MB:';
      sMsgValueNotAccepted := 'Wert wurde nicht akzeptiert.';
      sMsgMaxNumActiveScans := 'Maximanle Anzahl von gleichzeitig aktiven Virenprьfungen (1..10): ';
      sMsgInformation := 'Information';
      sMsgMemoryScanning := 'Arbeitsspeicher ueberpruefung';
      sMsgDriveScanning := 'Ьberprьfe das Laufwerk %s';
      sMsgModifiedFolder := 'Modifiziertes Verzeichnis';
      sMsgFileChanged := 'Verzeichnis: %s' + CRLF + 'Datei: %s';
      sMsgFolderChanged := 'Ein neues Verzeichnis wurde identifiziert.'
               + CRLF + 'Verzeichnis: %s';
      sMsgNewFileCreated := 'Ein neues Datei wurde identifiziert.'
               + CRLF + 'Verzeichnis: %s' + CRLF + 'Datei: %s';
      sMsgSuspiciousFile := 'Verdдchtige Datei';
      sMsgObfuscatedFile := 'Verschleierte Datei';
      sMsgbadPEHeader := 'Datei mit ungьltigen PE Format';
      sMsgCryptedFile := 'Verschlьsselte Datei';
      sMsgSuspiciousOriginFile := 'Verdдchtige original Datei';
      sMsgBadSignatureFile := 'Datei mit falscher Signatur';
      sMsgSignExpired := 'Datei mit Signatur abgelaufen';

      sMsgVerifySuspiciousFile := 'Bitte ьberprьfe die verdдchtige Datei:';

      sMsgModifiedRegistry := 'Registrierung wurde modifiziert';
      sMsgRegistryChanged := 'Es wurde eine verдnderung des Registrierungsschlьssels erkannt:'
        + CRLF + '%s';

      mnuSettings_RunOnStartup.Caption := 'Starte  '+cProjectName+' beim Systemstart';
      mnuClamSentinelWebsite.Caption := 'Besuche die '+cProjectName+' Webseite';

      mnuSettings_AdvancedSettings.Caption := 'Erweiterte Einstellungen';
      mnuAdvancedSettings_Drives.Caption := 'Wдhle Laufwerke zum ьberwachen';
      mnuAdvancedSettings_Extensions.Caption := 'Dateierweiterungen zum ьberprьfen';
      mnuAdvancedSettings_NoScan.Caption := 'Pfade oder Dateien die nicht ьberprьft werden';
      mnuAdvancedSettings_FullScan.Caption := 'Pfade oder Dateien die ьberprьft werden';
      mnuAdvancedSettings_MaxActiveScan.Caption := 'Maximanle Anzahl von gleichzeitig aktiven Virenprьfungen ';
      mnuAdvancedSettings_LogMaxSize.Caption := 'Maximale grцЯe der Protokolldateien';
      mnuAdvancedSettings_UseVxdOnWin98.Caption := 'Benutze VXD zum erkennen von Дnderungen am Dateisystem';
      mnuSettings.Caption := 'Einstellungen';
      mnuSettings_MemoryScanAtStartup.Caption := 'Ьberprьfe Arbeitsspeicher beim Programmstart';
      mnuSettings_Log.Caption := 'Schreibe aktivitдten beim ьberprьfen in die Protokoll Datei';
      mnuSettings_DetectNewDrives.Caption := 'Erkenne und ьberwache neue Laufwerke';
      mnuSettings_AskForScanNewDrives.Caption := 'Fragen ob neue Laufwerke ьberprьft werden sollen';

      mnuSettings_MonitorSystemChanges.Caption := 'Ьberwache das System vor Malware';
      mnuSettings_MonitorSystemChanges_DetectionWithWarnings.Caption := 'Erkenne verdдchtige Dateien und Warne vor Dateisystemдnderungen';
      mnuSettings_MonitorSystemChanges_DetectionOnly.Caption := 'Erkenne verdдchtige Dateien';
      mnuSettings_MonitorSystemChanges_NoDetection.Caption := 'Deaktiviert';
      mnuSettings_MonitorSystemChanges_SkipFilesWithValidSignature.Caption := 'Ьberspringt Dateien mit einer gьltigen digitalen Signatur';

      mnuSettings_InfectedFiles.Caption := 'Was soll getan werden wenn eine infizierte Datei gefunden wurde:';

      mnuInfectedFiles_MoveToQuarantine.Caption := 'In das Quarantдneverzeichnis verschieben';
      mnuInfectedFiles_ReportOnly.Caption := 'Nur Report';

      mnuSettings_NotifyNewVersion.Caption := 'Benachrichtigung auf neue Versionen';

      mnuMemoryScan.Caption := 'Arbeitsspeicher ьberprьfen';
      mnuQuarantine.Caption := 'Quarantдne Verzeichnis';
      mnuQuarantineTools.Caption := 'Quarantдne';
      mnuOpen.Caption := 'Protokolle';
      mnuRealTimeLog.Caption := 'Echtzeit Scan';
      mnuMessagesLog.Caption := 'Programm Meldungen';
      mnuQuarantineLog.Caption := 'Quarantдne';
      mnuMemoryLog.Caption := ' Arbeitsspeicher Scan';
      mnuDriveScanLog.Caption := 'Laufwerk Scan';
      mnuStart.Caption := 'Start';
      mnuStop.Caption := 'Stop';
      mnuCheckVersion.Caption := 'Ьberprьfe auf neueste Version';
      mnuAbout.Caption := 'Ьber';
      mnuExit.Caption := 'SchlieЯen';
    end;

    Spanish:
    begin
      sMsgStopped := 'Detener';
      sMsgRunOnSystemStartup := 'їQuiere usted ejecutar ' + cProjectName + ' en el inicio del sistema?';
      sMsgClamWinConfNotFound := 'El archivo '+SCANNER_CONF_FILE+' no fue encontrado.'
                 +CRLF+CRLF+'Por favor edite los valores correctos en el archivo:';
      sMsgFileNotFound := 'El archivo %s no fue encontrado.';
      sMsgScanning := 'Escaneando...';
      sMsgDriveHasBeenInserted := 'La unidad %s ha sido insertada.'
                    + CRLF + CRLF + 'їQuiere usted escanearlo en busca de virus ahora?';
      sMsgCaptionDetected := 'Confirme: La unidad %s detectada';
      sMsgVirusMovedToQuarantine := 'ЎUn %s fue movido a cuarentena!';
      sMsgVirusNotMovedToQuarantine := 'ЎUn %s fue encontrado pero no fue movido a cuarentena (solo reportar)!';
      sMsgFalsePositive := 'Un falso positivo';
      sMsgVersionInfo := cProjectName + ' la versiуn ' + VERSION;
      sMsgAboutInfo := 'Un escбner residente para el antivirus libre ClamWin.'
         +CR+CR + cURL + CR
         +CR+CR + 'Desarrollado por Andrea Russo Italia'
         +CR+CR + 'Un especial gracias a Robert Scroggins.'
         +CR+CR + 'Crйditos a: ' + sCredits;
      sMsgFileReadOnly := 'No se puede abrir el archivo %s para escritura.';
      sMsgHaveLatestVersion := 'Usted tiene la ъltima versiуn.';
      sMsgNewVersionAvailable := 'La nueva versiуn %s estб disponible.';
      sMsgCheckLatestVersion := 'Compruebe la ъltima Versiуn';
      sMsgMaxFilesizeForLogFile := 'Tamaсo maximo para archivo de registro MB:';
      sMsgValueNotAccepted := 'Valor no aceptado.';
      sMsgMaxNumActiveScans := 'Nъmero mбximo de escaneos simultбneamente activos (1..10): ';
      sMsgInformation := 'Informaciуn';
      sMsgMemoryScanning := 'Escaneo de memoria';
      sMsgDriveScanning := 'Escaneando la unidad %s';
      sMsgModifiedFolder := 'Carpeta modificada';
      sMsgFileChanged := 'Carpeta: %s' + CRLF + 'archivo: %s';
      sMsgFolderChanged := 'Una nueva carpeta ha sido identificada.'
               + CRLF + 'Carpeta: %s';
      sMsgNewFileCreated := 'Un nuevo archivo ha sido identificado.'
               + CRLF + 'Carpeta: %s' + CRLF + 'archivo: %s';
      sMsgSuspiciousFile := 'Archivo sospechoso';
      sMsgObfuscatedFile := 'Archivo ofuscado';
      sMsgbadPEHeader := 'Archivo con formato PE invбlido';
      sMsgCryptedFile := 'Archivo encriptado';
      sMsgSuspiciousOriginFile := 'Sospechoso el origen del archivo';
      sMsgBadSignatureFile := 'Archivo con firma no vбlida';
      sMsgSignExpired := 'Archivo con firma expirado';

      sMsgVerifySuspiciousFile := 'Por favor verifique este archivo sospechosoe:';

      sMsgModifiedRegistry := 'Registro modificado';
      sMsgRegistryChanged := 'Ha sido encontrada la modificaciуn de la clave de registro:'
        + CRLF + '%s';

      mnuSettings_RunOnStartup.Caption := 'Ejecutar '+cProjectName+' en el arranque';
      mnuClamSentinelWebsite.Caption := 'Visita '+cProjectName+' sitio Web';

      mnuSettings_AdvancedSettings.Caption := 'Configuraciуn avanzada';
      mnuAdvancedSettings_Drives.Caption := 'Elija discos a monitorear';
      mnuAdvancedSettings_Extensions.Caption := 'Extensiones a escanear';
      mnuAdvancedSettings_NoScan.Caption := 'Rutas o archivos no escaneados';
      mnuAdvancedSettings_FullScan.Caption := 'Rutas donde todos los archivos serбn escaneados';
      mnuAdvancedSettings_MaxActiveScan.Caption := 'Nъmero mбximo de escaneos simultбneamente activos';
      mnuAdvancedSettings_LogMaxSize.Caption := 'Tamaсo maximo para archivo de registro';
      mnuAdvancedSettings_UseVxdOnWin98.Caption := 'Use vxd para que detecte cambios del sistema de archivo';
      mnuSettings.Caption := 'Configuraciones';
      mnuSettings_MemoryScanAtStartup.Caption := 'Escanear la memoria cuando el programa inicia';
      mnuSettings_Log.Caption := 'Escribir la actividad del scaner en el registro';
      mnuSettings_DetectNewDrives.Caption := 'Detectar y monitorear las nuevas unidades';
      mnuSettings_AskForScanNewDrives.Caption := 'Preguntar para escanear unidades nuevas';

      mnuSettings_MonitorSystemChanges.Caption := 'Monitorear nuevos malwares en el sistema';
      mnuSettings_MonitorSystemChanges_DetectionWithWarnings.Caption := 'Detectar archivos sospechosos y avisar de cambios en el sistema';
      mnuSettings_MonitorSystemChanges_DetectionOnly.Caption := 'Detectar archivos sospechosos solamente';
      mnuSettings_MonitorSystemChanges_NoDetection.Caption := 'Deshabilitado';
      mnuSettings_MonitorSystemChanges_SkipFilesWithValidSignature.Caption := 'Salte de los ficheros con una firma numйrica vбlida';

      mnuSettings_InfectedFiles.Caption := 'Quй hacer cuando un archivo infectado ha sido encontrado';

      mnuInfectedFiles_MoveToQuarantine.Caption := 'Mover a carpeta de cuarentena';
      mnuInfectedFiles_ReportOnly.Caption := 'Reportar solamente';

      mnuSettings_NotifyNewVersion.Caption := 'Notificar de nuevas versiones';

      mnuMemoryScan.Caption := 'Escaneo de memoria';
      mnuQuarantine.Caption := 'Carpeta de Cuarentena';
      mnuQuarantineTools.Caption := 'Cuarentena';
      mnuOpen.Caption := 'Registros';
      mnuRealTimeLog.Caption := 'Escaneo de Tiempo Real';
      mnuMessagesLog.Caption := 'Mensajes';
      mnuQuarantineLog.Caption := 'Cuarentena';
      mnuMemoryLog.Caption := 'Escaneo de Memoria';
      mnuDriveScanLog.Caption := 'Escaneo de Unidad';
      mnuStart.Caption := 'Iniciar';
      mnuStop.Caption := 'Detener';
      mnuCheckVersion.Caption := 'Chequear la ъltima Versiуn';
      mnuAbout.Caption := 'Acerca de';
      mnuExit.Caption := 'Salida';
    end;

    Polish:
    begin
      sMsgStopped := 'Zatrzymany';
      sMsgRunOnSystemStartup := 'Czy chcesz uruchomi'+#$E6+' ' + cProjectName + ' na starcie systemu?';
      sMsgClamWinConfNotFound := 'Plik '+SCANNER_CONF_FILE+' nie znaleziony.'
                 +CRLF+CRLF+'Edytuj poprawne warto'+#$9C+'ci do pliku:';
      sMsgFileNotFound := 'Plik %s nie odnaleziony.';
      sMsgScanning := 'Skanowanie...';
      sMsgDriveHasBeenInserted := 'Dysk %s zosta'+#$B3+' w'+#$B3+'o'+#$BF+'ony.'
                    + CRLF + CRLF + 'Czy chcesz go przeskanowa'+#$E6+' w poszukiwaniu wirus'+#$F3+'w?';
      sMsgCaptionDetected := 'Potwierdzenie: Dysk %s wykryto';
      sMsgVirusMovedToQuarantine := '%s zosta'+#$B3+' przeniesiony do kwarantanny!';
      sMsgVirusNotMovedToQuarantine := '%s zosta'+#$B3+' znaleziony ale nie przeniesiony do kwarantanny (tylko zaraportowano)!';
      sMsgFalsePositive := 'fa'+#$B3+'szywie pozytywny';
      sMsgVersionInfo := cProjectName + ' Wersja ' + VERSION;
      sMsgAboutInfo := 'Skaner przynale'+#$BF+'ny do darmowego skanera ClamWin.'
       +CR+CR + cURL + CR
       +CR+CR + 'Rozwini'+#$EA+'ty przez Andrea Russo - W'+#$B3+'ochy'
       +CR+CR + 'Specjalne podzi'+#$EA+'kowania dla Roberta Scrogginsa.'
       +CR+CR + 'Ulgi: ' + sCredits;
      sMsgFileReadOnly := 'Plik %s nie mo'+#$BF+'ne zosta'+#$E6+' otwarty do zapisu.';
      sMsgHaveLatestVersion := 'Posiadasz aktualn'+#$B9+' wersj'+#$EA+'.';
      sMsgNewVersionAvailable := 'Nowa wersja %s jest dost'+#$EA+'pna.';
      sMsgCheckLatestVersion := 'Sprawd'+#$9F+' najnowsza wersje';
      sMsgMaxFilesizeForLogFile := 'Maksymalny rozmiar pliku dziennika (MB):';
      sMsgValueNotAccepted := 'warto'+#$9C+#$E6+' niezaakceptowana.';
      sMsgMaxNumActiveScans := 'Maksymalna liczba jednoczesnych aktywnych skanowa'+#$F1+' (1..10): ';
      sMsgInformation := 'Informacja';
      sMsgMemoryScanning := 'Skanowanie pami'+#$EA+'ci';
      sMsgDriveScanning := 'Skanowanie dysku %s';
      sMsgModifiedFolder := 'Folder modyfikowany';
      sMsgFileChanged := 'Folder: %s' + CRLF + 'Plik: %s';
      sMsgFolderChanged := 'Nowy folder zosta'+#$B3+' zidentyfikowany.'
               + CRLF + 'Folder: %s';
      sMsgNewFileCreated := 'Nowy plik zosta'+#$B3+' zidentyfikowany.'
               + CRLF + 'Folder: %s' + CRLF + 'Plik: %s';
      sMsgSuspiciousFile := 'Podejrzany plik';
      sMsgObfuscatedFile := 'ukryty plik';
      sMsgbadPEHeader := 'plik z niew'+#$B3+'a'+#$9C+'ciwym formatem PE';
      sMsgCryptedFile := 'plik zaszyfrowany';
      sMsgSuspiciousOriginFile := 'plik podejrzanego pochodzenia';
      sMsgBadSignatureFile := 'plik ze z'+#$B3+#$B9+' sygnatura';
      sMsgSignExpired := 'plik ze z'+#$B3+#$B9+' sygnatura (expired)';

      sMsgVerifySuspiciousFile := 'Prosz'+#$EA+' zweryfikowa'+#$E6+' podejrzany plik:';

      sMsgModifiedRegistry := 'Zmodyfikowanie rejestru';
      sMsgRegistryChanged := 'Znaleziono modyfikacja klucza rejestru:'
        + CRLF + '%s';

      mnuSettings_RunOnStartup.Caption := 'Uruchom '+cProjectName+' na starcie';
      mnuClamSentinelWebsite.Caption := 'Odwied'+#$9F+' stron'+#$EA+' '+cProjectName;

      mnuSettings_AdvancedSettings.Caption := 'Zaawansowane ustawienia';
      mnuAdvancedSettings_Drives.Caption := 'Wybierz dyski do monitorowania';
      mnuAdvancedSettings_Extensions.Caption := 'Rozszerzenia do skanowania';
      mnuAdvancedSettings_NoScan.Caption := 'Lokacje lub pliki nie skanowane';
      mnuAdvancedSettings_FullScan.Caption := 'Lokacja plik'+#$F3+'w skanowanych';
      mnuAdvancedSettings_MaxActiveScan.Caption := 'Maksymalna liczba jednoczesnych aktywnych skanowa'+#$F1;
      mnuAdvancedSettings_LogMaxSize.Caption := 'Maksymalny rozmiar pliku dziennika';
      mnuAdvancedSettings_UseVxdOnWin98.Caption := 'u'+#$BF+'yj VXD dla wykrycia zmian pliku systemu';
      mnuSettings.Caption := 'Ustawienia';
      mnuSettings_MemoryScanAtStartup.Caption := 'Skanuj pami'+#$EA+#$E6+' na starcie programu';
      mnuSettings_Log.Caption := 'Zapisz aktywno'+#$9C+#$E6+' skanowania do dziennika';
      mnuSettings_DetectNewDrives.Caption := 'Wykryj i monitoruj nowe dyski';
      mnuSettings_AskForScanNewDrives.Caption := 'Pytaj czy skanowa'+#$E6+' dyski';

      mnuSettings_MonitorSystemChanges.Caption := 'Monitoruj system przed z'+#$B3+'o'+#$9C+'liwym oprogramowaniem';
      mnuSettings_MonitorSystemChanges_DetectionWithWarnings.Caption := 'Wykryj podejrzane pliki i ostrze'+#$BF+' o zmianach w systemie';
      mnuSettings_MonitorSystemChanges_DetectionOnly.Caption := 'Wykryj tylko podejrzane pliki';
      mnuSettings_MonitorSystemChanges_NoDetection.Caption := 'Wy'+#$B3+#$B9+'czone';
      mnuSettings_MonitorSystemChanges_SkipFilesWithValidSignature.Caption := 'Przejd&'+#$9F+' pliki z wa&'+#$BF+'nym podpisem cyfrowym';

      mnuSettings_InfectedFiles.Caption := 'Co zrobi'+#$E6+' gdy znaleziono zainfekowany plik';

      mnuInfectedFiles_MoveToQuarantine.Caption := 'Przenie'+#$9C+' do kwarantanny';
      mnuInfectedFiles_ReportOnly.Caption := 'TYLKO zaraportuj';

      mnuSettings_NotifyNewVersion.Caption := 'Zawiadom o nowych wersjach';

      mnuMemoryScan.Caption := 'Skanowanie pami'+#$EA+'ci';
      mnuQuarantine.Caption := 'Folder kwarantanny';
      mnuQuarantineTools.Caption := 'Kwarantanny';
      mnuOpen.Caption := 'Dziennik';
      mnuRealTimeLog.Caption := 'Skanowanie w czasie rzeczywistym';
      mnuMessagesLog.Caption := 'Wiadomo'+#$9C+'ci';
      mnuQuarantineLog.Caption := 'Kwarantanna';
      mnuMemoryLog.Caption := 'Skan pami'+#$EA+'ci';
      mnuDriveScanLog.Caption := 'Skan dysk'+#$F3+'w';
      mnuStart.Caption := 'Uruchom';
      mnuStop.Caption := 'Zatrzymaj';
      mnuCheckVersion.Caption := 'Sprawd'+#$9F+' aktualna wersje';
      mnuAbout.Caption := 'O programie...';
      mnuExit.Caption := 'Wyj'+#$9C+'cie';
    end;

    Russian:
    begin
      sMsgStopped := 'Остановлено';
      sMsgRunOnSystemStartup := 'Разрешить автозапуск ' + cProjectName + ' ?';
      sMsgClamWinConfNotFound := 'Файл '+SCANNER_CONF_FILE+' не найден.'
                 +CRLF+CRLF+'Введите корректное значение в файл:';
      sMsgFileNotFound := 'Файл %s не найден.';
      sMsgScanning := 'Сканирование...';
      sMsgDriveHasBeenInserted := 'Было подключено устройство %s.'
                    + CRLF + CRLF + 'Желаете проверить его на вирусы?';
      sMsgCaptionDetected := 'Уведомление: Устройство %s обнаружено';
      sMsgVirusMovedToQuarantine := '%s перемещен в карантин!';
      sMsgVirusNotMovedToQuarantine := '%s обнаружен но НЕ перемещен в карантин (см. журнал)!';
      sMsgFalsePositive := 'ложноположительный';
      sMsgVersionInfo := cProjectName + ' Версии ' + VERSION;
      sMsgAboutInfo := 'Резидентный сканер для  свободного антивируса ClamWin.'
       +CR+CR + cURL + CR
       +CR+CR + 'Разработчик Andrea Russo Италия'
       +CR+CR + 'Огромное спасибо Robert Scroggins.'
       +CR+CR + 'Credits: ' + sCredits;
      sMsgFileReadOnly := 'Не могу открыть файл %s для записи.';
      sMsgHaveLatestVersion := 'У вас последняя версия.';
      sMsgNewVersionAvailable := 'Новая версия %s доступна.';
      sMsgCheckLatestVersion := 'Проверить обновление';
      sMsgMaxFilesizeForLogFile := 'Максимальный размер Log-Файла в Мб:';
      sMsgValueNotAccepted := 'Значение не принято.';
      sMsgMaxNumActiveScans := 'Максимальное количество одновременно активных потоков сканирования(1..10): ';
      sMsgInformation := 'Информация';
      sMsgMemoryScanning := 'Сканирование памяти';
      sMsgDriveScanning := 'Сканирование устройства %s';
      sMsgModifiedFolder := 'Измененный каталог';
      sMsgFileChanged := 'Каталог: %s' + CRLF + 'Файл: %s';
      sMsgFolderChanged := 'Обнаружен новый каталог.'
               + CRLF + 'Каталог: %s';
      sMsgNewFileCreated := 'Новый файл был создан'
               + CRLF + 'Каталог: %s' + CRLF + 'Файл: %s';
      sMsgSuspiciousFile := 'Подозрительный файл';
      sMsgObfuscatedFile := 'Неизвестный файл';
      sMsgbadPEHeader := 'Файл с поврежденным заголовком PE';
      sMsgCryptedFile := 'Зашифрованный файл';
      sMsgSuspiciousOriginFile := 'Файл подозрительного происхождения';
      sMsgBadSignatureFile := 'Файл с поврежденной сигнатурой';
      sMsgSignExpired  := 'Файл с поврежденной сигнатурой (expired)';

      sMsgVerifySuspiciousFile := 'Проверьте эти подозрительные файлы:';

      sMsgModifiedRegistry := 'Реестр изменён';
      sMsgRegistryChanged := 'Обнаружена модификация ключа реестра:'
        + CRLF + '%s';

      mnuSettings_RunOnStartup.Caption := 'Запускать '+cProjectName+' при старте системы';
      mnuClamSentinelWebsite.Caption := 'Посетите официальный сайт '+cProjectName+' !';

      mnuSettings_AdvancedSettings.Caption := 'Расширенные настройки';
      mnuAdvancedSettings_Drives.Caption := 'Выбрать контролируемые диски';
      mnuAdvancedSettings_Extensions.Caption := 'Расширения для сканирования';
      mnuAdvancedSettings_NoScan.Caption := 'Папки - исключения';
      mnuAdvancedSettings_FullScan.Caption := 'Папки где все файлы будут проверяться';
      mnuAdvancedSettings_MaxActiveScan.Caption := 'Количество активных потоков сканирования';
      mnuAdvancedSettings_LogMaxSize.Caption := 'Размер файла журнала';
      mnuAdvancedSettings_UseVxdOnWin98.Caption := 'Использовать vxd-драйвер для обнаружения системных изменений';
      mnuSettings.Caption := 'Настройки';
      mnuSettings_MemoryScanAtStartup.Caption := 'Проверка памяти при запуске программы';
      mnuSettings_Log.Caption := 'Записывать результаты проверки в журнал';
      mnuSettings_DetectNewDrives.Caption := 'Определять и проверять новые устройства';
      mnuSettings_AskForScanNewDrives.Caption := 'Запрос перед проверкой новых дисков';

      mnuSettings_MonitorSystemChanges.Caption := 'Система обнаружения новых вирусов';
      mnuSettings_MonitorSystemChanges_DetectionWithWarnings.Caption := 'Обнаруживает подозрительные файлы и предупреждает об изменениях в системе';
      mnuSettings_MonitorSystemChanges_DetectionOnly.Caption := 'Обнаруживать только подозрительные файлы';
      mnuSettings_MonitorSystemChanges_NoDetection.Caption := 'Выключена';
      mnuSettings_MonitorSystemChanges_SkipFilesWithValidSignature.Caption := 'Пропустить файлы с действительной цифровой подписи';

      mnuSettings_InfectedFiles.Caption := 'Действие при обнаружении инфицированных файлов';

      mnuInfectedFiles_MoveToQuarantine.Caption := 'Переместить в каталог карантина';
      mnuInfectedFiles_ReportOnly.Caption := 'Только отчет';

      mnuSettings_NotifyNewVersion.Caption := 'Уведомлять о новой версии';

      mnuMemoryScan.Caption := 'Сканировать память';
      mnuQuarantine.Caption := 'Открыть папку карантина';
      mnuQuarantineTools.Caption := 'Карантина';
      mnuOpen.Caption := 'Журнал';
      mnuRealTimeLog.Caption := 'Сканирования в реальном времени';
      mnuMessagesLog.Caption := 'Сообщений';
      mnuQuarantineLog.Caption := 'Карантина';
      mnuMemoryLog.Caption := 'Сканирования памяти';
      mnuDriveScanLog.Caption := 'Сканирования диска';
      mnuStart.Caption := 'Старт';
      mnuStop.Caption := 'Стоп';
      mnuCheckVersion.Caption := 'Проверить обновление';
      mnuAbout.Caption := 'О программе';
      mnuExit.Caption := 'Выход';
    end;

    Portuguese:
    begin
      sMsgStopped := 'Parado';
      sMsgRunOnSystemStartup := 'Vocк deseja executar o ' + cProjectName + ' ao iniciar o Windows?';
      sMsgClamWinConfNotFound := 'Arquivo '+SCANNER_CONF_FILE+' nгo encontrado.'
                 +CRLF+CRLF+' Por favor, edite corretamente os valores dentro do arquivo:';
      sMsgFileNotFound := 'Arquivo %s nгo encontrado.';
      sMsgScanning := 'Verificando...';
      sMsgDriveHasBeenInserted := 'Drive %s foi conectado.'
                    + CRLF + CRLF + 'Deseja fazer uma verificaзгo?';
      sMsgCaptionDetected := 'Confirmado: Drive %s detectado';
      sMsgVirusMovedToQuarantine := '%s foi movido para a quarentena';
      sMsgVirusNotMovedToQuarantine := '%s foi encontrado, mas NГO foi movido para a quarentena (somente reportar)!';
      sMsgFalsePositive := 'falso positivo';
      sMsgVersionInfo := cProjectName + ' Versгo ' + VERSION;
      sMsgAboutInfo := 'Scanner residente para o antivнrus ClamWin.'
       +CR+CR + cURL + CR
       +CR+CR + 'Desenvolvido por Russo Italy'
       +CR+CR + 'Agradecimento especial para Robert Scroggins.'
       +CR+CR + 'Crйditos: ' + sCredits;
      sMsgFileReadOnly := 'Nгo й possнvel abrir %s para escrita.';
      sMsgHaveLatestVersion := 'Vocк jб possui a versгo atual.';
      sMsgNewVersionAvailable := 'Nova versгo %s estб disponнvel.';
      sMsgCheckLatestVersion := 'Verificar atualizaзхes';
      sMsgMaxFilesizeForLogFile := 'Tamanho mбximo para o arquivo de log em MB:';
      sMsgValueNotAccepted := 'Valor nгo aceito.';
      sMsgMaxNumActiveScans := 'Nъmero mбximo de scans ativos simultaneamente (1..10): ';
      sMsgInformation := 'Informaзгo';
      sMsgMemoryScanning := 'Verificando memуria';
      sMsgDriveScanning := 'Verificando o drive %s';
      sMsgModifiedFolder := 'Pasta modificada';
      sMsgFileChanged := 'Pasta: %s ' + CRLF + 'Arquivo: %s';
      sMsgFolderChanged := 'Uma nova pasta foi identificada.'
               + CRLF + 'Pasta: %s';
      sMsgNewFileCreated := 'Um novo arquivo foi identificado.'
               + CRLF + 'Pasta: %s ' + CRLF + 'Arquivo: %s';
      sMsgSuspiciousFile := 'Arquivo suspeito';
      sMsgObfuscatedFile := 'Arquivo suspeito';
      sMsgbadPEHeader := 'Arquivo PE com formato invalido';
      sMsgCryptedFile := 'Arquivo';
      sMsgSuspiciousOriginFile := 'Arquivo de origem suspeita';
      sMsgBadSignatureFile := 'Arquivo com assinatura invalida';
      sMsgSignExpired := 'Arquivo com a assinatura expirou';

      sMsgVerifySuspiciousFile := 'Por favor verifique o arquivo suspeito:';

      sMsgModifiedRegistry := 'Registro modificado';
      sMsgRegistryChanged := 'Foi verificada a modificaзгo da chave do registro: '
        + CRLF + '%s';

      mnuSettings_RunOnStartup.Caption := 'Executar o '+cProjectName+' ao iniciar o Windows';
      mnuClamSentinelWebsite.Caption := 'Viste o site do '+cProjectName;

      mnuSettings_AdvancedSettings.Caption := 'Configuraзхes Avanзadas';
      mnuAdvancedSettings_Drives.Caption := 'Escolha os discos a serem monitorados';
      mnuAdvancedSettings_Extensions.Caption := 'Lista de extensхes monitoradas';
      mnuAdvancedSettings_NoScan.Caption := 'Pastas ou arquivos nгo monitorados';
      mnuAdvancedSettings_FullScan.Caption := 'Pastas ou arquivos a serem sempre verificados';
      mnuAdvancedSettings_MaxActiveScan.Caption := 'Nъmero mбximo de scans ativos simultaneamente';
      mnuAdvancedSettings_LogMaxSize.Caption := 'Tamanho mбximo para o arquivo de log';
      mnuAdvancedSettings_UseVxdOnWin98.Caption := 'Use o vxd para detectar mudanзas nos arquivos do sistema';
      mnuSettings.Caption := 'Configuraзхes';
      mnuSettings_MemoryScanAtStartup.Caption := 'Verificar memуria ao iniciar o programa';
      mnuSettings_Log.Caption := 'Registrar verificaзгo no arquivo de log';
      mnuSettings_DetectNewDrives.Caption := 'Detectar e monitorar novas unidades';
      mnuSettings_AskForScanNewDrives.Caption := 'Perguntar ao verificar novas unidades';

      mnuSettings_MonitorSystemChanges.Caption := 'Monitor do sistema para novo malware';
      mnuSettings_MonitorSystemChanges_DetectionWithWarnings.Caption := 'Detectar arquivos suspeitos e alertar sobre mudanзas no sistema';
      mnuSettings_MonitorSystemChanges_DetectionOnly.Caption := 'Detectar arquivos suspeitos';
      mnuSettings_MonitorSystemChanges_NoDetection.Caption := 'Desabilitado';
      mnuSettings_MonitorSystemChanges_SkipFilesWithValidSignature.Caption := 'Ir arquivos com uma assinatura digital vбlida';

      mnuSettings_InfectedFiles.Caption := 'O que fazer quando um arquivo infectado for encontrado';

      mnuInfectedFiles_MoveToQuarantine.Caption := 'Mover para a quarentena';
      mnuInfectedFiles_ReportOnly.Caption := 'Somente reportar';

      mnuSettings_NotifyNewVersion.Caption := 'Notificar sobre novas versхes';

      mnuMemoryScan.Caption := 'Verificar memуria';
      mnuQuarantine.Caption := 'Pasta de Quarentena';
      mnuQuarantineTools.Caption := 'Quarentena';
      mnuOpen.Caption := 'Logs';
      mnuRealTimeLog.Caption := 'Verificaзгo em tempo real';
      mnuMessagesLog.Caption := 'Mensagens';
      mnuQuarantineLog.Caption := 'Quarentena';
      mnuMemoryLog.Caption := 'Verificaзгo da memуria';
      mnuDriveScanLog.Caption := 'verificaзгo do disco';
      mnuStart.Caption := 'Iniciar';
      mnuStop.Caption := 'Parar';
      mnuCheckVersion.Caption := 'Verificar nova versгo';
      mnuAbout.Caption := 'Sobre';
      mnuExit.Caption := 'Sair';
    end;

    Bulgarian:
    begin
      sMsgStopped := 'Спиране';
      sMsgRunOnSystemStartup := 'Да бъде ли стартиран ' + cProjectName + ' при стартиране на системата?';
      sMsgClamWinConfNotFound := 'Файла '+SCANNER_CONF_FILE+' не е открит.'
                 +CRLF+CRLF+'Моля въведете точните стойности във файла:';
      sMsgFileNotFound := 'Файла %s не е открит.';
      sMsgScanning := 'Тече проверка…';
      sMsgDriveHasBeenInserted := 'Устройство %s беше свързано.'
                    + CRLF + CRLF + 'Искате ли да бъде сканирано за вируси сега?';
      sMsgCaptionDetected := 'Потвърждение : Устройство %s е открито';
      sMsgVirusMovedToQuarantine := 'Един %s е преместен под карантина!';
      sMsgVirusNotMovedToQuarantine := 'Един %s е открит, но не е преместен под карантина(само проверка)!';
      sMsgFalsePositive := 'Фалшиво положителни';
      sMsgVersionInfo := cProjectName + ' Версия ' + VERSION;
      sMsgAboutInfo := 'Постоянен скенер за свободната антивирусна ClamWin.'
       +CR+CR + cURL + CR
       +CR+CR + 'Създаден от Andrea Russo Италия'
       +CR+CR + 'Специална благодарност на Robert Scroggins.'
       +CR+CR + 'Участват: ' + sCredits;
      sMsgFileReadOnly := 'Не може да отвори файл %s за писане.';
      sMsgHaveLatestVersion := 'Разполагате с последна версия на програмата.';
      sMsgNewVersionAvailable := 'Нова версия на програмата %s е налична.';
      sMsgCheckLatestVersion := 'Проверка за обновление';
      sMsgMaxFilesizeForLogFile := 'Максимална големина на журналния файл MB:';
      sMsgValueNotAccepted := 'Променливата не е приета.';
      sMsgMaxNumActiveScans := 'Максимален брой активни едновременни сканирания (1..10): ';
      sMsgInformation := 'Информация';
      sMsgMemoryScanning := 'Сканиране на паметта';
      sMsgDriveScanning := 'Сканиране на устройство %s';
      sMsgModifiedFolder := 'Изменена папка';
      sMsgFileChanged := 'Папка: %s' + CRLF + 'Файл: %s';
      sMsgFolderChanged := 'Новата папка е идентифицирана.'
               + CRLF + 'Папка: %s';
      sMsgNewFileCreated := 'Новия файл беше идентифициран.'
               + CRLF + 'Папка: %s' + CRLF + 'Файл: %s';
      sMsgSuspiciousFile := 'Подозрителни файлове';
      sMsgObfuscatedFile := 'obfuscated file';
      sMsgbadPEHeader := ' Файл с невалиден PE формат';
      sMsgCryptedFile := 'Повреден файл';
      sMsgSuspiciousOriginFile := 'suspicious origin file';
      sMsgBadSignatureFile := 'Файл с невалиден подпис';
      sMsgSignExpired := 'Файл с невалиден подпис (expired)';
      sMsgVerifySuspiciousFile := 'Проверете този подозрителен файл:';

      sMsgModifiedRegistry := 'Регистъра е променен.';
      sMsgRegistryChanged := 'Открито е модифициране на системния регистър:'
        + CRLF + '%s';

      mnuSettings_RunOnStartup.Caption := 'Стартирай '+cProjectName+' при зареждане';
      mnuClamSentinelWebsite.Caption := 'Посети '+cProjectName+' страница';

      mnuSettings_AdvancedSettings.Caption := 'Настройки за напреднали';
      mnuAdvancedSettings_Drives.Caption := 'Избиране на диск за наблюдение';
      mnuAdvancedSettings_Extensions.Caption := 'Разширено сканиране';
      mnuAdvancedSettings_NoScan.Caption := 'Пътя или файла не е сканиран';
      mnuAdvancedSettings_FullScan.Caption := 'Пътя, където всички файлове ще бъдат сканирани';
      mnuAdvancedSettings_MaxActiveScan.Caption := 'Максимален брой на активни сканирания';
      mnuAdvancedSettings_LogMaxSize.Caption := 'Максимална големина на журналния файл';
      mnuAdvancedSettings_UseVxdOnWin98.Caption := 'Използвай vxd за промени по-файловата система';
      mnuSettings.Caption := 'Настройки';
      mnuSettings_MemoryScanAtStartup.Caption := 'Проверка на паметта при стартиране на програмата';
      mnuSettings_Log.Caption := 'Запиши сканирането в журнален файл';
      mnuSettings_DetectNewDrives.Caption := 'Откриване и наблюдаване на нови устройства';
      mnuSettings_AskForScanNewDrives.Caption := 'Питане за сканирани на нови устройства';

      mnuSettings_MonitorSystemChanges.Caption := 'Наблюдаване на системата за нови вируси';
      mnuSettings_MonitorSystemChanges_DetectionWithWarnings.Caption := 'Откриване на подозрителни файлове и предупреждаване за промени в системата';
      mnuSettings_MonitorSystemChanges_DetectionOnly.Caption := 'Откриване на подозрителни файлове само';
      mnuSettings_MonitorSystemChanges_NoDetection.Caption := 'Забранено';
      mnuSettings_MonitorSystemChanges_SkipFilesWithValidSignature.Caption := 'Пропусни файлове с валиден цифров подпис';
      
      mnuSettings_InfectedFiles.Caption := 'Какво да се извършва, когато е открит подозрителен файл';

      mnuInfectedFiles_MoveToQuarantine.Caption := 'Преместване под карантина';
      mnuInfectedFiles_ReportOnly.Caption := 'Само проверка';

      mnuSettings_NotifyNewVersion.Caption := 'Уведомяване за нова версия';

      mnuMemoryScan.Caption := 'Сканиране на паметта';
      mnuQuarantine.Caption := 'Карантинна папка';
      mnuQuarantineTools.Caption := 'Карантина';
      mnuOpen.Caption := 'Журнал';
      mnuRealTimeLog.Caption := 'Проверка в реално време';
      mnuMessagesLog.Caption := 'Съобщение';
      mnuQuarantineLog.Caption := 'Карантина';
      mnuMemoryLog.Caption := 'Проверка на паметта';
      mnuDriveScanLog.Caption := 'Проверка на устройство';
      mnuStart.Caption := 'Старт';
      mnuStop.Caption := 'Спиране';
      mnuCheckVersion.Caption := 'Проверка за нова версия';
      mnuAbout.Caption := 'За програмата';
      mnuExit.Caption := 'Изход';
    end;

    Indonesian:
    begin
      sMsgStopped := 'Dihentikan';
      sMsgRunOnSystemStartup := 'Akankah anda menjalankan ' + cProjectName + ' pada saat sistem dimulai?';
      sMsgClamWinConfNotFound := 'Berkas '+SCANNER_CONF_FILE+' tidak ditemukan.'
                 +CRLF+CRLF+'Silahkan menyunting nilai yang benar ke dalam berkas:';
      sMsgFileNotFound := 'Berkas %s tidak ditemukan.';
      sMsgScanning := 'Memindai...';
      sMsgDriveHasBeenInserted := 'Drive %s telah disisipkan.'
                    + CRLF + CRLF + 'Akankah anda memindai virus sekarang?';
      sMsgCaptionDetected := 'Konfirmasi: Drive %s terdeteksi';
      sMsgVirusMovedToQuarantine := 'Sejumlah %s telah dipindahkan ke karantina!';
      sMsgVirusNotMovedToQuarantine := 'Sejumlah %s telah ditemukan tetapi TIDAK dipindahkan ke karantina (hanya laporan)!';
      sMsgFalsePositive := 'positif palsu';
      sMsgVersionInfo := cProjectName + ' Versi ' + VERSION;
      sMsgAboutInfo := 'Suatu pemindai residensial untuk antivirus gratis ClamWin.'
       +CR+CR + cURL + CR
       +CR+CR + 'Dikembangkan oleh Andrea Russo Italy'
       +CR+CR + 'Terimakasih khusus untuk Robert Scroggins.'
       +CR+CR + 'Para penyumbang: ' + sCredits;
      sMsgFileReadOnly := 'Tidak dapat membuka berkas %s untuk penulisan.';
      sMsgHaveLatestVersion := 'Anda memiliki versi terbaru.';
      sMsgNewVersionAvailable := 'Versi baru %s telah tersedia.';
      sMsgCheckLatestVersion := 'Periksa Versi Terbaru';
      sMsgMaxFilesizeForLogFile := 'Ukuran maksimum untuk berkas log MB:';
      sMsgValueNotAccepted := 'Nilai tidak dapat diterima.';
      sMsgMaxNumActiveScans := 'Jumlah maksimum pemindaian aktif simultan (1..10): ';
      sMsgInformation := 'Informasi';
      sMsgMemoryScanning := 'Memindai memori';
      sMsgDriveScanning := 'Memindai drive %s';
      sMsgModifiedFolder := 'Folder telah diubah';
      sMsgFileChanged := 'Folder: %s' + CRLF + 'Berkas: %s';
      sMsgFolderChanged := 'Folder baru telah teridentifikasi.'
               + CRLF + 'Folder: %s';
      sMsgNewFileCreated := 'Berkas baru telah teridentifikasi.'
               + CRLF + 'Folder: %s' + CRLF + 'Berkas: %s';
      sMsgSuspiciousFile := 'berkas dicurigai';
      sMsgObfuscatedFile := 'berkas tidak jelas';
      sMsgbadPEHeader := 'berkas dengan format PE tidak valid';
      sMsgCryptedFile := 'berkas tersandi';
      sMsgSuspiciousOriginFile := 'berkas asli dicurigai';
      sMsgBadSignatureFile := 'berkas dengan tandatangan tidak valid';
      sMsgSignExpired  := 'berkas dengan tandatangan tidak valid (berakhir)';

      sMsgVerifySuspiciousFile := 'Silahkan verifikasi berkas yang dicurigai ini:';

      sMsgModifiedRegistry := 'Registri telah diubah';
      sMsgRegistryChanged := 'Telah ditemukan perubahan pada kunci registri:'
        + CRLF + '%s';

      mnuSettings_RunOnStartup.Caption := 'Jalankan '+cProjectName+' pada saat sistem dimulai';
      mnuClamSentinelWebsite.Caption := 'Kunjungi situs web '+cProjectName;

      mnuSettings_AdvancedSettings.Caption := 'Pengaturan yang diperluas';
      mnuAdvancedSettings_Drives.Caption := 'Pilih drive untuk dipantau';
      mnuAdvancedSettings_Extensions.Caption := 'Ekstensi telah dipindai';
      mnuAdvancedSettings_NoScan.Caption := 'Lokasi atau berkas tidak dipindai';
      mnuAdvancedSettings_FullScan.Caption := 'Lokasi di mana seluruh berkas akan dipindai';
      mnuAdvancedSettings_MaxActiveScan.Caption := 'Jumlah maksimum pemindaian aktif simultan';
      mnuAdvancedSettings_LogMaxSize.Caption := 'Ukuran maksimum untuk berkas log';
      mnuAdvancedSettings_UseVxdOnWin98.Caption := 'Gunakan vxd untuk mendeteksi perubahan pada berkas sistem';
      mnuSettings.Caption := 'Pengaturan';
      mnuSettings_MemoryScanAtStartup.Caption := 'Pindai memori saat program dimulai';
      mnuSettings_Log.Caption := 'Tulis aktifitas pemindaian ke dalam log';
      mnuSettings_DetectNewDrives.Caption := 'Deteksi dan pantau drive baru';
      mnuSettings_AskForScanNewDrives.Caption := 'Tanya untuk memindai drive baru';

      mnuSettings_MonitorSystemChanges.Caption := 'Pantau sistem terhadap malware baru';
      mnuSettings_MonitorSystemChanges_DetectionWithWarnings.Caption := 'Deteksi berkas yang dicurigai dan peringatkan jika terjadi perubahan pada sistem';
      mnuSettings_MonitorSystemChanges_DetectionOnly.Caption := 'Deteksi hanya berkas yang dicurigai';
      mnuSettings_MonitorSystemChanges_NoDetection.Caption := 'Tidak diaktifkan';
      mnuSettings_MonitorSystemChanges_SkipFilesWithValidSignature.Caption := 'Lewati file dengan tanda tangan digital sah';

      mnuSettings_InfectedFiles.Caption := 'Apa yang akan dilakukan ketika ditemukan berkas yang terinfeksi';

      mnuInfectedFiles_MoveToQuarantine.Caption := 'Pindahkan ke folder karantina';
      mnuInfectedFiles_ReportOnly.Caption := 'Hanya laporkan';

      mnuSettings_NotifyNewVersion.Caption := 'Beritahu versi baru';

      mnuMemoryScan.Caption := 'Pindai Memori';
      mnuQuarantine.Caption := 'Folder Karantina';
      mnuQuarantineTools.Caption := 'Karantina';
      mnuOpen.Caption := 'Log';
      mnuRealTimeLog.Caption := 'Pindai Waktu Nyata';
      mnuMessagesLog.Caption := 'Pesan';
      mnuQuarantineLog.Caption := 'Karantina';
      mnuMemoryLog.Caption := 'Pindai Memori';
      mnuDriveScanLog.Caption := 'Pindai Drive';
      mnuStart.Caption := 'Mulai';
      mnuStop.Caption := 'Berhenti';
      mnuCheckVersion.Caption := 'Periksa Versi Terbaru';
      mnuAbout.Caption := 'Tentang';
      mnuExit.Caption := 'Keluar';
    end;

    Azeri:
    begin
      sMsgStopped := 'Dayandirilan';
      sMsgRunOnSystemStartup := 'Sistem buraxilmasinda baslamaq  ' + cProjectName + ' istediyinizden eminsiniz?';
      sMsgClamWinConfNotFound := 'Fayl '+SCANNER_CONF_FILE+' movcud deyil.'
                 +CRLF+CRLF+'Lutfen fayla duzgun deyerleri redakte edin:';
      sMsgFileNotFound := 'Fayl %s movcud deyil.';
      sMsgScanning := 'Yoxlamaq...';
      sMsgDriveHasBeenInserted := 'Surucu %s qeyde alindi.'
                    + CRLF + CRLF + 'Viruslar ucun indi axtaris etmek isteyirsiniz?';
      sMsgCaptionDetected := 'Tesdiq edin: Surucu %s tapildi';
      sMsgVirusMovedToQuarantine := '%s karantine alindi!';
      sMsgVirusNotMovedToQuarantine := '%s tapildi ancaq karantine alinmadi (yalniz melumat)!';
      sMsgFalsePositive := 'yalan netice';
      sMsgVersionInfo := cProjectName + ' Versiya ' + VERSION;
      sMsgAboutInfo := 'Clamwin ucun daimi yoxlanma.'
       +CR+CR + cURL + CR
       +CR+CR + 'Andrea Russo Italy terefinden yaradildi'
       +CR+CR + 'Xususi minnetdarliq Robert Scroggins.'
       +CR+CR + 'Kreditler: ' + sCredits;
      sMsgFileReadOnly := 'Yazmaq ucun %s fayli aca bilmir.';
      sMsgHaveLatestVersion := 'Sizde en sonuncu versiyadir.';
      sMsgNewVersionAvailable := 'Yeni versiya %s movcuddur.';
      sMsgCheckLatestVersion := 'Sonuncu versiyani yoxlayin';
      sMsgMaxFilesizeForLogFile := 'Log fayllari ucun maks olcu MB:';
      sMsgValueNotAccepted := 'Deyer qebul olunmadi.';
      sMsgMaxNumActiveScans := 'Eyni zamanda aktiv axtarislarin maks sayi (1..10): ';
      sMsgInformation := 'Informasiya';
      sMsgMemoryScanning := 'Yaddasin axtarisi';
      sMsgDriveScanning := 'Surucunu axtar %s';
      sMsgModifiedFolder := 'Deyisdirilen qovluq';
      sMsgFileChanged := 'Qovluq: %s' + CRLF + 'Fayl: %s';
      sMsgFolderChanged := 'Yeni qovluq mueyyenlesdirildi.'
               + CRLF + 'Qovluq: %s';
      sMsgNewFileCreated := 'Yeni fayl mueyyenlesdirildi.'
               + CRLF + 'Qovluq: %s' + CRLF + 'Fayl: %s';
      sMsgSuspiciousFile := 'Supheli fayl';
      sMsgObfuscatedFile := 'qarisdirilan fayl';
      sMsgbadPEHeader := 'Etibarsiz PE fayl formati';
      sMsgCryptedFile := 'Sifrelenmis fayl';
      sMsgSuspiciousOriginFile := 'supheli kok fayli';
      sMsgBadSignatureFile := 'etibarsiz imza ile fayl';
      sMsgSignExpired  := 'etibarsiz imza ile fayl (expired)';
      sMsgVerifySuspiciousFile := 'Lutfen bu supheli fayli yoxla:';

      sMsgModifiedRegistry := 'Reyestr deyisdirildi';
      sMsgRegistryChanged := 'Bu reyestr acarinin deyisdirilmesi qeyde alinmishdir:'
        + CRLF + '%s';

      mnuSettings_RunOnStartup.Caption := 'Sistem baslanmasinda '+cProjectName+' ise sal';
      mnuClamSentinelWebsite.Caption := 'Ziyaret '+cProjectName+' Websayt';

      mnuSettings_AdvancedSettings.Caption := 'Esas qaydaya salmalar';
      mnuAdvancedSettings_Drives.Caption := 'Nezaret edilesi diskleri secin';
      mnuAdvancedSettings_Extensions.Caption := 'Genislemeleri yoxlayin';
      mnuAdvancedSettings_NoScan.Caption := 'Yol veya fayl axtarilmadi';
      mnuAdvancedSettings_FullScan.Caption := 'Butun fayllarin gozden kecirileceyi yollar';
      mnuAdvancedSettings_MaxActiveScan.Caption := 'Eyni zamanda aktiv axtarislarin maksimal sayi';
      mnuAdvancedSettings_LogMaxSize.Caption := 'Log fayllari ucun maks olcu';
      mnuAdvancedSettings_UseVxdOnWin98.Caption := 'Sistem deyisikliklerini bilmek ucun VXD aktiv edin';
      mnuSettings.Caption := 'Qurasdirmalar';
      mnuSettings_MemoryScanAtStartup.Caption := 'Proqram basliyanda yaddasi axtar';
      mnuSettings_Log.Caption := 'Scan melumatlarini loglasdir';
      mnuSettings_DetectNewDrives.Caption := 'Yeni suruculeri askar ve nezaret edin';
      mnuSettings_AskForScanNewDrives.Caption := 'Yeni surucunu axtaranda sorus';

      mnuSettings_MonitorSystemChanges.Caption := 'Yeni malware ucun sisteme nezaret edin';
      mnuSettings_MonitorSystemChanges_DetectionWithWarnings.Caption := 'Supheli fayli tapin ve sistem deyisiklikleri haqqinda melumat verin';
      mnuSettings_MonitorSystemChanges_DetectionOnly.Caption := 'Yalniz supheli fayli qeyde al';
      mnuSettings_MonitorSystemChanges_NoDetection.Caption := 'Sonludur';
      mnuSettings_MonitorSystemChanges_SkipFilesWithValidSignature.Caption := 'Heqiqi imzali fayli rahat buraxin';

      mnuSettings_InfectedFiles.Caption := 'Yoluxdurulan fayl tapildiqda ne edim';

      mnuInfectedFiles_MoveToQuarantine.Caption := 'Karantin qovluguna gonder';
      mnuInfectedFiles_ReportOnly.Caption := 'Yalniz melumat ver';

      mnuSettings_NotifyNewVersion.Caption := 'Yeni versiyalari bildirin';

      mnuMemoryScan.Caption := 'Yaddasi axtar';
      mnuQuarantine.Caption := 'Karantin qovlugu';
      mnuQuarantineTools.Caption := 'Karantin';
      mnuOpen.Caption := 'Loglar';
      mnuRealTimeLog.Caption := 'Operativ axtaris';
      mnuMessagesLog.Caption := 'Mesajlar';
      mnuQuarantineLog.Caption := 'Karantin';
      mnuMemoryLog.Caption := 'Yaddasi axtar';
      mnuDriveScanLog.Caption := 'Surucunu axtar';
      mnuStart.Caption := 'Basla';
      mnuStop.Caption := 'Dayan';
      mnuCheckVersion.Caption := 'Yeni versiyani yoxla';
      mnuAbout.Caption := 'Haqqinda';
      mnuExit.Caption := 'Cixis';
    end;

    Dutch:
    begin
      sMsgStopped := 'Gestopt';
      sMsgRunOnSystemStartup := 'Wilt u ' + cProjectName + ' uitvoeren tijdens opstarten?';
      sMsgClamWinConfNotFound := 'Bestand '+SCANNER_CONF_FILE+' niet gevonden.'
                 +CRLF+CRLF+'S.v.p. de juiste waarden invullen in bestand:';
      sMsgFileNotFound := 'Bestand %s niet gevonden.';
      sMsgScanning := 'Scannen...';
      sMsgDriveHasBeenInserted := 'Schijf %s werd ingevoerd.'
                    + CRLF + CRLF + 'Wilt u deze nu inspecteren op virusen?';
      sMsgCaptionDetected := 'Bevestigen: Schijf %s gedetecteerd';
      sMsgVirusMovedToQuarantine := 'Een %s werd in quarantaine geplaatst!';
      sMsgVirusNotMovedToQuarantine := 'Een %s werd aangetroffen, maar is NIET in quarantaine geplaatst (Alleen rapporteren)!';
      sMsgFalsePositive := 'false positive';
      sMsgVersionInfo := cProjectName + ' Versie: ' + VERSION;
      sMsgAboutInfo := 'Een residente scanner voor het gratis antivirus programma ClamWin.'
       +CR+CR + cURL + CR
       +CR+CR + 'Ontwikkeld door Andrea Russo, Italiл'
       +CR+CR + 'Met een speciaal woord van dank aan Robert Scroggins.'
       +CR+CR + 'Credits: ' + sCredits;
      sMsgFileReadOnly := 'Kan bestand %s niet openen om te schrijven.';
      sMsgHaveLatestVersion := 'U beschikt over de latest Versie.';
      sMsgNewVersionAvailable := 'Er is een nieuwe Versie %s beschikbaar.';
      sMsgCheckLatestVersion := 'Nieuwste versie ophalen';
      sMsgMaxFilesizeForLogFile := 'Max. bestandsgrootte for logbestanden [MB]:';
      sMsgValueNotAccepted := 'Ongeldige waarde.';
      sMsgMaxNumActiveScans := 'Maximum aantal gelijktijdig actieve scans (1..10): ';
      sMsgInformation := 'Informatie';
      sMsgMemoryScanning := 'Geheugenscan';
      sMsgDriveScanning := 'Scannen van schijf %s';
      sMsgModifiedFolder := 'Gewijzigde folder';
      sMsgFileChanged := 'Folder: %s' + CRLF + 'Bestand: %s';
      sMsgFolderChanged := 'Nieuwe folder gedetecteerd.'
               + CRLF + 'Folder: %s';
      sMsgNewFileCreated := 'Nieuw bestand gedetecteerd.'
               + CRLF + 'Folder: %s' + CRLF + 'Bestand: %s';
      sMsgSuspiciousFile := 'Verdacht bestand';
      sMsgObfuscatedFile := 'Vertroebeld Bestand';
      sMsgbadPEHeader := 'Bestand met ongeldig PE formaat';
      sMsgCryptedFile := 'Versleuteld bestand';
      sMsgSuspiciousOriginFile := 'Bestand metverdachte oorsprong';
      sMsgBadSignatureFile := 'Bestand met ongeldige ondertekening';
      sMsgSignExpired := 'Bestand met ongeldige ondertekening (expired)';
      sMsgVerifySuspiciousFile := 'S.v.p. het verdachte bestand verifiлren:';

      sMsgModifiedRegistry := 'Register aangepast';
      sMsgRegistryChanged := 'De inhoud van de registersleutel is gewijzigd:'
        + CRLF + '%s';

      mnuSettings_RunOnStartup.Caption := ''+cProjectName+'uitvoeren tijdens opstarten';
      mnuClamSentinelWebsite.Caption := 'Bezoek de '+cProjectName+' website';

      mnuSettings_AdvancedSettings.Caption := 'Geavanceerde instellingen';
      mnuAdvancedSettings_Drives.Caption := 'Kies schijf om te bewaken';
      mnuAdvancedSettings_Extensions.Caption := 'Extensies gescand';
      mnuAdvancedSettings_NoScan.Caption := 'Paden of bestanden die niet worden gescand';
      mnuAdvancedSettings_FullScan.Caption := 'Paden waarvan alle bestanden zullen worden gescand';
      mnuAdvancedSettings_MaxActiveScan.Caption := 'Maximum aantal gelijktijdig actieve scans';
      mnuAdvancedSettings_LogMaxSize.Caption := 'Max. bestandsgrootte voor logbestanden';
      mnuAdvancedSettings_UseVxdOnWin98.Caption := 'Gebruik vxd om wijzigingen in het bestandssysteem te detecteren';
      mnuSettings.Caption := 'Instellingen';
      mnuSettings_MemoryScanAtStartup.Caption := 'Scan het geheugen wanneer het programma wordt gestart';
      mnuSettings_Log.Caption := 'Scan activiteiten in logbestand schrijven';
      mnuSettings_DetectNewDrives.Caption := 'Detecteer en bewaak nieuwe schijven';
      mnuSettings_AskForScanNewDrives.Caption := 'Vragen om nieuwe schijven te scannen';

      mnuSettings_MonitorSystemChanges.Caption := 'Systeem bewaken tegen nieuwe malware';
      mnuSettings_MonitorSystemChanges_DetectionWithWarnings.Caption := 'Verdachte bestanden detecteren en waarschuwen bij systeemwijzigingen';
      mnuSettings_MonitorSystemChanges_DetectionOnly.Caption := 'Alleen vedachte bestanden detecteren';
      mnuSettings_MonitorSystemChanges_NoDetection.Caption := 'Uitgeschakeld';

      mnuSettings_MonitorSystemChanges_SkipFilesWithValidSignature.Caption := 'Bestanden met een geldige ondertekening overslaan';

      mnuSettings_InfectedFiles.Caption := 'Gewenste actie voor geпnfecteerde bestanden';

      mnuInfectedFiles_MoveToQuarantine.Caption := 'In quarantaine plaatsen';
      mnuInfectedFiles_ReportOnly.Caption := 'Alleen rapporteren';

      mnuSettings_NotifyNewVersion.Caption := 'Melding tonen voor nieuwe versies';

      mnuMemoryScan.Caption := 'Geheugenscan';
      mnuQuarantine.Caption := 'Quarantaine folder';
      mnuQuarantineTools.Caption := 'Quarantaine';
      mnuOpen.Caption := 'Logboeken';
      mnuRealTimeLog.Caption := 'Real-Time scan';
      mnuMessagesLog.Caption := 'Meldingen';
      mnuQuarantineLog.Caption := 'Quarantaine';
      mnuMemoryLog.Caption := 'Geheugenscan';
      mnuDriveScanLog.Caption := 'Schijfscan';
      mnuStart.Caption := 'Start';
      mnuStop.Caption := 'Stop';
      mnuCheckVersion.Caption := 'Nieuwste versie ophalen';
      mnuAbout.Caption := 'Info';
      mnuExit.Caption := 'Afsluiten';
    end;

    Galician:
    begin
      sMsgStopped := 'Parado';
      sMsgRunOnSystemStartup := 'Desexa executar ' + cProjectName + ' у inicio do sistema?';
      sMsgClamWinConfNotFound := 'Ficheiro '+SCANNER_CONF_FILE+' non encontrado.'
                 +CRLF+CRLF+'Por favor edite os valores correctos no ficheiro:';
      sMsgFileNotFound := 'Ficheiro %s non atopado.';
      sMsgScanning := 'Analizando...';
      sMsgDriveHasBeenInserted := 'A unidade %s foi insertada.'
                    + CRLF + CRLF + 'Desexa analizar para buscar agora virus?';
      sMsgCaptionDetected := 'Confirmar: Unidade %s detectada';
      sMsgVirusMovedToQuarantine := '%s foi movido б cuarentena!';
      sMsgVirusNotMovedToQuarantine := '%s foi encontrado pero NON movido б cuarantena (Sу informar)!';
      sMsgFalsePositive := 'falso positivo';
      sMsgVersionInfo := cProjectName + ' Version ' + VERSION;
      sMsgAboutInfo := 'Un analizador de virus residente para o antivirus ClamWin de cуdigo libre.'
       +CR+CR + cURL + CR
       +CR+CR + 'Desenrolado por Andrea Russo Italy'
       +CR+CR + 'Un millуn de grazas a Robert Scroggins.'
       +CR+CR + 'Creditos: ' + sCredits;
      sMsgFileReadOnly := 'Non se pode abrir o ficheiro %s para escribir.';
      sMsgHaveLatestVersion := 'Dispуn da ъltima versiуn publicada ata o momento.';
      sMsgNewVersionAvailable := 'Existe a versiуn %s dispoснbel.';
      sMsgCheckLatestVersion := 'Comprobar se hai nova versiуn';
      sMsgMaxFilesizeForLogFile := 'Tamaсo Mбximo para os ficheiros de rexistro en MB:';
      sMsgValueNotAccepted := 'Valor non aceptado.';
      sMsgMaxNumActiveScans := 'Nъmero Mбximo de anбlises simultбneos activos (1..10): ';
      sMsgInformation := 'Informaciуn';
      sMsgMemoryScanning := 'Anбlise na memoria';
      sMsgDriveScanning := 'Analizando a unidade %s';
      sMsgModifiedFolder := 'Cartafol modificado';
      sMsgFileChanged := 'Cartafol: %s' + CRLF + 'Ficheiro: %s';
      sMsgFolderChanged := 'Un cartafol novo foi identificado.'
               + CRLF + 'Cartafol: %s';
      sMsgNewFileCreated := 'Un novo ficheiro identificado.'
               + CRLF + 'Cartafol: %s' + CRLF + 'Ficheiro: %s';
      sMsgSuspiciousFile := 'ficheiro sospeitoso atopado';
      sMsgObfuscatedFile := 'Ficheiro ofuscado';
      sMsgbadPEHeader := 'ficheiro con formato PE invalido';
      sMsgCryptedFile := 'Ficheiro encriptado';
      sMsgSuspiciousOriginFile := 'Ficheiro de orixe sospeitosa';
      sMsgBadSignatureFile := 'ficheiro con unha firma invalida';
      sMsgSignExpired := 'ficheiro con unha firma expirada';
      sMsgVerifySuspiciousFile := 'Por favor verifique este ficheiro sospeitoso:';

      sMsgModifiedRegistry := 'Rexistro modificado';
      sMsgRegistryChanged := 'Foi atopada unha modificaciуn do rexistro nunha chave deste:'
        + CRLF + '%s';

      mnuSettings_RunOnStartup.Caption := 'Executar '+cProjectName+' у inicio';
      mnuClamSentinelWebsite.Caption := 'Visitar a pбxina Web de '+cProjectName+' oficial';

      mnuSettings_AdvancedSettings.Caption := 'Configuraciуns avanzadas';
      mnuAdvancedSettings_Drives.Caption := 'Escolla os discos a supervisar';
      mnuAdvancedSettings_Extensions.Caption := 'Extensiуns a analizar';
      mnuAdvancedSettings_NoScan.Caption := 'Rutas e ficheiros non analizados';
      mnuAdvancedSettings_FullScan.Caption := 'Rutas nas que todos os ficheiros serбn analizados';
      mnuAdvancedSettings_MaxActiveScan.Caption := 'Nъmero mбximo de analises simultaneos';
      mnuAdvancedSettings_UseVxdOnWin98.Caption := 'Use vxd para detectar cambios no ficheiro de arquivo';
      mnuSettings.Caption := 'Configuraciуns';
      mnuSettings_MemoryScanAtStartup.Caption := 'Analizar a memoria cando se execute o programa';
      mnuSettings_Log.Caption := 'Escribir actividade de anбlise no ficheiro de rexistro';
      mnuSettings_DetectNewDrives.Caption := 'Detectar e supervisar novas unidades';
      mnuSettings_AskForScanNewDrives.Caption := 'Preguntar se se desexa analizar unidades novas insertadas';

      mnuSettings_MonitorSystemChanges.Caption := 'Supervisar sistema para novos virus';
      mnuSettings_MonitorSystemChanges_DetectionWithWarnings.Caption := 'Detectar ficheiros sospeitosos e avisar acerca dos cambios do sistema';
      mnuSettings_MonitorSystemChanges_DetectionOnly.Caption := 'Detectar sу ficheiros sospeitosos';
      mnuSettings_MonitorSystemChanges_NoDetection.Caption := 'Desactivar';
      mnuSettings_MonitorSystemChanges_SkipFilesWithValidSignature.Caption := 'Saltar ficheiros con unha sinatura dixital vбlida (reduce a seguridade)';

      mnuSettings_InfectedFiles.Caption := 'Que facer cando un ficheiro infectado й encontrado';

      mnuInfectedFiles_MoveToQuarantine.Caption := 'Mover б cuarentena';
      mnuInfectedFiles_ReportOnly.Caption := 'Sу informar';

      mnuSettings_NotifyNewVersion.Caption := 'Notificar cando haxa unha nova versiуn';

      mnuMemoryScan.Caption := 'Anбlise da memoria';
      mnuQuarantine.Caption := 'Cartafol de Cuarentena';
      mnuQuarantineTools.Caption := 'Cuarentena';
      mnuOpen.Caption := 'Rexistros';
      mnuRealTimeLog.Caption := 'Anбlise en tempo real';
      mnuMessagesLog.Caption := 'Mensaxes';
      mnuQuarantineLog.Caption := 'Cuarentena';
      mnuMemoryLog.Caption := 'Anбlise da memoria';
      mnuDriveScanLog.Caption := 'Anбlise de unidades';
      mnuStart.Caption := 'Activar';
      mnuStop.Caption := 'Desactivar';
      mnuCheckVersion.Caption := 'Consultar ъltima versiуn';
      mnuAbout.Caption := 'Acerca de';
      mnuExit.Caption := 'Saнr';
    end;

    Hebrew:
    begin
      sMsgStopped := 'ртцш';
      sMsgRunOnSystemStartup := 'дан мдшйх аъ ' + cProjectName + ' бъзймд?';
      sMsgClamWinConfNotFound := 'чебх '+SCANNER_CONF_FILE+' ма роца.'
                 +CRLF+CRLF+'ра мтглп аъ дтшлйн бчебх:';
      sMsgFileNotFound := 'чебх %s ма роца.';
      sMsgScanning := 'сешч...';
      sMsgDriveHasBeenInserted := 'лерп %s делрс лтъ.'
                    + CRLF + CRLF + 'дан шцерк мсшеч рвг ейшесйн?';
      sMsgCaptionDetected := 'ра мащш: лерп %s жедд';
      sMsgVirusMovedToQuarantine := '%s детбш мдсвш!';
      sMsgVirusNotMovedToQuarantine := '%s роца ак _ма_ детбш мдсвш! мйгйтд бмбг!';
      sMsgFalsePositive := 'ажтчъ щееа';
      sMsgVersionInfo := cProjectName + ' вшсд ' + VERSION;
      sMsgAboutInfo := 'дъчп сшйчд тбеш ъелръ дарийейшес чмаоеейп - рцзеп-дсширеп.'
       +CR+CR + cURL + CR
       +CR+CR + 'феъз бйгй аршйад шесе байимйд'
       +CR+CR + 'ъегд ойезгъ мшебши счшевйрс.'
       +CR+CR + 'ъегеъ: ' + sCredits;
      sMsgFileReadOnly := 'чебх %s ма рфъз млъйбд.';
      sMsgHaveLatestVersion := 'бйглн двшсд дотеглръ бйеъш.';
      sMsgNewVersionAvailable := 'вшсд згщд %s чййоъ мдешгд.';
      sMsgCheckLatestVersion := 'бгче аъ двшсд дазшерд';
      sMsgMaxFilesizeForLogFile := 'вегм ойшбй мчбцй йеоп бовдбййи:';
      sMsgValueNotAccepted := 'тшк ма очебм.';
      sMsgMaxNumActiveScans := 'осфш ойшбй щм сшйчеъ бе жорйъ (1..10): ';
      sMsgInformation := 'ойгт';
      sMsgMemoryScanning := 'сшйчъ жлшеп';
      sMsgDriveScanning := 'сешч аъ лерп %s';
      sMsgModifiedFolder := 'ъйчййд дщъръд';
      sMsgFileChanged := 'ъйчййд: %s' + CRLF + 'чебх: %s';
      sMsgFolderChanged := 'жедъд ъйчййд згщд.'
               + CRLF + 'ъйчййд: %s';
      sMsgNewFileCreated := 'чебх згщ жедд.'
               + CRLF + 'ъйчййд: %s' + CRLF + 'File: %s';
      sMsgSuspiciousFile := 'чебх зщег';
      sMsgObfuscatedFile := 'чебх ма чшйа';
      sMsgbadPEHeader := 'чебх бъбрйъ PE ма ъчйрд';
      sMsgCryptedFile := 'чебх оецфп';
      sMsgSuspiciousOriginFile := 'чебх оочеш зщег';
      sMsgBadSignatureFile := 'чебх тн зъйод ма ъчйрд';
      sMsgSignExpired := 'чебх тн зъйод ма ъчйрд';
      sMsgVerifySuspiciousFile := 'ра бгче чебх зщег:';

      sMsgModifiedRegistry := 'дшйщен ботшлъ змереъ дщърд';
      sMsgRegistryChanged := 'роца щщйрей бофъз дшйщен:'
        + CRLF + '%s';

      mnuSettings_RunOnStartup.Caption := 'дшх '+cProjectName+' бтъ аъзем змереъ';
      mnuClamSentinelWebsite.Caption := 'бчше баъш '+cProjectName;

      mnuSettings_AdvancedSettings.Caption := 'двгшеъ оъчгоеъ';
      mnuAdvancedSettings_Drives.Caption := 'бзш леррйн мотчб';
      mnuAdvancedSettings_Extensions.Caption := 'рсшче дшзбеъ';
      mnuAdvancedSettings_NoScan.Caption := 'ойчен ае чбцйн щма рсшче';
      mnuAdvancedSettings_FullScan.Caption := 'осмемй чбцйн бдн лм дчбцйн ййсшче';
      mnuAdvancedSettings_MaxActiveScan.Caption := 'осфш ойшбй щм сшйчеъ бе жорйъ';
      mnuAdvancedSettings_LogMaxSize.Caption := 'вегм ойшбй щм чебх дйеоп';
      mnuAdvancedSettings_UseVxdOnWin98.Caption := 'дщъощ бVXD лгй мждеъ щйреййн ботшлъ дчбцйн';
      mnuSettings.Caption := 'двгшеъ';
      mnuSettings_MemoryScanAtStartup.Caption := 'сшеч аъ джйлшеп лщдълрйъ оъзймд';
      mnuSettings_Log.Caption := 'шщен аъ фтймеъ дсшйчд бйеоп';
      mnuSettings_DetectNewDrives.Caption := 'ждд етчеб азш леррйн згщйн';
      mnuSettings_AskForScanNewDrives.Caption := 'щам ан мсшеч леррйн згщйн';

      mnuSettings_MonitorSystemChanges.Caption := 'тчеб азш дотшлъ бзйфещ азш режчд згщд';
      mnuSettings_MonitorSystemChanges_DetectionWithWarnings.Caption := 'ждд чбцйн зщегйн егеез тм щйрейй отшлъ';
      mnuSettings_MonitorSystemChanges_DetectionOnly.Caption := 'ждд чбцйн зщегйн бмбг';
      mnuSettings_MonitorSystemChanges_NoDetection.Caption := 'ма фтйм';
      mnuSettings_MonitorSystemChanges_SkipFilesWithValidSignature.Caption := 'гмв тм чбцйн тн зъйод гйвйимйъ ъчйрд (оройк аъ шоъ дабизд)';

      mnuSettings_InfectedFiles.Caption := 'од мтщеъ лщроца чебх ожедн';

      mnuInfectedFiles_MoveToQuarantine.Caption := 'дтбш мъйчййъ ддсвш';
      mnuInfectedFiles_ReportOnly.Caption := 'гйеез бмбг';

      mnuSettings_NotifyNewVersion.Caption := 'дегт тм вшсаеъ згщеъ';

      mnuMemoryScan.Caption := 'сшйчъ жйлшеп';
      mnuQuarantine.Caption := 'ъйчййъ дсвш';
      mnuQuarantineTools.Caption := 'дсвш';
      mnuOpen.Caption := 'йеорйн';
      mnuRealTimeLog.Caption := 'сшйчд ъек лгй тбегд';
      mnuMessagesLog.Caption := 'дегтеъ';
      mnuQuarantineLog.Caption := 'дсвш';
      mnuMemoryLog.Caption := 'сшйчъ жйлшеп';
      mnuDriveScanLog.Caption := 'сшйчъ леррйн';
      mnuStart.Caption := 'дъзм';
      mnuStop.Caption := 'тцеш';
      mnuCheckVersion.Caption := 'бгеч вшсд азшерд';
      mnuAbout.Caption := 'аегеъ';
      mnuExit.Caption := 'йцйад';
    end;
  end;
end;
end;

procedure SetLanguage_FDrives(F : TFDrives);
begin
with F do
begin
  Case CurLang of
    Italian:
    begin
      chkListDrives.Font.Charset := ANSI_CHARSET;

      btnConfirm.Caption := '&Conferma';
      btnCancel.Caption := '&Annulla';
      Caption := cProjectName + ' - Dischi monitorati';

      sMsgRemovableDevice := 'Disco rimovibile';
      sMsgFixedDevice := 'Disco locale';
      sMsgRemoteDevice := 'Unitа di rete';
      sMsgAbsentDevice := 'Assente';
      sMsgNotReadyDevice := 'non pronto';
    end;

    English:
    begin
      chkListDrives.Font.Charset := ANSI_CHARSET;

      (*
      btnConfirm.Caption := '&Confirm';
      btnCancel.Caption := 'Ca&ncel';
      Caption := cProjectName + ' - Fixed disks monitored';
      *)

      sMsgRemovableDevice := 'Removable Disk';
      sMsgFixedDevice := 'Local Disk';
      sMsgRemoteDevice := 'Network Drive';
      sMsgAbsentDevice := 'Absent';
      sMsgNotReadyDevice := 'not ready';
    end;

    French:
    begin
      chkListDrives.Font.Charset := ANSI_CHARSET;

      btnConfirm.Caption := '&Confirmer';
      btnCancel.Caption := '&Annuler';
      Caption := cProjectName + ' - Surveillance des disques';

      sMsgRemovableDevice := 'Disque amovible';
      sMsgFixedDevice := 'Disque local';
      sMsgRemoteDevice := 'Disque rйseau';
      sMsgAbsentDevice := 'Absent';
      sMsgNotReadyDevice := 'pas prкt';
    end;

    Japanese:
    begin
      chkListDrives.Font.Charset := SHIFTJIS_CHARSET;

      btnConfirm.Caption := 'Љm”F';
      btnCancel.Caption := 'ѓLѓѓѓ“ѓZѓ‹';
      Caption := cProjectName + ' - ЊЕ’иѓfѓBѓXѓN‚рЉДЋ‹‚·‚й';
      sMsgRemovableDevice := 'ѓЉѓЂЃ[ѓoѓuѓ‹ѓfѓBѓXѓN';

      sMsgFixedDevice := 'ѓЌЃ[ѓJѓ‹ѓfѓBѓXѓN';
      sMsgRemoteDevice := 'ѓlѓbѓgѓЏЃ[ѓNѓhѓ‰ѓCѓu';
      sMsgAbsentDevice := 'Њ©‚В‚©‚з‚И‚ў';
      sMsgNotReadyDevice := 'ЏЂ”х‚ЄЏo—€‚Д‚ў‚И‚ў';
    end;

    German:
    begin
      chkListDrives.Font.Charset := ANSI_CHARSET;

      btnConfirm.Caption := '&Bestдtigen';
      btnCancel.Caption := '&Abbrechen';
      Caption := cProjectName + ' - Fest installierte Festplatten ьberwachen';

      sMsgRemovableDevice := 'Austauschbare Laufwerke';
      sMsgFixedDevice := 'Lokale Festplatten';
      sMsgRemoteDevice := 'Netzlaufwerk';
      sMsgAbsentDevice := 'Abwesend';
      sMsgNotReadyDevice := 'Nicht bereit';
    end;

    Spanish:
    begin
      chkListDrives.Font.Charset := ANSI_CHARSET;

      btnConfirm.Caption := 'Confirmar';
      btnCancel.Caption := 'Cancelar';
      Caption := cProjectName + ' - Marcar discos a monitorear';

      sMsgRemovableDevice := 'Disco Extraнble';
      sMsgFixedDevice := 'Disco Local';
      sMsgRemoteDevice := 'Disco de Red';
      sMsgAbsentDevice := 'Faltante';
      sMsgNotReadyDevice := 'No esta listo';
    end;

    Polish:
    begin
      chkListDrives.Font.Charset := EASTEUROPE_CHARSET;

      btnConfirm.Caption := 'Po&twierd'+#$9F;
      btnCancel.Caption := 'An&uluj';
      Caption := cProjectName + ' - Naprawiono monitorowane dyski';

      sMsgRemovableDevice := 'Dysk przeno'+#$9C+'ny';
      sMsgFixedDevice := 'Lokalny dysk';
      sMsgRemoteDevice := 'Dysk sieciowy';
      sMsgAbsentDevice := 'Nieobecny';
      sMsgNotReadyDevice := 'Niegotowy';
    end;

    Russian:
    begin
      chkListDrives.Font.Charset := RUSSIAN_CHARSET;
      btnConfirm.Caption := '&Ок';
      btnCancel.Caption := 'О&тмена';
      Caption := cProjectName + ' - Выберите диски';

      sMsgRemovableDevice := 'Съемный диск';
      sMsgFixedDevice := 'Локальный диск';
      sMsgRemoteDevice := 'Сетевой диск';
      sMsgAbsentDevice := 'Отсутствует';
      sMsgNotReadyDevice := 'не готов';
    end;

    Portuguese:
    begin
      chkListDrives.Font.Charset := ANSI_CHARSET;
      
      btnConfirm.Caption := '&Confirmar';
      btnCancel.Caption := 'Ca&ncelar';
      Caption := cProjectName + ' - Unidades monitoradas';

      sMsgRemovableDevice := 'Disco removнvel';
      sMsgFixedDevice := 'Drive local';
      sMsgRemoteDevice := 'Drive de rede';
      sMsgAbsentDevice := 'Ausente';
      sMsgNotReadyDevice := 'nгo estб pronto';
    end;

    Bulgarian:
    begin
      btnConfirm.Caption := '&Добре';
      btnCancel.Caption := 'От&каз';
      Caption := cProjectName + ' - Fixed disks monitored';

      sMsgRemovableDevice := 'Сменяем диск';
      sMsgFixedDevice := 'Локален диск';
      sMsgRemoteDevice := 'Мрежово устройство';
      sMsgAbsentDevice := 'Липсващ';
      sMsgNotReadyDevice := 'не е готово';
    end;

    Indonesian:
    begin
      btnConfirm.Caption := '&Konfirmasi';
      btnCancel.Caption := 'Ba&tal';
      Caption := cProjectName + ' - Disk fixed terpantau';

      sMsgRemovableDevice := 'Disk removable';
      sMsgFixedDevice := 'Disk Lokal';
      sMsgRemoteDevice := 'Drive Jejaring';
      sMsgAbsentDevice := 'Tidak ada';
      sMsgNotReadyDevice := 'tidak siap';
    end;

    Azeri:
    begin
      chkListDrives.Font.Charset := TURKISH_CHARSET;

      btnConfirm.Caption := '&Tekrar';
      btnCancel.Caption := 'Le&gv et';
      Caption := cProjectName + ' - Hazir diske nezaret edildi';

      sMsgRemovableDevice := 'Novbeli Disk';
      sMsgFixedDevice := 'Lokal Disk';
      sMsgRemoteDevice := 'Sebeke surucusu';
      sMsgAbsentDevice := 'Istirak etmeyin';
      sMsgNotReadyDevice := 'Hazir deyil';
    end;

    Dutch:
    begin
      btnConfirm.Caption := '&Bevestigen';
      btnCancel.Caption := '&Annuleren';
      Caption := cProjectName + ' - Vaste schijven worden bewaakt';

      sMsgRemovableDevice := 'Wisselbare schijven';
      sMsgFixedDevice := 'Lokale schijf';
      sMsgRemoteDevice := 'Networkschijf';
      sMsgAbsentDevice := 'Niet aanwezig';
      sMsgNotReadyDevice := 'Niet beschikbaar';
    end;

    Galician:
    begin
      btnConfirm.Caption := '&Confirmar';
      btnCancel.Caption := 'Ca&ncelar';
      Caption := cProjectName + ' - Supervisar discos seleccionados';

      sMsgRemovableDevice := 'Disco extraнble';
      sMsgFixedDevice := 'Disco local';
      sMsgRemoteDevice := 'Unidade de rede';
      sMsgAbsentDevice := 'Ausente';
      sMsgNotReadyDevice := 'non preparado';
    end;

    Hebrew:
    begin
      btnConfirm.Caption := '&amp;ащш';
      btnCancel.Caption := 'Ca&amp;ncel';
      Caption := cProjectName + ' - леррйн чбетйн ботчб';

      sMsgRemovableDevice := 'лерп рйъп мдсшд';
      sMsgFixedDevice := 'лерп очеой';
      sMsgRemoteDevice := 'лерп бшщъ';
      sMsgAbsentDevice := 'зсш';
      sMsgNotReadyDevice := 'ма оелп';
    end;
  end;
end;
end;

procedure SetLanguage_FChangeList(F : TFChangeList);
begin
with F do
begin
  Case CurLang of
    Italian:
    begin
      btnConfirm.Caption := '&Conferma';
      btnCancel.Caption := '&Annulla';

      btnReplace.Caption := 'Ca&mbia';
      btnAdd.Caption := 'A&ggiungi';
      btnDelete.Caption := 'Ca&ncella';

      sMsgValueAlreadyExists := 'Il valore %s и giа presente.';
      sMsgNoExtensionsScannedDefined := 'Nessuna estensione definita.';
      sMsgSelectFolder := 'Selezionare una cartella';
      sMsgSelectFile := 'Selezionare un file';

      sbSelectFolder.Hint := 'Seleziona una cartella';
      sbSelectFile.Hint := 'Seleziona un file';

      case ListType of
        ScanPaths: Caption := cProjectName + ' - Cartelle o file non controllati';
        Extensions: Caption := cProjectName + ' - Estensioni controllate';
        FullScanFolders: Caption := cProjectName + ' - Cartelle in cui controllare qualsiasi file';
      end;
    end;

    English:
    begin
      (*
      btnConfirm.Caption := '&Confirm';
      btnCancel.Caption := 'Ca&ncel';

      btnReplace.Caption := '&Replace';
      btnAdd.Caption := '&Add';
      btnDelete.Caption := '&Delete';
      *)

      sMsgValueAlreadyExists := 'Value %s already exists.';
      sMsgNoExtensionsScannedDefined := 'No extensions scanned defined.';
      sMsgSelectFolder := 'Select a folder';
      sMsgSelectFile := 'Select a file';

      sbSelectFolder.Hint := 'Select a folder';
      sbSelectFile.Hint := 'Select a file';

      case ListType of
        ScanPaths: Caption := cProjectName + ' - Paths or files not scanned';
        Extensions: Caption := cProjectName + ' - Extensions scanned';
        FullScanFolders: Caption := cProjectName + ' - Paths where all files will be scanned';
      end;
    end;

    French:
    begin
      btnConfirm.Caption := '&Confirmer';
      btnCancel.Caption := '&Annuler';

      btnReplace.Caption := 'R&emplacer';
      btnAdd.Caption := 'A&jouter';
      btnDelete.Caption := 'Su&pprimer';

      sMsgValueAlreadyExists := 'La valeur %s est dйja prйsente.';
      sMsgNoExtensionsScannedDefined := 'Aucune extension dйfinie.';

      sMsgSelectFolder := 'Sйlectionner un dossier';
      sMsgSelectFile := 'Sйlectionner un fichier';

      sbSelectFolder.Hint := 'Sйlectionner un dossier';
      sbSelectFile.Hint := 'Sйlectionner un fichier';

      case ListType of
        ScanPaths: Caption := cProjectName + ' - Dossier ou fichiers non surveillй';
        Extensions: Caption := cProjectName + ' - Extensions surveillйes';
        FullScanFolders: Caption := cProjectName + ' - Dossier complиtement surveillй';
      end;
    end;

    Japanese:
    begin
      btnConfirm.Caption := 'Љm”F';
      btnCancel.Caption := 'ѓLѓѓѓ“ѓZѓ‹';
      btnReplace.Caption := '’uЉ·';
      btnAdd.Caption := '’З‰Б';
      btnDelete.Caption := 'ЌнЏњ';
      sMsgValueAlreadyExists := '’l %s ‚НЉщ‚Й‘¶ЌЭ‚µ‚Д‚ў‚Ь‚·';
      sMsgNoExtensionsScannedDefined := 'Љg’ЈѓXѓLѓѓѓ“‚Н’и‹`‚і‚к‚Д‚ў‚Ь‚№‚с';
      sMsgSelectFolder := 'ѓtѓHѓ‹ѓ_‚р‘I‘р';
      sMsgSelectFile := 'ѓtѓ@ѓCѓ‹‚р‘I‘р';
      sbSelectFolder.Hint := 'ѓtѓHѓ‹ѓ_‚р‘I‘р';
      sbSelectFile.Hint := 'ѓtѓ@ѓCѓ‹‚р‘I‘р';

      case ListType of
        ScanPaths: Caption := cProjectName + ' - ѓXѓLѓѓѓ“‚і‚к‚И‚ўѓpѓX‚Ь‚Ѕ‚Нѓtѓ@ѓCѓ‹‚рЋw’и';
        Extensions: Caption := cProjectName + ' - Љg’ЈѓXѓLѓѓѓ“';
        FullScanFolders: Caption := cProjectName + ' - ѓXѓLѓѓѓ“‚і‚к‚йѓpѓX‚рЋw’и';
      end;
    end;

    German:
    begin
      btnConfirm.Caption := '&Bestдtigen';
      btnCancel.Caption := '&Abbrechen';

      btnReplace.Caption := '&Ersetzen';
      btnAdd.Caption := '&Hinzufьgen';
      btnDelete.Caption := '&Lцschen';

      sMsgValueAlreadyExists := 'Wert %s ist bereits vorhanden.';
      sMsgNoExtensionsScannedDefined := 'Keine Dateierweiterung zum ьberprьfen definiert.';
      sMsgSelectFolder := 'Wдhle ein Verzeichnis';
      sMsgSelectFile := 'Wдhle eine Datei';

      sbSelectFolder.Hint := 'Wдhle ein Verzeichnis';
      sbSelectFile.Hint := 'Wдhle eine Datei';

      case ListType of
        ScanPaths: Caption := cProjectName + ' - Pfade oder Dateien die nicht ьberprьft werden sollen';
        Extensions: Caption := cProjectName + ' - Dateierweiterungen ьberprьfen';
        FullScanFolders: Caption := cProjectName + ' - Pfad wo alle Dateien sind die ьberprьft werden sollen';
      end;
    end;

    Spanish:
    begin
      btnConfirm.Caption := 'Confirmar';
      btnCancel.Caption := 'Cancelar';

      btnReplace.Caption := 'Reemplazar';
      btnAdd.Caption := 'Aсadir';
      btnDelete.Caption := 'Eliminar';

      sMsgValueAlreadyExists := 'Los valores %s ya existen.';
      sMsgNoExtensionsScannedDefined := 'No hay extensiones definidas.';
      sMsgSelectFolder := 'Seleccione una carpeta';
      sMsgSelectFile := 'Seleccione un archivo';

      sbSelectFolder.Hint := 'Seleccione una carpeta';
      sbSelectFile.Hint := 'Seleccione un archivo';

      case ListType of
        ScanPaths: Caption := cProjectName + ' - Rutas o archivos no escaneados';
        Extensions: Caption := cProjectName + ' - Extensiones a escanear';
        FullScanFolders: Caption := cProjectName + ' - Rutas donde todos los archivos serбn escaneados';
      end;
    end;

    Polish:
    begin
      btnConfirm.Caption := 'Pot&wierd'+#$9F;
      btnCancel.Caption := 'An&uluj';

      btnReplace.Caption := 'Zast'+#$B9+'&p';
      btnAdd.Caption := 'D&odaj';
      btnDelete.Caption := 'Sk&asuj';

      sMsgValueAlreadyExists := 'Warto'+#$9C+#$E6+' %s ju'+#$BF+' istnieje.';
      sMsgNoExtensionsScannedDefined := 'Nie zdefiniowano wyj'+#$B9+'tk'+#$F3+'w.';
      sMsgSelectFolder := 'Wybierz folder';
      sMsgSelectFile := 'wybierz plik';

      sbSelectFolder.Hint := 'Wybierz folder';
      sbSelectFile.Hint := 'wybierz plik';

      case ListType of
        ScanPaths: Caption := cProjectName + ' - lokacje lub pliki nie skanowane (BIA'+#$A3+'A LISTA)';
        Extensions: Caption := cProjectName + ' - wyj'+#$B9+'tki';
        FullScanFolders: Caption := cProjectName + ' - Lokacje plik'+#$F3+'w do skanowania';
      end;
    end;

    Russian:
    begin
      btnConfirm.Caption := '&Ок';
      btnCancel.Caption := 'О&тмена';

      btnReplace.Caption := '&Заменить';
      btnAdd.Caption := '&Добавить';
      btnDelete.Caption := '&Удалить';

      sMsgValueAlreadyExists := 'Значение %s уже существует.';
      sMsgNoExtensionsScannedDefined := 'Не определены расширения для сканирования.';
      sMsgSelectFolder := 'Выберите каталог';
      sMsgSelectFile := 'Выберите файл';

      sbSelectFolder.Hint := 'Выберите каталог';
      sbSelectFile.Hint := 'Выберите файл';

      case ListType of
        ScanPaths: Caption := cProjectName + ' - Путь или Файл не найдены';
        Extensions: Caption := cProjectName + ' - Сканируемые расширения';
        FullScanFolders: Caption := cProjectName + ' - Каталоги где все файлы будут проверяться';
      end;
    end;

    Portuguese:
    begin
      btnConfirm.Caption := '&Confirmar';
      btnCancel.Caption := 'Ca&ncelar';

      btnReplace.Caption := '&Substituir';
      btnAdd.Caption := '&Adicionar';
      btnDelete.Caption := '&Remover';

      sMsgValueAlreadyExists := 'Valor %s jб existe.';
      sMsgNoExtensionsScannedDefined := 'Nenhuma extensгo foi definida.';
      sMsgSelectFolder := 'Selecione uma pasta';
      sMsgSelectFile := 'Selecione um arquivo';

      sbSelectFolder.Hint := 'Selecione uma pasta';
      sbSelectFile.Hint := 'Selecione um arquivo';

      case ListType of
        ScanPaths: Caption := cProjectName + ' - Pastas ou arquivos nгo monitorados';
        Extensions: Caption := cProjectName + ' - Lista de extensхes monitoradas';
        FullScanFolders: Caption := cProjectName + ' - Pastas ou arquivos a serem sempre verificados';
      end;
    end;

    Bulgarian:
    begin
      btnConfirm.Caption := '&Добре';
      btnCancel.Caption := 'От&каз';

      btnReplace.Caption := '&Замяна';
      btnAdd.Caption := '&Добавяне';
      btnDelete.Caption := '&Изтриване';

      sMsgValueAlreadyExists := 'Стойността %s вече съществува.';
      sMsgNoExtensionsScannedDefined := 'No extensions scanned defined.';
      sMsgSelectFolder := 'Избиране на папка';
      sMsgSelectFile := 'Избиране на файл';

      sbSelectFolder.Hint := 'Избиране на папка';
      sbSelectFile.Hint := 'Избиране на файл';

      case ListType of
        ScanPaths: Caption := cProjectName + ' - Пътя или файла не са сканирани';
        Extensions: Caption := cProjectName + ' - Разширено сканиране';
        FullScanFolders: Caption := cProjectName + ' - Път където всички файлове ще бъдат сканирани';
      end;
    end;

    Indonesian:
    begin
      btnConfirm.Caption := '&Konfirmasi';
      btnCancel.Caption := 'Ba&tal';

      btnReplace.Caption := '&Timpa';
      btnAdd.Caption := '&Tambah';
      btnDelete.Caption := '&Hapus';

      sMsgValueAlreadyExists := 'Nilai %s telah ada.';
      sMsgNoExtensionsScannedDefined := 'Tidak ada ekstensi terpantau yang terdefinisi.';
      sMsgSelectFolder := 'Pilih satu folder';
      sMsgSelectFile := 'Pilih satu berkas';

      sbSelectFolder.Hint := 'Pilih satu folder';
      sbSelectFile.Hint := 'Pilih satu berkas';

      case ListType of
        ScanPaths: Caption := cProjectName + ' - Lokasi atau berkas tidak dipindai';
        Extensions: Caption := cProjectName + ' - Ekstensi telah dipindai';
        FullScanFolders: Caption := cProjectName + ' - Lokasi di mana seluruh berkas akan dipindai';
      end;
    end;

    Azeri:
    begin
      btnConfirm.Caption := '&Tekrar';
      btnCancel.Caption := 'Le&gv et';

      btnReplace.Caption := '&Evez edin';
      btnAdd.Caption := '&Daxil edin';
      btnDelete.Caption := '&Silin';

      sMsgValueAlreadyExists := 'Deyer %s artiq movcuddur.';
      sMsgNoExtensionsScannedDefined := 'Gozden kecirilesi genislemeler yoxdur.';
      sMsgSelectFolder := 'Qovlugu secin';
      sMsgSelectFile := 'Fayli secin';

      sbSelectFolder.Hint := 'Qovlugu secin';
      sbSelectFile.Hint := 'Fayli secin';

      case ListType of
        ScanPaths: Caption := cProjectName + ' - Yol veya fayl axtarilmadi';
        Extensions: Caption := cProjectName + ' - Genislenmeler axtarildi';
        FullScanFolders: Caption := cProjectName + ' - Butun fayllarin axtarilacagi yollar';
      end;
    end;

    Dutch:
    begin
      btnConfirm.Caption := '&Bevestigen';
      btnCancel.Caption := '&Annuleren';

      btnReplace.Caption := '&Vervangen';
      btnAdd.Caption := '&Toevoegen';
      btnDelete.Caption := '&Wissen';

      sMsgValueAlreadyExists := 'Waarde %s bestaat al.';
      sMsgNoExtensionsScannedDefined := 'Geen extentiescan gedefinieerd.';
      sMsgSelectFolder := 'Selecteer een folder';
      sMsgSelectFile := 'Selecteer een bestand';

      sbSelectFolder.Hint := 'Selecteer een folder';
      sbSelectFile.Hint := 'Selecteer een bestand';

      case ListType of
        ScanPaths: Caption := cProjectName + ' - Paden of bestanden niet gescand';
        Extensions: Caption := cProjectName + ' - Extensies gescand';
        FullScanFolders: Caption := cProjectName + ' - Paden waar alle bestanden zullen worden gescand';
      end;
    end;

    Galician:
    begin
      btnConfirm.Caption := '&Confirmar';
      btnCancel.Caption := 'Ca&ncelar';

      btnReplace.Caption := '&Reemplazar';
      btnAdd.Caption := '&Engadir';
      btnDelete.Caption := '&Eliminar';

      sMsgValueAlreadyExists := 'O valor %s xa existe.';
      sMsgNoExtensionsScannedDefined := 'Ningunha extensiуn para o anбlise definida.';
      sMsgSelectFolder := 'Selecc un cartafol';
      sMsgSelectFile := 'Seleccione un ficheiro';

      sbSelectFolder.Hint := 'Seleccione un cartafol';
      sbSelectFile.Hint := 'Seleccione un ficheiro';

      case ListType of
        ScanPaths: Caption := cProjectName + ' - Rutas ou ficheiros non analizados';
        Extensions: Caption := cProjectName + ' - Extensiуns a analizar';
        FullScanFolders: Caption := cProjectName + ' - Rutas onde todos os ficheiros son analizados';
      end;
    end;

    Hebrew:
    begin
      btnConfirm.Caption := '&amp;ащш';
      btnCancel.Caption := 'б&amp;им';

      btnReplace.Caption := '&amp;дзму';
      btnAdd.Caption := 'де&amp;су';
      btnDelete.Caption := 'оз&amp;ч';

      sMsgValueAlreadyExists := 'дтшк %s лбш чййн.';
      sMsgNoExtensionsScannedDefined := 'ма девгше сйеоеъ.';
      sMsgSelectFolder := 'бзш ъйчйд';
      sMsgSelectFile := 'бзш чебх';

      sbSelectFolder.Hint := 'бзш ъйчййд';
      sbSelectFile.Hint := 'бзш чебх';

      case ListType of
        ScanPaths: Caption := cProjectName + ' - осмемйн ае чбцйн щма рсшче';
        Extensions: Caption := cProjectName + ' - сйеоеъ щрсшче';
        FullScanFolders: Caption := cProjectName + ' - осмемйн бдн лм дчбцйн ййсшче';
      end;
    end;
  end;
end;
end;

procedure SetLanguage_Utility;
begin
  Case CurLang of
    Italian:
    begin
      sMsgCannotOpenURL := 'Impossibile aprire l''URL %s';
      sMsgUnableInitializeWininet := 'Impossibile inizializzare Wininet';
    end;

    English:
    begin
      sMsgCannotOpenURL := 'Cannot open URL %s';
      sMsgUnableInitializeWininet := 'Unable to initialize Wininet';
    end;

    French:
    begin
      sMsgCannotOpenURL := 'Impossible d''ouvrir l''URL %s';
      sMsgUnableInitializeWininet := 'Impossible d''initialiser Wininet';
    end;

    Japanese:
    begin
      sMsgCannotOpenURL := 'URL %s ‚ЄЉJ‚Ї‚Ь‚№‚с';
      sMsgUnableInitializeWininet := 'Wininet‚рЏ‰Љъ‰»Џo—€‚Ь‚№‚с';
    end;

    German:
    begin
      sMsgCannotOpenURL := 'Kann Internetadresse %s nicht цffnen';
      sMsgUnableInitializeWininet := 'Kann Wininet nicht initialisieren';
    end;

    Spanish:
    begin
      sMsgCannotOpenURL := 'No se puede abrir la URL %s';
      sMsgUnableInitializeWininet := 'Incapaz de inicializar Wininet';
    end;

    Polish:
    begin
      sMsgCannotOpenURL := 'nie mo'+#$BF+'na otworzy'+#$E6+' adresu %s';
      sMsgUnableInitializeWininet := 'Nie mo'+#$BF+'na uruchomi'+#$E6+' Wininet';
    end;

    Russian:
    begin
      sMsgCannotOpenURL := 'Невозможно открыть URL %s';
      sMsgUnableInitializeWininet := 'Невозможно инициализировать Wininet';
    end;

    Portuguese:
    begin
      sMsgCannotOpenURL := 'Nгo й possнvel abrir a URL %s';
      sMsgUnableInitializeWininet := 'Nгo й possнvel inicializar o Wininet';
    end;

    Bulgarian:
    begin
      sMsgCannotOpenURL := 'Не може да отвори URL %s';
      sMsgUnableInitializeWininet := 'Не може да се инициализира Wininet';
    end;

    Indonesian:
    begin
      sMsgCannotOpenURL := 'Tidak dapat membuka URL %s';
      sMsgUnableInitializeWininet := 'Tidak dapat menginisiasi Wininet';
    end;

    Azeri:
    begin
      sMsgCannotOpenURL := 'URL acilmadi %s';
      sMsgUnableInitializeWininet := ' Wininet kalibrini olcmek alinmadi';
    end;

    Dutch:
    begin
      sMsgCannotOpenURL := 'Kan URL ''%s'' niet openen';
      sMsgUnableInitializeWininet := 'Kan Wininet niet initialiseren';
    end;

    Galician:
    begin
      sMsgCannotOpenURL := 'Non se pode cargar URL %s';
      sMsgUnableInitializeWininet := 'Unable to initialize Wininet';
    end;

    Hebrew:
    begin
      sMsgCannotOpenURL := 'ма йлем мфъез лъебъ %s';
      sMsgUnableInitializeWininet := 'ма оцмйз маъзм аъ Wininet';
    end;
  end;
end;

procedure SetLanguage_VxdThds;
begin
  Case CurLang of
    Italian:
    begin
      sMsgErrorLoadingVXDFile := 'Errore nell''apertura del driver VXD';
      sMsgCouldNotAccessDeviceDriver := 'Impossibile aprire la periferica';
    end;

    English:
    begin
      sMsgErrorLoadingVXDFile := 'Error loading VXD file';
      sMsgCouldNotAccessDeviceDriver := 'Couldn''t access device driver';
    end;

    French:
    begin
      sMsgErrorLoadingVXDFile := 'Erreur de chargement du fichier VXD';
      sMsgCouldNotAccessDeviceDriver := 'Impossible d''accйder au disque';
    end;

    Japanese:
    begin
      sMsgErrorLoadingVXDFile := 'Error VXD ѓtѓ@ѓCѓ‹‚рѓЌЃ[ѓhЏo—€‚Ь‚№‚с';
      sMsgCouldNotAccessDeviceDriver := 'ѓfѓoѓCѓXѓhѓ‰ѓCѓo‚ЙѓAѓNѓZѓXЏo—€‚Ь‚№‚с‚Е‚µ‚Ѕ';
    end;

    German:
    begin
      sMsgErrorLoadingVXDFile := 'Fehler beim Laden der VXD Datei';
      sMsgCouldNotAccessDeviceDriver := 'Kann nicht auf Gerдtetreiber zugreifen';
    end;

    Spanish:
    begin
      sMsgErrorLoadingVXDFile := 'Error cargando el archivo VXD';
      sMsgCouldNotAccessDeviceDriver := 'No se puede acceder al controlador de dispositivo';
    end;

    Polish:
    begin
      sMsgErrorLoadingVXDFile := 'B'+#$9F+#$B9+'d w otwarcia sterownik VxD';
      sMsgCouldNotAccessDeviceDriver := 'Couldn''t access device driver';
    end;

    Russian:
    begin
      sMsgErrorLoadingVXDFile := 'Ошибка загрузки VXD-драйвера';
      sMsgCouldNotAccessDeviceDriver := 'Нет доступа к драйверу устройства';
    end;

    Portuguese:
    begin
      sMsgErrorLoadingVXDFile := 'Erro ao carregar o arquivo VXD';
      sMsgCouldNotAccessDeviceDriver := 'Nгo foi possнvel acessar o driver do dispositivo';
    end;

    Bulgarian:
    begin
      sMsgErrorLoadingVXDFile := 'Грешка при зареждане на VXD файл';
      sMsgCouldNotAccessDeviceDriver := 'Няма достъл до устройството';
    end;

    Indonesian:
    begin
      sMsgErrorLoadingVXDFile := 'salah memuat berkas VXD';
      sMsgCouldNotAccessDeviceDriver := 'Tidak dapat mengakses driver perangkat';
    end;

    Azeri:
    begin
      sMsgErrorLoadingVXDFile := 'VXD faylinin yuklenmesinde sehv';
      sMsgCouldNotAccessDeviceDriver := 'Qurgu''surucusune giris elde edilmedi';
    end;

    Dutch:
    begin
      sMsgErrorLoadingVXDFile := 'Fout tijdens laden van VXD bestand';
      sMsgCouldNotAccessDeviceDriver := 'Toegang tot device driver is geblokkeerd';
    end;

    Galician:
    begin
      sMsgErrorLoadingVXDFile := 'Erro cargando o ficheiro VXD';
      sMsgCouldNotAccessDeviceDriver := 'Non se pode'' acceder б unidade';
    end;

    Hebrew:
    begin
      sMsgErrorLoadingVXDFile := 'щвйад битйръ чебх VXD';
      sMsgCouldNotAccessDeviceDriver := 'рлщмд двйщд мордм ддъчрйн';
    end;
  end;
end;

Initialization

CurLang := StartLanguage;

  Case CurLang of
    Italian:
      begin
        sMsgError := 'Errore';
        sMsgConfirm := '&Conferma';
        sMsgCancel := '&Annulla';

        HookResourceString(@SMsgDlgOK, sMsgConfirm);
        HookResourceString(@SMsgDlgCancel, sMsgCancel);
      end;

    English:
      begin
        sMsgError := 'Error';
        sMsgConfirm := '&Confirm';
        sMsgCancel := 'Ca&ncel';
        //HookResourceString(@SMsgDlgOK, sMsgConfirm);
        //HookResourceString(@SMsgDlgCancel, sMsgCancel);
      end;

    French:
      begin
        sMsgError := 'Erreur';
        sMsgConfirm := '&Confirmer';
        sMsgCancel := '&Annuler';
        HookResourceString(@SMsgDlgOK, sMsgConfirm);
        HookResourceString(@SMsgDlgCancel, sMsgCancel);
      end;

    Japanese:
      begin
        sMsgError := 'ѓGѓ‰Ѓ[';
        sMsgConfirm := 'Љm”F';
        sMsgCancel := 'ѓLѓѓѓ“ѓZѓ‹';
        HookResourceString(@SMsgDlgOK, sMsgConfirm);
        HookResourceString(@SMsgDlgCancel, sMsgCancel);
      end;

    German:
    begin
        sMsgError := 'Fehler';
        sMsgConfirm := '&Bestдtigen';
        sMsgCancel := '&Abbrechen';
        HookResourceString(@SMsgDlgOK, sMsgConfirm);
        HookResourceString(@SMsgDlgCancel, sMsgCancel);
    end;

    Spanish:
      begin
        sMsgError := 'Error';
        sMsgConfirm := 'Confirmar';
        sMsgCancel := 'Cancelar';
        HookResourceString(@SMsgDlgOK, sMsgConfirm);
        HookResourceString(@SMsgDlgCancel, sMsgCancel);
      end;

    Polish:
      begin
        sMsgError := 'BLAD';
        sMsgConfirm := 'Pot&wierd'+#$9F;
        sMsgCancel := 'An&uluj';
        HookResourceString(@SMsgDlgOK, sMsgConfirm);
        HookResourceString(@SMsgDlgCancel, sMsgCancel);
      end;

    Russian:
      begin
        sMsgError := 'Ошибка';
        sMsgConfirm := 'П&одтвердить';
        sMsgCancel := 'О&тмена';
        HookResourceString(@SMsgDlgOK, sMsgConfirm);
        HookResourceString(@SMsgDlgCancel, sMsgCancel);
      end;

    Portuguese:
    begin
        sMsgError := 'Erro';
        sMsgConfirm := '&Confirmar';
        sMsgCancel := 'Ca&ncelar';
        HookResourceString(@SMsgDlgOK, sMsgConfirm);
        HookResourceString(@SMsgDlgCancel, sMsgCancel);
    end;

    Bulgarian:
    begin
        sMsgError := 'Грешка';
        sMsgConfirm := '&Добре';
        sMsgCancel := 'От&каз';
        HookResourceString(@SMsgDlgOK, sMsgConfirm);
        HookResourceString(@SMsgDlgCancel, sMsgCancel);
    end;

    Indonesian:
    begin
        sMsgError := 'Salah';
        sMsgConfirm := '&Konfirmasi';
        sMsgCancel := 'Ba&tal';
        HookResourceString(@SMsgDlgOK, sMsgConfirm);
        HookResourceString(@SMsgDlgCancel, sMsgCancel);
    end;

    Azeri:
    begin
        sMsgError := 'Sehv';
        sMsgConfirm := '&Tekrar';
        sMsgCancel := 'Le&gv edin';

        HookResourceString(@SMsgDlgOK, sMsgConfirm);
        HookResourceString(@SMsgDlgCancel, sMsgCancel);
    end;

    Dutch:
    begin
        sMsgError := 'Fout';
        sMsgConfirm := '&Bevestigen';
        sMsgCancel := '&Annuleren';
        
        HookResourceString(@SMsgDlgOK, sMsgConfirm);
        HookResourceString(@SMsgDlgCancel, sMsgCancel);
    end;

    Galician:
    begin
      sMsgError := 'Erro';
      sMsgConfirm := '&Confirmar';
      sMsgCancel := 'Ca&ncelar';
    end;

    Hebrew:
    begin
        sMsgError := 'щвйад';
        sMsgConfirm := '&amp;ащш';
        sMsgCancel := 'б&amp;им';

        HookResourceString(@SMsgDlgOK, sMsgConfirm);
        HookResourceString(@SMsgDlgCancel, sMsgCancel);
    end;
  end;
end.