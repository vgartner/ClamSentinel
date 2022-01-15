unit Languages;

interface

{$R Languages.RES}

uses Classes,Forms,Windows,SysUtils,uRecover;

const
  LANG_GALICIAN = $56;

type TLanguages = (English, Italian, French, German, Spanish, Japanese, Polish, Russian, Portuguese, Bulgarian, Indonesian, Azeri, Dutch, Hebrew, Galician);

const LANG_AZERI = $2c;

const aLanguages: array[TLanguages] of string = ('EN','IT','FR','DE','ES','JP','PL','RU','PT','BG','ID','AZ','NL','HE','GL');
const aDescLanguages: array[TLanguages] of string = ('English','Italian','French','German','Spanish','Japanese','Polish','Russian','Portuguese','Bulgarian','Indonesian','Azeri','Dutch','Hebrew','Galician');

var CurLang : TLanguages;
    sMsgError : string;
    sMsgConfirm : string;
    sMsgCancel : string;
    crHandCursor : integer;

function  StartLanguage : TLanguages;
procedure UpdateFlag(F : TFRecover);

procedure SetLanguage_FRecover(F : TFRecover);

implementation

uses IniFiles,Utility,Consts,Graphics,Controls;

procedure UpdateFlag(F : TFRecover);
var
  MyBtnGlyph: HBitmap;
  MyBtnBmp: TBitmap;
begin
  MyBtnBmp := TBitmap.Create;
  try
    MyBtnGlyph := LoadBitmap(HINSTANCE, PChar(aLanguages[CurLang]));
    MyBtnBmp.Handle := MyBtnGlyph;
    F.sbLang.Glyph := MyBtnBmp;
    F.sbLang.NumGlyphs := 1;
    F.sbLang.Cursor := crHandCursor;
    F.sbLang.Hint := aDescLanguages[CurLang];
    F.sbLang.ShowHint := true;
  finally
    MyBtnBmp.Free;
  end;
end;

procedure HookResourceString(rs: PResStringRec; newStr: string);
var
  oldprotect: DWORD;
begin
  VirtualProtect(rs, SizeOf(rs^), PAGE_EXECUTE_READWRITE, @oldProtect);
  rs^.Identifier := Integer(PChar(newStr));
  VirtualProtect(rs, SizeOf(rs^), oldProtect, @oldProtect);
end;

function StartLanguage : TLanguages;
var
  IDLanguage : word;
  IniFile : TIniFile;
  sLang : string;
  sFile : string;
begin
  Result := English;
  sLang := '';

  sFile := GetIniFilePath(cProjectFileName);

  if CheckFileExists(sFile) then
  begin
    IniFile := TIniFile.Create(sFile);
    try
      sLang := trim(IniFile.ReadString('Params','Language',''));

      if sLang = 'English'    then Result := English;
      if sLang = 'Italian'    then Result := Italian;
      if sLang = 'French'     then Result := French;
      if sLang = 'German'     then Result := German;
      //todo if sLang = 'Spanish'  then Result := Spanish;
      if sLang = 'Japanese'   then Result := Japanese;   //SHIFT-JIS
      if sLang = 'Polish'     then Result := Polish;     //Windows 1250 + Special chars
      if sLang = 'Russian'    then Result := Russian;    //Windows 1251
      if sLang = 'Portuguese' then Result := Portuguese;
      if sLang = 'Bulgarian' then Result := Bulgarian;   //Windows 1251
      if sLang = 'Indonesian' then Result := Indonesian;
      if sLang = 'Azeri' then Result := Azeri;
      if sLang = 'Dutch' then Result := Dutch;
      if sLang = 'Galician' then Result := Galician;
      if sLang = 'Hebrew' then Result := Hebrew;
    finally
      IniFile.Free;
    end;
  end;
  
  if sLang='' then
  begin
    IDLanguage := windows.GetUserDefaultLangID() and $3FF; // low 9-bits
    case IDLanguage of
      LANG_ENGLISH:    Result := English;
      LANG_ITALIAN:    Result := Italian;
      LANG_FRENCH:     Result := French;
      LANG_GERMAN:     Result := German;
      LANG_RUSSIAN:    Result := Russian;
      //todo LANG_SPANISH:  Result := Spanish;
      LANG_JAPANESE:   Result := Japanese;
      LANG_POLISH:     Result := Polish;
      LANG_PORTUGUESE: Result := Portuguese;
      LANG_BULGARIAN:  Result := Bulgarian;
      LANG_INDONESIAN: Result := Indonesian;
      LANG_AZERI: Result := Azeri;
      LANG_DUTCH: Result := Dutch;
      LANG_GALICIAN: Result := Galician;
      //todo LANG_HEBREW: Result := Hebrew;
    end;
  end;
end;

procedure SetLanguage_FRecover(F : TFRecover);
begin

with F do
begin
  Case CurLang of
    Italian:
    begin
      Font.Charset := ANSI_CHARSET;
      ListViewFiles.Font.Charset := ANSI_CHARSET;

      mnuFile.Caption := '&File';
      mnuFile_Openquarantinefolder.Caption := '&Apri la cartella di quarantena...';
      mnuFile_RestoreSelected.Caption := '&Ripristina i file selezionati';
      mnuFile_DeleteSelected.Caption := '&Cancella i file selezionati';
      mnuFile_Exit.Caption := '&Esci';
      mnuFile_Reload.Caption := 'Ri&leggi la cartella di quarantena';

      mnuUtility.Caption := '&Utilitа';
      mnuUtility_SelectAll.Caption := '&Seleziona tutti';
      mnuUtility_UnselectAll.Caption := '&Deseleziona tutti';
      mnuUtility_CheckValidSignatures.Caption := 'Seleziona tutti i file con &firma digitale valida';
      mnuUtility_Log.Caption := '&Visualizza il log';

      sMsgOpeningFolder := 'Apertura cartella %s...';
      sMsgReady := 'Pronto';
      sMsgSelectQuarantineDirectory := 'Seleziona una cartella di quarantena';

      sMsgColumnOriginalLocation := 'Posizione originale';
      sMsgColumnQuarantineFile := 'File in quarantena';
      sMsgColumnQuarantineDate := 'Data di quarantena';

      sMsgRestoringFile := 'Ripristino del file: %s';
      sMsgFileDeleted := 'Cancellato file in quarantena: %s';
      sMsgFileRestored := 'File ripristinato: %s';
      sMsgFileNotRestored := 'File NON ripristinato: %s';
      sMsgAskRestoreFiles := 'Sono stati selezionati %s file.'
                                   +CRLF+'Si vuole effettuarne il ripristino?';
      sMsgAskDeleteFiles := 'Sono stati selezionati %s file.'
                                   +CRLF+'Si vuole cancellarli?';
      sMsgAttention := 'Attenzione';
      sMsgFilesSelectedRestored := 'File: selezionati: %u, ripristinati: %u';

      sbOpenFolder.Hint := 'Apri cartella';
      sbRestore.Hint := 'Ripristina i file selezionati';
      sbDelete.Hint := 'Cancella i file selezionati';
      sbCheckAll.Hint := 'Seleziona tutti';
      sbUncheckAll.Hint := 'Deseleziona tutti';
      sbViewLog.Hint := 'Visualizza il log';
      sbCheckValidSignatures.Hint := 'Seleziona i file con firma digitale valida';
      sbReload.Hint := 'Rileggi la cartella di quarantena';

      lblStopClamSentinel.Caption := 'Fermare ' + cMainProjectName + ' prima di ripristinare i file';
    end;

    English:
    begin
      Font.Charset := ANSI_CHARSET;
      ListViewFiles.Font.Charset := ANSI_CHARSET;

      mnuFile.Caption := '&File';
      mnuFile_Openquarantinefolder.Caption := '&Open Quarantine Folder...';
      mnuFile_RestoreSelected.Caption := '&Restore Selected Files';
      mnuFile_DeleteSelected.Caption := '&Delete Selected Files';
      mnuFile_Exit.Caption := '&Exit';
      mnuFile_Reload.Caption := 'Re&load the quarantine folder';

      mnuUtility.Caption := '&Utility';
      mnuUtility_SelectAll.Caption := '&Select All';
      mnuUtility_UnselectAll.Caption := '&Unselect All';
      mnuUtility_CheckValidSignatures.Caption := 'Select all files with a valid &digital signature';
      mnuUtility_Log.Caption := '&Log';

      sMsgOpeningFolder := 'Opening folder %s...';
      sMsgReady := 'Ready';
      sMsgSelectQuarantineDirectory := 'Select a quarantine directory';

      sMsgColumnOriginalLocation := 'Original location';
      sMsgColumnQuarantineFile := 'Quarantined file';
      sMsgColumnQuarantineDate := 'Quarantine date';

      sMsgRestoringFile := 'Restoring file: %s';
      sMsgFileDeleted := 'Quarantined file deleted: %s';
      sMsgFileRestored := 'File restored: %s';
      sMsgFileNotRestored := 'File NOT restored: %s';
      sMsgAskRestoreFiles := 'Has been selected %s files.'
                                   +CRLF+'Do you want to recover these files?';
      sMsgAskDeleteFiles := 'Has been selected %s files.'
                                   +CRLF+'Do you want to delete these files?';
      sMsgAttention := 'Attention';
      sMsgFilesSelectedRestored := 'Files: selected: %u, restored: %u';

      sbOpenFolder.Hint := 'Open';
      sbRestore.Hint := 'Restore selected files';
      sbDelete.Hint := 'Delete selected files';
      sbCheckAll.Hint := 'Select All';
      sbUncheckAll.Hint := 'Unselect All';
      sbViewLog.Hint := 'View the log';
      sbCheckValidSignatures.Hint := 'Select files with a valid digital signature';
      sbReload.Hint := 'Reload the quarantine folder';
      lblStopClamSentinel.Caption := 'Stop ' + cMainProjectName + ' before to restore the files';
    end;

    French:
    begin
      Font.Charset := ANSI_CHARSET;
      ListViewFiles.Font.Charset := ANSI_CHARSET;

      mnuFile.Caption := '&Fichier';
      mnuFile_Openquarantinefolder.Caption := '&Ouvrir le dossier de quarantaine...';
      mnuFile_RestoreSelected.Caption := '&Restaurer les fichiers Selectionйs';
      mnuFile_DeleteSelected.Caption := '&Supprimer les fichiers sйlectionnйs';
      mnuFile_Exit.Caption := '&Quitter';
      mnuFile_Reload.Caption := 'Re&charger le dossier de quarantaine';

      mnuUtility.Caption := '&Utilitaire';
      mnuUtility_SelectAll.Caption := '&Tout sйlectionner';
      mnuUtility_UnselectAll.Caption := '&Tout dйselectionner';
      mnuUtility_CheckValidSignatures.Caption := 'Sйlectionnez tous les fichiers avec une &signature numйrique valide';
      mnuUtility_Log.Caption := '&Log';

      sMsgOpeningFolder := 'Ouvrir le rйpertoire %s...';
      sMsgReady := 'Prкt';
      sMsgSelectQuarantineDirectory := 'Selectionner le dossier de quarantaine';

      sMsgColumnOriginalLocation := 'Emplacement d''origine';
      sMsgColumnQuarantineFile := 'Fichier en Quarantaine';
      sMsgColumnQuarantineDate := 'Date de la Quarantaine';

      sMsgRestoringFile := 'Restaurer fichier: %s';
      sMsgFileDeleted := 'Fichier mis en quarantaine supprimй: %s';
      sMsgFileRestored := 'Fichier restaurй: %s';
      sMsgFileNotRestored := 'Fichier Non restaurй: %s';
      sMsgAskRestoreFiles := 'A йtй choisi %s fichiers.'
                                   +CRLF+'Souhaitez vous restaurer les fichiers?';
      sMsgAskDeleteFiles := 'A йtй choisi %s fichiers.'
                                   +CRLF+'Souhaitez vous supprimer les fichiers?';
      sMsgAttention := 'Attention';
      sMsgFilesSelectedRestored := 'Fichiers: selectionner: %u, restaurer: %u';

      sbOpenFolder.Hint := 'Ouvrir';
      sbRestore.Hint := 'Restaurer les fichiers selectionйs';
      sbDelete.Hint := 'Supprimer les fichiers sйlectionnйs';
      sbCheckAll.Hint := 'Tout sйlectionner';
      sbUncheckAll.Hint := 'Tout dйselectionner';
      sbViewLog.Hint := 'Voir le log';
      sbCheckValidSignatures.Hint := 'Sйlectionnez les fichiers avec une signature numйrique valide';
      sbReload.Hint := 'Recharger le dossier de quarantaine';
      lblStopClamSentinel.Caption := 'Arrкtez ' + cMainProjectName + ' avant de restaurer les fichiers';
    end;

    Japanese:
    begin
      Font.Charset := SHIFTJIS_CHARSET;
      ListViewFiles.Font.Charset := SHIFTJIS_CHARSET;

      mnuFile.Caption := '&ГtГ@ГCГЛ';
      mnuFile_Openquarantinefolder.Caption := '&КuЧ£ВµВљГtГHГЛГ_ВрКJВ≠...';
      mnuFile_RestoreSelected.Caption := '&ГtГHГЛГ_ВрСIСрВµВƒХЬМ≥ВЈВй';
      mnuFile_DeleteSelected.Caption := '&СIСрВµВљГtГ@ГCГЛВрНнПЬВµВ№ВЈБB';
      mnuFile_Exit.Caption := '&ПIЧєВЈВй';
      mnuFile_Reload.Caption := 'КuЧ£ГtГHГЛГ_ВрГКГНБ[Гh';

      mnuUtility.Caption := '&ГЖБ[ГeГBГКГeГB';
      mnuUtility_SelectAll.Caption :='&ВЈВ„ВƒСIСрВЈВй';
      mnuUtility_UnselectAll.Caption := '&СIСрВрЙрПЬВЈВй';
      mnuUtility_CheckValidSignatures.Caption := 'ЧLМшВ»ГfГWГ^ГЛПРЦЉВ™ХtВҐВƒВҐВйВЈВ„ВƒВћГtГ@ГCГЛВрСIСрВµВ№ВЈБB';
      mnuUtility_Log.Caption := '&ГНГO';

      sMsgOpeningFolder := 'ГtГHГЛГ_ %s ВрКJВҐВƒВҐВ№ВЈ...';
      sMsgReady := 'ПАФхКЃЧє';
      sMsgSelectQuarantineDirectory := 'КuЧ£ВЈВйГfГBГМГNГgГКВрСIСрВЈВй';

      sMsgColumnOriginalLocation := 'ГtГ@ГCГЛВ™В†ВЅВљПкПК';
      sMsgColumnQuarantineFile := 'КuЧ£В≥ВкВљГtГ@ГCГЛ';
      sMsgColumnQuarantineDate := 'КuЧ£ВµВљУъХt';

      sMsgRestoringFile := 'ГtГ@ГCГЛВрХЬМ≥ТЖ: %s';
      sMsgFileDeleted := 'ГtГ@ГCГЛВ™НнПЬВ≥ВкВљБF: %s';
      sMsgFileRestored := 'ГtГ@ГCГЛВ™ХЬМ≥В≥ВкВ№ВµВљ: %s';
      sMsgFileNotRestored := 'ГtГ@ГCГЛВЌХЬМ≥В≥ВкВƒВҐВ№ВєВс: %s';
      sMsgAskRestoreFiles := '%s М¬ВћГtГ@ГCГЛВ™СIСрВ≥ВкВ№ВµВљ'
                                   +CRLF+'В±ВкВзВћГtГ@ГCГЛВрХЬМ≥ВµВ№ВЈВ©БH';
      sMsgAskDeleteFiles := '%s М¬ВћГtГ@ГCГЛВ™СIСрВ≥ВкВ№ВµВљ'
                                   +CRLF+'В±ВкВзВћГtГ@ГCГЛВрНнПЬВµВ№ВЈВ©БH';

      sMsgAttention := 'ТНИ”';
      sMsgFilesSelectedRestored := 'СIСрВ≥ВкВљГtГ@ГCГЛ: %u, ХЬМ≥В≥ВкВљГtГ@ГCГЛ: %u';

      sbOpenFolder.Hint := 'КJВ≠';
      sbRestore.Hint := 'СIСрВµВљГtГ@ГCГЛВрХЬМ≥ВЈВй';
      sbDelete.Hint := 'СIСрВµВљГtГ@ГCГЛВрНнПЬВµВ№ВЈБB';
      sbCheckAll.Hint := 'ВЈВ„ВƒСIСр';
      sbUncheckAll.Hint := 'ВЈВ„ВƒСIСрВµВ»ВҐ';
      sbViewLog.Hint := 'ГНГOВрМ©Вй';

      sbCheckValidSignatures.Hint := 'ЧLМшВ»ГfГWГ^ГЛПРЦЉВ™ХtВҐВƒВҐВйВЈВ„ВƒВћГtГ@ГCГЛВрСIСрВµВ№ВЈБB';
      sbReload.Hint := 'КuЧ£ГtГHГЛГ_ВрГКГНБ[Гh';
      lblStopClamSentinel.Caption := 'ГtГ@ГCГЛВрГКГXГgГAВЈВйСOВ…ВЌБA ' + cMainProjectName + ' ВрТвО~ВЈВйХKЧvВ™В ВиВ№ВЈ';
    end;

    German:
    begin
      Font.Charset := ANSI_CHARSET;
      ListViewFiles.Font.Charset := ANSI_CHARSET;

      mnuFile.Caption := '&Datei';
      mnuFile_Openquarantinefolder.Caption := '&÷ffne Quarantдne Verzeichnis...';
      mnuFile_RestoreSelected.Caption := '&Wiederherstellen der ausgewдhlten Dateien';
      mnuFile_DeleteSelected.Caption := '&Lцschen von ausgewдhlten Dateien';
      mnuFile_Exit.Caption := '&Exit';
      mnuFile_Reload.Caption := 'Laden Sie das &Quarantдnefaltblatt neu';

      mnuUtility.Caption := '&Zubehцr';
      mnuUtility_SelectAll.Caption := '&Alle auswдhlen';
      mnuUtility_UnselectAll.Caption := 'A&lle abwдhlen';
      mnuUtility_CheckValidSignatures.Caption := 'Wдhlen Sie alle Dateien mit einer gьltigen &digitalen Signatur';
      mnuUtility_Log.Caption := '&Log';

      sMsgOpeningFolder := '÷ffne Verzeichnis %s...';
      sMsgReady := 'Bereit';
      sMsgSelectQuarantineDirectory := 'Wдhle ein Quarantдne Verzeichnis';

      sMsgColumnOriginalLocation := 'Original Ort';
      sMsgColumnQuarantineFile := 'Quarantдne Datei';
      sMsgColumnQuarantineDate := 'Quarantдne Datum';

      sMsgRestoringFile := 'Wiederherstelle Datei: %s';
      sMsgFileDeleted := 'Quarantдne-Datei gelцscht: %s';
      sMsgFileRestored := 'Datei wiederhergestellt: %s';
      sMsgFileNotRestored := 'Datei nicht wiederhergestellt: %s';
      sMsgAskRestoreFiles := 'Sind %s ausgewдhlte Dateien.'
                                   +CRLF+'Willst du diese Dateien wiederherstellen?';
      sMsgAskDeleteFiles := 'Sind %s ausgewдhlte Dateien.'
                                   +CRLF+'Willst du diese Dateien zu lцschen?';
      sMsgAttention := 'Achtung';
      sMsgFilesSelectedRestored := 'Dateien: ausgewдhlt: %u, wiederhergestellt: %u';

      sbOpenFolder.Hint := 'Offen';
      sbRestore.Hint := 'Wiederherstelle ausgewдhlte Dateien';
      sbDelete.Hint := 'Lцschen von ausgewдhlten Dateien';
      sbCheckAll.Hint := 'Alle auswдhlen';
      sbUncheckAll.Hint := 'Alle abwдhlen';
      sbViewLog.Hint := 'Zeige die Log';
      sbCheckValidSignatures.Hint := 'Wдhlen Sie die Dateien mit einer gьltigen digitalen Signatur';
      sbReload.Hint := 'Laden Sie das Quarantдnefaltblatt neu';
      lblStopClamSentinel.Caption := 'Stop ' + cMainProjectName + ' vor dem Wiederherstellen von Dateien';
    end;

    Spanish:
    begin
      Font.Charset := ANSI_CHARSET;
      ListViewFiles.Font.Charset := ANSI_CHARSET;
    end;

    Polish:
    begin
      Font.Charset := EASTEUROPE_CHARSET;
      ListViewFiles.Font.Charset := EASTEUROPE_CHARSET;

      mnuFile.Caption := '&Plik';
      mnuFile_Openquarantinefolder.Caption := '&Otw'+#$F3+'rz folder kwarantanny...';
      mnuFile_RestoreSelected.Caption := 'P&rzywr'+#$F3+#$E6+' zaznaczone pliki';
      mnuFile_DeleteSelected.Caption := '&Usu'+#$F1+' zaznaczone pliki';
      mnuFile_Exit.Caption := 'Wyj'+#$9C+'ci&e';
      mnuFile_Reload.Caption := 'Prze'+#$B3+'aduj folderu &kwarantanny';

      mnuUtility.Caption := '&Narz'+#$EA+'dzie';
      mnuUtility_SelectAll.Caption := '&Zaznacz wszystko';
      mnuUtility_UnselectAll.Caption := 'O&dznacz wszystko';
      mnuUtility_CheckValidSignatures.Caption := 'Zaznacz wszystkie pliki z wa'+#$BF+'nym podpisem cyfrowym';
      mnuUtility_Log.Caption := 'Dzienn&ik';

      sMsgOpeningFolder := 'Otwieranie katalogu %s...';
      sMsgReady := 'Gotowy';
      sMsgSelectQuarantineDirectory := 'Wybierz '+#$9C+'cie'+#$BF+'k'+#$EA+' kwaranatanny';

      sMsgColumnOriginalLocation := 'Pierwotna lokalizacja';
      sMsgColumnQuarantineFile := 'Plik w kwarantannie';
      sMsgColumnQuarantineDate := 'Data kwarantanny';

      sMsgRestoringFile := 'Przewracanie pliku: %s';
      sMsgFileDeleted := 'Plik poddany kwarantannie usuni'+#$EA+'te: %s';
      sMsgFileRestored := 'Przywr'+#$F3+'cony plik: %s';
      sMsgFileNotRestored := 'Nie przywr'+#$F3+'cono pliku: %s';
      sMsgAskRestoreFiles := 'Zaznaczono %s plik'+#$F3+'w.'
                                   +CRLF+'Czy chcesz przywr'+#$F3+'ci'+#$E6+' te pliki?';
      sMsgAskDeleteFiles := 'Zaznaczono %s plik'+#$F3+'w.'
                                   +CRLF+'Czy chcesz usun'+#$B9+#$E6+' te pliki?';
      sMsgAttention := 'Uwaga!';
      sMsgFilesSelectedRestored := 'Pliki: zaznaczono: %u, przywr'+#$F3+'cono: %u';

      sbOpenFolder.Hint := 'Otw'+#$F3+'rz';
      sbRestore.Hint := 'Przywr'+#$F3+#$E6+' zaznaczone pliki';
      sbDelete.Hint := 'Usu'+#$F1+' zaznaczone pliki';
      sbCheckAll.Hint := 'Zaznacz wszystko';
      sbUncheckAll.Hint := 'Odznacz wszystko';
      sbViewLog.Hint := 'Obejrzyj dziennik';
      sbCheckValidSignatures.Hint := 'Zaznacz wszystkie pliki z wa'+#$BF+'nym podpisem cyfrowym';
      sbReload.Hint := 'Prze'+#$B3+'aduj folderu kwarantanny';
      lblStopClamSentinel.Caption := 'Musisz przesta'+#$E6+' ' + cMainProjectName + ' przed przywrci'+#$E6+' wybrane pliki';
    end;

    Russian:
    begin
      Font.Charset := RUSSIAN_CHARSET;
      ListViewFiles.Font.Charset := RUSSIAN_CHARSET;

      mnuFile.Caption := '&‘айл';
      mnuFile_Openquarantinefolder.Caption := '&ќткрыть папку карантина...';
      mnuFile_RestoreSelected.Caption := '&¬осстановить выбранные файлы';
      mnuFile_DeleteSelected.Caption := '&”далить выбранные файлы';
      mnuFile_Exit.Caption := '&¬ыход';
      mnuFile_Reload.Caption := 'ѕерезар€дите скоросшиватель &карантина';

      mnuUtility.Caption := '&»нструменты';
      mnuUtility_SelectAll.Caption := '&¬ыбрать все';
      mnuUtility_UnselectAll.Caption := '&—н€ть выделение';
      mnuUtility_CheckValidSignatures.Caption := '¬ыберите все файлы с действительной &цифровой подписи';
      mnuUtility_Log.Caption := '&∆урнал';

      sMsgOpeningFolder := 'ќткрыть папку %s...';
      sMsgReady := '√отов';
      sMsgSelectQuarantineDirectory := '¬ыберите каталог карантина';

      sMsgColumnOriginalLocation := '»сходное размещение';
      sMsgColumnQuarantineFile := '»м€ файла';
      sMsgColumnQuarantineDate := 'ƒата карантина';

      sMsgRestoringFile := '¬осстановление файла: %s';
      sMsgFileDeleted := ' арантин файл удален: %s';
      sMsgFileRestored := '‘айл восстановлен: %s';
      sMsgFileNotRestored := '‘айл не восстановлен: %s';
      sMsgAskRestoreFiles := 'Ѕыло выделено %s файлов.'
                                   +CRLF+'¬ы желаете их восстановить?';
      sMsgAskDeleteFiles := 'Ѕыло выделено %s файлов.'
                                   +CRLF+'¬ы хотите удалить эти файлы?';
      sMsgAttention := '¬нимание';
      sMsgFilesSelectedRestored := '‘айлы: ¬ыбрано: %u, восстановлено: %u';

      sbOpenFolder.Hint := 'ќткрыть';
      sbRestore.Hint := '¬осстановить выделенные файлы';
      sbDelete.Hint := '”далить выбранные файлы';
      sbCheckAll.Hint := '¬ыделить все';
      sbUncheckAll.Hint := '—н€ть выделение';
      sbViewLog.Hint := 'ѕосмотреть журнал';
      sbCheckValidSignatures.Hint := '¬ыберите файлы с действительной цифровой подписи';
      sbReload.Hint := 'ѕерезар€дите скоросшиватель карантина';
      lblStopClamSentinel.Caption := 'ќстановить программу ' + cMainProjectName + ', прежде чем восстановить файлы';
    end;

    Portuguese:
    begin
      mnuFile.Caption := '&Arquivo';
      mnuFile_Openquarantinefolder.Caption := 'A&brir pasta quarentena...';
      mnuFile_RestoreSelected.Caption := '&Restaurar arquivos selecionados';
      mnuFile_DeleteSelected.Caption := '&Exclui arquivos selecionados';
      mnuFile_Exit.Caption := '&Sair';
      mnuFile_Reload.Caption := 'Recarregue o dobrador da &quarentena';

      mnuUtility.Caption := '&Ferramentas';
      mnuUtility_SelectAll.Caption := '&Marcar Todos';
      mnuUtility_UnselectAll.Caption := '&Desmarcar Todos';
      mnuUtility_CheckValidSignatures.Caption := 'Marque todos os arquivos com uma &assinatura digital vбlida';
      mnuUtility_Log.Caption := '&Log';

      sMsgOpeningFolder := 'Abrindo pasta %s...';
      sMsgReady := 'Pronto';
      sMsgSelectQuarantineDirectory := 'Selecione o diretуrio da quarentena';

      sMsgColumnOriginalLocation := 'Localizaзгo original';
      sMsgColumnQuarantineFile := 'Arquivo em quarentena';
      sMsgColumnQuarantineDate := 'Data da remoзгo';

      sMsgRestoringFile := 'Restaurando arquivo: %s';
      sMsgFileDeleted := 'Arquivo em quarentena excluнdo: %s';
      sMsgFileRestored := 'Arquivo restaurado: %s';
      sMsgFileNotRestored := 'Arquivo N√O restaurado: %s';
      sMsgAskRestoreFiles := 'Foi selecinado %s arquivos.'
                                   +CRLF+'Vocк quer recuperar esses arquivos?';
      sMsgAskDeleteFiles := 'Foi selecinado %s arquivos.'
                                   +CRLF+'Vocк quer exclui esses arquivos?'; 

      sMsgAttention := 'Atenзгo';
      sMsgFilesSelectedRestored := 'Arquivos: selecionado: %u, restaurado: %u';

      sbOpenFolder.Hint := 'Abrir';
      sbRestore.Hint := 'Restaurar arquivos selecionados';
      sbDelete.Hint := 'Exclui os arquivos selecionados';
      sbCheckAll.Hint := 'Marcar Todos';
      sbUncheckAll.Hint := 'Desmarcar Todos';
      sbViewLog.Hint := 'Ver arquivo de log';
      sbCheckValidSignatures.Hint := 'Marque os arquivos com uma assinatura digital vбlida';
      sbReload.Hint := 'Recarregue o dobrador da quarentena';
      lblStopClamSentinel.Caption := 'Pare ' + cMainProjectName + ' antes para restaurar os arquivos';
    end;

    Bulgarian:
    begin
      mnuFile.Caption := '&‘айл';
      mnuFile_Openquarantinefolder.Caption := '&ќтвар€не на карантинната папкаЕ';
      mnuFile_RestoreSelected.Caption := '&¬ъзстанов€ване на избрани€ файл';
      mnuFile_DeleteSelected.Caption := '&»зтриване на избраните файлове';
      mnuFile_Exit.Caption := '&»зход';
      mnuFile_Reload.Caption := 'ѕрезареди папката за &карантина';

      mnuUtility.Caption := '&Utility';
      mnuUtility_SelectAll.Caption := '&ћаркиране на всичко';
      mnuUtility_UnselectAll.Caption := '&ќтмаркирване на всичко';
      mnuUtility_CheckValidSignatures.Caption := '»зберете всички файлове, с валиден цифров подпис';
      mnuUtility_Log.Caption := '&∆урнал';

      sMsgOpeningFolder := 'ќтвар€не на папка %sЕ';
      sMsgReady := 'Ready';
      sMsgSelectQuarantineDirectory := '»збиране на карантинна директори€';

      sMsgColumnOriginalLocation := 'ѕървоначален път';
      sMsgColumnQuarantineFile := 'ѕоставени под карантина файлове';
      sMsgColumnQuarantineDate := ' арантина дата';

      sMsgRestoringFile := '¬ъзстанов€ване на файл: %s';
      sMsgFileDeleted := '‘айл в карантината се заличава: %s';
      sMsgFileRestored := '‘айла е възстановен: %s';
      sMsgFileNotRestored := '‘айла Ќ≈ е възстановен: %s';
      sMsgAskRestoreFiles := 'Ѕеше избран %s файл.'
                                   +CRLF+'»скате ли да се поправ€т тези файлове?';
      sMsgAskDeleteFiles := 'Ѕеше избран %s файл.'
                                   +CRLF+'»скате ли да изтриете тези файлове?';
                                   
      sMsgAttention := '¬нимание';
      sMsgFilesSelectedRestored := '‘айл: избран: %и, възстановен: %u';

      sbOpenFolder.Hint := 'ќтвар€не';
      sbRestore.Hint := '¬ъзстанов€ване на избрани€ файл';
      sbDelete.Hint := '»зтриване на избраните файлове';
      sbCheckAll.Hint := 'ћаркиране на всички';
      sbUncheckAll.Hint := 'ќтмаркирване на всички';
      sbViewLog.Hint := 'ѕреглед на журнала';
      sbCheckValidSignatures.Hint := '»зберете всички файлове, с валиден цифров подпис';
      sbReload.Hint := 'ѕрезареди папката за карантина';
      lblStopClamSentinel.Caption := 'ћол€ те, спри ' + cMainProjectName + ', преди да възстановите файловете';
    end;

    Indonesian:
    begin
      mnuFile.Caption := '&Berkas';
      mnuFile_Openquarantinefolder.Caption := '&Buka Folder Karantina...';
      mnuFile_RestoreSelected.Caption := '&Kembalikan Berkas Terpilih';
      mnuFile_DeleteSelected.Caption := '&Hapus File yang Dipilih';
      mnuFile_Exit.Caption := '&Keluar';
      mnuFile_Reload.Caption := '&Muat lagi folder karantina';

      mnuUtility.Caption := '&Utilitas';
      mnuUtility_SelectAll.Caption := '&Pilih Semua';
      mnuUtility_UnselectAll.Caption := '&Tidak Pilih Semua';
      mnuUtility_CheckValidSignatures.Caption := 'Pilih semua berkas yang memiliki tandatangan &digital valid';
      mnuUtility_Log.Caption := '&Log';

      sMsgOpeningFolder := 'Membuka folder %s...';
      sMsgReady := 'Siap';
      sMsgSelectQuarantineDirectory := 'Pilih satu direktori karantina';

      sMsgColumnOriginalLocation := 'Lokasi asli';
      sMsgColumnQuarantineFile := 'Berkas terkarantina';
      sMsgColumnQuarantineDate := 'Tanggal karantina';

      sMsgRestoringFile := 'Mengembalikan berkas: %s';
      sMsgFileDeleted := 'Sebuah file di karantina dihapus: %s';
      sMsgFileRestored := 'Berkas yang telah dikembalikan: %s';
      sMsgFileNotRestored := 'Berkas TIDAK dikembalikan: %s';
      sMsgAskRestoreFiles := 'Telah terpilih %s berkas.'
                                   +CRLF+'Anda ingin mengembalikan berkas ini?';
      sMsgAskDeleteFiles := 'Telah terpilih %s berkas.'
                                   +CRLF+'Apakah Anda ingin menghapus file ini?';

      sMsgAttention := 'Peringatan';
      sMsgFilesSelectedRestored := 'Berkas: terpilih: %u, dikembalikan: %u';

      sbOpenFolder.Hint := 'Buka';
      sbRestore.Hint := 'Mengembalikan berkas terpilih';
      sbDelete.Hint := 'Hapus File yang Dipilih';
      sbCheckAll.Hint := 'Pilih Semua';
      sbUncheckAll.Hint := 'Tidak Pilih Semua';
      sbViewLog.Hint := 'Lihat log';
      sbCheckValidSignatures.Hint := 'Pilih berkas yang memiliki tandatangan digital valid';
      lblStopClamSentinel.Caption := 'Hentikan ' + cMainProjectName + ' sebelum mengembalikan berkas';
      sbReload.Hint := 'Muat lagi folder karantina';
    end;

    Azeri:
    begin
      mnuFile.Caption := '&Fayl';
      mnuFile_Openquarantinefolder.Caption := '&Karantin qovlugunu ac...';
      mnuFile_RestoreSelected.Caption := '&Secilen fayli geri qaytar';
      mnuFile_DeleteSelected.Caption := '&Secilen fayli sil';
      mnuFile_Exit.Caption := '&Cixis';
      mnuFile_Reload.Caption := 'Ka&rantin qovlugunu yeniden yukleyin';

      mnuUtility.Caption := '&Utilit';
      mnuUtility_SelectAll.Caption := '&Hamisini sec';
      mnuUtility_UnselectAll.Caption := '&Hecbirini secme';
      mnuUtility_CheckValidSignatures.Caption := 'Heqiqi imzali &butun fayllari secin';
      mnuUtility_Log.Caption := '&Log';

      sMsgOpeningFolder := 'Qovlugu acmaq %s...';
      sMsgReady := 'Hazir';
      sMsgSelectQuarantineDirectory := 'Karantin kataloqunu secin';

      sMsgColumnOriginalLocation := 'Orjinal yer';
      sMsgColumnQuarantineFile := 'Karantin olunmus fayl';
      sMsgColumnQuarantineDate := 'Karantin vaxti';

      sMsgRestoringFile := 'Fayli berpa etmek: %s';
      sMsgFileDeleted := 'Karantin olunmus fayl silindi: %s';
      sMsgFileRestored := 'Fayl berpa edildi: %s';
      sMsgFileNotRestored := 'Fayl berpa edilmedi: %s';
      sMsgAskRestoreFiles := 'Fayllar %s secilmishdir.'
                                   +CRLF+'Fayllari geri qaytarmaq isteyirsiniz?';
      sMsgAskDeleteFiles := 'Fayllar %s secilmisdir.'
                                   +CRLF+'Bu fayllar silmek isteyirsiniz?';
      sMsgAttention := 'DIQQET';
      sMsgFilesSelectedRestored := 'Fayllar: selected: %u, restored: %u';

      sbOpenFolder.Hint := 'Ac';
      sbRestore.Hint := 'Secilmis fayli berpa et';
      sbDelete.Hint := 'Secilmis fayllari sil';
      sbCheckAll.Hint := 'Hamisini sec';
      sbUncheckAll.Hint := 'Hec birini secme';
      sbViewLog.Hint := 'Loglara bax';
      sbCheckValidSignatures.Hint := 'Heqiqi imzali butun fayllari secin';
      sbReload.Hint := 'Karantin qovlugunu yeniden yukleyin';
      lblStopClamSentinel.Caption := 'Dayan ' + cMainProjectName + ' evvel fayllari berpa etmek ucun';
    end;

    Dutch:
    begin
      FRecover.Caption := cMainProjectName + ' - Herstellen';

      mnuFile.Caption := '&Bestand';
      mnuFile_Openquarantinefolder.Caption := '&Open quarantaine folder...';
      mnuFile_RestoreSelected.Caption := 'Geselecteerde bestanden &herstellen';
      mnuFile_DeleteSelected.Caption := 'Geselecteerde bestanden &wissen';
      mnuFile_Exit.Caption := '&Afsluiten';
      mnuFile_Reload.Caption := 'Quarantaine folder herstellen';

      mnuUtility.Caption := '&Extra';
      mnuUtility_SelectAll.Caption := '&Alles selecteren';
      mnuUtility_UnselectAll.Caption := 'Selectie &opheffen';
      mnuUtility_CheckValidSignatures.Caption := 'Selecteer alle bestanden met een geldige &digitale ondertekening';
      mnuUtility_Log.Caption := '&Logboek';

      sMsgOpeningFolder := 'Folder %s openen...';
      sMsgReady := 'Gereed';
      sMsgSelectQuarantineDirectory := 'Selecteer een quarantaine directory';

      sMsgColumnOriginalLocation := 'Originele locatie';
      sMsgColumnQuarantineFile := 'Bestand in quarantaine';
      sMsgColumnQuarantineDate := 'Quarantaine datum';

      sMsgRestoringFile := 'Bestand herstellen: %s';
      sMsgFileDeleted := 'Quarantaine bestand verwijderd: %s';
      sMsgFileRestored := 'Bestand hersteld: %s';
      sMsgFileNotRestored := 'Bestand NIET hersteld: %s';
      sMsgAskRestoreFiles := '%s bestanden geselecteerd.'
                                   +CRLF+'Wilt u deze bestanden herstellen?';
      sMsgAskDeleteFiles := '%s bestanden geselecteerd.'
                                   +CRLF+'Wilt u deze bestanden verwijderen?';
      sMsgAttention := 'Attentie';
      sMsgFilesSelectedRestored := 'Bestanden: geselecteerd: %u, hersteld: %u';

      sbOpenFolder.Hint := 'Open';
      sbRestore.Hint := 'Geselecteerde bestanden herstellen';
      sbDelete.Hint := 'Geselecteerde bestanden verwijderen';
      sbCheckAll.Hint := 'Alles selecteren';
      sbUncheckAll.Hint := 'Selectie opheffen';
      sbViewLog.Hint := 'Logboek inzien';
      sbCheckValidSignatures.Hint := 'Selecteer bestanden met een geldige digitale ondertekening';
      sbReload.Hint := 'Quarantine folder herstellen';
      lblStopClamSentinel.Caption := 'Eerst ' + cMainProjectName + ' stoppen, om het herstellen van bestanden te kunnen starten';
    end;

    Galician:
    begin
      mnuFile.Caption := '&Ficheiro';
      mnuFile_Openquarantinefolder.Caption := '&Abrir cartafol de cuarentena...';
      mnuFile_RestoreSelected.Caption := '&Restaurar ficheiros seleccionados';
      mnuFile_DeleteSelected.Caption := '&Eliminar o ficheiro seleccionado';
      mnuFile_Exit.Caption := '&Saнr';
      mnuFile_Reload.Caption := 'Re&cargar o cartafol de cuarentena';

      mnuUtility.Caption := '&Utilidade';
      mnuUtility_SelectAll.Caption := '&Selecc Todo';
      mnuUtility_UnselectAll.Caption := '&Des-Selecc Todo';
      mnuUtility_CheckValidSignatures.Caption := 'Seleccionar todos os ficheiros con unha sinatura &dixital vбlida';
      mnuUtility_Log.Caption := '&Rexistro';

      sMsgOpeningFolder := 'Abrindo cartafol %s...';
      sMsgReady := 'Listo';
      sMsgSelectQuarantineDirectory := 'Selecc o cartafol de cuarantena';

      sMsgColumnOriginalLocation := 'Localizaciуn Orixinal';
      sMsgColumnQuarantineFile := 'Ficheiro cuarentenado';
      sMsgColumnQuarantineDate := 'Data de cuarentenado';

      sMsgRestoringFile := 'Restaurando ficheiro: %s';
      sMsgFileDeleted := 'Ficheiro cuarentenado eliminado: %s';
      sMsgFileRestored := 'Ficheiro restaurado: %s';
      sMsgFileNotRestored := 'Ficheiro NON restaurado: %s';
      sMsgAskRestoreFiles := 'Foi seleccionado o ficheiro %s.'
                                   +CRLF+'Desexa recuperar este ficheiros?';
      sMsgAskDeleteFiles := 'Foron seleccionados todos os ficheiros %s.'
                                   +CRLF+'Desexa eliminalos?';
      sMsgAttention := 'Atenciуn!';
      sMsgFilesSelectedRestored := 'Ficheiros: seleccionados: %u, restaurados: %u';

      sbOpenFolder.Hint := 'Abrir';
      sbRestore.Hint := 'Restaurar ficheiros seleccionados';
      sbDelete.Hint := 'Eliminar ficheiros seleccionados';
      sbCheckAll.Hint := 'Seleccionar Todo';
      sbUncheckAll.Hint := 'De-seleccionar todo';
      sbViewLog.Hint := 'Ver o rexistro';
      sbCheckValidSignatures.Hint := 'Seleccionar ficheiros con unha sinatura vбlida';
      sbReload.Hint := 'Recargar cartafol de cuarentena';
      lblStopClamSentinel.Caption := 'Desactivar ' + cMainProjectName + ' antes de restaurar os ficheiros';
    end;

    Hebrew:
    begin
      mnuFile.Caption := '&amp;чебх';
      mnuFile_Openquarantinefolder.Caption := '&amp;фъз ъйчййъ дсвш...';
      mnuFile_RestoreSelected.Caption := 'дз&amp;жш чбцйн рбзшйн';
      mnuFile_DeleteSelected.Caption := 'оз&amp;ч чбцйн рбзшйн';
      mnuFile_Exit.Caption := 'й&amp;цйад';
      mnuFile_Reload.Caption := 'итп озг&amp;щ ъйчййъ дсвш';

      mnuUtility.Caption := '&amp;лмйн';
      mnuUtility_SelectAll.Caption := '&amp;бзш блм';
      mnuUtility_UnselectAll.Caption := '&amp;рчд длм';
      mnuUtility_CheckValidSignatures.Caption := 'бзш дчбцйн тн з&amp;ъйод ъчйрд';
      mnuUtility_Log.Caption := '&amp;йеоп';

      sMsgOpeningFolder := 'феъз ъйчййд %s...';
      sMsgReady := 'оелп';
      sMsgSelectQuarantineDirectory := 'бзш ойчен ъйчййъ дсвш';

      sMsgColumnOriginalLocation := 'ойчен очешй';
      sMsgColumnQuarantineFile := 'чебх бдсвш';
      sMsgColumnQuarantineDate := 'ъашйк дсвш';

      sMsgRestoringFile := 'ощйб чебх: %s';
      sMsgFileDeleted := 'чебх бдсвш розч: %s';
      sMsgFileRestored := 'чебх дещб: %s';
      sMsgFileNotRestored := 'чебх _ма_ дещб: %s';
      sMsgAskRestoreFiles := 'рбзше %s чбцйн.'
                                   +CRLF+'дан мдщйб чбцйн аме?';
      sMsgAskDeleteFiles := 'рбзше %s чбцйн.'
                                   +CRLF+'дан мозеч чбцйн аме?';
      sMsgAttention := 'щйое мб';
      sMsgFilesSelectedRestored := 'чбцйн: рбзше: %u, дещбе: %u';

      sbOpenFolder.Hint := 'фъз';
      sbRestore.Hint := 'дщб чбцйн рбзшйн';
      sbDelete.Hint := 'озч чбцйн рбзшйн';
      sbCheckAll.Hint := 'бзш длм';
      sbUncheckAll.Hint := 'рчд длм';
      sbViewLog.Hint := 'шад йеоп';
      sbCheckValidSignatures.Hint := 'бзш чбцйн тн зъйод ъчйрд';
      sbReload.Hint := 'итп озгщ аъ ъйчййъ ддсвш';
      lblStopClamSentinel.Caption := 'тцеш аъ ' + cMainProjectName + ' чегн, лгй мдщйб аъ дчбцйн';
    end;
  end;

  sbOpenFolder.ShowHint := true;
  sbDelete.ShowHint := true;
  sbReload.ShowHint := true;
  sbRestore.ShowHint := true;
  sbCheckAll.ShowHint := true;
  sbUncheckAll.ShowHint := true;
  sbCheckValidSignatures.ShowHint := true;
  sbViewLog.ShowHint := true;
end;
UpdateFlag(F);
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
        sMsgError := 'ГGГЙБ[';
        sMsgConfirm := 'КmФF';
        sMsgCancel := 'ГLГГГУГZГЛ';
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
        sMsgConfirm := 'Pot&wierdz';
        sMsgCancel := 'An&uluj';
        HookResourceString(@SMsgDlgOK, sMsgConfirm);
        HookResourceString(@SMsgDlgCancel, sMsgCancel);
      end;

    Russian:
      begin
        sMsgError := 'ќшибка';
        sMsgConfirm := 'ѕ&одтвердить';
        sMsgCancel := 'ќ&тмена';
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
        sMsgError := '√решка';
        sMsgConfirm := '&ѕотвърждаване';
        sMsgCancel := 'ќт&каз';
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
  end;

  crHandCursor := 5;
  Screen.Cursors[crHandCursor] := LoadCursor(HInstance, 'Hand');
end.
