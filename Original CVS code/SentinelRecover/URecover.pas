unit URecover;

{$I DelphiVersion.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ComCtrls, ToolWin, Buttons, ExtCtrls, ImgList, StdCtrls, CheckLst;

const
  cProjectFileName = 'SentinelRecover';
  cProjectName = 'Sentinel Recover';

  cMainProjectFileName = 'ClamSentinel';
  cMainProjectName = 'Clam Sentinel';

  LOGFILE_RESTORE =  cProjectFileName+'_RecoverLog.txt';

type
  TFRecover = class(TForm)
    MainMenu: TMainMenu;
    PanelTop: TPanel;
    sbRestore: TSpeedButton;
    sbOpenFolder: TSpeedButton;
    mnuFile: TMenuItem;
    mnuFile_Exit: TMenuItem;
    mnuSep1: TMenuItem;
    mnuFile_Openquarantinefolder: TMenuItem;
    mnuSep2: TMenuItem;
    mnuFile_RestoreSelected: TMenuItem;
    mnuUtility: TMenuItem;
    mnuUtility_SelectAll: TMenuItem;
    mnuUtility_UnselectAll: TMenuItem;
    sbUncheckAll: TSpeedButton;
    sbCheckAll: TSpeedButton;
    ListViewFiles: TListView;
    StatusBar: TStatusBar;
    ProgressBar: TProgressBar;
    sbViewLog: TSpeedButton;
    sbLang: TSpeedButton;
    mnuSep3: TMenuItem;
    mnuUtility_Log: TMenuItem;
    sbCheckValidSignatures: TSpeedButton;
    mnuUtility_CheckValidSignatures: TMenuItem;
    lblStopClamSentinel: TLabel;
    sbReload: TSpeedButton;
    mnuFile_Reload: TMenuItem;
    sbDelete: TSpeedButton;
    mnuFile_DeleteSelected: TMenuItem;
    N1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure mnuFile_ExitClick(Sender: TObject);
    procedure sbCheckAllClick(Sender: TObject);
    procedure sbUncheckAllClick(Sender: TObject);
    procedure sbOpenFolderClick(Sender: TObject);
    procedure mnuFile_OpenquarantinefolderClick(Sender: TObject);
    procedure sbRestoreClick(Sender: TObject);
    procedure mnuFile_RestoreSelectedClick(Sender: TObject);
    procedure mnuUtility_UnselectAllClick(Sender: TObject);
    procedure mnuUtility_SelectAllClick(Sender: TObject);
    procedure sbViewLogClick(Sender: TObject);
    procedure mnuUtility_LogClick(Sender: TObject);
    procedure sbLangClick(Sender: TObject);
    procedure ListViewFilesColumnClick(Sender: TObject;
      Column: TListColumn);
    procedure ListViewFilesCompare(Sender: TObject; Item1,
      Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure sbCheckValidSignaturesClick(Sender: TObject);
    procedure mnuUtility_CheckValidSignaturesClick(Sender: TObject);
    procedure sbReloadClick(Sender: TObject);
    procedure mnuFile_ReloadClick(Sender: TObject);
    procedure mnuFile_DeleteSelectedClick(Sender: TObject);
    procedure sbDeleteClick(Sender: TObject);
  private
    { Private declarations }
    sPathQuarantine : string;
    sCurrentDir : string;
    sPathLog : string;

    procedure GetQuarantineFolder;
    procedure OpenQuarantineFolder;
    procedure RestoreSelectedFiles;
    procedure DeleteSelectedFiles;
    function  RestoreFile(const sFileFrom : string; const sFileTo : string): boolean;
    procedure DeleteQuarantineFile(const sFileFrom : string);
    procedure SelectUnSelectAll(const bSelect : boolean);
    procedure Status(const s : string);
    procedure GetFileInfo(const sDir : string; const SR : TSearchRec; var Items : TListItems);
    procedure ListFileDir(Path: string; FileList: TListItems);
    procedure SelectFilesWithGoodSignature;
    procedure ReloadQuarantineFolder;
  public
    { Public declarations }
     sMsgSelectQuarantineDirectory : string;
     sMsgOpeningFolder : string;
     sMsgReady : string;
     sMsgColumnOriginalLocation : string;
     sMsgColumnQuarantineFile : string;
     sMsgColumnQuarantineDate : string;
     sMsgRestoringFile : string;
     sMsgFileRestored : string;
     sMsgFileDeleted : string;
     sMsgFileNotRestored : string;
     sMsgAskRestoreFiles : string;
     sMsgAskDeleteFiles : string;
     sMsgAttention : PChar;
     sMsgFilesSelectedRestored : string;
  end;

var
  FRecover: TFRecover;

implementation

uses FileCtrl, IniFiles, Utility, ShlObj, Languages, Sign;

{$R *.DFM}

procedure TFRecover.Status(const s : string);
begin
  Statusbar.Panels[1].Text := s;
end;

procedure TFRecover.GetFileInfo(const sDir : string; const SR : TSearchRec; var Items : TListItems);
 var F : TextFile;
     sLine : string;
     ListItem: TListItem;
     iPos : integer;
     sOriginal : string;
     sQuarantined : string;
     sFile : string;
begin
    sFile := SR.Name;

    AssignFile(F, sDir + sFile);
    try
      Reset(F);
      Readln(F, sLine);
    finally
      CloseFile(F);
    end;

    iPos := Pos(#9,sLine);

    if iPos>0 then
    begin
      sOriginal := Copy(sLine,1,iPos-1);
      sQuarantined := Copy(sLine,iPos+1,MAXINT);

      sOriginal := StringReplace(sOriginal,'\\?\','',[rfReplaceAll, rfIgnoreCase]);

      sQuarantined := StringReplace(sQuarantined,'\\?\','',[rfReplaceAll, rfIgnoreCase]);
      sQuarantined := StringReplace(sQuarantined, sDir,'',[rfReplaceAll, rfIgnoreCase]);

      ListItem := Items.Add;
      ListItem.Caption := sOriginal;
      ListItem.SubItems.Add(sQuarantined);
      ListItem.SubItems.Add(FormatDateTime('dddddd tt', FileDateToDateTime(SR.Time)));
    end;
end;

procedure TFRecover.ListFileDir(Path: string; FileList: TListItems);
var
  SR: TSearchRec;
  sDir : string;
  sMsg : string;
begin
  sMsg := Format(sMsgOpeningFolder,[Path]);
  Status(sMsg);

  FileList.Clear;
  sDir := AddSlash(Path);
  if FindFirst(sDir + '*.txt', faAnyFile, SR) = 0 then
  begin
    repeat
      if (SR.Attr <> faDirectory) then
      begin
        GetFileInfo(sDir,SR,FileList);
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
  ListViewFiles.AlphaSort;

  Status(sMsgReady);
end;

procedure TFRecover.GetQuarantineFolder;
  var
    sIniFile : string;
    sFile : string;
    sPathClamWinConf : string;
    IniFile : TIniFile;
    IniFileClamWinConfig : TIniFile;
    WinVer: TWindowsVersion;
    NS : integer;
begin
 sPathQuarantine := '';
 sIniFile := GetIniFilePath(cMainProjectFileName);
 
 if CheckFileExists(sIniFile) then
   begin
     IniFile  := TIniFile.Create(sIniFile);

     try
       sPathClamWinConf := AddSlash(ExpandEnvironment(
                              IniFile.ReadString('Params','PathClamWin',' ')));

       sPathLog :=
          AddSlash(ExpandEnvironment(IniFile.ReadString('Params', 'PathLog',  ' ')));

       NS := IniFile.ReadInteger('Params', 'NS', 0);
     finally
       IniFile.Free;
     end;

     if RemoveSlash(sPathLog) = '' then
       sPathLog := ExtractFilePath(sIniFile);

     if not(NS=2) then
       begin
         sFile := sPathClamWinConf + SCANNER_CONF_FILE;

         if CheckFileExists(sFile) then
         begin
           IniFileClamWinConfig  := TIniFile.Create(sFile);

           try
             sPathQuarantine :=
                ExpandEnvironment(IniFileClamWinConfig.ReadString('ClamAV', 'quarantinedir',  ' '));

             if trim(RemoveSlash(sPathLog))='' then
               sPathLog :=
                   AddSlash(ExpandEnvironment(ExtractFilePath(IniFileClamWinConfig.ReadString('ClamAV','logfile',' '))))
             else
               sPathLog := AddSlash(sPathLog);

           finally
             IniFileClamWinConfig.Free;
           end;
         end;
       end
     else
       begin
         sPathQuarantine := ExtractFilePath(sIniFile) + 'quarantine';
         sPathLog := AddSlash(sPathLog);
       end;
   end
 else
   begin
     if IsClamWinPortable then
       sPathQuarantine := LOCAL_PATH_CLAMWIN_PORTABLE
     else
     begin
       WinVer := GetWindowsVersion;

       case WinVer of
          cOSWin95, cOSWin98, cOSWin98SE, cOSWinME, cOSGenericWin9x:
            sPathQuarantine := AddSlash(GetSystemPath('AppData')) + '.clamwin\';

          cOSWinNT, cOSWin2k, cOSWinXP, cOSWin2k3, cOSVista, cOSSeven, cOSGenericWinNT:
            sPathQuarantine := AddSlash(ExpandEnvironment('%APPDATA%\.clamwin\'));
       end;
     end;
   end;
end;

procedure TFRecover.FormCreate(Sender: TObject);
  var
    NewColumn: TListColumn;
begin
  StatusBar.Panels[0].Width :=0;

  SetLanguage_FRecover(FRecover);
  
  NewColumn := ListViewFiles.Columns.Add;
  NewColumn.Caption := sMsgColumnOriginalLocation;
  NewColumn.AutoSize := true;

  NewColumn := ListViewFiles.Columns.Add;
  NewColumn.Caption := sMsgColumnQuarantineFile;
  NewColumn.AutoSize := true;

  NewColumn := ListViewFiles.Columns.Add;
  NewColumn.Caption := sMsgColumnQuarantineDate;
  NewColumn.AutoSize := true;

  GetQuarantineFolder;

  if sPathQuarantine <> '' then
  begin
    sCurrentDir := sPathQuarantine;
    ReloadQuarantineFolder;
  end;

  Status(sMsgReady);
end;

procedure TFRecover.mnuFile_ExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFRecover.sbCheckAllClick(Sender: TObject);
begin
  SelectUnSelectAll(true);
end;

procedure TFRecover.SelectUnSelectAll(const bSelect : boolean);
var
 i : integer;
begin
 Screen.cursor := crHourGlass;
 try
   for i := 0 to ListViewFiles.Items.Count-1 do
     ListViewFiles.Items[i].Checked := bSelect;
 finally
   Screen.cursor := crDefault;
 end;
end;

procedure TFRecover.sbUncheckAllClick(Sender: TObject);
begin
  SelectUnSelectAll(false);
end;

procedure TFRecover.OpenQuarantineFolder;
  var
    sDir : string;
begin
  sDir := '';

  sDir := BrowseDialog(sMsgSelectQuarantineDirectory, BIF_RETURNONLYFSDIRS);

  if sDir <> '' then
  begin
    sCurrentDir := sDir;
    Screen.cursor := crHourglass;
      ReloadQuarantineFolder;
    Screen.cursor := crDefault;
  end;
end;

procedure TFRecover.sbOpenFolderClick(Sender: TObject);
begin
  OpenQuarantineFolder;
end;

procedure TFRecover.mnuFile_OpenquarantinefolderClick(Sender: TObject);
begin
  OpenQuarantineFolder;
end;

function TFRecover.RestoreFile(const sFileFrom : string; const sFileTo : string) : boolean;
  var
    bMoved : boolean;
    sMsg : string;
begin
  result := false;

  sMsg := Format(sMsgRestoringFile, [sFileTo]);
  Status(sMsg);
  Log(sMsg, sPathLog + LOGFILE_RESTORE,true,true);

  if CheckFileExists(sFileFrom) then
  begin
    bMoved := CopyFile(PChar(sFileFrom),PChar(sFileTo),false);

    if bMoved then
      begin
        result := true;

        if CheckFileExists(sFileTo) then
        begin
          FileSetAttr(sFileFrom, faArchive);
          DeleteFile(sFileFrom);
          DeleteFile(sFileFrom + '.txt');
        end;

        sMsg := Format(sMsgFileRestored,[sFileTo]);
        Status(sMsg);
        Log(sMsg, sPathLog + LOGFILE_RESTORE,true,true);
      end
    else
      begin
        sMsg := Format(sMsgFileNotRestored,[sFileTo]);
        Status(sMsg);
        Log(sMsg, sPathLog + LOGFILE_RESTORE,true,true);
      end;
  end;
end;

procedure TFRecover.DeleteQuarantineFile(const sFileFrom : string);
  var
    sMsg : string;
begin

  if CheckFileExists(sFileFrom) then
  begin
    FileSetAttr(sFileFrom, faArchive);
    DeleteFile(sFileFrom);
    DeleteFile(sFileFrom + '.txt');

    sMsg := Format(sMsgFileDeleted,[sFileFrom]);
    Status(sMsg);
    Log(sMsg, sPathLog + LOGFILE_RESTORE,true,true);
  end;
end;

Procedure TFRecover.DeleteSelectedFiles;
  var i : integer;
      sFileFrom : string;
      sMsg : string;
      iFilesSelected : integer;
begin
iFilesSelected:=0;

for i := 0 to ListViewFiles.Items.Count-1 do
begin
  if ListViewFiles.Items[i].Checked then
   Inc(iFilesSelected);
end;

if iFilesSelected > 0 then
begin
  if Application.MessageBox(PChar(Format(sMsgAskDeleteFiles,[IntToStr(iFilesSelected)])), sMsgAttention, MB_YESNO) = IDNO then
    exit;

  Screen.cursor := crHourGlass;

  ProgressBar.Position := 0;
  ProgressBar.Max := ListViewFiles.Items.Count;

  try
    if sCurrentDir <> '' then
    begin
      for i := 0 to ListViewFiles.Items.Count-1 do
      begin
        if ListViewFiles.Items[i].Checked then
        begin
          sFileFrom := ListViewFiles.Items[i].SubItems[0];

          if Trim(ExtractFilePath(sFileFrom)) = '' then
            sFileFrom := AddSlash(sCurrentDir) + sFileFrom;

          DeleteQuarantineFile(sFileFrom);

          ProgressBar.Position := i;
        end;
      end;

      ReloadQuarantineFolder;
    end;

    Status(sMsg);
    Log(sMsg, sPathLog + LOGFILE_RESTORE,true,true);
  finally
    Screen.cursor := crDefault;
    ProgressBar.Position := 0;
  end;
end;
end;

procedure TFRecover.RestoreSelectedFiles;
  var i : integer;
      sFileFrom : string;
      sFileTo : string;
      iFilesRestored : integer;
      iFilesSelected : integer;
      sMsg : string;
begin

iFilesSelected:=0;

for i := 0 to ListViewFiles.Items.Count-1 do
begin
  if ListViewFiles.Items[i].Checked then
   Inc(iFilesSelected);
end;

if iFilesSelected > 0 then
begin
  if Application.MessageBox(PChar(Format(sMsgAskRestoreFiles,[IntToStr(iFilesSelected)])), sMsgAttention, MB_YESNO) = IDNO then
    exit;

  Screen.cursor := crHourGlass;

  iFilesRestored := 0;
  ProgressBar.Position := 0;
  ProgressBar.Max := ListViewFiles.Items.Count;

  try
    if sCurrentDir <> '' then
    begin
      for i := 0 to ListViewFiles.Items.Count-1 do
      begin
        if ListViewFiles.Items[i].Checked then
        begin
          sFileFrom := ListViewFiles.Items[i].SubItems[0];

          if Trim(ExtractFilePath(sFileFrom)) = '' then
            sFileFrom := AddSlash(sCurrentDir) + sFileFrom;

          sFileTo := Trim(ListViewFiles.Items[i].Caption);

          if RestoreFile(sFileFrom, sFileTo) then
            Inc(iFilesRestored);

          ProgressBar.Position := i;
        end;
      end;

      ReloadQuarantineFolder;
    end;

    sMsg := Format(sMsgFilesSelectedRestored, [iFilesSelected, iFilesRestored]);
    Status(sMsg);
    Log(sMsg, sPathLog + LOGFILE_RESTORE,true,true);
  finally
    Screen.cursor := crDefault;
    ProgressBar.Position := 0;
  end;
end;
end;

procedure TFRecover.sbRestoreClick(Sender: TObject);
begin
  sbRestore.Enabled := false;
  try
    RestoreSelectedFiles;
  finally
    sbRestore.Enabled := true;
  end;
end;

procedure TFRecover.mnuFile_RestoreSelectedClick(Sender: TObject);
begin
  mnuFile_RestoreSelected.enabled := false;
  try
    RestoreSelectedFiles;
  finally
    mnuFile_RestoreSelected.enabled := true;
  end;
end;

procedure TFRecover.mnuUtility_UnselectAllClick(Sender: TObject);
begin
  SelectUnSelectAll(false);
end;

procedure TFRecover.mnuUtility_SelectAllClick(Sender: TObject);
begin
  SelectUnSelectAll(true);
end;

procedure TFRecover.sbViewLogClick(Sender: TObject);
begin
  ShowFile(sPathLog + LOGFILE_RESTORE);
end;

procedure TFRecover.mnuUtility_LogClick(Sender: TObject);
begin
  ShowFile(sPathLog + LOGFILE_RESTORE);
end;

procedure TFRecover.sbLangClick(Sender: TObject);
begin
  if CurLang=High(aLanguages) then
    CurLang:=Low(aLanguages)
  else
    Inc(CurLang);

  SetLanguage_FRecover(FRecover);
end;

procedure TFRecover.ListViewFilesColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  ListViewFiles.Tag := ListViewFiles.Column[Column.Index].Index;
  ListViewFiles.AlphaSort;
end;

procedure TFRecover.ListViewFilesCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
  var
    SortColumn : integer;
begin
  SortColumn := ListViewFiles.Tag;
  if SortColumn = 0 then
    Compare := CompareText(Item1.Caption, Item2.Caption)
  else
    Compare := CompareText(Item1.SubItems[SortColumn - 1], Item2.SubItems[SortColumn - 1]);
end;

procedure TFRecover.SelectFilesWithGoodSignature;
var
  resultSign : TVerifyResult;
  i : integer;
  sFileFrom : string;
begin
  Screen.cursor := crHourGlass;

  ProgressBar.Position := 0;
  ProgressBar.Max := ListViewFiles.Items.Count;

  try
    for i := 0 to ListViewFiles.Items.Count-1 do
    begin
      sFileFrom := ListViewFiles.Items[i].SubItems[0];

      if Trim(ExtractFilePath(sFileFrom)) = '' then
        sFileFrom := AddSlash(sCurrentDir) + sFileFrom;

      resultSign := CheckFileTrust(sFileFrom);

      ListViewFiles.Items[i].Checked := (resultSign = SignOK);

      ProgressBar.Position := i;
    end;
  finally
    Screen.cursor := crDefault;
    ProgressBar.Position := 0;
  end;
end;

procedure TFRecover.sbCheckValidSignaturesClick(Sender: TObject);
begin
  SelectFilesWithGoodSignature;
end;

procedure TFRecover.mnuUtility_CheckValidSignaturesClick(Sender: TObject);
begin
  SelectFilesWithGoodSignature;
end;

procedure TFRecover.ReloadQuarantineFolder;
var
  CurrSave : TCursor;
begin
  CurrSave := Screen.Cursor;

  Screen.cursor := crHourglass;
  try
    ListFileDir(sCurrentDir, ListViewFiles.Items);
    ListViewFiles.Repaint;
  finally
    Screen.cursor := CurrSave;
  end;
end;

procedure TFRecover.sbReloadClick(Sender: TObject);
begin
  Screen.cursor := crHourglass;
    ReloadQuarantineFolder;
  Screen.cursor := crDefault;
end;

procedure TFRecover.mnuFile_ReloadClick(Sender: TObject);
begin
  Screen.cursor := crHourglass;
    ReloadQuarantineFolder;
  Screen.cursor := crDefault;
end;

procedure TFRecover.mnuFile_DeleteSelectedClick(Sender: TObject);
begin
  mnuFile_DeleteSelected.enabled := false;
  try
    DeleteSelectedFiles;
  finally
    mnuFile_DeleteSelected.enabled := true;
  end;
end;

procedure TFRecover.sbDeleteClick(Sender: TObject);
begin
  sbDelete.Enabled := false;
  try
    DeleteSelectedFiles;
  finally
    sbDelete.Enabled := true;
  end;
end;

end.
 