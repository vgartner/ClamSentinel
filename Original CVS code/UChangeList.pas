unit UChangeList;

{$I DelphiVersion.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TListType = (ScanPaths,Extensions,FullScanFolders);

type
  TFChangeList = class(TForm)
    PanelBase: TPanel;
    PanelBottom: TPanel;
    btnConfirm: TButton;
    btnCancel: TButton;
    Panel: TPanel;
    sbSelectFolder: TSpeedButton;
    sbSelectFile: TSpeedButton;
    EditValue: TEdit;
    btnReplace: TButton;
    btnAdd: TButton;
    btnDelete: TButton;
    ListValues: TListBox;
    OpenDialog: TOpenDialog;
    procedure btnConfirmClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCancelClick(Sender: TObject);
    procedure ListValuesClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure EditValueKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure sbSelectFolderClick(Sender: TObject);
    procedure sbSelectFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    List : TStrings;

    procedure CheckExtension(var sVal : string);
  public
    { Public declarations }

    sMsgValueAlreadyExists : string;
    sMsgNoExtensionsScannedDefined : string;
    sMsgSelectFolder : string;
    sMsgSelectFile : string;
    
    ListType : TListType;

    procedure Settings( var Lst : TStrings; TypeLst : TListType);
  end;

var
  FChangeList: TFChangeList;

implementation

uses Sentinel, Utility, Languages, Start, UVersion, FileCtrl, ShlObj;

{$R *.DFM}

procedure TFChangeList.btnConfirmClick(Sender: TObject);
begin
  if (ListType = Extensions) and (ListValues.Items.Count = 0) then
  begin
    TopMessage(Handle,sMsgNoExtensionsScannedDefined,cProjectName,MB_ICONERROR);
    exit;
  end;

  List.Clear;
  List.AddStrings(ListValues.Items);

  FClamSentinel.ModalResult := mrOk;
  Close;
end;

procedure TFChangeList.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FChangeList := nil;
  action := caFree;
end;

procedure TFChangeList.btnCancelClick(Sender: TObject);
begin
  FClamSentinel.ModalResult := mrCancel;
  Close;
end;

procedure TFChangeList.ListValuesClick(Sender: TObject);
begin
  if ListValues.ItemIndex>=0 then
    EditValue.Text := ListValues.Items[ListValues.ItemIndex];
end;

procedure TFChangeList.btnReplaceClick(Sender: TObject);
var
 iIndex : integer;
 sValue : string;
begin
  iIndex := ListValues.ItemIndex;
  sValue := trim(EditValue.Text);

  if (sValue<>'') and (iIndex>=0) then
  begin
    if ListType = Extensions then
      CheckExtension(sValue);

    if ListValues.Items.IndexOf(sValue) < 0 then
      begin
        ListValues.Items[iIndex] := sValue;
        EditValue.SetFocus;
        ListBoxHorScrollBar(ListValues);
      end
    else
      TopMessage(Handle,Format(sMsgValueAlreadyExists,[sValue]),cProjectName,MB_ICONERROR);
  end;
end;

procedure TFChangeList.btnAddClick(Sender: TObject);
var
 sValue : string;
begin
  sValue := trim(EditValue.Text);

  if sValue<>'' then
  begin
    if ListType = Extensions then
      CheckExtension(sValue);

    if ListValues.Items.IndexOf(sValue) < 0 then
      begin
        ListValues.items.Insert(0,sValue);
        ListValues.ItemIndex := 0;
        EditValue.Text := '';
        ListBoxHorScrollBar(ListValues);
      end
    else
      TopMessage(Handle,Format(sMsgValueAlreadyExists,[sValue]),cProjectName,MB_ICONERROR);
  end;
end;

procedure TFChangeList.btnDeleteClick(Sender: TObject);
var
 iIndex : integer;
 iPos : integer;
begin
  iIndex := ListValues.ItemIndex;

  if iIndex>=0 then
  begin
    ListValues.items.Delete(iIndex);

    iPos := ListValues.Items.Count-1;

    if ListValues.Items.Count > iIndex then
      iPos := iIndex;

    EditValue.Text := '';
    ListValues.ItemIndex := iPos;

    if iPos >= 0 then
      EditValue.Text := ListValues.Items[iPos];

    ListBoxHorScrollBar(ListValues);
  end;
end;

procedure TFChangeList.EditValueKeyPress(Sender: TObject; var Key: Char);
begin
  if (ListType in [ScanPaths,FullScanFolders]) and (Key in ['\' ,':']) then
    exit;

  if (ListType in [ScanPaths]) and (Key in ['*','?']) then
    exit;
    
  if (Key in [',']) or IsForbiddenCharForFilename(Key) then
  begin
    Key := #0;
    beep;
  end;
end;

procedure TFChangeList.Settings(var Lst : TStrings; TypeLst : TListType);
begin
  List := TStringList.Create;
  List := Lst;
  ListType := TypeLst;

  ListValues.Clear;
  ListValues.Sorted := true;
  ListValues.Items.AddStrings(List);
  ListValues.Sorted := false;
  
  If List.Count>0 then
    ListValues.ItemIndex := 0;

  ListBoxHorScrollBar(ListValues);
end;

procedure TFChangeList.CheckExtension(var sVal : string);
begin
  sVal := UpperCase(sVal);

  if Copy(sVal,1,1) <> '.' then
    sVal := '.' + sVal;

  EditValue.Text := sVal;
end;

procedure TFChangeList.FormShow(Sender: TObject);
begin
  sbSelectFolder.Visible := (ListType in [ScanPaths,FullScanFolders]);
  sbSelectFile.Visible := (ListType = ScanPaths);

  SetLanguage_FChangeList(FChangeList);
end;

procedure TFChangeList.sbSelectFolderClick(Sender: TObject);
var
  sDir: string;
begin
  sDir := BrowseDialog(sMsgSelectFolder, BIF_RETURNONLYFSDIRS);
  
  if sDir <> '' then
    EditValue.Text := AddSlash(sDir);
end;

procedure TFChangeList.sbSelectFileClick(Sender: TObject);
begin
  OpenDialog.DefaultExt := '';
  OpenDialog.Filter     := '';
  OpenDialog.Options    := [ofFileMustExist];
  OpenDialog.Title := sMsgSelectFile;

  OpenDialog.InitialDir := '';
  OpenDialog.FileName := '';

  if OpenDialog.Execute then
    EditValue.Text := OpenDialog.FileName;
end;

procedure TFChangeList.FormCreate(Sender: TObject);
begin
  // DPI fix begin
  ClientWidth := PanelBase.ClientWidth;
  ClientHeight := PanelBase.ClientHeight;
  PanelBase.Align := alClient;
  // DPI fix end
end;

end.
