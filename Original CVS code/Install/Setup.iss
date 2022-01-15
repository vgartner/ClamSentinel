; Script for Inno Setup

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{060FE577-1BDF-4330-ACCA-B6760AB07191}
AppName=Clam Sentinel
AppVerName=Clam Sentinel 1.22
VersionInfoVersion=1.22.0.0
VersionInfoProductTextVersion=1.22
OutputBaseFilename=ClamSentinel1.22
AppPublisher=Andrea Russo - Italy
AppCopyright=Copyright (C) 2010-2014 Andrea Russo - Italy
AppPublisherURL=http://clamsentinel.sourceforge.net/
AppSupportURL=http://clamsentinel.sourceforge.net/
AppUpdatesURL=http://clamsentinel.sourceforge.net/
DefaultDirName={pf}\ClamSentinel
DefaultGroupName=Clam Sentinel
AllowNoIcons=yes
Compression=lzma
SolidCompression=yes
PrivilegesRequired=none

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"; LicenseFile: "..\License.txt"
Name: "italian"; MessagesFile: "compiler:Languages\Italian.isl"; LicenseFile: "..\Licenza.txt"
Name: "french"; MessagesFile: "compiler:Languages\French.isl"; LicenseFile: "..\Licence.txt"
Name: "japanese"; MessagesFile: "compiler:Languages\Japanese.isl"; LicenseFile: "..\License.txt"
Name: "german"; MessagesFile: "compiler:Languages\German.isl"; LicenseFile: "..\License.txt"
Name: "spanish"; MessagesFile: "compiler:Languages\Spanish.isl"; LicenseFile: "..\Licencia.txt"
Name: "polish"; MessagesFile: "compiler:Languages\Polish.isl"; LicenseFile: "..\License.txt"
Name: "russian"; MessagesFile: "compiler:Languages\Russian.isl"; LicenseFile: "..\License.txt"
Name: "portuguese"; MessagesFile: "compiler:Languages\Portuguese.isl"; LicenseFile: "..\License.txt"
Name: "brazilianportuguese"; MessagesFile: "compiler:Languages\BrazilianPortuguese.isl"; LicenseFile: "..\License.txt"
Name: "bulgarian"; MessagesFile: "compiler:Languages\Bulgarian.isl"; LicenseFile: "..\License.txt"
Name: "dutch"; MessagesFile: "compiler:Languages\Dutch.isl"; LicenseFile: "..\License.txt"
Name: "galician"; MessagesFile: "compiler:Languages\Galician.isl"; LicenseFile: "..\Licenza_gl.txt"
;Name: "hebrew"; MessagesFile: "compiler:Languages\Hebrew.isl"; LicenseFile: "..\License.txt"

[CustomMessages]
english.ProgramIsRunning = The program Clam Sentinel is running.
english.PleaseCloseTheProgramFirst = Please close the program first.
english.ClamWinNotInstalled = Setup detected that ClamWin is not installed.
english.ClamWinIsRequired = Clam Sentinel requires the installation of ClamWin http://www.clamwin.com/
english.SetupWillTerminateNow = Setup will terminate now.
english.AutostartKeyForAllUser = Start Clam Sentinel automatically for all users
english.SpanishQuestion =

italian.ProgramIsRunning = Il programma Clam Sentinel � in esecuzione.
italian.PleaseCloseTheProgramFirst = Chiudere il programma prima di continuare con l'installazione.
italian.ClamWinNotInstalled = Il setup ha rilevato che ClamWin non � installato.
italian.ClamWinIsRequired = Clam Sentinel richiede l'installazione di ClamWin http://it.clamwin.com/
italian.SetupWillTerminateNow = L'installazione terminer� adesso.
italian.AutostartKeyForAllUser = Avviare automaticamente Clam Sentinel per tutti gli utenti
italian.SpanishQuestion =

french.ProgramIsRunning = Le programme Clam Sentinel en cours d'ex�cution.
french.PleaseCloseTheProgramFirst = Fermez le programme avant de poursuivre avec l'installation.
french.ClamWinNotInstalled = Le programme d'installation a d�tect� que ClamWin n'est pas install�.
french.ClamWinIsRequired = Clam Sentinel n�cessite l'installation de ClamWin http://fr.clamwin.com/
french.SetupWillTerminateNow = L'installation se terminera maintenant.
french.AutostartKeyForAllUser = D�marrer Clam Sentinel automatiquement pour tous les utilisateurs
french.SpanishQuestion =

japanese.ProgramIsRunning = Clam Sentinel�͍쓮���ł�
japanese.PleaseCloseTheProgramFirst = �ŏ��Ƀv���O��������ĉ�����
japanese.ClamWinNotInstalled = ClamWin���C���X�g�[������Ă��܂���
japanese.ClamWinIsRequired = Clam Sentinel�̃C���X�g�[���ɂ�ClamWin���K�v�ł� http://www.clamwin.com/
japanese.SetupWillTerminateNow = �Z�b�g�A�b�v���I�����܂�
japanese.AutostartKeyForAllUser = ���ׂẴ��[�U�[�Ŏ����I��Clam Sentinel���N�����܂����H
japanese.SpanishQuestion =

german.ProgramIsRunning = Das Programm ist noch aktiv.
german.PleaseCloseTheProgramFirst = Bitte schlie�e das Programm zuerst.
german.ClamWinNotInstalled = Das Setup hat erkannt das ClamWin nicht Installiert ist.
german.ClamWinIsRequired = Clam Sentinel ben�tigt die Installation von ClamWin http://de.clamwin.com/
german.SetupWillTerminateNow = Das Setup wird nun beendet.
german.AutostartKeyForAllUser = Start Clam Sentinel automatisch f�r alle Benutzer
german.SpanishQuestion =

spanish.ProgramIsRunning = El programa Clam Sentinel se ejecuta.
spanish.PleaseCloseTheProgramFirst = Por favor cierre el programa primero.
spanish.ClamWinNotInstalled = Se detect� que ClamWin no esta instalado.
spanish.ClamWinIsRequired = Clam Sentinel requiere la instalaci�n de ClamWin http://www.clamwin.com/
spanish.SetupWillTerminateNow = La instalaci�n finalizar� ahora.
spanish.AutostartKeyForAllUser = Iniciar Clam Sentinel automaticamente para todos los usuarios
spanish.SpanishQuestion = �

polish.ProgramIsRunning = Program Clam Sentinel jest uruchomiony.
polish.PleaseCloseTheProgramFirst = Najpierw prosze zamknac program.
polish.ClamWinNotInstalled = Program instalacyjny wykryl, ze ClamWin nie jest zainstalowany.
polish.ClamWinIsRequired = Clam Sentinel wymaga instalacji ClamWin`a http://www.clamwin.com/
polish.SetupWillTerminateNow = Program instalacyjny zostanie teraz zamkniety.
polish.AutostartKeyForAllUser = Uruchom Clam Sentinel automatycznie dla wszystkich uzytkownik�w
polish.SpanishQuestion = ''

russian.ProgramIsRunning = ���������� Clam Sentinel ��������.
russian.PleaseCloseTheProgramFirst = �������� ���������� ��� ����������� ���������.
russian.ClamWinNotInstalled = ��� ��������� ���������� ��� ClamWin ������������.
russian.ClamWinIsRequired = ��� Clam Sentinel ��������� ��������� ClamWin http://www.clamwin.com/
russian.SetupWillTerminateNow = ��������� ������ ����� ���������.
russian.AutostartKeyForAllUser = ��������� Clam Sentinel ������������� ��� ���� �������������
russian.SpanishQuestion = ''

portuguese.ProgramIsRunning = O Clam Sentinel est� sendo executado.
portuguese.PleaseCloseTheProgramFirst = Por favor, antes de remover feche o Clam Sentinel.
portuguese.ClamWinNotInstalled = O Setup detectou que o ClamWin n�o est� instalado.
portuguese.ClamWinIsRequired = O Clam Sentinel requer a instala��o do ClamWin http://www.clamwin.com/
portuguese.SetupWillTerminateNow = O Setup ser� encerrado agora.
portuguese.AutostartKeyForAllUser = Iniciar o Clam Sentinel automaticamente para todos os usu�rios
portuguese.SpanishQuestion = ''

brazilianportuguese.ProgramIsRunning = O Clam Sentinel est� sendo executado.
brazilianportuguese.PleaseCloseTheProgramFirst = Por favor, antes de remover feche o Clam Sentinel.
brazilianportuguese.ClamWinNotInstalled = O Setup detectou que o ClamWin n�o est� instalado.
brazilianportuguese.ClamWinIsRequired = O Clam Sentinel requer a instala��o do ClamWin http://www.clamwin.com/
brazilianportuguese.SetupWillTerminateNow = O Setup ser� encerrado agora.
brazilianportuguese.AutostartKeyForAllUser = Iniciar o Clam Sentinel automaticamente para todos os usu�rios
brazilianportuguese.SpanishQuestion = ''

bulgarian.ProgramIsRunning = ���������� Clam Sentinel �� ��������.
bulgarian.PleaseCloseTheProgramFirst = ���� ��������� ���������� �����.
bulgarian.ClamWinNotInstalled = ���������� �� ������� ClamWin �� � ����������.
bulgarian.ClamWinIsRequired = Clam Sentinel ������� ���������� ��  ClamWin http://www.clamwin.com/
bulgarian.SetupWillTerminateNow = ������������ �� ���� ���������� ����.
bulgarian.AutostartKeyForAllUser = ��������� Clam Sentinel ����������� �� ������ �����������.
bulgarian.SpanishQuestion = ''

dutch.ProgramIsRunning = Het programma Clam Sentinel is actief.
dutch.PleaseCloseTheProgramFirst = S.v.p. eerst het programma sluiten.
dutch.ClamWinNotInstalled = Setup heeft gedetecteerd dat ClamWin niet is ge�nstalleerd.
dutch.ClamWinIsRequired = Clam Sentinel heeft een ge�nstalleerde versie van ClamWin nodig (http://www.clamwin.com/)
dutch.SetupWillTerminateNow = Setup zal worden afgesloten.
dutch.AutostartKeyForAllUser = Start Clam Sentinel automatisch voor alle gebruikers
dutch.SpanishQuestion = ''

galician.ProgramIsRunning = O programa Clam Sentinel est� execut�ndose
galician.PleaseCloseTheProgramFirst = Por favor p�cheo primeiro.
galician.ClamWinNotInstalled = O instalador detectou que ClamWin non est� instalado.
galician.ClamWinIsRequired = Clam Sentinel require a instalaci�n de ClamWin http://www.es.clamwin.com/
galician.SetupWillTerminateNow = A instalaci�n rematar� agora.
galician.AutostartKeyForAllUser = Activar Clam Sentinel automaticamente para todos os usuarios
galician.SpanishQuestion = ''

;hebrew.ProgramIsRunning = The program Clam Sentinel is running.
;hebrew.PleaseCloseTheProgramFirst = Please close the program first.
;hebrew.ClamWinNotInstalled = Setup detected that ClamWin is not installed.
;hebrew.ClamWinIsRequired = Clam Sentinel requires the installation of ClamWin http://www.clamwin.com/
;hebrew.SetupWillTerminateNow = Setup will terminate now.
;hebrew.AutostartKeyForAllUser = Start Clam Sentinel automatically for all users
;hebrew.SpanishQuestion =


[Registry]
Root: HKCU; Subkey: "Software\Microsoft\Windows\CurrentVersion\Run"; Valuename: "Clam Sentinel"; Flags: uninsdeletevalue dontcreatekey
Root: HKLM; Subkey: "Software\Microsoft\Windows\CurrentVersion\Run"; ValueType: string; ValueName: "Clam Sentinel"; ValueData: "{app}\ClamSentinel.exe"; Flags: uninsdeletevalue; Check: RegKeyForAllUsers

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "..\ClamSentinel.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\SentinelRecover\SentinelRecover.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Readme.txt"; DestDir: "{app}"; Languages: english; Flags: ignoreversion
Source: "..\Leggimi.txt"; DestDir: "{app}"; Languages: italian; Flags: ignoreversion
Source: "..\Lisezmoi.txt"; DestDir: "{app}"; Languages: french; Flags: ignoreversion
Source: "..\Readme.txt"; DestDir: "{app}"; Languages: japanese; Flags: ignoreversion
Source: "..\LiesMich.txt"; DestDir: "{app}"; Languages: german; Flags: ignoreversion
Source: "..\Leggimi.txt"; DestDir: "{app}"; Languages: spanish; Flags: ignoreversion
Source: "..\Readme.txt"; DestDir: "{app}"; Languages: polish; Flags: ignoreversion
Source: "..\Readme.txt"; DestDir: "{app}"; Languages: portuguese; Flags: ignoreversion
Source: "..\Readme.txt"; DestDir: "{app}"; Languages: brazilianportuguese; Flags: ignoreversion
Source: "..\Readme.txt"; DestDir: "{app}"; Languages: bulgarian; Flags: ignoreversion
Source: "..\LiesMich.txt"; DestDir: "{app}"; Languages: dutch; Flags: ignoreversion
Source: "..\Leame.txt"; DestDir: "{app}"; Languages: galician; Flags: ignoreversion
;Source: "..\Readme.txt"; DestDir: "{app}"; Languages: hebrew; Flags: ignoreversion

Source: "..\Opis programu Clam Sentinel i jego instalacji.html"; DestDir: "{app}"; Languages: polish; Flags: ignoreversion
Source: "..\zapewnic_lepsza_ochrone.html"; DestDir: "{app}"; Languages: polish; Flags: ignoreversion
Source: "..\�ӫ�Կ�ѡ�.txt"; DestDir: "{app}"; Languages: russian; Flags: ignoreversion
Source: "..\SENTINEL.VXD"; DestDir: "{app}"; Flags: ignoreversion; Check: not UsingWinNT
Source: "..\SentinelSimpleGuide_gl.html"; DestDir: "{app}"; Languages: galician; Flags: ignoreversion
Source: "..\ExtraProtection_gl.html"; DestDir: "{app}"; Languages: galician; Flags: ignoreversion

Source: "..\SentinelSimpleGuide.html"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\ExtraProtection.html"; DestDir: "{app}"; Flags: ignoreversion

; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\Clam Sentinel"; Filename: "{app}\ClamSentinel.exe"
Name: "{group}\Sentinel Recover"; Filename: "{app}\SentinelRecover.exe"
Name: "{group}\The Clam Sentinel Program Description And Setup"; Filename: "{app}\SentinelSimpleGuide.html"
Name: "{group}\{cm:UninstallProgram,Clam Sentinel}"; Filename: "{uninstallexe}"
Name: "{commondesktop}\Clam Sentinel"; Filename: "{app}\ClamSentinel.exe"; Tasks: desktopicon
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\Clam Sentinel"; Filename: "{app}\ClamSentinel.exe"; Tasks: quicklaunchicon

[InstallDelete]
Type: files; Name: "{app}\MaxSentinel.html"

[UninstallDelete]
Type: files; Name: "{app}\ClamSentinel.ini"
Type: files; Name: "{app}\*.bat"
Type: files; Name: "{%temp}\MemoryScan.bat"
Type: files; Name: "{%temp}\DriveScan.bat"
Type: filesandordirs; Name: "{userappdata}\ClamSentinel"
Type: files; Name: "{userappdata}\.ClamWin\log\ClamSentinel*"
Type: files; Name: "{commonappdata}\.ClamWin\log\ClamSentinel*"
Type: files; Name: "{userappdata}\..\.ClamWin\log\ClamSentinel*"
Type: files; Name: "{commonappdata}\..\.ClamWin\log\ClamSentinel*"

[Run]
Filename: "{app}\ClamSentinel.exe"; Description: "{cm:LaunchProgram,Clam Sentinel}"; Flags: nowait postinstall skipifsilent

[Code]
var
  bRegKeyForAllUser : boolean;
  AllUsersPage: TInputOptionWizardPage;

function RegKeyForAllUsers() : Boolean;
begin
  result := AllUsersPage.Values[0];
end;
 
function HasPrivileges(): Boolean;
begin
  Result := IsAdminLoggedOn or IsPowerUserLoggedOn;
end;

procedure CheckForRegKey_AllUser();
var
 sDummy : string;
begin
  bRegKeyForAllUser := false;

  if HasPrivileges then
  begin
    if RegQueryStringValue(HKEY_LOCAL_MACHINE, 'SOFTWARE\Microsoft\Windows\CurrentVersion\Run',
      'Clam Sentinel', sDummy) then
          bRegKeyForAllUser := true;
  end
end;

function FindWindow(class_name : PChar; window_name : PChar) : longint; external 'FindWindowA@user32.dll stdcall';

function WaitUntilSentinelClose() : boolean;
var
  Res : longint;
begin
  Result := true;

  if FindWindow('TFClamSentinel','')<>0 then
  begin
    Repeat
      Res:=MsgBox(CustomMessage('ProgramIsRunning')+#10#13#10#13+CustomMessage('PleaseCloseTheProgramFirst')+#10#13#10#13,mbInformation, MB_OKCancel);
    until (Res=IDCancel) or (FindWindow('TFClamSentinel','')=0);

    if Res=IDCancel then
      Result := False;
  end;
end;

function InitializeSetup (): Boolean;
var
  Value : Cardinal;
begin
  Result := True;
  bRegKeyForAllUser := false;

  Value := 0;
  if not RegQueryDWordValue(HKEY_CURRENT_USER, 'SOFTWARE\Clamwin', 'Version',  Value) then
  begin
    Value := 0;
    RegQueryDWordValue(HKEY_LOCAL_MACHINE, 'SOFTWARE\Clamwin', 'Version',  Value)
  end;

  if Value = 0 then
  begin
    SuppressibleMsgBox(CustomMessage('ClamWinNotInstalled') + #10#13#10#13
         + CustomMessage('ClamWinIsRequired') + #10#13#10#13
         + CustomMessage('SetupWillTerminateNow'), mbError, MB_OK, IDOK);
    Result := False;
    exit;
  end;

  Result := WaitUntilSentinelClose;
  CheckForRegKey_AllUser;
end;

function InitializeUninstall(): boolean;
begin
  Result := WaitUntilSentinelClose;
  bRegKeyForAllUser := HasPrivileges;
end;

procedure InitializeWizard;
begin
  AllUsersPage := CreateInputOptionPage(wpLicense,
   'AutoStart', CustomMessage('SpanishQuestion') + CustomMessage('AutostartKeyForAllUser')+'?','', False, False);
  AllUsersPage.Add(CustomMessage('AutostartKeyForAllUser'));
  AllUsersPage.Values[0] := bRegKeyForAllUser;
end;

function ShouldSkipPage(PageID: Integer): Boolean;
begin
  { Skip pages that shouldn't be shown }
  if (PageID = AllUsersPage.ID) and not HasPrivileges then
    Result := True
  else
    Result := False;
end;

function UpdateReadyMemo(Space, NewLine, MemoUserInfoInfo, MemoDirInfo, MemoTypeInfo,
  MemoComponentsInfo, MemoGroupInfo, MemoTasksInfo: String): String;
var
  S: String;
begin
  { Fill the 'Ready Memo' with the normal settings and the custom settings }
  S := '';
  
  if MemoUserInfoInfo<>'' then
    S := S + MemoUserInfoInfo + NewLine;
    
  if MemoDirInfo<>'' then
    S := S + MemoDirInfo + NewLine;

  if MemoTypeInfo<>'' then
    S := S + MemoTypeInfo + NewLine;

  if MemoComponentsInfo<>'' then
    S := S + MemoComponentsInfo + NewLine;

  if MemoGroupInfo<>'' then
    S := S + MemoGroupInfo + NewLine;

  if MemoTasksInfo<>'' then
    S := S + MemoTasksInfo + NewLine;

  if RegKeyForAllUsers then
    S := S + NewLine + CustomMessage('AutostartKeyForAllUser') + NewLine;

  Result := S;
end;
