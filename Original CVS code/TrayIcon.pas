unit TrayIcon;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, ShellApi, Balloon;

const
  NIIF_INFO = $00000001;
  NIIF_WARNING = $00000002;
  NIIF_ERROR = $00000003;

  NIF_INFO = $10;

  NIN_BALLOONSHOW = WM_USER + 2;  
  NIN_BALLOONHIDE = WM_USER + 3;
  NIN_BALLOONTIMEOUT = WM_USER + 4;
  NIN_BALLOONUSERCLICK = WM_USER + 5;

  {new NotifyIconData structure definition}

  SEC_DURATION_VIRUS_ALERT = 86400;
  SEC_DURATION_ABOUT = 30;
  SEC_DURATION_LATEST_VERSION = 30;
  SEC_DURATION_REGISTRY_ALERT = 86400;
  SEC_DURATION_MODIFIED_FOLDER_ALERT = 5;
  SEC_DURATION_NEW_FILE_CREATED = 6;

  SEC_DURATION_ALERTS_WIN2000 = 30;

type
  PNewNotifyIconData = ^TNewNotifyIconData;
  TDUMMYUNIONNAME    = record
    case Integer of
      0: (uTimeout: UINT);
      1: (uVersion: UINT);
  end;

  TNewNotifyIconData = record
    cbSize: DWORD;
    Wnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
   //Version 5.0 is 128 chars, old ver is 64 chars
    szTip: array [0..127] of Char;
    dwState: DWORD; //Version 5.0
    dwStateMask: DWORD; //Version 5.0
    szInfo: array [0..255] of Char; //Version 5.0
    DUMMYUNIONNAME: TDUMMYUNIONNAME;
    szInfoTitle: array [0..63] of Char; //Version 5.0
    dwInfoFlags: DWORD;   //Version 5.0
  end;

type

  TNewNotifyIconDataW = record
    cbSize: DWORD;
    Wnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array [0..127] of WideChar;
    dwState: DWORD;
    dwStateMask: DWORD;
    szInfo: array [0..255] of WideChar;
    uTimeout: UINT;
    szInfoTitle: array [0..63] of WideChar;
    dwInfoFlags: DWORD;
  end;

var
  IconData : TNewNotifyIconData;
  IconDataW : TNewNotifyIconDataW;

  TrayBalloon : TBalloonControl;

  NotWin9x : boolean;
  IsWin9x : boolean;

  procedure ChangeToolTip(s : string);
  procedure ChangeToolTipW(sw : WideString);
  procedure OnSystemTry(Sender: TObject);
  procedure AddIcon(F : TForm);
  procedure GoTray(F : TForm);
  procedure DeleteIcon;
  procedure RefreshIcon;
  procedure NewIcon(Icon : hIcon);
  procedure ShowBalloonTips(const sTitle : string; const sInfo : string;
                         const InfoFlags: DWORD; const iSecDuration : Cardinal = 0);
  procedure DeleteBalloonTips;
  procedure IsNotBalloonTip;
  procedure ShowBalloonWin9x(const sTitle : string; const sInfo : string;
                       const InfoFlags: DWORD; const iSecDuration : Cardinal = 0);

  function ShowBalloonTipsW(const swTitle : WideString; const swInfo : WideString;
                       const InfoFlags: DWORD; const iSecDuration : Cardinal = 0) : boolean;

implementation

uses Graphics, Math;

procedure EraseArrayOfWideChar(var awc : array of WideChar);
var
 i : integer;
begin
  for i:=0 to Length(awc)-1 do awc[i]:=#0;
end;

function StringToWide(const s : string): WideString;
const
  SIZE = 4096;
var
  Buffer: array[0..SIZE-1] of WideChar;
begin
  FillChar(Buffer, SizeOf(Buffer), 0);
  result := WideString(StringToWideChar(Copy(s,1,SIZE-1), Buffer, SizeOf(Buffer)));
end;

procedure MoveWideStringToArrayOfWideChar(const sw : WideString; var awc : array of WideChar);
begin
  EraseArrayOfWideChar(awc);
  move(sw[1],awc,Min(Length(awc),Length(sw))*SizeOf(WideChar));
end;

procedure NewIcon(Icon : hIcon);
begin
  //IsNotBalloonTip;

  if NotWin9x then
    IconDataW.hIcon := Icon
  else
    IconData.hIcon := Icon;

  RefreshIcon;
end;

procedure DeleteIcon;
begin
  if NotWin9x then
    Shell_NotifyIconW(NIM_DELETE, @IconDataW)
  else
    Shell_NotifyIcon(NIM_DELETE, @IconData);

  Application.ProcessMessages;
end;

procedure AddIcon(F : TForm);
var
  sw : WideString;
begin

if NotWin9x then
  begin
    IconDataW.hIcon := Application.Icon.Handle;

    IconDataW.cbSize := sizeof(IconDataW);
    IconDataW.Wnd := F.Handle;
    IconDataW.uID := 100;
    IconDataW.uFlags := NIF_MESSAGE + NIF_ICON + NIF_TIP;
    IconDataW.uCallbackMessage := WM_USER + 1;
    //IconDataW.hIcon := Application.Icon.Handle;

    sw := StringToWide(Application.Title);
    MoveWideStringToArrayOfWideChar(sw,IconDataW.szTip);

    Shell_NotifyIconW(NIM_ADD, @IconDataW);
  end
else
  begin
    IconData.hIcon := Application.Icon.Handle;

    IconData.cbSize := sizeof(IconData);
    IconData.Wnd := F.Handle;
    IconData.uID := 100;
    IconData.uFlags := NIF_MESSAGE + NIF_ICON + NIF_TIP;
    IconData.uCallbackMessage := WM_USER + 1;
    //IconData.hIcon := Application.Icon.Handle;
    StrPCopy(IconData.szTip, Application.Title);
    Shell_NotifyIcon(NIM_ADD, @IconData);
  end;

  Application.ProcessMessages;
end;

procedure RefreshIcon;
begin
  if NotWin9x then
    Shell_NotifyIconW(NIM_MODIFY, @IconDataW)
  else
    Shell_NotifyIcon(NIM_MODIFY, @IconData);

  Application.ProcessMessages;
end;

procedure GoTray(F : TForm);
begin
   ShowWindow(F.Handle, SW_HIDE);
   ShowWindow(Application.Handle, SW_HIDE);
   Application.ProcessMessages;
end;

procedure OnSystemTry(Sender: TObject);
begin
  AddIcon(TForm(Sender));
  GoTray(TForm(Sender));
end;

procedure ChangeToolTipW(sw : WideString);
begin
  //IsNotBalloonTip;

  MoveWideStringToArrayOfWideChar(sw,IconDataW.szTip);

  RefreshIcon;
end;

procedure ChangeToolTip(s : string);
begin
  if NotWin9x then
    ChangeToolTipW(StringToWide(s))
  else
    begin
      //IsNotBalloonTip;
      StrPCopy(IconData.szTip, s);
      RefreshIcon;
  end;
end;

procedure IsNotBalloonTip;
begin
  if NotWin9x then
  begin
    IconDataW.uFlags := NIF_MESSAGE + NIF_ICON + NIF_TIP;
    RefreshIcon;
  end;
end;

procedure DeleteBalloonTips;

begin
  if NotWin9x then
  begin
    try
      IconDataW.uFlags := NIF_INFO;

      EraseArrayOfWideChar(IconDataW.szInfo);

      RefreshIcon;
    except
      //old version of IE
    end;

    IsNotBalloonTip;
  end;
end;

procedure ShowBalloonWin9x(const sTitle : string; const sInfo : string;
                       const InfoFlags: DWORD; const iSecDuration : Cardinal = 0);
const
 BORDER = 5;
var
 rSysTray : TRect;
 hwndTray : THandle;
 f : TFont;

 ncm: TNonClientMetrics;

begin
  with TrayBalloon do
  begin
    Title := sTitle;
    Text.Clear;
    Text.Add(sInfo);

    if iSecDuration > 0 then
      Duration := iSecDuration
    else
      Duration := 30;

    f := TFont.Create;

    ncm.cbSize := SizeOf(TNonClientMetrics);
    SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(TNonClientMetrics), @ncm, 0);

    {
    lfCaptionFont - regular captions
    lfSmCaptionFont - small captions
    lfMenuFont - menus
    lfStatusFont - status bars and tooltips
    lfMessageFont - message boxes
    }

    f.Handle := CreateFontIndirect(ncm.lfStatusFont);

    case InfoFlags of
         NIIF_INFO:
           begin
             BalloonType := blnInfo;
             f.Color := clInfoText;
             CustomTitleFont := f;
           end;
         NIIF_ERROR:
           begin
             BalloonType := blnError;
             f.Color := clRed;
             CustomTitleFont := f;
           end;

         NIIF_WARNING:
           begin
             BalloonType := blnWarning;
             f.Color := clRed;
             CustomTitleFont := f;
           end;
    end;

    hwndTray := FindWindowEx(FindWindow('Shell_TrayWnd', nil), 0, 'TrayNotifyWnd', nil);

    with rSysTray do
    begin
      if hwndTray <> 0 then
      begin
        GetWindowRect(hwndTray, rSysTray);

        //showmessage(format('left: %d top: %d right: %d bottom: %d',[left,top,right,bottom]));
        if (Left > BORDER) and (Top > BORDER) then
        begin
          //SysTray on Bottom or Right
          Position := blnArrowTopLeft;
          PixelCoordinateX:= Left;
          PixelCoordinateY:= Top;
        end;

        if Left < BORDER then
        begin
          //SysTray on Left
          Position := blnArrowTopRight;
          PixelCoordinateX:= Right;
          PixelCoordinateY:= Top;
        end;

        if Top < BORDER then
        begin
          //SysTray on Top
          Position := blnArrowBottomLeft;
          PixelCoordinateX:= Left;
          PixelCoordinateY:= Top + (Bottom-Top);
        end;
      end
    else
      begin
       SystemParametersInfo(SPI_GETWORKAREA, 0, @rSysTray, 0);
       Position := blnArrowTopLeft;
       PixelCoordinateX:= (Right div 3)*2;
       PixelCoordinateY:= Bottom;
      end;
    end;
    ShowPixelBalloon;
    f.free;
  end;
end;

procedure ShowBalloonTips(const sTitle : string; const sInfo : string;
                       const InfoFlags: DWORD; const iSecDuration : Cardinal = 0);
  var bShowWinBalloon : boolean;
begin
  if IsWin9x then
    bShowWinBalloon := true
  else
    bShowWinBalloon := not ShowBalloonTipsW(StringToWide(sTitle),StringToWide(sInfo),InfoFlags, iSecDuration);

  if bShowWinBalloon then
    ShowBalloonWin9x(sTitle,sInfo,InfoFlags, iSecDuration);
end;

function ShowBalloonTipsW(const swTitle : WideString; const swInfo : WideString;
                       const InfoFlags: DWORD; const iSecDuration : Cardinal = 0) : boolean;
begin
  result := true;
  try
    DeleteBalloonTips;

    IconDataW.cbSize := SizeOf(IconDataW);
    //IconDataW.uFlags := NIF_INFO;
    IconDataW.uFlags := NIF_INFO + NIF_MESSAGE + NIF_ICON + NIF_TIP;

    MoveWideStringToArrayOfWideChar(swInfo,IconDataW.szInfo);

    if iSecDuration > 0 then
      IconDataW.uTimeout := iSecDuration*1000
    else
      IconDataW.uTimeout := 3000;

    MoveWideStringToArrayOfWideChar(swTitle,IconDataW.szInfoTitle);

    IconDataW.dwInfoFlags := InfoFlags; //NIIF_INFO; //NIIF_ERROR; //NIIF_WARNING;
    RefreshIcon;
  except
    //old version of IE
     IconDataW.uFlags := NIF_MESSAGE + NIF_ICON + NIF_TIP;
     RefreshIcon;
     result := false;
  end;
end;

initialization
  NotWin9x := not(Win32Platform = VER_PLATFORM_WIN32_WINDOWS);

  IsWin9x := not(NotWin9x);

  //if IsWin9x then
    TrayBalloon := TBalloonControl.Create(nil);

finalization
  //if IsWin9x then
    TrayBalloon.Free;
end.
