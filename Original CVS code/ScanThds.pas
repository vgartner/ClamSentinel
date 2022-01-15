unit ScanThds;

interface

uses
  Classes;

type
  TScanThread = class(TThread)
  private
    { Private declarations }
    FApplication : string;
    FCommandLine : string;
    FScannerPriority : string;
    
    FLogFile : string;

    FOut: string;
    FErr: string;

    FShowCmd : integer;
    FFiles : string;

  protected
    procedure Execute; override;
    procedure GetDosOutput(const sApplication : string;
                 const sCommandLine: string;
                 var strOut : string; var strErr: string;
                 const ShowCmd : integer; const sScannerPriority : string);
  public
    property Output : string read FOut;
    property Error : string read FErr;
    property LogFile : string read FLogFile;
    property Files : string read FFiles;

    constructor Create(const sApplication : string;
                     const sCommandLine : string; const sLogFile : string;
                     const ShowCmd : integer; const sScannerPriority : string;
                     const sFiles : string);
  end;

implementation

uses Windows, ShellApi;

{ TScanThread }

constructor TScanThread.Create(const sApplication : string;
                          const sCommandLine : string; const sLogFile : string;
                          const ShowCmd : integer; const sScannerPriority : string;
                          const sFiles : string);
begin
  FApplication := sApplication;
  FCommandLine := sCommandLine;
  FScannerPriority := sScannerPriority;
  
  FLogFile := sLogFile;
  FOut := '';
  FErr := '';
  FFiles := sFiles;

  FShowCmd := ShowCmd;

  FreeOnTerminate := True;

  inherited Create(false);
end;

procedure TScanThread.Execute;
begin
  { Place thread code here }
  GetDosOutput(FApplication, FCommandLine, FOut, FErr, FShowCmd, FScannerPriority);
end;

procedure TScanThread.GetDosOutput(const sApplication : string;
                   const sCommandLine: string;
                   var strOut : string; var strErr: string;
                   const ShowCmd : integer; const sScannerPriority : string);
var
  SA: TSecurityAttributes;
  SI: TStartupInfo;
  PI: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  StdErrPipeRead, StdErrPipeWrite: THandle;
  WasOK: Boolean;
  Buffer: array[0..255] of Char;
  BytesRead: Cardinal;
  Handle: Boolean;
  Priority : Cardinal;

begin
  strOut := '';
  strErr := '';

  with SA do begin
    nLength := SizeOf(SA);
    bInheritHandle := True;
    lpSecurityDescriptor := nil;
  end;
  CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0);
  CreatePipe(StdErrPipeRead, StdErrPipeWrite, @SA, 0);
  try
    with SI do
    begin
      FillChar(SI, SizeOf(SI), 0);
      cb := SizeOf(SI);
      dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      wShowWindow := ShowCmd;
      hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
      hStdOutput := StdOutPipeWrite;
      hStdError := StdErrPipeWrite;
    end;

    if sScannerPriority = 'LOW' then
      Priority := IDLE_PRIORITY_CLASS
    else
      Priority := NORMAL_PRIORITY_CLASS;

    Handle := CreateProcess(PChar(sApplication), PChar(sCommandLine),
                            nil, nil, True, Priority,
                            nil, nil, SI, PI);
    CloseHandle(StdOutPipeWrite);
    CloseHandle(StdErrPipeWrite);
    if Handle then
      try
        repeat
          WasOK := ReadFile(StdOutPipeRead, Buffer, 255, BytesRead, nil);
          if BytesRead > 0 then
          begin
            Buffer[BytesRead] := #0;
            strOut := strOut + Buffer;
          end;
        until not WasOK or (BytesRead = 0);

        repeat
          WasOK := ReadFile(StdErrPipeRead, Buffer, 255, BytesRead, nil);
          if BytesRead > 0 then
          begin
            Buffer[BytesRead] := #0;
            strErr := strErr + Buffer;
          end;
        until not WasOK or (BytesRead = 0);

        WaitForSingleObject(PI.hProcess, INFINITE);
      finally
        CloseHandle(PI.hThread);
        CloseHandle(PI.hProcess);
      end;
  finally
    CloseHandle(StdOutPipeRead);
    CloseHandle(StdErrPipeRead);
  end;
end;

end.
