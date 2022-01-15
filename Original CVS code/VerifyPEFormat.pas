unit VerifyPEFormat;

interface

uses Windows, Classes, SysUtils, ImageHlp, Math, Sign, Utility, Sentinel;

const IMAGE_FILE_MACHINE_IA64      = $0200; // Intel 64
      IMAGE_FILE_MACHINE_AMD64     = $8664; // AMD64 (K8)
      IMAGE_FILE_MACHINE_ALPHA64   = $0284; // ALPHA64

      INFINITY_POINTS = 9999;

type
  PImageOptionalHeader64 = ^TImageOptionalHeader64;
  _IMAGE_OPTIONAL_HEADER64 = packed record
    { Standard fields. }
    Magic: Word;
    MajorLinkerVersion: Byte;
    MinorLinkerVersion: Byte;
    SizeOfCode: DWORD;
    SizeOfInitializedData: DWORD;
    SizeOfUninitializedData: DWORD;
    AddressOfEntryPoint: DWORD;
    BaseOfCode: DWORD;
    { NT additional fields. }
    ImageBase: Int64;
    SectionAlignment: DWORD;
    FileAlignment: DWORD;
    MajorOperatingSystemVersion: Word;
    MinorOperatingSystemVersion: Word;
    MajorImageVersion: Word;
    MinorImageVersion: Word;
    MajorSubsystemVersion: Word;
    MinorSubsystemVersion: Word;
    Win32VersionValue: DWORD;
    SizeOfImage: DWORD;
    SizeOfHeaders: DWORD;
    CheckSum: DWORD;
    Subsystem: Word;
    DllCharacteristics: Word;
    SizeOfStackReserve: Int64;
    SizeOfStackCommit: Int64;
    SizeOfHeapReserve: Int64;
    SizeOfHeapCommit: Int64;
    LoaderFlags: DWORD;
    NumberOfRvaAndSizes: DWORD;
    DataDirectory: packed array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1] of TImageDataDirectory;
  end;
  TImageOptionalHeader64 = _IMAGE_OPTIONAL_HEADER64;
  IMAGE_OPTIONAL_HEADER64 = _IMAGE_OPTIONAL_HEADER64;

function IsPEFormat(const sFileName : string;
   var Size : Cardinal;
   var Sections : Word;
   var Checksum : Cardinal;
   var bSectionHeaderOK : boolean;
   var bCheckEntropy : boolean;
   var iBadPoints : Cardinal;
   var bBadNTHeader : boolean;
   var bHasEmbeddedSignature : boolean;
   var FileType : TFileTypeDetected;
   var bBadHeaderTimeStamp : boolean;
   var PointsForAllFiles : Cardinal;
   var bHasRsrcSection : boolean) : boolean;

function ComputePEChecksum(FileName: string): DWORD;

procedure CalcEntropyFile(const sFilename : string; var ent : double;
                                                          var perc : double);

procedure IncBad(var iBadPoints : Cardinal; const i : Cardinal = 1);

function IsExecutable(const sFilename: string): TFileTypeDetected;

implementation

procedure IncBad(var iBadPoints : Cardinal; const i : Cardinal = 1);
begin
  iBadPoints := iBadPoints + i;
end;

function IsPEFormat(const sFileName : string;
   var Size : Cardinal;
   var Sections : Word;
   var Checksum : Cardinal;
   var bSectionHeaderOK : boolean;
   var bCheckEntropy : boolean;
   var iBadPoints : Cardinal;
   var bBadNTHeader : boolean;
   var bHasEmbeddedSignature : boolean;
   var FileType : TFileTypeDetected;
   var bBadHeaderTimeStamp : boolean;
   var PointsForAllFiles : Cardinal;
   var bHasRsrcSection : boolean) : boolean;

//const IMAGE_OS2_SIGNATURE_LX = $584C; { LX }
var
  fs: TFilestream;
  dos_header: IMAGE_DOS_HEADER;
  pe_header: IMAGE_FILE_HEADER;
  opt_header: IMAGE_OPTIONAL_HEADER;
  opt_header64: IMAGE_OPTIONAL_HEADER64;
  signature: DWORD;
//  short_signature : DWORD;

  TotCodeSection : Cardinal;
  TotInitializedDataSection : Cardinal;
  TotUninitializedDataSection : Cardinal;
  GoodSections : boolean;
  bCodeSectionSizeOK : boolean;
  bIsWin9xMe : boolean;
  Machine : Word;
  bIs64Image : boolean;
  sExt : string;

  Year, Month, Day: Word;

     procedure GetPESectionHeader(var fs: TFilestream; const sFilename : string;
         const nSections : Word;
         var TotCodeSection : Cardinal;
         var TotInitializedDataSection : Cardinal;
         var TotUninitializedDataSection : Cardinal;
         var GoodSections : boolean;
         var CheckEntropy : boolean;
         var iBadPoints : Cardinal;
         var bHasRsrcSection : boolean);
     var
       sec_header : IMAGE_SECTION_HEADER;
       j : integer;
       Len : DWORD;
       bFoundDataSection : boolean;

       function Check(const Characteristic : DWord; const Value : DWord) : boolean;
       begin
         result := ((Characteristic and Value) = Value);
       end;

     begin
       GoodSections := true;
       bFoundDataSection:=false;
       bHasRsrcSection := not(((FileType = IsExe) or (Filetype = IsDll))
           and IsExtInList(UpperCase(sFilename),FClamSentinel.lstExtToMonitor));

       TotCodeSection := 0;
       TotInitializedDataSection := 0;
       TotUninitializedDataSection := 0;

       for j:= 0 to nSections-1 do
       begin
         FillChar(sec_header,0,sizeof(sec_header));

         fs.read(sec_header, SizeOf(sec_header));

         Len := sec_header.SizeOfRawData;

         if sec_header.Characteristics = 0 then
         begin
           GoodSections := false;
           IncBad(iBadPoints,3);
         end;

         if sec_header.PointerToLinenumbers <> 0 then
           IncBad(iBadPoints);

         if Check(sec_header.Characteristics, IMAGE_SCN_CNT_CODE) then
         begin
           //Code section
           TotCodeSection := TotCodeSection + Len;

           if not((FileType = IsSys) or (FileType = IsDll)) then
           begin
             if Check(sec_header.Characteristics, IMAGE_SCN_MEM_WRITE) then
             begin
               CheckEntropy := true;

               IncBad(iBadPoints);
             end;

             if bFoundDataSection and (j = nSections-1) then
             begin
               GoodSections := false;
               IncBad(iBadPoints,3);
             end;
           end;
         end;

         if Check(sec_header.Characteristics, IMAGE_SCN_CNT_INITIALIZED_DATA) then
         begin
           //Initialized data section
           TotInitializedDataSection := TotInitializedDataSection + Len;

           if ((FileType = IsExe) or (Filetype = IsDll))
               and ((sec_header.Name[0] = 46)      and (sec_header.Name[1] = 114)
               and (sec_header.Name[2] = 115) and (sec_header.Name[3] = 114)
               and (sec_header.Name[4] = 99)  and (sec_header.Name[5] = 0)
               and (sec_header.Name[6] = 0)   and (sec_header.Name[7] = 0)) then //.rsrc
           begin
             bHasRsrcSection := true;

             if (FileType = IsExe)
                   and (Check(sec_header.Characteristics, IMAGE_SCN_MEM_EXECUTE)) then
             begin
               GoodSections := false;
               PointsForAllFiles := INFINITY_POINTS;
               IncBad(iBadPoints,3);
             end;
           end;

           bFoundDataSection := true;
         end;

         if Check(sec_header.Characteristics, IMAGE_SCN_CNT_UNINITIALIZED_DATA) then
         begin
           //Uninitialized data section
           TotUninitializedDataSection := TotUninitializedDataSection + Len;

           bFoundDataSection := true;
         end;
       end;

       if not bHasRsrcSection then
         begin
           GoodSections := false;
           CheckEntropy := true;
           IncBad(iBadPoints);
         end;
     end;

begin
  Result := false;
  bSectionHeaderOK := true;
  bBadHeaderTimeStamp := false;
  bCheckEntropy := false;
  bBadNTHeader := false;
  bHasEmbeddedSignature := false;
  FileType := IsUndefined;
  bIsWin9xMe := (Win32Platform = VER_PLATFORM_WIN32_WINDOWS);

  try
    fs := TFilestream.Create(sFileName, fmOpenread or fmShareExclusive);

    try
      Size := fs.Size;

      sExt := UpperCase(ExtractFileExt(sFilename));

      if sExt = '.EXE' then
        FileType := IsExe;

      if Size > (SizeOf(dos_header) + SizeOf(pe_header)) then
      begin
        FillChar(dos_header,0,sizeof(dos_header));
        fs.read(dos_header, SizeOf(dos_header));

        if dos_header.e_magic = IMAGE_DOS_SIGNATURE then
        begin
          fs.seek(dos_header._lfanew, soFromBeginning);
          fs.read(signature, SizeOf(signature));

          if signature = IMAGE_NT_SIGNATURE then
            begin
              Result := true;

              FillChar(pe_header,0,sizeof(pe_header));
              fs.read(pe_header, SizeOf(pe_header));

              Machine := pe_header.Machine;
              bIs64Image := ((Machine = IMAGE_FILE_MACHINE_IA64) or (Machine = IMAGE_FILE_MACHINE_AMD64)
                   or (Machine = IMAGE_FILE_MACHINE_ALPHA64));

              Sections := pe_header.NumberOfSections;

              if ((IMAGE_FILE_EXECUTABLE_IMAGE and pe_header.Characteristics) <> 0) then
                FileType := IsExe;

              if ((IMAGE_FILE_DLL and pe_header.Characteristics) <> 0) then
                FileType := IsDll;

              try
                DecodeDate(EncodeDate(1970, 1, 1) + pe_header.Timedatestamp / SecsPerDay, Year, Month, Day);

                if Year > 2040 then
                  bBadHeaderTimeStamp := true;
              except
                bBadHeaderTimeStamp := true;
              end;

              //if ((IMAGE_FILE_BYTES_REVERSED_HI and pe_header.Characteristics) <> 0)
              //    or ((IMAGE_FILE_BYTES_REVERSED_LO and pe_header.Characteristics) <> 0)
              //    or ((IMAGE_FILE_RELOCS_STRIPPED and pe_header.Characteristics) <> 0) then
              if ((IMAGE_FILE_RELOCS_STRIPPED and pe_header.Characteristics) <> 0) then
                IncBad(iBadPoints);

              if sExt = '.SYS' then
                FileType := IsSys;

              if pe_header.SizeOfOptionalHeader > 0 then
              begin
                if bIs64Image then
                  begin
                    FillChar(opt_header64,0,sizeof(opt_header64));
                    fs.read(opt_header64, SizeOf(opt_header64));

                    Checksum := opt_header64.CheckSum;
                    bHasEmbeddedSignature := (opt_header64.DataDirectory[IMAGE_DIRECTORY_ENTRY_SECURITY].Size > 0);

                    if not((FileType = IsDll) or (FileType = IsSys)) then
                    begin
                      bSectionHeaderOK :=
                        (opt_header64.AddressOfEntryPoint
                              <= opt_header64.BaseOfCode + opt_header64.SizeOfCode)
                        and (opt_header64.AddressOfEntryPoint >= opt_header64.BaseOfCode)
                    end;
                  end
                else
                  begin
                    FillChar(opt_header,0,sizeof(opt_header));
                    fs.read(opt_header, SizeOf(opt_header));

                    Checksum := opt_header.CheckSum;
                    bHasEmbeddedSignature := (opt_header.DataDirectory[IMAGE_DIRECTORY_ENTRY_SECURITY].Size > 0);

                    if not((FileType = IsDll) or (FileType = IsSys)) then
                    begin
                      bSectionHeaderOK :=
                        (opt_header.AddressOfEntryPoint
                              <= opt_header.BaseOfCode + opt_header.SizeOfCode)
                        and (opt_header.AddressOfEntryPoint >= opt_header.BaseOfCode)
                    end;
              end;

                  end;

              if bSectionHeaderOK then
                begin
                  GetPESectionHeader(fs, sFilename, Sections, TotCodeSection,
                     TotInitializedDataSection, TotUninitializedDataSection, GoodSections,
                     bCheckEntropy, iBadPoints, bHasRsrcSection);

                  bSectionHeaderOK := GoodSections;

                  if pe_header.SizeOfOptionalHeader > 0 then
                  begin
                    if bIs64Image then
                      begin
                        if opt_header64.NumberOfRvaAndSizes <> 16 then
                          IncBad(iBadPoints);

                        bCodeSectionSizeOK := (TotCodeSection = opt_header64.SizeOfCode);
                      end
                    else
                      begin
                        if opt_header.NumberOfRvaAndSizes <> 16 then
                          IncBad(iBadPoints);

                        bCodeSectionSizeOK := (TotCodeSection = opt_header.SizeOfCode);
                      end;

                    bSectionHeaderOK := bSectionHeaderOK and bCodeSectionSizeOK;

                    if not(bCodeSectionSizeOK) then
                    begin
                      bCheckEntropy := true;

                      if not bIsWin9xMe then
                        IncBad(iBadPoints);
                    end;
                  end;
                end
              else
                IncBad(iBadPoints,3);
            end
  //        else
  //          begin
  //            short_signature := signature and $FFFF;
  //            if not((short_signature = IMAGE_VXD_SIGNATURE)
  //                    or (short_signature = IMAGE_OS2_SIGNATURE)
  //                    or (short_signature = IMAGE_OS2_SIGNATURE_LE)
  //                    or (short_signature = IMAGE_DOS_SIGNATURE)
  //                    or (short_signature = IMAGE_OS2_SIGNATURE_LX)) then
  //              bBadNTHeader := true;
  //          end;
          end
        else
          begin
            //not IMAGE_DOS_SIGNATURE
            if FileType = IsExe then
            begin
              if not( (dos_header.e_magic = $4150)
                     and (dos_header.e_cblp = $3033) ) then
              begin
                //bBadNTHeader := true;
                //IncBad(iBadPoints);
                Result := false;
              end
            end;
          end;
      end;
    finally
      fs.Free;
    end;
  except
  end;
end;

function ComputePEChecksum(FileName: string): DWORD;
var
  h, hMap: Cardinal;
  pMem: Pointer;
  headersum, checksum, fsizehigh, fsizelow: DWORD;
  nth: PImageNtHeaders;
begin
  pMem := nil;
  h := INVALID_HANDLE_VALUE;
  hMap := 0;

  Result := 0;

  headersum := 0;
  checksum  := 0;
  try
    h := Windows.CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ,
      nil, OPEN_EXISTING, 0, 0);

    if (h = INVALID_HANDLE_VALUE) then
      exit;

    fsizelow := Windows.GetFileSize(h, Pointer(@fsizehigh));

    hMap := Windows.CreateFileMapping(h, nil, PAGE_READONLY, fsizeHigh, fsizeLow, nil);
    if (hMap <> 0) then
    begin
      pMem := Windows.MapViewOfFile(hMap, FILE_MAP_READ, 0, 0, 0);
      if (pMem <> nil) then
      begin
        nth := CheckSumMappedFile(pMem, fsizeLow, @headersum, @checksum);

        if (nth = nil) then
          checksum := 0;
      end;
    end;
  finally
    if (pMem <> nil) then
      Windows.UnmapViewOfFile(pMem);
    if (hMap <> 0) then
      Windows.CloseHandle(hMap);
    if (h <> INVALID_HANDLE_VALUE) then
      Windows.CloseHandle(h);
  end;

  Result := checksum;
end;

procedure CalcEntropyFile(const sFilename : string; var ent : double;
                                                          var perc : double);
var
  aFreq : array[0..255] of Cardinal;
  aBuffer : array[0..255] of byte;

  nRead : integer;

  p : Double;
  Entropy : Extended;
  i : Cardinal;

  F : File;
  Size : Cardinal;

  procedure ReadBytes;
    var
      i : integer;
  begin
    nRead := 0;

    if not Eof(F) then
    begin
      FillChar(aBuffer,Length(aBuffer),0);
      BlockRead(F, aBuffer, Length(aBuffer), nRead);

      for i := 0 to nRead-1 do
        aFreq[aBuffer[i]] := aFreq[aBuffer[i]] + 1;
    end;
  end;
begin
  ent := 0;
  perc := 0;

  Entropy := 0;

if CheckFileExists(sFileName) then
begin
  try
    for i := 0 to 255 do
      aFreq[i] := 0;

    AssignFile(F, sFileName);

    try
      Reset(F,1);
      Size := FileSize(F);

      while not Eof(F) do
      begin
        ReadBytes;
      end;
    finally
      CloseFile(F);
    end;

    for i := 0 to 255 do
    begin
      if Size > 0 then
        p := aFreq[i] / Size
      else
        p := 0;

      if (p > 0) then Entropy := Entropy - p*log2(p);
    end;

    ent := Entropy;
    perc := (100*ent)/8;
  except
  end;
end;
end;

function IsExecutable(const sFilename: string): TFileTypeDetected;
var
  fs: TFilestream;
  dos_header: IMAGE_DOS_HEADER;
  pe_header: IMAGE_FILE_HEADER;
  signature: DWORD;
  Size : Cardinal;
begin
  result := IsUndefined;

  try
    fs := TFilestream.Create(sFileName, fmOpenread or fmShareDenyNone); //fmShareExclusive);

    try
      Size := fs.Size;

      if Size > (SizeOf(dos_header) + SizeOf(pe_header)) then
      begin
        FillChar(dos_header,0,sizeof(dos_header));
        fs.read(dos_header, SizeOf(dos_header));

        if dos_header.e_magic = IMAGE_DOS_SIGNATURE then
        begin
          fs.seek(dos_header._lfanew, soFromBeginning);
          fs.read(signature, SizeOf(signature));

          if signature = IMAGE_NT_SIGNATURE then
          begin
            FillChar(pe_header,0,sizeof(pe_header));
            fs.read(pe_header, SizeOf(pe_header));

            if ((IMAGE_FILE_EXECUTABLE_IMAGE and pe_header.Characteristics) <> 0) then
              result := IsExe;

            if ((IMAGE_FILE_DLL and pe_header.Characteristics) <> 0) then
              result := IsDll;
          end;
        end;
      end;
    finally
      fs.Free;
    end;
  except
  end;
end;




end.
