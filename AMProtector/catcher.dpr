
// {$IMAGEBASE $10000000}

{$A2}

(**************************************************************************************

      AMProtector - I AM protector - simple and strong exe protector project

      # Strong LZMA compression
      # Strong AMPRNG encryption
      # Very simple algorithm

      Original idea by Alexander Myasnikow

      www.darksoftware.narod.ru

      e-mail: darksoftware@ya.ru

      Use "FACTORY" define to build compressor and "CATCHER" define to build PE loader


    Copyright (C) 2009  Alexander Myasnikow

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.


***************************************************************************************)

program protector;

uses
 {$IFDEF FACTORY}
  Classes,  {$ENDIF}
  Windows,
  amprng in 'amprng.pas',
  lzma in 'lzma\lzma.pas',
  lzmaBaseU in 'lzma\lzmaBaseU.pas',
  lzmaCommonU in 'lzma\lzmaCommonU.pas',
 {$IFDEF CATCHER}
  lzmaDecoderU in 'lzma\lzmaDecoderU.pas',  {$ENDIF}
  lzmadestreamu in 'lzma\lzmadestreamu.pas' {$IFDEF FACTORY},
  lzmaEncoderU in 'lzma\lzmaEncoderU.pas',
  unitNTModule in 'unitNTModule.pas',
  unitResourceDetails in 'unitResourceDetails.pas'  {$ENDIF};


// Set this to any large value to avoid range check errors in debug mode


const
  MAX_SECTIONS = 0;


type
  TSections = array [0..MAX_SECTIONS] of TImageSectionHeader;


const
  fmOpenRead  = $0000;
  fmOpenWrite = $0001;
  fmOpenReadWrite = $0002;
  fmShareDenyNone = $0040;

  function StrScan (const Str: PChar; Chr: char): PChar;
    begin
      Result := Str;
      while Result^ <> Chr do
        begin
        if Result^ = #0 then
          begin
          Result := nil;
          Exit;
          end;
        Inc(Result);
        end;
    end;

  function LastDelimiter (const Delimiters, S: string): integer;
    var
      P: PChar;
    begin
      Result := Length(S);
      P := PChar(Delimiters);
      while Result > 0 do
        begin
        if (S[Result] <> #0) and (StrScan(P, S[Result]) <> nil) then
          Exit;
        Dec(Result);
        end;
    end;


  function ExtractFilePath (const FileName: string): string;
    var
      I: integer;
    begin
      I := LastDelimiter('\:', FileName);
      Result := Copy(FileName, 1, I);
    end;


  function FileOpen (const FileName: string; Mode: longword): integer;
    const
      AccessMode: array[0..2] of longword = (
        GENERIC_READ,
        GENERIC_WRITE,
        GENERIC_READ or GENERIC_WRITE);
      ShareMode: array[0..4] of longword = (
        0,
        0,
        FILE_SHARE_READ,
        FILE_SHARE_WRITE,
        FILE_SHARE_READ or FILE_SHARE_WRITE);
    begin
      Result := -1;
      if ((Mode and 3) <= fmOpenReadWrite) and ((Mode and $F0) <= fmShareDenyNone) then
        Result := integer(CreateFile(PChar(FileName), AccessMode[Mode and 3],
          ShareMode[(Mode and $F0) shr 4], nil, OPEN_EXISTING,
          FILE_ATTRIBUTE_NORMAL, 0));
    end;

  function FileRead (Handle: integer; var Buffer; Count: longword): integer;
    begin
      if not ReadFile(THandle(Handle), Buffer, Count, longword(Result), nil) then
        Result := -1;
    end;

  function FileWrite (Handle: integer; const Buffer; Count: longword): integer;
    begin
      if not WriteFile(THandle(Handle), Buffer, Count, longword(Result), nil) then
        Result := -1;
    end;

  function FileSeek (Handle, Offset, Origin: integer): integer;
    begin
      Result := SetFilePointer(THandle(Handle), Offset, nil, Origin);
    end;


  {$IFDEF FACTORY}

  {$R UPX.res}

var
  HConsoleInput: THandle;

  function ReadKey: char;
  var
    LReadCount: DWORD;
    LInputRec: TInputRecord;
    LKey: char;
  begin
    repeat
      ReadConsoleInput(HConsoleInput, LInputRec, 1, LReadCount);
      if (LInputRec.EventType = KEY_EVENT) and
        (LInputRec.Event.KeyEvent.bKeyDown) then
      begin
        LKey := char(LInputRec.Event.KeyEvent.wVirtualKeyCode);

        // Ignore system control keys ... Shift, Ctrl, Alt, Caps Lock
        if (LInputRec.Event.KeyEvent.wVirtualKeyCode = 16) or
          (LInputRec.Event.KeyEvent.wVirtualKeyCode = 17) or
          (LInputRec.Event.KeyEvent.wVirtualKeyCode = 18) or
          (LInputRec.Event.KeyEvent.wVirtualKeyCode = 20) then
          LKey := #0;

        if (LKey >= 'A') and (LKey <= 'Z') and
          ((LInputRec.Event.KeyEvent.dwControlKeyState and SHIFT_PRESSED) = 0) then
          Dec(LKey, Ord('a') - Ord('A'));   // DownCase(LKey);
      end
      else
        LKey := #0;

    until LKey <> #0;

    Result := LKey;
  end;


  function GetPass: string;
  var
    s: string;
    c: char;
  begin
    AllocConsole();

    HConsoleInput := GetStdHandle(STD_INPUT_HANDLE);

    writeln('AMProtector');
    writeln('(c) 2009 Original idea by A.Myasnikow');
    writeln('Enter password: ');
    s := '';
    repeat

      c := readkey;

      if c <> #13 then
      begin
        s := s + c;
        Write('*');
      end;

    until c = #13;

    Result := s;

    FreeConsole;
  end;


  {$ENDIF}


  // This function calculates the aligned size of a section.
  function GetAlignedSize (Size: DWORD; Alignment: DWORD): DWORD;
    begin
      if ((Size mod Alignment) = 0) then
        Result := Size
      else
        Result := ((Size div Alignment) + 1) * Alignment;
    end;

  function ImageSize (Image: pointer): dword;
    var
      Alignment: dword;
      ImageNtHeaders: PImageNtHeaders;
      PSections: ^TSections;
      SectionLoop: dword;
    begin
      ImageNtHeaders := pointer(dword(Image) + dword(PImageDosHeader(Image)._lfanew));
      Alignment := ImageNtHeaders.OptionalHeader.SectionAlignment;
      if ((ImageNtHeaders.OptionalHeader.SizeOfHeaders mod Alignment) = 0) then
        begin
        Result := ImageNtHeaders.OptionalHeader.SizeOfHeaders;
        end
      else
        begin
        Result := ((ImageNtHeaders.OptionalHeader.SizeOfHeaders div Alignment) + 1) *
          Alignment;
        end;
      PSections := pointer(DWORD(@ImageNtHeaders.OptionalHeader) +
        ImageNtHeaders.FileHeader.SizeOfOptionalHeader);
      for SectionLoop := 0 to ImageNtHeaders.FileHeader.NumberOfSections - 1 do
        begin
        if PSections[SectionLoop].Misc.VirtualSize <> 0 then
          begin
          if ((PSections[SectionLoop].Misc.VirtualSize mod Alignment) = 0) then
            begin
            Result := Result + PSections[SectionLoop].Misc.VirtualSize;
            end
          else
            begin
            Result := Result + (((PSections[SectionLoop].Misc.VirtualSize div Alignment) +
              1) * Alignment);
            end;
          end;
        end;
    end;

type
  TProcInfo = packed record
    BaseAddr:  DWORD;
    ImageSize: DWORD;
    end;

  function ZwUnmapViewOfSection (ProcessHandle: THandle; BaseAddress: Pointer): DWORD;
  stdcall; external 'ntdll.dll'


  function CreateProcessEx (FileMemory: Pointer; CMD: PChar; wait: boolean): BOOL;
    var
      Data: DWORD;
      HeaderSize: DWORD;
      InjectSize: DWORD;
      SectionLoop: DWORD;
      SectionSize: DWORD;
      CurAddr: DWORD;
      V: Pointer;
      FileData: Pointer;
      InjectMemory: Pointer;
      OldProtect: cardinal;
      Context: TContext;
      ProcInfo2: TProcInfo;
      PSections: ^TSections;
      StartInfo: TStartupInfo;
      ImageNtHeaders: PImageNtHeaders;
      ProcInfo: TProcessInformation;
      MemInfo: MEMORY_BASIC_INFORMATION;
      hToken: THandle;
      SeDebugNameValue: int64;
      tkp: TOKEN_PRIVILEGES;
      ReturnLength: cardinal;
      CMem: pointer;
    begin
      Result := False;
      ImageNtHeaders := Pointer(DWORD(FileMemory) +
        DWORD(PImageDosHeader(FileMemory)._lfanew));
      InjectSize := ImageSize(FileMemory);
      InjectMemory := VirtualAlloc(nil, InjectSize, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
        try
        FileData  := InjectMemory;
        HeaderSize := ImageNtHeaders.OptionalHeader.SizeOfHeaders;
        PSections := Pointer(PChar(@(ImageNtHeaders.OptionalHeader)) +
          ImageNtHeaders.FileHeader.SizeOfOptionalHeader);

        // certain PE files have sectionHeaderSize value > size of PE file itself.
        // this loop handles this situation by find the section that is nearest to
        // the PE header.

        for SectionLoop := 0 to ImageNtHeaders.FileHeader.NumberOfSections - 1 do
          if PSections[SectionLoop].PointerToRawData < HeaderSize then
            HeaderSize := PSections[SectionLoop].PointerToRawData;

        // read the PE header

        CopyMemory(FileData, FileMemory, HeaderSize);
        FileData := Pointer(DWORD(FileData) + GetAlignedSize(
          ImageNtHeaders.OptionalHeader.SizeOfHeaders,
          ImageNtHeaders.OptionalHeader.SectionAlignment));

        // read the sections

        for SectionLoop := 0 to ImageNtHeaders.FileHeader.NumberOfSections - 1 do
          begin
          if PSections[SectionLoop].SizeOfRawData > 0 then
            begin
            SectionSize := PSections[SectionLoop].SizeOfRawData;
            if SectionSize > PSections[SectionLoop].Misc.VirtualSize then
              SectionSize := PSections[SectionLoop].Misc.VirtualSize;
            CopyMemory(FileData, Pointer(DWORD(FileMemory) +
              PSections[SectionLoop].PointerToRawData),
              SectionSize);
            FileData := Pointer(DWORD(FileData) + GetAlignedSize(
              PSections[SectionLoop].Misc.VirtualSize,
              ImageNtHeaders.OptionalHeader.SectionAlignment));
            end
          else

          // This handles the case where the PE file has an empty section. E.g. UPX0
          // section in UPXed files.

            if PSections[SectionLoop].Misc.VirtualSize <> 0 then
              FileData := Pointer(DWORD(FileData) + GetAlignedSize(
                PSections[SectionLoop].Misc.VirtualSize,
                ImageNtHeaders.OptionalHeader.SectionAlignment));
          end;
        ZeroMemory(@StartInfo, SizeOf(StartupInfo));
        ZeroMemory(@Context, SizeOf(TContext));
        StartInfo.cb := SizeOf(StartupInfo);

        // Creates the original EXE in suspended mode and returns its info in the
        // PROCINFO structure.


        ///
        if not OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES or
          TOKEN_QUERY, hToken) then
          exit;

        // Получаем LUID привилегии
        if not LookupPrivilegeValue(nil, 'SeDebugPrivilege', SeDebugNameValue) then
          begin
          CloseHandle(hToken);
          exit;
          end;

        tkp.PrivilegeCount := 1;
        tkp.Privileges[0].Luid := SeDebugNameValue;
        tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;

        // Добавляем привилегию к нашему процессу
        AdjustTokenPrivileges(hToken, False, tkp, SizeOf(tkp), tkp, ReturnLength);
        if GetLastError() <> ERROR_SUCCESS then
          exit;
        ///


        if CreateProcess(nil, CMD, nil, nil, True, CREATE_SUSPENDED,
          nil, nil, StartInfo, ProcInfo) then
          begin

          // This is to fix aligment bug when working in Vista/Win7 

          GetMem(CMem, SizeOf(_Context) shl 1);
          ZeroMemory(Cmem, SizeOf(_Context) shl 1);
          _Context(CMem^).ContextFlags := CONTEXT_FULL;
          GetThreadContext(ProcInfo.hThread, _Context(Cmem^));
          CopyMemory(@Context, CMem, SizeOf(_Context));
          FreeMem(CMem);

          ReadProcessMemory(ProcInfo.hProcess, Pointer(Context.Ebx + 8),
            @ProcInfo2.BaseAddr, SizeOf(DWORD), Data);
          CurAddr := procinfo2.BaseAddr;
          while (VirtualQueryEx(ProcInfo.hProcess, Pointer(curAddr),
              MemInfo, SizeOf(MemInfo)) <> 0) do
            begin
            if MemInfo.State = MEM_FREE then
              Break;
            CurAddr := CurAddr + MemInfo.RegionSize;
            end;
          ProcInfo2.ImageSize := DWORD(CurAddr) - DWORD(procinfo2.BaseAddr);

          // if new EXE has same baseaddr and is its size is <= to the original EXE,
          // just overwrite it in memory

          V := nil;
          if (Injectsize <= ProcInfo2.ImageSize) and
            (ImageNtHeaders.OptionalHeader.ImageBase = ProcInfo2.BaseAddr) then
            begin
            V := Pointer(ProcInfo2.baseAddr);
            VirtualProtectEx(ProcInfo.hProcess, Pointer(ProcInfo2.BaseAddr),
              ProcInfo2.ImageSize, PAGE_EXECUTE_READWRITE, @OldProtect);
            end
          else
            begin

            // try to unmap the original EXE image

            if (ZwUnmapViewOfSection(Procinfo.hProcess,
              Pointer(ProcInfo2.baseAddr))) = 0 then
              begin
              // allocate memory for the new EXE image at the prefered imagebase.
              V := VirtualAllocEx(ProcInfo.hProcess,
                Pointer(ImageNtHeaders.OptionalHeader.ImageBase), InjectSize,
                MEM_RESERVE or MEM_COMMIT, PAGE_EXECUTE_READWRITE);
              end;
            end;
          if (cardinal(V) > 0) then
            begin

            // patch the EXE base addr in PEB (PEB + 8 holds process base addr)

            WriteProcessMemory(ProcInfo.hProcess, Pointer(Context.Ebx + 8),
              @V, SizeOf(DWORD), Data);
            if WriteProcessMemory(ProcInfo.hProcess, V, InjectMemory,
              InjectSize, Data) then
              begin
              Context.ContextFlags := CONTEXT_FULL;
              if DWORD(V) = Procinfo2.BaseAddr then
                Context.Eax := DWORD(ImageNtHeaders.OptionalHeader.ImageBase) +
                  ImageNtHeaders.OptionalHeader.AddressOfEntryPoint
              else
                Context.Eax := DWORD(V) +
                  ImageNtHeaders.OptionalHeader.AddressOfEntryPoint;

              GetMem(CMem, SizeOf(_Context) shl 1);
              ZeroMemory(CMem, SizeOf(_Context) shl 1);
              CopyMemory(CMem, @Context, SizeOf(_Context));

              SetThreadContext(ProcInfo.hThread, _Context(CMem^));

              FreeMem(CMem);

              if ResumeThread(ProcInfo.hThread) <> longword(-1) then
                begin
                Result := True;
                if wait then
                  begin
                  WaitforSingleObject(ProcInfo.hProcess, INFINITE);
                  CloseHandle(ProcInfo.hProcess);
                  CloseHandle(ProcInfo.hThread);
                  end;
                end;
              end
            else
              begin
              TerminateProcess(ProcInfo.hProcess, 0);
              end;
            end;
          end;
        finally
        end;
      CloseHandle(ProcInfo.hProcess);
      tkp.Privileges[0].Attributes := 0;
      AdjustTokenPrivileges(hToken, False, tkp, SizeOf(tkp), tkp, ReturnLength);
    end;


  function FSize (fileName: PAnsiChar): integer;
    var
      SR: TWIN32FINDDATA;
      H:  cardinal;
    begin
      Windows.FindFirstFile(FileName, SR);

      H := FindFirstFile(fileName, sr);

      if (H <> INVALID_HANDLE_VALUE) and (H <> ERROR_NO_MORE_FILES) then
        Result := ((sr.nFileSizeHigh shl 32) + sr.nFileSizeLow)
      else
        Result := -1;

      Windows.FindClose(H);
    end;


var
  fbuff: array [0..16383] of byte;
const
  PRT_ID = $C0BA;

    {$IFDEF FACTORY}


  procedure CopyIco(f1, f2: string);
  var
    NTM:     TResourceModule;
    i, c:    integer;
    details: TResourceDetails;
  begin
    ntm := TNTModule.Create;
    ntm.LoadFromFile(f1);


    i := 0;

    repeat

      details := ntm.ResourceDetails[i];

      if (details.ResourceType <> '14') and (PChar(details.ResourceType) <> '3') then
      begin
        ntm.DeleteResource(i);
      end
      else
      begin
        Inc(i);
      end;

      c := ntm.ResourceCount;


    until i = c;


    ntm.SaveToFile(f2);
    ntm.Free;
  end;

  {$ENDIF}

  procedure SelfMade (fn: string; var key: array of byte);
    var
      F:  integer;
    var
      ka, kb: array [0..15] of byte;
      sb: array [0..255] of byte;
      I, N: integer;
      p, t: byte;
    begin

      f := FileOpen(fn, fmOpenRead + fmShareDenyNone);
      FileRead(f, fbuff, 16384);
      CloseHandle(f);


    {$I rnd.inc}


      for I := 0 to 255 do
        sb[i] := 255 - i;


      for I := 0 to 63 do
        key[i] := i;

      p := 64;

      for n := 0 to 768 do
        begin

        p := key[(key[(p + key[n mod 64]) mod 64] + ka[n mod 16]) mod 64] mod 64;

        t := key[n mod 64];
        key[n mod 64] := key[p];
        key[p] := t;

        end;


      for n := 0 to 768 do
        begin
        p := sb[(sb[(p + sb[n mod 256]) mod 256] + kb[n mod 16]) mod 256];
        t := sb[n mod 256];
        sb[n mod 256] := sb[p];
        sb[p] := t;
        end;


      for I := 0 to 16383 do
        key[i mod 64] := sb[(key[i mod 64] + fbuff[i]) mod 256];

    end;


var
  paramrec: record
    id:  word;
    ffs: longint;
    key: array [0..63] of byte;
    offset: longint;
    end;

{$IFDEF CATCHER}

var
  Buffer: Pointer;


  procedure Catch;
  var
    F, size: longint;
    tk:      array [0..63] of byte;

    am: TAMPRNG;
  begin

    SelfMade(ParamStr(0), tk);

    size := FSize(PAnsiChar(ParamStr(0)));
    f    := FileOpen(ParamStr(0), fmOpenRead + fmShareDenyNone);
    FileSeek(f, 0, 2);
    FileSeek(f, -SizeOf(paramrec), 1);
    FileRead(f, paramrec, SizeOf(paramrec));

    am := TAMPRNG.Create(tk, 64, False, 768);
    am.Crypt(@paramrec, SizeOf(paramrec));
    am.Free;

    if paramrec.id <> PRT_ID then
    begin

      CloseHandle(f);

      MessageBox(GetDeskTopWindow, 'File is damaged!', 'Access denied',
        MB_OK + MB_ICONEXCLAMATION + MB_SYSTEMMODAL);
    end
    else
    begin

      GetMem(Buffer, paramrec.ffs);
      FileSeek(f, paramrec.offset, 0);
      FileRead(f, Buffer^, size - (paramrec.offset + SizeOf(paramrec)));
      CloseHandle(f);

      am := TAMPRNG.Create(paramrec.key, 64, False, 768);

      am.Crypt(Buffer, size - (paramrec.offset + SizeOf(paramrec)));

      am.Free;

      if (byte(Buffer^) = byte(93)) and (byte(Pointer(longword(Buffer) + 1)^) =
        byte(0)) then
      begin

        {r := }lzmadcomp(Buffer, size - (paramrec.offset + SizeOf(paramrec)));


        if (byte(Buffer^) = byte('M')) and
          (byte(Pointer(longword(Buffer) + 1)^) = byte('Z')) then
        begin
          CreateProcessEx(Buffer, GetCommandLine, false);
        end
        else
        begin
          MessageBox(GetDeskTopWindow, 'Decompression failed!', 'Access denied',
            MB_OK + MB_ICONEXCLAMATION + MB_SYSTEMMODAL);
        end;

      end
      else
      begin

        MessageBox(GetDeskTopWindow, 'Deciphering failed!', 'Access denied',
          MB_OK + MB_ICONEXCLAMATION + MB_SYSTEMMODAL);

      end;


      FreeMem(Buffer);
    end;

  end;

  {$ENDIF CATCHER}


 {$IFDEF FACTORY}

var
  Buff: Pointer;

  procedure UPXruntime(fn: string);
  var
    RS: TResourceStream;
  begin
    RS := TResourceStream.Create(HINSTANCE, 'UPX', 'RCUPX');
    RS.Position := 0;
    CreateProcessEx(RS.Memory, PChar(
      '--strip-relocs=1 --compress-icons=2 --best --overlay=strip ' + fn), true);
    RS.Free;

  end;


  procedure Inject(fromf, tof: string);
  var
    F, T, r: longint;
    AM:      TAMPRNG;
    passwd:  string;
    tk:      array [0..63] of byte;
  begin

    passwd := GetPass;

    if passwd = '' then
      exit;

    ExpandUserKey(passwd, paramrec.key, 64);
    am := TAMPRNG.Create(paramrec.key, 64, False, 768);

    DeleteFile(PChar(tof));
    CopyFile(PAnsiChar(ExtractFilePath(ParamStr(0)) + 'catcher.exe'),
      PAnsiChar(tof), False);

    CopyIco(fromf, tof);

    Upxruntime(tof);

    paramrec.offset := FSize(PChar(tof));

    SelfMade(tof, tk);

    T := FileOpen(tof, fmOpenReadWrite + fmShareDenyNone);
    FileSeek(T, paramrec.offset, 0);


    paramrec.ffs := FSize(PChar(fromf));
    GetMem(Buff, paramrec.ffs * 3);

    F := FileOpen(fromf, fmOpenRead + fmShareDenyNone);
    FileSeek(f, 0, 0);
    FileRead(F, Buff^, paramrec.ffs);
    CloseHandle(F);

    r := lzma.lzmacomp(Buff, paramrec.ffs);
    am.Crypt(Buff, r);
    am.Free;
    FileWrite(T, Buff^, r);

    am := TAMPRNG.Create(tk, 64, False, 768);
    paramrec.id := PRT_ID;
    am.Crypt(@paramrec, SizeOf(paramrec));
    am.Free;

    FileWrite(T, paramrec, SizeOf(paramrec));

    CloseHandle(T);

    FreeMem(Buff);

  end;

{$ENDIF FACTORY}

begin

  {$IFDEF FACTORY}
  Inject(ParamStr(1), ParamStr(2));
 {$ENDIF FACTORY}

 {$IFDEF CATCHER}
  Catch;
{$ENDIF}

end.
