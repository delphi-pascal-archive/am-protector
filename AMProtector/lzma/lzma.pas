unit LZMA;



interface

uses
  Classes,
  lzmaBaseU,
  lzmaCommonU,
  {$IFDEF FACTORY}
  lzmaEncoderU
{$ENDIF}
  {$IFDEF CATCHER}
  lzmaDecoderU
  {$ENDIF};

const
  kEncode = 0;
  kDecode = 1;


 {$IFDEF CATCHER}
function lzmadcomp(frommem: pointer; fromsize: integer): integer;
{$ENDIF}

{$IFDEF FACTORY}
function lzmacomp(frommem: pointer; fromsize: integer): integer;
{$ENDIF}

implementation


var
  params: string;

function GetStr(const str: string; const offset: integer): string;
var
  i: integer;
begin
  Result := '';
  for i := offset to length(str) do
    Result := Result + str[i];
end;



var
  command:    integer;
  dictionarySize: integer;
  dictionarySizeIsDefined: boolean;
  lc, lp, pb: integer;

  fb: integer;
  fbIsDefined: boolean;

  eos: boolean;

  algorithm:   integer;
  matchFinder: integer;

  inFile:  string;
  outFile: string;


procedure LZmain(inStream, OutStream: TMemoryStream);
const
  PROPERTIES_SIZE = 5;
var
  {$IFDEF FACTORY}
  encoder: TLZMAEncoder;
  {$ENDIF}
  {$IFDEF CATCHER}
  decoder: TLZMADecoder;
  {$ENDIF}
  eos: boolean;
  {$IFDEF FACTORY}
  fileSize: int64;
  {$ENDIF}
  i: integer;
  {$IFDEF CATCHER}
  properties: packed array [0..PROPERTIES_SIZE - 1] of byte;
  outSize: int64;
  v: byte;
  {$ENDIF}
begin

  if (command = kEncode) or (command = kDecode) then
    try

      eos := False;
       {$IFDEF FACTORY}
      if command = kEncode then
      begin
        encoder := TLZMAEncoder.Create();
        outstream.Seek(0, 0);
        try
          if not encoder.SetAlgorithm(algorithm) then      {EXCEPTION HERE};
          if not encoder.SetDictionarySize(dictionarySize) then  {EXCEPTION HERE};
          if not encoder.SeNumFastBytes(fb) then     {EXCEPTION HERE};
          if not encoder.SetMatchFinder(matchFinder) then {EXCEPTION HERE};
          if not encoder.SetLcLpPb(lc, lp, pb) then  {EXCEPTION HERE};
          encoder.SetEndMarkerMode(eos);
          encoder.WriteCoderProperties(outStream);
          if eos then
            fileSize := -1
          else
            fileSize := inStream.Size;
          for i := 0 to 7 do
            lzmaStreamWriteByte(outStream, (fileSize shr (8 * i)) and $FF);
          encoder.OnProgress := nil;
          encoder.Encode(inStream, outStream, -1, -1);
        finally
          encoder.Free();
        end;
      end
      else
    {$ENDIF}

      {$IFDEF CATCHER}

      begin
        instream.Seek(0, 0);
        inStream.ReadBuffer(properties, PROPERTIES_SIZE);
        decoder := TLZMADecoder.Create();
        try
          if not decoder.SetDecoderProperties(properties) then
          begin
          {EXCEPTION HERE}
          decoder.Free;
          end;
          outSize := 0;
          for i := 0 to 7 do
          begin
            v := lzmaStreamReadByte(inStream);
            outSize := outSize or (v shl (8 * i));
          end;
          decoder.OnProgress := nil;
          if not decoder.Decode(inStream, outStream, outSize) then
            begin
           {EXCEPTION HERE}
            decoder.Free;
            end;
        finally
          decoder.Free();
        end;
      end;
      {$ENDIF}
    finally
    end
  else
  begin
 {EXCEPTION HERE}
  end;
end;


  {$IFDEF FACTORY}

function lzmacomp(frommem: pointer; fromsize: integer): integer;
var
  M1, M2: TMemoryStream;
begin

  command := -1;
  lc      := 3;
  lp      := 0;
  pb      := 2;
  fbIsDefined := False;
  eos     := False;
  algorithm := 2;
  matchFinder := 1;

  dictionarySize := 4096 * 1024;
  fb      := 64;
  fbIsDefined := True;
  dictionarySizeIsDefined := True;
  command := kencode;

  M1 := TMemoryStream.Create;
  M2 := TMemoryStream.Create;
  M1.Write(frommem^, fromsize);
  M1.Seek(0, 0);
  M2.Seek(0, 0);
  lzmain(M1, M2);
  M1.Free;
  M2.Seek(0, 0);
  M2.Read(frommem^, M2.Size);
  Result := M2.Size;
  M2.Free;
end;

{$ENDIF}

{$IFDEF CATCHER}

function lzmadcomp(frommem: pointer; fromsize: integer): integer;
var
  M1, M2: TMemoryStream;
begin
  command := -1;
  lc      := 3;
  lp      := 0;
  pb      := 2;
  fb      := 128;
  fbIsDefined := False;
  eos     := False;
  algorithm := 2;
  matchFinder := 1;

  dictionarySize := 1 shl 24;
  fb      := 273;
  fbIsDefined := True;
  dictionarySizeIsDefined := True;
  command := kdecode;

  M1 := TMemoryStream.Create;
  M2 := TMemoryStream.Create;
  M1.Write(frommem^, fromsize);
  lzmain(M1, M2);
  M1.Free;
  M2.Seek(0, 0);
  M2.Read(frommem^, M2.Size);
  Result := M2.Size;
  M2.Free;

end;

{$ENDIF}


begin
end.
