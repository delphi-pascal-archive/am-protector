 // LZMA SDK   Copyright (C) 1999-2006 Igor Pavlov
 // original translation here:
 //   http://www.birtles.org.uk/programming/LZMA.442b.7z
 // napilniked %-) by Ketmar // Avalon Group
 //   http://avalon-group.ho.com.ua/
 //   ketmar@online.ua

// GNU LGPL / CPL


{$Q-}

unit lzmaDecoderU;

interface

uses
  SysUtils, Classes,
  lzmaBaseU, lzmaCommonU;

const
  kTopMask     = not ((1 shl 24) - 1);
  kNumBitModelTotalBits = 11;
  kBitModelTotal = (1 shl kNumBitModelTotalBits);
  kNumMoveBits = 5;


type
  TLZMARangeDecoder = class
  public
    range, code: integer;
    stream:      TStream;

    procedure SetStream(const aStream: TStream);
    procedure ReleaseStream();
    procedure Init();
    function DecodeDirectBits(const numTotalBits: integer): integer;
    function DecodeBit(var probs: array of smallint; const index: integer): integer;
  end;

  TLZMABitTreeDecoder = class
  public
    models: array of smallint;
    numBitLevels: integer;

    constructor Create(const pNumBitLevels: integer);
    destructor Destroy(); override;

    procedure Init();
    function Decode(const rangeDecoder: TLZMARangeDecoder): integer;
    function ReverseDecode(const rangeDecoder: TLZMARangeDecoder): integer;
  end;

  TLZMALZOutWindow = class
  public
    buffer: array of byte;
    pos:    integer;
    windowSize: integer;
    streamPos: integer;
    stream: TStream;

    destructor Destroy(); override;

    procedure _Create(const windowSize: integer);
    procedure SetStream(const stream: TStream);
    procedure ReleaseStream();
    procedure Init(const solid: boolean);
    procedure Flush();
    procedure CopyBlock(const distance: integer; len: integer);
    procedure PutByte(const b: byte);
    function GetByte(const distance: integer): byte;
  end;

  TLZMALenDecoder     = class;
  TLZMALiteralDecoder = class;

  TLZMADecoder = class
  private
    fOnProgress: TLZMAProgressProc;

    procedure DoProgress(const current, total: integer);

  public
    fOutWindow:    TLZMALZOutWindow;
    fRangeDecoder: TLZMARangeDecoder;

    fIsMatchDecoders:    array [0..lzmaBaseU.kNumStates shl
      lzmaBaseU.kNumPosStatesBitsMax - 1] of smallint;
    fIsRepDecoders:      array [0..lzmaBaseU.kNumStates - 1] of smallint;
    fIsRepG0Decoders:    array [0..lzmaBaseU.kNumStates - 1] of smallint;
    fIsRepG1Decoders:    array [0..lzmaBaseU.kNumStates - 1] of smallint;
    fIsRepG2Decoders:    array [0..lzmaBaseU.kNumStates - 1] of smallint;
    fIsRep0LongDecoders: array [0..lzmaBaseU.kNumStates shl
      lzmaBaseU.kNumPosStatesBitsMax - 1] of smallint;

    fPosSlotDecoder: array [0..lzmaBaseU.kNumLenToPosStates - 1] of TLZMABitTreeDecoder;
    fPosDecoders:    array [0..lzmaBaseU.kNumFullDistances -
    lzmaBaseU.kEndPosModelIndex - 1] of smallint;

    fPosAlignDecoder: TLZMABitTreeDecoder;

    fLenDecoder:    TLZMALenDecoder;
    fRepLenDecoder: TLZMALenDecoder;

    fLiteralDecoder: TLZMALiteralDecoder;

    fDictionarySize:      integer;
    fDictionarySizeCheck: integer;

    fPosStateMask: integer;

    constructor Create();
    destructor Destroy(); override;

    procedure Init();
    function SetDictionarySize(const dictionarySize: integer): boolean;
    function SetLcLpPb(const lc, lp, pb: integer): boolean;
    function Decode(const inStream, outStream: TStream; outSize: integer): boolean;
    function SetDecoderProperties(const properties: array of byte): boolean;

    property OnProgress: TLZMAProgressProc Read fOnProgress Write fOnProgress;
  end;

  TLZMALenDecoder = class
  public
    fChoice:    array [0..1] of smallint;
    fLowCoder:  array [0..lzmaBaseU.kNumPosStatesMax - 1] of TLZMABitTreeDecoder;
    fMidCoder:  array [0..lzmaBaseU.kNumPosStatesMax - 1] of TLZMABitTreeDecoder;
    fHighCoder: TLZMABitTreeDecoder;
    fNumPosStates: integer;

    constructor Create();
    destructor Destroy(); override;

    procedure _Create(const numPosStates: integer);
    procedure Init();
    function Decode(const rangeDecoder: TLZMARangeDecoder;
      const posState: integer): integer;
  end;

  TLZMADecoder2 = class
  public
    fDecoders: array [0..$300 - 1] of smallint;

    procedure Init();
    function DecodeNormal(const rangeDecoder: TLZMARangeDecoder): byte;
    function DecodeWithMatchByte(const rangeDecoder: TLZMARangeDecoder;
      matchByte: byte): byte;
  end;

  TLZMALiteralDecoder = class
  public
    fCoders:      array of TLZMADecoder2;
    fNumPrevBits: integer;
    fNumPosBits:  integer;
    fPosMask:     integer;

    destructor Destroy(); override;

    procedure _Create(const numPosBits, numPrevBits: integer);
    procedure Init();
    function GetDecoder(const pos: integer; const prevByte: byte): TLZMADecoder2;
  end;


implementation


function Max(a, b: integer): integer;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

procedure XInitBitModels(var probs: array of smallint);
var
  i: integer;
begin
  for i := 0 to length(probs) - 1 do
    probs[i] := kBitModelTotal shr 1;
end;

function XReverseDecode(var models: array of smallint; const startIndex: integer;
  const rangeDecoder: TLZMARangeDecoder; const numBitLevels: integer): integer;
var
  m, symbol, bitindex, bit: integer;
begin
  m      := 1;
  symbol := 0;
  for bitindex := 0 to numBitLevels - 1 do
  begin
    bit    := rangeDecoder.DecodeBit(models, startIndex + m);
    m      := (m shl 1) + bit;
    symbol := symbol or (bit shl bitindex);
  end;
  Result := symbol;
end;


{ TLZMARangeDecoder }
procedure TLZMARangeDecoder.SetStream(const aStream: TStream);
begin
  stream := aStream;
end;

procedure TLZMARangeDecoder.ReleaseStream();
begin
  stream := nil;
end;

procedure TLZMARangeDecoder.Init();
var
  i: integer;
begin
  code  := 0;
  range := -1;
  for i := 0 to 4 do
    code := (code shl 8) or byte(lzmaStreamReadByte(stream));
end;

function TLZMARangeDecoder.DecodeDirectBits(const numTotalBits: integer): integer;
var
  i, t: integer;
begin
  Result := 0;
  for i := numTotalBits downto 1 do
  begin
    range  := range shr 1;
    t      := ((code - range) shr 31);
    code   := code - range and (t - 1);
    Result := (Result shl 1) or (1 - t);
    if (range and kTopMask) = 0 then
    begin
      code  := (code shl 8) or lzmaStreamReadByte(stream);
      range := range shl 8;
    end;
  end;
end;

function TLZMARangeDecoder.DecodeBit(var probs: array of smallint;
  const index: integer): integer;
var
  prob, newbound: integer;
begin
  prob     := probs[index];
  newbound := (range shr kNumBitModelTotalBits) * prob;
  if (integer((integer(code) xor integer($80000000))) < integer(
    (integer(newBound) xor integer($80000000)))) then
  begin
    range := newBound;
    probs[index] := (prob + ((kBitModelTotal - prob) shr kNumMoveBits));
    if (range and kTopMask) = 0 then
    begin
      code  := (code shl 8) or lzmaStreamReadByte(stream);
      range := range shl 8;
    end;
    Result := 0;
  end
  else
  begin
    range := range - newBound;
    code  := code - newBound;
    probs[index] := (prob - ((prob) shr kNumMoveBits));
    if (range and kTopMask) = 0 then
    begin
      code  := (code shl 8) or lzmaStreamReadByte(stream);
      range := range shl 8;
    end;
    Result := 1;
  end;
end;


{ TLZMABitTreeDecoder }
constructor TLZMABitTreeDecoder.Create(const pNumBitLevels: integer);
begin
  inherited Create();
  numBitLevels := pNumBitLevels;
  SetLength(models, 1 shl pNumBitLevels);
end;

destructor TLZMABitTreeDecoder.Destroy();
begin
  SetLength(models, 0);
  inherited;
end;

procedure TLZMABitTreeDecoder.Init();
begin
  XInitBitModels(models);
end;

function TLZMABitTreeDecoder.Decode(const rangeDecoder: TLZMARangeDecoder): integer;
var
  m, bitIndex: integer;
begin
  m := 1;
  for bitIndex := numBitLevels downto 1 do
    m := (m shl 1) + rangeDecoder.DecodeBit(models, m);
  Result := m - (1 shl numBitLevels);
end;

function TLZMABitTreeDecoder.ReverseDecode(
  const rangeDecoder: TLZMARangeDecoder): integer;
var
  m, symbol, bitindex, bit: integer;
begin
  m      := 1;
  symbol := 0;
  for bitindex := 0 to numBitLevels - 1 do
  begin
    bit    := rangeDecoder.DecodeBit(models, m);
    m      := (m shl 1) + bit;
    symbol := symbol or (bit shl bitIndex);
  end;
  Result := symbol;
end;


{ TLZMALZOutWindow }
destructor TLZMALZOutWindow.Destroy();
begin
  SetLength(buffer, 0);
  inherited;
end;

procedure TLZMALZOutWindow._Create(const windowSize: integer);
begin
  if (length(buffer) = 0) or (self.windowSize <> windowSize) then
    SetLength(buffer, windowSize);
  self.windowSize := windowSize;
  pos := 0;
  streamPos := 0;
end;

procedure TLZMALZOutWindow.SetStream(const stream: TStream);
begin
  ReleaseStream();
  self.stream := stream;
end;

procedure TLZMALZOutWindow.ReleaseStream();
begin
  Flush();
  self.stream := nil;
end;

procedure TLZMALZOutWindow.Init(const solid: boolean);
begin
  if not solid then
  begin
    streamPos := 0;
    pos := 0;
  end;
end;

procedure TLZMALZOutWindow.Flush();
var
  size: integer;
begin
  size := pos - streamPos;
  if size = 0 then
    exit;
  stream.WriteBuffer(buffer[streamPos], size);
  if pos >= windowSize then
    pos := 0;
  streamPos := pos;
end;

procedure TLZMALZOutWindow.CopyBlock(const distance: integer; len: integer);
var
  pos: integer;
begin
  pos := self.pos - distance - 1;
  if pos < 0 then
    pos := pos + windowSize;
  while len <> 0 do
  begin
    if pos >= windowSize then
      pos := 0;
    buffer[self.pos] := buffer[pos];
    Inc(self.pos);
    Inc(pos);
    if self.pos >= windowSize then
      Flush();
    Dec(len);
  end;
end;

procedure TLZMALZOutWindow.PutByte(const b: byte);
begin
  buffer[pos] := b;
  Inc(pos);
  if pos >= windowSize then
    Flush();
end;

function TLZMALZOutWindow.GetByte(const distance: integer): byte;
var
  pos: integer;
begin
  pos := self.pos - distance - 1;
  if pos < 0 then
    pos := pos + windowSize;
  Result := buffer[pos];
end;


{ TLZMALenDecoder }
constructor TLZMALenDecoder.Create();
begin
  inherited;
  fHighCoder    := TLZMABitTreeDecoder.Create(lzmaBaseU.kNumHighLenBits);
  fNumPosStates := 0;
end;

destructor TLZMALenDecoder.Destroy();
var
  i: integer;
begin
  FreeAndNil(fHighCoder);
  for i := Low(fLowCoder) to High(fLowCoder) do
  begin
    FreeAndNil(fLowCoder[i]);
    FreeAndNil(fMidCoder[i]);
  end;
  inherited;
end;

procedure TLZMALenDecoder._Create(const numPosStates: integer);
begin
  while fNumPosStates < numPosStates do
  begin
    fLowCoder[fNumPosStates] := TLZMABitTreeDecoder.Create(lzmaBaseU.kNumLowLenBits);
    fMidCoder[fNumPosStates] := TLZMABitTreeDecoder.Create(lzmaBaseU.kNumMidLenBits);
    Inc(fNumPosStates);
  end;
end;

procedure TLZMALenDecoder.Init();
var
  posState: integer;
begin
  XInitBitModels(fChoice);
  for posState := 0 to fNumPosStates - 1 do
  begin
    fLowCoder[posState].Init();
    fMidCoder[posState].Init();
  end;
  fHighCoder.Init();
end;

function TLZMALenDecoder.Decode(const rangeDecoder: TLZMARangeDecoder;
  const posState: integer): integer;
var
  symbol: integer;
begin
  if rangeDecoder.DecodeBit(fChoice, 0) = 0 then
  begin
    Result := fLowCoder[posState].Decode(rangeDecoder);
    exit;
  end;
  symbol := lzmaBaseU.kNumLowLenSymbols;
  if rangeDecoder.DecodeBit(fChoice, 1) = 0 then
    symbol := symbol + fMidCoder[posState].Decode(rangeDecoder)
  else
    symbol := symbol + lzmaBaseU.kNumMidLenSymbols + fHighCoder.Decode(rangeDecoder);
  Result := symbol;
end;


{ TLZMADecoder2 }
procedure TLZMADecoder2.Init();
begin
  XInitBitModels(fDecoders);
end;

function TLZMADecoder2.DecodeNormal(const rangeDecoder: TLZMARangeDecoder): byte;
var
  symbol: integer;
begin
  symbol := 1;
  repeat
    symbol := (symbol shl 1) or rangeDecoder.DecodeBit(fDecoders, symbol);
  until not (symbol < $100);
  Result := byte(symbol);
end;

function TLZMADecoder2.DecodeWithMatchByte(const rangeDecoder: TLZMARangeDecoder;
  matchByte: byte): byte;
var
  symbol, matchbit, bit: integer;
begin
  symbol := 1;
  repeat
    matchBit := (matchByte shr 7) and 1;
    matchByte := byte(matchByte shl 1);
    bit    := rangeDecoder.DecodeBit(fDecoders, ((1 + matchBit) shl 8) + symbol);
    symbol := (symbol shl 1) or bit;
    if matchBit <> bit then
    begin
      while symbol < $100 do
        symbol := (symbol shl 1) or rangeDecoder.DecodeBit(fDecoders, symbol);
      break;
    end;
  until not (symbol < $100);
  Result := byte(symbol);
end;


{ TLZMALiteralDecoder }
destructor TLZMALiteralDecoder.Destroy();
var
  i: integer;
begin
  for i := Low(fCoders) to High(fCoders) do
    FreeAndNil(fCoders[i]);
  inherited;
end;

procedure TLZMALiteralDecoder._Create(const numPosBits, numPrevBits: integer);
var
  numStates, i: integer;
begin
  if (length(fCoders) <> 0) and (fNumPrevBits = numPrevBits) and
    (fNumPosBits = numPosBits) then
    exit;
  fNumPosBits  := numPosBits;
  fPosMask     := (1 shl numPosBits) - 1;
  fNumPrevBits := numPrevBits;
  numStates    := 1 shl (fNumPrevBits + fNumPosBits);
  SetLength(fCoders, numStates);
  for i := 0 to numStates - 1 do
    fCoders[i] := TLZMADecoder2.Create();
end;

procedure TLZMALiteralDecoder.Init();
var
  numStates, i: integer;
begin
  numStates := 1 shl (fNumPrevBits + fNumPosBits);
  for i := 0 to numStates - 1 do
    fCoders[i].Init();
end;

function TLZMALiteralDecoder.GetDecoder(const pos: integer;
  const prevByte: byte): TLZMADecoder2;
begin
  Result := fCoders[((pos and fPosMask) shl fNumPrevBits) +
    ((prevByte and $FF) shr (8 - fNumPrevBits))];
end;


{ TLZMADecoder }
constructor TLZMADecoder.Create();
var
  i: integer;
begin
  inherited;
  fOnProgress     := nil;
  fOutWindow      := TLZMALZOutWindow.Create();
  fRangeDecoder   := TLZMARangeDecoder.Create();
  fPosAlignDecoder := TLZMABitTreeDecoder.Create(lzmaBaseU.kNumAlignBits);
  fLenDecoder     := TLZMALenDecoder.Create();
  fRepLenDecoder  := TLZMALenDecoder.Create();
  fLiteralDecoder := TLZMALiteralDecoder.Create();
  fDictionarySize := -1;
  fDictionarySizeCheck := -1;
  for i := 0 to lzmaBaseU.kNumLenToPosStates - 1 do
    fPosSlotDecoder[i] := TLZMABitTreeDecoder.Create(lzmaBaseU.kNumPosSlotBits);
end;

destructor TLZMADecoder.Destroy();
var
  i: integer;
begin
  FreeAndNil(fOutWindow);
  FreeAndNil(fRangeDecoder);
  FreeAndNil(fPosAlignDecoder);
  FreeAndNil(fLenDecoder);
  FreeAndNil(fRepLenDecoder);
  FreeAndNil(fLiteralDecoder);
  for i := 0 to lzmaBaseU.kNumLenToPosStates - 1 do
    FreeAndNil(fPosSlotDecoder[i]);
  inherited;
end;

function TLZMADecoder.SetDictionarySize(const dictionarySize: integer): boolean;
begin
  if dictionarySize < 0 then
    Result := False
  else
  begin
    if fDictionarySize <> dictionarySize then
    begin
      fDictionarySize      := dictionarySize;
      fDictionarySizeCheck := Max(fDictionarySize, 1);
      fOutWindow._Create(Max(fDictionarySizeCheck, (1 shl 12)));
    end;
    Result := True;
  end;
end;

function TLZMADecoder.SetLcLpPb(const lc, lp, pb: integer): boolean;
var
  numPosStates: integer;
begin
  if (lc > lzmaBaseU.kNumLitContextBitsMax) or (lp > 4) or
    (pb > lzmaBaseU.kNumPosStatesBitsMax) then
  begin
    Result := False;
    exit;
  end;
  fLiteralDecoder._Create(lp, lc);
  numPosStates := 1 shl pb;
  fLenDecoder._Create(numPosStates);
  fRepLenDecoder._Create(numPosStates);
  fPosStateMask := numPosStates - 1;
  Result := True;
end;

procedure TLZMADecoder.Init();
var
  i: integer;
begin
  fOutWindow.Init(False);

  XInitBitModels(fIsMatchDecoders);
  XInitBitModels(fIsRep0LongDecoders);
  XInitBitModels(fIsRepDecoders);
  XInitBitModels(fIsRepG0Decoders);
  XInitBitModels(fIsRepG1Decoders);
  XInitBitModels(fIsRepG2Decoders);
  XInitBitModels(fPosDecoders);

  fLiteralDecoder.Init();
  for i := 0 to lzmaBaseU.kNumLenToPosStates - 1 do
    fPosSlotDecoder[i].Init();
  fLenDecoder.Init();
  fRepLenDecoder.Init();
  fPosAlignDecoder.Init();
  fRangeDecoder.Init();
end;

function TLZMADecoder.Decode(const inStream, outStream: TStream;
  outSize: integer): boolean;
var
  state, rep0, rep1, rep2, rep3: integer;
  nowPos64: integer;
  prevByte: byte;
  posState: integer;
  decoder2: TLZMADecoder2;
  len, distance, posSlot, numDirectBits: integer;
  lpos:     integer;
  progint:  integer;
begin
  DoProgress(0, outSize);
  fRangeDecoder.SetStream(inStream);
  fOutWindow.SetStream(outStream);
  Init();

  state := lzmaBaseU.StateInit();
  rep0  := 0;
  rep1  := 0;
  rep2  := 0;
  rep3  := 0;

  nowPos64 := 0;
  prevByte := 0;
  progint  := outsize div CODE_PROGRESS_INTERVAL;
  lpos     := progint;
  while (outSize < 0) or (nowPos64 < outSize) do
  begin
    if nowPos64 >= lpos then
    begin
      DoProgress(nowPos64, outSize);
      Inc(lpos, progint);
    end;
    posState := nowPos64 and fPosStateMask;
    if fRangeDecoder.DecodeBit(fIsMatchDecoders,
      (state shl lzmaBaseU.kNumPosStatesBitsMax) + posState) = 0 then
    begin
      decoder2 := fLiteralDecoder.GetDecoder(nowPos64, prevByte);
      if not lzmaBaseU.StateIsCharState(state) then
        prevByte := decoder2.DecodeWithMatchByte(fRangeDecoder, fOutWindow.GetByte(rep0))
      else
        prevByte := decoder2.DecodeNormal(fRangeDecoder);
      fOutWindow.PutByte(prevByte);
      state := lzmaBaseU.StateUpdateChar(state);
      Inc(nowPos64);
    end
    else
    begin
      if fRangeDecoder.DecodeBit(fIsRepDecoders, state) = 1 then
      begin
        len := 0;
        if fRangeDecoder.DecodeBit(fIsRepG0Decoders, state) = 0 then
        begin
          if fRangeDecoder.DecodeBit(fIsRep0LongDecoders,
            (state shl lzmaBaseU.kNumPosStatesBitsMax) + posState) = 0 then
          begin
            state := lzmaBaseU.StateUpdateShortRep(state);
            len   := 1;
          end;
        end
        else
        begin
          if fRangeDecoder.DecodeBit(fIsRepG1Decoders, state) = 0 then
            distance := rep1
          else
          begin
            if fRangeDecoder.DecodeBit(fIsRepG2Decoders, state) = 0 then
              distance := rep2
            else
            begin
              distance := rep3;
              rep3     := rep2;
            end;
            rep2 := rep1;
          end;
          rep1 := rep0;
          rep0 := distance;
        end;
        if len = 0 then
        begin
          len   := fRepLenDecoder.Decode(fRangeDecoder, posState) +
            lzmaBaseU.kMatchMinLen;
          state := lzmaBaseU.StateUpdateRep(state);
        end;
      end
      else
      begin
        rep3    := rep2;
        rep2    := rep1;
        rep1    := rep0;
        len     := lzmaBaseU.kMatchMinLen + fLenDecoder.Decode(fRangeDecoder, posState);
        state   := lzmaBaseU.StateUpdateMatch(state);
        posSlot := fPosSlotDecoder[lzmaBaseU.GetLenToPosState(len)].Decode(fRangeDecoder);
        if posSlot >= lzmaBaseU.kStartPosModelIndex then
        begin
          numDirectBits := (posSlot shr 1) - 1;
          rep0 := ((2 or (posSlot and 1)) shl numDirectBits);
          if posSlot < lzmaBaseU.kEndPosModelIndex then
            rep0 := rep0 + XReverseDecode(fPosDecoders, rep0 -
              posSlot - 1, fRangeDecoder, numDirectBits)
          else
          begin
            rep0 := rep0 + (fRangeDecoder.DecodeDirectBits(
              numDirectBits - lzmaBaseU.kNumAlignBits) shl
              lzmaBaseU.kNumAlignBits);
            rep0 := rep0 + fPosAlignDecoder.ReverseDecode(fRangeDecoder);
            if rep0 < 0 then
            begin
              if rep0 = -1 then
                break;
              Result := False;
              exit;
            end;
          end;
        end
        else
          rep0 := posSlot;
      end;
      if (rep0 >= nowPos64) or (rep0 >= fDictionarySizeCheck) then
      begin
        fOutWindow.Flush();
        Result := False;
        exit;
      end;
      fOutWindow.CopyBlock(rep0, len);
      nowPos64 := nowPos64 + len;
      prevByte := fOutWindow.GetByte(0);
    end;
  end;
  fOutWindow.Flush();
  fOutWindow.ReleaseStream();
  fRangeDecoder.ReleaseStream();
  DoProgress(nowPos64, nowPos64);
  Result := True;
end;

function TLZMADecoder.SetDecoderProperties(const properties: array of byte): boolean;
var
  val, lc, remainder, lp, pb, dictionarysize, i: integer;
begin
  if length(properties) < 5 then
  begin
    Result := False;
    exit;
  end;
  val := properties[0] and $FF;
  lc  := val mod 9;
  remainder := val div 9;
  lp  := remainder mod 5;
  pb  := remainder div 5;
  dictionarySize := 0;
  for i := 0 to 3 do
    dictionarySize := dictionarysize + ((properties[1 + i]) and $FF) shl (i * 8);
  if not SetLcLpPb(lc, lp, pb) then
  begin
    Result := False;
    exit;
  end;
  Result := SetDictionarySize(dictionarySize);
end;

procedure TLZMADecoder.DoProgress(const current, total: integer);
begin
  if Assigned(fOnProgress) then
    fOnProgress(current, total);
end;


end.
