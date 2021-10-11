 // LZMA SDK   Copyright (C) 1999-2006 Igor Pavlov
 // original translation here:
 //   http://www.birtles.org.uk/programming/LZMA.442b.7z
 // napilniked %-) by Ketmar // Avalon Group
 //   http://avalon-group.ho.com.ua/
 //   ketmar@online.ua

// GNU LGPL / CPL


{$Q-}

unit lzmaEncoderU;

interface

uses
  SysUtils, Classes,
  lzmaCommonU, lzmaBaseU;

const
  kNumBitPriceShiftBits = 6;
  kTopMask     = not ((1 shl 24) - 1);
  kNumBitModelTotalBits = 11;
  kBitModelTotal = (1 shl kNumBitModelTotalBits);
  kNumMoveBits = 5;
  kNumMoveReducingBits = 2;

  kHash2Size      = 1 shl 10;
  kHash3Size      = 1 shl 16;
  kBT2HashSize    = 1 shl 16;
  kStartMaxLen    = 1;
  kHash3Offset    = kHash2Size;
  kEmptyHashValue = 0;
  kMaxValForNormalize = (1 shl 30) - 1;

  EMatchFinderTypeBT2 = 0;
  EMatchFinderTypeBT4 = 1;
  kIfinityPrice: integer = $FFFFFFF;
  kDefaultDictionaryLogSize = 22;
  kNumFastBytesDefault = $20;
  kNumLenSpecSymbols = lzmaBaseU.kNumLowLenSymbols + lzmaBaseU.kNumMidLenSymbols;
  kNumOpts  = 1 shl 12;
  kPropSize = 5;


type
  TLZMARangeEncoder = class
  private
    probPrices: array [0..kBitModelTotal shr kNumMoveReducingBits - 1] of integer;
  public
    stream: TMemoryStream;
    low, position: int64;
    range, cacheSize, cache: integer;

    constructor Create();

    procedure SeTMemoryStream(const stream: TMemoryStream);
    procedure ReleaseStream();
    procedure Init();
    procedure FlushData();
    procedure FlushStream();
    procedure ShiftLow();
    procedure EncodeDirectBits(const v, numTotalBits: integer);
    function GetProcessedSizeAdd(): int64;
    procedure Encode(var probs: array of smallint; const index, symbol: integer);
    function GetPrice(const prob, symbol: integer): integer;
    function GetPrice0(const prob: integer): integer;
    function GetPrice1(const prob: integer): integer;
  end;

  TLZMABitTreeEncoder = class
  public
    models: array of smallint;
    numBitLevels: integer;

    constructor Create(const aNumBitLevels: integer);

    procedure Init();
    procedure Encode(const rangeEncoder: TLZMARangeEncoder; const symbol: integer);
    procedure ReverseEncode(const rangeEncoder: TLZMARangeEncoder; symbol: integer);
    function GetPrice(const symbol: integer): integer;
    function ReverseGetPrice(symbol: integer): integer;
  end;

  TLZMALZInWindow = class
  public
    bufferBase: array of byte;// pointer to buffer with data
    stream:     TMemoryStream;
    posLimit:   integer;
    // offset (from _buffer) of first Byte when new block reading must be done
    streamEndWasReached: boolean; // if (true) then _streamPos shows real end of stream

    pointerToLastSafePosition: integer;

    bufferOffset: integer;

    blockSize: integer;       // Size of Allocated memory block
    pos: integer;             // offset (from _buffer) of curent Byte
    keepSizeBefore: integer;  // how many BYTEs must be kept in buffer before _pos
    keepSizeAfter: integer;   // how many BYTEs must be kept buffer after _pos
    streamPos: integer;       // offset (from _buffer) of first not read Byte from Stream

    procedure MoveBlock();
    procedure ReadBlock();
    procedure _Free();
    procedure _Create(const keepSizeBefore, keepSizeAfter, keepSizeReserv: integer);
      virtual;
    procedure SeTMemoryStream(const stream: TMemoryStream);
    procedure ReleaseStream();
    procedure Init(); virtual;
    procedure MovePos(); virtual;
    function GetIndexByte(const index: integer): byte;
    // index + limit have not to exceed _keepSizeAfter;
    function GetMatchLen(const index: integer; distance, limit: integer): integer;
    function GetNumAvailableBytes(): integer;
    procedure ReduceOffsets(const subValue: integer);
  end;

  TLZMALZBinTree = class(TLZMALZInWindow)
  public
    cyclicBufferPos:  integer;
    cyclicBufferSize: integer;
    matchMaxLen:      integer;

    son:  array of integer;
    hash: array of integer;

    cutValue:    integer;
    hashMask:    integer;
    hashSizeSum: integer;

    hashArray: boolean;

    kNumHashDirectBytes: integer;
    kMinMatchCheck: integer;
    kFixHashSize: integer;

    constructor Create();

    procedure SetType(const aNumHashBytes: integer);
    procedure Init(); override;
    procedure MovePos(); override;
    function _Create(const historySize, keepAddBufferBefore, matchMaxLen,
      keepAddBufferAfter: integer): boolean; reintroduce;
    function GetMatches(var distances: array of integer): integer;
    procedure Skip(num: integer);
    procedure NormalizeLinks(var items: array of integer;
      const numItems, subValue: integer);
    procedure Normalize();
    procedure SetCutValue(const cutValue: integer);
  end;

  TLZMAEncoder2 = class;
  TLZMALiteralEncoder = class;
  TLZMAOptimal  = class;
  TLZMALenPriceTableEncoder = class;

  TLZMAEncoder = class
  private
    fOnProgress: TLZMAProgressProc;

    procedure DoProgress(const current, total: integer);

  public
    fFastPos: array [0..1 shl 11 - 1] of byte;
    fState:   integer;
    fPreviousByte: byte;
    fRepDistances: array [0..lzmaBaseU.kNumRepDistances - 1] of integer;

    fOptimum:      array [0..kNumOpts - 1] of TLZMAOptimal;
    fMatchFinder:  TLZMALZBinTree;
    fRangeEncoder: TLZMARangeEncoder;

    fIsMatch:    array [0..lzmaBaseU.kNumStates shl
      lzmaBaseU.kNumPosStatesBitsMax - 1] of smallint;
    fIsRep:      array [0..lzmaBaseU.kNumStates - 1] of smallint;
    fIsRepG0:    array [0..lzmaBaseU.kNumStates - 1] of smallint;
    fIsRepG1:    array [0..lzmaBaseU.kNumStates - 1] of smallint;
    fIsRepG2:    array [0..lzmaBaseU.kNumStates - 1] of smallint;
    fIsRep0Long: array [0..lzmaBaseU.kNumStates shl
      lzmaBaseU.kNumPosStatesBitsMax - 1] of smallint;

    fPosSlotEncoder: array [0..lzmaBaseU.kNumLenToPosStates - 1] of
    TLZMABitTreeEncoder; // kNumPosSlotBits

    fPosEncoders:     array [0..lzmaBaseU.kNumFullDistances -
    lzmaBaseU.kEndPosModelIndex - 1] of smallint;
    fPosAlignEncoder: TLZMABitTreeEncoder;

    fLenEncoder: TLZMALenPriceTableEncoder;
    fRepMatchLenEncoder: TLZMALenPriceTableEncoder;

    fLiteralEncoder: TLZMALiteralEncoder;

    fMatchDistances: array [0..lzmaBaseU.kMatchMaxLen * 2 + 1] of integer;

    fNumFastBytes:     integer;
    fLongestMatchLength: integer;
    fNumDistancePairs: integer;

    fAdditionalOffset: integer;

    fOptimumEndIndex:     integer;
    fOptimumCurrentIndex: integer;

    fLongestMatchWasFound: boolean;

    fPosSlotPrices:   array [0..1 shl (lzmaBaseU.kNumPosSlotBits +
    lzmaBaseU.kNumLenToPosStatesBits) - 1] of integer;
    fDistancesPrices: array [0..lzmaBaseU.kNumFullDistances shl
      lzmaBaseU.kNumLenToPosStatesBits - 1] of integer;
    fAlignPrices:     array [0..lzmaBaseU.kAlignTableSize - 1] of integer;
    fAlignPriceCount: integer;

    fDistTableSize: integer;

    fPosStateBits: integer;
    fPosStateMask: integer;
    fNumLiteralPosStateBits: integer;
    fNumLiteralContextBits: integer;

    fDictionarySize:     integer;
    fDictionarySizePrev: integer;
    fNumFastBytesPrev:   integer;

    nowPos64:  int64;
    fFinished: boolean;
    fInStream: TMemoryStream;

    fMatchFinderType: integer;
    fWriteEndMark:    boolean;

    fNeedReleaseMFStream: boolean;

    reps:     array [0..lzmaBaseU.kNumRepDistances - 1] of integer;
    repLens:  array [0..lzmaBaseU.kNumRepDistances - 1] of integer;
    backRes:  integer;
    processedInSize: int64;
    processedOutSize: int64;
    finished: boolean;
    properties: array [0..kPropSize] of byte;
    tempPrices: array [0..lzmaBaseU.kNumFullDistances - 1] of integer;
    fMatchPriceCount: integer;

    constructor Create();
    destructor Destroy(); override;

    function GetPosSlot(const pos: integer): integer;
    function GetPosSlot2(const pos: integer): integer;
    procedure BaseInit();
    procedure _Create();
    procedure SetWriteEndMarkerMode(const writeEndMarker: boolean);
    procedure Init();
    function ReadMatchDistances(): integer;
    procedure MovePos(const num: integer);
    function GetRepLen1Price(const state, posState: integer): integer;
    function GetPureRepPrice(const repIndex, state, posState: integer): integer;
    function GetRepPrice(const repIndex, len, state, posState: integer): integer;
    function GetPosLenPrice(const pos, len, posState: integer): integer;
    function Backward(cur: integer): integer;
    function GetOptimum(position: integer): integer;
    function ChangePair(const smallDist, bigDist: integer): boolean;
    procedure WriteEndMarker(const posState: integer);
    procedure Flush(const nowPos: integer);
    procedure ReleaseMFStream();
    procedure CodeOneBlock(var inSize, outSize: int64; var finished: boolean);
    procedure FillDistancesPrices();
    procedure FillAlignPrices();
    procedure SetOuTMemoryStream(const ouTMemoryStream: TMemoryStream);
    procedure ReleaseOuTMemoryStream();
    procedure ReleaseStreams();
    procedure SeTMemoryStreams(const inStream, ouTMemoryStream: TMemoryStream;
      const inSize, outSize: int64);
    procedure Encode(const inStream, ouTMemoryStream: TMemoryStream;
      const inSize, outSize: int64);
    procedure WriteCoderProperties(const ouTMemoryStream: TMemoryStream);
    function SetAlgorithm(const algorithm: integer): boolean;
    function SetDictionarySize(dictionarySize: integer): boolean;
    function SeNumFastBytes(const numFastBytes: integer): boolean;
    function SetMatchFinder(const matchFinderIndex: integer): boolean;
    function SetLcLpPb(const lc, lp, pb: integer): boolean;
    procedure SetEndMarkerMode(const endMarkerMode: boolean);

    property OnProgress: TLZMAProgressProc Read fOnProgress Write fOnProgress;
  end;

  TLZMALiteralEncoder = class
  public
    fCoders:      array of TLZMAEncoder2;
    fNumPrevBits: integer;
    fNumPosBits:  integer;
    fPosMask:     integer;

    procedure _Create(const numPosBits, numPrevBits: integer);
    destructor Destroy(); override;

    procedure Init();
    function GetSubCoder(const pos: integer; const prevByte: byte): TLZMAEncoder2;
  end;

  TLZMAEncoder2 = class
  public
    fEncoders: array[0..$300 - 1] of smallint;

    procedure Init();
    procedure Encode(const rangeEncoder: TLZMARangeEncoder; const symbol: byte);
    procedure EncodeMatched(const rangeEncoder: TLZMARangeEncoder;
      const matchByte, symbol: byte);
    function GetPrice(const matchMode: boolean; const matchByte, symbol: byte): integer;
  end;

  TLZMALenEncoder = class
  public
    fChoice:    array[0..1] of smallint;
    fLowCoder:  array [0..lzmaBaseU.kNumPosStatesEncodingMax - 1] of TLZMABitTreeEncoder;
    fMidCoder:  array [0..lzmaBaseU.kNumPosStatesEncodingMax - 1] of TLZMABitTreeEncoder;
    fHighCoder: TLZMABitTreeEncoder;

    constructor Create();
    destructor Destroy(); override;

    procedure Init(const numPosStates: integer);
    procedure Encode(const rangeEncoder: TLZMARangeEncoder;
      symbol: integer; const posState: integer); virtual;
    procedure SetPrices(const posState, numSymbols: integer;
      var prices: array of integer; const st: integer);
  end;

  TLZMALenPriceTableEncoder = class(TLZMALenEncoder)
  public
    fPrices:    array [0..lzmaBaseU.kNumLenSymbols shl
      lzmaBaseU.kNumPosStatesBitsEncodingMax - 1] of integer;
    fTableSize: integer;
    fCounters:  array [0..lzmaBaseU.kNumPosStatesEncodingMax - 1] of integer;

    procedure SetTableSize(const tableSize: integer);
    function GetPrice(const symbol, posState: integer): integer;
    procedure UpdateTable(const posState: integer);
    procedure UpdateTables(const numPosStates: integer);
    procedure Encode(const rangeEncoder: TLZMARangeEncoder; symbol: integer;
      const posState: integer); override;
  end;

  TLZMAOptimal = class
  public
    state: integer;

    prev1IsChar: boolean;
    prev2: boolean;

    posPrev2:  integer;
    backPrev2: integer;

    price:    integer;
    posPrev:  integer;
    backPrev: integer;

    backs0: integer;
    backs1: integer;
    backs2: integer;
    backs3: integer;

    procedure MakeAsChar();
    procedure MakeAsShortRep();
    function IsShortRep(): boolean;
  end;


implementation


var
  rangeEncoder: TLZMARangeEncoder = nil;
  crcTable:     array [0..255] of integer;


function Min(a, b: integer): integer;
begin
  if a > b then
    Result := b
  else
    Result := a;
end;

procedure XInitBitModels(var probs: array of smallint);
var
  i: integer;
begin
  for i := 0 to length(probs) - 1 do
    probs[i] := kBitModelTotal shr 1;
end;

function XReverseGetPrice(var models: array of smallint;
  const startIndex, numBitLevels: integer; symbol: integer): integer;
var
  price, m, i, bit: integer;
begin
  price := 0;
  m     := 1;
  for i := numBitLevels downto 1 do
  begin
    bit    := symbol and 1;
    symbol := symbol shr 1;
    price  := price + rangeEncoder.GetPrice(models[startIndex + m], bit);
    m      := (m shl 1) or bit;
  end;
  Result := price;
end;

procedure XReverseEncode(var models: array of smallint; const startIndex: integer;
  const rangeEncoder: TLZMARangeEncoder; const numBitLevels: integer; symbol: integer);
var
  m, i, bit: integer;
begin
  m := 1;
  for i := 0 to numBitLevels - 1 do
  begin
    bit := symbol and 1;
    rangeEncoder.Encode(models, startIndex + m, bit);
    m      := (m shl 1) or bit;
    symbol := symbol shr 1;
  end;
end;


{ TLZMARangeEncoder }
constructor TLZMARangeEncoder.Create();
var
  kNumBits: integer;
  i, j, start, _end: integer;
begin
  kNumBits := (kNumBitModelTotalBits - kNumMoveReducingBits);
  for i := kNumBits - 1 downto 0 do
  begin
    start := 1 shl (kNumBits - i - 1);
    _end  := 1 shl (kNumBits - i);
    for j := start to _end - 1 do
      probPrices[j] := (i shl kNumBitPriceShiftBits) +
        (((_end - j) shl kNumBitPriceShiftBits) shr (kNumBits - i - 1));
  end;
end;

procedure TLZMARangeEncoder.SeTMemoryStream(const stream: TMemoryStream);
begin
  self.stream := stream;
end;

procedure TLZMARangeEncoder.ReleaseStream();
begin
  stream := nil;
end;

procedure TLZMARangeEncoder.Init();
begin
  position := 0;
  low      := 0;
  range    := -1;
  cacheSize := 1;
  cache    := 0;
end;

procedure TLZMARangeEncoder.FlushData();
var
  i: integer;
begin
  for i := 0 to 4 do
    ShiftLow();
end;

procedure TLZMARangeEncoder.FlushStream();
begin
  //stream.Flush();
end;

procedure TLZMARangeEncoder.ShiftLow();
var
  lowHi, temp: integer;
begin
  lowHi := (low shr 32);
  if (lowHi <> 0) or (low < int64($FF000000)) then
  begin
    position := position + cacheSize;
    temp     := cache;
    repeat
      lzmaStreamWriteByte(stream, byte(temp + lowHi));
      temp := $FF;
      Dec(cacheSize);
    until (cacheSize = 0);
    cache := (low shr 24);
  end;
  Inc(cacheSize);
  low := (low and integer($FFFFFF)) shl 8;
end;

procedure TLZMARangeEncoder.EncodeDirectBits(const v, numTotalBits: integer);
var
  i: integer;
begin
  for i := numTotalBits - 1 downto 0 do
  begin
    range := range shr 1;
    if ((v shr i) and 1) = 1 then
      Inc(low, range);
    if (range and kTopMask) = 0 then
    begin
      range := range shl 8;
      ShiftLow();
    end;
  end;
end;

function TLZMARangeEncoder.GetProcessedSizeAdd(): int64;
begin
  Result := cacheSize + position + 4;
end;

procedure TLZMARangeEncoder.Encode(var probs: array of smallint;
  const index, symbol: integer);
var
  prob, newbound: integer;
begin
  prob     := probs[index];
  newBound := (range shr kNumBitModelTotalBits) * prob;
  if symbol = 0 then
  begin
    range := newBound;
    probs[index] := (prob + ((kBitModelTotal - prob) shr kNumMoveBits));
  end
  else
  begin
    low   := low + (newBound and int64($FFFFFFFF));
    range := range - newBound;
    probs[index] := (prob - ((prob) shr kNumMoveBits));
  end;
  if (range and kTopMask) = 0 then
  begin
    range := range shl 8;
    ShiftLow();
  end;
end;

function TLZMARangeEncoder.GetPrice(const prob, symbol: integer): integer;
begin
  Result := probPrices[(((prob - symbol) xor ((-symbol))) and (kBitModelTotal - 1)) shr
    kNumMoveReducingBits];
end;

function TLZMARangeEncoder.GetPrice0(const prob: integer): integer;
begin
  Result := probPrices[prob shr kNumMoveReducingBits];
end;

function TLZMARangeEncoder.GetPrice1(const prob: integer): integer;
begin
  Result := probPrices[(kBitModelTotal - prob) shr kNumMoveReducingBits];
end;


{ TLZMABitTreeEncoder }
constructor TLZMABitTreeEncoder.Create(const aNumBitLevels: integer);
begin
  numBitLevels := aNumBitLevels;
  SetLength(models, 1 shl aNumBitLevels);
end;

procedure TLZMABitTreeEncoder.Init();
begin
  XInitBitModels(models);
end;

procedure TLZMABitTreeEncoder.Encode(const rangeEncoder: TLZMARangeEncoder;
  const symbol: integer);
var
  m, bitindex, bit: integer;
begin
  m := 1;
  for bitIndex := numBitLevels - 1 downto 0 do
  begin
    bit := (symbol shr bitIndex) and 1;
    rangeEncoder.Encode(models, m, bit);
    m := (m shl 1) or bit;
  end;
end;

procedure TLZMABitTreeEncoder.ReverseEncode(const rangeEncoder: TLZMARangeEncoder;
  symbol: integer);
var
  m, i, bit: integer;
begin
  m := 1;
  for i := 0 to numBitLevels - 1 do
  begin
    bit := symbol and 1;
    rangeEncoder.Encode(models, m, bit);
    m      := (m shl 1) or bit;
    symbol := symbol shr 1;
  end;
end;

function TLZMABitTreeEncoder.GetPrice(const symbol: integer): integer;
var
  price, m, bitindex, bit: integer;
begin
  price := 0;
  m     := 1;
  for bitIndex := numBitLevels - 1 downto 0 do
  begin
    bit   := (symbol shr bitIndex) and 1;
    price := price + rangeEncoder.GetPrice(models[m], bit);
    m     := (m shl 1) + bit;
  end;
  Result := price;
end;

function TLZMABitTreeEncoder.ReverseGetPrice(symbol: integer): integer;
var
  price, m, i, bit: integer;
begin
  price := 0;
  m     := 1;
  for i := numBitLevels downto 1 do
  begin
    bit    := symbol and 1;
    symbol := symbol shr 1;
    price  := price + rangeEncoder.GetPrice(models[m], bit);
    m      := (m shl 1) or bit;
  end;
  Result := price;
end;


{ TLZMALZInWindow }
procedure TLZMALZInWindow.MoveBlock();
var
  offset, numbytes, i: integer;
begin
  offset := bufferOffset + pos - keepSizeBefore;
  // we need one additional Byte, since MovePos moves on 1 Byte.
  if offset > 0 then
    Dec(offset);
  numBytes := bufferOffset + streamPos - offset;
  // check negative offset ????
  for i := 0 to numBytes - 1 do
    bufferBase[i] := bufferBase[offset + i];
  bufferOffset := bufferOffset - offset;
end;

procedure TLZMALZInWindow.ReadBlock();
var
  size, numreadbytes, pointerToPostion: integer;
begin
  if streamEndWasReached then
    exit;
  while True do
  begin
    size := (0 - bufferOffset) + blockSize - streamPos;
    if size = 0 then
      exit;
    numReadBytes := stream.Read(bufferBase[bufferOffset + streamPos], size);
    if numReadBytes = 0 then
    begin
      posLimit := streamPos;
      pointerToPostion := bufferOffset + posLimit;
      if pointerToPostion > pointerToLastSafePosition then
        posLimit := pointerToLastSafePosition - bufferOffset;
      streamEndWasReached := True;
      exit;
    end;
    streamPos := streamPos + numReadBytes;
    if streamPos >= pos + keepSizeAfter then
      posLimit := streamPos - keepSizeAfter;
  end;
end;

procedure TLZMALZInWindow._Free();
begin
  SetLength(bufferBase, 0);
end;

procedure TLZMALZInWindow._Create(const keepSizeBefore, keepSizeAfter,
  keepSizeReserv: integer);
var
  blocksize: integer;
begin
  self.keepSizeBefore := keepSizeBefore;
  self.keepSizeAfter := keepSizeAfter;
  blockSize := keepSizeBefore + keepSizeAfter + keepSizeReserv;
  if (length(bufferBase) = 0) or (self.blockSize <> blockSize) then
  begin
    _Free();
    self.blockSize := blockSize;
    setlength(bufferBase, self.blockSize);
  end;
  pointerToLastSafePosition := self.blockSize - keepSizeAfter;
end;

procedure TLZMALZInWindow.SeTMemoryStream(const stream: TMemoryStream);
begin
  self.stream := stream;
end;

procedure TLZMALZInWindow.ReleaseStream();
begin
  stream := nil;
end;

procedure TLZMALZInWindow.Init();
begin
  bufferOffset := 0;
  pos := 0;
  streamPos := 0;
  streamEndWasReached := False;
  ReadBlock();
end;

procedure TLZMALZInWindow.MovePos();
var
  pointerToPostion: integer;
begin
  Inc(pos);
  if pos > posLimit then
  begin
    pointerToPostion := bufferOffset + pos;
    if pointerToPostion > pointerToLastSafePosition then
      MoveBlock();
    ReadBlock();
  end;
end;

function TLZMALZInWindow.GetIndexByte(const index: integer): byte;
begin
  Result := bufferBase[bufferOffset + pos + index];
end;

function TLZMALZInWindow.GetMatchLen(const index: integer;
  distance, limit: integer): integer;
var
  pby, i: integer;
begin
  if streamEndWasReached then
    if (pos + index) + limit > streamPos then
      limit := streamPos - (pos + index);
  Inc(distance);
  // Byte *pby = _buffer+(size_t)_pos+index;
  pby := bufferOffset + pos + index;
  i   := 0;
  while (i < limit) and (bufferBase[pby + i] = bufferBase[pby + i - distance]) do
    Inc(i);
  Result := i;
end;

function TLZMALZInWindow.GetNumAvailableBytes(): integer;
begin
  Result := streamPos - pos;
end;

procedure TLZMALZInWindow.ReduceOffsets(const subvalue: integer);
begin
  bufferOffset := bufferOffset + subValue;
  posLimit := posLimit - subValue;
  pos := pos - subValue;
  streamPos := streamPos - subValue;
end;


{ TLZMALZBinTree }
constructor TLZMALZBinTree.Create();
begin
  inherited Create();
  cyclicBufferSize := 0;
  cutValue     := $FF;
  hashSizeSum  := 0;
  hashArray    := True;
  kNumHashDirectBytes := 0;
  kMinMatchCheck := 4;
  kFixHashsize := kHash2Size + kHash3Size;
end;

procedure TLZMALZBinTree.SetType(const aNumHashBytes: integer);
begin
  hashArray := (aNumHashBytes > 2);
  if hashArray then
  begin
    kNumHashDirectBytes := 0;
    kMinMatchCheck := 4;
    kFixHashSize := kHash2Size + kHash3Size;
  end
  else
  begin
    kNumHashDirectBytes := 2;
    kMinMatchCheck := 2 + 1;
    kFixHashSize := 0;
  end;
end;

procedure TLZMALZBinTree.Init();
var
  i: integer;
begin
  inherited Init();
  for i := 0 to hashSizeSum - 1 do
    hash[i] := kEmptyHashValue;
  cyclicBufferPos := 0;
  ReduceOffsets(-1);
end;

procedure TLZMALZBinTree.MovePos();
begin
  Inc(cyclicBufferPos);
  if cyclicBufferPos >= cyclicBufferSize then
    cyclicBufferPos := 0;
  inherited MovePos();
  if pos = kMaxValForNormalize then
    Normalize();
end;

function TLZMALZBinTree._Create(const historySize, keepAddBufferBefore,
  matchMaxLen, keepAddBufferAfter: integer): boolean;
var
  windowReservSize: integer;
  cyclicBufferSize: integer;
  hs: integer;
begin
  if historySize > kMaxValForNormalize - 256 then
  begin
    Result := False;
    exit;
  end;
  cutValue := 16 + (matchMaxLen shr 1);
  windowReservSize := (historySize + keepAddBufferBefore + matchMaxLen +
    keepAddBufferAfter) div 2 + 256;
  inherited _Create(historySize + keepAddBufferBefore,
    matchMaxLen + keepAddBufferAfter, windowReservSize);
  self.matchMaxLen := matchMaxLen;
  cyclicBufferSize := historySize + 1;
  if self.cyclicBufferSize <> cyclicBufferSize then
  begin
    self.cyclicBufferSize := cyclicBufferSize;
    SetLength(son, cyclicBufferSize * 2);
  end;
  hs := kBT2HashSize;
  if hashArray then
  begin
    hs := historySize - 1;
    hs := hs or (hs shr 1);
    hs := hs or (hs shr 2);
    hs := hs or (hs shr 4);
    hs := hs or (hs shr 8);
    hs := hs shr 1;
    hs := hs or $FFFF;
    if hs > (1 shl 24) then
      hs := hs shr 1;
    hashMask := hs;
    Inc(hs);
    hs := hs + kFixHashSize;
  end;
  if hs <> hashSizeSum then
  begin
    hashSizeSum := hs;
    SetLength(hash, hashSizeSum);
  end;
  Result := True;
end;

function TLZMALZBinTree.GetMatches(var distances: array of integer): integer;
var
  lenLimit: integer;
  offset, matchMinPos, cur, maxlen, hashvalue, hash2value, hash3value: integer;
  temp, curmatch, curmatch2, curmatch3, ptr0, ptr1, len0, len1, Count: integer;
  delta, cyclicpos, pby1, len: integer;
begin
  if pos + matchMaxLen <= streamPos then
    lenLimit := matchMaxLen
  else
  begin
    lenLimit := streamPos - pos;
    if lenLimit < kMinMatchCheck then
    begin
      MovePos();
      Result := 0;
      exit;
    end;
  end;
  offset := 0;
  if pos > cyclicBufferSize then
    matchMinPos := (pos - cyclicBufferSize)
  else
    matchMinPos := 0;
  cur := bufferOffset + pos;
  maxLen     := kStartMaxLen; // to avoid items for len < hashSize;
  hash2Value := 0;
  hash3Value := 0;

  if hashArray then
  begin
    temp      := crcTable[bufferBase[cur] and $FF] xor (bufferBase[cur + 1] and $FF);
    hash2Value := temp and (kHash2Size - 1);
    temp      := temp xor ((bufferBase[cur + 2] and $FF) shl 8);
    hash3Value := temp and (kHash3Size - 1);
    hashValue := (temp xor (crcTable[bufferBase[cur + 3] and $FF] shl 5)) and hashMask;
  end
  else
    hashValue := ((bufferBase[cur] and $FF) xor ((bufferBase[cur + 1] and $FF) shl 8));

  curMatch := hash[kFixHashSize + hashValue];
  if hashArray then
  begin
    curMatch2 := hash[hash2Value];
    curMatch3 := hash[kHash3Offset + hash3Value];
    hash[hash2Value] := pos;
    hash[kHash3Offset + hash3Value] := pos;
    if curMatch2 > matchMinPos then
      if bufferBase[bufferOffset + curMatch2] = bufferBase[cur] then
      begin
        maxLen := 2;
        distances[offset] := maxLen;
        Inc(offset);
        distances[offset] := pos - curMatch2 - 1;
        Inc(offset);
      end;
    if curMatch3 > matchMinPos then
      if bufferBase[bufferOffset + curMatch3] = bufferBase[cur] then
      begin
        if curMatch3 = curMatch2 then
          offset := offset - 2;
        maxLen := 3;
        distances[offset] := maxlen;
        Inc(offset);
        distances[offset] := pos - curMatch3 - 1;
        Inc(offset);
        curMatch2 := curMatch3;
      end;
    if (offset <> 0) and (curMatch2 = curMatch) then
    begin
      offset := offset - 2;
      maxLen := kStartMaxLen;
    end;
  end;

  hash[kFixHashSize + hashValue] := pos;

  ptr0 := (cyclicBufferPos shl 1) + 1;
  ptr1 := (cyclicBufferPos shl 1);

  len0 := kNumHashDirectBytes;
  len1 := len0;

  if kNumHashDirectBytes <> 0 then
  begin
    if (curMatch > matchMinPos) then
    begin
      if (bufferBase[bufferOffset + curMatch + kNumHashDirectBytes] <>
        bufferBase[cur + kNumHashDirectBytes]) then
      begin
        maxLen := kNumHashDirectBytes;
        distances[offset] := maxLen;
        Inc(offset);
        distances[offset] := pos - curMatch - 1;
        Inc(offset);
      end;
    end;
  end;

  Count := cutValue;
  while True do
  begin
    if (curMatch <= matchMinPos) or (Count = 0) then
    begin
      son[ptr1] := kEmptyHashValue;
      son[ptr0] := son[ptr1];
      break;
    end;
    Dec(Count);
    delta := pos - curMatch;
    if delta <= cyclicBufferPos then
      cyclicpos := (cyclicBufferPos - delta) shl 1
    else
      cyclicpos := (cyclicBufferPos - delta + cyclicBufferSize) shl 1;

    pby1 := bufferOffset + curMatch;
    len  := Min(len0, len1);
    if bufferBase[pby1 + len] = bufferBase[cur + len] then
    begin
      Inc(len);
      while len <> lenLimit do
      begin
        if bufferBase[pby1 + len] <> bufferBase[cur + len] then
          break;
        Inc(len);
      end;
      if maxLen < len then
      begin
        maxLen := len;
        distances[offset] := maxlen;
        Inc(offset);
        distances[offset] := delta - 1;
        Inc(offset);
        if len = lenLimit then
        begin
          son[ptr1] := son[cyclicPos];
          son[ptr0] := son[cyclicPos + 1];
          break;
        end;
      end;
    end;
    if (bufferBase[pby1 + len] and $FF) < (bufferBase[cur + len] and $FF) then
    begin
      son[ptr1] := curMatch;
      ptr1      := cyclicPos + 1;
      curMatch  := son[ptr1];
      len1      := len;
    end
    else
    begin
      son[ptr0] := curMatch;
      ptr0      := cyclicPos;
      curMatch  := son[ptr0];
      len0      := len;
    end;
  end;
  MovePos();
  Result := offset;
end;

procedure TLZMALZBinTree.Skip(num: integer);
var
  lenLimit, matchminpos, cur, hashvalue, temp: integer;
  hash2value, hash3value, curMatch: integer;
  ptr0, ptr1, len, len0, len1, Count, delta, cyclicpos, pby1: integer;
begin
  repeat
    if pos + matchMaxLen <= streamPos then
      lenLimit := matchMaxLen
    else
    begin
      lenLimit := streamPos - pos;
      if lenLimit < kMinMatchCheck then
      begin
        MovePos();
        Dec(num);
        continue;
      end;
    end;

    if pos > cyclicBufferSize then
      matchminpos := (pos - cyclicBufferSize)
    else
      matchminpos := 0;
    cur := bufferOffset + pos;

    if hashArray then
    begin
      temp      := crcTable[bufferBase[cur] and $FF] xor (bufferBase[cur + 1] and $FF);
      hash2Value := temp and (kHash2Size - 1);
      hash[hash2Value] := pos;
      temp      := temp xor ((bufferBase[cur + 2] and $FF) shl 8);
      hash3Value := temp and (kHash3Size - 1);
      hash[kHash3Offset + hash3Value] := pos;
      hashValue := (temp xor (crcTable[bufferBase[cur + 3] and $FF] shl 5)) and hashMask;
    end
    else
      hashValue := ((bufferBase[cur] and $FF) xor ((bufferBase[cur + 1] and $FF) shl 8));

    curMatch := hash[kFixHashSize + hashValue];
    hash[kFixHashSize + hashValue] := pos;

    ptr0 := (cyclicBufferPos shl 1) + 1;
    ptr1 := (cyclicBufferPos shl 1);

    len0 := kNumHashDirectBytes;
    len1 := kNumHashDirectBytes;

    Count := cutValue;
    while True do
    begin
      if (curMatch <= matchMinPos) or (Count = 0) then
      begin
        son[ptr1] := kEmptyHashValue;
        son[ptr0] := son[ptr1];
        break;
      end
      else
        Dec(Count);

      delta := pos - curMatch;
      if (delta <= cyclicBufferPos) then
        cyclicpos := (cyclicBufferPos - delta) shl 1
      else
        cyclicpos := (cyclicBufferPos - delta + cyclicBufferSize) shl 1;

      pby1 := bufferOffset + curMatch;
      len  := Min(len0, len1);
      if bufferBase[pby1 + len] = bufferBase[cur + len] then
      begin
        Inc(len);
        while len <> lenLimit do
        begin
          if bufferBase[pby1 + len] <> bufferBase[cur + len] then
            break;
          Inc(len);
        end;
        if len = lenLimit then
        begin
          son[ptr1] := son[cyclicPos];
          son[ptr0] := son[cyclicPos + 1];
          break;
        end;
      end;
      if (bufferBase[pby1 + len] and $FF) < (bufferBase[cur + len] and $FF) then
      begin
        son[ptr1] := curMatch;
        ptr1      := cyclicPos + 1;
        curMatch  := son[ptr1];
        len1      := len;
      end
      else
      begin
        son[ptr0] := curMatch;
        ptr0      := cyclicPos;
        curMatch  := son[ptr0];
        len0      := len;
      end;
    end;
    MovePos();
    Dec(num);
  until num = 0;
end;

procedure TLZMALZBinTree.NormalizeLinks(var items: array of integer;
  const numItems, subValue: integer);
var
  i, Value: integer;
begin
  for i := 0 to NumItems - 1 do
  begin
    Value := items[i];
    if Value <= subValue then
      Value := kEmptyHashValue
    else
      Value := Value - subValue;
    items[i] := Value;
  end;
end;

procedure TLZMALZBinTree.Normalize();
var
  subvalue: integer;
begin
  subValue := pos - cyclicBufferSize;
  NormalizeLinks(son, cyclicBufferSize * 2, subValue);
  NormalizeLinks(hash, hashSizeSum, subValue);
  ReduceOffsets(subValue);
end;

procedure TLZMALZBinTree.SetCutValue(const cutvalue: integer);
begin
  self.cutValue := cutValue;
end;


{ TLZMAEncoder }
constructor TLZMAEncoder.Create();
var
  kFastSlots, c, slotFast, j, k: integer;
begin
  inherited;
  kFastSlots := 22;
  c := 2;
  fFastPos[0] := 0;
  fFastPos[1] := 1;
  for slotFast := 2 to kFastSlots - 1 do
  begin
    k := (1 shl ((slotFast shr 1) - 1));
    for j := 0 to k - 1 do
    begin
      fFastPos[c] := slotFast;
      Inc(c);
    end;
  end;
  fState      := lzmaBaseU.StateInit();
  fMatchFinder := nil;
  fRangeEncoder := TLZMARangeEncoder.Create();
  fPosAlignEncoder := TLZMABitTreeEncoder.Create(lzmaBaseU.kNumAlignBits);
  fLenEncoder := TLZMALenPriceTableEncoder.Create();
  fRepMatchLenEncoder := TLZMALenPriceTableEncoder.Create();
  fLiteralEncoder := TLZMALiteralEncoder.Create();
  fNumFastBytes := kNumFastBytesDefault;
  fDistTableSize := (kDefaultDictionaryLogSize * 2);
  fPosStateBits := 2;
  fPosStateMask := (4 - 1);
  fNumLiteralPosStateBits := 0;
  fNumLiteralContextBits := 3;

  fDictionarySize := (1 shl kDefaultDictionaryLogSize);
  fDictionarySizePrev := -1;
  fNumFastBytesPrev := -1;
  fMatchFinderType := EMatchFinderTypeBT4;
  fWriteEndMark := False;

  fNeedReleaseMFStream := False;
end;

destructor TLZMAEncoder.Destroy();
var
  i: integer;
begin
  FreeAndNil(fRangeEncoder);
  FreeAndNil(fPosAlignEncoder);
  FreeAndNil(fLenEncoder);
  FreeAndNil(fRepMatchLenEncoder);
  FreeAndNil(fLiteralEncoder);
  FreeAndNil(fMatchFinder);
  for i := 0 to kNumOpts - 1 do
    FreeAndNil(fOptimum[i]);
  for i := 0 to lzmaBaseU.kNumLenToPosStates - 1 do
    FreeAndNil(fPosSlotEncoder[i]);
  inherited;
end;

procedure TLZMAEncoder._Create();
var
  bt: TLZMALZBinTree;
  numHashBytes, i: integer;
begin
  if fMatchFinder = nil then
  begin
    bt := TLZMALZBinTree.Create();
    numHashBytes := 4;
    if fMatchFinderType = EMatchFinderTypeBT2 then
      numHashBytes := 2;
    bt.SetType(numHashBytes);
    fMatchFinder := bt;
  end;
  fLiteralEncoder._Create(fNumLiteralPosStateBits, fNumLiteralContextBits);

  if (fDictionarySize = fDictionarySizePrev) and
    (fNumFastBytesPrev = fNumFastBytes) then
    exit;
  fMatchFinder._Create(fDictionarySize, kNumOpts, fNumFastBytes,
    lzmaBaseU.kMatchMaxLen + 1);
  fDictionarySizePrev := fDictionarySize;
  fNumFastBytesPrev   := fNumFastBytes;

  for i := 0 to kNumOpts - 1 do
    fOptimum[i] := TLZMAOptimal.Create();
  for i := 0 to lzmaBaseU.kNumLenToPosStates - 1 do
    fPosSlotEncoder[i] := TLZMABitTreeEncoder.Create(lzmaBaseU.kNumPosSlotBits);
end;

function TLZMAEncoder.GetPosSlot(const pos: integer): integer;
begin
  if pos < (1 shl 11) then
    Result := fFastPos[pos]
  else
  if pos < (1 shl 21) then
    Result := (fFastPos[pos shr 10] + 20)
  else
    Result := (fFastPos[pos shr 20] + 40);
end;

function TLZMAEncoder.GetPosSlot2(const pos: integer): integer;
begin
  if pos < (1 shl 17) then
    Result := (fFastPos[pos shr 6] + 12)
  else
  if pos < (1 shl 27) then
    Result := (fFastPos[pos shr 16] + 32)
  else
    Result := (fFastPos[pos shr 26] + 52);
end;

procedure TLZMAEncoder.BaseInit();
var
  i: integer;
begin
  fState := lzmaBaseU.StateInit();
  fPreviousByte := 0;
  for i := 0 to lzmaBaseU.kNumRepDistances - 1 do
    fRepDistances[i] := 0;
end;

procedure TLZMAEncoder.SetWriteEndMarkerMode(const writeEndMarker: boolean);
begin
  fWriteEndMark := writeEndMarker;
end;

procedure TLZMAEncoder.Init();
var
  i: integer;
begin
  BaseInit();
  fRangeEncoder.Init();

  XInitBitModels(fIsMatch);
  XInitBitModels(fIsRep0Long);
  XInitBitModels(fIsRep);
  XInitBitModels(fIsRepG0);
  XInitBitModels(fIsRepG1);
  XInitBitModels(fIsRepG2);
  XInitBitModels(fPosEncoders);

  fLiteralEncoder.Init();
  for i := 0 to lzmaBaseU.kNumLenToPosStates - 1 do
    fPosSlotEncoder[i].Init();

  fLenEncoder.Init(1 shl fPosStateBits);
  fRepMatchLenEncoder.Init(1 shl fPosStateBits);

  fPosAlignEncoder.Init();

  fLongestMatchWasFound := False;
  fOptimumEndIndex      := 0;
  fOptimumCurrentIndex  := 0;
  fAdditionalOffset     := 0;
end;

function TLZMAEncoder.ReadMatchDistances(): integer;
var
  lenRes: integer;
begin
  lenRes := 0;
  fNumDistancePairs := fMatchFinder.GetMatches(fMatchDistances);

  if fNumDistancePairs > 0 then
  begin
    lenRes := fMatchDistances[fNumDistancePairs - 2];
    if lenRes = fNumFastBytes then
      lenRes := lenRes + fMatchFinder.GetMatchLen(lenRes - 1,
        fMatchDistances[fNumDistancePairs - 1], lzmaBaseU.kMatchMaxLen - lenRes);
  end;
  Inc(fAdditionalOffset);
  Result := lenRes;
end;

procedure TLZMAEncoder.MovePos(const num: integer);
begin
  if num > 0 then
  begin
    fMatchFinder.Skip(num);
    fAdditionalOffset := fAdditionalOffset + num;
  end;
end;

function TLZMAEncoder.GetRepLen1Price(const state, posState: integer): integer;
begin
  Result := rangeEncoder.GetPrice0(fIsRepG0[state]) +
    rangeEncoder.GetPrice0(fIsRep0Long[(state shl lzmaBaseU.kNumPosStatesBitsMax) +
    posState]);
end;

function TLZMAEncoder.GetPureRepPrice(
  const repIndex, state, posState: integer): integer;
var
  price: integer;
begin
  if repIndex = 0 then
  begin
    price := rangeEncoder.GetPrice0(fIsRepG0[state]);
    price := price + rangeEncoder.GetPrice1(
      fIsRep0Long[(state shl lzmaBaseU.kNumPosStatesBitsMax) + posState]);
  end
  else
  begin
    price := rangeEncoder.GetPrice1(fIsRepG0[state]);
    if repIndex = 1 then
      price := price + rangeEncoder.GetPrice0(fIsRepG1[state])
    else
    begin
      price := price + rangeEncoder.GetPrice1(fIsRepG1[state]);
      price := price + rangeEncoder.GetPrice(fIsRepG2[state], repIndex - 2);
    end;
  end;
  Result := price;
end;

function TLZMAEncoder.GetRepPrice(const repIndex, len, state, posState:
  integer): integer;
var
  price: integer;
begin
  price  := fRepMatchLenEncoder.GetPrice(len - lzmaBaseU.kMatchMinLen, posState);
  Result := price + GetPureRepPrice(repIndex, state, posState);
end;

function TLZMAEncoder.GetPosLenPrice(const pos, len, posState: integer): integer;
var
  price, lenToPosState: integer;
begin
  lenToPosState := lzmaBaseU.GetLenToPosState(len);
  if pos < lzmaBaseU.kNumFullDistances then
    price := fDistancesPrices[(lenToPosState * lzmaBaseU.kNumFullDistances) + pos]
  else
    price := fPosSlotPrices[(lenToPosState shl lzmaBaseU.kNumPosSlotBits) +
      GetPosSlot2(pos)] + fAlignPrices[pos and lzmaBaseU.kAlignMask];
  Result := price + fLenEncoder.GetPrice(len - lzmaBaseU.kMatchMinLen, posState);
end;

function TLZMAEncoder.Backward(cur: integer): integer;
var
  posMem, backMem, posPrev, backCur: integer;
begin
  fOptimumEndIndex := cur;
  posMem  := fOptimum[cur].posPrev;
  backMem := fOptimum[cur].backPrev;
  repeat
    if fOptimum[cur].prev1IsChar then
    begin
      fOptimum[posMem].MakeAsChar();
      fOptimum[posMem].posPrev := posMem - 1;
      if fOptimum[cur].prev2 then
      begin
        fOptimum[posMem - 1].prev1IsChar := False;
        fOptimum[posMem - 1].posPrev     := fOptimum[cur].posPrev2;
        fOptimum[posMem - 1].backPrev    := fOptimum[cur].backPrev2;
      end;
    end;
    posPrev := posMem;
    backCur := backMem;

    backMem := fOptimum[posPrev].backPrev;
    posMem  := fOptimum[posPrev].posPrev;

    fOptimum[posPrev].backPrev := backCur;
    fOptimum[posPrev].posPrev := cur;
    cur := posPrev;
  until not (cur > 0);
  backRes := fOptimum[0].backPrev;
  fOptimumCurrentIndex := fOptimum[0].posPrev;
  Result  := fOptimumCurrentIndex;
end;

function TLZMAEncoder.GetOptimum(position: integer): integer;
var
  lenRes, lenMain, numDistancePairs, numAvailableBytes, repMaxIndex, i: integer;
  matchPrice, repMatchPrice, shortRepPrice, lenEnd, len, repLen, price: integer;
  curAndLenPrice, normalMatchPrice, offs, distance, cur, newLen: integer;
  posPrev, state, pos, curPrice, curAnd1Price, numAvailableBytesFull: integer;
  lenTest2, t, state2, posStateNext, nextRepMatchPrice, offset: integer;
  startLen, repIndex, lenTest, lenTestTemp, curAndLenCharPrice: integer;
  nextMatchPrice, curBack: integer;
  optimum, opt, nextOptimum: TLZMAOptimal;
  currentByte, matchByte, posState: byte;
  nextIsChar: boolean;
begin
  if fOptimumEndIndex <> fOptimumCurrentIndex then
  begin
    lenRes  := fOptimum[fOptimumCurrentIndex].posPrev - fOptimumCurrentIndex;
    backRes := fOptimum[fOptimumCurrentIndex].backPrev;
    fOptimumCurrentIndex := fOptimum[fOptimumCurrentIndex].posPrev;
    Result  := lenRes;
    exit;
  end;//if optimumendindex
  fOptimumCurrentIndex := 0;
  fOptimumEndIndex     := 0;

  if not fLongestMatchWasFound then
    lenMain := ReadMatchDistances()
  else
  begin //if not longest
    lenMain := fLongestMatchLength;
    fLongestMatchWasFound := False;
  end;//if not longest else
  numDistancePairs := fNumDistancePairs;

  numAvailableBytes := fMatchFinder.GetNumAvailableBytes + 1;
  if numAvailableBytes < 2 then
  begin
    backRes := -1;
    Result  := 1;
    exit;
  end;//if numavailable
  //???if numAvailableBytes > lzmaBaseU.kMatchMaxLen then numAvailableBytes := lzmaBaseU.kMatchMaxLen;

  repMaxIndex := 0;
  for i := 0 to lzmaBaseU.kNumRepDistances - 1 do
  begin
    reps[i]    := fRepDistances[i];
    repLens[i] := fMatchFinder.GetMatchLen(0 - 1, reps[i], lzmaBaseU.kMatchMaxLen);
    if repLens[i] > repLens[repMaxIndex] then
      repMaxIndex := i;
  end;//for i
  if repLens[repMaxIndex] >= fNumFastBytes then
  begin
    backRes := repMaxIndex;
    lenRes  := repLens[repMaxIndex];
    MovePos(lenRes - 1);
    Result := lenRes;
    exit;
  end;//if replens[]

  if lenMain >= fNumFastBytes then
  begin
    backRes := fMatchDistances[numDistancePairs - 1] + lzmaBaseU.kNumRepDistances;
    MovePos(lenMain - 1);
    Result := lenMain;
    exit;
  end;//if lenMain

  currentByte := fMatchFinder.GetIndexByte(0 - 1);
  matchByte   := fMatchFinder.GetIndexByte(0 - fRepDistances[0] - 1 - 1);

  if (lenMain < 2) and (currentByte <> matchByte) and (repLens[repMaxIndex] < 2) then
  begin
    backRes := -1;
    Result  := 1;
    exit;
  end;//if lenmain<2

  fOptimum[0].state := fState;
  posState := (position and fPosStateMask);
  fOptimum[1].price := rangeEncoder.GetPrice0(
    fIsMatch[(fState shl lzmaBaseU.kNumPosStatesBitsMax) + posState]) +
    fLiteralEncoder.GetSubCoder(position, fPreviousByte).GetPrice(not
    lzmaBaseU.StateIsCharState(fState), matchByte, currentByte);
  fOptimum[1].MakeAsChar();

  matchPrice    := rangeEncoder.GetPrice1(
    fIsMatch[(fState shl lzmaBaseU.kNumPosStatesBitsMax) + posState]);
  repMatchPrice := matchPrice + rangeEncoder.GetPrice1(fIsRep[fState]);

  if matchByte = currentByte then
  begin
    shortRepPrice := repMatchPrice + GetRepLen1Price(fState, posState);
    if shortRepPrice < fOptimum[1].price then
    begin
      fOptimum[1].price := shortRepPrice;
      fOptimum[1].MakeAsShortRep;
    end;//if shortrepprice
  end;//if matchbyte

  if lenMain >= repLens[repMaxIndex] then
    lenEnd := lenMain
  else
    lenEnd := repLens[repMaxIndex];

  if lenEnd < 2 then
  begin
    backRes := fOptimum[1].backPrev;
    Result  := 1;
    exit;
  end;//if lenend<2

  fOptimum[1].posPrev := 0;
  fOptimum[0].backs0 := reps[0];
  fOptimum[0].backs1 := reps[1];
  fOptimum[0].backs2 := reps[2];
  fOptimum[0].backs3 := reps[3];
  len := lenEnd;
  repeat
    fOptimum[len].price := kIfinityPrice;
    Dec(len);
  until not (len >= 2);

  for i := 0 to lzmaBaseU.kNumRepDistances - 1 do
  begin
    repLen := repLens[i];
    if repLen < 2 then
      continue;
    price := repMatchPrice + GetPureRepPrice(i, fState, posState);
    repeat
      curAndLenPrice := price + fRepMatchLenEncoder.GetPrice(repLen - 2, posState);
      optimum := fOptimum[repLen];
      if curAndLenPrice < optimum.price then
      begin
        optimum.price    := curAndLenPrice;
        optimum.posPrev  := 0;
        optimum.backPrev := i;
        optimum.prev1IsChar := False;
      end;//if curandlenprice
      Dec(replen);
    until not (repLen >= 2);
  end;//for i

  normalMatchPrice := matchPrice + rangeEncoder.GetPrice0(fIsRep[fState]);

  if repLens[0] >= 2 then
    len := repLens[0] + 1
  else
    len := 2;

  if len <= lenMain then
  begin
    offs := 0;
    while len > fMatchDistances[offs] do
      Inc(offs, 2);
    while True do
    begin
      distance := fMatchDistances[offs + 1];
      curAndLenPrice := normalMatchPrice + GetPosLenPrice(distance, len, posState);
      optimum  := fOptimum[len];
      if curAndLenPrice < optimum.price then
      begin
        optimum.price    := curAndLenPrice;
        optimum.posPrev  := 0;
        optimum.backPrev := distance + lzmaBaseU.kNumRepDistances;
        optimum.prev1IsChar := False;
      end;//if curlenandprice
      if len = fMatchDistances[offs] then
      begin
        offs := offs + 2;
        if offs = numDistancePairs then
          break;
      end;//if len=_match
      Inc(len);
    end;//while (true)
  end;//if len<=lenmain

  cur := 0;
  while True do
  begin
    Inc(cur);
    if cur = lenEnd then
    begin
      Result := Backward(cur);
      exit;
    end;//if cur=lenEnd
    newLen := ReadMatchDistances;
    numDistancePairs := fNumDistancePairs;
    if newLen >= fNumFastBytes then
    begin
      fLongestMatchLength := newLen;
      fLongestMatchWasFound := True;
      Result := Backward(cur);
      exit;
    end;//if newlen=_numfast
    Inc(position);
    posPrev := fOptimum[cur].posPrev;
    if fOptimum[cur].prev1IsChar then
    begin
      Dec(posPrev);
      if fOptimum[cur].prev2 then
      begin
        state := fOptimum[fOptimum[cur].posPrev2].state;
        if fOptimum[cur].backPrev2 < lzmaBaseU.kNumRepDistances then
          state := lzmaBaseU.StateUpdateRep(state)
        else
          state := lzmaBaseU.StateUpdateMatch(state);
      end//if fOptimum[cur].prev2
      else
        state := fOptimum[posPrev].state;
      state := lzmaBaseU.StateUpdateChar(state);
    end//if fOptimum[cur].prev1IsChar
    else
      state := fOptimum[posPrev].state;
    if posPrev = cur - 1 then
    begin
      if fOptimum[cur].IsShortRep then
        state := lzmaBaseU.StateUpdateShortRep(state)
      else
        state := lzmaBaseU.StateUpdateChar(state);
    end //if posPrev = cur-1
    else
    begin
      if fOptimum[cur].prev1IsChar and fOptimum[cur].prev2 then
      begin
        posPrev := fOptimum[cur].posPrev2;
        pos     := fOptimum[cur].backPrev2;
        state   := lzmaBaseU.StateUpdateRep(state);
      end//if fOptimum[cur].prev1IsChar
      else
      begin
        pos := fOptimum[cur].backPrev;
        if pos < lzmaBaseU.kNumRepDistances then
          state := lzmaBaseU.StateUpdateRep(state)
        else
          state := lzmaBaseU.StateUpdateMatch(state);
      end;//if else  fOptimum[cur].prev1IsChar
      opt := fOptimum[posPrev];
      if pos < lzmaBaseU.kNumRepDistances then
      begin
        if pos = 0 then
        begin
          reps[0] := opt.backs0;
          reps[1] := opt.backs1;
          reps[2] := opt.backs2;
          reps[3] := opt.backs3;
        end//if pos=0
        else
        if pos = 1 then
        begin
          reps[0] := opt.backs1;
          reps[1] := opt.backs0;
          reps[2] := opt.backs2;
          reps[3] := opt.backs3;
        end //if pos=1
        else
        if pos = 2 then
        begin
          reps[0] := opt.backs2;
          reps[1] := opt.backs0;
          reps[2] := opt.backs1;
          reps[3] := opt.backs3;
        end//if pos=2
        else
        begin
          reps[0] := opt.backs3;
          reps[1] := opt.backs0;
          reps[2] := opt.backs1;
          reps[3] := opt.backs2;
        end;//else if pos=
      end// if pos < lzmaBaseU.kNumRepDistances
      else
      begin
        reps[0] := (pos - lzmaBaseU.kNumRepDistances);
        reps[1] := opt.backs0;
        reps[2] := opt.backs1;
        reps[3] := opt.backs2;
      end;//if else pos < lzmaBaseU.kNumRepDistances
    end;//if else posPrev = cur-1
    fOptimum[cur].state := state;
    fOptimum[cur].backs0 := reps[0];
    fOptimum[cur].backs1 := reps[1];
    fOptimum[cur].backs2 := reps[2];
    fOptimum[cur].backs3 := reps[3];
    curPrice := fOptimum[cur].price;

    currentByte := fMatchFinder.GetIndexByte(0 - 1);
    matchByte   := fMatchFinder.GetIndexByte(0 - reps[0] - 1 - 1);

    posState := (position and fPosStateMask);

    curAnd1Price := curPrice + rangeEncoder.GetPrice0(
      fIsMatch[(state shl lzmaBaseU.kNumPosStatesBitsMax) + posState]) +
      fLiteralEncoder.GetSubCoder(position, fMatchFinder.GetIndexByte(0 - 2)).GetPrice(not lzmaBaseU.StateIsCharState(state), matchByte, currentByte);

    nextOptimum := fOptimum[cur + 1];

    nextIsChar := False;
    if curAnd1Price < nextOptimum.price then
    begin
      nextOptimum.price   := curAnd1Price;
      nextOptimum.posPrev := cur;
      nextOptimum.MakeAsChar;
      nextIsChar := True;
    end;//if curand1price

    matchPrice    := curPrice + rangeEncoder.GetPrice1(
      fIsMatch[(state shl lzmaBaseU.kNumPosStatesBitsMax) + posState]);
    repMatchPrice := matchPrice + rangeEncoder.GetPrice1(fIsRep[state]);

    if (matchByte = currentByte) and
      (not ((nextOptimum.posPrev < cur) and (nextOptimum.backPrev = 0))) then
    begin
      shortRepPrice := repMatchPrice + GetRepLen1Price(state, posState);
      if shortRepPrice <= nextOptimum.price then
      begin
        nextOptimum.price   := shortRepPrice;
        nextOptimum.posPrev := cur;
        nextOptimum.MakeAsShortRep;
        nextIsChar := True;
      end;//if shortRepPrice <= nextOptimum.price
    end;//if (matchByte = currentByte) and

    numAvailableBytesFull := fMatchFinder.GetNumAvailableBytes + 1;
    numAvailableBytesFull := Min(kNumOpts - 1 - cur, numAvailableBytesFull);
    numAvailableBytes     := numAvailableBytesFull;

    if numAvailableBytes < 2 then
      continue;
    if numAvailableBytes > fNumFastBytes then
      numAvailableBytes := fNumFastBytes;
    if (not nextIsChar) and (matchByte <> currentByte) then
    begin
      // try Literal+rep0
      t := Min(numAvailableBytesFull - 1, fNumFastBytes);
      lenTest2 := fMatchFinder.GetMatchLen(0, reps[0], t);
      if lenTest2 >= 2 then
      begin
        state2 := lzmaBaseU.StateUpdateChar(state);
        posStateNext := (position + 1) and fPosStateMask;
        nextRepMatchPrice := curAnd1Price + rangeEncoder.GetPrice1(
          fIsMatch[(state2 shl lzmaBaseU.kNumPosStatesBitsMax) + posStateNext]) +
          rangeEncoder.GetPrice1(fIsRep[state2]);

        offset := cur + 1 + lenTest2;
        while lenEnd < offset do
        begin
          Inc(lenEnd);
          fOptimum[lenEnd].price := kIfinityPrice;
        end;//while lenend
        curAndLenPrice := nextRepMatchPrice +
          GetRepPrice(0, lenTest2, state2, posStateNext);
        optimum := fOptimum[offset];
        if curAndLenPrice < optimum.price then
        begin
          optimum.price    := curAndLenPrice;
          optimum.posPrev  := cur + 1;
          optimum.backPrev := 0;
          optimum.prev1IsChar := True;
          optimum.prev2    := False;
        end;//if curandlenprice
      end;//if lentest
    end;//if not nextischar and ...

    startLen := 2; // speed optimization
    for repIndex := 0 to lzmaBaseU.kNumRepDistances - 1 do
    begin
      lenTest := fMatchFinder.GetMatchLen(0 - 1, reps[repIndex], numAvailableBytes);
      if lenTest < 2 then
        continue;
      lenTestTemp := lenTest;
      repeat
        while lenEnd < cur + lenTest do
        begin
          Inc(lenEnd);
          fOptimum[lenEnd].price := kIfinityPrice;
        end;//while lenEnd
        curAndLenPrice := repMatchPrice + GetRepPrice(repIndex, lenTest,
          state, posState);
        optimum := fOptimum[cur + lenTest];
        if curAndLenPrice < optimum.price then
        begin
          optimum.price    := curAndLenPrice;
          optimum.posPrev  := cur;
          optimum.backPrev := repIndex;
          optimum.prev1IsChar := False;
        end;//if curandlen
        Dec(lenTest);
      until not (lenTest >= 2);
      lenTest := lenTestTemp;

      if repIndex = 0 then
        startLen := lenTest + 1;

      // if (_maxMode)
      if lenTest < numAvailableBytesFull then
      begin
        t := Min(numAvailableBytesFull - 1 - lenTest, fNumFastBytes);
        lenTest2 := fMatchFinder.GetMatchLen(lenTest, reps[repIndex], t);
        if lenTest2 >= 2 then
        begin
          state2 := lzmaBaseU.StateUpdateRep(state);
          posStateNext := (position + lenTest) and fPosStateMask;
          curAndLenCharPrice :=
            repMatchPrice + GetRepPrice(repIndex, lenTest, state, posState) +
            rangeEncoder.GetPrice0(
            fIsMatch[(state2 shl lzmaBaseU.kNumPosStatesBitsMax) + posStateNext]) +
            fLiteralEncoder.GetSubCoder(position + lenTest,
            fMatchFinder.GetIndexByte(lenTest - 1 - 1)).GetPrice(True,
            fMatchFinder.GetIndexByte(lenTest - 1 - (reps[repIndex] + 1)),
            fMatchFinder.GetIndexByte(lenTest - 1));
          state2 := lzmaBaseU.StateUpdateChar(state2);
          posStateNext := (position + lenTest + 1) and fPosStateMask;
          nextMatchPrice := curAndLenCharPrice + rangeEncoder.GetPrice1(
            fIsMatch[(state2 shl lzmaBaseU.kNumPosStatesBitsMax) + posStateNext]);
          nextRepMatchPrice := nextMatchPrice + rangeEncoder.GetPrice1(fIsRep[state2]);

          // for(; lenTest2 >= 2; lenTest2--)
          offset := lenTest + 1 + lenTest2;
          while lenEnd < cur + offset do
          begin
            Inc(lenEnd);
            fOptimum[lenEnd].price := kIfinityPrice;
          end;//while lenEnd
          curAndLenPrice := nextRepMatchPrice +
            GetRepPrice(0, lenTest2, state2, posStateNext);
          optimum := fOptimum[cur + offset];
          if curAndLenPrice < optimum.price then
          begin
            optimum.price     := curAndLenPrice;
            optimum.posPrev   := cur + lenTest + 1;
            optimum.backPrev  := 0;
            optimum.prev1IsChar := True;
            optimum.prev2     := True;
            optimum.posPrev2  := cur;
            optimum.backPrev2 := repIndex;
          end;//if curAndLenPrice < optimum.price
        end;//if lenTest2 >= 2
      end;//if lenTest < numAvailableBytesFull
    end;//for repIndex

    if newLen > numAvailableBytes then
    begin
      newLen := numAvailableBytes;
      numDistancePairs := 0;
      while newLen > fMatchDistances[numDistancePairs] do
        numDistancePairs := numDistancePairs + 2;
      fMatchDistances[numDistancePairs] := newLen;
      numDistancePairs := numDistancePairs + 2;
    end;//if newLen > numAvailableBytes
    if newLen >= startLen then
    begin
      normalMatchPrice := matchPrice + rangeEncoder.GetPrice0(fIsRep[state]);
      while lenEnd < cur + newLen do
      begin
        Inc(lenEnd);
        fOptimum[lenEnd].price := kIfinityPrice;
      end;//while lenEnd
      offs := 0;
      while startLen > fMatchDistances[offs] do
        Inc(offs, 2);
      lenTest := startLen;
      while True do
      begin
        curBack := fMatchDistances[offs + 1];
        curAndLenPrice := normalMatchPrice + GetPosLenPrice(curBack, lenTest, posState);
        optimum := fOptimum[cur + lenTest];
        if curAndLenPrice < optimum.price then
        begin
          optimum.price    := curAndLenPrice;
          optimum.posPrev  := cur;
          optimum.backPrev := curBack + lzmaBaseU.kNumRepDistances;
          optimum.prev1IsChar := False;
        end;//if curAndLenPrice < optimum.price
        if lenTest = fMatchDistances[offs] then
        begin
          if lenTest < numAvailableBytesFull then
          begin
            t := Min(numAvailableBytesFull - 1 - lenTest, fNumFastBytes);
            lenTest2 := fMatchFinder.GetMatchLen(lenTest, curBack, t);
            if lenTest2 >= 2 then
            begin
              state2 := lzmaBaseU.StateUpdateMatch(state);
              posStateNext := (position + lenTest) and fPosStateMask;
              curAndLenCharPrice :=
                curAndLenPrice + rangeEncoder.GetPrice0(
                fIsMatch[(state2 shl lzmaBaseU.kNumPosStatesBitsMax) + posStateNext]) +
                fLiteralEncoder.GetSubCoder(position + lenTest,
                fMatchFinder.GetIndexByte(lenTest - 1 - 1)).GetPrice(True,
                fMatchFinder.GetIndexByte(lenTest - (curBack + 1) - 1),
                fMatchFinder.GetIndexByte(lenTest - 1));
              state2 := lzmaBaseU.StateUpdateChar(state2);
              posStateNext := (position + lenTest + 1) and fPosStateMask;
              nextMatchPrice :=
                curAndLenCharPrice + rangeEncoder.GetPrice1(
                fIsMatch[(state2 shl lzmaBaseU.kNumPosStatesBitsMax) + posStateNext]);
              nextRepMatchPrice :=
                nextMatchPrice + rangeEncoder.GetPrice1(fIsRep[state2]);
              offset := lenTest + 1 + lenTest2;
              while lenEnd < cur + offset do
              begin
                Inc(lenEnd);
                fOptimum[lenEnd].price := kIfinityPrice;
              end;//while lenEnd
              curAndLenPrice :=
                nextRepMatchPrice + GetRepPrice(0, lenTest2, state2, posStateNext);
              optimum := fOptimum[cur + offset];
              if curAndLenPrice < optimum.price then
              begin
                optimum.price     := curAndLenPrice;
                optimum.posPrev   := cur + lenTest + 1;
                optimum.backPrev  := 0;
                optimum.prev1IsChar := True;
                optimum.prev2     := True;
                optimum.posPrev2  := cur;
                optimum.backPrev2 := curBack + lzmaBaseU.kNumRepDistances;
              end;//if curAndLenPrice < optimum.price
            end;//if lenTest2 >= 2
          end;//lenTest < numAvailableBytesFull
          Inc(offs, 2);
          if offs = numDistancePairs then
            break;
        end;//if lenTest = fMatchDistances[offs]
        Inc(lenTest);
      end;//while(true)
    end;//if newLen >= startLen
  end;//while (true)
end;

function TLZMAEncoder.ChangePair(const smallDist, bigDist: integer): boolean;
var
  kDif: integer;
begin
  kDif   := 7;
  Result := (smallDist < (1 shl (32 - kDif))) and (bigDist >= (smallDist shl kDif));
end;

procedure TLZMAEncoder.WriteEndMarker(const posState: integer);
var
  len, posSlot, lenToPosState, footerBits, posReduced: integer;
begin
  if not fWriteEndMark then
    exit;
  fRangeEncoder.Encode(fIsMatch, (fState shl lzmaBaseU.kNumPosStatesBitsMax) +
    posState, 1);
  fRangeEncoder.Encode(fIsRep, fState, 0);
  fState := lzmaBaseU.StateUpdateMatch(fState);
  len    := lzmaBaseU.kMatchMinLen;
  fLenEncoder.Encode(fRangeEncoder, len - lzmaBaseU.kMatchMinLen, posState);
  posSlot := (1 shl lzmaBaseU.kNumPosSlotBits) - 1;
  lenToPosState := lzmaBaseU.GetLenToPosState(len);
  fPosSlotEncoder[lenToPosState].Encode(fRangeEncoder, posSlot);
  footerBits := 30;
  posReduced := (1 shl footerBits) - 1;
  fRangeEncoder.EncodeDirectBits(posReduced shr lzmaBaseU.kNumAlignBits,
    footerBits - lzmaBaseU.kNumAlignBits);
  fPosAlignEncoder.ReverseEncode(fRangeEncoder, posReduced and lzmaBaseU.kAlignMask);
end;

procedure TLZMAEncoder.Flush(const nowPos: integer);
begin
  ReleaseMFStream();
  WriteEndMarker(nowPos and fPosStateMask);
  fRangeEncoder.FlushData();
  fRangeEncoder.FlushStream();
end;

procedure TLZMAEncoder.CodeOneBlock(var inSize, outSize: int64; var finished: boolean);
var
  progressPosValuePrev: int64;
  posState, len, pos, complexState, distance, i, posSlot, lenToPosState: integer;
  footerBits, baseVal, posReduced: integer;
  curByte, matchByte: byte;
  subcoder: TLZMAEncoder2;
begin
  inSize   := 0;
  outSize  := 0;
  finished := True;
  if fInStream <> nil then
  begin
    fMatchFinder.SeTMemoryStream(fInStream);
    fMatchFinder.Init();
    fNeedReleaseMFStream := True;
    fInStream := nil;
  end;

  if fFinished then
    exit;
  fFinished := True;

  progressPosValuePrev := nowPos64;
  if nowPos64 = 0 then
  begin
    if fMatchFinder.GetNumAvailableBytes = 0 then
    begin
      Flush(nowPos64);
      exit;
    end;
    ReadMatchDistances();
    posState := integer(nowPos64) and fPosStateMask;
    fRangeEncoder.Encode(fIsMatch, (fState shl lzmaBaseU.kNumPosStatesBitsMax) +
      posState, 0);
    fState  := lzmaBaseU.StateUpdateChar(fState);
    curByte := fMatchFinder.GetIndexByte(0 - fAdditionalOffset);
    fLiteralEncoder.GetSubCoder(integer(nowPos64), fPreviousByte).Encode(
      fRangeEncoder, curByte);
    fPreviousByte := curByte;
    Dec(fAdditionalOffset);
    Inc(nowPos64);
  end;
  if fMatchFinder.GetNumAvailableBytes = 0 then
  begin
    Flush(integer(nowPos64));
    exit;
  end;
  while True do
  begin
    len      := GetOptimum(integer(nowPos64));
    pos      := backRes;
    posState := integer(nowPos64) and fPosStateMask;
    complexState := (fState shl lzmaBaseU.kNumPosStatesBitsMax) + posState;
    if (len = 1) and (pos = -1) then
    begin
      fRangeEncoder.Encode(fIsMatch, complexState, 0);
      curByte  := fMatchFinder.GetIndexByte(0 - fAdditionalOffset);
      subCoder := fLiteralEncoder.GetSubCoder(integer(nowPos64), fPreviousByte);
      if not lzmaBaseU.StateIsCharState(fState) then
      begin
        matchByte := fMatchFinder.GetIndexByte(0 - fRepDistances[0] -
          1 - fAdditionalOffset);
        subCoder.EncodeMatched(fRangeEncoder, matchByte, curByte);
      end
      else
        subCoder.Encode(fRangeEncoder, curByte);
      fPreviousByte := curByte;
      fState := lzmaBaseU.StateUpdateChar(fState);
    end
    else
    begin
      fRangeEncoder.Encode(fIsMatch, complexState, 1);
      if pos < lzmaBaseU.kNumRepDistances then
      begin
        fRangeEncoder.Encode(fIsRep, fState, 1);
        if pos = 0 then
        begin
          fRangeEncoder.Encode(fIsRepG0, fState, 0);
          if len = 1 then
            fRangeEncoder.Encode(fIsRep0Long, complexState, 0)
          else
            fRangeEncoder.Encode(fIsRep0Long, complexState, 1);
        end
        else
        begin
          fRangeEncoder.Encode(fIsRepG0, fState, 1);
          if pos = 1 then
            fRangeEncoder.Encode(fIsRepG1, fState, 0)
          else
          begin
            fRangeEncoder.Encode(fIsRepG1, fState, 1);
            fRangeEncoder.Encode(fIsRepG2, fState, pos - 2);
          end;
        end;
        if len = 1 then
          fState := lzmaBaseU.StateUpdateShortRep(fState)
        else
        begin
          fRepMatchLenEncoder.Encode(fRangeEncoder,
            len - lzmaBaseU.kMatchMinLen, posState);
          fState := lzmaBaseU.StateUpdateRep(fState);
        end;
        distance := fRepDistances[pos];
        if pos <> 0 then
        begin
          for i := pos downto 1 do
            fRepDistances[i] := fRepDistances[i - 1];
          fRepDistances[0] := distance;
        end;
      end
      else
      begin
        fRangeEncoder.Encode(fIsRep, fState, 0);
        fState := lzmaBaseU.StateUpdateMatch(fState);
        fLenEncoder.Encode(fRangeEncoder, len - lzmaBaseU.kMatchMinLen, posState);
        pos     := pos - lzmaBaseU.kNumRepDistances;
        posSlot := GetPosSlot(pos);
        lenToPosState := lzmaBaseU.GetLenToPosState(len);
        fPosSlotEncoder[lenToPosState].Encode(fRangeEncoder, posSlot);
        if posSlot >= lzmaBaseU.kStartPosModelIndex then
        begin
          footerBits := integer((posSlot shr 1) - 1);
          baseVal    := ((2 or (posSlot and 1)) shl footerBits);
          posReduced := pos - baseVal;
          if posSlot < lzmaBaseU.kEndPosModelIndex then
            XReverseEncode(fPosEncoders,
              baseVal - posSlot - 1, fRangeEncoder, footerBits, posReduced)
          else
          begin
            fRangeEncoder.EncodeDirectBits(posReduced shr lzmaBaseU.kNumAlignBits,
              footerBits - lzmaBaseU.kNumAlignBits);
            fPosAlignEncoder.ReverseEncode(fRangeEncoder, posReduced and
              lzmaBaseU.kAlignMask);
            Inc(fAlignPriceCount);
          end;
        end;
        distance := pos;
        for i := lzmaBaseU.kNumRepDistances - 1 downto 1 do
          fRepDistances[i] := fRepDistances[i - 1];
        fRepDistances[0] := distance;
        Inc(fMatchPriceCount);
      end;
      fPreviousByte := fMatchFinder.GetIndexByte(len - 1 - fAdditionalOffset);
    end;
    fAdditionalOffset := fAdditionalOffset - len;
    nowPos64 := nowPos64 + len;
    if fAdditionalOffset = 0 then
    begin
      // if (!_fastMode)
      if fMatchPriceCount >= (1 shl 7) then
        FillDistancesPrices();
      if fAlignPriceCount >= lzmaBaseU.kAlignTableSize then
        FillAlignPrices();
      inSize  := nowPos64;
      outSize := fRangeEncoder.GetProcessedSizeAdd();
      if fMatchFinder.GetNumAvailableBytes = 0 then
      begin
        Flush(integer(nowPos64));
        exit;
      end;
      if (nowPos64 - progressPosValuePrev >= (1 shl 12)) then
      begin
        fFinished := False;
        finished  := False;
        exit;
      end;
    end;
  end;
end;

procedure TLZMAEncoder.ReleaseMFStream();
begin
  if (fMatchFinder <> nil) and fNeedReleaseMFStream then
  begin
    fMatchFinder.ReleaseStream();
    fNeedReleaseMFStream := False;
  end;
end;

procedure TLZMAEncoder.SetOuTMemoryStream(const ouTMemoryStream: TMemoryStream);
begin
  fRangeEncoder.SeTMemoryStream(ouTMemoryStream);
end;

procedure TLZMAEncoder.ReleaseOuTMemoryStream();
begin
  fRangeEncoder.ReleaseStream();
end;

procedure TLZMAEncoder.ReleaseStreams();
begin
  ReleaseMFStream();
  ReleaseOuTMemoryStream();
end;

procedure TLZMAEncoder.SeTMemoryStreams(const inStream, ouTMemoryStream: TMemoryStream;
  const inSize, outSize: int64);
begin
  fInStream := inStream;
  fFinished := False;
  _Create();
  SetOuTMemoryStream(ouTMemoryStream);
  Init();

  // if (!_fastMode)
  FillDistancesPrices();
  FillAlignPrices();

  fLenEncoder.SetTableSize(fNumFastBytes + 1 - lzmaBaseU.kMatchMinLen);
  fLenEncoder.UpdateTables(1 shl fPosStateBits);
  fRepMatchLenEncoder.SetTableSize(fNumFastBytes + 1 - lzmaBaseU.kMatchMinLen);
  fRepMatchLenEncoder.UpdateTables(1 shl fPosStateBits);

  nowPos64 := 0;
end;

procedure TLZMAEncoder.Encode(const inStream, ouTMemoryStream: TMemoryStream;
  const inSize, outSize: int64);
var
  lpos, progint, inputsize: int64;
begin
  if insize = -1 then
    inputsize := instream.Size - instream.Position
  else
    inputsize := insize;
  progint := inputsize div CODE_PROGRESS_INTERVAL;
  lpos := progint;

  fNeedReleaseMFStream := False;
  DoProgress(0, inputsize);
  try
    SeTMemoryStreams(inStream, ouTMemoryStream, inSize, outSize);
    while True do
    begin
      CodeOneBlock(processedInSize, processedOutSize, finished);
      if finished then
      begin
        DoProgress(inputsize, inputsize);
        exit;
      end;
      if processedInSize >= lpos then
      begin
        DoProgress(processedInSize, inputsize);
        lpos := lpos + progint;
      end;
    end;
  finally
    ReleaseStreams();
  end;
end;

procedure TLZMAEncoder.WriteCoderProperties(const ouTMemoryStream: TMemoryStream);
var
  i: integer;
begin
  properties[0] := (fPosStateBits * 5 + fNumLiteralPosStateBits) *
    9 + fNumLiteralContextBits;
  for i := 0 to 3 do
    properties[1 + i] := byte(fDictionarySize shr (8 * i));
  ouTMemoryStream.Write(properties, kPropSize);

end;

procedure TLZMAEncoder.FillDistancesPrices();
var
  i, posSlot, footerBits, baseVal, lenToPosState, st, st2: integer;
  encoder: TLZMABitTreeEncoder;
begin
  for i := lzmaBaseU.kStartPosModelIndex to lzmaBaseU.kNumFullDistances - 1 do
  begin
    posSlot    := GetPosSlot(i);
    footerBits := integer((posSlot shr 1) - 1);
    baseVal    := (2 or (posSlot and 1)) shl footerBits;
    tempPrices[i] :=
      XReverseGetPrice(fPosEncoders, baseVal - posSlot - 1, footerBits, i - baseVal);
  end;

  for lenToPosState := 0 to lzmaBaseU.kNumLenToPosStates - 1 do
  begin
    encoder := fPosSlotEncoder[lenToPosState];
    st      := (lenToPosState shl lzmaBaseU.kNumPosSlotBits);
    for posSlot := 0 to fDistTableSize - 1 do
      fPosSlotPrices[st + posSlot] := encoder.GetPrice(posSlot);
    for posSlot := lzmaBaseU.kEndPosModelIndex to fDistTableSize - 1 do
      fPosSlotPrices[st + posSlot] :=
        fPosSlotPrices[st + posSlot] +
        ((((posSlot shr 1) - 1) - lzmaBaseU.kNumAlignBits) shl kNumBitPriceShiftBits);
    st2 := lenToPosState * lzmaBaseU.kNumFullDistances;
    for i := 0 to lzmaBaseU.kStartPosModelIndex - 1 do
      fDistancesPrices[st2 + i] := fPosSlotPrices[st + i];
    for i := lzmaBaseU.kStartPosModelIndex to lzmaBaseU.kNumFullDistances - 1 do
      fDistancesPrices[st2 + i] := fPosSlotPrices[st + GetPosSlot(i)] + tempPrices[i];
  end;
  fMatchPriceCount := 0;
end;

procedure TLZMAEncoder.FillAlignPrices();
var
  i: integer;
begin
  for i := 0 to lzmaBaseU.kAlignTableSize - 1 do
    fAlignPrices[i] := fPosAlignEncoder.ReverseGetPrice(i);
  fAlignPriceCount := 0;
end;

function TLZMAEncoder.SetAlgorithm(const algorithm: integer): boolean;
begin
  //_fastMode = (algorithm == 0);
  //_maxMode = (algorithm >= 2);
  Result := True;
end;

function TLZMAEncoder.SetDictionarySize(dictionarySize: integer): boolean;
var
  kDicLogSizeMaxCompress, dicLogSize: integer;
begin
  kDicLogSizeMaxCompress := 29;
  if (dictionarySize < (1 shl lzmaBaseU.kDicLogSizeMin)) or
    (dictionarySize > (1 shl kDicLogSizeMaxCompress)) then
  begin
    Result := False;
    exit;
  end;
  fDictionarySize := dictionarySize;
  dicLogSize      := 0;
  while dictionarySize > (1 shl dicLogSize) do
    Inc(dicLogSize);
  fDistTableSize := dicLogSize * 2;
  Result := True;
end;

function TLZMAEncoder.SeNumFastBytes(const numFastBytes: integer): boolean;
begin
  if (numFastBytes < 5) or (numFastBytes > lzmaBaseU.kMatchMaxLen) then
  begin
    Result := False;
    exit;
  end;
  fNumFastBytes := numFastBytes;
  Result := True;
end;

function TLZMAEncoder.SetMatchFinder(const matchFinderIndex: integer): boolean;
var
  matchFinderIndexPrev: integer;
begin
  if (matchFinderIndex < 0) or (matchFinderIndex > 2) then
  begin
    Result := False;
    exit;
  end;
  matchFinderIndexPrev := fMatchFinderType;
  fMatchFinderType     := matchFinderIndex;
  if (fMatchFinder <> nil) and (matchFinderIndexPrev <> fMatchFinderType) then
  begin
    fDictionarySizePrev := -1;
    fMatchFinder := nil;
  end;
  Result := True;
end;

function TLZMAEncoder.SetLcLpPb(const lc, lp, pb: integer): boolean;
begin
  if (lp < 0) or (lp > lzmaBaseU.kNumLitPosStatesBitsEncodingMax) or
    (lc < 0) or (lc > lzmaBaseU.kNumLitContextBitsMax) or (pb < 0) or
    (pb > lzmaBaseU.kNumPosStatesBitsEncodingMax) then
  begin
    Result := False;
    exit;
  end;
  fNumLiteralPosStateBits := lp;
  fNumLiteralContextBits := lc;
  fPosStateBits := pb;
  fPosStateMask := ((1) shl fPosStateBits) - 1;
  Result := True;
end;

procedure TLZMAEncoder.SetEndMarkerMode(const endMarkerMode: boolean);
begin
  fWriteEndMark := endMarkerMode;
end;


{ TLZMAEncoder2 }
procedure TLZMAEncoder2.Init();
begin
  XInitBitModels(fEncoders);
end;

procedure TLZMAEncoder2.Encode(const rangeEncoder: TLZMARangeEncoder;
  const symbol: byte);
var
  context: integer;
  bit, i:  integer;
begin
  context := 1;
  for i := 7 downto 0 do
  begin
    bit := ((symbol shr i) and 1);
    rangeEncoder.Encode(fEncoders, context, bit);
    context := (context shl 1) or bit;
  end;
end;

procedure TLZMAEncoder2.EncodeMatched(const rangeEncoder: TLZMARangeEncoder;
  const matchByte, symbol: byte);
var
  context, i, bit, state, matchbit: integer;
  same: boolean;
begin
  context := 1;
  same    := True;
  for i := 7 downto 0 do
  begin
    bit   := ((symbol shr i) and 1);
    state := context;
    if same then
    begin
      matchBit := ((matchByte shr i) and 1);
      state    := state + ((1 + matchBit) shl 8);
      same     := (matchBit = bit);
    end;
    rangeEncoder.Encode(fEncoders, state, bit);
    context := (context shl 1) or bit;
  end;
end;

function TLZMAEncoder2.GetPrice(const matchMode: boolean;
  const matchByte, symbol: byte): integer;
var
  price, context, i, matchbit, bit: integer;
begin
  price := 0;
  context := 1;
  i := 7;
  if matchMode then
    while i >= 0 do
    begin
      matchBit := (matchByte shr i) and 1;
      bit      := (symbol shr i) and 1;
      price    := price + rangeEncoder.GetPrice(
        fEncoders[((1 + matchBit) shl 8) + context], bit);
      context  := (context shl 1) or bit;
      if matchBit <> bit then
      begin
        Dec(i);
        break;
      end;
      Dec(i);
    end;
  while i >= 0 do
  begin
    bit     := (symbol shr i) and 1;
    price   := price + rangeEncoder.GetPrice(fEncoders[context], bit);
    context := (context shl 1) or bit;
    Dec(i);
  end;
  Result := price;
end;


{ TLZMALiteralEncoder }
procedure TLZMALiteralEncoder._Create(const numPosBits, numPrevBits: integer);
var
  numstates, i: integer;
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
    fCoders[i] := TLZMAEncoder2.Create();
end;

destructor TLZMALiteralEncoder.Destroy();
var
  i: integer;
begin
  for i := Low(fCoders) to High(fCoders) do
    FreeAndNil(fCoders[i]);
  inherited;
end;

procedure TLZMALiteralEncoder.Init();
var
  numstates, i: integer;
begin
  numStates := 1 shl (fNumPrevBits + fNumPosBits);
  for i := 0 to numStates - 1 do
    fCoders[i].Init();
end;

function TLZMALiteralEncoder.GetSubCoder(const pos: integer;
  const prevByte: byte): TLZMAEncoder2;
begin
  Result := fCoders[((pos and fPosMask) shl fNumPrevBits) +
    ((prevByte and $FF) shr (8 - fNumPrevBits))];
end;


{ TLZMALenEncoder }
constructor TLZMALenEncoder.Create();
var
  posState: integer;
begin
  inherited;
  fHighCoder := TLZMABitTreeEncoder.Create(lzmaBaseU.kNumHighLenBits);
  for posState := 0 to lzmaBaseU.kNumPosStatesEncodingMax - 1 do
  begin
    fLowCoder[posState] := TLZMABitTreeEncoder.Create(lzmaBaseU.kNumLowLenBits);
    fMidCoder[posState] := TLZMABitTreeEncoder.Create(lzmaBaseU.kNumMidLenBits);
  end;
end;

destructor TLZMALenEncoder.Destroy();
var
  posState: integer;
begin
  FreeAndNil(fHighCoder);
  for posState := 0 to lzmaBaseU.kNumPosStatesEncodingMax - 1 do
  begin
    FreeAndNil(fLowCoder[posState]);
    FreeAndNil(fMidCoder[posState]);
  end;
  inherited;
end;

procedure TLZMALenEncoder.Init(const numPosStates: integer);
var
  posState: integer;
begin
  XInitBitModels(fChoice);
  for posState := 0 to numPosStates - 1 do
  begin
    fLowCoder[posState].Init();
    fMidCoder[posState].Init();
  end;
  fHighCoder.Init();
end;

procedure TLZMALenEncoder.Encode(const rangeEncoder: TLZMARangeEncoder;
  symbol: integer; const posState: integer);
begin
  if symbol < lzmaBaseU.kNumLowLenSymbols then
  begin
    rangeEncoder.Encode(fChoice, 0, 0);
    fLowCoder[posState].Encode(rangeEncoder, symbol);
  end
  else
  begin
    symbol := symbol - lzmaBaseU.kNumLowLenSymbols;
    rangeEncoder.Encode(fChoice, 0, 1);
    if symbol < lzmaBaseU.kNumMidLenSymbols then
    begin
      rangeEncoder.Encode(fChoice, 1, 0);
      fMidCoder[posState].Encode(rangeEncoder, symbol);
    end
    else
    begin
      rangeEncoder.Encode(fChoice, 1, 1);
      fHighCoder.Encode(rangeEncoder, symbol - lzmaBaseU.kNumMidLenSymbols);
    end;
  end;
end;

procedure TLZMALenEncoder.SetPrices(const posState, numSymbols: integer;
  var prices: array of integer; const st: integer);
var
  a0, a1, b0, b1, i: integer;
begin
  a0 := rangeEncoder.GetPrice0(fChoice[0]);
  a1 := rangeEncoder.GetPrice1(fChoice[0]);
  b0 := a1 + rangeEncoder.GetPrice0(fChoice[1]);
  b1 := a1 + rangeEncoder.GetPrice1(fChoice[1]);
  i  := 0;
  while i < lzmaBaseU.kNumLowLenSymbols do
  begin
    if i >= numSymbols then
      exit;
    prices[st + i] := a0 + fLowCoder[posState].GetPrice(i);
    Inc(i);
  end;
  while i < lzmaBaseU.kNumLowLenSymbols + lzmaBaseU.kNumMidLenSymbols do
  begin
    if i >= numSymbols then
      exit;
    prices[st + i] := b0 + fMidCoder[posState].GetPrice(i - lzmaBaseU.kNumLowLenSymbols);
    Inc(i);
  end;
  while i < numSymbols do
  begin
    prices[st + i] := b1 + fHighCoder.GetPrice(i - lzmaBaseU.kNumLowLenSymbols -
      lzmaBaseU.kNumMidLenSymbols);
    Inc(i);
  end;
end;


{ TLZMALenPriceTableEncoder }
procedure TLZMALenPriceTableEncoder.SetTableSize(const tableSize: integer);
begin
  fTableSize := tableSize;
end;

function TLZMALenPriceTableEncoder.GetPrice(const symbol, posState: integer): integer;
begin
  Result := fPrices[posState * lzmaBaseU.kNumLenSymbols + symbol];
end;

procedure TLZMALenPriceTableEncoder.UpdateTable(const posState: integer);
begin
  SetPrices(posState, fTableSize, fPrices, posState * lzmaBaseU.kNumLenSymbols);
  fCounters[posState] := fTableSize;
end;

procedure TLZMALenPriceTableEncoder.UpdateTables(const numPosStates: integer);
var
  posState: integer;
begin
  for posState := 0 to numPosStates - 1 do
    UpdateTable(posState);
end;

procedure TLZMALenPriceTableEncoder.Encode(const rangeEncoder: TLZMARangeEncoder;
  symbol: integer; const posState: integer);
begin
  inherited Encode(rangeEncoder, symbol, posState);
  Dec(fCounters[posState]);
  if fCounters[posState] = 0 then
    UpdateTable(posState);
end;


{ TLZMAOptimal }
procedure TLZMAOptimal.MakeAsChar();
begin
  backPrev    := -1;
  prev1IsChar := False;
end;

procedure TLZMAOptimal.MakeAsShortRep();
begin
  backPrev    := 0;
  prev1IsChar := False;
end;

function TLZMAOptimal.IsShortRep(): boolean;
begin
  Result := (backPrev = 0);
end;

procedure TLZMAEncoder.DoProgress(const current, total: integer);
begin
  //  if Assigned(fOnProgress) then  fOnProgress(current, total);
end;


procedure InitCRC();
var
  i, r, j: integer;
begin
  for i := 0 to 255 do
  begin
    r := i;
    for j := 0 to 7 do
      if (r and 1) <> 0 then
        r := (r shr 1) xor integer($EDB88320)
      else
        r := r shr 1;
    crcTable[i] := r;
  end;
end;


initialization
  InitCRC();
  rangeEncoder := TLZMARangeEncoder.Create();

finalization
  rangeEncoder.Free();
end.
