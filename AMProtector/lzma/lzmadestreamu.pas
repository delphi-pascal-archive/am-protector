 // LZMA SDK   Copyright (C) 1999-2006 Igor Pavlov
 // converted from java-lzma (c) Igor Pavlov
 // conversion by Ketmar // Avalon Group
 //   http://avalon-group.ho.com.ua/
 //   ketmar@online.ua

// GNU LGPL / CPL

unit lzmaDeStreamU;

interface

uses
  SysUtils, Classes;

type
  PLZMALWArray = ^TLZMALWArray;
  TLZMALWArray = packed array [0..MaxInt div 4 - 1] of longword;

  TLZMARangeDecoder = class
  protected
    fInStream: TStream;

    fRange: int64;
    fCode:  int64;

    fBuffer:     packed array of byte;
    fBufferSize: integer;
    fBufferInd:  integer;

  public
    constructor Create(inS: TStream);

    function ReadByte(): longword;
    function DecodeDirectBits(numTotalBits: integer): integer;
    function BitDecode(prob: PLZMALWArray; index: integer): integer;
    function BitTreeDecode(fprobs: PLZMALWArray; index, numLevels: integer): integer;
    function ReverseBitTreeDecode(fprobs: PLZMALWArray;
      index, numLevels: integer): integer;
    function LzmaLiteralDecode(fprobs: PLZMALWArray; index: integer): byte;
    function LzmaLiteralDecodeMatch(fprobs: PLZMALWArray; index: integer;
      matchbyte: byte): byte;
    function LzmaLenDecode(fprobs: PLZMALWArray; index, posState: integer): integer;
  end;

  TLZMADecompressStream = class(TStream)
  protected
    fInStream: TStream;

    fIsClosed: boolean;
    fRangeDecoder: TLZMARangeDecoder;
    fDictionary: packed array of byte;
    fDictionarySize: integer;
    fDictionaryPos: integer;
    fGlobalPos: integer;
    fRep0:  integer;
    fRep1:  integer;
    fRep2:  integer;
    fRep3:  integer;
    fLc:    integer;
    fLp:    integer;
    fPb:    integer;
    fState: integer;
    fPreviousIsMatch: boolean;
    fRemainLen: integer;
    fProbs: array of longword;

    fUncompressedBuffer: packed array of byte;
    fUncompressedSize: integer;
    fUncompressedOffset: integer;
    fGlobalNowPos:  int64;
    fGlobalOutSize: int64;

    fCurPos, fNewPos: int64;

    procedure LzmaDecode(outSize: integer);
    procedure FillBuffer();
    procedure ReadHeader(hdr: Pointer; pOutSz: int64);
    procedure Close();

  public
    constructor Create(inS: TStream); overload;
    constructor Create(inS: TStream; hdr: Pointer; pOutSz: int64); overload;
    destructor Destroy(); override;

    function Read(var buf; len: integer): integer; override;
    function Write(const buffer; Count: longint): longint; override;
    function Seek(offset: longint; origin: word): longint; override;

    property GlobalNowPos: int64 Read fGlobalNowPos;
    property GlobalOutSize: int64 Read fGlobalOutSize;
  end;


implementation


const
  kNumTopBits   = 24;
  kTopValue     = (1 shl kNumTopBits);
  kTopValueMask = not (kTopValue - 1);

  kNumBitModelTotalBits = 11;
  kBitModelTotal = (1 shl kNumBitModelTotalBits);
  kNumMoveBits   = 5;


  kNumPosBitsMax   = 4;
  kNumPosStatesMax = (1 shl kNumPosBitsMax);

  kLenNumLowBits     = 3;
  kLenNumLowSymbols  = (1 shl kLenNumLowBits);
  kLenNumMidBits     = 3;
  kLenNumMidSymbols  = (1 shl kLenNumMidBits);
  kLenNumHighBits    = 8;
  kLenNumHighSymbols = (1 shl kLenNumHighBits);

  LenChoice  = 0;
  LenChoice2 = (LenChoice + 1);
  LenLow     = (LenChoice2 + 1);
  LenMid     = (LenLow + (kNumPosStatesMax shl kLenNumLowBits));
  LenHigh    = (LenMid + (kNumPosStatesMax shl kLenNumMidBits));
  kNumLenProbs = (LenHigh + kLenNumHighSymbols);


  LZMA_BASE_SIZE = 1846;
  LZMA_LIT_SIZE  = 768;

  kBlockSize = $10000;

  kNumStates = 12;

  kStartPosModelIndex = 4;
  kEndPosModelIndex   = 14;
  kNumFullDistances   = (1 shl (kEndPosModelIndex shr 1));

  kNumPosSlotBits    = 6;
  kNumLenToPosStates = 4;

  kNumAlignBits   = 4;
  kAlignTableSize = (1 shl kNumAlignBits);

  kMatchMinLen = 2;

  constIsMatch    = 0;
  constIsRep      = (constIsMatch + (kNumStates shl kNumPosBitsMax));
  constIsRepG0    = (constIsRep + kNumStates);
  constIsRepG1    = (constIsRepG0 + kNumStates);
  constIsRepG2    = (constIsRepG1 + kNumStates);
  constIsRep0Long = (constIsRepG2 + kNumStates);
  constPosSlot    = (constIsRep0Long + (kNumStates shl kNumPosBitsMax));
  constSpecPos    = (constPosSlot + (kNumLenToPosStates shl kNumPosSlotBits));
  constAlign      = (constSpecPos + kNumFullDistances - kEndPosModelIndex);
  constLenCoder   = (constAlign + kAlignTableSize);
  constRepLenCoder = (constLenCoder + kNumLenProbs);
  constLiteral    = (constRepLenCoder + kNumLenProbs);


{ TLZMARangeDecoder }
constructor TLZMARangeDecoder.Create(inS: TStream);
var
  i: integer;
begin
  SetLength(fBuffer, 1 shl 14);
  fBufferSize := 0;
  fBufferInd := 0;
  fInStream := inS;
  fCode  := 0;
  fRange := $FFFFFFFF;
  for i := 0 to 4 do
    fCode := (fCode shl 8) or ReadByte();
end;

function TLZMARangeDecoder.ReadByte(): longword;
begin
  if fBufferSize = fBufferInd then
  begin
    fBufferInd  := 0;
    fBufferSize := fInStream.Size - fInStream.Position;
    if fBufferSize > Length(fBuffer) then
      fBufferSize := Length(fBuffer);
    if fBufferSize < 1 then
      raise Exception.Create('LZMA: Data Error');
    fInStream.ReadBuffer(fBuffer[0], fBufferSize);
  end;
  Result := fBuffer[fBufferInd] and $FF;
  Inc(fBufferInd);
end;

function TLZMARangeDecoder.DecodeDirectBits(numTotalBits: integer): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to numTotalBits - 1 do
  begin
    fRange := fRange shr 1;
    Result := Result shl 1;
    if fCode >= fRange then
    begin
      Dec(fCode, fRange);
      Result := Result or 1;
    end;
    if fRange < kTopValue then
    begin
      fRange := fRange shl 8;
      fCode  := (fCode shl 8) or ReadByte();
    end;
  end;
end;

function TLZMARangeDecoder.BitDecode(prob: PLZMALWArray; index: integer): integer;
var
  newBound: longword;
begin
  newBound := (fRange shr kNumBitModelTotalBits) * prob[index];
  if fCode < newBound then  // unsigned comparison
  begin
    fRange := newBound;
    Inc(prob[index], (kBitModelTotal - prob[index]) shr kNumMoveBits);
    if fRange < kTopValue then
    begin
      fCode  := (fCode shl 8) or ReadByte();
      fRange := fRange shl 8;
    end;
    Result := 0;
    exit;
  end
  else
  begin
    Dec(fRange, newBound);
    Dec(fCode, newBound);
    Dec(prob[index], prob[index] shr kNumMoveBits);
    if fRange < kTopValue then
    begin
      fCode  := (fCode shl 8) or ReadByte();
      fRange := fRange shl 8;
    end;
    Result := 1;
  end;
end;

function TLZMARangeDecoder.BitTreeDecode(fProbs: PLZMALWArray;
  index, numLevels: integer): integer;
var
  i, mi: integer;
begin
  mi := 1;
  for i := 0 to numLevels - 1 do
    mi := (mi + mi) + BitDecode(fProbs, index + mi);
  Result := mi - (1 shl numLevels);
end;

function TLZMARangeDecoder.ReverseBitTreeDecode(fProbs: PLZMALWArray;
  index, numLevels: integer): integer;
var
  i, mi, symbol, bit: integer;
begin
  mi     := 1;
  symbol := 0;
  for i := 0 to numLevels - 1 do
  begin
    bit    := BitDecode(fProbs, index + mi);
    mi     := mi + mi + bit;
    symbol := symbol or (bit shl i);
  end;
  Result := symbol;
end;

function TLZMARangeDecoder.LzmaLiteralDecode(fProbs: PLZMALWArray;
  index: integer): byte;
var
  symbol: integer;
begin
  symbol := 1;
  repeat
    symbol := (symbol shl 1) or BitDecode(fProbs, index + symbol);
    if not (symbol < $100) then
      break;
  until False;
  Result := byte(symbol and $FF);
end;

function TLZMARangeDecoder.LzmaLiteralDecodeMatch(fProbs: PLZMALWArray;
  index: integer; matchbyte: byte): byte;
var
  symbol, matchBit, bit: integer;
begin
  symbol := 1;
  repeat
    matchBit := (matchbyte shr 7) and $01;
    matchbyte := (matchbyte and $7F) shl 1;
    bit    := BitDecode(fProbs, index + ((1 + matchBit) shl 8) + symbol);
    symbol := (symbol shl 1) or bit;
    if matchBit <> bit then
    begin
      while symbol < $100 do
      begin
        symbol := (symbol + symbol) or BitDecode(fProbs, index + symbol);
      end;
      break;
    end;
    if not (symbol < $100) then
      break;
  until False;
  Result := byte(symbol and $FF);
end;


function TLZMARangeDecoder.LzmaLenDecode(fProbs: PLZMALWArray;
  index, posState: integer): integer;
begin
  if BitDecode(fProbs, index + LenChoice) = 0 then
    Result := BitTreeDecode(fProbs, index + LenLow + (posState shl kLenNumLowBits),
      kLenNumLowBits)
  else
  if BitDecode(fProbs, index + LenChoice2) = 0 then
    Result := kLenNumLowSymbols + BitTreeDecode(fProbs, index + LenMid +
      (posState shl kLenNumMidBits), kLenNumMidBits)
  else
    Result := kLenNumLowSymbols + kLenNumMidSymbols +
      BitTreeDecode(fProbs, index + LenHigh, kLenNumHighBits);
end;


{ LZMADecodeStream }
constructor TLZMADecompressStream.Create(inS: TStream; hdr: Pointer; pOutSz: int64);
begin
  inherited Create();

  fInStream     := inS;
  fIsClosed     := False;
  fRangeDecoder := nil;
  SetLength(fDictionary, 0);
  fDictionarySize := 0;
  fDictionaryPos := 0;
  fGlobalPos := 0;
  fRep0  := 0;
  fRep1  := 0;
  fRep2  := 0;
  fRep3  := 0;
  fLc    := 0;
  fLp    := 0;
  fPb    := 0;
  fState := 0;
  fPreviousIsMatch := False;
  fRemainLen := 0;
  SetLength(fProbs, 0);

  SetLength(fUncompressedBuffer, 0);
  fUncompressedSize := 0;
  fUncompressedOffset := 0;
  fGlobalNowPos := 0;
  fGlobalOutSize := 0;
  fCurPos := 0;
  fNewPos := 0;

  ReadHeader(hdr, pOutSz);
  FillBuffer();
end;

constructor TLZMADecompressStream.Create(inS: TStream);
begin
  Create(inS, nil, -1);
end;

destructor TLZMADecompressStream.Destroy();
begin
  Close();
  FreeAndNil(fRangeDecoder);
  inherited Destroy();
end;

procedure TLZMADecompressStream.LzmaDecode(outSize: integer);
var
  previousbyte, matchbyte: byte;
  posStateMask, literalPosMask: integer;
  pos, posState, posSlot, ind_prob: integer;
  distance, n:   integer;
  numDirectBits: integer;
begin
  posStateMask      := (1 shl fPb) - 1;
  literalPosMask    := (1 shl fLp) - 1;
  fUncompressedSize := 0;
  if fRemainLen = -1 then
    exit;
  while (fRemainLen > 0) and (fUncompressedSize < outSize) do
  begin
    pos := fDictionaryPos - fRep0;
    if pos < 0 then
      Inc(pos, fDictionarySize);
    fDictionary[fDictionaryPos] := fDictionary[pos];
    fUncompressedBuffer[fUncompressedSize] := fDictionary[fDictionaryPos];
    Inc(fUncompressedSize);
    Inc(fDictionaryPos);
    if fDictionaryPos = fDictionarySize then
      fDictionaryPos := 0;
    Dec(fRemainLen);
  end;

  if fDictionaryPos = 0 then
    previousbyte := fDictionary[fDictionarySize - 1]
  else
    previousbyte := fDictionary[fDictionaryPos - 1];

  while fUncompressedSize < outSize do
  begin
    posState := (int64(fUncompressedSize) + fGlobalPos) and posStateMask;
    if fRangeDecoder.BitDecode(@(fProbs[0]), constIsMatch +
      (fState shl kNumPosBitsMax) + posState) = 0 then
    begin
      ind_prob := constLiteral + (LZMA_LIT_SIZE *
        ((integer((int64(fUncompressedSize) + fGlobalPos) and literalPosMask) shl fLc) +
        ((previousbyte and $FF) shr (8 - fLc))));
      if fState < 4 then
        fState := 0
      else
      if fState < 10 then
        Dec(fState, 3)
      else
        Dec(fState, 6);
      if fPreviousIsMatch then
      begin
        pos := fDictionaryPos - fRep0;
        if pos < 0 then
          Inc(pos, fDictionarySize);
        matchbyte    := fDictionary[pos];
        previousbyte := fRangeDecoder.LzmaLiteralDecodeMatch(@(fProbs[0]),
          ind_prob, matchbyte);
        fPreviousIsMatch := False;
      end
      else
        previousbyte := fRangeDecoder.LzmaLiteralDecode(@(fProbs[0]), ind_prob);

      fUncompressedBuffer[fUncompressedSize] := previousbyte;
      Inc(fUncompressedSize);
      fDictionary[fDictionaryPos] := previousbyte;
      Inc(fDictionaryPos);
      if fDictionaryPos = fDictionarySize then
        fDictionaryPos := 0;
    end
    else
    begin
      fPreviousIsMatch := True;
      if fRangeDecoder.BitDecode(@(fProbs[0]), constIsRep + fState) = 1 then
      begin
        if fRangeDecoder.BitDecode(@(fProbs[0]), constIsRepG0 + fState) = 0 then
        begin
          if fRangeDecoder.BitDecode(@(fProbs[0]), constIsRep0Long +
            (fState shl kNumPosBitsMax) + posState) = 0 then
          begin
            if (fUncompressedSize + fGlobalPos) = 0 then
              raise Exception.Create('LZMA: Data Error');
            if fState < 7 then
              fState := 9
            else
              fState := 11;

            pos := fDictionaryPos - fRep0;
            if pos < 0 then
              Inc(pos, fDictionarySize);
            previousbyte := fDictionary[pos];
            fDictionary[fDictionaryPos] := previousbyte;
            Inc(fDictionaryPos);
            if fDictionaryPos = fDictionarySize then
              fDictionaryPos := 0;

            fUncompressedBuffer[fUncompressedSize] := previousbyte;
            Inc(fUncompressedSize);
            continue;
          end;
        end
        else
        begin
          //distance := 0;
          if fRangeDecoder.BitDecode(@(fProbs[0]), constIsRepG1 + fState) = 0 then
            distance := fRep1
          else
          begin
            if fRangeDecoder.BitDecode(@(fProbs[0]), constIsRepG2 + fState) = 0 then
              distance := fRep2
            else
            begin
              distance := fRep3;
              fRep3    := fRep2;
            end;
            fRep2 := fRep1;
          end;
          fRep1 := fRep0;
          fRep0 := distance;
        end;
        fRemainLen := fRangeDecoder.LzmaLenDecode(@(fProbs[0]),
          constRepLenCoder, posState);
        if fState < 7 then
          fState := 8
        else
          fState := 11;
      end
      else
      begin
        fRep3 := fRep2;
        fRep2 := fRep1;
        fRep1 := fRep0;
        if fState < 7 then
          fState := 7
        else
          fState := 10;
        fRemainLen := fRangeDecoder.LzmaLenDecode(@(fProbs[0]), constLenCoder, posState);

        if fRemainLen < kNumLenToPosStates then
          n := fRemainLen
        else
          n := kNumLenToPosStates - 1;

        posSlot := fRangeDecoder.BitTreeDecode(@(fProbs[0]), constPosSlot +
          (n shl kNumPosSlotBits), kNumPosSlotBits);
        if posSlot >= kStartPosModelIndex then
        begin
          numDirectBits := ((posSlot shr 1) - 1);
          fRep0 := ((2 or (posSlot and 1)) shl numDirectBits);
          if posSlot < kEndPosModelIndex then
          begin
            Inc(fRep0, fRangeDecoder.ReverseBitTreeDecode(@(fProbs[0]),
              constSpecPos + fRep0 - posSlot - 1, numDirectBits));
          end
          else
          begin
            Inc(fRep0, fRangeDecoder.DecodeDirectBits(numDirectBits - kNumAlignBits) shl
              kNumAlignBits);
            Inc(fRep0, fRangeDecoder.ReverseBitTreeDecode(@(fProbs[0]),
              constAlign, kNumAlignBits));
          end;
        end
        else
          fRep0 := posSlot;
        Inc(fRep0);
      end;
      if fRep0 = 0 then
      begin
        Dec(fRemainLen);
        break;
      end;
      if fRep0 > fUncompressedSize + fGlobalPos then
        raise Exception.Create('LZMA: Data Error');
      Inc(fRemainLen, kMatchMinLen);
      repeat
        pos := fDictionaryPos - fRep0;
        if pos < 0 then
          Inc(pos, fDictionarySize);
        previousbyte := fDictionary[pos];
        fDictionary[fDictionaryPos] := previousbyte;
        Inc(fDictionaryPos);
        if fDictionaryPos = fDictionarySize then
          fDictionaryPos := 0;

        fUncompressedBuffer[fUncompressedSize] := previousbyte;
        Inc(fUncompressedSize);
        Dec(fRemainLen);
        if not ((fRemainLen > 0) and (fUncompressedSize < outSize)) then
          break;
      until False;
    end;
  end;
  Inc(fGlobalPos, fUncompressedSize);
end;

procedure TLZMADecompressStream.FillBuffer();
var
  lblockSize: int64;
  blockSize:  integer;
begin
  if fGlobalNowPos < fGlobalOutSize then
  begin
    fUncompressedOffset := 0;
    lblockSize := fGlobalOutSize - fGlobalNowPos;
    if lblockSize > kBlockSize then
      blockSize := kBlockSize
    else
      blockSize := integer(lblockSize);

    LzmaDecode(blockSize);

    if fUncompressedSize = 0 then
      fGlobalOutSize := fGlobalNowPos
    else
      Inc(fGlobalNowPos, fUncompressedSize);
  end;
end;

procedure TLZMADecompressStream.ReadHeader(hdr: Pointer; pOutSz: int64);
var
  i:     integer;
  prop0: byte;
  lzmaInternalSize: integer;
begin
  if hdr = nil then
  begin
    fInStream.ReadBuffer(prop0, 1);
    fInStream.ReadBuffer(fDictionarySize, 4);
  end
  else
  begin
    Move(hdr^, prop0, 1);
    Inc(PChar(hdr), 1);
    Move(hdr^, fDictionarySize, 4);
    Inc(PChar(hdr), 4);
  end;
  if pOutSz = -1 then
  begin
    if hdr = nil then
      fInStream.ReadBuffer(fGlobalOutSize, SizeOf(fGlobalOutSize))
    else
      Move(hdr^, fGlobalOutSize, SizeOf(fGlobalOutSize));
  end
  else
    fGlobalOutSize := pOutSz;
  ASSERT(fGlobalOutSize <> -1);

  if prop0 >= (9 * 5 * 5) then
    raise Exception.Create('LZMA header corrupted: Properties error');

  fPb := 0;
  while prop0 >= (9 * 5) do
  begin
    Inc(fPb);
    Dec(prop0, (9 * 5));
  end;

  fLp := 0;
  while prop0 >= 9 do
  begin
    Inc(fLp);
    Dec(prop0, 9);
  end;

  fLc := prop0;
  lzmaInternalSize := (LZMA_BASE_SIZE + (LZMA_LIT_SIZE shl (fLc + fLp)));
  SetLength(fProbs, lzmaInternalSize);
  SetLength(fDictionary, fDictionarySize);
  fRangeDecoder := TLZMARangeDecoder.Create(fInStream);
  fDictionaryPos := 0;
  fGlobalPos := 0;
  fRep0      := 1;
  fRep1      := 1;
  fRep2      := 1;
  fRep3      := 1;
  fState     := 0;
  fPreviousIsMatch := False;
  fRemainLen := 0;
  fDictionary[fDictionarySize - 1] := 0;
  for i := 0 to High(fProbs) do
    fProbs[i] := kBitModelTotal shr 1;
  SetLength(fUncompressedBuffer, kBlockSize);
  fUncompressedSize := 0;
  fUncompressedOffset := 0;
  fGlobalNowPos := 0;
end;

function TLZMADecompressStream.Read(var buf; len: integer): integer;
var
  l, rd: integer;
  p:     PChar;
begin
  Result := 0;
  if fIsClosed or (len = 0) then
    exit;

  if fNewPos < fCurPos then
    raise EReadError.Create('invalid file position');
  while fCurPos < fNewPos do
  begin
    rd := fNewPos - fCurPos;
    // skip
    if fUncompressedOffset = fUncompressedSize then
      FillBuffer();
    if fUncompressedOffset = fUncompressedSize then
      exit;
    l := fUncompressedSize - fUncompressedOffset;
    if l > rd then
      l := rd;
    Inc(fUncompressedOffset, l);
    Inc(fCurPos, l);
  end;

  Result := 0;
  p      := @buf;
  while len > 0 do
  begin
    if fUncompressedOffset = fUncompressedSize then
      FillBuffer();
    if fUncompressedOffset = fUncompressedSize then
      exit;
    l := fUncompressedSize - fUncompressedOffset;
    if len < l then
      l := len;
    Move(fUncompressedBuffer[fUncompressedOffset], p^, l);
    Inc(fUncompressedOffset, l);
    Inc(fCurPos, l);
    fNewPos := fCurPos;
    Dec(len, l);
    Inc(p, l);
    Inc(Result, l);
  end;
end;

procedure TLZMADecompressStream.Close();
begin
  FreeAndNil(fRangeDecoder);
  fIsClosed := True;
  fInStream := nil;
end;

function TLZMADecompressStream.Write(const buffer; Count: longint): longint;
begin
  raise EWriteError.Create('TLZMADecompressStream: no writes!');
  Result := 0;
end;

function TLZMADecompressStream.Seek(offset: longint; origin: word): longint;
begin
  case origin of
    soFromCurrent: Inc(offset, fNewPos);
    soFromEnd: offset := fGlobalOutSize - offset;
  end;
  fNewPos := offset;
  Result  := fNewPos;
end;


end.
