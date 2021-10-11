 // LZMA SDK   Copyright (C) 1999-2006 Igor Pavlov
 // original translation here:
 //   http://www.birtles.org.uk/programming/LZMA.442b.7z
 // napilniked %-) by Ketmar // Avalon Group
 //   http://avalon-group.ho.com.ua/
 //   ketmar@online.ua

// GNU LGPL / CPL

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}
unit lzmaBaseU;

interface


const
  kNumRepDistances = 4;
  kNumStates      = 12;
  kNumPosSlotBits = 6;
  kDicLogSizeMin  = 0;
  //kDicLogSizeMax = 28;
  //kDistTableSizeMax = kDicLogSizeMax * 2;

  kNumLenToPosStatesBits = 2; // it's for speed optimization
  kNumLenToPosStates     = 1 shl kNumLenToPosStatesBits;

  kMatchMinLen = 2;

  kNumAlignBits   = 4;
  kAlignTableSize = 1 shl kNumAlignBits;
  kAlignMask      = (kAlignTableSize - 1);

  kStartPosModelIndex = 4;
  kEndPosModelIndex = 14;
  kNumPosModels = kEndPosModelIndex - kStartPosModelIndex;

  kNumFullDistances = 1 shl (kEndPosModelIndex div 2);

  kNumLitPosStatesBitsEncodingMax = 4;
  kNumLitContextBitsMax = 8;

  kNumPosStatesBitsMax = 4;
  kNumPosStatesMax     = (1 shl kNumPosStatesBitsMax);
  kNumPosStatesBitsEncodingMax = 4;
  kNumPosStatesEncodingMax = (1 shl kNumPosStatesBitsEncodingMax);

  kNumLowLenBits    = 3;
  kNumMidLenBits    = 3;
  kNumHighLenBits   = 8;
  kNumLowLenSymbols = 1 shl kNumLowLenBits;
  kNumMidLenSymbols = 1 shl kNumMidLenBits;
  kNumLenSymbols    = kNumLowLenSymbols + kNumMidLenSymbols + (1 shl kNumHighLenBits);
  kMatchMaxLen      = kMatchMinLen + kNumLenSymbols - 1;


function StateInit(): integer;
function StateUpdateChar(const index: integer): integer;
function StateUpdateMatch(const index: integer): integer;
function StateUpdateRep(const index: integer): integer;
function StateUpdateShortRep(const index: integer): integer;
function StateIsCharState(const index: integer): boolean;
function GetLenToPosState(len: integer): integer;


implementation


function StateInit(): integer;
begin
  Result := 0;
end;

function StateUpdateChar(const index: integer): integer;
begin
  if index < 4 then
    Result := 0
  else
  if index < 10 then
    Result := index - 3
  else
    Result := index - 6;
end;

function StateUpdateMatch(const index: integer): integer;
begin
  if index < 7 then
    Result := 7
  else
    Result := 10;
end;

function StateUpdateRep(const index: integer): integer;
begin
  if index < 7 then
    Result := 8
  else
    Result := 11;
end;

function StateUpdateShortRep(const index: integer): integer;
begin
  if index < 7 then
    Result := 9
  else
    Result := 11;
end;

function StateIsCharState(const index: integer): boolean;
begin
  Result := index < 7;
end;

function GetLenToPosState(len: integer): integer;
begin
  len := len - kMatchMinLen;
  if len < kNumLenToPosStates then
    Result := len
  else
    Result := (kNumLenToPosStates - 1);
end;


end.
