(*******************************************************************************************


AMPRNG - I'M PRNG
Pseudo-random   probably safe number generator

State length: 2048 bit x 2

Initialization key: 80-512 bit
Initialization vector: 80-512 bit

Uses some ideas from RC and VMPC

(copyleft) Alexander Myasnikow, 2009

Web: www.alexanderwdark.narod.ru

********************************************************************************************)


{$R-}
{$Q-}
{$D+}
{$O+}

unit amprng;


{$DEFINE PARANOID}

interface

type
  TAMPRNG = class

  var
    P, MG: array [0..255] of byte;
    pos:   integer;
    m, z:  byte;


  const
    NLBOX: array [0..255] of byte =
      ($96, $62, $29, $e5, $0c, $b7, $bb, $1e, $7c, $1b, $80, $cd,
      $8d, $3b, $83, $3c, $17, $cb, $11, $87, $16, $1d, $c0, $a1, $e9, $dd, $2f, $8f,
      $89, $fa, $58, $28, $8b, $f2, $3a, $f4, $73, $ca, $55, $e7, $94, $9b, $bf, $af,
      $1c, $c9, $2b, $74, $ba, $53, $98, $75, $bc, $aa, $6c, $ec, $b1, $c4, $08, $01,
      $c6, $59, $a4, $4a, $fe, $c2, $86, $df, $ff, $56, $6a, $79, $d8, $b8, $b3, $c5,
      $72, $dc, $6d, $32, $69, $4f, $7a, $f9, $7d, $db, $b4, $92, $9c, $a9, $64, $e1,
      $12, $d6, $fc, $15, $9a, $02, $06, $cf, $19, $d0, $d2, $be, $b9, $50, $47, $23,
      $5f, $7f, $e6, $5d, $b5, $3e, $48, $33, $ea, $a6, $30, $43, $e0, $52, $67, $34,
      $31, $fd, $00, $ae, $c1, $f6, $18, $27, $97, $f8, $e4, $7b, $8c, $2c, $b2, $22,
      $9f, $10, $ab, $f7, $e2, $5c, $4d, $5b, $05, $1a, $39, $03, $0f, $38, $04, $d1,
      $ee, $60, $99, $41, $e8, $a7, $70, $de, $ed, $81, $7e, $57, $d5, $82, $85, $88,
      $6b, $0d, $d4, $a3, $a8, $ef, $84, $ad, $20, $21, $4c, $35, $66, $42, $91, $0a,
      $6f, $8e, $90, $0b, $f5, $4b, $2d, $d7, $d3, $71, $44, $07, $9d, $fb, $40, $68,
      $bd, $14, $2e, $09, $cc, $1f, $a5, $93, $c8, $c3, $54, $65, $76, $a0, $77, $95,
      $a2, $49, $d9, $ac, $8a, $3f, $24, $36, $45, $f1, $13, $9e, $3d, $46, $f0, $b6,
      $5e, $37, $c7, $6e, $2a, $ce, $63, $26, $e3, $0e, $78, $b0, $4e, $5a, $f3, $eb,
      $51, $61, $25, $da);


  const
    TKBOX: array [0..255] of byte =
      ($90, $16, $7c, $a3, $44, $8c, $ed, $83, $fc, $65, $c2, $05,
      $46, $19, $1d, $51, $41, $aa, $4f, $bb, $02, $61, $94, $f5, $ac, $c8, $11, $07,
      $1f, $34, $de, $d7, $92, $c4, $39, $fb, $fa, $e8, $62, $9c, $17, $04, $b6, $db,
      $01, $06, $fe, $f2, $b3, $0c, $f9, $03, $e3, $ef, $b5, $31, $2c, $b2, $5e, $15,
      $00, $23, $ee, $53, $09, $d6, $3c, $5d, $36, $14, $cf, $72, $6a, $ae, $71, $a6,
      $56, $84, $b7, $af, $5a, $79, $73, $7d, $2f, $18, $e4, $49, $38, $e1, $08, $47,
      $7a, $3a, $df, $6c, $91, $66, $8a, $99, $86, $58, $1c, $b4, $43, $8d, $bd, $4e,
      $24, $75, $f8, $e7, $9f, $6b, $a5, $ce, $cb, $cc, $6f, $8e, $c6, $80, $54, $9e,
      $3f, $4a, $0a, $29, $e0, $7e, $45, $bc, $0b, $89, $d4, $d9, $95, $d8, $55, $a9,
      $3d, $5b, $ff, $32, $b8, $8b, $0f, $37, $cd, $2b, $57, $69, $78, $a8, $74, $50,
      $d0, $85, $81, $97, $ea, $4c, $6d, $13, $d2, $e2, $70, $10, $f6, $2d, $c1, $d1,
      $7b, $96, $1b, $26, $68, $ba, $b1, $12, $9d, $f0, $ab, $e9, $1e, $be, $30, $a7,
      $c3, $8f, $f4, $33, $a1, $f1, $20, $f7, $3e, $da, $0e, $d3, $b0, $42, $64, $67,
      $98, $e5, $d5, $6e, $ad, $e6, $9b, $2e, $88, $ca, $25, $2a, $0d, $4b, $52, $93,
      $1a, $a2, $c7, $c0, $77, $35, $27, $f3, $87, $21, $48, $82, $bf, $dc, $b9, $76,
      $dd, $5c, $60, $59, $40, $22, $ec, $a0, $4d, $28, $7f, $eb, $3b, $c9, $9a, $5f,
      $fd, $63, $c5, $a4);

  const
    MKBOX: array [0..255] of byte =
      ($cd, $b1, $f3, $3a, $99, $38, $f4, $97, $a0, $1a, $62, $c1,
      $12, $52, $ea, $e8, $f5, $ec, $2c, $09, $7f, $6e, $79, $d1,
      $df, $88, $75, $ba, $87, $9f, $ee, $53, $aa, $3f, $56, $c2,
      $b5, $48, $d7, $4e, $e4, $03, $33, $f9, $4f, $08, $91, $d6,
      $1d, $b9, $c3, $54, $72, $06, $8b, $0d, $b8, $d8, $9c, $f8,
      $83, $c8, $4a, $77, $fe, $44, $32, $d2, $89, $cc, $20, $43,
      $9e, $46, $e5, $27, $17, $63, $a3, $96, $30, $3d, $de, $e1,
      $4c, $29, $c7, $95, $57, $05, $04, $37, $ac, $64, $94, $7d,
      $b7, $59, $82, $d4, $7c, $f6, $ce, $81, $e7, $dc, $4b, $8e,
      $ad, $60, $73, $14, $61, $cb, $66, $a6, $2f, $31, $78, $5c,
      $5b, $41, $c0, $4d, $34, $c4, $f0, $16, $6b, $76, $15, $36,
      $a9, $71, $26, $a4, $e0, $9a, $85, $47, $13, $3c, $11, $58,
      $2d, $1f, $0c, $7a, $d3, $7e, $6f, $6c, $be, $02, $fd, $3b,
      $cf, $0f, $a8, $b0, $90, $fa, $5d, $da, $28, $23, $bf, $42,
      $f1, $6a, $ab, $c9, $ae, $c6, $e6, $d9, $1e, $8f, $bd, $50,
      $86, $e2, $e3, $40, $67, $dd, $af, $10, $b4, $9b, $51, $22,
      $0b, $5e, $f7, $21, $25, $d5, $2b, $ca, $3e, $65, $1b, $bc,
      $c5, $a7, $ff, $8d, $74, $1c, $49, $92, $b6, $45, $a5, $fc,
      $98, $00, $68, $ef, $d0, $70, $b2, $39, $01, $b3, $2a, $5f,
      $eb, $5a, $8c, $8a, $07, $0e, $f2, $19, $7b, $bb, $18, $80,
      $84, $24, $a2, $0a, $69, $93, $a1, $ed, $9d, $55, $db, $6d,
      $2e, $e9, $35, $fb);

  const
    MGBOX: array [0..255] of byte =
      ($f2, $94, $5a, $4b, $0f, $73, $da, $db, $83, $1d, $b3, $09,
      $fc, $36, $b8, $46, $41, $d4, $d0, $55, $f3, $87, $76, $9a,
      $28, $35, $71, $fb, $f7, $1c, $03, $7d, $15, $84, $0a, $43,
      $fa, $9e, $60, $97, $98, $7e, $a3, $52, $d1, $29, $a5, $59,
      $c5, $7a, $2f, $25, $e1, $3b, $38, $0c, $90, $c6, $d9, $f6,
      $d8, $cf, $16, $95, $21, $a7, $6d, $1f, $58, $51, $48, $2a,
      $be, $ab, $f1, $5d, $9b, $f9, $80, $ed, $e5, $a6, $f5, $6f,
      $31, $ba, $79, $4f, $85, $81, $b1, $23, $e4, $72, $ea, $2c,
      $ca, $6b, $6a, $1e, $5c, $04, $d5, $ae, $b2, $e6, $c3, $39,
      $4d, $5f, $3a, $45, $b4, $67, $c8, $4e, $1a, $a4, $30, $e0,
      $cc, $10, $bd, $33, $ce, $b7, $e3, $34, $c4, $8a, $ff, $b0,
      $3f, $02, $7c, $dc, $bf, $08, $df, $8d, $fd, $6c, $eb, $12,
      $6e, $24, $cd, $2e, $99, $5e, $14, $8e, $50, $7f, $aa, $13,
      $89, $8f, $44, $e7, $88, $b9, $20, $26, $a1, $c7, $05, $07,
      $69, $ef, $de, $af, $18, $b6, $dd, $64, $01, $d2, $a2, $2b,
      $0d, $11, $a9, $0b, $bb, $c2, $82, $c1, $c0, $e9, $9f, $1b,
      $ee, $cb, $54, $00, $96, $ad, $27, $74, $5b, $68, $0e, $e8,
      $d6, $22, $a8, $ac, $3e, $3d, $fe, $70, $17, $77, $49, $06,
      $86, $37, $9d, $d7, $53, $63, $3c, $57, $2d, $78, $c9, $75,
      $8b, $62, $7b, $f8, $4c, $f0, $e2, $40, $a0, $93, $b5, $19,
      $65, $92, $56, $32, $47, $f4, $4a, $9c, $8c, $bc, $42, $ec,
      $d3, $66, $91, $61);

    constructor Create(const key: array of byte; const kl: longint{$IFDEF PARANOID};
      useiv: boolean = False{$ENDIF}; drop: integer = 0);
    procedure SetIV(const iv: array of byte; const ivl: longint);
    function PRNG: byte;
    function RandomU32: longword;
    function Randoms32: longint;
    function RandomU64: uint64;
    function RandomS64: int64;
    procedure GenerateRandom(var buffer: array of byte; len: integer);
    procedure Crypt(var buffer: array of byte; len: integer); overload;
    procedure Crypt(buffer: pointer; len: integer); overload;
    function RandomRange(const AFrom, ATo: integer; safe: boolean = False): integer;
    function Random(const max: integer; safe: boolean = False): integer;

  protected
  {$IFDEF PARANOID}
    savedkey: array [0..255] of byte;
    savedkl:  longint;
    droplen:  longint;
  {$ENDIF}

    procedure setup(const key: array of byte; const kl: longint; iv: boolean);

  end;


// For first user password string to binary key expansion

procedure ExpandUserKey(material: string; var key: array of byte; kl: integer);


implementation

type
  TByteArray = array [0..7] of byte;

type
  PByteArray = ^TByteArray;

{
  State array generation and transformation
}

procedure TAMPRNG.setup(const key: array of byte; const kl: longint; iv: boolean);

var
  i, idx: integer;
  t:      byte;
  nl, tk, seed: array [0..255] of byte;


  procedure Morph;
  var
    i: integer;
  begin

    for I := 0 to kl - 1 do
      seed[i] := MKBOX[seed[i]];

  end;


  procedure Cycle;
  var
    I: integer;
  begin
    for I := 0 to 2047 do
    begin
      idx    := i mod 256;
      m      := P[(P[idx] + NL[(m + TK[(seed[i mod kl] + idx) mod 256]) mod
        256]) mod 256];
      t      := P[idx];
      P[idx] := P[m];
      P[m]   := t;
    end;

  end;


  procedure MGCycle;
  var
    I: integer;
  begin

    for I := 0 to 2047 do
    begin
      idx   := i mod 256;
      z     := MG[(MG[idx] + NL[(z + TK[(seed[i mod kl] + idx) mod 256]) mod
        256]) mod 256];
      t     := MG[idx];
      MG[idx] := MG[z];
      MG[z] := t;
    end;

  end;


  procedure SWAP1;
  var
    T: array [0..255] of byte;
    i: integer;
  begin
    for I := 0 to 255 do
      T[i] := P[i];

    for I := 0 to 255 do
      P[i] := NL[i];

    for I := 0 to 255 do
      NL[i] := T[i];

  end;


  procedure SWAP2;
  var
    T: array [0..255] of byte;
    i: integer;
  begin
    for I := 0 to 255 do
      T[i] := P[i];

    for I := 0 to 255 do
      P[i] := TK[i];

    for I := 0 to 255 do
      TK[i] := T[i];

  end;


  procedure SWAP3;
  var
    T: array [0..255] of byte;
    i: integer;
  begin
    for I := 0 to 255 do
      T[i] := NL[i];

    for I := 0 to 255 do
      NL[i] := TK[i];

    for I := 0 to 255 do
      TK[i] := T[i];

  end;

begin

  for I := 0 to kl - 1 do
    seed[i] := key[i];

  for I := 0 to 255 do
  begin
    if not IV then
    begin
      P[i] := i;
    end;
    NL[i] := NLBOX[i];
    TK[i] := TKBOX[i];
  end;


  m := 0;

  Morph;
  Cycle;
  SWAP1;
  Morph;
  Cycle;
  SWAP2;
  Morph;
  Cycle;
  SWAP1;
  Morph;
  Cycle;
  Morph;
  SWAP3;
  Morph;

  if not IV then
  begin
    for I := 0 to 255 do
    begin
      MG[i] := MGBOX[i];
    end;
  end;

  z := 0;
  MGCycle;
  pos := 0;

end;

{
PRNG init routine:
generates P and MG states depending on key.

Set useiv to True if you want to call iv setup routine also.

}

constructor TAMPRNG.Create(const key: array of byte;
  const kl: integer{$IFDEF PARANOID}; useiv: boolean = False{$ENDIF}; drop: integer = 0);
var
  i: integer;
begin
  Self.setup(key, kl, False);

 {$IFDEF PARANOID}
  if useiv then
  begin

    savedkl := kl;

    for I := 0 to kl - 1 do
    begin
      savedkey[i] := key[i];
    end;

  end;
 {$ENDIF}

  droplen := drop;
end;


{
PRNG init routine:
transforms P and MG states depending on IV
}

procedure TAMPRNG.SetIV(const iv: array of byte; const ivl: integer);
var
  i: integer;
begin
  Self.setup(iv, ivl, True);

  {$IFDEF PARANOID}
  Self.setup(savedkey, savedkl, True);
  for I := 0 to 255 do
  begin
    savedkey[i] := 0;
  end;
  savedkl := 0;
 {$ENDIF}
end;


{
   One step of PRNG:
   Transforms P and MG states and outputs one byte
 }

function TAMPRNG.PRNG: byte;
var
  t: byte;
begin

  if droplen = 0 then
    droplen := 1;


  repeat

    m      := P[MG[(m + P[pos]) and 255]];
    z      := MG[P[(z + MG[pos]) and 255]];
    Result := P[P[(P[m] + 1) and 255]];
    t      := P[pos];
    P[pos] := P[m];
    P[m]   := t;
    t      := MG[pos];
    MG[pos] := MG[z];
    MG[z]  := t;
    pos    := (pos + 1) and 255;

    Dec(droplen);

  until droplen = 0;

end;


{

Example routines - PRNG wrappers

}

{
Random 32-bit unsigned integer
}

function TAMPRNG.RandomU32;
begin
  Result := 0;
  PByteArray(Result)[0] := prng;
  PByteArray(Result)[1] := prng;
  PByteArray(Result)[2] := prng;
  PByteArray(Result)[3] := prng;
end;

{
Random 32-bit signed integer
}


function TAMPRNG.RandomS32;
begin
  Result := 0;
  PByteArray(Result)[0] := prng;
  PByteArray(Result)[1] := prng;
  PByteArray(Result)[2] := prng;
  PByteArray(Result)[3] := prng;
end;

{
Random 64-bit unsigned integer
}


function TAMPRNG.RandomU64;
begin
  Result := 0;
  PByteArray(Result)[0] := prng;
  PByteArray(Result)[1] := prng;
  PByteArray(Result)[2] := prng;
  PByteArray(Result)[3] := prng;
  PByteArray(Result)[4] := prng;
  PByteArray(Result)[5] := prng;
  PByteArray(Result)[6] := prng;
  PByteArray(Result)[7] := prng;
end;

{
Random 64-bit signed integer
}


function TAMPRNG.RandomS64;
begin
  Result := 0;
  PByteArray(Result)[0] := prng;
  PByteArray(Result)[1] := prng;
  PByteArray(Result)[2] := prng;
  PByteArray(Result)[3] := prng;
  PByteArray(Result)[4] := prng;
  PByteArray(Result)[5] := prng;
  PByteArray(Result)[6] := prng;
  PByteArray(Result)[7] := prng;
end;


{
Genetates array of random bytes
}

procedure TAMPRNG.GenerateRandom(var buffer: array of byte; len: integer);
var
  i: integer;
begin
  for I := 0 to len - 1 do
    buffer[i] := prng;

end;


{
Encrypts array of bytes
}

procedure TAMPRNG.Crypt(var buffer: array of byte; len: integer);
var
  i: integer;
begin
  for I := 0 to len - 1 do
    buffer[i] := buffer[i] xor prng;

end;

procedure TAMPRNG.Crypt(buffer: pointer; len: integer);
var
  i: integer;
begin
  for I := 0 to len - 1 do
  begin
    byte(Buffer^)    := byte(Buffer^) xor prng;
    longword(Buffer) := longword(Buffer) + 1;
  end;

end;


function TAMPRNG.RandomRange(const AFrom, ATo: integer; safe: boolean = False): integer;
begin
  if AFrom > ATo then
    Result := Self.Random(AFrom - ATo, safe) + ATo
  else
    Result := Self.Random(ATo - AFrom, safe) + AFrom;
end;

function TAMPRNG.Random(const max: integer; safe: boolean = False): integer;
begin

  Result := 0;

  case safe of
    False:
      Result := Self.RandomS32 mod max;
    True:
    begin

      repeat

        Result := Self.Randoms32;

      until Result < max;

    end;

  end;

end;


(*******************************************************************************************

Usage:

Call create(key, key_length)
Call setiv (iv, iv_length)

for n = 1 to random_length do random[n] := prng

This gives you random bytes

if you want to crypt some data
use this:

Call create(key, key_length)
Call setiv (iv, iv_length)

for n = 1 to data_length do data[n] = data[n] xor prng

********************************************************************************************)


const
  ESB: array [0..255] of byte =
    ($74, $65, $2f, $32, $ee, $a2, $ff, $fe, $33, $aa, $35, $12,
    $02, $1a, $56, $a3, $51, $60, $81, $ba, $2b, $09, $55, $e9,
    $ea, $9e, $10, $c1, $4e, $79, $54, $3b, $44, $3d, $c2, $61,
    $66, $8b, $4f, $25, $38, $ca, $e3, $78, $7b, $c9, $00, $47,
    $fc, $e8, $d4, $70, $9d, $2d, $5f, $16, $8e, $03, $62, $ec,
    $69, $1c, $6f, $c5, $ae, $4d, $7a, $8a, $c4, $95, $85, $bf,
    $23, $df, $af, $d5, $22, $28, $5c, $b1, $ac, $86, $80, $19,
    $0d, $14, $b7, $4c, $71, $db, $fd, $13, $7e, $e6, $49, $a4,
    $d7, $48, $a9, $f5, $0c, $87, $1e, $18, $3c, $f2, $cc, $8f,
    $2c, $57, $68, $d9, $eb, $5e, $f3, $f1, $40, $99, $8c, $2e,
    $83, $36, $53, $6b, $41, $52, $b8, $9a, $dd, $17, $6e, $bb,
    $4a, $2a, $fb, $27, $43, $08, $c6, $a6, $b0, $ed, $cf, $3a,
    $a8, $42, $64, $9f, $82, $30, $31, $3e, $59, $6d, $98, $f6,
    $bc, $a7, $3f, $04, $b9, $11, $0f, $73, $67, $c0, $88, $75,
    $a5, $0a, $ef, $5d, $c8, $72, $e2, $63, $ab, $da, $d3, $d0,
    $de, $e7, $6c, $15, $c7, $d6, $5b, $f8, $45, $91, $6a, $a0,
    $21, $90, $5a, $46, $9b, $dc, $f0, $cd, $07, $b2, $ad, $b4,
    $96, $b6, $97, $76, $d1, $20, $f4, $bd, $d8, $c3, $d2, $7c,
    $b5, $9c, $1f, $1d, $94, $77, $fa, $e4, $be, $24, $34, $1b,
    $a1, $01, $29, $58, $84, $93, $92, $05, $f9, $e5, $7d, $ce,
    $7f, $89, $26, $b3, $f7, $4b, $37, $cb, $0b, $e0, $8d, $06,
    $e1, $50, $39, $0e);


procedure ExpandUserKey(material: string; var key: array of byte; kl: integer);
var
  keymat: array [0..63] of byte;
  AM:     TAMPRNG;
  I, L, max, step: integer;

  function rotate(a, b: byte): byte;
  asm
           MOV     AL, a
           MOV     CL, b
           ROL     AL, CL
  end;

begin

  l := length(material);

  for I := 0 to 63 do
    keymat[i] := 0;

  if l > 63 then
    max := l - 1
  else
    max := 63;

  step := 0;

  for I := 0 to max do
  begin

    if i mod 8 = 7 then
      step := step + 1;

    if i < (l - 1) then
      keymat[i mod 64] := ESB[keymat[i mod 64] + byte(material[(i mod l) + 1])]
    else
      keymat[i mod 64] := ESB[keymat[i mod 64] +
        rotate(byte(material[(i mod l) + 1]), ((i + step) mod 7) + 1)];

  end;


  am := TAMPRNG.Create(keymat, 64, False, 768);
  am.GenerateRandom(key, kl);
  am.Free;

end;

end.
