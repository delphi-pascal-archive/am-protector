program makernd;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Windows,
  amprng in 'amprng.pas';

var
  T: TextFile;
  i: integer;


var
  AM:   TAMPRNG;
  key:  array [0..79] of byte;
  tsc:  uint64;
  tsc2: int64;

  function RDTSC: UInt64; register;
  asm
           RDTSC
  end;

begin

  Randomize;

  for I := 0 to 63 do
    key[i] := random(256);

  tsc := rdtsc;

  tsc := tsc + (tsc shl 6) xor (tsc shl 9);

  QueryPerformanceCounter(tsc2);

  tsc2 := tsc2 + (tsc2 shl 6) xor (tsc2 shl 9);

  move(tsc, key[64], 8);
  move(tsc2, key[72], 8);

  am := TAMPRNG.Create(key, 80, False, 16384);

  Assignfile(T, 'rnd.inc');
  ReWrite(T);

  for I := 0 to 15 do
  begin

    WriteLn(T, 'ka[', i, '] := ', am.PRNG, ';');
    WriteLn(T, 'kb[', i, '] := ', am.PRNG, ';');

  end;


  am.Free;

  CloseFile(T);

end.
