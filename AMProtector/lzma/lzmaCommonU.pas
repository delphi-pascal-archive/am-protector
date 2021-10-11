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
unit lzmaCommonU;

interface

uses
  SysUtils, Classes;

const
  CODE_PROGRESS_INTERVAL = 200;
  //approx. number of times an OnProgress event will be fired during coding

  LZMA_DEF_BUFS_BUF_SIZE = $10000;//64K


type
  TLZMAProgressProc = procedure(const current, total: integer) of object;

  TLZMABFSMode = (lzmaBFMRead, lzmaBFMWrite);

  TLZMABufStream = class(TStream)
  private
    fStream: TStream;
    fOwn:    boolean;
    fMemBuffer: packed array [0..LZMA_DEF_BUFS_BUF_SIZE - 1] of byte;
    fBytesInBuffer: integer;
    fBufferPos: integer;
    fBufferDirty: boolean;
    fMode:   TLZMABFSMode;

    procedure Init();
    procedure Flush();
    procedure RefillBuffer();

  public
    constructor Create(aStream: TStream; aOwn: boolean = False);

    destructor Destroy(); override;

    function Read(var buffer; Count: longint): longint; override;
    function Write(const buffer; Count: longint): longint; override;
    function Seek(const offset: int64; origin: TSeekOrigin): int64; override;
  end;


function lzmaStreamReadByte(const stream: TStream): byte;
procedure lzmaStreamWriteByte(const stream: TStream; const b: byte);


implementation


type
  PByteArray = ^TByteArray;
  TByteArray = packed array of byte;


function lzmaStreamReadByte(const stream: TStream): byte;
begin
  stream.ReadBuffer(Result, 1);
end;

procedure lzmaStreamWriteByte(const stream: TStream; const b: byte);
begin
  stream.WriteBuffer(b, 1);
end;


function Min(a, b: integer): integer;
begin
  if a > b then
    Result := b
  else
    Result := a;
end;

function MovePointer(const p: Pointer; const dist: integer): Pointer;
begin
  Result := Pointer(integer(p) + dist);
end;


{ TLZMABufStream }
constructor TLZMABufStream.Create(aStream: TStream; aOwn: boolean = False);
begin
  inherited Create();
  fOwn    := aOwn;
  fStream := aStream;
  Init();
end;

destructor TLZMABufStream.Destroy();
begin
  Flush();
  if fOwn then
    FreeAndNil(fStream);
  inherited;
end;

procedure TLZMABufStream.Init();
begin
  fBytesInBuffer := 0;
  fBufferPos := 0;
  fBufferDirty := False;
  fMode := lzmaBFMWrite;
end;

procedure TLZMABufStream.Flush();
begin
  if fBufferDirty then
    fStream.WriteBuffer(fMemBuffer[0], fBufferPos);
  fBufferDirty   := False;
  fBytesInBuffer := 0;
  fBufferPos     := 0;
end;

procedure TLZMABufStream.RefillBuffer();
var
  rd: int64;
begin
  Flush();
  rd := fStream.Size - fStream.Position;
  if rd > LZMA_DEF_BUFS_BUF_SIZE then
    rd := LZMA_DEF_BUFS_BUF_SIZE;
  if rd > 0 then
    fStream.ReadBuffer(fMemBuffer, rd);
  fBytesInBuffer := rd;
  fBufferPos     := 0;
end;

function TLZMABufStream.Read(var buffer; Count: longint): longint;
var
  p: PByteArray;
  bytesToRead: integer;
  b: integer;
begin
  if fMode = lzmaBFMWrite then
    Flush();
  fMode  := lzmaBFMRead;
  Result := 0;
  if Count <= fBytesInBuffer then
  begin
    //all data already in buffer
    Move(fMemBuffer[fBufferPos], buffer, Count);
    fBytesInBuffer := fBytesInBuffer - Count;
    fBufferPos := fBufferPos + Count;
    Result := Count;
  end
  else
  begin
    bytesToRead := Count;
    if (bytesToRead <> 0) and (fBytesInBuffer <> 0) then
    begin
      //read data remaining in buffer and increment data pointer
      b      := Read(buffer, fBytesInBuffer);
      p      := PByteArray(@(TByteArray(buffer)[b]));
      bytesToRead := bytesToRead - b;
      Result := b;
    end
    else
      p := @buffer;
    if bytesToRead >= LZMA_DEF_BUFS_BUF_SIZE then
    begin
      //data to read is larger than the buffer, read it directly
      Result := Result + fStream.Read(p^, bytesToRead);
    end
    else
    begin //refill buffer
      RefillBuffer();
      //recurse
      Result := Result + Read(p^, Min(bytesToRead, fBytesInBuffer));
    end;
  end;
end;

function TLZMABufStream.Write(const buffer; Count: longint): longint;
var
  p: Pointer;
  bytesToWrite: integer;
  b: integer;
begin
  if fMode = lzmaBFMRead then
  begin
    Seek(-LZMA_DEF_BUFS_BUF_SIZE + fBufferPos, soFromCurrent);
    fBytesInBuffer := 0;
    fBufferPos     := 0;
  end;
  fMode  := lzmaBFMWrite;
  Result := 0;
  if Count <= LZMA_DEF_BUFS_BUF_SIZE - fBytesInBuffer then
  begin
    //all data fits in buffer
    fBufferDirty := True;
    Move(buffer, fMemBuffer[fBufferPos], Count);
    fBytesInBuffer := fBytesInBuffer + Count;
    fBufferPos := fBufferPos + Count;
    Result := Count;
  end
  else
  begin
    bytesToWrite := Count;
    if (bytesToWrite <> 0) and (fBytesInBuffer <> LZMA_DEF_BUFS_BUF_SIZE) and
      (fBytesInBuffer <> 0) then
    begin
      //write data to remaining space in buffer and increment data Pointer
      b      := Write(buffer, LZMA_DEF_BUFS_BUF_SIZE - fBytesInBuffer);
      p      := MovePointer(@buffer, b);
      bytesToWrite := bytesToWrite - b;
      Result := b;
    end
    else
      p := @buffer;
    if bytesToWrite >= LZMA_DEF_BUFS_BUF_SIZE then
    begin
      //empty buffer
      Flush();
      //data to write is larger than the buffer, write it directly
      Result := Result + fStream.Write(p^, bytesToWrite);
    end
    else
    begin //empty buffer
      Flush();
      //recurse
      Result := Result + Write(p^, bytesToWrite);
    end;
  end;
end;

function TLZMABufStream.Seek(const offset: int64; origin: TSeekOrigin): int64;
begin
  if (origin = soCurrent) and (offset = 0) then
    Result := fStream.Seek(offset, origin) + fBufferPos
  else
  begin
    Flush();
    Result := fStream.Seek(offset, origin);
  end;
end;


end.
