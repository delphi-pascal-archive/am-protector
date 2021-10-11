(*======================================================================*
 | unitResourceDetails                                                  |
 |                                                                      |
 | Ultra-light classes to wrap resources and resource modules.          |
 |                                                                      |
 | TResourceModule is an abstract base class for things that can        |
 | provide lists of resources - eg. .RES files, modules, etc.           |
 |                                                                      |
 | TResourceDetails is a base class for resources.                      |
 |                                                                      |
 | ... and here's the neat trick...                                     |
 |                                                                      |
 | Call the class function TResourceDetails.CreateResourceDetails to    |
 | create an instance of the appropriate registered TResourceDetails    |
 | descendant                                                           |
 |                                                                      |
 | ** Gold code **                                                      |
 |                                                                      |
 | Copyright (c) Colin Wilson 2001                                      |
 |                                                                      |
 | All rights reserved                                                  |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      06/02/2001  CPWW  Original                                  |
 |          28/05/2005  CPWW  ClearDirty made Protected instead of      |
 |                            Public                                    |
 |                            TResourceDetails.Create can now take      |
 |                            optional data.                            | 
 *======================================================================*)


unit unitResourceDetails;

interface

uses Windows, Classes, SysUtils;

type

  TResourceDetails      = class;
  TResourceDetailsClass = class of TResourceDetails;

{$region 'TResourceModule class'}
  //======================================================================
  // TResourceModule class

  TResourceModule = class
  private
    fDirty: boolean;
    function GetDirty: boolean;
  protected
    function GetResourceCount: integer; virtual; abstract;
    function GetResourceDetails(idx: integer): TResourceDetails; virtual; abstract;
    procedure ClearDirty;

  public
    procedure DeleteResource(idx: integer); virtual;
    procedure InsertResource(idx: integer; details: TResourceDetails); virtual;
    function AddResource(details: TResourceDetails): integer; virtual;
    function IndexOfResource(details: TResourceDetails): integer; virtual; abstract;
    function GetUniqueResourceName(const tp: string): string;

    procedure SaveToStream(stream: TStream); virtual;
    procedure LoadFromStream(stream: TStream); virtual;

    procedure SaveToFile(const FileName: string); virtual;
    procedure LoadFromFile(const FileName: string); virtual;
    procedure SortResources; virtual;

    function FindResource(const tp, Name: string;
      ALanguage: integer): TResourceDetails;

    property ResourceCount: integer Read GetResourceCount;
    property ResourceDetails[idx: integer]: TResourceDetails Read GetResourceDetails;
    property Dirty: boolean Read GetDirty Write fDirty;
  end;

{$endregion}

{$region 'TResourceDetails class'}
  //======================================================================
  // TResourceDetails class

  TResourceDetails = class
  private
    fParent:   TResourceModule;
    fData:     TMemoryStream;
    fCodePage: integer;
    fResourceLanguage: LCID;
    fResourceName: string;
    fResourceType: string;

    fMemoryFlags: word;                    // Resource memory flags
    fDataVersion, fVersion: DWORD;         // Resource header version info
    fCharacteristics: DWORD;
    fDirty: boolean;
    fTag:   integer;
    // Resource header characteristics

  protected
    constructor Create(AParent: TResourceModule; ALanguage: integer;
      const AName, AType: string; ASize: integer; AData: pointer); virtual;
    procedure InitNew; virtual;
    procedure SetResourceName(const Value: string); virtual;
    class function SupportsRCData(const AName: string; Size: integer;
      Data: Pointer): boolean; virtual;
    class function SupportsData(Size: integer; Data: Pointer): boolean; virtual;
  public
    class function CreateResourceDetails(AParent: TResourceModule;
      ALanguage: integer; const AName, AType: string; ASize: integer;
      AData: pointer): TResourceDetails;
    class function GetBaseType: string; virtual;

    constructor CreateNew(AParent: TResourceModule; ALanguage: integer;
      const AName: string); virtual;
    destructor Destroy; override;
    procedure BeforeDelete; virtual;

    procedure ChangeData(newData: TMemoryStream); virtual;

    property Parent: TResourceModule Read fParent;
    property Data: TMemoryStream Read fData;
    property ResourceName: string Read fResourceName Write SetResourceName;
    property ResourceType: string Read fResourceType;
    property ResourceLanguage: LCID Read fResourceLanguage Write fResourceLanguage;

    property CodePage: integer Read fCodePage Write fCodePage;
    property Characteristics: DWORD Read fCharacteristics Write fCharacteristics;
    property Version: DWORD Read fVersion Write fDataVersion;
    property DataVersion: DWORD Read fDataVersion Write fDataVersion;
    property MemoryFlags: word Read fMemoryFlags Write fMemoryFlags;

    property Dirty: boolean Read fDirty Write fDirty;
    property Tag: integer Read fTag Write fTag;
  end;

{$endregion}

{$region 'TAnsiResourceDetails class'}
  //======================================================================
  // TAnsiResourceDetails class

  TAnsiResourceDetails = class(TResourceDetails)
  private
    function GetText: string;
    procedure SetText(const Value: string);
  protected
    procedure InitNew; override;
    class function SupportsData(Size: integer; Data: Pointer): boolean; override;
  public
    property Text: string Read GetText Write SetText;
  end;

{$endregion}

{$region 'TUnicodeResourceDetails'}
  //======================================================================
  // TAnsiResourceDetails class

  TUnicodeResourceDetails = class(TResourceDetails)
  private
    function GetText: WideString;
    procedure SetText(const Value: WideString);
  protected
    procedure InitNew; override;
    class function SupportsData(Size: integer; Data: Pointer): boolean; override;
  public
    property Text: WideString Read GetText Write SetText;
  end;

{$endregion}

 //======================================================================
 // Global function definitions

procedure RegisterResourceDetails(resourceClass: TResourceDetailsClass);
procedure UnRegisterResourceDetails(resourceClass: TResourceDetailsClass);
function ResourceWideCharToStr(var wstr: PWideChar; codePage: integer): string;
procedure ResourceStrToWideChar(const s: string; var p: PWideChar; codePage: integer);
function ResourceNameToInt(const s: string): integer;
function CompareDetails(p1, p2: Pointer): integer;

implementation

{$region 'Local Declarations and Functions'}
var
  registeredResourceDetails:      array of TResourceDetailsClass;
  registeredResourceDetailsCount: integer = 0;

resourcestring
  rstNoBaseType  = 'Can''t register resource details class with no base type';
  rstNoStreaming = 'Module doesn''t support streaming';

(*----------------------------------------------------------------------*
 | procedure RegisterResourceDetails                                    |
 |                                                                      |
 | Add a class, derived from TResourceDetails, to the list of           |
 | registered resource details classes                                  |
 *----------------------------------------------------------------------*)
procedure RegisterResourceDetails(resourceClass: TResourceDetailsClass);
begin
  if Length(registeredResourceDetails) = registeredResourceDetailsCount then
    SetLength(registeredResourceDetails, Length(registeredResourceDetails) + 10);

  registeredResourceDetails[registeredResourceDetailsCount] := resourceClass;

  Inc(registeredResourceDetailsCount);
end;

(*----------------------------------------------------------------------*
 | procedure UnRegisterResourceDetails                                  |
 |                                                                      |
 | Remove a class, derived from TResourceDetails, from the list of      |
 | registered resource details classes                                  |
 *----------------------------------------------------------------------*)
procedure UnRegisterResourceDetails(resourceClass: TResourceDetailsClass);
var
  i: integer;
begin
  i := 0;
  while i < registeredResourceDetailsCount do
    if registeredResourceDetails[i] = resourceClass then
    begin
      if i < Length(registeredResourceDetails) - 1 then
        Move(registeredResourceDetails[i + 1], registeredResourceDetails[i],
          (Length(registeredResourceDetails) - i - 1) * sizeof(TResourceDetailsClass));

      Dec(registeredResourceDetailsCount);
    end
    else
      Inc(i);
end;

(*----------------------------------------------------------------------------*
 | procedure ResourceWideCharToStr ()                                         |
 |                                                                            |
 | Convert Pascal-style WideChar array to a string                            |
 |                                                                            |
 | Parameters:                                                                |
 |   WStr : PWChar             The characters                                 |
 *----------------------------------------------------------------------------*)
function ResourceWideCharToStr(var wstr: PWideChar; codePage: integer): string;
var
  len: word;
begin
  len := word(wstr^);
  SetLength(Result, len);
  Inc(wstr);
  WideCharToMultiByte(codePage, 0, WStr, Len, PChar(Result), Len + 1, nil, nil);
  Inc(wstr, len);
  Result := PChar(Result);
end;

(*----------------------------------------------------------------------------*
 | procedure ResourceStrToWideChar ()                                         |
 |                                                                            |
 | Convert a string to a Pascal style Wide char array                         |
 |                                                                            |
 | Parameters:                                                                |
 |   s : string                The string                                     |
 |   var p : PWideChar         [in]  Points to the start of the receiving buf |
 |                             [out] Points after the characters.             |
 *----------------------------------------------------------------------------*)
procedure ResourceStrToWideChar(const s: string; var p: PWideChar; codePage: integer);
var
  buffer:    PWideChar;
  len, size: word;
begin
  len  := Length(s);
  size := (Length(s) + 1) * sizeof(widechar);
  GetMem(buffer, size);
  try
    MultiByteToWideChar(codePage, 0, PChar(s), -1, buffer, size);
    p^ := widechar(len);
    Inc(p);
    Move(buffer^, p^, len * sizeof(widechar));
    Inc(p, len)
  finally
    FreeMem(buffer)
  end;
end;

(*----------------------------------------------------------------------*
 | procedure ResourceNameToInt                                          |
 |                                                                      |
 | Get integer value of resource name (or type).  Return -1 if it's     |
 | not numeric.                                                         |
 *----------------------------------------------------------------------*)
function ResourceNameToInt(const s: string): integer;
var
  isNumeric: boolean;
  i: integer;
begin
  isNumeric := Length(s) > 0;
  for i := 1 to Length(s) do
    if not (s[i] in ['0'..'9']) then
    begin
      isNumeric := False;
      break;
    end;

  if isNumeric then
    Result := StrToInt(s)
  else
    Result := -1;
end;

(*----------------------------------------------------------------------*
 | function CompareDetails                                              |
 |                                                                      |
 | 'Compare' function used when sorting resources.  p1 and p2 must be   |
 | TResourceDetails references.  Returns > 0 if details at p1 are >     |
 | details at p2.                                                       |
 |                                                                      |
 | *  Compare resource types.  If they match then compare names.        |
 | *  'Integer' ids or names must come *after* non integer ids or names.|
 *----------------------------------------------------------------------*)
function CompareDetails(p1, p2: Pointer): integer;
var
  d1:     TResourceDetails;
  d2:     TResourceDetails;
  i1, i2: integer;
begin
  d1 := TResourceDetails(p1);
  d2 := TResourceDetails(p2);

  i1 := ResourceNameToInt(d1.ResourceType);
  i2 := ResourceNameToInt(d2.ResourceType);

  if i1 >= 0 then
    if i2 >= 0 then
      Result := i1 - i2         // Compare two integer ids
    else
      Result := 1               // id1 is int, so it's greater than non-int id2
  else
  if i2 >= 0 then
    Result := -1              // id2 is int, so it's less than non-int id1
  else
    // Compare two string resource ids
    Result := CompareText(d1.ResourceType, d2.ResourceType);

  if Result = 0 then            // If they match, do the same with the names
  begin
    i1 := ResourceNameToInt(d1.ResourceName);
    i2 := ResourceNameToInt(d2.ResourceName);

    if i1 >= 0 then
      if i2 >= 0 then
        Result := i1 - i2
      else
        Result := 1
    else
    if i2 >= 0 then
      Result := -1
    else
      Result := CompareText(d1.ResourceName, d2.ResourceName);
  end;
end;

(*----------------------------------------------------------------------*
 | function LCIDTOCodePage                                              |
 |                                                                      |
 | Get the ANSI code page for a given language ID                       |
 *----------------------------------------------------------------------*)
function LCIDToCodePage(ALcid: LCID): integer;
var
  Buffer: array [0..6] of char;
begin
  GetLocaleInfo(ALcid, LOCALE_IDEFAULTANSICODEPAGE, Buffer, SizeOf(Buffer));
  Result := StrToIntDef(Buffer, GetACP);
end;

{$endregion}

{$region 'TResourceDetails implementation'}
{ TResourceDetails }

(*----------------------------------------------------------------------*
 | TResourceDetails.BeforeDelete                                        |
 |                                                                      |
 | Can override this to clear up before deleting.  Eg. deleting an      |
 | icon removes it from the icon group it's in.  Deleting an icon group |
 | removes the individual icon resources, etc.                          |
 *----------------------------------------------------------------------*)
procedure TResourceDetails.BeforeDelete;
begin
  // Stub
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.ChangeData                                          |
 |                                                                      |
 | Change all the data.  Handy for implementing 'undo', etc.            |
 *----------------------------------------------------------------------*)
procedure TResourceDetails.ChangeData(newData: TMemoryStream);
begin
  fData.Clear;
  fData.CopyFrom(newData, 0);
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.Create                                              |
 |                                                                      |
 | Raw - protected - constructor for resource details.                  |
 *----------------------------------------------------------------------*)
constructor TResourceDetails.Create(AParent: TResourceModule;
  ALanguage: integer; const AName, AType: string; ASize: integer; AData: pointer);
begin
  fParent   := AParent;
  fResourceLanguage := ALanguage;
  fCodePage := LCIDToCodePage(fResourceLanguage);
  fResourceName := AName;
  fResourceType := AType;
  fData     := TMemoryStream.Create;
  if AData <> nil then
    fData.Write(AData^, ASize)
  else
    InitNew;
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.CreateNew                                           |
 |                                                                      |
 | Constructor to be used when adding new resources to a module.        |
 *----------------------------------------------------------------------*)
constructor TResourceDetails.CreateNew(AParent: TResourceModule;
  ALanguage: integer; const aName: string);
begin
  fParent   := AParent;
  fResourceLanguage := ALanguage;
  fCodePage := LCIDToCodePage(fResourceLanguage);
  fResourceName := AName;
  fResourceType := GetBaseType;
  if Assigned(AParent) then
    AParent.AddResource(Self);
  fData := TMemoryStream.Create;
  InitNew;
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.CreateResourceDetails                               |
 |                                                                      |
 | Create a class derived from TResourceDetals that reflects the 'Type' |
 | If no matching class is registered, create a base 'TResourceDetails' |
 | class.    (Ha!  Try doing *that* in C++ ! )                          |
 *----------------------------------------------------------------------*)
class function TResourceDetails.CreateResourceDetails(AParent: TResourceModule;
  ALanguage: integer; const AName, AType: string; ASize: integer;
  AData: pointer): TResourceDetails;
var
  i: integer;
begin
  Result := nil;

  if (Length(AType) > 0) then
    try

      // Check for exact match

      for i := 0 to registeredResourceDetailsCount - 1 do
        if registeredResourceDetails[i].GetBaseType = AType then
        begin
          if (AType <> IntToStr(integer(RT_RCDATA))) or
            registeredResourceDetails[i].SupportsRCData(AName, ASize, AData) then
          begin
            Result := registeredResourceDetails[i].Create(AParent,
              ALanguage, AName, AType, ASize, AData);
            break;
          end;
        end;
    except
    end;

  // If no exact match, check each clas to see if it supports the data
  if Result = nil then
    try
      for i := 0 to registeredResourceDetailsCount - 1 do
        if registeredResourceDetails[i].SupportsData(ASize, AData) then
        begin
          Result := registeredResourceDetails[i].Create(AParent,
            ALanguage, AName, AType, ASize, AData);
          break;
        end;
    except
    end;

  if Result = nil then
    if TAnsiResourceDetails.SupportsData(ASize, AData) then
      Result := TAnsiResourceDetails.Create(AParent, ALanguage,
        AName, AType, ASize, AData)
    else
    if TUnicodeResourceDetails.SupportsData(ASize, AData) then
      Result := TUnicodeResourceDetails.Create(AParent, ALanguage,
        AName, AType, ASize, AData)
    else
      Result := TResourceDetails.Create(AParent, ALanguage, AName,
        AType, ASize, AData);
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.Destroy                                             |
 *----------------------------------------------------------------------*)
destructor TResourceDetails.Destroy;
begin
  fData.Free;
  inherited;
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.GetBaseType                                         |
 |                                                                      |
 | Return the base type for the resource details.  This is overridden   |
 | in derived classes.                                                  |
 *----------------------------------------------------------------------*)
class function TResourceDetails.GetBaseType: string;
begin
  Result := '0';
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.InitNew                                             |
 |                                                                      |
 | Override this to initialize a new resource being added to a module.  |
 *----------------------------------------------------------------------*)
procedure TResourceDetails.InitNew;
begin
  // Stub
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.SetResourceName                                     |
 |                                                                      |
 | Set the resource name.                                               |
 *----------------------------------------------------------------------*)
procedure TResourceDetails.SetResourceName(const Value: string);
begin
  fResourceName := Value;
  fDirty := True;
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.SupportsData                                        |
 |                                                                      |
 | Can be overridden to support a custom resource class, where you can  |
 | determine the custom class from the data - eg. RIFF data, etc.       |
 *----------------------------------------------------------------------*)
class function TResourceDetails.SupportsData(Size: integer; Data: Pointer): boolean;
begin
  Result := False; // stub
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.SupportsData                                        |
 |                                                                      |
 | Can be overridden to support RC data where you can determine the     |
 | type from the data and name - eg. the Delphi splash screen JPEG      |
 *----------------------------------------------------------------------*)
class function TResourceDetails.SupportsRCData(const AName: string;
  Size: integer; Data: Pointer): boolean;
begin
  Result := False; // stub
end;

{$endregion}

{$region 'TResourceModule implementation'}
{ TResourceModule }

function TResourceModule.AddResource(details: TResourceDetails): integer;
begin
  Result := -1;
  // Stub
end;

procedure TResourceModule.ClearDirty;
var
  i: integer;
begin
  fDirty := False;
  for i := 0 to ResourceCount - 1 do
    ResourceDetails[i].Dirty := False;
end;

(*----------------------------------------------------------------------*
 | TResourceModule.DeleteResource                                       |
 |                                                                      |
 | Must be overridden to remove the resource details object from        |
 | wherever it's stored.  The overriding method must call               |
 | inherited                                                            |
 *----------------------------------------------------------------------*)
procedure TResourceModule.DeleteResource(idx: integer);
begin
  fDirty := True;
  ResourceDetails[idx].BeforeDelete;
end;

(*----------------------------------------------------------------------*
 | TResourceModule.FindResource                                         |
 |                                                                      |
 | Find a resource with a given type/name                               |
 *----------------------------------------------------------------------*)
function TResourceModule.FindResource(const tp, Name: string;
  ALanguage: integer): TResourceDetails;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to ResourceCount - 1 do
    if (ResourceDetails[i].fResourceType = tp) and
      (ResourceDetails[i].fResourceName = Name) and
      (integer(ResourceDetails[i].fResourceLanguage) = ALanguage) then
    begin
      Result := ResourceDetails[i];
      break;
    end;

  if not Assigned(Result) then
    for i := 0 to ResourceCount - 1 do
      if (ResourceDetails[i].fResourceType = tp) and
        (ResourceDetails[i].fResourceName = Name) and
        (ResourceDetails[i].fResourceLanguage = 0) then
      begin
        Result := ResourceDetails[i];
        break;
      end;
end;

(*----------------------------------------------------------------------*
 | TResourceModule.GetDirty                                             |
 |                                                                      |
 | Returns true if the module or it's resources are 'dirty'             |
 |                                                                      |
 | nb. fDirty is only set if resources have been deleted.               |
 |     After adding a resource make sure the resource's Dirty is set to |
 |     true.                                                            |
 *----------------------------------------------------------------------*)
function TResourceModule.GetDirty: boolean;
var
  i: integer;
begin
  Result := fDirty;
  if not fDirty then
    for i := 0 to ResourceCount - 1 do
      if ResourceDetails[i].Dirty then
      begin
        Result := True;
        break;
      end;
end;

(*----------------------------------------------------------------------*
 | TResourceModule.GetUniqueResourceName                                |
 |                                                                      |
 | Generate a unique resource name for a given type.  Names start at    |
 | 1 (though string lists downgrade that to '0')                        |
 *----------------------------------------------------------------------*)
function TResourceModule.GetUniqueResourceName(const tp: string): string;
var
  i:     integer;
  n, n1: integer;
  details: TResourceDetails;
begin
  n := 0;

  for i := 0 to ResourceCount - 1 do
  begin
    details := ResourceDetails[i];
    if details.ResourceType = tp then
    begin
      n1 := ResourceNametoInt(details.ResourceName);
      if n1 > n then
        n := n1;
    end;
  end;

  Result := IntToStr(n + 1);
end;

procedure TResourceModule.InsertResource(idx: integer; details: TResourceDetails);
begin
  // Stub
end;

(*----------------------------------------------------------------------*
 | TResourceModule.LoadFromFile                                         |
 |                                                                      |
 | Load from file.  This can be overriden but usually isn't as it       |
 | relies on LoadFromStream, which must be.                             |
 *----------------------------------------------------------------------*)
procedure TResourceModule.LoadFromFile(const FileName: string);
var
  s: TFileStream;
begin
  s := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(s);
  finally
    s.Free
  end;
end;


procedure TResourceModule.LoadFromStream(stream: TStream);
begin
  raise Exception.Create(rstNoStreaming);
end;

(*----------------------------------------------------------------------*
 | TResourceModule.SaveToFile                                           |
 |                                                                      |
 | Save to file.  This can be overriden but usually isn't as it         |
 | relies on SaveToStream, which must be.                               |
 *----------------------------------------------------------------------*)
procedure TResourceModule.SaveToFile(const FileName: string);
var
  s: TFileStream;
  oldFileName, ext: string;
  p: PChar;
begin
  // Rename old file to .~ext'
  oldFileName := FileName;
  UniqueString(oldFileName);
  p := StrRScan(PChar(oldFileName), '.');
  if p <> nil then
  begin
    p^ := #0;
    Inc(p);
    ext := p;
    oldFileName := PChar(oldFileName);
  end
  else
    ext := '';
  ext := '~' + ext;
  oldFileName := oldFileName + '.' + ext;

  if FileExists(oldFileName) then
    DeleteFile(oldFileName);

  RenameFile(FileName, oldFileName);

  try
    s := TFileStream.Create(FileName, fmCreate);
    try
      SaveToStream(s);
      ClearDirty
    finally
      s.Free
    end
  except
    // Failed.  Rename old file back.
    DeleteFile(FileName);
    RenameFile(oldFileName, FileName);
    raise
  end;
end;

procedure TResourceModule.SaveToStream(stream: TStream);
begin
  raise Exception.Create(rstNoStreaming);
end;

procedure TResourceModule.SortResources;
begin
  // Stub
end;

{$endregion}

{$region 'TAnsiResourceDetails implementation'}
{ TAnsiResourceDetails }

function TAnsiResourceDetails.GetText: string;
begin
  Data.Seek(0, soFromBeginning);
  SetString(Result, PChar(Data.Memory), Data.Size);
end;

procedure TAnsiResourceDetails.InitNew;
begin
  Data.Clear;
end;

procedure TAnsiResourceDetails.SetText(const Value: string);
begin
  Data.Clear;
  Data.Write(Value[1], Length(Value));
end;

class function TAnsiResourceDetails.SupportsData(Size: integer; Data: Pointer): boolean;
var
  i, sample: integer;
  pc: PChar;
begin
  Result := Size > 0;
  sample := Size;
  if Sample > 1024 then
    Sample := 1024;
  pc := PChar(Data);

  if Result then
    for i := 0 to Sample - 1 do
    begin
      if (pc^ < ' ') or (pc^ > #127) then
        if not (pc^ in [#9, #10, #13]) then
        begin
          Result := False;
          break;
        end;

      Inc(pc);
    end;
end;

{$endregion}

{$region 'TUnicodeResourceDetails implementation'}
{ TUnicodeResourceDetails }

function TUnicodeResourceDetails.GetText: WideString;
begin
  SetLength(Result, Data.Size div sizeof(widechar));
  Move(Data.Memory^, Result[1], Data.Size);
end;

procedure TUnicodeResourceDetails.InitNew;
begin
  Data.Clear;
end;

procedure TUnicodeResourceDetails.SetText(const Value: WideString);
begin
  Data.Write(Value[1], Length(Value) * sizeof(widechar));
end;

class function TUnicodeResourceDetails.SupportsData(Size: integer;
  Data: Pointer): boolean;
var
  i, sample: integer;
  pc: PChar;
begin
  Result := Size > 5;
  sample := Size div 2;
  if Sample > 1024 then
    Sample := 1024
  else
    Dec(Sample);
  pc := PChar(Data);

  if Result then
    for i := 0 to Sample - 2 do
    begin
      if (pc^ < ' ') or (pc^ > #127) then
        if not (pc^ in [#9, #10, #13]) then
        begin
          Result := False;
          break;
        end;

      Inc(pc, 2);
    end;
end;

{$endregion}

end.
