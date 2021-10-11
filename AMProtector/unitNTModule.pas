(*======================================================================*
 | unitNTModule unit                                                    |
 |                                                                      |
 | Load resources from a module                                         |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2002.  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      04/04/2002  CPWW  Original                                  |
 |          28/05/2005  CPWW  Implemented changing resources.           |
 *======================================================================*)

unit unitNTModule;

interface

uses Windows, Classes, SysUtils, unitResourceDetails, ConTnrs;

type

  TNTModule = class(TResourceModule)
  private
    fDetailList: TObjectList;
    fTag: integer;

    procedure AddResourceToList(AType, AName: PWideChar; ADataLen: integer;
      AData: pointer; ALang: word);
    function LoadResourceFromModule(hModule: integer; const resType, resName: PChar;
      language: word): boolean;
  protected
    function GetResourceCount: integer; override;
    function GetResourceDetails(idx: integer): TResourceDetails; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
    procedure LoadResources(const fileName: string; tp: PChar);
    procedure DeleteResource(resourceNo: integer); override;
    procedure InsertResource(idx: integer; details: TResourceDetails); override;
    function AddResource(details: TResourceDetails): integer; override;
    function IndexOfResource(details: TResourceDetails): integer; override;
    property Tag: integer Read fTag Write fTag;
  end;

implementation

resourcestring
  rstCantUpdate = 'Must use Windows NT, 2000 or XP to update resoures';

type
  TfnBeginUpdateResource = function(pFileName: PChar;
    bDeleteExistingResources: BOOL): THandle; stdcall;
  TfnUpdateResource = function(hUpdate: THandle; lpType, lpName: PChar;
    wLanguage: word; lpData: Pointer; cbData: DWORD): BOOL; stdcall;
  TfnEndUpdateResource = function(hUpdate: THandle; fDiscard: BOOL): BOOL; stdcall;

var
  fnBeginUpdateResource: TfnBeginUpdateResource = nil;
  fnEndUpdateResource:   TfnEndUpdateResource = nil;
  fnUpdateResource:      TfnUpdateResource = nil;

(*----------------------------------------------------------------------------*
 | function EnumResLangProc ()                                                |
 |                                                                            |
 | Callback for EnumResourceLanguages                                         |
 |                                                                            |
 | lParam contains the resource module instance.                              |
 *----------------------------------------------------------------------------*)
function EnumResLangProc(hModule: integer; resType, resName: PChar;
  wIDLanguage: word; lParam: integer): BOOL; stdcall;
begin
  TNTModule(lParam).LoadResourceFromModule(hModule, resType, resName, wIDLanguage);
  Result := True;
end;

(*----------------------------------------------------------------------*
 | EnumResNamesProc                                                     |
 |                                                                      |
 | Callback for EnumResourceNames                                       |
 |                                                                      |
 | lParam contains the resource module instance.                        |
 *----------------------------------------------------------------------*)
function EnumResNamesProc(hModule: integer; resType, resName: PChar;
  lParam: integer): BOOL; stdcall;
begin
  if not EnumResourceLanguages(hModule, resType, resName, @EnumResLangProc, lParam) then
    RaiseLastOSError;
  Result := True;
end;

(*----------------------------------------------------------------------*
 | EnumResTypesProc                                                     |
 |                                                                      |
 | Callback for EnumResourceTypes                                       |
 |                                                                      |
 | lParam contains the resource module instance.                        |
 *----------------------------------------------------------------------*)
function EnumResTypesProc(hModule: integer; resType: PChar;
  lParam: integer): BOOL; stdcall;
begin
  EnumResourceNames(hModule, resType, @EnumResNamesProc, lParam);
  Result := True;
end;

{ TNTModule }

const
  rstNotSupported = 'Not supported';

(*----------------------------------------------------------------------*
 | TNTModule.AddResourceToList                                          |
 |                                                                      |
 | Add resource to the resource details list                            |
 *----------------------------------------------------------------------*)
function TNTModule.AddResource(details: TResourceDetails): integer;
begin
  Result := fDetailList.Add(details);
end;

procedure TNTModule.AddResourceToList(AType, AName: PWideChar;
  ADataLen: integer; AData: pointer; ALang: word);
var
  details: TResourceDetails;

  function ws(ws: PWideChar): string;
  begin
    if (integer(ws) and $ffff0000) <> 0 then
      Result := ws
    else
      Result := IntToStr(integer(ws));
  end;

begin
  details := TResourceDetails.CreateResourceDetails(self, ALang,
    ws(AName), ws(AType), ADataLen, AData);
  fDetailList.Add(details);
end;

(*----------------------------------------------------------------------*
 | TNTModule.Create                                                     |
 |                                                                      |
 | Constructor for TNTModule                                            |
 *----------------------------------------------------------------------*)
constructor TNTModule.Create;
begin
  inherited Create;
  fDetailList := TObjectList.Create;
end;

(*----------------------------------------------------------------------*
 | TNTModule.Destroy                                                    |
 |                                                                      |
 | Destructor for TNTModule                                             |
 *----------------------------------------------------------------------*)
procedure TNTModule.DeleteResource(resourceNo: integer);
var
  res: TResourceDetails;
begin
  res := ResourceDetails[resourceNo];
  inherited;
  resourceNo := IndexOfResource(Res);
  if resourceNo <> -1 then
    fDetailList.Delete(resourceNo);
end;

destructor TNTModule.Destroy;
begin
  fDetailList.Free;
  inherited;
end;

(*----------------------------------------------------------------------*
 | TNTModule.GetResourceCount                                           |
 |                                                                      |
 | Get method for ResourceCount property                                |
 *----------------------------------------------------------------------*)
function TNTModule.GetResourceCount: integer;
begin
  Result := fDetailList.Count;
end;

(*----------------------------------------------------------------------*
 | TNTModule.GetResourceDetails                                         |
 |                                                                      |
 | Get method for resource details property                             |
 *----------------------------------------------------------------------*)
function TNTModule.GetResourceDetails(idx: integer): TResourceDetails;
begin
  Result := TResourceDetails(fDetailList[idx]);
end;

(*----------------------------------------------------------------------*
 | TNTModule.IndexOfResource                                            |
 |                                                                      |
 | Find the index for specified resource details                        |
 *----------------------------------------------------------------------*)
function TNTModule.IndexOfResource(details: TResourceDetails): integer;
begin
  Result := fDetailList.IndexOf(details);
end;

(*----------------------------------------------------------------------*
 | TNTModule.LoadFromFile                                               |
 |                                                                      |
 | Load all of a module's resources                                     |
 *----------------------------------------------------------------------*)
procedure TNTModule.InsertResource(idx: integer; details: TResourceDetails);
begin
  fDetailList.Insert(idx, details);
end;

procedure TNTModule.LoadFromFile(const FileName: string);
begin
  LoadResources(FileName, nil);
end;

(*----------------------------------------------------------------------*
 | TNTModule.LoadResourceFromModule                                     |
 |                                                                      |
 | Load a particular resource from a resource handle.  Called from      |
 | EnumResLangProc when enumerating resources                           |
 *----------------------------------------------------------------------*)
function TNTModule.LoadResourceFromModule(hModule: integer;
  const resType, resName: PChar; language: word): boolean;
var
  resourceHandle: integer;
  infoHandle, size: integer;
  p:      PChar;
  pt, pn: PWideChar;
  wType, wName: WideString;
begin
  Result := True;
  resourceHandle := Windows.FindResource(hModule, resName, resType);
  if resourceHandle <> 0 then
  begin
    size := SizeOfResource(hModule, resourceHandle);
    infoHandle := LoadResource(hModule, resourceHandle);
    if infoHandle <> 0 then
      try
        p := LockResource(infoHandle);

        if (integer(resType) and $ffff0000) = 0 then
          pt := PWideChar(resType)
        else
        begin
          wType := resType;
          pt    := PWideChar(wType);
        end;

        if (integer(resName) and $ffff0000) = 0 then
          pn := PWideChar(resName)
        else
        begin
          wName := resName;
          pn    := PWideChar(wName);
        end;

        AddResourceToList(pt, pn, size, p, language);
      finally
        FreeResource(infoHandle)
      end
    else
      RaiseLastOSError;
  end
  else
    RaiseLastOSError;
end;

(*----------------------------------------------------------------------*
 | TNTModule.LoadResources                                              |
 |                                                                      |
 | Load resources of a particular type                                  |
 *----------------------------------------------------------------------*)
procedure TNTModule.LoadResources(const fileName: string; tp: PChar);
var
  Instance: THandle;
begin
  Instance := LoadLibraryEx(PChar(fileName), 0, LOAD_LIBRARY_AS_DATAFILE);
  if Instance <> 0 then
    try
      fDetailList.Clear;
      if tp = nil then
        EnumResourceTypes(Instance, @EnumResTypesProc, integer(self))
      else
      begin                           // ... no.  Load specified type...
        // ... but if that's an Icon or Cursor group, load
        // the icons & cursors, too!

        if tp = RT_GROUP_ICON then
          EnumResourceNames(Instance, RT_ICON, @EnumResNamesProc, integer(self))
        else
        if tp = RT_GROUP_CURSOR then
          EnumResourceNames(Instance, RT_CURSOR, @EnumResNamesProc, integer(self));

        EnumResourceNames(Instance, tp, @EnumResNamesProc, integer(self));
      end
    finally
      FreeLibrary(Instance)
    end
  else
    RaiseLastOSError;
end;

(*----------------------------------------------------------------------*
 | TNTModule.SaveToFile                                                 |
 |                                                                      |
 | Update the module's resources.                                       |
 *----------------------------------------------------------------------*)
procedure TNTModule.SaveToFile(const FileName: string);
var
  UpdateHandle: THandle;
  i: integer;
  details: TResourceDetails;
  discard: boolean;
  namest, tpst: string;

  function ResourceNameInt(const Name: string): PChar;
  var
    n: integer;
  begin
    n := ResourceNameToInt(Name);
    if n = -1 then
      Result := PChar(Name)
    else
      Result := PChar(n);
  end;

begin
  if not Assigned(fnUpdateResource) or not Assigned(fnBeginUpdateResource) or
    not Assigned(fnEndUpdateResource) then
    raise Exception.Create(rstCantUpdate);

  discard      := True;
  UpdateHandle := fnBeginUpdateResource(PChar(FileName), True);
  try
    for i := 0 to ResourceCount - 1 do
    begin
      details := ResourceDetails[i];

      namest := details.ResourceName;
      tpst   := details.ResourceType;

      if not fnUpdateResource(UpdateHandle, ResourceNameInt(tpst),
        ResourceNameInt(namest), details.ResourceLanguage,
        details.Data.Memory, details.Data.Size) then
        RaiseLastOSError;
    end;
    ClearDirty;
    discard := False

  finally
    fnEndUpdateResource(UpdateHandle, discard)
  end;
end;

procedure Initialize;
var
  hkernel: THandle;
begin
  hkernel := LoadLibrary('kernel32.dll');
  fnBeginUpdateResource := TfnBeginUpdateResource(
    GetProcAddress(hkernel, 'BeginUpdateResourceA'));
  fnEndUpdateResource := TfnEndUpdateResource(GetProcAddress(hkernel,
    'EndUpdateResourceA'));
  fnUpdateResource := TfnUpdateResource(GetProcAddress(hkernel, 'UpdateResourceA'));
end;

begin
  Initialize
end.
