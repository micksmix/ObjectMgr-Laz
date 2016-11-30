{******************************************************************************}
{                                                                              }
{ ObjectMgr sample program to demonstrate the object namespace                 }
{                                                                              }
{ ObjectMgrHelper.dpr - This is the most important part of the whole project.  }
{                       It cares for enumeration of objects and object         }
{                       directories as well as pipes and mailslots. Internally }
{                       it handles all memory allocation and deallocation and  }
{                       provides the interface to some sort of a tree          }
{                       structure.                                             }
{                                                                              }
{ Copyright (C) 2005 Marcel van Brakel (brakelm)                               }
{ Copyright (C) 2005 Oliver Schneider (assarbad)                               }
{                                                                              }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace them with the notice and other provisions required by the LGPL       }
{ License. If you do not delete the provisions above, a recipient may use      }
{ your version of this file under either the MPL or the LGPL License.          }
{                                                                              }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{                                                                              }
{******************************************************************************}

unit ObjectMgrHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLIntf, LCLType, LMessages,
  JwaNative, JwaWinType, JwaWinNT, JwaNtStatus,
  UnicodeString;

type
  TObjMgrListItem = class
  private
    FName: IUnicodeString;
    FFullPath: IUnicodeString;
    FLinkTarget: IUnicodeString;
    FTypeName: IUnicodeString;
    FParent: TObjMgrListItem;
    FList: TList;
    FIsDirectory: Boolean; // workaround for the object namespace root
    procedure FreeList(var List: TList);
    function OpenAsSymbolicLink(var oa: OBJECT_ATTRIBUTES): Boolean;
    function OpenAsObjectDirectory(var oa: OBJECT_ATTRIBUTES): Boolean;
  public
    constructor Create(ParentDirectory: TObjMgrListItem; Name: PUNICODE_STRING; TypeName: PUNICODE_STRING);
    destructor Destroy; override;
    procedure RefreshList;
    function ObjectName: WideString;
    function FullPath: WideString;
    function ObjectTypeName: WideString;
    function LinkTarget: WideString;
    function IsObjectDirectory: Boolean;
    function IsSymbolicLink: Boolean;
    function IsNamespaceRoot: Boolean;

    property ItemList: TList read FList;
  end;

  TEnumType = (
    etPipes,
    etMailslots
    );

  PMailslotAndPipeItem = ^TMailslotAndPipeItem;
  TMailslotAndPipeItem = record
    ItemName: IUnicodeString;
    Instances,
      MaxInstances: DWORD;
  end;

  TMailslotAndPipeCollection = class
  private
    FList: TList;
    FEnumType: TEnumType;
    procedure FreeList(var list: TList);
  public
    property ItemList: TList read FList;
    constructor Create(enumType: TEnumType);
    destructor Destroy(); override;
    procedure RefreshList();
  end;

//function ListPipes():TMailslotAndPipeCollection;

implementation

const
  MinSize = $1000;

  lpwszBackSlash = '\';

  usBackSlash: UNICODE_STRING =
  (
    Length: sizeof(WideChar);
    MaximumLength: sizeof(WideChar);
    Buffer: lpwszBackSlash;
    );

// Function not declared in Delphi 4!
//function InterlockedExchangePointer(var Target: Pointer; Value: Pointer): Pointer; stdcall; external 'kernel32.dll' name 'InterlockedExchange';

{ TObjMgrListItem }

constructor TObjMgrListItem.Create(ParentDirectory: TObjMgrListItem; Name: PUNICODE_STRING; TypeName: PUNICODE_STRING);
begin
  FParent := ParentDirectory;
  FName := usCreate(Name);
  FTypeName := usCreate(TypeName);

  if Assigned(ParentDirectory) then
  begin
    // Build full path
    FFullPath := usCreate(ParentDirectory.FFullPath);
    if not FFullPath.Equal(usBackSlash) then
      FFullPath.Append(usBackSlash);
    FFullPath.Append(FName);
  end
  else
  begin
    // Full path for the root object is only a backslash
    FFullPath := usCreate(usBackSlash);
    FName.Append(usBackSlash);
    FIsDirectory := True;
  end;

  RefreshList();
end;

destructor TObjMgrListItem.Destroy;
begin
  FName := nil;
  FFullPath := nil;
  FTypeName := nil;
  FLinkTarget := nil;
  FreeList(FList);
  inherited Destroy;
end;

function TObjMgrListItem.ObjectName(): WideString;
begin
  if Assigned(FName) then
    result := FName.AsWideString
  else
    result := '';
end;

function TObjMgrListItem.FullPath(): WideString;
begin
  if Assigned(FFullPath) then
    result := FFullPath.AsWideString
  else
    result := '';
end;

function TObjMgrListItem.ObjectTypeName(): WideString;
begin
  if Assigned(FTypeName) then
    result := FTypeName.AsWideString
  else
    result := '';
end;

function TObjMgrListItem.LinkTarget(): WideString;
begin
  if Assigned(FLinkTarget) then
    result := FLinkTarget.AsWideString
  else
    result := '';
end;

procedure TObjMgrListItem.FreeList(var List: TList);
var
  I: Integer;
begin
  if Assigned(List) then
  begin
    for I := 0 to List.Count - 1 do
      if Assigned(List[I]) then
      begin
        TObjMgrListItem(List[I]).Free;
      end;
    List.Free;
    List := nil;
  end;
end;

function TObjMgrListItem.OpenAsSymbolicLink(var oa: OBJECT_ATTRIBUTES): Boolean;
var
  hObject: HANDLE;
  Status: NTSTATUS;
  newBuffer,
    Buffer: PVOID;
  BufferSize: ULONG;

  // Initializes the MaximumLength and Buffer members
  procedure InitBufferAsUnicodeString(Buffer: PUNICODE_STRING; BufferSize: ULONG);
  begin
    // This function takes a plain contiguous buffer and initializes the first
    // two members with words that represent the first two members of a
    // UNICODE_STRING structure.
    Buffer^.Length := 0;
    Buffer^.MaximumLength := BufferSize - sizeof(UNICODE_STRING);
    // ... the third member is initialized to point at the position of the
    // buffer which is exactly after the first few bytes of the buffer which are
    // of the same size as the UNICODE_STRING structure.
    Buffer^.Buffer := Pointer(DWORD(Buffer) + sizeof(UNICODE_STRING));
  end;

begin
  result := False;
  // Try to open as a symbolic link
  Status := ZwOpenSymbolicLinkObject(@hObject, SYMBOLIC_LINK_QUERY, @oa);
  // If this is not a symbolic link object we won't follow it
  if (NT_SUCCESS(Status)) then
  try
    // Allocate an initial buffer
    BufferSize := MAX_PATH;
    Buffer := RtlAllocateHeap(NtpGetProcessHeap(), HEAP_ZERO_MEMORY, BufferSize);
    if Assigned(Buffer) then
    try
      // Make it appear as a valid UNICODE_STRING structure
      InitBufferAsUnicodeString(Buffer, BufferSize);
      // Query all entries
      Status := ZwQuerySymbolicLinkObject(hObject, Buffer, nil);
      // Check if the buffer was too small ...
      while (Status = STATUS_BUFFER_TOO_SMALL) do
      begin
        // In this case double the size and reallocate
        BufferSize := BufferSize * 2;
        newBuffer := RtlReAllocateHeap(NtpGetProcessHeap(), HEAP_ZERO_MEMORY, Buffer, BufferSize);
        if Assigned(newBuffer) then
        begin
          Buffer := newBuffer;
          // Try once more ...
          InitBufferAsUnicodeString(Buffer, BufferSize);
          Status := ZwQuerySymbolicLinkObject(hObject, Buffer, nil);
        end;
      end;
      // The query was successful, so we proceed
      if NT_SUCCESS(Status) then
      begin
        // Set the FLinkTarget property to contain the name of the link target
        FLinkTarget := usCreate(PUNICODE_STRING(Buffer));
        result := True;
      end;
    finally
      // Free the allocated memory
      if Assigned(Buffer) then
        RtlFreeHeap(NtpGetProcessHeap(), 0, Buffer);
    end;
  finally
    // Close the current "symbolic link" object
    ZwClose(hObject);
  end;
end;

function TObjMgrListItem.OpenAsObjectDirectory(var oa: OBJECT_ATTRIBUTES): Boolean;
var
  hObject: HANDLE;
  Status: NTSTATUS;
  tempList: TList;
  newBuffer,
    Buffer: PVOID;
  BufferSize,
    dwLength,
    dwContext: ULONG;
begin
  result := False;
  // Try to open as an object directory
  Status := ZwOpenDirectoryObject(@hObject, DIRECTORY_TRAVERSE or DIRECTORY_QUERY, @oa);
  // If this is a object directory object we follow it
  if NT_SUCCESS(Status) then
  try
    tempList := TList.Create();
    if (Assigned(tempList)) then
    try
      // First allocate an initial buffer
      BufferSize := MinSize;
      Buffer := RtlAllocateHeap(NtpGetProcessHeap(), HEAP_ZERO_MEMORY, BufferSize);
      if Assigned(Buffer) then
      try
        // Query all entries, starting with the first one ...
        Status := ZwQueryDirectoryObject(hObject, Buffer, BufferSize, True, True, @dwContext, @dwLength);
        // If the buffer was too small, enlarge it
        while (Status = STATUS_BUFFER_TOO_SMALL) do
        begin
          // Reallocate a larger buffer
          BufferSize := BufferSize * 2;
          newBuffer := RtlReAllocateHeap(NtpGetProcessHeap(), HEAP_ZERO_MEMORY, Buffer, BufferSize);
          if Assigned(newBuffer) then
          begin
            Buffer := newBuffer;
            // Try once more ...
            Status := ZwQueryDirectoryObject(hObject, Buffer, BufferSize, True, True, @dwContext, @dwLength);
          end;
        end;
        // Check for success of the "first" call to ZwQueryDirectoryObject
        while (NT_SUCCESS(Status)) do
        begin
          // Get the data from the buffer
          with PDIRECTORY_BASIC_INFORMATION(Buffer)^ do
          begin
            // This basically creates a new list attached to the current item.
            // This allows us to mimic the tree-like structure and implicitly
            // adds a kind of recursion.
            tempList.Add(TObjMgrListItem.Create(Self, @ObjectName, @ObjectTypeName));
          end;
          // Proceed with the next entry ...
          Status := ZwQueryDirectoryObject(hObject, Buffer, BufferSize, True, False, @dwContext, @dwLength);
        end;
        // Was the query successful?
        result := NT_SUCCESS(Status) or (Status = STATUS_NO_MORE_ENTRIES);
      finally
        // Free the allocated memory
        if Assigned(Buffer) then
          RtlFreeHeap(NtpGetProcessHeap(), 0, Buffer);
      end;
    finally
      // Close the current "object directory" object
      ZwClose(hObject);
    end;
  finally
    // Finally assign the new list to our object and ...
    // This is a threadsafe approach
    tempList := TList(System.InterlockedExchange(
             Pointer(FList),
             tempList));
    // ... free the old one
    FreeList(tempList);
  end;
end;

procedure TObjMgrListItem.RefreshList();
var
  oa: OBJECT_ATTRIBUTES;
begin
  // Fill the OBJECT_ATTRIBUTES structure
  InitializeObjectAttributes(@oa, FFullPath.AsPUnicodeString, OBJ_CASE_INSENSITIVE, 0, nil);
  // Try to open as symbolic link first
  if OpenAsSymbolicLink(oa) then
    Exit;
  // If we are here we try to open the object as directory object ...
  if OpenAsObjectDirectory(oa) then
    Exit;
end;

function TObjMgrListItem.IsObjectDirectory(): Boolean;
begin
  // Valid results only after a call to RefreshList()
  result := Assigned(FList) or FIsDirectory;
end;

function TObjMgrListItem.IsSymbolicLink(): Boolean;
begin
  result := Assigned(FLinkTarget);
end;

function TObjMgrListItem.IsNamespaceRoot(): Boolean;
begin
  result := FFullPath.Equal(usBackSlash);
end;

{ TMailslotAndPipeCollection }

constructor TMailslotAndPipeCollection.Create(enumType: TEnumType);
begin
  FList := TList.Create();
  FEnumType := enumType;
  RefreshList();
end;

destructor TMailslotAndPipeCollection.Destroy();
begin
  FreeList(FList);
  inherited;
end;

procedure TMailslotAndPipeCollection.FreeList(var list: TList);
var
  i: Integer;
  CurrItem: PMailslotAndPipeItem;
begin
  if Assigned(list) then
  try
    list.Pack();
    for i := 0 to list.Count - 1 do
    begin
      CurrItem := list[i];
      // Free IUnicodeString
      CurrItem^.ItemName := nil;
      // Dispose the rest
      Dispose(CurrItem);
      list[i] := nil;
    end;
  finally
    list.Free();
  end;
end;

procedure TMailslotAndPipeCollection.RefreshList();
var
  us: IUnicodeString;
  oa: OBJECT_ATTRIBUTES;
  iostat: IO_STATUS_BLOCK;
  hObject: THandle;
  Status: NTSTATUS;
  Buffer: Pointer;
  BufferSize: DWORD;
  CurrItem: PMailslotAndPipeItem;
  tempList: TList;
begin
  tempList := nil;
  case FEnumType of
    etPipes:
      us := usCreate(PChar('\??\Pipe\'));
    etMailslots:
      us := usCreate(PChar('\??\Mailslot\'));
  else
    us := nil;
  end;
  if Assigned(us) then
  try
    InitializeObjectAttributes(@oa, us.AsPUnicodeString, OBJ_CASE_INSENSITIVE, 0, nil);
    // Open pipe symlink
    Status := ZwCreateFile(@hObject,
      GENERIC_READ or SYNCHRONIZE or FILE_READ_ATTRIBUTES,
      @oa,
      @iostat,
      nil, 0,
      FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
      FILE_OPEN,
      FILE_SYNCHRONOUS_IO_NONALERT or FILE_NON_DIRECTORY_FILE,
      nil, 0);
    if NT_SUCCESS(Status) then
    try
      BufferSize := MinSize;
      // Allocate some memory
      Buffer := RtlAllocateHeap(NtpGetProcessHeap(), HEAP_ZERO_MEMORY, BufferSize);
      if Assigned(Buffer) then
      try
        RtlZeroMemory(@iostat, sizeof(iostat));
        RtlZeroMemory(Buffer, BufferSize);
        // Query a single entry from the "directory", from the start
        Status := ZwQueryDirectoryFile(hObject, 0, nil, nil, @iostat, Buffer, BufferSize, FileDirectoryInformation, True, nil, True);
        while (NT_SUCCESS(Status)) do
        begin
          if not Assigned(tempList) then
          begin
            tempList := TList.Create();
          end;
          New(CurrItem);
          if Assigned(CurrItem) then
          begin
            CurrItem^.ItemName := usCreate(PWideChar(@PFILE_DIRECTORY_INFORMATION(Buffer)^.FileName));
            CurrItem^.Instances := PFILE_DIRECTORY_INFORMATION(Buffer)^.EndOfFile.LowPart;
            CurrItem^.MaxInstances := PFILE_DIRECTORY_INFORMATION(Buffer)^.AllocationSize.LowPart;
            tempList.Add(CurrItem);
          end;

          // Query a single entry from the "directory", from the last position
          RtlZeroMemory(Buffer, BufferSize);
          Status := ZwQueryDirectoryFile(hObject, 0, nil, nil, @iostat, Buffer, BufferSize, FileDirectoryInformation, True, nil, False);
        end;
      finally
        if Assigned(Buffer) then
          RtlFreeHeap(NtpGetProcessHeap(), 0, Buffer);
      end;
    finally
      ZwClose(hObject);
    end;
  finally
    us := nil;
  end;
  // Finally assign the new list to our object and ...
  // This is a threadsafe approach
  if Assigned(tempList) then
    tempList := TList(System.InterlockedExchange(Pointer(FList), tempList));
  // ... free the old one
  FreeList(tempList);
end;

end.

