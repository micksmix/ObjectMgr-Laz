{******************************************************************************}
{                                                                              }
{ ObjectMgr sample program to demonstrate the object namespace                 }
{                                                                              }
{ UnicodeString.dpr   - This object wraps around the UNICODE_STRING type to    }
{                       provide for easier handling. To make it appear as a    }
{                       stack object it has been implemented as an interface.  }
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

unit UnicodeString;

{$MODE Delphi}

interface
uses
  JwaWinType;

type
  IUnicodeString = interface
    function GetOk: Boolean;
    procedure SetOk(Value: Boolean);

    function AsAnsiString: AnsiString;
    function AsWideString: WideString;
    function AsPWideChar: PWideChar;
    function AsPUnicodeString: PUnicodeString;
    function GetMaximumLength: Word;
    function GetLength: Word;
    function Copy(s: IUnicodeString): IUnicodeString;
    procedure Erase;
    procedure Free;
    procedure DownCase;
    procedure UpperCase;
    function Hexstr2Int: ULONG;
    function Decstr2Int: ULONG;
    function Octstr2Int: ULONG;
    function Binstr2Int: ULONG;
    function IsDosDeviceName:Boolean;

    function Append(const s: TUnicodeString): Boolean; overload;
    function Append(s: PUnicodeString): Boolean; overload;
    function Append(s: IUnicodeString): Boolean; overload;
    function Append(s: PWideChar): Boolean; overload;

    function Equal(const s: TUnicodeString; CaseInsensitive: Boolean = False): Boolean; overload;
    function Equal(s: PWideChar; CaseInsensitive:Boolean = False): Boolean; overload;
    function Equal(s: PUnicodeString; CaseInsensitive: Boolean = False): Boolean; overload;
    function Equal(s: IUnicodeString; CaseInsensitive: Boolean = False): Boolean; overload;

    function Compare(const s: TUnicodeString; CaseInsensitive:Boolean = False): Longint; overload;
    function Compare(s: PWideChar; CaseInsensitive: Boolean = False): Longint; overload;
    function Compare(s: PUnicodeString; CaseInsensitive: Boolean = False): Longint; overload;
    function Compare(s: IUnicodeString; CaseInsensitive: Boolean = False): Longint; overload;

    property Ok: Boolean read GetOk write SetOk;
  end;

function usCreate(s: PAnsiChar): IUnicodeString; overload;
function usCreate(s: PWideChar): IUnicodeString; overload;
function usCreate(const s: TUnicodeString): IUnicodeString; overload;
function usCreate(s: PUnicodeString): IUnicodeString; overload;
function usCreate(s: IUnicodeString): IUnicodeString; overload;

function usCreateEmpty(CharacterLength: Word = 0): IUnicodeString;

function usCreateNtFromDosPath(s: PAnsiChar): IUnicodeString; overload;
function usCreateNtFromDosPath(s: PWideChar): IUnicodeString; overload;

function usCreateFromSid(sid: Pointer (*PSID*)): IUnicodeString;

function usCreateCurrentDirectory: IUnicodeString;

implementation

uses
  JwaNative;

type
  TUnicodeStringClass = class(TInterfacedObject, IUnicodeString)
  private
    FString: TUnicodeString;
    FOk: Boolean;
    function FreeBuffer: Boolean;
  public
    constructor CreateFromPAnsiChar(s: PAnsiChar);
    constructor CreateFromPWideChar(s: PWideChar);
    constructor CreateFromUnicodeString(const s: TUnicodeString); overload;
    constructor CreateFromUnicodeString(s: PUnicodeString); overload;
    constructor CreateFromUnicodeString(s: IUnicodeString); overload;
    constructor CreateNtFromDosPathA(dospath: PAnsiChar);
    constructor CreateNtFromDosPathW(dospath: PWideChar);
    destructor Destroy; override;

    function GetOk: Boolean;
    procedure SetOk(Value: Boolean);

    function AsAnsiString: AnsiString;
    function AsWideString: WideString;
    function AsPWideChar: PWideChar;
    function AsPUnicodeString: PUnicodeString;
    function GetMaximumLength: Word;
    function GetLength: Word;

    function Copy(S: IUnicodeString): IUnicodeString;
    procedure Erase;
    procedure Free; // mvb todo
    procedure DownCase;
    procedure UpperCase;

    function Hexstr2Int: ULONG;
    function Decstr2Int: ULONG;
    function Octstr2Int: ULONG;
    function Binstr2Int: ULONG;
    function IsDosDeviceName: Boolean;

    function Append(const s: TUnicodeString): Boolean; overload;
    function Append(s: PUnicodeString): Boolean; overload;
    function Append(s: IUnicodeString): Boolean; overload;
    function Append(s: PWideChar): Boolean; overload;

    function Equal(const s: TUnicodeString; CaseInsensitive: Boolean = False):Boolean; overload;
    function Equal(s: PWideChar; CaseInsensitive: Boolean = False): Boolean; overload;
    function Equal(s: PUnicodeString; CaseInsensitive: Boolean = False): Boolean; overload;
    function Equal(s: IUnicodeString; CaseInsensitive: Boolean = False): Boolean; overload;

    function Compare(const s: TUnicodeString; CaseInsensitive: Boolean = False): Longint; overload;
    function Compare(s: PWideChar; CaseInsensitive: Boolean = False): Longint; overload;
    function Compare(s: PUnicodeString; CaseInsensitive: Boolean = False): Longint; overload;
    function Compare(s: IUnicodeString; CaseInsensitive: Boolean = False): Longint; overload;
  end;

(***
 * Creates the string from the passed-in PAnsiChar/AnsiString.
 ***)
constructor TUnicodeStringClass.CreateFromPAnsiChar(s: PAnsiChar);
begin
  FreeBuffer;
  if not Assigned(s) then
  begin
    FOk := True;
    Exit;
  end;
  if not RtlCreateUnicodeStringFromAsciiz(@FString, s) then
    FreeBuffer
  else
  begin
    FOk := True;
  end;
end;

(***
 * Creates the string from the passed-in PWideChar/WideString.
 ***)
constructor TUnicodeStringClass.CreateFromPWideChar(s: PWideChar);
begin
  FreeBuffer;
  if not Assigned(s) then
  begin
    FOk := True;
    Exit;
  end;
  if not (RtlCreateUnicodeString(@FString, s)) then
    FreeBuffer()
  else
    FOk := True;
end;

(***
 * Creates an NT compatible path from the DOS-path which was passed as parameter.
 ***)
constructor TUnicodeStringClass.CreateNtFromDosPathA(DosPath: PAnsiChar);
var
  Temp: TUnicodeString;
begin
  FreeBuffer();
  if not Assigned(DosPath) then
  begin
    FOk := True;
    Exit;
  end;
  if RtlCreateUnicodeStringFromAsciiz(@Temp, DosPath) then
  try
    if not RtlDosPathNameToNtPathName_U(Temp.Buffer, FString, nil, nil) then
      FreeBuffer
    else
      FOk := True;
  finally
    RtlFreeHeap(NtpGetProcessHeap, 0, Temp.Buffer);
  end;
end;

(***
 * Creates an NT compatible path from the DOS-path which was passed as parameter.
 ***)
constructor TUnicodeStringClass.CreateNtFromDosPathW(DosPath: PWideChar);
begin
  FreeBuffer;
  if not Assigned(DosPath) then
  begin
    FOk := True;
    Exit;
  end;
  if not RtlDosPathNameToNtPathName_U(DosPath, FString, nil, nil) then
    FreeBuffer
  else
    FOk := True;
end;

(***
 * Destroys the object.
 ***)
destructor TUnicodeStringClass.Destroy;
begin
  FreeBuffer();
  inherited Destroy;
end;

(***
 * This private method checks whether the buffer of the internal UNICODE_STRING
 * is assigned and if so frees it. Furthermore all the UNICODE_STRING's members
 * are being reset to nil/0 respectively.
 ***)
function TUnicodeStringClass.FreeBuffer: Boolean;
begin
  FOk := False;
  Result := False;
  if Assigned(FString.Buffer) then Result := RtlFreeHeap(NtpGetProcessHeap, 0, FString.Buffer);
  if Result then
    with FString do
    begin
      Buffer := nil;
      Length := 0;
      MaximumLength := 0;
    end;
end;

(***
 * This is to check whether construction was successful.
 ***)

function TUnicodeStringClass.GetOk: Boolean;
begin
  Result := FOk;
end;

(***
 * Performs a deep copy to provide a real different WideString. Note, that
 * this does not rely on #0 as termination character to determine the string
 * length. Instead it copies the maximum available characters from the
 * UNICODE_STRING and then truncates to the actual length.
 ***)
function TUnicodeStringClass.AsWideString: WideString;
var
  Temp: WideString;
begin
  // Perform a deep copy!
  SetLength(Temp, FString.MaximumLength div SizeOf(WideChar));
  RtlCopyMemory(@Temp[1], FString.Buffer, FString.MaximumLength);
  SetLength(Temp, FString.Length div SizeOf(WideChar));
  Result := Temp;
end;

function TUnicodeStringClass.AsPWideChar: PWideChar;
begin
  Result := FString.Buffer;
end;

function TUnicodeStringClass.AsAnsiString: AnsiString;
begin
  Result := AsWideString;
end;

(***
 * Dangerous or not? Returning a pointer to an object's internal member.
 ***)
function TUnicodeStringClass.AsPUnicodeString: PUnicodeString;
begin
  Result := @FString;
end;

(***
 * Create a deep copy of the passed in UNICODE_STRING
 ***)
constructor TUnicodeStringClass.CreateFromUnicodeString(const s: TUnicodeString);
begin
  FreeBuffer;
  if not Assigned(@s) then
  begin
    FOk := True;
    Exit;
  end;
  FString.Length := s.Length;
  FString.MaximumLength := s.MaximumLength;
  FString.Buffer := nil;
  if s.MaximumLength > 0 then
    FString.Buffer := RtlAllocateHeap(NtpGetProcessHeap, HEAP_ZERO_MEMORY, s.MaximumLength);
  if Assigned(FString.Buffer) then
  begin
    RtlCopyMemory(FString.Buffer, s.Buffer, s.MaximumLength);
    FOk := True;
  end;
end;

(***
 * Create a deep copy of the passed in PUNICODE_STRING
 ***)
constructor TUnicodeStringClass.CreateFromUnicodeString(s: PUnicodeString);
begin
  CreateFromUnicodeString(s^);
end;

(***
 * Create a deep copy of the passed in TUnicodeStringClass.
 * A copy constructor, mo1re or less.
 ***)
constructor TUnicodeStringClass.CreateFromUnicodeString(s: IUnicodeString);
begin
  if not Assigned(s) then
  begin
    FOk := True;
    Exit;
  end;
  // Clone the other string object's contents
  CreateFromUnicodeString(s.AsPUnicodeString);
end;

(***
 * Create a deep copy of the passed in TUnicodeStringClass.
 ***)
function TUnicodeStringClass.Copy(s: IUnicodeString): IUnicodeString;
begin
  Result := nil;
  if not Assigned(s) then
  begin
    FOk := True;
    Exit;
  end;
  Result := TUnicodeStringClass.CreateFromUnicodeString(s);
end;

function TUnicodeStringClass.Append(const s: TUnicodeString): Boolean;
var
  Temp: PWideChar;
  NewLength : DWORD;
begin
  Result := False;
  if Assigned(@s) then
  begin
    NewLength := (Self.FString.Length + s.Length + sizeof(WideChar)) div sizeof(WideChar);
    Temp := RtlReAllocateHeap(NtpGetProcessHeap, 0, Self.FString.Buffer, NewLength * SizeOf(WideChar));
    if Assigned(Temp) then
    begin
      FString.Buffer := Temp;
      FString.MaximumLength := NewLength * SizeOf(WideChar);
      RtlAppendUnicodeStringToString(@FString, @s);
      Result := True;
    end;
  end;
end;

function TUnicodeStringClass.Append(s: PUnicodeString):Boolean;
begin
  Result := Append(s^);
end;

function TUnicodeStringClass.Append(s: IUnicodeString):Boolean;
begin
  Result := Append(s.AsPUnicodeString);
end;

function TUnicodeStringClass.Append(s: PWideChar): Boolean;
var
  Temp: PWideChar;
  NewLength: DWORD;
begin
  Result := False;
  if Assigned(s) then
  begin
    NewLength := (FString.Length + Length(s) * SizeOf(WideChar) + SizeOf(WideChar)) div SizeOf(WideChar);
    Temp := RtlReAllocateHeap(NtpGetProcessHeap, 0, FString.Buffer, NewLength * SizeOf(WideChar));
    if Assigned(Temp) then
    begin
      FString.Buffer := Temp;
      FString.MaximumLength := NewLength * SizeOf(WideChar);
      RtlAppendUnicodeToString(@FString, s);
      Result := True;
    end;
  end;
end;

function TUnicodeStringClass.GetMaximumLength: Word;
begin
  Result := FString.MaximumLength;
end;

function TUnicodeStringClass.GetLength: Word;
begin
  Result := FString.Length;
end;

procedure TUnicodeStringClass.Erase;
begin
  RtlEraseUnicodeString(@FString);
end;

procedure TUnicodeStringClass.Free;
begin
  FreeBuffer();
end;

function TUnicodeStringClass.Equal(const s: TUnicodeString; CaseInsensitive:Boolean): Boolean;
begin
  Result := RtlEqualUnicodeString(@FString, @s, CaseInsensitive);
end;

function TUnicodeStringClass.Equal(s: PUnicodeString; CaseInsensitive: Boolean): Boolean;
begin
  Assert(Assigned(s));
  Result := Equal(s^, CaseInsensitive);
end;

function TUnicodeStringClass.Equal(s: IUnicodeString; CaseInsensitive: Boolean): Boolean;
begin
  Result := Equal(s.AsPUnicodeString, CaseInsensitive);
end;

function TUnicodeStringClass.Equal(s: PWideChar; CaseInsensitive: Boolean): Boolean;
var
  Temp: TUnicodeString;
begin
  RtlInitUnicodeString(@Temp, s);
  Result := Equal(Temp, CaseInsensitive);
end;

procedure TUnicodeStringClass.DownCase;
begin
  RtlDowncaseUnicodeString(@FString, @FString, False);
end;

procedure TUnicodeStringClass.UpperCase;
begin
  RtlUpcaseUnicodeString(@FString, @FString, False);
end;

function TUnicodeStringClass.Hexstr2Int: ULONG;
begin
  RtlUnicodeStringToInteger(@FString, 16, @Result);
end;

function TUnicodeStringClass.Decstr2Int(): ULONG;
begin
  RtlUnicodeStringToInteger(@FString, 10, @Result);
end;

function TUnicodeStringClass.Octstr2Int(): ULONG;
begin
  RtlUnicodeStringToInteger(@FString, 8, @Result);
end;

function TUnicodeStringClass.Binstr2Int(): ULONG;
begin
  RtlUnicodeStringToInteger(@FString, 2, @Result);
end;

function TUnicodeStringClass.Compare(const s: TUnicodeString; CaseInsensitive: Boolean): Longint;
begin
  Result := RtlCompareUnicodeString(@FString, @s, CaseInsensitive);
end;

function TUnicodeStringClass.Compare(s: PUnicodeString; CaseInsensitive:Boolean): Longint;
begin
  Result := Compare(s^, CaseInsensitive);
end;

function TUnicodeStringClass.Compare(s: IUnicodeString; CaseInsensitive: Boolean): Longint;
begin
  Result := Compare(s.AsPUnicodeString, CaseInsensitive);
end;

function TUnicodeStringClass.Compare(s: PWideChar; CaseInsensitive:Boolean): Longint;
var
  Temp: TUnicodeString;
begin
  RtlInitUnicodeString(@Temp, s);
  Result := Compare(Temp, CaseInsensitive);
end;

function TUnicodeStringClass.IsDosDeviceName: Boolean;
begin
  Result := RtlIsDosDeviceName_U(FString.Buffer) <> 0;
end;

procedure TUnicodeStringClass.SetOk(Value: Boolean);
begin
  FOk := Value;
end;

(******************************************************************************
  Exported functions
 ******************************************************************************)

function usCreate(s: PAnsiChar): IUnicodeString; overload;
begin
  Result := TUnicodeStringClass.CreateFromPAnsiChar(s);
end;

function usCreate(s:PWideChar): IUnicodeString; overload;
begin
  Result := TUnicodeStringClass.CreateFromPWideChar(s);
end;

function usCreate(const s: TUnicodeString): IUnicodeString; overload;
begin
  Result := TUnicodeStringClass.CreateFromUnicodeString(s);
end;

function usCreate(s: PUnicodeString): IUnicodeString; overload;
begin
  Result := TUnicodeStringClass.CreateFromUnicodeString(s);
end;

function usCreate(s: IUnicodeString): IUnicodeString; overload;
begin
  Result := TUnicodeStringClass.CreateFromUnicodeString(s);
end;

function usCreateEmpty(CharacterLength:Word = 0): IUnicodeString;
var
  Obj: IUnicodeString;
begin
  Obj := TUnicodeStringClass.CreateFromPWideChar(nil);
  Obj.AsPUnicodeString^.MaximumLength := CharacterLength * SizeOf(WideChar);
  Obj.AsPUnicodeString^.Length := 0;
  Obj.AsPUnicodeString^.Buffer := RtlAllocateHeap(NtpGetProcessHeap, HEAP_ZERO_MEMORY, Obj.AsPUnicodeString^.MaximumLength);
  Obj.SetOk(True);
  Result := Obj;
  Obj := nil;
end;

function usCreateNtFromDosPath(s: PAnsiChar): IUnicodeString; overload;
begin
  Result := TUnicodeStringClass.CreateNtFromDosPathA(s);
end;

function usCreateNtFromDosPath(s: PWideChar): IUnicodeString; overload;
begin
  Result := TUnicodeStringClass.CreateNtFromDosPathW(s);
end;

function usCreateFromSid(sid: Pointer (*PSID*)): IUnicodeString;
var
  Obj: IUnicodeString;
begin
  Obj := TUnicodeStringClass.CreateFromPWideChar(nil);
  RtlConvertSidToUnicodeString(Obj.AsPUnicodeString, sid, True);
  Result := Obj;
  Obj := nil;
end;

function usCreateCurrentDirectory: IUnicodeString;
var
  Obj: IUnicodeString;
  RequiredSize: DWORD;
begin
  Result := nil;
  RequiredSize := RtlGetCurrentDirectory_U(0, nil);
  if RequiredSize > 0 then
  begin
    Obj := TUnicodeStringClass.CreateFromPWideChar(nil);
    Obj.AsPUnicodeString^.MaximumLength := RequiredSize;
    Obj.AsPUnicodeString^.Length := 0;
    Obj.AsPUnicodeString^.Buffer := RtlAllocateHeap(NtpGetProcessHeap, HEAP_ZERO_MEMORY, RequiredSize);
    if RtlGetCurrentDirectory_U(RequiredSize, Obj.AsPUnicodeString^.Buffer) <= RequiredSize then
    begin
      RtlInitUnicodeString(Obj.AsPUnicodeString, Obj.AsPUnicodeString^.Buffer);
      Obj.SetOk(True);
      Result := Obj;
    end;
    Obj := nil;
  end;
end;

end.
