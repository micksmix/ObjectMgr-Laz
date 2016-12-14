unit uIoctl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows;

type
  TIoctlDef = record
    dwIOCTL: DWORD;
    dwLowerSize: DWORD;
    dwUpperSize: DWORD;
  end;

  type

    { TIoRequest }

    TIoRequest = class(TObject)
      constructor Create(hDevice : THandle);
      public
         overlp : OVERLAPPED;
         function GetIoCode() : DWORD;
         procedure SetIoCode(iocode: DWORD); //{this->iocode=iocode;}
         function testSendForValidRequest(deep : Boolean;var lastError : DWORD) : boolean;
         function testSendForValidBufferSize(testSize: DWORD) : boolean;
         procedure reset();
         function sendSync() : boolean;
         function sendAsync() : DWORD;
         //BOOL fuzz(FuzzingProvider*, mt19937*);
      private
        // Static arrays of known interesting errors
        invalidIoctlErrorCodes : array of DWORD;
        invalidBufSizeErrorCodes : array of DWORD;
        // Members
        hDev : THandle;
        iocode : DWORD;
        inBuf : array of Byte;   //uchar == Byte
        outBuf : array of Byte;
        // Functions
        function allocBuffers(inSize, outSize : DWORD) : boolean;
        function sendRequest(async : boolean; var lastError : DWORD) : boolean;
        function getInputBufferLength() : DWORD; //{return inBuf.size()*sizeof(UCHAR);}
        function getOutputBufferLength() : DWORD; //{return outBuf.size()*sizeof(UCHAR);}
    end;

 {
        // Statics initialization
const DWORD IoRequest::invalidIoctlErrorCodes[] = {
    ERROR_INVALID_FUNCTION,
    ERROR_NOT_SUPPORTED,
    ERROR_INVALID_PARAMETER,
    ERROR_NO_SYSTEM_RESOURCES
};
const DWORD IoRequest::invalidBufSizeErrorCodes[] = {
    ERROR_INSUFFICIENT_BUFFER,
    ERROR_BAD_LENGTH,
};
  }
implementation

constructor TIoRequest.Create(hDevice : THandle);
begin
   //MyStrings := TStringList.Create;
end;

function TIoRequest.GetIoCode: DWORD;
begin
  //
end;

procedure TIoRequest.SetIoCode(iocode: DWORD);
begin
  //
end;

function TIoRequest.testSendForValidRequest(deep: Boolean; var lastError: DWORD
  ): boolean;
begin
 //
end;

function TIoRequest.testSendForValidBufferSize(testSize: DWORD): boolean;
begin
  //
end;

procedure TIoRequest.reset;
begin
  //
end;

function TIoRequest.sendSync: boolean;
begin
  //
end;

function TIoRequest.sendAsync: DWORD;
begin
  //
end;

function TIoRequest.allocBuffers(inSize, outSize: DWORD): boolean;
begin
  //
end;

function TIoRequest.sendRequest(async: boolean; var lastError: DWORD): boolean;
begin
  //
end;

function TIoRequest.getInputBufferLength: DWORD;
begin
  //
end;

function TIoRequest.getOutputBufferLength: DWORD;
begin
  //
end;

end.

