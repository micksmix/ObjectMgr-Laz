unit  frmFuzz;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LResources, LCLType, Classes, SysUtils, FileUtil, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ExtCtrls, Windows, uioctl;

type

  { TFormFuzz }

  TFormFuzz = class(TForm)
    btnDoAllBruteforce: TButton;
    LabeledEdit1: TLabeledEdit;
    Memo1: TMemo;
    procedure btnDoAllBruteforceClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    deviceName: string;
    fileName: string;
    gotDeviceName: boolean;
    gotFileName: boolean;
    iocode : DWORD;
    lstIoctls: TList; //ioctldef objects
    procedure SmartBruteCheck(hDevice: THandle; dwIOCTLStart, dwIOCTLEnd: DWORD;
      bDeepBruteForce: boolean);
  public
    { public declarations }
    sDevicePath: string;
  end;

var
  FormFuzz: TFormFuzz;

const
  START_IOCTL_VALUE = $0;
  END_IOCTL_VALUE = $fffffff;
  MAX_IOCTLS = 512;
  TOTAL_ERROR_CHECKS = 250000;

implementation

{$R *.lfm}

procedure TFormFuzz.FormShow(Sender: TObject);
begin

  LabeledEdit1.Text := sDevicePath;
end;


//INPUT:
// hDevice - Handle to device object.
// dwIOCTLStart - Where to start IOCTL codes
// dwIOCTLEnd - Where to end IOCTL codes
// bDeepBruteForce - Boolean telling us if we are doing the deep bruteforce or not.

//OUTPUT:
// Always returns true.
{
BOOL Dibf::SmartBruteCheck(HANDLE hDevice, DWORD dwIOCTLStart, DWORD dwIOCTLEnd, BOOL bDeepBruteForce)
{
    DWORD dwIOCTL, lastError, dwIOCTLIndex = 0;
    IoRequest ioRequest(hDevice);  // This unique request gets reused iteratively

    TPRINT(VERBOSITY_INFO, _T("Starting Smart Error Handling\n"))
    for (dwIOCTL=dwIOCTLStart; dwIOCTL< dwIOCTLEnd; dwIOCTL++) {
        if (dwIOCTL-dwIOCTLStart>TOTAL_ERROR_CHECKS) {
            break;
        }
        lastError = 0;
        ioRequest.SetIoCode(dwIOCTL);
        if (ioRequest.testSendForValidRequest(bDeepBruteForce, lastError)) {
            if (++returnMap[lastError] == MAX_IOCTLS) {
                TPRINT(VERBOSITY_INFO, _T("Adding error to banned list: %#.8x\n"), lastError)
                    bannedErrors.resize(dwIOCTLIndex + 1);
                bannedErrors[dwIOCTLIndex++] = lastError;
            }
        }
    }
    TPRINT(VERBOSITY_INFO, _T("Smart error handling complete\n"))
    return TRUE;
}
}

procedure TFormFuzz.SmartBruteCheck(hDevice: THandle; dwIOCTLStart, dwIOCTLEnd: DWORD;
  bDeepBruteForce: boolean);
var
  dwIOCTL, lastError, dwIOCTLIndex: DWORD;
  ioRequest : TIoRequest;
begin
  dwIOCTL := 0;
  lastError := 0;
  dwIOCTLIndex := 0;

  Memo1.Lines.Add('starting smart error handling');
  dwIOCTL := dwIOCTLStart;
  while (dwIOCTL < dwIOCTLEnd) do
  begin
    if (dwIOCTL-dwIOCTLStart>TOTAL_ERROR_CHECKS) then
    begin
      break;
    end;

    lastError := 0;
    iocode := dwIOCTL;
    ioRequest := TIoRequest.Create(hDevice);

    if (ioRequest.testSendForValidRequest(bDeepBruteForce, lastError)) then
    begin

    end;

    {
        if (++returnMap[lastError] == MAX_IOCTLS) {
            TPRINT(VERBOSITY_INFO, _T("Adding error to banned list: %#.8x\n"), lastError)
                bannedErrors.resize(dwIOCTLIndex + 1);
            bannedErrors[dwIOCTLIndex++] = lastError;
        }
    }



    Inc(dwIOCTL);
  end;
end;

procedure TFormFuzz.btnDoAllBruteforceClick(Sender: TObject);
var
  hDevice: THandle;
  bDeep: boolean;
begin
  hDevice := Windows.CreateFile(PChar(sDevicePath),
    MAXIMUM_ALLOWED, FILE_SHARE_READ + FILE_SHARE_WRITE,
    nil, OPEN_EXISTING, FILE_FLAG_OVERLAPPED,
    0);

  if (hDevice <> INVALID_HANDLE_VALUE) then
  begin
    Memo1.Lines.Add('Bruteforcing ioctle codes for: ' + sDevicePath);

    bDeep := False;

    SmartBruteCheck(
      hDevice,
      START_IOCTL_VALUE,
      END_IOCTL_VALUE,
      bDeep);




  end;
  {
      HANDLE hDevice = CreateFile(deviceName, MAXIMUM_ALLOWED, FILE_SHARE_READ|FILE_SHARE_WRITE, NULL, OPEN_EXISTING, FILE_FLAG_OVERLAPPED, NULL);
    if(hDevice!=INVALID_HANDLE_VALUE) {
        //Bruteforce IOCTLs
        TPRINT(VERBOSITY_DEFAULT, _T("<<<< GUESSING IOCTLS %s>>>>\n"), deep?_T("(DEEP MODE)"):_T(""));
        TPRINT(VERBOSITY_INFO, _T("Bruteforcing ioctl codes\n"));
        SmartBruteCheck(hDevice, dwIOCTLStart, dwIOCTLEnd, deep);
        bResult = BruteForceIOCTLs(hDevice, dwIOCTLStart, dwIOCTLEnd, deep);
        if(bResult) {
            TPRINT(VERBOSITY_DEFAULT, _T("---------------------------------------\n\n"));
            TPRINT(VERBOSITY_INFO, _T("Bruteforcing buffer sizes\n"));
            bResult = BruteForceBufferSizes(hDevice);
            if(bResult) {
                TPRINT(VERBOSITY_DEFAULT, _T("---------------------------------------\n\n"));
                WriteBruteforceResult();
            }
        }
        else {
            TPRINT(VERBOSITY_ERROR, _T("Unable to find any valid IOCTLs, exiting...\n"));
            hDevice = INVALID_HANDLE_VALUE;
        }
        CloseHandle(hDevice);
    }
    else {
        TPRINT(VERBOSITY_ERROR, _T("Unable to open device %s, error %#.8x\n"), (LPCTSTR)deviceName, GetLastError());
    }
    return bResult;
    }
end;

procedure TFormFuzz.FormCreate(Sender: TObject);
begin
  lstIoctls := TList.Create;
end;

procedure TFormFuzz.FormDestroy(Sender: TObject);
begin
  FreeAndNil(lstIoctls);
end;

begin

end.
