unit uThreadClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock, fpjson, md5;

type
  TStatusChangeEvent = procedure(const ASender: TTCPBlockSocket; const AStatus: Integer) of object;

  { TClientThread }
  TClientThread = class(TThread)
  strict private
    FBlockSocket  : TTCPBlockSocket;
    FIPAddress    : string;
    FPort         : string;
    FTimeOut      : Integer;
    FPath         : string;
    FFilesToUpdate: TJSONArray;
  strict private
    function HashFileEquals(const AFile, AHash: string): Boolean;

    procedure CompareFiles(const AFileList: TJSONArray);
    procedure RequestFileList;
    procedure RequestFilesToUpdate;
    procedure GetFileList;
    procedure GetFilesToUpdate;
  protected
    procedure Execute; override;
  public
    constructor Create(const AFreeOnTerminate: Boolean); reintroduce;
    destructor Destroy; override;

    property IPAddress: string read FIPAddress write FIPAddress;
    property Port: string read FPort write FPort;
    property TimeOut: Integer read FTimeOut write FTimeOut;
    property Path: string read FPath write FPath;
  end;


implementation

const
  STATUS_SUCCESS      = 200;
  STATUS_ERROR        = 400;
  OPERATION_GET_LIST  = 1;
  OPERATION_GET_FILE  = 2;

{ TClientThread }

function TClientThread.HashFileEquals(const AFile, AHash: string): Boolean;
var
  LocalFile: string;
begin
  LocalFile := IncludeTrailingPathDelimiter(FPath) + AFile;
  Result    := FileExists(LocalFile) and (AHash = MD5Print(MD5File(LocalFile)));
end;

procedure TClientThread.RequestFileList;
var
  RequestJSON: TJSONObject;
begin
  RequestJSON := TJSONObject.Create;
  try
    RequestJSON.Integers['operation'] := OPERATION_GET_LIST;
    RequestJSON.Strings['path']       := FPath;
    FBlockSocket.SendString(RequestJSON.AsJSON + CRLF);
    { TODO: Check error }
  finally
    RequestJSON.Free;
  end;
end;

procedure TClientThread.GetFileList;
var
  ResponseString: String;
  ResponseJSON: TJSONObject;
begin
  ResponseString := FBlockSocket.RecvString(FTimeOut);
  if FBlockSocket.LastError = 0 then
  begin
    ResponseJSON := GetJSON(ResponseString) as TJSONObject;
    try
      case ResponseJSON.Integers['status'] of
        STATUS_SUCCESS: CompareFiles(ResponseJSON.Arrays['data']);
        STATUS_ERROR: Terminate;
      end;
    finally
      ResponseJSON.Free;
    end;
  end else
    Raise Exception.Create(FBlockSocket.LastErrorDesc);
end;

procedure TClientThread.GetFilesToUpdate;
begin

end;

procedure TClientThread.CompareFiles(const AFileList: TJSONArray);
var
  I: Integer;
begin
  FFilesToUpdate.Clear;
  for I:= 0 to AFileList.Count -1 do
  begin
    with AFileList.Objects[I] do
    begin
      if not HashFileEquals(Strings['file'], Strings['hash']) then
        FFilesToUpdate.Objects[FFilesToUpdate.Add] := TJSONObject(GetJSON(Format('{"file": "%s"}', [Strings['file']])));
    end;
  end;
end;

procedure TClientThread.RequestFilesToUpdate;
var
  RequestJSON: TJSONObject;
begin
  RequestJSON := TJSONObject.Create;
  try
    RequestJSON.Integers['operation'] := OPERATION_GET_FILE;
    RequestJSON.Arrays['data']        := FFilesToUpdate;
    FBlockSocket.SendString(RequestJSON.AsJSON + CRLF);
    { TODO verificar erro }
  finally
    RequestJSON.Free;
  end;
end;

procedure TClientThread.Execute;
begin
  FBlockSocket.Connect(FIPAddress, FPort);
  if FBlockSocket.LastError = 0 then
  begin
    if FBlockSocket.CanWrite(1000) then
    begin
      RequestFileList;
      GetFileList;
      RequestFilesToUpdate;
    end;
  end;
end;

constructor TClientThread.Create(const AFreeOnTerminate: Boolean);
begin
  inherited Create(True);
  FBlockSocket    := TTCPBlockSocket.Create;
  FFilesToUpdate  := TJSONArray.Create;
  FreeOnTerminate := AFreeOnTerminate;
end;

destructor TClientThread.Destroy;
begin
  FBlockSocket.CloseSocket;
  /// FFilesToUpdate.Free;
  FBlockSocket.Free;
  inherited Destroy;
end;

end.

