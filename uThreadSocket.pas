unit uThreadSocket;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uThreadBase, blcksock, synsock, fpjson, jsonparser;

type

  { TThreadSocket }

  TThreadSocket = class(TThreadBase)
  strict private
    FPath: string;
  strict private
    function GetSocket: TSocket;
    function GetRemoteIP: string;
    function GetRemotePort: string;

    procedure SendFileList(const ARequest: TJSONObject);
    procedure SendNewFiles(const ARequest: TJSONObject);
  protected
    procedure Execute; override;
  public
    constructor Create(const ASocket: TSocket); reintroduce;

    property Socket: TSocket read GetSocket;
    property RemoteIP: string read GetRemoteIP;
    property RemotePort: string read GetRemotePort;
  end;

implementation

uses uConst, uFunc;

{ TThreadSocket }

function TThreadSocket.GetSocket: TSocket;
begin
  Result := BlockSocket.Socket;
end;

function TThreadSocket.GetRemoteIP: string;
begin
  Result := BlockSocket.GetRemoteSinIP;
end;

function TThreadSocket.GetRemotePort: string;
begin
  Result := BlockSocket.GetRemoteSinPort.ToString;
end;

procedure TThreadSocket.SendFileList(const ARequest: TJSONObject);
begin
  FPath := ARequest.Strings['path'];
  if not FileExists(GetReferenceFileFullName(FPath)) then
    FatalError := SReferenceFileNotFound
  else begin
    with TJSONObject.Create do
    try
      Integers['status'] := STATUS_SUCCESS;
      Arrays['data']     := GetJSONArrayOfFileList(FPath);
      BlockSocket.SendString(AsJSON + CRLF);
      { TODO: Check error }
    finally
      Free;
    end;
  end;
end;

procedure TThreadSocket.SendNewFiles(const ARequest: TJSONObject);
var
  FilesJSONArray: TJSONArray;
  Stream        : TMemoryStream;
begin
  FilesJSONArray := ARequest.Arrays['data'];
  Stream         := TMemoryStream.Create;
  try
    Stream.Position := 0;
    if FileArrayToZipStream(FPath, FilesJSONArray, Stream) then
    begin
      with TJSONObject.Create do
      try
        Integers['status'] := 200;
        Strings['data']    := Stream.ReadAnsiString;
        BlockSocket.SendString(AsJSON + CRLF);
        { TODO: Check error }
      finally
        Free;
      end;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TThreadSocket.Execute;
var
  RequestString: string;
  RequestJSON  : TJSONObject;
begin
  while IsRunning do
  begin
    if BlockSocket.CanRead(500) then
    begin
      RequestString := BlockSocket.RecvString(TimeOut);
      if BlockSocket.LastError <> 0 then
        Break;

      RequestJSON := TJSONObject(GetJSON(RequestString));
      with RequestJSON do
      try
        case Integers['operation'] of
          OPERATION_GET_LIST: SendFileList(RequestJSON);
          OPERATION_GET_FILE: SendNewFiles(RequestJSON);
          { TODO: Add else option }
        end;
      finally
        RequestJSON.Free;
      end;
    end;
  end;
end;

constructor TThreadSocket.Create(const ASocket: TSocket);
begin
  Inherited Create(False);
  BlockSocket.Socket := ASocket;
end;

end.

