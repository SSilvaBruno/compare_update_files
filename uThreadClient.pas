unit uThreadClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock, fpjson,
  uThreadBase;

type

  { TThreadClient }

  TThreadClient = class(TThreadBase)
  strict private
    FFilesToUpdate: TJSONArray;
    FPath         : string;
    FRemoteIP     : string;
    FRemotePort   : string;
  strict private
    procedure CompareFiles(const AFileList: TJSONArray);
    procedure RequestFileList;
    procedure GetFileList;
    procedure RequestFilesToUpdate;
    procedure GetFilesToUpdate;
  protected
    procedure Execute; override;
  public
    constructor Create(const AFreeOnTerminate: Boolean);
    destructor Destroy; override;

    property Path: string read FPath write FPath;
    property RemoteIP: string read FRemoteIP write FRemoteIP;
    property RemotePort: string read FRemotePort write FRemotePort;
  end;


implementation

uses uConst, uFunc;

{ TThreadClient }

procedure TThreadClient.CompareFiles(const AFileList: TJSONArray);
var
  I: Integer;
  FileItem: TJSONObject;
begin
  FFilesToUpdate.Clear;
  for I := 0 to AFileList.Count - 1 do
  begin
    with AFileList.Objects[I] do
    begin
      if not FileHashEquals(FPath, Strings['file'], Strings['hash']) then
      begin
        FileItem := FFilesToUpdate.Objects[FFilesToUpdate.Add];
        FileItem.Strings['file'] := Strings['file'];
      end;
    end;
  end;
end;

procedure TThreadClient.RequestFileList;
begin
  with TJSONObject.Create do
  try
    Integers['operation'] := OPERATION_GET_LIST;
    Strings['path']       := FPath;
    BlockSocket.SendString(AsJSON + CRLF);
    { TODO: Check error }
  finally
    Free;
  end;
end;

procedure TThreadClient.GetFileList;
var
  ResponseString: string;
begin
  ResponseString := BlockSocket.RecvString(TimeOut);
  if BlockSocket.LastError = 0 then
  begin
    with GetJSON(ResponseString) as TJSONObject do
    try
      case Integers['status'] of
        STATUS_SUCCESS: CompareFiles(Arrays['data']);
        STATUS_ERROR: Stop;
      end;
    finally
      Free;
    end;
  end else
    Raise Exception.Create(BlockSocket.LastErrorDesc);
end;

procedure TThreadClient.RequestFilesToUpdate;
begin
  with TJSONObject.Create do
  try
    Integers['operation'] := OPERATION_GET_FILE;
    Arrays['data']        := FFilesToUpdate;
    BlockSocket.SendString(AsJSON + CRLF);
    { TODO verificar erro }
  finally
    Free;
  end;
end;

procedure TThreadClient.GetFilesToUpdate;
var
  ResponseString: string;
  Stream: TMemoryStream;
begin
  ResponseString := BlockSocket.RecvString(TimeOut);
  if BlockSocket.LastError = 0 then
  begin
    with GetJSON(ResponseString) as TJSONObject do
    try
      if Integers['status'] = STATUS_SUCCESS then
      begin
        Stream := TMemoryStream.Create;
        try
          Stream.Position := 0;
          Stream.WriteAnsiString(Strings['data']);
          ZipStreamToFile(FPath, Stream);
        finally
          Stream.Free;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TThreadClient.Execute;
begin
  BlockSocket.Connect(FRemoteIP, FRemotePort);
  if BlockSocket.LastError = 0 then
  begin
    if BlockSocket.CanWrite(100) then
    begin
      RequestFileList;
      GetFileList;
      RequestFilesToUpdate;
      GetFilesToUpdate;
    end;
  end else
    raise Exception.Create(BlockSocket.LastErrorDesc);
end;

constructor TThreadClient.Create(const AFreeOnTerminate: Boolean);
begin
  Inherited Create(AFreeOnTerminate);
  FFilesToUpdate := TJSONArray.Create;
  FPath          := '';
  FRemoteIP      := '';
  FRemotePort    := '';
end;

destructor TThreadClient.Destroy;
begin
  FFilesToUpdate.Free;
  inherited Destroy;
end;

end.

