unit uThreadClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, blcksock, synsock, uThreadSocket, fpjson,
  jsonparser, md5;

type
  TStatusChangeEvent = procedure(const ASender: TTCPBlockSocket; const AStatus: Integer) of object;

  { TUserThreadList }
  TUserThreadList = class
  strict private
    FList: TObjectList;
  strict private
    function GetItems(AIndex: Integer): TThreadSocket;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const ASocket: TSocket): Integer;
    function Count: Integer;
    function Find(const ASocket: TSocket): Boolean;

    procedure Delete(const AIndex: Integer);
    procedure StopThreads;

    property Items[AIndex: Integer]: TThreadSocket read GetItems; default;
  end;

  { TServerThread }
  TServerThread = class(TThread)
  strict private
    FBlockSocket   : TTCPBlockSocket;
    FList          : TUserThreadList;
    FIPAddress     : string;
    FPort          : string;
    FTimeOut       : Integer;
    FOnStatusChange: TStatusChangeEvent;
  strict private
    procedure SyncEventMonitor;

    procedure BlockSocketOnStatus(Sender: TObject; Reason: THookSocketReason; const Value: String);
  strict protected
    property FreeOnTerminate;
  protected
    procedure Execute; override;
  public
    constructor Create(const AFreeOnTerminate: Boolean); reintroduce;
    destructor Destroy; override;

    property IPAddress: string read FIPAddress write FIPAddress;
    property Port: string read FPort write FPort;
    property TimeOut: Integer read FTimeOut write FTimeOut;
    property OnStatusChange: TStatusChangeEvent read FOnStatusChange write FOnStatusChange;
  end;

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
        STATUS_ERROR:
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

{ TServerThread }

procedure TServerThread.SyncEventMonitor;
begin
  if Assigned(FOnStatusChange) then
    FOnStatusChange(FBlockSocket, 0);
  Sleep(0);
end;

procedure TServerThread.BlockSocketOnStatus(Sender: TObject; Reason: THookSocketReason; const Value: String);
begin
  case Reason of
    HR_Accept: Synchronize(@SyncEventMonitor);
    HR_Error: { Create event OnError? };
  end;
end;

procedure TServerThread.Execute;
var
  I: Integer;
  Socket: TSocket;
begin
  FBlockSocket.CreateSocket;
  FBlockSocket.Bind(FIPAddress, FPort);
  FBlockSocket.Listen;
  try
    if FBlockSocket.LastError = 0 then
    begin
      while not Terminated do
      begin
        if FBlockSocket.CanRead(1000) then
        begin
          Socket := FBlockSocket.Accept;
          if not FList.Find(Socket) then
          begin
            I := FList.Add(Socket);
            if FBlockSocket.LastError = 0 then
              FList[I].Start
            else
              FList.Delete(I);
          end;
        end;
      end;
    end;
  finally
    FBlockSocket.CloseSocket;
    FBlockSocket.Purge;
  end;
end;

constructor TServerThread.Create(const AFreeOnTerminate: Boolean);
begin
  inherited Create(True);
  FBlockSocket          := TTCPBlockSocket.Create;
  FList                 := TUserThreadList.Create;
  FreeOnTerminate       := AFreeOnTerminate;
  FBlockSocket.OnStatus := @BlockSocketOnStatus;
end;

destructor TServerThread.Destroy;
begin
  FList.Free;
  FBlockSocket.CloseSocket;
  FBlockSocket.Free;
  inherited Destroy;
end;

{ TUserThreadList }

function TUserThreadList.GetItems(AIndex: Integer): TThreadSocket;
begin
  Result := FList.Items[AIndex] as TThreadSocket;
end;

procedure TUserThreadList.StopThreads;
var
  I: Integer;
  Thread: TThreadSocket;
begin
  for I := 0 to FList.Count -1 do
  begin
    Thread := Items[I];
    Thread.Stop;
    Thread.WaitFor;
  end;
end;

constructor TUserThreadList.Create;
begin
  FList := TObjectList.Create(True);
end;

destructor TUserThreadList.Destroy;
begin
  StopThreads;
  FList.Free;
  inherited Destroy;
end;

function TUserThreadList.Add(const ASocket: TSocket): Integer;
begin
  Result := FList.Add(TThreadSocket.Create(ASocket));
end;

function TUserThreadList.Count: Integer;
begin
  Result := FList.Count;
end;

function TUserThreadList.Find(const ASocket: TSocket): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FList.Count -1 do
  begin
    Result := Items[I].Socket = ASocket;
    if Result then
      Break;
  end;
end;

procedure TUserThreadList.Delete(const AIndex: Integer);
begin
  FList.Delete(AIndex);
end;

end.

