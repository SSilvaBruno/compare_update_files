unit uThreadClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, blcksock, synsock, fpjson, jsonparser;

type
  { TUserThread }
  TUserThread = class(TThread)
  strict private
    FBlockSocekt: TTCPBlockSocket;
    FTimeOut    : Integer;
  strict private
    procedure SetSocket(const AValue: TSocket);
    procedure SendFileList;
    procedure SendNewFiles;
    procedure SendOpError;
  strict protected
    property FreeOnTerminate;
  protected
    procedure Execute; override;
  public
    constructor Create(const ASocket: TSocket); reintroduce;
    destructor Destroy; override;

    property Socket: TSocket write SetSocket;
    property TimeOut: Integer read FTimeOut write FTimeOut;
  end;

  { TUserThreadList }
  TUserThreadList = class
  strict private
    FList: TObjectList;
  strict private
    function GetItems(AIndex: Integer): TUserThread;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const ASock: TSocket): Integer;
    function Count: Integer;

    procedure StopThreads;
    property Items[AIndex: Integer]: TUserThread read GetItems; default;
  end;

  { TServerThread }

  TServerThread = class(TThread)
  strict private
    FBlockSocket: TTCPBlockSocket;
    FList       : TUserThreadList;
    FIPAddress  : string;
    FPort       : string;
    FTimeOut    : Integer;
  strict protected
    property FreeOnTerminate;
  protected
    procedure Execute; override;
  public
    constructor Create(const AFreeOnTerminate: Boolean);
    destructor Destroy; override;

    property IPAddress: string read FIPAddress write FIPAddress;
    property Port: string read FPort write FPort;
    property TimeOut: Integer read FTimeOut write FTimeOut;
  end;


implementation

const
  OPERATION_GET_LIST = 1;
  OPERATION_GET_FILE = 2;

{ TServerThread }

procedure TServerThread.Execute;
var
  I: Integer;
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
          I := FList.Add(FBlockSocket.Accept);
          if FBlockSocket.LastError = 0 then
            FList[I].Start
          else
            FList[I]; { Deletar item inserido }
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
  FBlockSocket    := TTCPBlockSocket.Create;
  FList           := TUserThreadList.Create;
  FreeOnTerminate := AFreeOnTerminate;
end;

destructor TServerThread.Destroy;
begin
  FList.Free;
  FBlockSocket.CloseSocket;
  FBlockSocket.Free;
  inherited Destroy;
end;

{ TUserThreadList }

function TUserThreadList.GetItems(AIndex: Integer): TUserThread;
begin
  Result := FList.Items[AIndex] as TUserThread;
end;

procedure TUserThreadList.StopThreads;
var
  I: Integer;
  Thread: TUserThread;
begin
  for I := 0 to FList.Count -1 do
  begin
    Thread := Items[I];
    Thread.Terminate;
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

function TUserThreadList.Add(const ASock: TSocket): Integer;
begin
  Result := FList.Add(TUserThread.Create(ASock));
end;

function TUserThreadList.Count: Integer;
begin
  Result := FList.Count;
end;

{ TUserThread }
procedure TUserThread.SetSocket(const AValue: TSocket);
begin
  FBlockSocekt.Socket := AValue;
end;

procedure TUserThread.SendFileList;
begin

end;

procedure TUserThread.SendNewFiles;
begin

end;

procedure TUserThread.SendOpError;
begin

end;

procedure TUserThread.Execute;
var
  Request: string;
begin
  while not Terminated do
  begin
    Request := FBlockSocekt.RecvString(FTimeOut);
    if FBlockSocekt.LastError <> 0 then
      Break;

    with (GetJSON(Request) as TJSONObject) do
    begin
      case Integers['operation'] of
        OPERATION_GET_LIST: SendFileList;
        OPERATION_GET_FILE: SendNewFiles;
      else
        SendOpError;
      end;
    end;
  end;
end;

constructor TUserThread.Create(const ASocket: TSocket);
begin
  inherited Create(True);
  FBlockSocekt        := TTCPBlockSocket.Create;
  FBlockSocekt.Socket := ASocket;
  FreeOnTerminate     := False;
end;

destructor TUserThread.Destroy;
begin
  FBlockSocekt.CloseSocket;
  FBlockSocekt.Free;
  inherited Destroy;
end;

end.

