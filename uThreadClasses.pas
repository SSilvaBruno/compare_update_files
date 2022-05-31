unit uThreadClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock, fpjson, jsonparser;

type

  { TUserThread }

  TUserThread = class(TThread)
  strict private
    FBlockSocekt: TTCPBlockSocket;
    FStop: Boolean;
    FTimeOut: Integer;
  strict private
    procedure SetSocket(const AValue: TSocket);
    procedure SendFileList;
    procedure SendNewFiles;
  protected
    procedure Execute; override;
  public
    constructor Create(const ASocket: TSocket); reintroduce;
    destructor Destroy; override;

    property Socket: TSocket write SetSocket;
    property Stop: Boolean read FStop write FStop;
    property TimeOut: Integer read FTimeOut write FTimeOut;
  end;

implementation

const
  OPERATION_GET_LIST = 1;
  OPERATION_GET_FILE = 2;

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

procedure TUserThread.Execute;
var
  Request: string;
begin
  while (not FStop) and (not Terminated) do
  begin
    Request := FBlockSocekt.RecvString(FTimeOut);
    if FBlockSocekt.LastError <> 0 then
      Break;

    with (GetJSON(Request) as TJSONObject) do
    begin
      case Integers['operation'] of
        OPERATION_GET_LIST: SendFileList;
        OPERATION_GET_FILE: SendNewFiles;
      end;
    end;
  end;
end;

constructor TUserThread.Create(const ASocket: TSocket);
begin
  inherited Create(True);
  FBlockSocekt := TTCPBlockSocket.Create;
  FreeOnTerminate := True;
  FBlockSocekt.Socket := ASocket;
end;

destructor TUserThread.Destroy;
begin
  FBlockSocekt.CloseSocket;
  FBlockSocekt.Free;
  inherited Destroy;
end;

end.

