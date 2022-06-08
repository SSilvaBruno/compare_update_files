unit uThreadServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock, fpjson, jsonparser,
  uThreadBase, uThreadSocketList;

type

  { TThreadServer }

  TThreadServer = class(TThreadBase)
  strict private
    FClientList: TThreadSocketList;
    FLocalIP   : string;
    FLocalPort : string;
  protected
    procedure Execute; override;
  public
    constructor Create(const AFreeOnTerminate: Boolean);
    destructor Destroy; override;
  end;

implementation

{ TThreadServer }

procedure TThreadServer.Execute;
var
  I: Integer;
  Socket: TSocket;
begin
  BlockSocket.CreateSocket;
  BlockSocket.Bind(FLocalIP, FLocalPort);
  BlockSocket.Listen;
  if BlockSocket.LastError = 0 then
  begin
    while IsRunning do
    begin
    if BlockSocket.CanRead(1000) then
    begin
      Socket := BlockSocket.Accept;
      if not FClientList.Find(Socket) then
      begin
        I := FClientList.Add(Socket);
        if BlockSocket.LastError = 0 then
          FClientList[I].Start
        else
          FClientList.Delete(I);
        end;
      end;
    end;
  end;
end;

constructor TThreadServer.Create(const AFreeOnTerminate: Boolean);
begin
  Inherited Create(AFreeOnTerminate);
  FClientList := TThreadSocketList.Create;
  FLocalIP    := '127.0.0.1';
  FLocalPort  := '3270';
end;

destructor TThreadServer.Destroy;
begin
  FClientList.Free;
  inherited Destroy;
end;

end.

