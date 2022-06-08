unit uThreadSocketList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, blcksock, synsock,
  uThreadSocket;

type

  { TThreadSocketList }

  TThreadSocketList = class
  strict private
    FList: TObjectList;
  strict private
    function GetItems(AIndex: Integer): TThreadSocket;

    procedure ThreadSocketOnTerminate(Sender: TObject);
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

implementation

{ TThreadSocketList }

function TThreadSocketList.GetItems(AIndex: Integer): TThreadSocket;
begin
  Result := FList.Items[AIndex] as TThreadSocket;
end;

procedure TThreadSocketList.ThreadSocketOnTerminate(Sender: TObject);
var
  I: Integer;
begin
  I := FList.IndexOf(Sender);
  if I >= 0 then
    Delete(I);
end;

constructor TThreadSocketList.Create;
begin
  inherited Create;
  FList := TObjectList.Create(False);
end;

destructor TThreadSocketList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TThreadSocketList.Add(const ASocket: TSocket): Integer;
var
  ThreadSocket: TThreadSocket;
begin
  ThreadSocket             := TThreadSocket.Create(ASocket);
  ThreadSocket.OnTerminate := @ThreadSocketOnTerminate;
  Result                   := FList.Add(ThreadSocket);
end;

function TThreadSocketList.Count: Integer;
begin
  Result := FList.Count;
end;

function TThreadSocketList.Find(const ASocket: TSocket): Boolean;
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

procedure TThreadSocketList.Delete(const AIndex: Integer);
var
  ThreadSocket: TThreadSocket;
begin
  if AIndex <= Count -1 then
  begin
    ThreadSocket := Items[AIndex];
    ThreadSocket.Stop;
    ThreadSocket.Free;
    FList.Delete(AIndex);
  end;
end;

procedure TThreadSocketList.StopThreads;
var
  Thread: TThreadSocket;
  I     : Integer;
begin
  for I := 0 to FList.Count -1 do
  begin
    Thread := Items[I];
    Thread.Stop;
    Thread.WaitFor;
  end;
end;

end.

