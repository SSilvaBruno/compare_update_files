unit uThreadBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock;

type
  TDisconectEvent = procedure(const ASocket: TSocket) of object;

  { TThreadBase }

  TThreadBase = class(TThread)
  strict private
    FBlockSocket: TTCPBlockSocket;
    FTimeOut    : Integer;
    FFatalError : string;
  strict protected
    procedure Terminate;
  strict protected
    property BlockSocket: TTCPBlockSocket read FBlockSocket;
    property FreeOnTerminate;
  public
    constructor Create(const AFreeOnTerminate: Boolean); reintroduce;
    destructor Destroy; override;

    function IsRunning: Boolean;
    procedure Stop; virtual;

    property TimeOut: Integer read FTimeOut write FTimeOut;
    property FatalError: string read FFatalError write FFatalError;
  end;

implementation

{ TThreadBase }

procedure TThreadBase.Terminate;
begin
  inherited Terminate;
end;

constructor TThreadBase.Create(const AFreeOnTerminate: Boolean);
begin
  Inherited Create(True);
  FBlockSocket    := TTCPBlockSocket.Create;
  FreeOnTerminate := AFreeOnTerminate;
  FFatalError     := '';
end;

destructor TThreadBase.Destroy;
begin
  FBlockSocket.CloseSocket;
  FBlockSocket.Free;
  inherited Destroy;
end;

function TThreadBase.IsRunning: Boolean;
begin
  Result := (not Terminated) and (FFatalError = '');
end;

procedure TThreadBase.Stop;
begin
  if not Terminated then
    Terminate;
end;

end.

