unit uFormPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uThreadServer, ExStdCtrls;

type

  { TFormPrincipal }

  TFormPrincipal = class(TForm)
    btnStartServer: TButton;
    mmConnections: TExMemo;
    procedure btnStartServerClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
  private
    FServer: TThreadServer;
  public

  end;

var
  FormPrincipal: TFormPrincipal;

implementation

{$R *.lfm}

{ TFormPrincipal }

procedure TFormPrincipal.btnStartServerClick(Sender: TObject);
begin
  FServer           := TThreadServer.Create(True);
  FServer.LocalIP   := '192.168.1.6';
  FServer.LocalPort := '3270';
  FServer.TimeOut   := 10000;
  FServer.Start;
end;

procedure TFormPrincipal.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FServer) and FServer.IsRunning then
    FServer.Stop;
end;

end.

