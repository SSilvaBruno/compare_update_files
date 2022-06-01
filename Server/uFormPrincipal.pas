unit uFormPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uThreadClasses, ExStdCtrls;

type

  { TFormPrincipal }

  TFormPrincipal = class(TForm)
    btnStartServer: TButton;
    mmConnections: TExMemo;
    procedure btnStartServerClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
  private
    FServer: TServerThread;
  public

  end;

var
  FormPrincipal: TFormPrincipal;

implementation

{$R *.lfm}

{ TFormPrincipal }

procedure TFormPrincipal.btnStartServerClick(Sender: TObject);
begin
  FServer           := TServerThread.Create(True);
  FServer.IPAddress := '127.0.0.1';
  FServer.Port      := '3270';
  FServer.TimeOut   := 10000;
  FServer.Start;
end;

procedure TFormPrincipal.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FServer) then
  begin
    FServer.Terminate;
    FServer.WaitFor;
  end;
end;

end.

