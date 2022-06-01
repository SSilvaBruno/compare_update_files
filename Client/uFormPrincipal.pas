unit uFormPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uThreadClasses;

type

  { TFormPrincipal }

  TFormPrincipal = class(TForm)
    btnStatClient: TButton;
    procedure btnStatClientClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    FThread: TClientThread;
  public

  end;

var
  FormPrincipal: TFormPrincipal;

implementation


{$R *.lfm}

{ TFormPrincipal }

procedure TFormPrincipal.btnStatClientClick(Sender: TObject);
begin
  FThread           := TClientThread.Create(True);
  FThread.IPAddress := '127.0.0.1';
  FThread.Port      := '3270';
  FThread.TimeOut   := 10000;
  FThread.Path      := 'C:\Tecnobyte\SAC_Pro';
  FThread.Start;
end;

procedure TFormPrincipal.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    FThread.WaitFor;
  end;
end;

end.

