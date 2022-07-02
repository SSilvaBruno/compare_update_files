unit uFormPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  MaskEdit, ExtCtrls, Menus, EditBtn, uThreadClient;

type

  { TFormPrincipal }

  TFormPrincipal = class(TForm)
    btnStatClient: TButton;
    edtRemoteIP: TLabeledEdit;
    edtRemotePort: TLabeledEdit;
    edtBaseDir: TLabeledEdit;
    procedure btnStatClientClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
  private
    FThread: TThreadClient;
  public

  end;

var
  FormPrincipal: TFormPrincipal;

implementation


{$R *.lfm}

{ TFormPrincipal }

procedure TFormPrincipal.btnStatClientClick(Sender: TObject);
begin
  FThread            := TThreadClient.Create(True);
  FThread.RemoteIP   := edtRemoteIP.Text;
  FThread.RemotePort := edtRemotePort.Text;
  FThread.Path       := edtBaseDir.Text;
  FThread.TimeOut    := 10000;
  FThread.Start;
end;

procedure TFormPrincipal.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FThread) and FThread.IsRunning then
    FThread.Stop;
end;

end.

