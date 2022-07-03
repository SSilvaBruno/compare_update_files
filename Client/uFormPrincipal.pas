unit uFormPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  MaskEdit, ExtCtrls, Menus, EditBtn, IniFiles,
  uThreadClient;

type

  { TFormPrincipal }

  TFormPrincipal = class(TForm)
    btnStatClient: TButton;
    edtRemoteIP: TLabeledEdit;
    edtRemotePort: TLabeledEdit;
    edtBaseDir: TLabeledEdit;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);

    procedure btnStatClientClick(Sender: TObject);
  private
    FThread: TThreadClient;
    FIniFile: TIniFile;
  public

  end;

var
  FormPrincipal: TFormPrincipal;

implementation


{$R *.lfm}

{ TFormPrincipal }

procedure TFormPrincipal.FormCreate(Sender: TObject);
begin
  FIniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
end;

procedure TFormPrincipal.FormShow(Sender: TObject);
begin
  edtRemoteIP.Text := FIniFile.ReadString('Server', 'IP', '127.0.0.1');
  edtRemotePort.Text := FIniFile.ReadString('Server', 'Port', '3270');
  edtBaseDir.Text := FIniFile.ReadString('Server', 'BaseDir', ExtractFileDir(ParamStr(0)));
end;

procedure TFormPrincipal.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FIniFile.WriteString('Server', 'IP', edtRemoteIP.Text);
  FIniFile.WriteString('Server', 'Port', edtRemotePort.Text);
  FIniFile.WriteString('Server', 'BaseDir', edtBaseDir.Text);

  if Assigned(FThread) and FThread.IsRunning then
    FThread.Stop;
end;

procedure TFormPrincipal.FormDestroy(Sender: TObject);
begin
  FIniFile.Free;
end;

procedure TFormPrincipal.btnStatClientClick(Sender: TObject);
begin
  FThread            := TThreadClient.Create(True);
  FThread.RemoteIP   := edtRemoteIP.Text;
  FThread.RemotePort := edtRemotePort.Text;
  FThread.Path       := edtBaseDir.Text;
  FThread.TimeOut    := 10000;
  FThread.Start;
end;

end.

