unit uFormPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uThreadClasses;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  Server: TServerThread;
begin
  Server           := TServerThread.Create(True);
  Server.IPAddress := '127.0.0.1';
  Server.Port      := '3270';
  Server.TimeOut   := 10000;
  Server.Start;
end;

end.

