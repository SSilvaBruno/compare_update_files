unit uFormPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  EditBtn, StdCtrls, ComCtrls, Menus, Buttons, uFunc, fpjson, jsonparser,
  strutils;

type

  { TFormPrincipal }

  TFormPrincipal = class(TForm)
    pgcRules: TPageControl;
    tabFiles: TTabSheet;
    tabExtensions: TTabSheet;
    tabExceptions: TTabSheet;
    pnlRuleFiles: TPanel;
    pnlDirectory: TPanel;
    edtBaseDir: TDirectoryEdit;
    lblBaseDir: TLabel;
    pnlButton: TPanel;
    pnlRuleExtensions: TPanel;
    pnlRuleExceptions: TPanel;
    mmRuleFiles: TMemo;
    lblRuleFiles: TLabel;
    btnRuleFile: TSpeedButton;
    edtRuleFiles: TEditButton;
    btnCreateJSONFile: TBitBtn;

    procedure edtBaseDirChange(Sender: TObject);
    procedure edtRuleFilesButtonClick(Sender: TObject);
    procedure btnRuleFileClick(Sender: TObject);
    procedure btnCreateJSONFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  strict private
    FJSONArray: TJSONArray;
  public
    procedure UpdateForm;
  end;

var
  FormPrincipal: TFormPrincipal;

implementation

{$R *.lfm}

{ TFormPrincipal }

procedure TFormPrincipal.edtBaseDirChange(Sender: TObject);
begin
  UpdateForm;
end;

procedure TFormPrincipal.edtRuleFilesButtonClick(Sender: TObject);
var
  OpenDlg: TOpenDialog;
  Confirmed, InBaseDir: Boolean;
begin
  OpenDlg := TOpenDialog.Create(Self);
  try
    OpenDlg.Title      := 'Select file';
    OpenDlg.InitialDir := edtBaseDir.Text;
    repeat
      Confirmed := OpenDlg.Execute;
      InBaseDir := FileExists(OpenDlg.FileName) and (Pos(edtBaseDir.Text, OpenDlg.FileName) > 0);
      OpenDlg.InitialDir := edtBaseDir.Text;

      if Confirmed and (not InBaseDir) then
        ShowMessage('File does not exist or is not in the specified root folder.');
    until (not Confirmed) or (Confirmed and InBaseDir);

    if Confirmed and InBaseDir then
      edtRuleFiles.Text := OpenDlg.FileName;
    UpdateForm;
  finally
    OpenDlg.Free;
  end;
end;

procedure TFormPrincipal.btnRuleFileClick(Sender: TObject);
var
  P: Integer;
begin
  if not FileExists(edtRuleFiles.Text) then
    raise Exception.CreateFmt('File not exists: %s', [edtRuleFiles.Text]);
  if mmRuleFiles.Lines.IndexOf(edtRuleFiles.Text) >= 0 then
    raise Exception.CreateFmt('File has already been added: %s', [edtRuleFiles.Text]);

  P := Length(edtRuleFiles.Text) - (Length(edtBaseDir.Text) + 1);
  mmRuleFiles.Lines.Add(RightStr(edtRuleFiles.Text, P));
  edtRuleFiles.Clear;
end;

procedure TFormPrincipal.btnCreateJSONFileClick(Sender: TObject);
var
  I, ItemIndex: Integer;
begin
  for I := 0 to mmRuleFiles.Lines.Count -1 do
  begin
    ItemIndex := FJSONArray.Add(TJSONObject.Create);
    with FJSONArray.Objects[ItemIndex] do
    begin
      Strings['file'] := mmRuleFiles.Lines[I];
      Strings['hash'] := GetHashFile(GetFileFullName(edtBaseDir.Text, mmRuleFiles.Lines[I]));
    end;
  end;
  SaveStringToFile(FJSONArray.AsJSON, 'filelist.JSON');
end;

procedure TFormPrincipal.FormCreate(Sender: TObject);
begin
  FJSONArray := TJSONArray.Create;
end;

procedure TFormPrincipal.FormDestroy(Sender: TObject);
begin
  FJSONArray.Free;
end;

procedure TFormPrincipal.UpdateForm;
begin
  pgcRules.Enabled    := edtBaseDir.Text <> '';
  btnRuleFile.Enabled := pgcRules.Enabled and (edtRuleFiles.Text <> '');
end;

end.

