unit uFunc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, AbZipper, AbZipTyp, AbUtils;

  function GetFileFullName(const APath, AFile: string): string;
  function GetReferenceFileFullName(const APath: string): string;
  function GetJSONArrayOfFileList(const APath: string): TJSONArray;
  function FileArrayToZipStream(const ABaseDir: string; const AFilesArray: TJSONArray; var AStream: TMemoryStream): Boolean;

implementation

uses uConst;

function GetFileFullName(const APath, AFile: string): string;
begin
  Result := IncludeTrailingPathDelimiter(APath) + AFile;
end;

function GetReferenceFileFullName(const APath: string): string;
begin
  Result := GetFileFullName(APath, REFERENCE_FILE_NAME);
end;

function GetJSONArrayOfFileList(const APath: string): TJSONArray;
var
  ReferenceFile: string;
  Stream       : TMemoryStream;
begin
  ReferenceFile := GetReferenceFileFullName(APath);
  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(ReferenceFile);
    Stream.Position := 0;
    Result          := TJSONArray(GetJSON(Stream));
  finally
    Stream.Free;
  end;
end;

function FileArrayToZipStream(const ABaseDir: string; const AFilesArray: TJSONArray; var AStream: TMemoryStream): Boolean;
var
  FileStream: TMemoryStream;
  FileName  : string;
  Zip       : TAbZipper;
  I         : Integer;
begin
  Result     := True;
  FileStream := TMemoryStream.Create;
  Zip        := TAbZipper.Create(nil);
  try
    Zip.BaseDirectory   := ABaseDir;
    Zip.DeflationOption := doMaximum;
    Zip.ArchiveType     := atZip;
    Zip.ForceType       := True;
    Zip.Password        := '';
    Zip.Stream          := AStream;

    for I := 0 to AFilesArray.Count - 1 do
    begin
      FileName := GetFileFullName(ABaseDir, AFilesArray.Objects[I].Strings['file']);
      FileStream.Clear;
      if FileExists(FileName) then
      begin
        FileStream.LoadFromFile(FileName);
        Zip.AddFromStream(AFilesArray.Strings[I], FileStream);
      end else
      begin
        Result := False;
        Break;
      end;
    end;
  finally
    FileStream.Free;
    Zip.Free;
  end;
end;

end.

