unit uFunc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, AbZipper, AbUnzper, AbZipTyp, AbUtils,
  AbArcTyp, md5;

  function GetFileFullName(const APath, AFile: string): string;
  function GetReferenceFileFullName(const APath: string): string;
  function GetJSONArrayOfFileList(const APath: string): TJSONArray;
  function GetHashFile(const AFile: string): string;
  function FileArrayToZipStream(const ABaseDir: string; const AFilesArray: TJSONArray; var AStream: TMemoryStream): Boolean;
  function FileHashEquals(const APath, AFile, AHash: string): Boolean;
  function ZipStreamToFile(const ABaseDir: string; const AStream: TMemoryStream): Boolean;
  function ZipStreamToString(const AStream: TStream): string;

  procedure SaveStringToFile(const AData, AFileName: string);

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

function GetHashFile(const AFile: string): string;
begin
  Result := MD5Print(MD5File(AFile));
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
        Zip.AddFromStream(AFilesArray.Objects[I].Strings['file'], FileStream);
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

function FileHashEquals(const APath, AFile, AHash: string): Boolean;
var
  LocalFile: string;
begin
  LocalFile := GetFileFullName(APath, AFile);
  Result    := FileExists(LocalFile) and (AHash = GetHashFile(LocalFile));
end;

function ZipStreamToFile(const ABaseDir: string; const AStream: TMemoryStream): Boolean;
var
  UnZip: TAbUnZipper;
begin
  Result     := True;
  UnZip      := TAbUnZipper.Create(nil);
  try
    UnZip.ExtractOptions := [eoCreateDirs, eoRestorePath];
    UnZip.BaseDirectory   := ABaseDir;
    UnZip.ArchiveType     := atZip;
    UnZip.ForceType       := True;
    UnZip.Password        := '';
    UnZip.Stream          := AStream;
    try
      UnZip.ExtractFiles('*.*');
    except
      Result := False;
      raise;
    end;
  finally
    UnZip.Free;
  end;
end;

function ZipStreamToString(const AStream: TStream): string;
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create('');
  try
    AStream.Position := 0;
    StringStream.CopyFrom(AStream, AStream.Size);
    Result := StringStream.DataString;
  finally
    StringStream.Free;
  end;
end;

procedure SaveStringToFile(const AData, AFileName: string);
var
  StringStream: TStringStream;
  FileStream: TFileStream;
begin
  StringStream := TStringStream.Create(AData);
  try
    FileStream := TFileStream.Create(AFileName, fmCreate);
    try
      FileStream.CopyFrom(StringStream, StringStream.Size);
    finally
      FileStream.Free;
    end;
  finally
    StringStream.Free;
  end;
end;

end.

