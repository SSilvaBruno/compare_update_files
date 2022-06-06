unit uConst;

{$mode objfpc}{$H+}

interface

const
  STATUS_SUCCESS      = 200;
  STATUS_ERROR        = 400;
  OPERATION_GET_LIST  = 1;
  OPERATION_GET_FILE  = 2;
  REFERENCE_FILE_NAME = 'filelist.JSON';

resourcestring
  SReferenceFileNotFound = 'Reference file not found.';

implementation

end.

