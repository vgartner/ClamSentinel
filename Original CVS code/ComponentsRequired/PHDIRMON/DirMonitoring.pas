{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DirMonitoring;

interface

uses
  DirMon, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('DirMon', @DirMon.Register);
end;

initialization
  RegisterPackage('DirMonitoring', @Register);
end.
