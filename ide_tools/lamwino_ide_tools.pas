{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lamwino_ide_tools;

interface

uses
  lamwino_ide_menu_items, uFormBuildFPCAVRCross, uformlamwinosettingspaths, 
  uFormGetFPCSourceTrunk, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('lamwino_ide_menu_items', @lamwino_ide_menu_items.Register);
end;

initialization
  RegisterPackage('lamwino_ide_tools', @Register);
end.
