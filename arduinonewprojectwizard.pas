{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ArduinoNewProjectWizard;

interface

uses
  ArduinoNewProjectWizard_intf, uArduinoFormWorkspace, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ArduinoNewProjectWizard_intf', 
    @ArduinoNewProjectWizard_intf.Register);
end;

initialization
  RegisterPackage('ArduinoNewProjectWizard', @Register);
end.
