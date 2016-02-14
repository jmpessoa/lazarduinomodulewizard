unit lamwino_ide_menu_items; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, IDECommands, MenuIntf, Forms,
  uformlamwinosettingspaths, uFormBuildFPCAVRCross;

procedure StartPathLamwinoTool(Sender: TObject);
procedure StartPathToBuildFpcAVRCross(Sender: TObject);

procedure Register;

implementation

uses
  LazIDEIntf, IDEMsgIntf, IDEExternToolIntf, ProjectIntf, Controls, IniFiles;

procedure StartPathLamwinoTool(Sender: TObject);
begin
  // Call path tool Code
  FormLamwinoSettingsPaths:=  TFormLamwinoSettingsPaths.Create(Application);
  FormLamwinoSettingsPaths.ShowModal;
end;

procedure StartPathToBuildFpcAVRCross(Sender: TObject);
begin
  FormBuildFPCAVRCross:= TFormBuildFPCAVRCross.Create(Application);
  FormBuildFPCAVRCross.ShowModal;
end;

procedure RunLamwino(Sender: TObject);
var
  Project: TLazProject;
  pathToArduinoIDE: string;
  deviceCOMPort, userString, strTemp: string;
  configFile: string;
  p, q, n: integer;
  Tool: TIDEExternalToolOptions;
  avrCHIP: string;
  projectFullName: string;
  Params: TStringList;
  pathToAvrdude: string;
  strExt: string;
  saveDeviceCOMPort, savePathToArduinoIDE: string;
begin

  Project := LazarusIDE.ActiveProject;

  if Assigned(Project) then
  begin
    if Project.CustomData.Values['LAMWINO'] = '' then
    begin
      ShowMessage('Sorry... The active project is not a LAMWINO project!');
      Exit;
    end;
  end;

  avrCHIP:=  Project.CustomData.Values['AVRCHIP'];

  projectFullName:= LazarusIDE.ActiveProject.ProjectInfoFile;
  projectFullName:= ChangeFileExt(projectFullName, '.hex');

  pathToArduinoIDE:= '';
  deviceCOMPort:= '';

  configFile:= LazarusIDE.GetPrimaryConfigPath+ DirectorySeparator+ 'AVRArduinoProject.ini';
  with TIniFile.Create(configFile) do
  try
    pathToArduinoIDE:= ReadString('NewProject','PathToArduinoIDE', '');
    deviceCOMPort:=  ReadString('NewProject','COMPort', '');
  finally
    Free;
  end;

  if pathToArduinoIDE =  '' then
  begin
    userString:= 'C:\Program Files (x86)\Arduino';
    if InputQuery('Configure Path', 'Path to Arduino IDE', userString) then
        pathToArduinoIDE:= userString;
  end;
  savePathToArduinoIDE:= pathToArduinoIDE;

  if deviceCOMPort =  '' then
  begin
    userString:= 'COM20';
    if InputQuery('Configure COM Port', 'Arduino USB/COM Port', userString) then
    begin
      deviceCOMPort:= userString;
    end;
  end;

  p:= Pos(' ', savePathToArduinoIDE);
  if p > 0 then
  begin
     q:= Pos('Program Files (x86)', savePathToArduinoIDE);
     if q > 0 then
     begin
        pathToArduinoIDE:= 'C:\Progra~2\Arduino';
     end
     else
     begin
        q:= Pos('Program Files', pathToArduinoIDE);
        if q > 0 then
        begin
           pathToArduinoIDE:= 'C:\Progra~1\Arduino';
        end;
     end;
  end;

  if Pos(' ', pathToArduinoIDE) > 0 then
  begin
     userString:= 'C:\Progra~1\Arduino';
     if InputQuery('Configure Path','Enter DOS "8.3" Format Name', userString) then
     begin
       if Pos(' ', userString) > 0 then
       begin
         ShowMessage('Sorry.. Invalid Path!');
         Exit;
       end;
       pathToArduinoIDE:= userString;
     end
     else Exit;
  end;

  saveDeviceCOMPort:= deviceCOMPort;

  p:= Pos('COM', deviceCOMPort);
  if p > 0 then
  begin
     deviceCOMPort:= '\\.\' + deviceCOMPort;
  end;

  with TIniFile.Create(configFile) do
  try
    WriteString('NewProject','PathToArduinoIDE', savePathToArduinoIDE);
    WriteString('NewProject','COMPort', saveDeviceCOMPort);
  finally
    Free;
  end;

  try         //thanks to Anton [aka "A.S"]
    IDEMessagesWindow.BringToFront;
    Params:= TStringList.Create;
    Tool := TIDEExternalToolOptions.Create;
    try
      Tool.Title := 'Running Extern [avrdute] Tool ... ';

      pathToAvrdude:= pathToArduinoIDE + DirectorySeparator +
                               'hardware' + DirectorySeparator +
                               'tools' + DirectorySeparator +
                               'avr'   + DirectorySeparator +
                               'bin';

      Tool.WorkingDirectory := ExtractFileDir(projectFullName);

      //C:\PROGRA~2\Arduino\hardware\tools\avr\bin\avrdude
      //-CC:\PROGRA~2\Arduino\hardware\tools\avr\etc\avrdude.conf
      //-v
      //-patmega328p
      //-carduino
      //-P\\.\COM20
      //-b115200
      //-D
      //-Uflash:w:Blinky.hex:i

      Params.Add('-v');
      Params.Add('-p'+ avrCHIP);
      Params.Add('-carduino');
      Params.Add('-P'+deviceCOMPort);
      Params.Add('-b115200');
      Params.Add('-D');
      Params.Add('-Uflash:w:'+projectFullName+':i');

      strExt:= '';
      {$IFDEF WINDOWS}
      strExt:= '.exe';
      {$ENDIF}

      Tool.CmdLineParams :=  '-C'+ pathToArduinoIDE+DirectorySeparator+
                                       'hardware'+DirectorySeparator+
                                       'tools' + DirectorySeparator+
                                       'avr' + DirectorySeparator+
                                       'etc' + DirectorySeparator+
                                       'avrdude.conf ' + Params.Text;

      Tool.Executable := pathToAvrdude + DirectorySeparator + 'avrdude' + strExt;
      Tool.Scanners.Add(SubToolDefault);

      if not RunExternalTool(Tool) then
        raise Exception.Create('Cannot Run Extern [avrdude] Tool!');

    finally
      Tool.Free;
      Params.Free;
    end;

  except
    on e: Exception do
      IDEMessagesWindow.SelectMsgLine(
        IDEMessagesWindow.AddCustomMessage(mluFatal,
          '[' + e.ClassName + '] Failed: ' + e.Message, '', 0, 0, 'Exception'));
  end;

end;

procedure Register;
var
  ideMnuLamwino: TIDEMenuSection;
  ideSubMnuLamwino: TIDEMenuSection;
begin
  // Register main menu
  ideMnuLamwino:= RegisterIDEMenuSection(mnuTools,'LAMWINO');
  // Register submenu
  ideSubMnuLamwino:= RegisterIDESubMenu(ideMnuLamwino, 'LAMWINO', '[Lamwino] Arduino Module Wizard');
  // Adding first entry
  RegisterIDEMenuCommand(ideSubMnuLamwino, 'PathLamwinoToolCmd', 'Settings [Paths, COM Port, ...]', nil, @StartPathLamwinoTool);
  // Adding 2a. entry
  RegisterIDEMenuCommand(ideSubMnuLamwino, 'PathToBuildFPCAVRCross', '[Lamwino] Build FPC Cross Arduino', nil, @StartPathToBuildFpcAVRCross);
  // And so on...
  RegisterIDEMenuCommand(itmRunBuilding, 'RunLamwino', '[Lamwino] Install/Run',nil, @RunLamwino);
end;

end.
