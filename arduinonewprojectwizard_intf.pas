unit ArduinoNewProjectWizard_intf;

{$MODE DELPHI}

interface

uses

  uArduinoFormWorkspace,
  FileUtil, StrUtils,
  Classes, SysUtils, Controls, Forms, Dialogs,
  LazIDEIntf, ProjectIntf;

type

  { TArduinoApplicationDescriptor }

  TArduinoApplicationDescriptor = class(TProjectDescriptor)
  private

    FProjectName: string;
    FProjectPath   : string;
    FTargetSpecific: string; //-Wp
    FDeleteGeneratedAssembler: boolean; //-a
    FInstructionSet: string;  //-Cp
    FGeneratedCode: TGeneratedCode;
    FPathToArduinoIDE: string;

    function SettingsFilename: string;
    function GetWorkSpaceFromForm: boolean;
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function DoInitDescriptor: TModalResult; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

var
  ProjectDescriptorArduinoApplication: TArduinoApplicationDescriptor;

procedure Register;

implementation

procedure Register;
begin
  ProjectDescriptorArduinoApplication := TArduinoApplicationDescriptor.Create;
  RegisterProjectDescriptor(ProjectDescriptorArduinoApplication);
end;

 {TArduinoApplicationDescriptor}

function TArduinoApplicationDescriptor.SettingsFilename: string;
begin
  Result := AppendPathDelim(LazarusIDE.GetPrimaryConfigPath) + 'AVRArduinoProject.ini'
end;

function TArduinoApplicationDescriptor.GetWorkSpaceFromForm: boolean;
var
  frm: TArduinoFormWorkspace;
  installList: TStringList;
begin
  Result := False;
  frm := TArduinoFormWorkspace.Create(nil);
  frm.LoadSettings(SettingsFilename);
  if frm.ShowModal = mrOK then
  begin

    frm.SaveSettings(SettingsFilename);

    FTargetSpecific:= frm.TargetSpecific; //-Wp
    FDeleteGeneratedAssembler:= frm.DeleteGeneratedAssembler; //-a
    FInstructionSet:= frm.InstructionSet;  //-Cp
    FProjectName := trim(frm.ProjectName);
    FProjectPath := Trim(frm.WorkspacePath) + DirectorySeparator + FProjectName;
    FGeneratedCode:= frm.GeneratedCode;
    FPathToArduinoIDE:= frm.PathToArduinoIDE;

    ForceDirectory(FProjectPath);
    chdir(FProjectPath);

    installList:= TStringList.Create;   //thanks to @HatForGat!
    installList.Add(FPathToArduinoIDE+DirectorySeparator+
                     'hardware'+DirectorySeparator+
                     'tools'+DirectorySeparator+
                     'avr'+DirectorySeparator+
                     'bin'+DirectorySeparator+
                     'avrdude -C'+FPathToArduinoIDE+
                     'hardware'+DirectorySeparator+
                     'tools'+DirectorySeparator+
                     'avr'+DirectorySeparator+
                     'etc'+DirectorySeparator+
                     'avrdude.conf -v -patmega328p -carduino -P\\.\%1 -b115200 -D -Uflash:w:'+FProjectName+'.hex:i');

    installList.SaveToFile(FProjectPath+DirectorySeparator+'install.bat');

    Result := True;
  end;
  frm.Free;
end;

constructor TArduinoApplicationDescriptor.Create;
begin
  inherited Create;
  Name := 'Create New Arduino AVR Module [Lamwino]';
end;

function TArduinoApplicationDescriptor.GetLocalizedName: string;
begin
  Result:= 'Arduino AVR Module [Lamwino]';
end;

function TArduinoApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result:=  'Arduino [AVR] Module'+ LineEnding +
            '[Native "*.hex" Executable]'+ LineEnding +
            'The project is maintained by Lazarus [Lamwino].'
end;

function TArduinoApplicationDescriptor.DoInitDescriptor: TModalResult;
begin
  if GetWorkSpaceFromForm then
      Result := mrOK
  else
    Result := mrAbort;
end;

function TArduinoApplicationDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  NewSource: TStringList;
  MainFile: TLazProjectFile;
begin
  inherited InitProject(AProject);

  Aproject.Title := FProjectName;
  NewSource:= TStringList.Create;

  if FGeneratedCode = gcBlinking then
  begin
    NewSource.Add('program ' + FProjectName +'; //Lamwino: Lazarus Arduino Module Wizard :: '+DateTimeToStr(Now));
    NewSource.Add('//thanks to @ykot! ref. http://forum.lazarus.freepascal.org/index.php/topic,30960.0.html');
    NewSource.Add(' ');
    NewSource.Add('//uses');
    NewSource.Add(' ');
    NewSource.Add(' ');
    NewSource.Add('const');
    NewSource.Add('  PB5 = 1 shl 5; //Bit 5 in "PortB" control UNO Pin13 [internal LED]');
    NewSource.Add(' ');
    NewSource.Add('var');
    NewSource.Add('  DelayVar: Integer = 0;');
    NewSource.Add(' ');
    NewSource.Add('procedure SomeDelay;');
    NewSource.Add('var');
    NewSource.Add('  I: LongInt;');
    NewSource.Add('begin');
    NewSource.Add('  for I := 0 to 400000 do');
    NewSource.Add('    Dec(DelayVar);');
    NewSource.Add('end;');
    NewSource.Add(' ');
    NewSource.Add(' ');
    NewSource.Add('begin');
    NewSource.Add(' ');
    NewSource.Add('  DDRB := DDRB or PB5; //force DDRB bit 5 to 1 !!  [i.e,  signalize PORTB bit 5 [Pin13] as output]');
    NewSource.Add(' ');
    NewSource.Add('  while True do');
    NewSource.Add('  begin');
    NewSource.Add(' ');
    NewSource.Add('     PORTB := PORTB and (not PB5); //force PORTB bit 5 [Pin13]  to 0!');
    NewSource.Add('     SomeDelay;');
    NewSource.Add(' ');
    NewSource.Add('     PORTB := PORTB or PB5;         //force PORTB bit 5 [Pin13] to 1!');
    NewSource.Add('     SomeDelay;');
    NewSource.Add(' ');
    NewSource.Add('  end;');
    NewSource.Add(' ');
    NewSource.Add('end.');
  end;

  if FGeneratedCode = gcMinimal then
  begin
      NewSource.Add('program ' + FProjectName+'; //Lamwino: Lazarus Arduino Module Wizard :: '+DateTimeToStr(Now));
      NewSource.Add(' ');
      NewSource.Add('//uses');
      NewSource.Add(' ');
      NewSource.Add(' ');
      NewSource.Add('//const');
      NewSource.Add(' ');
      NewSource.Add('var');
      NewSource.Add('  DelayVar: Integer = 0;');
      NewSource.Add(' ');
      NewSource.Add('procedure SomeDelay; //by @ykot');
      NewSource.Add('var');
      NewSource.Add('  I: LongInt;');
      NewSource.Add('begin');
      NewSource.Add('  for I := 0 to 400000 do');
      NewSource.Add('    Dec(DelayVar);');
      NewSource.Add('end;');

      NewSource.Add('begin');
      NewSource.Add(' ');
      NewSource.Add('  while True do');
      NewSource.Add('  begin');
      NewSource.Add(' ');
      NewSource.Add('     SomeDelay;');
      NewSource.Add(' ');
      NewSource.Add('  end;');
      NewSource.Add(' ');
      NewSource.Add('end.');
  end;

  MainFile := AProject.CreateProjectFile( FProjectPath + DirectorySeparator+ Lowercase(FProjectName) + '.lpr');
  MainFile.SetSourceText(NewSource.Text);
  MainFile.IsPartOfProject := True;
  AProject.AddFile(MainFile, False {NOT Added To Project Uses Clause});
  AProject.MainFileID := 0;

  AProject.Flags := AProject.Flags - [pfMainUnitHasCreateFormStatements,
                                      pfMainUnitHasTitleStatement];
  AProject.UseManifest:= False;
  AProject.UseAppBundle:= False;

  {Parsing}
  AProject.LazCompilerOptions.SyntaxMode:= 'Delphi';

  {CodeGeneration}
  AProject.LazCompilerOptions.SmartLinkUnit:= True;
  AProject.LazCompilerOptions.TargetCPU:= 'avr';    {-P}
  AProject.LazCompilerOptions.TargetOS:= 'embedded';
  AProject.LazCompilerOptions.OptimizationLevel:= 3;
  AProject.LazCompilerOptions.Win32GraphicApp:= False;

   {Linking}
  AProject.LazCompilerOptions.StripSymbols:= True; {-Xs}
  AProject.LazCompilerOptions.LinkSmart:= True {-XX};
  AProject.LazCompilerOptions.GenerateDebugInfo:= False;
  //AProject.LazCompilerOptions.SmallerCode:= True;
  AProject.LazCompilerOptions.SmartLinkUnit:= True;

  AProject.LazCompilerOptions.IncludePath:='$(ProjOutDir)';         //-Fi
  AProject.LazCompilerOptions.UnitOutputDirectory := '\lib\$(TargetCPU)-$(TargetOS)';  //-FU
  AProject.LazCompilerOptions.TargetFilename:= FProjectName;   //-o

  //http://www.freepascal.org/docs-html/user/userap1.html
  // -W<x>  Target-specific options (targets);
  // -a     The compiler does not delete the generated assembler file;
  // -Cp<x>     Select instruction set;

  //FTargetSpecific:= 'atmega328p'; //-Wp
  //FDeleteGeneratedAssembler:= frm.DeleteGeneratedAssembler; //-a
  //FInstructionSet:= 'avr5';  //-Cp

  if not FDeleteGeneratedAssembler then
     AProject.LazCompilerOptions.CustomOptions:= '-Cp'+FInstructionSet+ ' -Wp'+FTargetSpecific  +' -a'
  else
    AProject.LazCompilerOptions.CustomOptions:= '-Cp'+FInstructionSet+ ' -Wp'+FTargetSpecific;

  AProject.CustomData.Values['LAMWINO'] := 'AVR';
  //AProject.CustomData.Values['MODULENAME'] := FProjectName+ '.hex';
  AProject.CustomData.Values['AVRCHIP'] := FTargetSpecific;

  AProject.ProjectInfoFile := FProjectPath + DirectorySeparator + ChangeFileExt( FProjectName, '.lpi');

  NewSource.Free;
  Result := mrOK;
end;

function TArduinoApplicationDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  if AProject=nil then Exit;
  LazarusIDE.DoSaveProject([sfSaveAs]);
  Result := mrOK;
end;

end.

