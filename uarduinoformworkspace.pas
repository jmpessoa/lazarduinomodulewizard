unit uArduinoFormWorkspace;

{$mode objfpc}{$H+}

interface

uses
  inifiles, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls;

type

  TCodeTemplate = (ctMinimal, ctBlinking, ctSerial);

  { TArduinoFormWorkspace }

  TArduinoFormWorkspace = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ComboBoxCodeTemplate: TComboBox;
    EditBasePath: TEdit;
    EditProjectName: TEdit;
    GroupBox1: TGroupBox;
    GroupBoxCodeTemplate: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Panel3: TPanel;
    RadioGroupTarget: TRadioGroup;
    selDir: TSelectDirectoryDialog;
    SpeedButton1: TSpeedButton;
    StatusBar1: TStatusBar;
    procedure ComboBoxCodeTemplateChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
    FWorkspacePath: string;
    FProjectName: string;
    FTargetSpecific: string; //-Wp
    FDeleteGeneratedAssembler: boolean; //-a
    FInstructionSet: string;  //-Cp
    FCodeTemplate: TCodeTemplate;
    FPathToArduinoIDE: string;
    FPathToCodeTemplates: string;
    //FBaud: string;
  public
    { public declarations }
    procedure LoadSettings(const pFilename: string);
    procedure SaveSettings(const pFilename: string);

    property WorkspacePath: string read FWorkspacePath write FWorkspacePath;
    property ProjectName: string read FProjectName write FProjectName;

    property TargetSpecific: string read FTargetSpecific write FTargetSpecific; //-Wp
    property DeleteGeneratedAssembler: boolean read FDeleteGeneratedAssembler write FDeleteGeneratedAssembler; //-a
    property InstructionSet: string  read FInstructionSet write FInstructionSet;  //-Cp
    property CodeTemplate: TCodeTemplate  read FCodeTemplate write FCodeTemplate;
    property PathToArduinoIDE: string read FPathToArduinoIDE write FPathToArduinoIDE;
    property PathToCodeTemplates: string read FPathToCodeTemplates write FPathToCodeTemplates;

  end;

var
  ArduinoFormWorkspace: TArduinoFormWorkspace;

implementation

{$R *.lfm}

{ TArduinoFormWorkspace }

procedure TArduinoFormWorkspace.FormShow(Sender: TObject);
begin
  if EditBasePath.Text = '' then
    EditBasePath.SetFocus
  else
    EditProjectName.SetFocus;
end;

procedure TArduinoFormWorkspace.SpeedButton1Click(Sender: TObject);
begin
  if SelDir.Execute then
  begin
    EditBasePath.Text := SelDir.FileName;
    EditProjectName.SetFocus;
  end;
end;

procedure TArduinoFormWorkspace.LoadSettings(const pFilename: string);
begin
  with TIniFile.Create(pFilename) do
  begin
    FWorkspacePath := ReadString('NewProject', 'PathToWorkspace', '');
    FPathToArduinoIDE := ReadString('NewProject', 'PathToArduinoIDE', '');
    FPathToCodeTemplates := ReadString('NewProject', 'PathToCodeTemplates', '');
    Free;
  end;
  EditBasePath.Text := FWorkspacePath;
end;

procedure TArduinoFormWorkspace.SaveSettings(const pFilename: string);
begin
  FWorkspacePath:= Trim(EditBasePath.Text);
  with TInifile.Create(pFilename) do
  begin
    WriteString('NewProject', 'PathToWorkspace', FWorkspacePath);
    WriteString('NewProject', 'PathToArduinoIDE', FPathToArduinoIDE);
    WriteString('NewProject', 'PathToCodeTemplates', FPathToCodeTemplates);
    Free;
  end;
end;

procedure TArduinoFormWorkspace.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  FProjectName:= EditProjectName.Text;
  FWorkspacePath:= EditBasePath.Text;
  //FBaud:= ComboBoxBaud.Text;

  case RadioGroupTarget.ItemIndex of
    0: begin    //UNO
         FTargetSpecific:= 'atmega328p'; //-Wp
         FInstructionSet:= 'avr5'     //-Cp
       end;
    1: begin   //MEGA
         FTargetSpecific:= 'atmega2560'; //-Wp
         FInstructionSet:= 'avr6'     //-Cp
       end;
  end;

  FDeleteGeneratedAssembler:= False; //-a

  case ComboBoxCodeTemplate.ItemIndex of
     0: FCodeTemplate:= ctMinimal;
     1: FCodeTemplate:= ctBlinking;
     2: FCodeTemplate:= ctSerial;
  end;

end;

procedure TArduinoFormWorkspace.ComboBoxCodeTemplateChange(Sender: TObject);
var
  userString: string;
begin
  if ComboBoxCodeTemplate.ItemIndex = 2 then //serial
  begin
     if FPathToCodeTemplates = '' then
     begin
        userString:= 'C:\lazarus\components\arduinomodulewizard\templates';
        if InputQuery('Configure Path', 'Path to code templates', userString) then
        begin
           FPathToCodeTemplates:= userString;
        end;
     end;
  end;
end;

end.

