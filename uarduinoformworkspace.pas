unit uArduinoFormWorkspace;

{$mode objfpc}{$H+}

interface

uses
  inifiles, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls;

type

  TGeneratedCode = (gcBlinking, gcMinimal);

  { TArduinoFormWorkspace }

  TArduinoFormWorkspace = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    EditBasePath: TEdit;
    EditProjectName: TEdit;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Panel3: TPanel;
    RadioGroupGeneratedCode: TRadioGroup;
    RadioGroupTarget: TRadioGroup;
    selDir: TSelectDirectoryDialog;
    SpeedButton1: TSpeedButton;
    StatusBar1: TStatusBar;
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
    FGeneratedCode: TGeneratedCode;
    FPathToArduinoIDE: string;
  public
    { public declarations }
    procedure LoadSettings(const pFilename: string);
    procedure SaveSettings(const pFilename: string);

    property WorkspacePath: string read FWorkspacePath write FWorkspacePath;
    property ProjectName: string read FProjectName write FProjectName;

    property TargetSpecific: string read FTargetSpecific write FTargetSpecific; //-Wp
    property DeleteGeneratedAssembler: boolean read FDeleteGeneratedAssembler write FDeleteGeneratedAssembler; //-a
    property InstructionSet: string  read FInstructionSet write FInstructionSet;  //-Cp
    property GeneratedCode: TGeneratedCode  read FGeneratedCode write FGeneratedCode;
    property PathToArduinoIDE: string read FPathToArduinoIDE write FPathToArduinoIDE;

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
    Free;
  end;
end;

procedure TArduinoFormWorkspace.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  FProjectName:= EditProjectName.Text;
  FWorkspacePath:= EditBasePath.Text;

  case RadioGroupTarget.ItemIndex of
    0: begin
         FTargetSpecific:= 'atmega328p'; //-Wp
         FInstructionSet:= 'avr5'     //-Cp
       end;
  end;

  FDeleteGeneratedAssembler:= False; //-a

  case RadioGroupGeneratedCode.ItemIndex of
     0: FGeneratedCode:= gcBlinking;
     1: FGeneratedCode:= gcMinimal;
  end;

end;

end.

