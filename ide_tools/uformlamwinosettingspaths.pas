unit uformlamwinosettingspaths;

{$mode objfpc}{$H+}

interface

uses
  inifiles, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls, LazIDEIntf;

type

  { TFormLamwinoSettingsPaths }

  TFormLamwinoSettingsPaths  = class(TForm)
    BitBtnOK: TBitBtn;
    BitBtnCancel: TBitBtn;
    EditCOMPort: TEdit;
    EditPathToArduinoIDE: TEdit;
    Label1: TLabel;
    LabelPathToArduinoIDE: TLabel;
    SelDirDlgPathToArduinoIDE: TSelectDirectoryDialog;
    SpBPathToArduinoIDE: TSpeedButton;
    SpeedButton1: TSpeedButton;
    StatusBar1: TStatusBar;
    procedure BitBtnOKClick(Sender: TObject);
    procedure BitBtnCancelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure SpBPathToArduinoIDEClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
    FOk: boolean;
  public
    { public declarations }
    procedure LoadSettings(const fileName: string);
    procedure SaveSettings(const fileName: string);
  end;

var
   FormLamwinoSettingsPaths: TFormLamwinoSettingsPaths;

implementation

{$R *.lfm}

{ TFormLamwinoSettingsPaths }

procedure TFormLamwinoSettingsPaths.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if FOk then
    Self.SaveSettings(LazarusIDE.GetPrimaryConfigPath + DirectorySeparator + 'AVRArduinoProject.ini' );
end;


procedure TFormLamwinoSettingsPaths.FormShow(Sender: TObject);
var
   strINI: TStringList;
begin

   if not FileExists(LazarusIDE.GetPrimaryConfigPath + DirectorySeparator+ 'AVRArduinoProject.ini') then
   begin
     strINI:= TStringList.Create;
     strINI.SaveToFile(LazarusIDE.GetPrimaryConfigPath + DirectorySeparator+ 'AVRArduinoProject.ini');
     strINI.Free;
   end;

   FOk:= False;
   Self.LoadSettings(LazarusIDE.GetPrimaryConfigPath + DirectorySeparator+ 'AVRArduinoProject.ini');
end;

procedure TFormLamwinoSettingsPaths.FormActivate(Sender: TObject);
begin
  EditPathToArduinoIDE.SetFocus;
end;

procedure TFormLamwinoSettingsPaths.BitBtnCancelClick(Sender: TObject);
begin
  FOk:= False;
  Close;
end;

procedure TFormLamwinoSettingsPaths.BitBtnOKClick(Sender: TObject);
begin
   FOk:= True;
   Close;
end;

procedure TFormLamwinoSettingsPaths.SpBPathToArduinoIDEClick(Sender: TObject);
begin
  if SelDirDlgPathToArduinoIDE.Execute then
  begin
    EditPathToArduinoIDE.Text := SelDirDlgPathToArduinoIDE.FileName;
  end;
end;

procedure TFormLamwinoSettingsPaths.SpeedButton1Click(Sender: TObject);
begin

ShowMessage('If Arduino is connected' + sLineBreak +
'and installed [drive!], try:'+sLineBreak +sLineBreak +
'1.Windows 10' + sLineBreak +
'Start Menu' + sLineBreak +
'Settings' + sLineBreak +
'Devices' + sLineBreak +
'Connected Devices' + sLineBreak + sLineBreak +
'2. All Windows:' + sLineBreak +
'Controls Panel' + sLineBreak +
'Device Manager' + sLineBreak +
'Ports(COM & LPT)');

end;

procedure TFormLamwinoSettingsPaths.LoadSettings(const fileName: string);
begin
  with TIniFile.Create(fileName) do
  try
      EditPathToArduinoIDE.Text := ReadString('NewProject','PathToArduinoIDE', '');
      EditCOMPort.Text := ReadString('NewProject','COMPort', '');
  finally
      Free;
  end;
end;

procedure TFormLamwinoSettingsPaths.SaveSettings(const fileName: string);
begin
  with TInifile.Create(fileName) do
  try
     WriteString('NewProject', 'PathToArduinoIDE', EditPathToArduinoIDE.Text);
     WriteString('NewProject', 'COMPort', EditCOMPort.Text);
  finally
     Free;
  end;
end;

end.

