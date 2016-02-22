unit uformlamwinosettingspaths;

{$mode objfpc}{$H+}

interface

uses
  inifiles, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls, LazIDEIntf {$IFDEF WINDOWS}, Registry {$ENDIF} ;

type

  { TFormLamwinoSettingsPaths }

  TFormLamwinoSettingsPaths  = class(TForm)
    BitBtnOK: TBitBtn;
    BitBtnCancel: TBitBtn;
    ComboBoxCOMPort: TComboBox;
    EditCodeTemplates: TEdit;
    EditPathToArduinoIDE: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    LabelPathToArduinoIDE: TLabel;
    SelDirDlgPathToArduinoIDE: TSelectDirectoryDialog;
    SpBPathToArduinoIDE: TSpeedButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    StatusBar1: TStatusBar;
    procedure BitBtnOKClick(Sender: TObject);
    procedure BitBtnCancelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure SpBPathToArduinoIDEClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
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

//ref. http://forum.lazarus.freepascal.org/index.php?topic=14313.0
procedure TFormLamwinoSettingsPaths.SpeedButton1Click(Sender: TObject);
var
  {$IFDEF WINDOWS}
  reg: TRegistry;
  l: TStringList;
  n: integer;
  {$ENDIF}
  res: string;
begin
  //ShowMessage('Please, check if Arduino and PC are connecteds via USB!');
  res:='';
  ComboBoxCOMPort.Items.Clear;
  {$IFDEF WINDOWS}
  l:= TStringList.Create;
  reg:= TRegistry.Create;
  try
    {$IFNDEF VER100}
    reg.Access:= KEY_READ;
    {$ENDIF}
    reg.RootKey:= HKEY_LOCAL_MACHINE;
    reg.OpenKeyReadOnly('HARDWARE\DEVICEMAP\SERIALCOMM');//, false);
    reg.GetValueNames(l);

    for n:= 0 to l.Count - 1 do
    begin
      ComboBoxCOMPort.Items.Add(reg.ReadString(l[n]));
    end;

  finally
    reg.Free;
    l.Free;
  end;
  {$ENDIF}

  {$IFDEF LINUX}
    ShowMessage('Sorry... Not implemented yet...[LINUX]');
  {$ENDIF}

end;

procedure TFormLamwinoSettingsPaths.SpeedButton2Click(Sender: TObject);
begin
    if SelDirDlgPathToArduinoIDE.Execute then
  begin
    EditCodeTemplates.Text := SelDirDlgPathToArduinoIDE.FileName;
  end;
end;

procedure TFormLamwinoSettingsPaths.LoadSettings(const fileName: string);
begin
  with TIniFile.Create(fileName) do
  try
     EditPathToArduinoIDE.Text := ReadString('NewProject','PathToArduinoIDE', '');
     ComboBoxCOMPort.Text := ReadString('NewProject','COMPort', '');
     EditCodeTemplates.Text:= ReadString('NewProject','PathToCodeTemplates', '');
  finally
     Free;
  end;
end;

procedure TFormLamwinoSettingsPaths.SaveSettings(const fileName: string);
begin
  with TInifile.Create(fileName) do
  try
     WriteString('NewProject', 'PathToArduinoIDE', EditPathToArduinoIDE.Text);
     WriteString('NewProject', 'COMPort', ComboBoxCOMPort.Text);
     WriteString('NewProject', 'PathToCodeTemplates', EditCodeTemplates.Text);
  finally
     Free;
  end;
end;

end.

