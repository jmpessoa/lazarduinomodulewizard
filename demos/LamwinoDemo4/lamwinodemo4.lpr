program LamwinoDemo4; //Lamwino: Lazarus Arduino Module Wizard :: 2/23/2016 22:07:46
 
{$mode delphi}
 
uses
  LamwinoAvrSerial;
 
//const
 
var
  DelayVar: Integer = 0;
  prompt: PChar;
  shortStr: shortstring;
  shortNumber: byte;
 
procedure SomeDelay; //by @ykot
var
  I: LongInt;
begin
  for I := 0 to 400000 do
    Dec(DelayVar);
end;
 
begin

  Serial.Init(9600);

  Serial.StringTerminator:= '\';   // [default] need by ReadString ..

  prompt:= 'Please, enter a [short]string terminated by backslash: ';

  Serial.WriteString('[ Lamwino [short] String Demo ]');
 
  Serial.WriteLineBreak;

  Serial.WriteString(prompt);
  Serial.WriteLineBreak;

  shortStr:= Serial.ReadString();
  Serial.WriteString(shortStr);

  Serial.WriteLineBreak;
  Serial.WriteLineBreak;

  while True do
  begin

     Serial.WriteString('Please, enter a [short < 255] decimal number terminated by backslash: ');
     shortNumber:= Serial.ReadDecimalStringAsByte();
     Serial.WriteByteAsDecimalString(shortNumber);  //try (shortNumber + 2) ... etc..
 
     SomeDelay;
     Serial.WriteLineBreak;
 
  end;
 
end.
