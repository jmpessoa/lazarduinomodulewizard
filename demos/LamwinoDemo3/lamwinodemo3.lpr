program LamwinoDemo3; //Lamwino: Lazarus Arduino Module Wizard :: 2/15/2016 23:18:08

{$mode delphi}

uses
  LamwinoAvrSerial;

const
  PB5 = 1 shl 5; //Bit 5 in "PortB" control UNO Pin13 [internal LED]

var
  DelayVar: Integer = 0;
  ch: char;
  binStrBuffer: PChar;
  prompt: PChar;
  byteAsciiCode: byte;  //  [ <=255 ]!

procedure SomeDelay; //thanks to @ykot! ref. http://forum.lazarus.freepascal.org/index.php/topic,30960.0.html
var
  I: LongInt;
begin
  for I := 0 to 200000 {400000} do
    Dec(DelayVar);
end;

begin

  DDRB:= DDRB or PB5; //force DDRB bit 5 to 1 !!  [i.e,  signalize PORTB bit 5 [Pin13] as output]

  binStrBuffer:= '00000000';  // init/aloc buffer [dummy]
  prompt:= 'Please, Enter a char/digit:';

  Serial.Init(9600);

  while True do
  begin
     PORTB:= PORTB and (not PB5); //force PORTB bit 5 [Pin13]  to 0!
     SomeDelay;

     Serial.WriteLineBreak;

     Serial.WriteString(prompt);

     Serial.WriteLineBreak;

     ch:= Serial.ReadChar();

     Serial.WriteChar(ch);

     Serial.WriteLineBreak;

     Serial.WriteByteAsBinaryString(byte(ch));

     Serial.WriteLineBreak;

     byteAsciiCode:= Ord(ch); //char ascii code
     Serial.WriteByteAsDecimalString(byteAsciiCode);

     Serial.WriteLineBreak;

     //just test...
     Serial.ByteToBinaryString(byte(ch), binStrBuffer);
     Serial.WriteString(binStrBuffer);

     Serial.WriteLineBreak;

     PORTB := PORTB or PB5; //force PORTB bit 5 [Pin13] to 1!
     SomeDelay;
  end;
end.
