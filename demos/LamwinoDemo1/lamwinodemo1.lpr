program LamwinoDemo1; //[thanks to @ykot] ref. http://forum.lazarus.freepascal.org/index.php/topic,30960.0.html
 
//uses
 
 
const
  PB5 = 1 shl 5; //Bit 5 in "PortB" control UNO Pin13 [internal LED]
 
var
  DelayVar: Integer = 0;
 
procedure SomeDelay;
var
  I: LongInt;
begin
  for I := 0 to 400000 do
    Dec(DelayVar);
end;
 
 
begin
 
  DDRB := DDRB or PB5; //force DDRB bit 5 to 1 !!  [i.e,  signalize PORTB bit 5 [Pin13] as output]
 
  while True do
  begin
 
     PORTB := PORTB and (not PB5); //force PORTB bit 5 [Pin13]  to 0!
     SomeDelay;
 
     PORTB := PORTB or PB5;         //force PORTB bit 5 [Pin13] to 1!
     SomeDelay;
 
  end;
 
end.
