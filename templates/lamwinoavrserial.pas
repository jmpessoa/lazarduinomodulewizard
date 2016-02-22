unit LamwinoAvrSerial;

{$mode delphi}

(*references
http://brittonkerin.com/cduino/lessons.html
http://www.delphitricks.com/source-code/math/convert_a_binary_number_into_a_decimal_number.html
http://www.swissdelphicenter.ch/torry/showcode.php?id=442
http://programmersheaven.com/discussion/389747/binary-to-decimal-and-decimal-to-binary-conversion
http://galfar.vevb.net/wp/2009/shift-right-delphi-vs-c/
https://sites.google.com/site/myfeup/avr/avr-gcc/usart
*)

interface

type

  TAvrSerial = record
    constructor Init(usart_baud: Cardinal);
    procedure WriteChar(c: char);
    procedure WriteByte(b: byte);
    function ReadChar(): char;
    function ReadByte(): byte;
    procedure Flush();
    procedure WriteString(PStr: PChar; len: integer);   overload;
    procedure WriteString(PStr: PChar);    overload;
    procedure WriteByteAsBinaryString(value: byte);
    function BinaryStringToByte(PBinaryString: PChar): byte;
    procedure ByteToBinaryString(value: byte; binStrBuffer: PChar);
    procedure WriteByteAsDecimalString(value: byte);
    procedure WriteLineBreak();
  end;

var
  Serial: TAvrSerial;

implementation


{8N1
9600 bits per second
8 data bits
No parity (you might see instead: E=even, O=odd)
1 stop bit
http://www.gammon.com.au/forum/?id=10894
}

constructor TAvrSerial.Init(usart_baud: Cardinal);
var
  USART_UBBR: byte;
begin
    USART_UBBR:= byte(16000000 div usart_baud div 16) - 1; //U2X0 = 0 --> Assyn normal mod
    // Enable receiver and transmitter
    UCSR0B := (1 shl RXEN0) or (1 shl TXEN0);

    //Set frame format to 8N1
    UCSR0C :=
        (3 shl UCSZ0) // 8 data bits   00000110
        or (0 shl UPM0)  // no parity bit
        or (0 shl USBS0); // 1 stop bit

    // Set baudrate
    UBRR0H :=  USART_UBBR shr 8;
    UBRR0L :=  USART_UBBR;

    UCSR0A:=UCSR0A or (1 shl TXC0); // clear existing transmits
end;

procedure TAvrSerial.WriteLineBreak();
var
  brline: char;
begin
    brline:= #10;
    // Wait if UDR is not free
    while( (UCSR0A and (1 shl UDRE0) ) = 0 ) do ;

    UCSR0A:= UCSR0A or (1 shl TXC0); // clear  txc flag

    // Transmit data
    UDR0:= byte(brline);
end;

procedure TAvrSerial.WriteChar(c: char );
begin
  while( (UCSR0A and (1 shl UDRE0) ) = 0 ) do ;

  UCSR0A:= UCSR0A or (1 shl TXC0); // clear txc flag

  // Transmit data
  UDR0:= byte(c);
end;

procedure TAvrSerial.WriteByte(b: byte);
begin
    // Wait if UDR is not free
    while( (UCSR0A and (1 shl UDRE0) ) = 0 ) do ;

    UCSR0A:= UCSR0A or (1 shl TXC0); // clear txc flag

    // Transmit data
    UDR0:= b;
end;

function TAvrSerial.ReadChar(): char;
var
  value: byte;
begin
  //Wait until a byte has been received
  value:= UCSR0A and (1 shl RXC0);  //Axxxxxxx and 1000 0000  = 0   <--->  A=0
  while value = 0  do                                      // <> 0  <--->  A=1
  begin
    value:= UCSR0A and (1 shl RXC0);
  end;

  //Return received data
  Result:= char(UDR0);
end;


function TAvrSerial.ReadByte(): byte;
var
  value: byte;
begin
  //Wait until a byte has been received
  value:= UCSR0A and (1 shl RXC0);  //Axxxxxxx and 1000 0000  = 0   <--->  A=0
  while value = 0  do                                      // <> 0  <--->  A=1
  begin
    value:= UCSR0A and (1 shl RXC0);
  end;

  //Return received data
  Result:= UDR0;
end;

procedure TAvrSerial.Flush();
var
  value: byte;
begin
  //Wait until a byte has been received
  value:= UCSR0A and (1 shl RXC0);  //Axxxxxxx and 1000 0000  = 0   <--->  A=0
  while value <> 0  do                                      // <> 0  <--->  A=1
  begin
    Self.ReadByte(); //empty...
    value:= UCSR0A and (1 shl RXC0);
  end;
end;

procedure TAvrSerial.WriteString(PStr: PChar; len: integer);
var
  i: integer;
begin
  for i:=0  to len-1 do
     Self.WriteChar( char(PStr[i]) );
end;

procedure TAvrSerial.WriteString(PStr: PChar);
var
  i, len: integer;
begin
  len:= Length(PStr);
  for i:=0  to len-1 do
     Self.WriteChar( char(PStr[i]) );
end;

function TAvrSerial.BinaryStringToByte(PBinaryString: PChar): byte;
var
  i: byte;
begin
  Result:= 0;
  for i:= 7 downto 0 do
  begin
    if PBinaryString[i] = '1' then
       Result:= Result + byte(1 shl (8 - i));
  end;
end;

procedure TAvrSerial.ByteToBinaryString(value: byte; binStrBuffer: PChar);
var
  index: byte;
begin
  for index:=8 downto 1 do
  begin
     if ( (value shr (index-1)) and $01) <> 0  then
        binStrBuffer[8-index]:= '1'
     else
        binStrBuffer[8-index]:= '0'
  end;
end;

procedure TAvrSerial.WriteByteAsBinaryString(value: byte);
var
  index: byte;
begin
  for index:=8 downto 1 do
  begin
     if ( (value shr (index-1)) and $01) <> 0  then
       Self.WriteChar('1')
     else
       Self.WriteChar('0');
  end;
end;

procedure TAvrSerial.WriteByteAsDecimalString(value: byte);
var
  t: byte;
begin
  t:= value mod 100;
  Self.WriteChar(char( ORD('0') + (value div 100) ));
  Self.WriteChar(char( ORD('0') + (t div 10)  ));
  Self.WriteChar(char( ORD('0') + (t mod 10)  ));
end;

end.

