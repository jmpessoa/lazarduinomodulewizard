program LamwinoDemo2; {by Lamwino: Lazarus Arduino Module Wizard}

{
    Assumptions:
    - LED connected to PIN 13 [PortB bit 5]
    - Switch/PushButton connected to PIN 2 [PORTD bit 2]
    - ref. http://brittonkerin.com/cduino/xlinked_source_html/lesson3.c.html

    -My experiment assembly: https://od.lk/f/Ml8xMTIzODM1NDFf
}


var
  DelayVar: Integer = 0;

//This function simulates some kind of delay by looping. [Thanks to @ykot!]
procedure SomeDelay;
var
  I: LongInt;
begin
  for I := 0 to 400000 do
    Dec(DelayVar);
end;


const

  PB5 = 1 shl 5; //set Bit index 5 in "Port B" [Pin 13/LED]
  //00000001 shl 5 -->  00100000

  PD2 = 1 shl 2;  //set Bit index 2 in "Port D" [Pin 2]
  //00000001 shl 2 -->  00000100


var
  value: byte;

begin

  //http://urbanhonking.com/ideasfordozens/2009/05/18/an_tour_of_the_arduino_interna/
  //Bit 5 in "Set B" control Pin 13 [internal LED]
  //Turn ON the bit in DDRB corresponding to pin 13 [PorB bit 5] to configure it for "output"
  //Data Direction  --> output [ => 1]   //can be manipulated using PORTB
  DDRB:= DDRB or PB5;  //force DDRB bit 5 to 1 !!  [direction Pin13 is output] --> LED

  //Data Direction --> input [=> 0]
  DDRD:= DDRD and (not PD2); //(not PD2 = 11111011)  force DDRD bit 2 to 0 !!  [direction Pin2 is "input"]
  //The pins marked as input now has the capability to read the voltage level at that pin...

  //http://www.avr-tutorials.com/digital/about-avr-8-bit-microcontrollers-digital-io-ports
  //DDRx is an 8-bit register which stores configuration information for the pins of Portx.
  //Writing a 1 in the pin location in the DDRx makes the physical pin of that port
  //an output pin and writing a 0 makes that pin an input pin

  //PIND is an 8-bit register that stores the logic value, the current state,
  //of the physical pins on PortD.
  //So to read the values on the pins of PortD,
  //you read the values that are in its PIN register.

  while True do
  begin

    //NOTE: In input mode, when pull-up is enabled, default state of pin becomes = 1.
    //So even if you don’t connect anything to pin and if you try to read it, it will read as 1.
    //Now, when you externally drive that pin to zero
    //(i.e. connect to ground / or pull-down),only then it will be read as 0.

    //ref. http://www.elecrom.com/2008/02/12/avr-tutorial-2-avr-input-output/

    value:= PIND and PD2; //HOWTO read a pin ... [yyyyyByy and 00000100]: value <> 0 <----> B = 1 [on]
                          //                                              value  = 0 <----> B = 0 [off]
    if value <> 0 then
        PORTB:= PORTB or PB5            {set [internal] LED to ON }
    else
      	PORTB:= PORTB and  (not PB5);   {set [internal] led OFF}     //not PB5 --> 11011111

    SomeDelay;

  end;

end.
