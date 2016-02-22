
Lamwino: Lazarus Arduino Module Wizard: 
	Version 0.1		

	"A wizard to create Arduino loadable module (.hex) using Lazarus/Free Pascal"
		https://github.com/jmpessoa/lazarduinomodulewizard
		http://forum.lazarus.freepascal.org/index.php?topic=31513.msg201993

	Key Features:
		1.Builder for FPC avr/arduino cross compiler

		2.New Project option:  "Arduino [avr] Module"

		3.Install ".hex" module in Arduino device

	Author: 
		Jose Marques Pessoa
			jmpessoa_hotmail_com
			https://github.com/jmpessoa/lazarduinomodulewizard

	Acknowledgements [all teachings!]: 
              
		-Ygot, 
		-HatForCat
		-Engkin 
			ref. http://forum.lazarus.freepascal.org/index.php/topic,30960.msg201921.html#msg201921

Version 0.1 revision 02 - 22 Feb - 2016

	NEW! Added support to Serial [COM]

	NEW! LamwinoDemo3 [Serial demo] //<---- Please, use some "Serial Monitor" app to send and receive info...

	WARNING! Please, configure NEW "Path do code Templates"
		IDE menu "Tools" --> [Lamwino] Arduino Module Wizard --> Settings

	IMPROVED! Added support to commons Arduino chip

	IMPROVED! Usability

Version 0.1 revision 01 - 14 Feb - 2016

	NEW! Added support to Arduino Mega 2560

1. Prerequisites/Infrastructure

	Arduino IDE: [drives, binutils, toolchains, avrdude, serial monitor, etc...]
		ref. https://www.arduino.cc/en/Main/Software

	SlikSvn: command line SVN client
		ref. https://sliksvn.com/pub/Slik-Subversion-1.8.11-win32.msi	[windows]

	Optional: Serial-Monitor-Deluxe [Pure FPC/Lazarus app!!]	 [windows]
		ref. http://www.idogendel.com/en/products/serial-monitor-deluxe

2. Lamwino Install

	arduinonewprojectwizard.lpk
	lamwino_ide_tools.lpk     [..../ide-tools]

3. Lamwino Use

	3. 1. Settings:

		Lazarus IDE menu "Tools" ---> "[Lamwino] Arduino Module Wizard" -->  "Settings ..."

	3.2. Building FPC AVR/Arduino cross compiler

		Lazarus IDE menu "Tools" ---> "[Lamwino] Arduino Module Wizard" -->  "Build FPC Cross Arduino"	
	
			.Get FPC Source [if you do not have!]
				::Note: Path do "FPC Source Code" is mandatory! 
			.Build
			.Install

	3.3 New Project

		.Lazarus IDE select "Project" -> "New Project" 
		.Arduino Avr Module [Lamwino]

		.Path to projects folder  
		.Project Name
		.Target
		.Generad Code [to start]
			[x]Blinking     //<---- * LamwinoDemo1, thanks to @ykot!
		
		.OK

		.Save
	
		.Lazarus IDE menu "Run" ---> "Build"

		{-------------------------------------
		.check for usb/pc/arduino connection
		.check for arduino COM port used ***
		-------------------------------------}

		.Lazarus IDE menu "Run" ---> "[Lamwino] Install/Run"

		.Congratulations!!!  \o/ \o/ \o/ \o/
 
		***How to Detect Arduino used COM Port
		1.Windows 10 
			Start Menu
			Settings
			Devices
			Connected Devices

		2. All Windows:
			Controls Panel
			Device Manager
			Ports(COM & LPT)


	3.4 Using Demos [....\demos]

		.Go to project ".lpi" and [if needed] change: atmega328p [and avr5] 
		according to you arduino device [and compiler!]		

			<Item0 Name="AVRCHIP" Value="atmega328p"/>
			<CustomOptions Value="-Cpavr5 -Wpatmega328p -a"/>

		Note: atmega328p and avr5 are "Arduino Uno" compatibles!

4. References:

	.FPC/Lazarus Arduino Tutorial
		ref. http://forum.lazarus.freepascal.org/index.php/topic,30960.msg201921.html#msg201921

	.Arduino UNO Pinout Diagram
		ref. http://forum.arduino.cc/index.php?topic=146315.0

	.I/O Port Operations in AVR
		ref. http://maxembedded.com/2011/06/port-operations-in-avr/

	.Cduino: Arduino Programming with C and Make
		ref. http://brittonkerin.com/cduino/lessons.html		
		LamwinoDemo2  <<--- lesson3!

Thanks to All!

by jmpessoa
[jmpessoa[AT]hotmail[DOT]com]