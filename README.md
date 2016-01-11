# rhombus
A minimal m68k system utilizing the 68020 microprocessor 

Building upon the Motorola Application Note 1015 which can be found at the following address:
http://archive.retro.co.za/archive/computers/AppNotes/Motorola-AN1015.pdf

So far, there have been several typos which have been corrected within the original Motorola code, 
as well as one hardware fault in the included schematic.
As of https://github.com/zenmetsu/rhombus/commit/a4d3d180cdeede65c75aaab68ca234bd8a448fac   the code
is functional and will boot up to produce serial output.

The schematic within the application note is correct to the best of my knowledge with the exception
that on the MC68901, the TCO output must be tied externally to RC, and TDO needs to be tied to TC.
The provided schematic omitted these connections and the documentation for the multi-function peripheral
leaves much to be desired, leading one to suspect that the clock could be passed internally 
via configuration.

The system board currently consists of CPU, RAM, ROM, an MC68901 multifunction peripheral, and a Xilinx XC95108 CPLD.

RAM sizing and error checking is in place.  Monitor has the ability to analyze or change memory contents.
Secondary monitor written to expand functionality.  Register viewing/modification possible, as is vectored
selection of input/output devices.  Added NMI exception handler and trace/breakpoint functionality.

Second monitor now contains all of the functionality of the original.  I am currently attempting to migrate the system to a VME style bus, however I am debating using a standard VME bus with a non-standard 4x32 pin header.  The issue revolves around the feasibility of such a design due to the need for boards with more than 2 layers.

Next steps:

--Addition of an MC68681 DUART which will work as auxillary serial port as well as keyboard input--
Addition of FT245 USB FIFO for auxillary serial connection
S-Record load over serial
Increasing RAM data bus to 32bits.  ROM to remain 8bit due to simplicity of flashing devices.
Floppy drive interface
PS/2 Keyboard via intel N8042 microcontroller
