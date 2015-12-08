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

Next steps:
Proper RAM sizing/detection and testing.
Increasing RAM data bus to 32bits.  ROM to remain 8bit due to simplicity of flashing devices.
Floppy drive interface
PS/2 Keyboard
