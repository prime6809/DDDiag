#
# Makefile for Dragon Data Diags
#
# 21/11/2004, P.Harvey-Smith.
#

AS=lwasm
ASFLAGS=-9 -r -I defs

ZIP=pkzip
RM=rm

all: dddiagd dddiagc

# Diag cart, Dragon
dddiagd: dddiag.asm
		$(AS) $(ASFLAGS) -DDragon -oroms/dddiagd.rom -llist/dddiag.lst dddiag.asm 

# Diag cart, Tandy CoCo
dddiagc: dddiag.asm
		$(AS) $(ASFLAGS) -DTandy -oroms/dddiagc.rom -llist/dddiagc.lst dddiag.asm 

clean:
		$(RM) -f roms/*.rom
		$(RM) -f list/*.lst
		
check:
		cmp -l roms/dddiagd.rom ../ROMS/DIAG.ROM
		
