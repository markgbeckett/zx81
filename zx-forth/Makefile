ASM = z80asm
AFLAGS = -L -o hforth.rom

ifeq ($(OS),Windows_NT)
	ERASE = erase
	else
	ERASE = rm
endif

SRC = hforth.asm
HEADERS = 

OBJS = $(SRC:.asm=.rom)

all: $(OBJS)

%.rom:	%.asm Makefile $(HEADERS)
	$(ASM) $(AFLAGS) $< > hforth.sym 2>&1

compare:
	fc /b hforth.rom "C:\opt\EightyOne V1.41\ROMs\Replacement Operating Systems\zx81-forth.rom"

clean:
	$(ERASE) $(OBJS)
