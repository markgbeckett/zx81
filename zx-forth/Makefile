ASM = z80asm
AFLAGS = -L -o zx-forth.rom

ifeq ($(OS),Windows_NT)
	ERASE = erase
	else
	ERASE = rm
endif

SRC = zx-forth.asm
HEADERS = 

OBJS = $(SRC:.asm=.rom)

all: $(OBJS)

%.rom:	%.asm Makefile $(HEADERS)
	$(ASM) $(AFLAGS) $< > zx-forth.sym 2>&1

compare:
	fc /b zx-forth.rom "C:\opt\EightyOne V1.41\ROMs\Replacement Operating Systems\zx81-forth.rom"

clean:
	$(ERASE) $(OBJS)
