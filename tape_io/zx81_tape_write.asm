;; ZX81 mic signal controlled by two I/O operations:
;; - OUT 0xFF, A -- mic signal high (as well as end Vertical Retrace,
;;   restart Line Counter -- hence screen interference)
;; - IN 0xFE, A -- mic signal low (as well as reading keyboard --
;;   exploited to test for BREAK during tape operations)
;; 
;; See https://problemkaputt.de/zxdocs.htm#zx80zx81ioports
;; 
;; ZX81 tape interface described in Ian Logan's book "Understanding Your
;; ZX81 ROM" (Appendix 1, The SAVE Command)
;; 
;; - Core routine (which sends a byte to Mic socket) starts at 0x031E,
;;   which is basis for this routine ( see
;;   https://github.com/ZXSpectrumVault/rom-disassemblies/blob/master/Sinclair%20ZX81/Sinclair-ZX81.asm

	;; Generates a square wave, with wavelength of 505 (high) + 496 (low)
	;; The pulse is repeated 9 times for a '1' and 4 times for a zero.
	;;
	;; On a 3.25 MHz Z80, this gives:
	;; - wavelength of 0.308ms (frequency of 3.25 kHz).
	;; - duration of '0' pulse is 1.232 ms
	;; - duration of '1' pulse is 2.772 ms
	;;
	;; The gap between pulses is 4,902 (-496) T states = 1.356 ms
	org 0x6000 		; Start address TBC

WRITE_BYTE:
	ld e, (hl)		; Retrieve byte into E
	scf 			; Marker bit (ensures E non-zero until done)

NEXT_BIT:	
	rl e			; (8) Move next bit into Carry (note that
				; Carry will be zero except on first
				; iteration)
	ret z			; (11/5) Check if done
	sbc a,a			; (4) If Carry set, A=0xFF; otherwise A=0x00
	and 0x05		; (7)
	add 0x04		; (7) If Carry set, A=09; otherwise A=0x04
	ld c,a			; (4) Counter for bit signal
OUT_BIT:
	out (0xFF),a		; (11) Set mic signal high

	;; Timing = 7+35*13+8 = 470
	ld b, 0x23		; (7) Timing loop
LOOP1:	djnz LOOP1		; (13/8)

	;; Timing (up to end of OUT statement) = 35
	;; Timing (after OUT statement) = 64 (assuming BREAK not pressed)
	call TEST_BREAK		; (17) Also sets mic signal low
	jr nc, BREAK_EXIT	; (12/7) Exit, if BREAK detected

	;; Timing = 7+30*13+8 = 405
	ld b, 0x1E		; (7) Timing loop
LOOP2:	djnz LOOP2		; (13/8)

	;;  Timing if another loop = 16
	;;  Timing if complete = 11
	dec c			; (4) Wavelength counter
	jr nz, OUT_BIT		; (12/7) Repeat if not done

	;; Timing is 256*(4+13)+(4+8) = 4,364
LOOP3:	and a			; (4) Timing loop (on entry B=0x00)
	djnz LOOP3		; (13/8)

	jr NEXT_BIT		; (12) Note, Carry is clear at this
				; point

	;; On entry:
	;;
	;; On exit:
	;;   Carry reset = Break pressed; Carry set = No break
TEST_BREAK:
	ld a, 0x7F		; (7) 0x7FFE tests "Space", ".", "M", "N", "B"
	in a, (0xFE)		; (11)
	rra			; (4) Rotate Bit 0 into carry

DEBOUNCE:			; Not relevant, except for timing
	res 0,(iy+0x3B)		; (23)
	ld a, 0xFF		; (7)
	ld (0x4027), a		; (13) Debounce system varioable
	ret			; (10)
