	;; Cathy's Program - to produce music on your ZX81. Taken from
	;; Toni Baker "Mastering Machine Code on Your ZX81", Chapter 12,
	;; page 110

	;; ROM routines (8K ROM)
KSCAN:	equ 0x02BB
FINDCHR: equ 0x07BD

	org 0x4082 		; Start of contents of REM statement at
				; beginning of BASIC program space
	
	;; Loop-up table for notes
NOTES: 	db 0x9B, 0x89, 0x73, 0x69
	db 0x00, 0x93, 0x7E, 0x00, 0x5E
	db 0x00, 0x3B, 0x31, 0x28, 0x24
	db 0x00, 0x00, 0x36, 0x2C, 0x00
	db 0x00, 0x00, 0x0F, 0x16, 0x1E
	db 0x00, 0x0A, 0x0C, 0x12, 0x1A
	db 0x00, 0x00, 0x00, 0x41, 0x4C
	db 0x00, 0x38, 0x3C, 0x46, 0x53

	;; Subroutine to create a precise-length pause
PAUSE:	ld a,b
HOLD:	dec a
	jr nz, HOLD
	ret

START:	call KSCAN		; Wait until a key is pressed
	ld b,h
	ld c,l
	ld d,c
	inc d
	jr z, START		; *** BUG : Was NZ ***
	call FINDCHR		; Find which key is being pressed
	ld de, NOTES-0x7E
	add hl, de
	ld b,(hl)		; Select note
	xor a
	cp b			; Check thqat this note is
	jr z,START		; not a "pause"

	;; Play this note
	in a, (0xFE)		; *** BUG : Was 0xFF ***
	call PAUSE
	out (0xFF), a
	call PAUSE
	jr START		; Go round loop again
END:	
