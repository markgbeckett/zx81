	;; Cathy's Program - to produce music on your ZX81. Taken from
	;; Toni Baker "Mastering Machine Code on Your ZX81", Chapter 12,
	;; page 110

	;; Select 4K (OLDROM) or 8K ROM
OLDROM:	equ 0x01
	
	;; ROM references
	if OLDROM=1
KTABLE:	equ 0x006C
	else			; (or 0x006C)
KSCAN:	equ 0x02BB
FINDCHR: equ 0x07BD
KTABLE:	equ 0x007E		; (or 0x006C)
	endif

	;; Start of contents of REM statement at beginning of BASIC
	;; program space
	;; POKE 16403,10 (to avoid listing line 10)
	if OLDROM=1
	org 0x402B
	else
	org 0x4082 		
	endif
		
	
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

	;; Check for Shift-Zero
	call CHK_ESC
	ret z
	
	call FINDCHR		; Find which key is being pressed
TEST:	ld de, NOTES-KTABLE
	add hl, de
	ld b,(hl)		; Select note
	xor a
	cp b			; Check that this note is
	jr z,START		; not a "pause"

	;; Play this note
MARK:	in a, (0xFF)		; *** BUG : Was 0xFF ***
MARK2:	call PAUSE
	out (0xFF), a
	call PAUSE
	jr START		; Go round loop again

CHK_ESC:
	and a
	ld hl,0xFCEF
	sbc hl,bc
	ld a,h
	or l
	ret
	
	;; Local copies of keyboard handling routines are needed for the
	;; 4K ROM and are included below. For the 8K ROM, these routines
	;; exist in an accessible form in the ROM
	if OLDROM=1

KSCAN:	ld hl, 0xFFFF		; Initially, assume no keys pressed

	ld bc, 0xFEFE		; Address bottom-left half-row
	in a,(c)
	or 0x01			; Ignore Shift (for first half-row only)
KS_LOOP:
	;; Record if key pressed in current half-row
	or 0xE0			; Mask off upper three bits
	ld d,a			; Backup value to D
	cpl			; For bits 0--4, 0=not pressed and 1=pressed
	cp 0x01			; Check for A=0 (no key pressed)
	sbc a,a			; A = 0 (key)/ A = FF (no key)
	or b			; A = half-row identifier (key)/ A = FF
				; (no key)
	and l			; Update L to include half-row information
	ld l,a			; if key pressed

	;; Record key position within half-row
	ld a,h			; Store location (in half-row) of key(s)
	and d			; pressed
	ld h,a

	; Advance to next half-row
	rlc b			
	in a,(c)
	jr c, KS_LOOP		; Repeat if not done

	;; Capture Shift status into H (last scan is repeat of
	;; bottom-left half-row)
	rra
	rl h

	ret			; Done

FINDCHR:
	ld d,0x00
	sra b
	sbc a,a
	or 0x26
	ld l,0x05
	sub l
FC_LOOP:
	add a,l
	scf
	rr c
	jr c,FC_LOOP
	inc c
	ret nz
	ld c,b
	dec l
	ld l,0x01
	jr nz, FC_LOOP
	ld hl, KTABLE-1
	ld e,a
	add hl, de
	ret

	endif
END:	
