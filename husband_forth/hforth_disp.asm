
	
DFILE: 	equ 0xF000
NEXT_DISP_ROUTINE: equ DFILE + 24*32
P_DFILE: equ DFILE + 24*32 + 2

	org 0x0000

RST_00:	out (0xFD),a		; Disable NMI generator
	ld sp, DFILE-2

	;; Initialise display buffer
	ld hl, DFILE
	ld de, DFILE+1
	ld (hl), "*"
	ld bc, 32*24-1
	ldir

	;; Check for shift (included only becuase of IN)
	ld a,0x7F		;
	in a,(0xFE)		; 

	out (0xFD),a		; Disable NMI Generator

	ld a, 0x1E		; ???
	ld i,a

	;; Set next display routine (bot not counter)
	ld hl, RUN_VSYNC
	ld (NEXT_DISP_ROUTINE),hl

	out (0xFE),a		; Enable NMI Generator
	ld hl,DFILE
	ld (P_DFILE),hl
	
MAIN_LOOP:
	jr MAIN_LOOP
	
	ds 0x0038-$		; Space

	;; 
	;; Maskable interrupt, used to handle main display
	;; 
INT:	dec c			; Decrement scan-line counter
	jr nz, I_NEXT_SCANLINE	; Skip forward if not done

	pop hl			; Retrieve return address

	dec b			; Decrement row counter

	ret z			; Exit interrupt, if done

	set 3,c			; Reset scan-line counter

I_EXEC_DISPLAY:
	ld r,a			; Reset refresh counter
	ei			; Enable interrupts

	nop			; Timing???
	nop

	jp (hl) 		; Execute next line

I_NEXT_SCANLINE:
	pop de			; Discard return address (start of current display row in HL)

	ret z			; Never satisfied, as always NZ

	jr I_EXEC_DISPLAY	; Continue to execute display line

	ds 0x0066-$		; Spacer

	;;
	;; NMI routine - used to produce top/ bottom borders
	;; 
NMI:	ex af, af'		; Retrieve timing counder

	dec a			; Check if done
	jr z, NMI_DONE

	ex af, af'		; Otherwise continue

	ret

NMI_DONE:			; 0x006C
	push hl			; Save register

	ld hl,(NEXT_DISP_ROUTINE) ; Proceed to next routine
	
	jp (hl)


	;; access from NMI routine, when counter decrements to zero
RUN_DISPLAY:
	;; Set up next display routine (bottom border)
	ld a, 0x49		; Set next NMI counter (was 4A)
	ex af, af'

	ld hl, RUN_VSYNC
	ld (NEXT_DISP_ROUTINE), hl

	;; Save remaining registers
	push af
	push bc
	push de

	ld bc,0x1809		; 24 rows and 8 scan lines +1
	ld hl,(P_DFILE)

	ld a,0xEA		; User to set refresh register

	halt			; Execute one more NMI

TB_OFF:	out (0xFD),a		; disable NMI Generator

	call RD_KERNEL		; Run display

BB_ON:	out (0xFE),a		; Enable NMI Generator

	;; Restore registers
	pop de
	pop bc
	pop af

	pop hl

	ret

RD_KERNEL:
	ld r,a			; Set refresh counter

	ld a, 0xDD		; Update default counter for subsequent
				; rows
	ei

	halt			; Wait for maskable interrupt. Note, the
				; code never returns from the maskable
				; interrupt. It returns to the calling
				; program (by dropping the first return
				; address)
RUN_VSYNC:
BB_OFF:	
	out (0xFD),a		; (11) Disable NMI Generator
VSYNC_ON:
	in a,(0xFE)		; (11) Turn on VSync

	;; Set up next display routine (top border)
	ld a, 0x1D		; (7) Set next NMI counter
	ex af, af'		; (4)

	ld hl, RUN_DISPLAY	; (10)
	ld (NEXT_DISP_ROUTINE),hl ; (16) 

	;;  Timing routine for VSync (needs to be 1,300 - 48 = 1,252). We have managed 8 + 13*(96-1) + 7 = 1,250
	ld b, 0x60		; (7)
VS_LOOP:
	djnz VS_LOOP 		; (13/8)

VSYNC_OFF:
	out (0xFF),a		; (11) Disable VSync
TB_ON:	out (0xFE),a		; Enable NMI Generator

	;; Balance stack and done
	pop hl

	ret
	
