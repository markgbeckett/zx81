	;; Stripped down version of the Husband Forth ROM, for the ZX81,
	;; including only the code that is part of generation of the
	;; display.
	;;
	;; This is done in part to help understand how it works and in
	;; part to allow experiments and improvements to support for
	;; Husband Forth on other Z80 systems.
	;;
	;; Written by George Beckett - @markgbeckett
	;;
	;; Change log:
	;;   24/JUL/25 - First implementation
	

	;; Relevant system variables, related to display handling
DBUFFER:		equ 0xF000 		; Location of display
						; buffer
NEXT_DISP_ROUTINE: 	equ DBUFFER + 24*32 ; User to store address of
					    ; next display routine, used
					    ; at end of NMI-controlled
					    ; border generation
P_DBUFFER: 		equ DBUFFER + 24*32 + 2	     ; Pointer to start
						     ; of display buffer

	org 0x0000

RST_00:	out (0xFD),a		; Disable NMI generator
	ld sp, DBUFFER-2	; Set up stack

	;; Note: Code seems to expect Maskable Interrupt Mode 1, though
	;; this is never set, so runs with MI Mode 0.

	;; Initialise display buffer
	ld hl,DBUFFER
	ld (P_DBUFFER),hl
	
	;; Fill display buffer with asterisks
	ld hl, DBUFFER
	ld de, DBUFFER+1
	ld (hl), "*"
	ld bc, 32*24-1
	ldir

	;; Check for shift (included from H Forth ROM, only becuase it
	;; involves IN instruction)
	ld a,0x7F		; Keyboard buffer read at 0x7FFE
	in a,(0xFE)		; 

	out (0xFD),a		; Disable NMI Generator

	ld a, 0x1E		; Not sure what this does as IM2
	ld i,a			; not used, though I is used for upper
				; byte of memory address during refresh
				; cycle

	;; Set next display routine (will be VSync)
	ld hl, RUN_VSYNC
	ld (NEXT_DISP_ROUTINE),hl

	out (0xFE),a		; Enable NMI Generator. Note, H Forth
				; does not initially set countdown
				; timer. Typically A' contains 0x00 so,
				; initially, machine runs 256 NMI
				; cycles, before triggering VSync for
				; first proper display frame.

	;; Dummy loop which will simply run an infinite loop
MAIN_LOOP:
	jr MAIN_LOOP
	
	ds 0x0038-$		; Padding, so maskable interrupt routine
				; is located correctly for IM1

	;; 
	;; Maskable interrupt, used to produce row of main (character)
	;; display. Setup of the required parameters for this done in
	;; RUN_DISPLAY.
	;;
	;; On entry:
	;;   B  - number of character rows still to generate.
	;;   C  - number of scan lines in current character row still to
	;;        generate + 1.
	;;   A  - number of refresh cycles until will reach end of
	;;        display line (32 plus any cycles for preamble code).
	;;   HL - address of start of display row being executed.
	;;
	;;   On exit:
	;;        Return address is dropped from stack, so returns to
	;;        parent call (which will be main program)
INT:	dec c			; Decrement scan-line counter
	jr nz, I_NEXT_SCANLINE	; Skip forward if more scan lines to
				; produce

	pop hl			; Retrieve return address (next
				; character to execute in display
				; buffer)

	dec b			; Decrement row counter

	ret z			; Exit, if done

	set 3,c			; Reset scan-line counter to 8

	;;  Run display line
I_EXEC_DISPLAY:
	ld r,a			; Reset refresh counter
	ei			; Enable interrupts

	nop			; Assume these are for timing purposes
	nop

	jp (hl) 		; Execute next display line

I_NEXT_SCANLINE:
	pop de			; Discard return address as we need to
				; re-run current row (address still in
				; HL))

	ret z			; Never satisfied, as always NZ, so
				; assume this is timing-related

	jr I_EXEC_DISPLAY	; Continue to execute display line

	ds 0x0066-$		; Spacer to ensure NMI routine is
				; located correctly

	;;
	;; NMI routine - used to produce top/ bottom borders
	;;
	;; On entry:
	;;   A' - counter of how many NMI pulses still required for
	;;        current ly being generated border
	;;
	;; On exit:
	;;   A' - updated, if more NMI cycles required
	;; or
	;;   Advances to one of several NMI exit routines, depending on
	;;   which component of frame is being generated next
NMI:	ex af, af'		; Retrieve and decrement cycle counter
	dec a			

	jr z, NMI_DONE		; Move on to next phase of display, if
				; done

	ex af, af'		; Otherwise, restore counter and 

	ret			; return to calling program, with
				; maskable interrupt disabled (that is,
				; not using RETN here.

NMI_DONE: ; 0x006C
	push hl 		; Save register

	ld hl,(NEXT_DISP_ROUTINE) ; Proceed to next routine (either
	jp (hl)			  ; RUN_VSYNC or RUN_DISPLAY)


	;; Continuation of NMI cycle when top border has been
	;; generated. This code segment produces main character display,
	;; using the maskable interrupt.
	;;
	;; Note NMI Generator still active at this point, A and A' have
	;; been exchanged, and HL has been stacked.
RUN_DISPLAY:
	;; Set up next display routine (bottom border)
	ld a, 0x4A		; Set next NMI counter for bottom border
	ex af, af'

	ld hl, RUN_VSYNC	; Store address of next but one display
	ld (NEXT_DISP_ROUTINE), hl ; routine (VSync)

	;; Save remaining registers
	push af
	push bc
	push de

	ld bc,0x1809		; 24 rows and 8 scan lines + 1
	ld hl,(P_DBUFFER)	; Point to start of display buffer
				; (execution address in upper 32kB of
				; memory)

	ld a,0xEA		; User to set refresh register, so
				; sufficient display-buffer cells
				; executed before subsequent maskable
				; interrupt signalled (when bit 6 of R
				; register goes low)

	halt			; Execute one more NMI cycle. Not sure
				; why this happens: again, assume it is
				; timing related

TB_OFF:	out (0xFD),a		; Disable NMI Generator

	call RD_KERNEL		; Run display

BB_ON:	out (0xFE),a		; Enable NMI Generator, to kick off
				; generation of bottom border

	;; Restore registers (including HL, which was stacking in NMI
	;; routine)
	pop de
	pop bc
	pop af

	pop hl

	ret			; Return to main program: display
				; generation will continue, when next
				; NMI pulse is generated.

RD_KERNEL:
	ld r,a			; Set refresh counter

	ld a, 0xDD		; Update start value for refresh counter
				; based on time to execute a row of
				; display (plus preamble).
	
	ei			; Enable maskable interrupt (will be
				; triggered when bit 6 of R register
				; drops low) starting production of main
				; part of display.

	halt			; Wait for maskable interrupt. Note, the
				; code never returns from the maskable
				; interrupt. It returns to the calling
				; program (by dropping the first return
				; address)


	;; Continuation of NMI cycle when bottom border has been
	;; generated. This code segment produces VSync signal for around
	;; 400 microseconds while (for real H Forth code) reading
	;; keyboard.
	;;
	;; Note NMI Generator still active at this point, A and A' have
	;; been exchanged, and HL has been stacked.
RUN_VSYNC:
BB_OFF:	
	out (0xFD),a		; (11) Disable NMI Generator

VSYNC_ON:
	in a,(0xFE)		; (11) Turn on VSync

	;; Set up next display routine (top border)
	ld a, 0x1E		; (7) Set next NMI counter
	ex af, af'		; (4)

	ld hl, RUN_DISPLAY	; (10) Set next-but-one display step,
	ld (NEXT_DISP_ROUTINE),hl ; (16) which is main display

	;; In H Forth, the keyboard is read at this point, taking
	;;  1,304--1,310 T states. Here, we simply implement a wait
	;;  loop.
	;;
	;;  Timing routine for VSync (needs to be 1,300 - 48 =
	;;  1,252). We have managed 8 + 13*(96-1) + 7 = 1,250
	ld b, 0x60		; (7)
VS_LOOP:
	djnz VS_LOOP 		; (13/8)

VSYNC_OFF:
	out (0xFF),a		; (11) Disable VSync
TB_ON:	out (0xFE),a		; Enable NMI Generator (to initiate
				; production of the top border of the
				; display

	;; Balance stack and done
	pop hl

	;; In H Forth, routine services multi-tasking schedule, before
	;; returning to calling program.
	ret
