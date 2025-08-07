; z80dasm.1.6
; command line: z80dasm -g 0 -a -l -o hforth.asm husband_forth.bin


	;; Display mechanism
	;;
	;; As with Sinclair's BASIC ROM for the ZX81, much of the
	;; processors time is spent producing a display. For Husband
	;; Forth, this is done in four stages, as follows:
	;;
	;; 1 - A VSync signal is produced by a routine starting at
	;;     0x0098--0x0100. The VSync is activated by reading from
	;;     port 0xFE (any even port would do) and needs to be active
	;;     for 400 micro-seconds, during which time the Z80 is
	;;     available to do other work -- in this case, reading the
	;;     keyboard (though there isn't enough time to analyse the
	;;     keypress). The NEXT_DISPLAY_ROUTINE is set to content of
	;;     0xFC82, which -- on startup -- is 0x0071
	;;
	;;     Step 1 is triggered at the end of Step 4 (the NMI
	;;     routine, when finished, runs the NEXT_DISPLAY_ROUTINE)
	;;     which is set to 0x0098
	;; 
	;; 2 - Next a number of blank lines are displayed for the
	;;     top-border of the display (see code from 0x0100 to
	;;     0x0200). During this phase, the NMI mechanism is turned
	;;     on and the processor is interupted by an NMI signal every
	;;     64 microseconds to check if the upper board is done. A
	;;     counter in the A' register is initialised during Step 1
	;;     to have the value 0x1E, counts down to zero, at which
	;;     point the upper border is considered generated. Again,
	;;     during the generation of the upper border (when not
	;;     servicing the NMU signal), the Z80 is able to do other
	;;     things -- in this case, servicing the multi-tasking queue
	;;     mechanism and interpreting any key presses.
	;; 
	;; The system variable, NEXT_DISP_ROUTINE, is used to determine
	;; which is the next display routine to run. It will either
	;; point at 0x0098 or the content of 0xFC82 (either 0x0071 or
	;; ???)
	;;
	;; Interacting with the display:
	;;   OUT (FE),A - Enable NMI Generator (and enable HSync)
	;;   OUT (FD),A - Disable NMI Generator
	;;   IN A,(FE)  - Turn on Vsync
	;;   OUT (??),A - Turn off Vsync
	;;
	;; Role of 
	;;   NMI generated every 64 microseconds
	;;   R register increments (lower 7 bits) every instruction fetch
	;;   A6 is tied to Maskable Interrupt

	;;   Maskable interrupt is generated whenever A6 goes
	;;   low. Therefore, in normal operation it should be
	;;   disabled. The mechanism for triggering maskable interrupts
	;;   is when the the R register causes A6 to go low at the end
	;;   of executing a display line.
	;; 
	;; System variables
	;;
	;; FC40 - interrogated at end of restart
	;; FC54 - frame counter???
	;; FC56 - RAMSIZE (in 256k pages)
	;; FC57 - RAM-related
	;; FC5C - TIME - system clock (see Ch 15 of manual)
	;; FC68 - PERiod - tic limit at which clock resets (see Ch 15 of manual)
	;; FC6C - Start of RAM 
	;; FC6E - Pointer to DFILE - Set to BD00/ FD00 during
	;;        initialisation, based on memory test
	;; FC76 - Set to BD00/ FD00 during initialisation, based on memory test
	;; FC78 - Set to FF during restart, to indicate warm restart is
	;;        possible
	;; FC7C - Location of task scheduler ??? (Set to 0050 during restart)
	;; FC7E - Offset to most recently read entry in Keyboard Input Buffer
	;; FC7F - Offset to most recently  written value in keyboard input buffer
	;; FC80 - Next video handling routine
	;; FC82 - Particular video handling routine (0x57 or 0x71)
	;; FC84 - Pointer to character stack
	;; FC86 - BASE (16-bit base for processing numbers)
	;; FC88 - Counted string (two characters) to check for when
	;;        parsing token, potentially triggers extended
	;;        functionality?
	;; FC8A -
	;; FC8C - Default value for 0xFC8C (that is, 0x0AFB)
	;; FC8E - Init value of FC8A
	;; FC90 - Start of parameter stack
	;; FC94 - MSTACK0: Pointer to machine-stack 0 (grows down from FB80)
	;; FC96 - MSTACK1: Pointer to machine-stack 1 (FC3E)
	;; FC98 - Address of multi-tasking-related routine
	;; FC9A...FC9D/ FC9E...FCA1/FCA2...FCA5 are 4-byte buffers
	;; FCA5 - Flags (Bit 0 - ; Bit 6 - tested at 0937 ; Bit 7 - set at 1071
	;; FCAB - POSSIBLE_KEY
	;; FCAC - LAST_KEY

	;; FCAD - FLAGS(Bit 0 ; Bit 4 -
	;;        0 - Execution / Editor mode
	;;        1 - Set to prevent printing to editor window
	;;        2 - Set to prevent printing to console
	;;        4 - Machine stack in use (0/1)
	;;        5 - Switch machine stack lock

	;; FCAE - Screen info (editor)
	;;        0 - current col value
	;;        1 - current row value
	;;        2 - leftmost column (window)
	;;        3 - top-most row (window)
	;;        4 - rightmost column (window)
	;;        5 - bottommost row (window)
	;;        6 - XOR'ed with character before displaying ???
	;;        7 - blank character for screen
	;; FCB6 - Screen info (console)
	;;        +0 - current col value
	;;        +1 - current row value
	;;        +2 - leftmost column (window)
	;;        +3 - top-most row (window)
	;;        +4 - rightmost column (window)
	;;        +5 - bottommost row (window)
	;;        +6 - XOR'ed with character before displaying ???
	;;        +7 - blank character for screen
	;; FCBE - FLAGS2 , passed into main loop (context 0) as HL
	;;        0 - printer disabled/ enabled

	;; FCBF - Character count for current token in input string

	;; Memory map
	;; 
	;; FB80 - top of machine stack 0
	;; FB80--FBBF - Keyboard Input Buffer
	;; FBC0 - PAD
	;; FC3E - top of machine stack 1
	;; FCC0--FCFF -- Character stack (wraps around)
	;; FD00--FFFF - video RAM

TOP_BORDER_LINES:	equ 56	; H_FORTH uses 4Ah (30d) / better 54
BOT_BORDER_LINES:	equ 55	; H_FORTH uses 1Eh (74d) / better 55

VARS:		equ 0xFC40	; Start of system variables
RAM_SIZE:	equ 0xFC56	; Amount of RAM (in 256-byte pages)
RAM_START:	equ 0xFC6C	; Start of RAM
P_DBUFFER:	equ 0xFC6E	; Address of display buffer
PRINT_DRVR:	equ 0xFC74	; Address of printer driver
P_MTASK:	equ 0xFC7C	; Location of multitasking scheduler???
P_RUN_DISP:	equ 0xFC82	; Address of routine to produce main
				; display
P_STACKC:	equ 0xFC84	; Address of next entry in Char Stack
BASE:		equ 0xFC86	; Base for numerical calculations
START_OF_DICT:	equ 0xFC88	; Special string (used with expansion ROM)

START_OF_DICT_DEF:	equ 0xFC8C

STACKP_BASE:	equ 0xFC90	; Base offset of Char stack

MSTACK0:	equ 0xFC94
MSTACK1:	equ 0xFC96
POSSIBLE_KEY:	equ 0xFCAB
LAST_KEY:	equ 0xFCAC
FLAGS:		equ 0xFCAD
STACK0_BASE:	equ 0xFB80
PAD:		equ 0xFBC0	; Start of PAD
FLAGS2:		equ 0xFCBE	; Further flags
STACKC_BASE:	equ 0xFCC0
STACK1_BASE:	equ 0xFC3E
F_WARM_RESTART:	equ 0xFC78	; 0xFF indicates warm restart possible
NEXT_DISP_ROUTINE:	equ 0xFC80	
SCR_INFO_ED:	equ 0xFCAE
SCR_INFO_CO:	equ 0xFCB6
KIB_R_OFFSET:	equ 0xFC7E
KIB_W_OFFSET:	equ 0xFC7F
FLAGS3:		equ 0xFCA5	; Bits:
				;  6 - 0 = stop tasks on restart; 1 =
				;  continue multitasking
TOKEN_LN:	equ 0xFCBF	; Track length of currently being entered token

	org	00000h

	;;
	;; RST 00 routine (cold or warm restart)
	;; 
l0000h:	out (0xFD),a		;0000 - Disable NMI Generator
l0002h:	ld sp,STACK0_BASE	;0002 - Reset stack pointer
l0005h:	jp RESTART		;0005 - Continue with reset


	;; Push HL onto parameter stack
	;; IY points to parameter stack
sub_0008h:
	dec iy		;0008
l000ah:
	ld (iy+000h),h		;000a
l000dh:
	jr l004bh		;000d

	rst 38h			;000f

	;; Pop from Parameter Stack into HL
	;; IY points to parameter stack
l0010h:	ld l,(iy+000h)		;0010
	inc iy			;0013
	jr l0051h		;0015

	
	rst 38h			;0017

	;; RST 18 - Push A onto character stack
	push hl			;0018
	push af			;0019
	ld hl,P_STACKC		;001a
	jp l0201h		;001d


	;; RST 20h - Pop from character stack into A
l0020h:	push hl			;0020
l0021h:	ld hl,P_STACKC		;0021
	ld a,(hl)			;0024
	jp l020bh		;0025



	;; RST 28h 
	jp 0200ah		;0028 - Beyond end of standard ROM
				;       (possibly, floating-point
				;       handler?)
	
	nop			;002b
	nop			;002c
	nop			;002d
	nop			;002e
	nop			;002f

	;; RST 30h
	jp 0200dh		;0030 - Beyond end of standard ROM
				;       (possibly, floating-point
				;       handler?)
	
	nop			;0033
	nop			;0034
	nop			;0035
	nop			;0036
	nop			;0037

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
	;;
	;; Also, note the IOREQ signal enables the Hsync, which occurs
	;; at the second instruction of the routine (jp nz) and
	;; automatically disables the Hsync signal at the start of the
	;; fourth instruction, meaning a 20 T-state Hsync (see
	;; https://youtu.be/QPGE8JjC700?si=rvKLr70LLXWQUwNn for
	;; details).
	;; 
	;; Each scanline should be 64us (208 T states at 3.235 MHz)
INT:
	;; srl d		; (8) Timing

				; Hsync signal enabled

	dec c			; (4) Decrement scan-line counter

	jp nz, I_NEXT_SCANLINE	; (10) Skip forward if more scan lines
				; to produce (Use JP to ensure
				; consistent timing with/ without a
				; branch).

	pop hl			; (10) Retrieve return address (next
				; character to execute in display
				; buffer)

				; Hsync turned off by flip-flop circuit
	
	dec b			; (4) Decrement row counter

	ret z			; (11/5) Exit, if done

	set 3,c			; (8) Reset scan-line counter to 8 (used
				; instead of ld c,8, to ensure
				; consistent timing)

	;;  Run display line
I_EXEC_DISPLAY:
	ld r,a			; (9) Reset refresh counter
	ei			; (4) Enable interrupts

	nop			; (4) Included for timing, to pad
	nop			; (4) scanline to close to 64us
	
	jp (hl) 		; (4) Execute next display line

I_NEXT_SCANLINE:
	pop de			; (10) Discard return address as we need
				; to re-run current row (address still
				; in HL))
	
				; Hsync turned off by flip-flop circuit
	
	ret z			; (5) Timing-related: this is never
	 			; satisfied, so always adds 5 T states
	 			; to the code to delay start of HSYNC

	jr I_EXEC_DISPLAY	; (12) Continue to execute display line

	
	;; Continuation of PUSH_HL restart routine (RST 08)
l004bh:	dec iy			;004b
	ld (iy+000h),l		;004d

l0050h:	ret			;0050 - also used for null routine (just
				;       RET)

	;; Continuation of POP_HL routine (RST 10)
l0051h:	ld h,(iy+000h)		;0051
	inc iy			;0054
	
	ret			;0056

	;; Routine to transition between display modes
l0057h:	ld a,0ffh		;0057 - E4h in 60 Hz version
	ex af,af'		;0059
	ld hl,RUN_VSYNC		;005a - Set routine for video handling
	ld (NEXT_DISP_ROUTINE),hl	;005d

l0060h:	pop hl			;0060

	ret			;0061

	;; Padding???
	nop			;0062
	nop			;0063
l0064h:	nop			;0064

jump_to_hl:
	jp (hl)			;0065

	;;
	;; NMI routine - used to produce top/ bottom borders, while also
	;; running user code. When active, an NMI pulse is generated
	;; every 64 us (that is, every one scanline) plus triggers
	;; Hsync, so can be used to count scanlines in each border.
	;;
	;; On entry:
	;;   A' - counter of how many scanlines still required for
	;;        currently being generated border
	;;   (NEXT_DISP_ROUTINE) - address of routine to call, when countdown
	;;                    reaches zero
	;;
	;; On exit:
	;;   A' - updated, if more NMI cycles required
	;; or
	;;   Advances to one of several NMI exit routines, depending on
	;;   which component of frame is being generated next
	;;
	;; N.B. The Z80 takes 11 clock cycles to jump to this NMI
	;; routine on an interrupt
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
	ld a, BOT_BORDER_LINES+1	; Set next NMI counter for
					; bottom border
	ex af, af'

	;; Save remaining registers
	push af
	push bc
	push de

	ld bc,0x1809		; 24 rows and 8 scan lines + 1
	ld hl, RUN_VSYNC	; Store address of next but one display
	ld (NEXT_DISP_ROUTINE), hl ; routine (VSync)

	ld hl,(P_DBUFFER)	; Point to start of display buffer
				; (execution address in upper 32kB of
				; memory)
	;; set 7,h		; Switch address to upper memory
	
	ld a,0xEA		; Sets a pause before the main display
				; starts to execute. Used to set refresh
				; register on first iteration of main
				; loop in RD_KERNEL, for first maskable
				; interrupt. Tends to affect left-right
				; alignment of text.


	;; Resynchronisation step (to ensure that, whatever user code
	;; was running when the previous NMI occurred, the main display
	;; starts at the same point in the frame).
	halt			; Execute one more NMI. Not sure
				; why this happens: again, assume it is
				; timing related.

TB_OFF:	out (0xFD),a		; (11) Disable NMI Generator

	call RD_KERNEL		; (17) Run display buffer.

BB_ON:	out (0xFE),a		; (11) Enable NMI Generator, to kick off
				; generation of bottom border

	;; Restore registers (including HL, which was stacking in NMI
	;; routine)
	pop de			; (10)
	pop bc			; (10)
	pop af			; (10)

	pop hl			; (10)

	ret			; (10) Return to main program: display
				; generation will continue, when next
				; NMI pulse is generated.

RD_KERNEL:
	ld r,a			; (9) Set refresh counter

	ld a, 0xDD		; (7) Update start value for refresh
				; counter based on time to execute a row
				; of display (plus preamble). Value of
				; DC adds one more cycle at the end of
				; the row.
	
	ei			; (4) Enable maskable interrupt (will be
				; triggered when bit 6 of R register
				; drops low) starting production of main
				; part of display.

	halt			; (4) Wait for maskable interrupt. Note, the
				; code never returns from the maskable
				; interrupt. It returns to the calling
				; program (by dropping the first return
				; address)


	;; ----------------------------------------------------------------
	;; Video handling routine (VSync and read keyboard)
RUN_VSYNC:
BB_OFF:	out (0FDh),a		;0098 - Disable NMI Generator
VS_ON:	in a,(0FEh)		;009a - Turn on VSync

	;;
	;; Vsync duration is 11 + 32 + 1,255 + 11 = 1,310 (including final OUT)
	;;                or 1,304 (400 us) (if no key press)
	;; 		  or 1,312 if shifted key press
	;; 
	ld a,TOP_BORDER_LINES-1	; (7) 009c - Set counter for top margin,
				;     REPLACED 0x1E
	ex af,af'		; (4) 009e   and store for next NMI cycle.

	;; Set next display routine
	ld hl,(P_RUN_DISP)	; (16) 009f - Usually contains 0071h
	ld (NEXT_DISP_ROUTINE),hl ; (16)

	;; Timing = 42 T states for prep
	push bc			; (11) 00a5
	push af			; (11) 00a6

	;; Read keyboard
	ld hl,0x0000		; (10) 00a7 - Initialise buffer for key press
	ld bc,0xFEFE		; (10) 00aa - I/O port for first half-row
				;       (B,...,Shift). Single bit that
				;       is reset will also be used as
				;       flag to confirm when keyboard
				;       reading is finished

	;; Check keyboard one half row at a time, starting with FEFE ->
	;; FDFE -> FBFE -> F7FE -> EFFE -> DFFE -> BFFE -> 7FFE

	;; Timing: Typical iteration is 75 T states (or 78 T states, if key
	;; prssed). Final iteration is 70 T states or 73 T states). Assuming one key is pressed, timing will be 75*7 + 70 + 3 = 598 T states
l00adh:	in a,(c)		; (12) 00ad - Read keyboard

	or %11100000		; (7) 00af - Mask off top three bits
				;       (keyboard half-row only contains
				;       five keys)
	cpl			; (4) 00b1 - Complement, so any keys pressed
				;       will be set

	or a			; (4) 00b2 - Check if zero, meaning no
	jr z,l00b7h		; (12/7) 00b3 - key pressed

	set 0,l			; (8) 00b5 - L is used to confirm half-row in
				;       which key is detected
l00b7h:	rrc l			; (8) 00b7 - 8-bit rotation right (bit 0 also
				;       copied into carry

	or h			; (4) 00b9 - Save column value into H
	ld h,a			; (4) (keeping any previously detected
				;       key presses)

	rlc b			; (8) 00bb - Next half-row 
	jr c,l00adh		; (12/7) 00bd - Move on to next half row (unless
				;       carry is reset, which means eight
				;       half-rows have been scanned.,

	;; At this point, HL contains column and half-row identity of
	;; any key that has been pressed, respectively
	;;  1 - 2 - 3 - 4 - 5 -> register L = 08 and H=offset (from 1)
	;;  0 - 9 - 8 - 7 - 6 -> register L = 10 and H=offset (from 1)
	;;  Q - W - E - R - T -> register L = 20 and H=offset (from 1)
	;;  P - O - I - U - Y -> register L = 04 and H=offset (from 1)
	;;  A - S - D - F - G -> register L = 02 and H=offset (from 1)
	;; N/L- L - K - J - H -> register L = 40 and H=offset (from 1)
	;; SHF- Z - X - C - V -> register L = 01 and H=offset (from 1)
	;; SPC- . - M - N - B -> register L = 80 and H=offset (from 1)
	;; 
	;; H = 01, 02, 04, 08, 10 - based on position

	;; Check for Shift (L(0)=1 and H(0)=1
	;;
	;; Timing = 34 T states (or 37 T states, if shift pressed)
	ld a,l		; (4) 00bf - L=1, if shift half-row
	and h		; (4) 00c0 - H=1, if shift half-row
	rrca		; (4) 00c1 - Rotate bit 0 into carry

	ld bc,l0800h	; (10) 00c2 - B accounts for eight possible,
			;       half-rows; C is offset to character
	jr nc,l00c9h	; (12/7) 00c5 - Skip forward if Shift not pressed
	set 6,c		; (8) 00c7 - Add 64 to key value, if shifted

	;; Normalise H and L on zero (also drops shift)
	;;
	;; Timing (no multikey): 16 + 27 + 20 + 7 = 70
l00c9h:	srl h		; (8) 00c9 - Shift column value right
	srl l		; (8) 00cb - Shift row value right

	;; Check for multiple key presses (not including shift, which
	;; has been dealt with)
	ld a,h		; (4) 00cd - Load column reference into A
	neg		; (8) 00ce - 0-col/2
	and h		; (4) 00d0
	sub h		; (4) 00d1
	jr nz,l00d9h	; (12/7) 00d2 - Skip forward if multiple keypresses (in
			;same half-row)
	
	ld a,l		; (4) 00d4 - Retrieve row value
	neg		; (8) 00d5
	and l		; (4) 00d7
	sub l		; (4) 00d8

l00d9h:	jr nz,l00f4h	; (12/7) 00d9 - Skip forward if multiple keypresses

	;; Turn half-row indicator into index of offset of bit
	;;
	;; Timing = 32 * 7 + 27 + 1 = 252 T states (or 251, if no key press)
l00dbh:	rlc l		; (8) 00db - Check next bit
	jr nc,l00e0h	; (12/7) 00dd
 	add a,b		; (4) 00df - Set offset (will happen no more than
			;       once and will add current value of
			;       countdown)
l00e0h:	djnz l00dbh	; (13/8) 00e0

	;; Timing = 26 T states
	add a,c		; (4) 00e2
	ld c,a		; (4) 00e3 - C is half-row offset

	ld b,005h	; (7) 00e4 - Five possible key positions
	ld l,008h	; (7) 00e6 - Step size for each key position
	xor a		; (4) 00e8 - Key position in half-row gives
			;       8* offset (normalised on zero)

	;; Now work out offset for key press (in half row)
	;;
	;; Timing = 4*40 + 35 + 5 = 200 T states (or, 195, if no key press)
l00e9h:	add a,l		; (7) 00e9 
	rrc h		; (8) 00ea
	jr nc,l00f0h	; (12/7) 00ec
	add a,c		; (4) 00ee
	ld c,a		; (4) 00ef
l00f0h:	djnz l00e9h	; (13/8) 00f0

	;; At this point C contains offset for keypress
	;; Timing = 12 T states
	jr l00fah	; (12) 00f2 - Skip handling of multiple key
				;       presses

	;; Multiple key presses detected: implement pause to
	;; resynchronise with main loop
l00f4h:	ld b,022h	; (7) 00f4
l00f6h:	djnz l00f6h	; (13/8) 00f6
	set 7,c		; (8) 00f8 - Indicates multiple key presses

	;; At this point C points to key press (bit 6 indicates if shift
	;; if pressed, and bit 7 indicates multiple key presses)
l00fah:	ld hl,POSSIBLE_KEY	; (10) 00fa

	;; Check if no (real) keys pressed (that is, ignore Shift alone)
	;; Timing = 21 T states
	ld a,c		; (4) 00fd
	and %10111111	; (7) 00fe - Mask off shift

	;; *** End of VSYNC generation phase of display routine ***
l0100h:
VS_OFF:	out (0ffh),a	; (11) 0100 - Disable VSync REPLACE - was at 0x100
TB_ON:	out (0feh),a	; (11) 0102 - Enable NMI Generator

	jr nz,l0109h	; (12/7) 0104 - Jump forward if key other than
			;       shift pressed

	;; No real keys pressed, so set LAST_KEY to zero and skip
	;; forward
	ld (hl),a		;0106
	jr l0119h		;0107

	;; For real key presses, need to debounce
l0109h:	ld a,c			; (4) 0109 - Retrieve keypress into A
	bit 7,(hl)		; (14) 010a - Check if change of key detected on previous iteration
	jr nz,l0119h		; (12/7) 010c   Jump forward if so

	xor (hl)		;010e - Check if same key pressed as on previous iteration
	jr z,l011eh		;010f   Jump forward if so

	cp c			;0111 - Check if first detection of new
				;       key press, and no keypress on previous
				;       iteraton

	jr nz,l0117h		;0112 - Jump forward if different key from
				;       that previously detected

	ld (hl),c		;0114 - Otherwise, store key press
	jr l0119h		;0115

l0117h:	set 7,(hl)		;0117

	;; Multiple key presses
l0119h:	dec hl			; (6) 0119
	ld (hl),003h		; (10) 011a - Set countdown
	jr l012eh		; (12) 011c   and skip forward

	;; Handle key press
l011eh:	dec hl			;011e
	dec (hl)		;011f - Decrement debounce counter
	ld a,(hl)		;0120 
	and %00011111		;0121 - Check lower five bits
	jr nz,l012eh		;0123 - Jump forward if counter non-zero

	;; Check if need to test for long keypress on Space (for Cold Restart)
	bit 5,(hl)		;0125 - Is it Space key?
	jr z,l012bh		;0127 
	ld (hl),024h		;0129 - Related to long Break 

	;; Debounce counter is zero, so process key press
l012bh:	inc hl			;012b - HL -> LASTKEY
	inc hl			;012c - HL -> INKEY
	ld (hl),c		;012d - Store keypress

	;; Handle multitasking
l012eh:	ld hl,(0fc70h)		;012e
	push hl			;0131
	jr l0139h		;0132

l0134h:	ex (sp),ix		;0134
l0136h:	pop ix		;0136
	pop hl			;0138

l0139h:	push hl			;0139
	ld a,(hl)		;013a
	inc hl			;013b
	ld h,(hl)		;013c
	ld l,a			;013d
	xor a			;013e
	ex (sp),hl		;013f
	cp h			;0140
	jr z,l0178h		;0141
	inc hl			;0143
	inc hl			;0144
	ld bc,004ffh		;0145
	push hl			;0148
l0149h:
	cp (hl)			;0149
	jr nz,l016bh		;014a
	inc hl			;014c
	djnz l0149h		;014d
l014fh:
	ex (sp),ix		;014f
	ld b,004h		;0151
l0153h:
	ld a,(ix+004h)		;0153
	ld (ix+000h),a		;0156
	inc ix			;0159
	djnz l0153h		;015b
	inc c			;015d
	jr z,l0136h		;015e
	bit 6,(ix+004h)		;0160
	jr nz,l0136h		;0164
	inc (ix+004h)		;0166
	jr l0136h		;0169
l016bh:
	pop hl			;016b
	push hl			;016c
	ld bc,l0400h		;016d
l0170h:
	inc (hl)		;0170
	jr nz,l0134h		;0171

	inc hl			;0173
	djnz l0170h		;0174
	jr l014fh		;0176

	
l0178h:	pop hl			;0178
l0179h:	pop hl			;0179
	push hl			;017a
	ld a,(hl)		;017b
	inc hl			;017c
	ld h,(hl)		;017d
	ld l,a			;017e
	ex (sp),hl		;017f
	ld a,l			;0180
	add a,00ah		;0181
	ld l,a			;0183
	ld a,000h		;0184
	adc a,h			;0186
	ld h,a			;0187
	or a			;0188
	jr z,l01dbh		;0189 - Move on to check for keypress
	
l018bh:
	ld a,(hl)		;018b
	bit 7,a			;018c
	jr nz,l01dbh		;018e
	bit 6,a			;0190
	jr nz,l0179h		;0192
	or a			;0194
	jr z,l0179h		;0195
	dec (hl)		;0197
	set 7,(hl)		;0198
	push hl			;019a
	push de			;019b
	exx			;019c
	push hl			;019d
	push de			;019e
	push bc			;019f
	push ix		;01a0
	exx			;01a2
	inc hl			;01a3
	ld a,(hl)			;01a4
	inc hl			;01a5
	ld h,(hl)			;01a6
	ld l,a			;01a7
	push hl			;01a8
	ld hl,FLAGS		;01a9
	bit 4,(hl)		;01ac
	jr nz,l01b6h		;01ae
	pop hl			;01b0
	call jump_to_hl		;01b1
	jr l01cfh		;01b4

l01b6h:	res 4,(hl)		;01b6
	set 5,(hl)		;01b8
	ex (sp),hl		;01ba - Extract return address into HL
	
	;; Switch from Stack 1 to Stack 0
	ld (MSTACK1),sp		;01bb 
	ld sp,(MSTACK0)		;01bf

	call jump_to_hl		;01c3
	
	ld sp,(MSTACK1)		;01c6 - Switch back to Stack 1 (discard
				;Stack 0 pointer)

	pop hl			;01ca
	res 5,(hl)		;01cb
l01cdh:	set 4,(hl)		;01cd

l01cfh:	pop ix		;01cf
	pop bc			;01d1
	pop de			;01d2
	pop hl			;01d3
	exx			;01d4
	pop de			;01d5
	pop hl			;01d6
	res 7,(hl)		;01d7
	jr l018bh		;01d9

l01dbh:	pop hl			;01db

	;; Interpret key presses
	ld hl,LAST_KEY		;01dc
	ld a,(hl)		;01df

	;; Check if have handled this key already and return, if so
	bit 7,a			;01e0
	jr nz,l01fdh		;01e2

	;; Confirm now handled
	set 7,(hl)		;01e4

	push hl			;01e6

	;; Retrieve ASCII character corresponding to keypress
	ld hl,KEY_CODES		;01e7 - Compute offset
	add a,l			;01ea
	ld l,a			;01eb
	ld a,(hl)		;01ec - Retrieve code

	;; Check if Shift-Space pressed
	bit 7,a			;01ed
	jp nz,l094dh		;01ef - Warm restart

	ld hl,(0fca6h)		;01f2 - $04CF for Execution Context/
				;       Editor Context
	call jump_to_hl		;01f5

	pop hl			;01f8

	jr nc,l01fdh		;01f9 - Skip forward, if key successfully processed
	res 7,(hl)		;01fb - Otherwise, reset to try again next time

l01fdh:	pop af			;01fd - End of RUN_VSYNC routine
	pop bc			;01fe
	pop hl			;01ff

l0200h:	ret			;0200


	;; Continuation of PUSHC_A (rst 18)
l0201h:	ld a,(hl)		;0201
	dec a			;0202
	or 0c0h			;0203
	ld (hl),a		;0205
	ld l,a			;0206
	pop af			;0207
	ld (hl),a		;0208
	pop hl			;0209

	ret			;020a

	;; Continuation of POPC_A (RST 20)
l020bh:	ld l,a			; Move character-stack pointer
				; into L to construct 16-bit address
				; (high byte is always FCh)
	inc a			;020c - Increase character stack pointer
	or 0c0h			;020d - Wrap around from FF to C0
	ld (P_STACKC),a		;020f - Store new pointer value
	ld a,(hl)		;0212 - Retrieve value from stack
	pop hl			;0213 - Done

	ret			;0214

	;; Move current row up by one row (part of screen-scroll)
	;;
	;; On entry:
	;;   B - screen width
	;;   C - current row
	;;   DE - display width
	;;   HL - pointer to destination row
	;;
	;; On exit:
	;;   A  - corrupted
SCR_ROW_SCROLL:
	push hl			;0215
	push de			;0216
	push bc			;0217

	;; Set HL to point to source row and DE to point to destination
	;; row
	ex de,hl		;0218
	add hl,de		;0219

	;; Copy current character to row above
l021ah:	ld a,(hl)		;021a 
	ld (de),a		;021b

	;; Advance to next source and destination position
	inc de			;021c
	inc hl			;021d

	djnz l021ah		;021e - Repeat if more characters

	;; Retrieve previous registers
	pop bc			;0220
	pop de			;0221
	pop hl			;0222

	ret			;0223

	;; Blank row of screen
	;;
	;; On entry:
	;;   A  - blank character to use
	;;   HL - pointer to location in screen buffer
	;;   B  - row length
	;;
	;; On exit:
	;;   All registers preserved
SCR_BLANK_ROW:
	push hl			;0224
	push bc			;0225

l0226h:	ld (hl),a		;0226
	inc hl			;0227
	djnz l0226h		;0228

	pop bc			;022a
	pop hl			;022b

	ret			;022c

	;; Retrieve offset of cursor location into current screen
	;;
	;; On entry:
	;;   IX - pointer to coordinates of current location
	;;
	;; On exit:
	;;   HL - offset
sub_022dh:
	push af			;022d - Save A

	ld a,(ix+001h)		;022e - Retrieve row coordinate and
	rrca			;0231   multiply by 32 (equiv of five
	rrca			;0232   rotate-left operations)
	rrca			;0233
	ld h,a			;0234 - Store row offset

	;; Work out column offset
	and 0e0h		;0235
	or (ix+000h)		;0237
	ld l,a			;023a

	;; Normalise high-byte of address
	ld a,h			;023b
	and 003h		;023c
	ld h,a			;023e
	
	pop af			;023f - Restore A
	
	ret			;0240 - Done

	;; Retrieve address of current screen
GET_SCR_ADDR:
	push ix			;0241
	push de			;0243

	;; Update IX to point to current-location information for screen
	ld de,0x0002		;0244
	add ix,de		;0247

	;; Retrieve address of current screen into DE (preserving HL)
	ex de,hl		;0249
	ld hl,(P_DBUFFER)		;024a
	ex de,hl		;024d

	call sub_022dh		;024e - Retrieve offset of current screen
	
	add hl,de		;0251 - Work out address
	
	pop de			;0252 - Restore registers
	pop ix			;0253

	ret			;0255

	;; Retrieve address of current screen location into HL
	;;
	;; On entry:
	;;   IX - points to screen information for current screen
	;;
	;; On exit:
	;;   HL - points to start of screen
GET_SCR_POSN:
	push de			;0256 - Save DE

	call GET_SCR_ADDR	;0257 - Retrieve start of current screen
				;       into HL?
	ex de,hl		;025a - Move to DE
	call sub_022dh		;025b - Retrieve offset to current
				;       screen location into HL
	add hl,de		;025e - Compute address of current
				;       screen location

	pop de			;025f - Restore DE

	ret			;0260 - Done

	;; Retrieve screen dimensions
	;;
	;; On entry:
	;;   IX - pointer to screen information
	;; 
	;; On exit:
	;;   BC - screen width and height or CF set if negative
	;;        dimensions
	;;   A  - corrupted
GET_SCR_SIZE:
	ld bc,l0000h		;0261 - Required adjustment (to screen
				;       location)
	ld a,(ix+004h)		;0264 - Retrieve rightmost column
	sub (ix+002h)		;0267 - Subtract leftmost column
	ret c			;026a - Return if negative
	
	ld b,a			;026b - Save screen width
	
	ld a,(ix+005h)		;026c - Retrieve bottom-most row
	sub (ix+003h)		;026f - Subtract top-most row
	ret c			;0272 - Return if negative

	ld c,a			;0273 - Save screen height

	inc b			;0274 - Adjust to give actual height
	inc c			;0275 - Adjust to give actual width

	ret			;0276

	;; Retrieve and invert character at cursor location
INVERT_CUR_CHAR:
	push af			;0277
	push hl			;0278

	call GET_SCR_POSN	;0279
	ld a,(hl)		;027c
	xor 080h		;027d
	ld (hl),a		;027f

	pop hl			;0280
	pop af			;0281

	ret			;0282

	call GET_SCR_SIZE	;0283
	ret c			;0286 - Return if zero screen
	
	call GET_SCR_ADDR		;0287

	ld de,l0020h		;028a
	ld a,(ix+007h)		;028d - Retrieve blank character for screen

l0290h:	call SCR_BLANK_ROW	;0290
	add hl,de		;0293
	dec c			;0294
	jr nz,l0290h		;0295
	call CR			;0297
	ld (ix+001h),000h	;029a
	ret			;029e

	;; Carriage return (no line feed)
CR:
	ld (ix+000h),000h		;029f
	ret			;02a3

	;; Deal with cursor moving off bottom of screen
SCROLL_SCRN:
	push hl			;02a4
	push de			;02a5
	push bc			;02a6

	call GET_SCR_SIZE	;02a7 - Retrieve screen size into BC
	jr c,l02c2h		;02aa - Done if screen is negative sized
	call GET_SCR_ADDR		;02ac

	ld de,l0020h		;02af - Set DE to width of display

	;; Scroll screen
l02b2h:	dec c			;02b2 - Rows to scroll is screen height-1
	jr z,l02bch		;02b3 - Jump forward if only one row on
				;       screen

	;; Move current row up by one
l02b5h:	call SCR_ROW_SCROLL	;02b5

	;; Advance to next row
	add hl,de		;02b8

	dec c			;02b9 - Repeat if more rows to scroll
	jr nz,l02b5h		;02ba

l02bch:	ld a,(ix+007h)		;02bc
	call SCR_BLANK_ROW	;02bf

l02c2h:	pop bc			;02c2
	pop de			;02c3
	pop hl			;02c4

	ret			;02c5

	
	push hl			;02c6 - NB Don't think this is ever
	push de			;02c7   executed
	push bc			;02c8

	;; Advance cursor to next row
LINE_FEED:
	call GET_SCR_SIZE	;02c9
	jr c,LF_DONE		;02cc - Skip forward if negative-sized screen
				; NB: Could be jr c, AC_DONE

	ld a,(ix+001h)		;02ce - Retrieve current row
	inc a			;02d1 - Advance to next row
	cp c			;02d2 - Compare to screen height
	jr c,LF_CONT		;02d3 - Jump forward if not off bottom
				;       of screen

	dec c			;02d5 - Set A to be last but one row
	ld a,c			;02d6   of screen

	push af			;02d7

	call SCROLL_SCRN	;02d8 - Scroll screen

	pop af			;02db

LF_CONT:
	ld (ix+001h),a		;02dc - Store new current row

LF_DONE:
	pop bc			;02df
	pop de			;02e0
	pop hl			;02e1

	ret			;02e2

	;; Advance current character position
ADVANCE_CUR:
	push hl			;02e3
	push de			;02e4
	push bc			;02e5

	call GET_SCR_SIZE	;02e6 - BC = width/ height
	jr c,AC_DONE		;02e9 - Done if zero-size screen
	
	ld a,(ix+000h)		;02eb - Get current column 
	inc a			;02ee - Advance right
	cp b			;02ef - Compare to screen width
	jr c,l02f8h		;02f0 - Jump forward if not past end of screen
	
	call CR			;02f2 - Reset current column

	jp LINE_FEED		;02f5 - Advance to next row and done

l02f8h:	ld (ix+000h),a		;02f8 - Store current column and done

AC_DONE:
	pop bc			;02fb
	pop de			;02fc
	pop hl			;02fd

	ret			;02fe

sub_02ffh:
	ld a,(ix+001h)		;02ff
	or a			;0302
	ret z			;0303
	dec (ix+001h)		;0304
	ret			;0307
	ld a,(ix+000h)		;0308
	or a			;030b
	ld b,a			;030c
	jr nz,l0317h		;030d
	call sub_02ffh		;030f
	nop			;0312
	call GET_SCR_SIZE		;0313
	ret c			;0316

l0317h:	dec b			;0317
	ld (ix+000h),b		;0318
	ret			;031b
	call GET_SCR_SIZE		;031c
	dec c			;031f
	ld (ix+001h),c		;0320
	ret			;0323

	;;
	;; Invert character at current cursor location
	;;
	;; On entry:
	;;   IX - screen info
	;;
	;; On exit:
	;;   A  - (new) character code at current screen location
	;;   All other registers preserved
SCR_INV_CUR:
	push af			;0324
	push hl			;0325

	call GET_SCR_POSN	;0326 - Retrieve screen location into HL

	ld a,(hl)		;0329 - Retrieve character

	;; Manipulate character
	and %00111111		;032a - Mask off bits 7 and 6
	xor (ix+006h)		;032c - Invert with 0x00

	ld (hl),a		;032f - Replace character

	pop hl			;0330
	pop af			;0331

	ret			;0332

	;; Print character to screen
	;;
	;; Handles both printable and non-printable characters
	;;
	;; On entry:
	;;   IX - screen information
	;;   A  - character to be printed
	;; 
	;; On exit:
	;; 
SCR_PR_CHR:
	push af			;0333
	push hl			;0334
	push de			;0335
	push bc			;0336

	call SCR_INV_CUR		;0337 - (Un-)invert character at
				;	current screen location (as
				;	cursor highlight will move on)

	;; Convert character to 7-bit ASCII (lowercase letters replaced
	;; by uppercase)
	and 07fh		;033a - Mask off Bit 7 of character to
				;       print
	cp "`"			;033c - Is it < "£" symbol (same as
				;       ASCII `)
	jr c,SPC_CONT		;033e - Jump forward, if so

	add a,"A"-"a"		;0340 - Values in 0x60 -- 0x7F (i.e., £,
				;       a, b, ..., z, ... are shifted to
				;       0x40 -- 0x5F (note that Bit 7 of
				;       code already masked off). This
				;       effectively maps lower-case
				;       characters and less common ASCII
				;       symbols into upper-case letters
				;       and more common symbols.

SPC_CONT:
	cp " "			;0342 - Is it >= " " (printable
					;ASCII char)
	jr nc,SPC_PRINT_IT	;0344 - Jump forward if so

	;; Special character (0--1F)
	ld hl,SPECIAL_CHAR_TABLE ;0346 - Compute 1CC0+2*A, which is address
	add a,a			;0349   of routine to handle keypress
	add a,l			;034a
	ld l,a			;034b
	call JP_ADDR_HL		;034c - Call service routine 

SPC_DONE:
	call INVERT_CUR_CHAR	;034f - Reinstate cursor inversion

	pop bc			;0352
	pop de			;0353
	pop hl			;0354
	pop af			;0355

	ret			;0356

	;; Handle printable character (20--5F, with codes 60--7F shifted
	;; down to 40--5F)
SPC_PRINT_IT:
	add a,0e0h		;0357 - Shift character code to 00--3F
	call GET_SCR_POSN	;0359 - Get current screen location
				;       (into HL)
	xor (ix+006h)		;035c - Apply cursor mask

	ld (hl),a		;035f - Write character to screen

	call ADVANCE_CUR	;0360 - Advance cursor

	jr SPC_DONE		;0363 - Done

	;; Jump to address for to which HL points
	;; 
	;; On entry:
	;;   HL - location of address
	;;
	;; On exit
	;;   A corrupt
	;;   HL = HL+1
JP_ADDR_HL:ld a,(hl)		;0365
	inc hl			;0366
	ld h,(hl)		;0367
	ld l,a			;0368

	jp (hl)			;0369


	;; Insert/ delete character into/ from editor screen at current
	;; location
	;;
	;; On entry
	;;   A - character to insert
	;;   HL - current cursor position
	;;   DE - next cursor position
	;;   BC - length of row to right of cursor
	;; 
sub_036ah:
	push hl			;036a
	push de			;036b
	push bc			;036c
	push hl			;036d

	;; Check if inserting/ deleting character
	or a			;036e
	sbc hl,de		;036f
	jr nc,l037eh		;0371 - Jump forward, if deleting

	;; Find end of current line (and store in DE)
	ex de,hl		;0373
	add hl,bc		;0374
	ex de,hl		;0375

	;; Find last character to keep on line
	pop hl			;0376
	add hl,bc		;0377

	;; Shift tail of line right
	dec hl			;0378
	dec de			;0379
	lddr			;037a

	jr l0381h		;037c - Done

	;; Shift tail of line left
l037eh:	pop hl			;037e

	ldir			;037f

l0381h:	pop bc			;0381
	pop de			;0382
	pop hl			;0383

	ret			;0384

	call GET_SCR_POSN	;0385
	call GET_SCR_SIZE	;0388
	ld a,b			;038b
	ld b,000h		;038c
	sub (ix+000h)		;038e
	dec a			;0391
	ld c,a			;0392
	jr z,l039bh		;0393
	push hl			;0395
	pop de			;0396
	inc hl			;0397
	call sub_036ah		;0398
l039bh:
	ld a,c			;039b
	add a,e			;039c
	ld l,a			;039d
	ld a,(ix+007h)		;039e
	ld (hl),a			;03a1
	ret			;03a2
l03a3h:
	ld a,(ix+003h)		;03a3
	push af			;03a6
	add a,(ix+001h)		;03a7
	ld (ix+003h),a		;03aa
	call SCROLL_SCRN	;03ad
	pop af			;03b0
	ld (ix+003h),a		;03b1
	ret			;03b4
sub_03b5h:
	push hl			;03b5
	ld hl,FLAGS		;03b6
	bit 0,(hl)		;03b9
	pop hl			;03bb
	ret			;03bc
sub_03bdh:
	push hl			;03bd
	push de			;03be
	push bc			;03bf
	call GET_SCR_SIZE		;03c0
	ld a,(ix+003h)		;03c3
	push af			;03c6
	ld a,(ix+005h)		;03c7
	ld (ix+003h),a		;03ca
	call GET_SCR_ADDR	;03cd
	pop af			;03d0
	ld (ix+003h),a		;03d1
	ld de,0ffe0h		;03d4
	jp l02b2h		;03d7
sub_03dah:
	ld a,(ix+003h)		;03da
	push af			;03dd
	add a,(ix+001h)		;03de
	ld (ix+003h),a		;03e1
	call sub_03bdh		;03e4
	pop af			;03e7
	ld (ix+003h),a		;03e8
	jp CR		;03eb
sub_03eeh:
	call sub_03b5h		;03ee
	ret z			;03f1
	call CR		;03f2
	call GET_SCR_POSN	;03f5
	push hl			;03f8
	ld hl,(P_DBUFFER)		;03f9
	ld de,l0200h		;03fc
	add hl,de		;03ff

l0400h:	ex de,hl		;0400
	pop hl			;0401
	call GET_SCR_SIZE		;0402
	ld c,b			;0405
	ld b,000h		;0406
	call sub_036ah		;0408
	ex de,hl		;040b
	ld b,c			;040c
	call sub_0411h		;040d
	ret			;0410

sub_0411h:
	push hl			;0411
	push bc			;0412

l0413h:	ld a,(hl)		;0413
	xor 080h		;0414
	ld (hl),a		;0416
	inc hl			;0417
	djnz l0413h		;0418
	pop bc			;041a
	pop hl			;041b
	ret			;041c

l041dh:	call sub_03b5h		;041d
	ret z			;0420
	call GET_SCR_SIZE		;0421
	ld hl,(P_DBUFFER)		;0424
	ld de,l0200h		;0427
	add hl,de			;042a
	call sub_0411h		;042b
	ex de,hl			;042e
	call CR		;042f
	call GET_SCR_POSN		;0432
	ex de,hl			;0435
	ld c,b			;0436
	ld b,000h		;0437
	call sub_036ah		;0439
	ld b,c			;043c
	call sub_0411h		;043d
	ret			;0440
	call sub_03b5h		;0441
	ret z			;0444
	call sub_03eeh		;0445
	jp l03a3h		;0448
	call sub_03b5h		;044b
	ret z			;044e
	call sub_03dah		;044f
	jp l041dh		;0452
	push hl			;0455
	ld hl,FLAGS		;0456
	bit 0,(hl)		;0459
	jr nz,l0478h		;045b
	set 0,(hl)		;045d
	bit 6,(hl)		;045f
	jr nz,l0476h		;0461
	set 6,(hl)		;0463
	ld l,0b9h		;0465
	ld (hl),011h		;0467
	ld a,011h		;0469
	call sub_04cfh		;046b
	ld a,00ch		;046e
	call sub_04cfh		;0470
	call SCR_PR_CHR		;0473

l0476h:	pop hl			;0476

	ret			;0477

l0478h:	res 0,(hl)		;0478
	pop hl			;047a

	ret			;047b

	;; Handle special character 0x1F
	;;  Flip Bit 7 of FLAGS
	push hl			;047c

	ld hl,FLAGS		;047d
	bit 7,(hl)		;0480
	set 7,(hl)		;0482
	jr z,l048bh		;0484
	res 7,(hl)		;0486
	call INVERT_CUR_CHAR		;0488

l048bh:	pop hl			;048b

	ret			;048c

sub_048dh:
	ld a,01fh		;048d
	ld hl,(0fca6h)		;048f

	jp (hl)			;0492


	;; Print character (inc. control chars) to screen and printer
	;; 
	;; On entry:
	;;   A = character to print
	;;
	;; On exit:
	;;   CF - reset = success ; set = nothing printed
PRINT_A:
	push hl			;0493 - Save HL
	
	ld hl,FLAGS2		;0494

	;; Check for special case of cursor flash (which is periodically
	;; inserted into KIB, during periods of inactivity). Skip over
	;; printer check, if so, as not needed.
	cp 0x1F			; 
	jr z,PA_CONT		; Jump forward if so, as not for
				; printer

	;; Check if printer is enabled, and jump forward if not
	bit 0,(hl)		;049b 
	jr z,PA_CONT		;049d

	;; Send current character to printer
	push hl			;049f
	push af			;04a0

	;; Push character ASCII code onto Parameter stack
	ld l,a			;04a1
	ld h,000h		;04a2
	rst 8			;04a4

	;; Send to printer
	ld hl,(PRINT_DRVR)	;04a5
	call jump_to_hl		;04a8

	pop af			;04ab
	pop hl			;04ac

	;; Skip screen printing, if FLAGS2(1)=1 or (FLAGS(2)=1 and not
	;; handling cursor flash).

	;; Handle character 0x1F or (FCBE)(0) being reset. At this point,
	;; HL points to FLAGS2 and A is character being handled
PA_CONT:
	bit 1,(hl)		;04ad - If FLAGS2(1) is set, then do not
	jr nz,PA_DONE_NP	;04af - print: Set carry flag and done

	ld hl,FLAGS		;04b1
	
	cp 0x1E			;04b4 Check for EDIT (character 0x1E)
	jr z,PR_PRINT_A		;04b6 Jump forward, if so

	bit 2,(hl)		;04b8 - If FLAGS(2) is non-zero, then do
				;not print
	set 2,(hl)		;04ba   

PA_DONE_NP:
	scf			;04bc

	jr nz,PA_DONE		;04bd

	;; Print character to console
PR_PRINT_A:
	push ix			;04bf

	ld ix,SCR_INFO_CO	;04c1 - Retrieve info for command window
	call SCR_PR_CHR		;04c5

	pop ix			;04c8

	or a			;04ca - CCF and
	res 2,(hl)		;04cb   FLAGS(2)=0

PA_DONE:
	pop hl			;04cd - Restore HL

	ret			;04ce

	
	;; Handle keypress?
	;; On entry, A contains ASCII code of key press
sub_04cfh:
	push hl			;04cf
	ld hl,FLAGS		;04d0 - Check entry mode
	bit 0,(hl)		;04d3
	scf			;04d5
	jr nz,l04e0h		;04d6 - Jump forward if editing mode

	ld hl,(0fca8h)		;04d8 - Likely 0x08f8 - Add to keyboard
				;       input buffer ???
	call jump_to_hl		;04db

l04deh:	pop hl			;04de
	
	ret			;04df

	;; Handle keypress in edit mode
l04e0h:	bit 1,(hl)		;04e0 - Check FLAGS(1) and skip printing if set
	set 1,(hl)		;04e2
	jr nz,l04deh		;04e4
	
	push ix			;04e6
	ld ix,SCR_INFO_ED	;04e8
	cp 020h			;04ec - Check for printable character
	jr c,l04fah		;04ee   and jump forward if not
	call sub_0511h		;04f0

l04f3h:	res 1,(hl)		;04f3
	pop ix			;04f5
	or a			;04f7
	jr l04deh		;04f8 - Done

	;; Handle control characters in EDIT mode
l04fah:	cp 0x0D			;04fa - Check for Enter
	jr nz,l0508h		;04fc   and jump forward if not
	call SCR_PR_CHR		;04fe
	ld a,00ah		;0501

l0503h:	call SCR_PR_CHR		;0503
	jr l04f3h		;0506

l0508h:	cp 018h			;0508 - Check for Shift-Q (Compile Line)
	jr nz,l0503h		;050a   and jump if not

	call sub_0539h		;050c

	jr l04f3h		;050f - Done

	;; Print character to editor window
sub_0511h:
	push hl			;0511
	push de			;0512
	push bc			;0513
	push af			;0514
	call SCR_INV_CUR	;0515
	call GET_SCR_POSN	;0518 - HL = screen position
	call GET_SCR_SIZE	;051b - B = width ; C = height
	ld a,b			;051e - Retrieve width
	ld b,000h		;051f
	sub (ix+000h)		;0521 - Current column value
	dec a			;0524
	ld c,a			;0525
	jr z,l052eh		;0526
	push hl			;0528
	pop de			;0529
	inc de			;052a
	call sub_036ah		;052b
l052eh:	call INVERT_CUR_CHAR		;052e
	pop af			;0531
	call SCR_PR_CHR		;0532

	pop bc			;0535
	pop de			;0536
	pop hl			;0537

	ret			;0538


	;; Shift-Q -- Compile line (editor mode)
sub_0539h:
	push hl			;0539
	push de			;053a
	push bc			;053b
	call SCR_INV_CUR	;053c
	call CR			;053f
	call GET_SCR_POSN	;0542
	call GET_SCR_SIZE	;0545
	dec b			;0548
	push hl			;0549
	ld a,b			;054a
	add a,l			;054b
	ld l,a			;054c
	jr nc,l0550h		;054d
	inc h			;054f
l0550h:
	ld a,(hl)			;0550
	and 07fh		;0551
	jr nz,l0568h		;0553
l0555h:
	dec hl			;0555
	ld a,(hl)			;0556
	and 07fh		;0557
	jr nz,l055dh		;0559
	djnz l0555h		;055b

l055dh:	pop hl			;055d
	call sub_0574h		;055e
	ld a,00dh		;0561
	call sub_0581h		;0563
	jr l056dh		;0566

l0568h:	pop hl			;0568
	inc b			;0569
	call sub_0574h		;056a

l056dh:	call INVERT_CUR_CHAR	;056d
	pop bc			;0570
	pop de			;0571
	pop hl			;0572
	ret			;0573
sub_0574h:
	xor a			;0574
	cp b			;0575
	ret z			;0576
l0577h:
	ld a,(hl)			;0577
	add a,020h		;0578
	call sub_0581h		;057a
	inc hl			;057d
	djnz l0577h		;057e
	ret			;0580
sub_0581h:
	push hl			;0581
	ld hl,(0fca8h)		;0582
	call jump_to_hl		;0585
	pop hl			;0588
	ret			;0589
sub_058ah:
	call sub_03b5h		;058a
	ret nz			;058d
	ld hl,FLAGS3		;058e
	bit 1,(hl)		;0591
	res 1,(hl)		;0593
	ret z			;0595
	ld b,00fh		;0596
	ld a,01eh		;0598
	call sub_05b5h		;059a
	ld a,001h		;059d
	call sub_04cfh		;059f
l05a2h:
	ld a,018h		;05a2
	call sub_04cfh		;05a4
	ld a,00ah		;05a7
	call sub_05b5h		;05a9
	djnz l05a2h		;05ac
	ld a,018h		;05ae
	call sub_05b5h		;05b0
	ld a,01eh		;05b3
sub_05b5h:
	call sub_04cfh		;05b5
	jp l08efh		;05b8


	;; Switch machine stack
	;;
	;; Husband Forth operates with two interoperating main loops (or
	;; contexts), each with their own stack. This routine switches
	;; between the two stacks.
	;; 
	;; NOTE: This means that the routine will not return to the
	;; calling program, but the equivalent calling routine from the
	;; alternate context. It will return to the calling routine only
	;; if the context is switched again
	;;
	;; On entry:
	;;   FLAGS - confirms reflects machine-stack status
	;; 
	;; On exit:
	;;   All registers preserved (though not flags)	
SWITCH_MSTACK:
	;; Save registers
	push hl			;05bb
	push de			;05bc
	push bc			;05bd
	push ix			;05be

	;; Stack change is controlled by two flags:
	;; - FLAGS(4) confirms which stack is currently in use
	;; - FLAGS(5) is used to lock out stack change (e.g., when
	;;   changing the stack to prevent inconsistent state)
	ld hl,FLAGS		;05c0

	;; Check if stack change is locked out and exit, if so
	bit 5,(hl)		;05c3
	jr nz,SMS_EXIT		;05c5

	;; Enable stack-change lock, in case an NMI or interrupt leads
	;; to another stack change
	set 5,(hl)		;05c7

	;; Check which stack is in use and switch
	bit 4,(hl)		;05c9
	jr z,SMS_01		;05cb - skip forward if Stack 0

	;; Switch from Stack 1 to Stack 0
	res 4,(hl)		;05cd - confirm switch to Stack 0
	ld (MSTACK1),sp		;05cf - Save Stack 1 pointer and switch
	ld sp,(MSTACK0)		;05d3   in Stack 0 pointer

	jr SMS_DONE		;05d7 - Skip forward

	;; Switch from Stack 0 to Stack 1
SMS_01:	set 4,(hl)		;05d9 - confirm switch to Stack 1
	ld (MSTACK0),sp		;05db - Save Stack 0 pointer and switch
	ld sp,(MSTACK1)		;05df   in Stack 1 pointer

	;; Remove stack-change lock
SMS_DONE:
	res 5,(hl)		;05e3

	;; Restore registers and done
SMS_EXIT:
	pop ix			;05e5
	pop bc			;05e7
	pop de			;05e8
	pop hl			;05e9

	ret			;05ea

	;; Startup routine
sub_05ebh:
	ld hl,FLAGS		;05eb
	res 4,(hl)		;05ee - Using Stack 0
	res 5,(hl)		;05f0 - Stack switch not in progress

	ld hl,C1_MAIN_LOOP	; Insert address of main loop at
				; head (05f2)
	ld (STACK1_BASE),hl	; of Stack 1 as return address

	ld hl,STACK1_BASE-8	; Set pointer to fourth word on
	ld (MSTACK1),hl		; machine stack 1. Will balance
				; the first time that we switch to
				; machine stack 1

	ret			;05fe

	;; Handle most recent Keyboard Input Buffer Entry
PARSE_KIB_ENTRY:
	cp 01eh			;05ff
	jp z,PRINT_A		;0601
	set 7,a			;0604 
	push hl			;0606 - Save KIB offset

	;; Check which context is active and, unless FLAG 3 is set,
	;; switch to Execution context
	ld hl,FLAGS		;0607
	bit 4,(hl)		;060a
	jr z,l061eh		;060c - Jump forward, if execution stack
	bit 3,(hl)		;060e
	set 3,(hl)		;0610
	jr nz,l061eh		;0612
	call SWITCH_MSTACK	;0614 - Change execution context
	res 3,(hl)		;0617
	or a			;0619
	bit 7,a			;061a
	jr z,l061fh		;061c
l061eh:
	scf			;061e
l061fh:
	pop hl			;061f

	ret			;0620


	;; Switch to Stack 1
	;;
	;; On entry:
	;;   Carry significant ???
	;;
	;; On exit:
	;;   Depends on what happens in Stack 1 context
SWITCH_TO_MSTACK1:
	ld a,(FLAGS)		; Check which machine stack is active
	bit 4,a		

	ret nz			; Nothing to do if Stack 1 active

	;; Know Stack 0 is active
	sbc a,a			; A = 0, if carry clear, otherwise A = 0xFF

	call SWITCH_MSTACK	; Switch stack
	
	ret			; 062b - Return once context has
				; switched back to Execution Stack

sub_062ch:
	push hl			;062c
	ld hl,FLAGS3		;062d
	set 0,(hl)		;0630
	call sub_06dah		;0632
	pop hl			;0635
	ret			;0636


	;; Push string onto character stack (in reverse order)
	;;
	;; On entry:
	;;   HL - pointer to countable string
	;;
	;; On exit:
	;;   Length of string on parameter stack
	;;   String (reversed) on character stack
	;;   AF - corrupted
PUSH_STRING:
	push hl			;0637 - Save address of string

	ld a,(hl)		;0638 - Retrieve string length and limit
	ld h,000h		;0639   to 63 bytes maximum
	and 03fh		;063b 
	ld l,a			;063d
	
	rst 8			;063e - Push length (that is, HL) onto Param stack

	jr z,l0650h		;063f - Jump forward if zero-length string

	pop hl			;0641 - Retrieve string address
	push hl			;0642

	push bc			;0643 - Save BC
	ld b,a			;0644 - Set B to length of string

	;;  Find address of last character of string
	add a,l			;0645 - Set HL to point to end of string
	ld l,a			;0646   taking account of potential carry-over
	jr nc,l064ah		;0647   from low byte to high
	inc h			;0649

	;;  Push string onto stack in reverse order, so can be pop'ed in right order
l064ah:	ld a,(hl)		;064a - Retrieve next character
	rst 18h			;064b - Push A onto character stack
	dec hl			;064c - Decrement pointer and repeat,
	djnz l064ah		;064d   if necessary

	pop bc			;064f - Restore registers
l0650h:	pop hl			;0650

	ret			;0651 - Done

	;; Print string (from character stack, based on length on
	;; parameter stack)
PRINT_STRING:
	push af			;0652 - Save registers
	push hl			;0653
	
	rst 10h			;0654 - Retrieve string length from
				;       parameter stack into HL
	ld a,l			;0655 - Max length of string is 63 characters
	and 03fh		;0656   (assumes H is zero)

	jr z,l0662h		;0658 - Exit routine, if empty string

	ld l,a			;065a - HL is length of string

l065bh:	rst 20h			;065b - Retrieve next character from
				;       Character Stack into A
	call PRINT_A		;065c - GOT THIS FAR

	dec l			;065f - Decrement counter and repeat,
	jr nz,l065bh		;0660   if necessary

l0662h:	pop hl			;0662 - Restore registers
	pop af			;0663

	ret			;0664 - Done


	;; Print String pointed to by HL register (via character stack)
	;;
	;; On entry:
	;;  HL - address of string
	;;
	;; On exit:
	;; 
PRINT_STR_HL:
	push af			;0665 - Save A

	call PUSH_STRING	;0666 - Push string onto character stack
	call PRINT_STRING	;0669

	pop af			;066c

	ret			;066d

sub_066eh:
	push hl			;066e
	ld hl,l1d60h		;066f
	call PRINT_STR_HL		;0672
	pop hl			;0675
	ret			;0676


	;; Convert a character into a digit, in current base
ATOI:
	and %01111111		;0677
	cp 060h			;0679 - Check for letter
	jr c,l067fh		;067b
	add a,0e0h		;067d - Subtract 0x20
l067fh:	add a,0d0h		;067f - Add 
	cp 010h			;0681
	ret c			;0683

	add a,0f9h		;0684

	ret			;0686
sub_0687h:
	push af			;0687
	push hl			;0688
	push de			;0689
	ex de,hl			;068a
	rst 10h			;068b
	ld a,l			;068c
	and 03fh		;068d
	ld (de),a			;068f
	jr z,l0699h		;0690
	ld l,a			;0692
l0693h:
	inc de			;0693
	rst 20h			;0694
	ld (de),a			;0695
	dec l			;0696
	jr nz,l0693h		;0697
l0699h:
	pop de			;0699
	pop hl			;069a
	pop af			;069b
	ret			;069c
sub_069dh:
	push hl			;069d
	push de			;069e
	push bc			;069f
	rst 10h			;06a0
	ex de,hl			;06a1
	rst 10h			;06a2
	ld a,(hl)			;06a3
	inc hl			;06a4
	jr l06b1h		;06a5

	;; Match string
	;;
	;; On entry:
	;; - HL - points to candidate match (counted string)
	;; - String on Character stack and string-length on Parameter
	;;   Stack
	;;
	;; On exit:
	;; - Z = match (or zero match string)/ NZ = no match
	;; - A corrupted
MATCH_STRING:
	push hl			;06a7
	push de			;06a8
	push bc			;06a9

	ex de,hl		;06aa - DE points to match string

	;; Retrieve length of token into A (should be one byte) via HL
	rst 10h			;06ab - Pop HL from Param stack
	rst 8			;06ac - Push HL to Param stack
	ld a,l			;06ad 

	ld hl,(P_STACKC)	;06ae - Retrieve character-stack current
				;       position

l06b1h:	and %0111111		;06b1 - Max 127-byte match
	ld b,a			;06b3 - Set loop counter to length of string
	ld a,(de)		;06b4 - Retrieve length of match string
	and %0111111		;06b5 - Max 127
	cp b			;06b7 - Check if strings are same length
	jr nz,MS_DONE		;06b8 - Skip forward, if not
	or a			;06ba - Check if match-string is zero-length
	jr z,MS_DONE		;06bb - Skip forward if 0

MS_LOOP:
	inc de			;06bd - Advance to next character
	ld a,(de)		;06be - Retrieve and ...
	cp (hl)			;06bf - ... compare to corresponding in string
	jr nz,MS_DONE		;06c0 - Exit if not same

	inc hl			;06c2 - Check again
	djnz MS_LOOP		;06c3 - Repeat for every character

MS_DONE:
	pop bc			;06c5
	pop de			;06c6
	pop hl			;06c7
	
	ret			;06c8

	;; Routine to handle user error (Part 2)
sub_06c9h:
	push hl			;06c9
	ld hl,l1d66h		;06ca
	call PRINT_STR_HL		;06cd
	call PRINT_A		;06d0
	ld a,020h		;06d3
	call PRINT_A		;06d5
	pop hl			;06d8

	ret			;06d9

sub_06dah:
	push hl			;06da
	ld hl,l1d34h		;06db - Message 02, 0D, 0A
	call PRINT_STR_HL	;06de
	pop hl			;06e1
	ret			;06e2


	;; Possibly, parse word (number) from input buffer into HL
sub_06e3h:
	call sub_0782h		;06e3
	ld a,055h		;06e6
	jp c,l0878h		;06e8
	call sub_082ah		;06eb
	jp l0d6bh		;06ee


	;; Read a token into screen from keyboard
	;;
	;; Read token from keyboard, echoing key entries to screen
	;;
	;; On exit:
	;;   Token is on Character Stack (length on Parameter Stack)
	;;   A - corrupted
READ_TOKEN:
	push hl			;06f1
	
	ld hl,TOKEN_LN		;06f2 - Zero current-token length
	ld (hl),000h		;06f5

RT_GET_CHAR:
	or a			;06f7 - Clear carry for next subroutine

	call SWITCH_TO_MSTACK1	;06f8 - Activate Context 1 (read keyboard?)

	;; At this point, A contains most recent entry in KIB + 0x80
	and %01111111		;06fb - Mask off bit 7

	cp 0x1E			; Check if control character (other than
	jr c,RT_CTRL		; EDIT (1E) or FLASH (1F): branch if so

	call PRINT_A		;0701 - Print character

	cp 020h			;0704 - Check for SPACE (triggers parser
				;       in console ???)
	jr z,RT_PARSE_TOKEN	;0706 - Parse token
	
	call nc,STR_ADD_CHR	;0708

	jr RT_GET_CHAR		;070b - Loop

RT_PARSE_TOKEN:
	ld a,(hl)		;070d - Check token length
	or a			;070e
	jr z,RT_GET_CHAR	;070f - Loop, if zero (as initially)

RT_DONE:
	call PUSH_STRING	;0711

	pop hl			;0714 - Balance stack

	or a			;0715 - Reset carry

	ret			;0716 - Token read, so done

	;; Parse control character (0x00--0x1D)
RT_CTRL:
	cp 00dh			;0717 - Check for carriage return
	jr nz,RT_CONT		;0719 - Jump forward, if not
	call sub_062ch		;071b
	jr RT_DONE		;071e

RT_CONT:
	cp 01bh			;0720 - Check for ???
	jr z,l072eh		;0722 - Jump forward, if so
	call STR_ADD_CHR	;0724
	ld a,0x2E		;0727 - Display as "."

l0729h:	call PRINT_A		;0729
	jr RT_GET_CHAR		;072c

l072eh:
	ld a,(hl)		;072e
	or a			;072f
	jr z,RT_GET_CHAR	;0730
	dec (hl)		;0732
	ld a,008h		;0733
	jr l0729h		;0735

	;; Add character to string
STR_ADD_CHR:
	push hl			;0737
	push af			;0738

	;; Increment token length
	ld a,(hl)		;0739 - Retrieve current length of token 
	inc a			;073a   and increment (for new character)
	and 03fh		;073b - Max length is 63 characters
	ld (hl),a		;073d

	;; Move to character position in buffer
	add a,l			;073e
	ld l,a			;073f

	;; Retrieve and store character
	pop af			;0740
	ld (hl),a		;0741
	pop hl			;0742

	ret			;0743

sub_0744h:
	ld hl,(02008h)		;0744
	ld (0fc98h),hl		;0747
	ld hl,(02006h)		;074a
	ld (0fc8ah),hl		;074d

	;; Deal with recognised ROM
l0750h:	ld hl,(02004h)		;0750 - Jump address in ROM
	ld (START_OF_DICT),hl		;0753

sub_0756h:
	push hl			;0756
	ld hl,(START_OF_DICT)		;0757
	ld (START_OF_DICT_DEF),hl		;075a
	ld hl,(0fc8ah)		;075d
	ld (0fc8eh),hl		;0760
	pop hl			;0763

	ret			;0764

sub_0765h:
	push hl			;0765
	ld hl,(START_OF_DICT_DEF)		;0766
	ld (START_OF_DICT),hl		;0769
	ld hl,(0fc8eh)		;076c
	ld (0fc8ah),hl		;076f
	pop hl			;0772
	ret			;0773


sub_0774h:
	push de			;0774
	ex de,hl			;0775
	ld hl,(START_OF_DICT)		;0776
	or a			;0779
	sbc hl,de		;077a
	call sub_07c4h		;077c
	ex de,hl			;077f
	pop de			;0780
	ret			;0781

sub_0782h:
	call READ_TOKEN		;0782

CHECK_FOR_WORD:
	;; Check if token in string buffer corresponds to Forth word in
	;; dictionary.
	;;
	;; On exit:
	;;   HL - matching word
	;;   CF - reset (matched) / set (no match)
	ld hl,(START_OF_DICT)	;0785
CW_LOOP:
	call MATCH_STRING	;0788
	ret z			;078b - Return if matched (HL points to
				;       word)

	;; Advance to next word (pointer in HL)
	call FIND_NEXT_WORD		;078c

	;; Check if end of dictionary (if H=0)
	xor a			;078f
	cp h			;0790
	
	jr nz,CW_LOOP		;0791

	scf			;0793

	ret			;0794

	;; Advance to next word (in dictionary)
	;;
	;; On entry:
	;;  HL - address of current word
	;;
	;; On exit:
	;; 
FIND_NEXT_WORD:
	push de			;0795

	;; Move address of word into HL
	ld d,h			;0796 
	ld e,l			;0797

	;; Retrieve word-name-length
	ld a,(de)		;0798 
	and 03fh		;0799

	;;  Advance HL to word-length field
	add a,l			;079b
	ld l,a			;079c
	jr nc,l07a0h		;079d - Skip forward if no carry
	inc h			;079f
l07a0h:	inc hl			;07a0

	;; Advance to next word
	ld a,(hl)		;07a1
	inc hl			;07a2
	ld h,(hl)		;07a3
	ld l,a			;07a4
	add hl,de		;07a5

	pop de			;07a6
	
	ret			;07a7

sub_07a8h:
	ld hl,(0fc8ah)		;07a8
	call sub_0d8dh		;07ab
	call sub_0687h		;07ae
	ex de,hl			;07b1
	rst 10h			;07b2
	add hl,de			;07b3
	inc hl			;07b4
	ld (0fc8ah),hl		;07b5
	ret			;07b8
sub_07b9h:
	push hl			;07b9
	ld hl,(0fc8ah)		;07ba
	ld (hl),a			;07bd
	inc hl			;07be
	ld (0fc8ah),hl		;07bf
	pop hl			;07c2
	ret			;07c3
sub_07c4h:
	push af			;07c4
	ld a,l			;07c5
	call sub_07b9h		;07c6
	ld a,h			;07c9
	call sub_07b9h		;07ca
	pop af			;07cd
	ret			;07ce
sub_07cfh:
	ld a,0c3h		;07cf
	jr l07d5h		;07d1
sub_07d3h:
	ld a,0cdh		;07d3
l07d5h:
	call sub_07b9h		;07d5
	jp sub_07c4h		;07d8
sub_07dbh:
	ld a,021h		;07db
	call l07d5h		;07dd
	ld a,0cfh		;07e0
	jp sub_07b9h		;07e2
sub_07e5h:
	ld a,0c9h		;07e5
	jp sub_07b9h		;07e7


	;; Parse word entered at keyboard
	;;
	;; On entry:
	;;   Word is on parameter stack
	;;   HL - length of work
	;;
	;; On exit:
	;; 
PROCESS_TOKEN:
	push hl			;07ea Save token length

	call CHECK_FOR_WORD	;07eb - Search dictionary for matching word
	jr nc,l0805h		;07ee - Jump forward if match found (HL
				;       points to word)

	;; Attempt to parse as a number
	ld hl,(0xFC7A)		;07f0 - Contains 0A36
	call jump_to_hl		;07f3

	;; GOT THIS FAR
	jr nc,l0803h		;07f6 - Skip forward if done

	;; Deal with error?
	call sub_06dah		;07f8
	call PRINT_STRING	;07fb

	ld a,055h		;07fe
l0800h:	call sub_06c9h		;0800

PT_DONE:
	pop hl			;0803

	ret			;0804

	;; Process word (pointed to by HL)
l0805h:	call l0d6bh		;0805
	bit 7,(hl)		;0808
	push af			;080a
	call sub_082ah		;080b
	pop af			;080e
	jr nz,l0819h		;080f
	call jump_to_hl		;0811
	call sub_0756h		;0814

	pop hl			;0817

	ret			;0818

l0819h:	call jump_to_hl		;0819
	call sub_07e5h		;081c
	ld hl,(0fc8eh)		;081f
	call jump_to_hl		;0822
	call sub_0765h		;0825
	pop hl			;0828
	ret			;0829
sub_082ah:
	ld a,(hl)			;082a
	and 03fh		;082b
	bit 6,(hl)		;082d
	jr z,l0833h		;082f
	add a,002h		;0831
l0833h:
	add a,003h		;0833
	add a,l			;0835
	ld l,a			;0836
	ret nc			;0837
	inc h			;0838
	ret			;0839
	ld a,(hl)			;083a
	bit 6,a		;083b
	scf			;083d
	ret z			;083e
	and 03fh		;083f
	add a,003h		;0841
	add a,l			;0843
	ld l,a			;0844
	jr nc,l0848h		;0845
	inc h			;0847
l0848h:
	ld a,(hl)			;0848
	inc hl			;0849
	ld h,(hl)			;084a
	ld l,a			;084b
	ret			;084c


	;; Routine to handle user error (Part 1)
sub_084dh:
	push af			;084d - Save error code
	call sub_06dah		;084e
	call CDUP		;0851 - Duplicate string on character stack
	call PRINT_STRING	;0854

	pop af			;0857 - Retrieve error code
	
	jp sub_06c9h		;0858
	

sub_085bh:
	call sub_0782h		;085b
	ret c			;085e
	ld a,052h		;085f
	call sub_084dh		;0861
	or a			;0864
	ret			;0865
sub_0866h:
	ld hl,(0fc8ah)		;0866
	push hl			;0869
	call sub_085bh		;086a
	call sub_07a8h		;086d
	pop hl			;0870
	call sub_0774h		;0871
	ld (START_OF_DICT),hl		;0874
	ret			;0877

	;; Handle user error
	;;
	;; On entry:
	;;   A - error code
	;;
	;; On exit:
	;;   
l0878h:	call sub_084dh		;0878
	jp WARM_RESTART		;087b

l087eh:
	call sub_07d3h		;087e
l0881h:
	call sub_0782h		;0881
	jr c,l089ch		;0884
	call l0d6bh		;0886
	bit 7,(hl)		;0889
	push af			;088b
	call sub_082ah		;088c
	pop af			;088f
	jr z,l087eh		;0890
	ld de,l0878h		;0892
	push de			;0895
	call jump_to_hl		;0896
	pop de			;0899
	jr l0881h		;089a
l089ch:
	rst 10h			;089c
	xor a			;089d
	or l			;089e
	jr z,l0881h		;089f
	rst 8			;08a1
	ld hl,(0fc7ah)		;08a2
	call jump_to_hl		;08a5
	call sub_08adh		;08a8
	jr l0881h		;08ab
sub_08adh:
	bit 7,a		;08ad
	ld a,055h		;08af
	jp c,l0878h		;08b1
	jr z,l08bbh		;08b4
	rst 10h			;08b6
	call l08bbh		;08b7
	rst 8			;08ba
l08bbh:
	push hl			;08bb
	rst 10h			;08bc
	call sub_07dbh		;08bd
	pop hl			;08c0
	ret			;08c1

sub_08c2h:
	ld hl,FLAGS		;08c2 - Flags
	ld a,(hl)			;08c5
	and 070h		;08c6
	ld (hl),a			;08c8
	
	ld hl,FLAGS3		;08c9
	ld a,(hl)			;08cc
	and 0c0h		;08cd
	ld (hl),a			;08cf

	ld hl,FLAGS2		;08d0
	ld a,(hl)			;08d3
	and 0fch		;08d4
	ld (hl),a			;08d6

	ret			;08d7

	;; Kernel of context 0 loop (GOT THIS FAR)
C0_KERNEL:
	call READ_TOKEN		;08d8 - Read and parse token from
				;       keyboard
	rst 10h			;08db - Pop string length from Parameter
				;       stack into HL

	;; Check if L is zero
	xor a			;08dc - Token length zero?
	cp l			;08dd
	jr z,l08e4h		;08de

	rst 8			;08e0 - Push HL onto Parameter stack
				;       (restore previous state)
	
	call PROCESS_TOKEN	;08e1

l08e4h:	ld hl,FLAGS3		;08e4
	bit 0,(hl)		;08e7
	res 0,(hl)		;08e9

	call nz,sub_066eh	;08eb

	ret			;08ee

	;;	
l08efh:	call READ_FROM_KIB	;08ef
	ret c			;08f2
	call PARSE_KIB_ENTRY	;08f3

	jr l08efh		;08f6

	;; ================================================================
	;; Routines to handle Keyboard Input Buffer, which is a circular
	;; buffer, with capacity for 64 key presses, stored in
	;; FB80,...FBBF. Two offset pointers, KIB_R_OFFSET and
	;; KIB_W_OFFSET, track most recent buffer read and buffer write,
	;; respectively. Because offset pointers are stored in system
	;; variables with addresses of FCXX and because the pointer
	;; contents is typically accessed via HL, it is possible to
	;; change from an offset pointer to an address simply by
	;; decrementing H and depositing offset into L. For example:
	;;
	;;   ld hl,(KIB_R_OFFSET)
	;;   la a,(hl)
	;;   dec h
	;;   ld a,l
	;;
	;; This trick is when both writing to KIB and reading from it.
	;; 
	;; ================================================================
		
	;; ================================================================
	;; Write to KIB
	;;
	;; On entry:
	;;   A - character to write
	;;
	;; On exit:
	;;   Carry reset to indicate success
	;; ================================================================
sub_08f8h:
	push hl			;08f8 - Save register
	push af			;08f9 - Save key value

	ld hl,KIB_W_OFFSET	;08fa - Retrieve offset to current write
	ld a,(hl)		;08fd   location in KIB

	inc a			;08fe - Advance to next location in
	and %10111111		;08ff   buffer, wrapping around if
				;       necessary
	ld (hl),a		;0901 - Save new offset

	;; Compute address of new write location in KIB
	ld l,a			;0902
	dec h			;0903

	pop af			;0904

	ld (hl),a		;0905 - Write value
	or a			;0906 - Clear carry

	pop hl			;0907

	ret			;0908

	;; ================================================================
	;; Check if unread keypresses in Keyboard Input Buffer
	;;
	;; On entry:
	;;
	;; On exit:
	;;   Z -  reset ifnew data, set otherwise
	;;   HL - KIB_W_OFFSET
	;;   A -  current read offset in buffer
	;;
	;; ================================================================
CHECK_KIB:
	ld hl,KIB_R_OFFSET	;0909 - Retrieve read-offset pointer
	ld a,(hl)		;090c 
	inc hl			;090d - Advance to write-offset pointer
	cp (hl)			;090e - Compare, setting Z accordingly

	ret			;090f

	;; ================================================================
	;; Retrieve next (i.e., to be processed) value from KIB.
	;;
	;; On entry:
	;;
	;; On exit:
	;;   CF - reset if value read, set otherwise
	;;   A  - value read
	;;   HL - corrupted
	;; ================================================================
READ_FROM_KIB:
	call CHECK_KIB		;0910 - Compare KIB read-offset to
				;write-offset, setting zero according

	;; At this point, HL points to KIB_W_OFFSET and A contains
	;; content of KIB_R_OFFSET
	scf			;0913
	ret z			;0914 - Done if (FC7E) = (FC7F) (carry set)

	dec hl			;0915 - HL points to KIB_R_OFFSET
	inc a			;0916 - A set to offset of next byte in
	and %10111111		;0917   buffer (wrapping, if necessary)
	
	ld (hl),a		;0919 - Store new read offset

	;; Retrieve value from Keyboard Input Buffer
	ld l,a			;091a
	dec h			;091b 
	ld a,(hl)		;091c

	ret			;091d - Done

	
sub_091eh:
	add a,090h		;091e
	daa			;0920
	adc a,040h		;0921
	daa			;0923
	ret			;0924


	;; ================================================================
	;; Check for parameter stack underflow
	;;
	;; On entry:
	;;   IY - current top of Parameter stack (as per usual)
	;; 
	;; On exit:
	;;   DE, HL - corrupted
	;;   CF - reset, if no underflow
	;;   ERR - underflow
	;; ================================================================
CHECK_STACKP:
	ld hl,(STACKP_BASE)	;0925 - Retrieve address of top of
				;       parameter stack
	push iy			;0928 - Load current parameter stack location 
l092ah:	pop de			;092a   into DE

	or a			;092b - Subtract DE from HL 
	sbc hl,de		;092c

	ret nc			;092e - Return if DE <= HL -- that is,
				;       not a stack underflow

	ld a,053h		;092f - Deal with stack underflow
	jp l0878h		;0931


sub_0934h:
	ld hl,FLAGS3		;0934 - Check if multitasking is enabled
	bit 6,(hl)		;0937   (as set by TON)
	ret nz			;0939 - Return if so

	;; Disable multitasking?
	ld hl,l0050h		;093a - Points to RET statement 
	ld (P_MTASK),hl		;093d - Store for later use in
				;       jump_to_hl at $0A27

	call 01128h		;0940 - Service multitasking

	jp l106eh		;0943

	;; 
	;; Continuation of RST 0x00 routine
	;;
RESTART:
	;;  Check if Shift key is pressed (to force cold restart)
	ld a,07fh		;0946
l0948h:	in a,(0feh)		;0948 - Read keyboard half-row for
				;       'Shift', 'z', ..., 'v'
	rrca			;094a - Rotate 'shift key' into carry
	jr nc,COLD_RESTART		;094b - Branch, if shift pressed (to
				;       force cold restart)

	;; Check if warm restart is possible
l094dh:	ld hl,F_WARM_RESTART	;094d - Check if warm restart is
	inc (hl)		;       possible (if (FC78)=FF)
	jr z,WARM_RESTART		;0951 - Jump forward if warm restart

	;; Continuation of cold-restart routine
COLD_RESTART: im 1	   		; REPLACEMENT CODE ( as NMI generated
				; already disabled ).
	;; out (0fdh),a		;0953 - Disable NMI Generator

	;; 
	;; Check memory configuation of machine (i.e., how much memory
	;; there is)
	;;
	;; Start by checking for RAM above 0x4000
	ld hl,040ffh		;0955 - Last byte of first page of
				;       RAM. This is guaranteed to exist
				;       on all RAM configurations

	;; Write test data (high byte of address) to end of each 256-byte page
l0958h:	ld (hl),h		;0958
	inc h			;0959 - Move to next page
	jr nz,l0958h		;095a - Repeat if address did not overflow 

	;; Attempt to read back (oddly, this routine expects
	;; differences, based on mirroring of main RAM area)
	ld h,040h		;095c
l095eh:	ld a,(hl)		;095e
	cp h			;095f
	jr z,l0965h		;0960 - If reads back same, means done
	inc h			;0962
	jr nz,l095eh		;0963

	;; Calculate size of memory 
l0965h:	cpl			;0965 - Calculate $10000-HL
	ld h,a			;0966   and store in HL
	inc hl			;0967

	;; Save current registers
	exx			;0968

	;; Zero RAM
	ld hl,04000h		;0969
	ld de,l0000h+1		;096c
	xor a			;096f
l0970h:	ld (hl),a		;0970
	add hl,de		;0971
	jr nc,l0970h		;0972

	;; Initialise (Second half of) system variables
	ld hl,DEFVARS		;0974
	ld de,VARS+0x20		;0977
	ld bc,l0060h		;097a
	ldir			;097d

	;; Restore registers
	exx			;097f

	ld (RAM_SIZE),hl	;0980 - Store RAM size

	;; Check how much memory was found and set system variables at
	;; FC6E and FC76 accordingly.
	ld a,0xBF		;0983
	cp h			;0985 - H is number of RAM pages
				;       detected -- e.g., 08 = 2K, 10 =
				;       4K, 40 = 16K, or 80 = 32K RAM
	jr nc,l0991h		;0986 - Skip forward if <= 32K RAM
	
	ld hl,0xBD00		;0988 - Need to adjust memory
				;       configuration for 48k RAM.
	ld (P_DBUFFER),hl	;098b - By default, these variables
	ld (0fc76h),hl		;098e   contain FD00, so only need to
				;       change if largest RAM
				;       configuration

l0991h:	call CHECK_FP_ROM	;0991 - check if ROM/ RAM in 2000--3FFF
	
	;; Display copyright message
	ld hl,COPYRIGHT_MSG	;0994
	call PRINT_STR_HL	;0997

	;; ============================================================
	;; Warm restart
	;; ============================================================
WARM_RESTART:
	ld sp,STACK0_BASE	; Reset machine-stack #0
	
	ld hl,STACKC_BASE	;099d - Reset character stack
	ld (P_STACKC),hl	;09a0

	;; Initialise KIB (32-byte circular buffer at FB80--FBBF)
	ld hl,08080h		;09a3 - Initial offset for both
	ld (KIB_R_OFFSET),hl	;09a6 - last-read and lastwrite offset are
				;       stored in consecutive bytes

l09a9h:	call sub_0934h		;09a9 - Service multitasking (and setup
				;       display routine)
	call sub_05ebh		;09ac - Initialise two machine stacks
	call sub_0765h		;09af - Initialise some variables
	call sub_08c2h		;09b2 - Set/ reset some flags

	ld iy,(STACKP_BASE)	;09b5 - Reset parameter stack

	;; Set I register to point to page containing character set
	ld a,CHARS>>8		;09b9 - Not sure if this is necessary
	ld i,a			;09bb

	;; Initiate display production
	out (0xFE),a		;09bd - Enable NMI Generator, to start
				;       display handling (Note AF' (used
				;       to count NMI signals) has not
				;       been set). Initially,
				;       NEXT_DISP_ROUTINE is set to be
				;       0x0098
	
	;; Check if system variables are corrupted (assumed if value at
	;; FC40 is non-zero, as is zeroed as part of cold restart)
	;;
	;; NOTE: Would it be better to do this before starting display
	;; routine?
	ld a,(VARS)		;09bf - Check for zero at start of Sys
	or a			;09c2   Variables. Otherwise assume
				;       corrupt
	jr nz,COLD_RESTART	;09c3 - Jump to cold restart, if corrupt

	;; Main loop (Context 0 - Execution mode)
C0_MAIN_LOOP:
	call C0_KERNEL		;09c5
	jr C0_MAIN_LOOP		;09c8
	
	;; Check memory from 2000h--3FFFh -- e.g. to see if
	;; floating-point ROM
CHECK_FP_ROM:
	ld hl,02000h		;09ca
	ld a,(hl)		;09cd
	cpl			;09ce
	ld (hl),a		;09cf
	cp (hl)			;09d0 - Check if ROM/ RAM - Z => RAM
	cpl			;09d1 - Reinstate value in RAM
	ld (hl),a		;09d2 

	jr nz,l0a0fh		;09d3 - Jump forward if not RAM

	;; RAM detected at 0x2000
	push af			;09d5

	;; Update RAM stats
	ld hl,0fc57h		;09d6 - Some measure of RAM

	;; Update RAM start
	ld a,020h		;09d9 - 32 256-byte pages / also high
				;       byte of RAM_START
	ld (RAM_START+1),a	;09db
	add a,(hl)		;09de - Increase count by 2k
	ld (hl),a		;09df

	ld hl,FLAGS3		;09e0
	set 4,(hl)		;09e3

	pop af			;09e5

	;; Check for known RAM content?
	xor 0a5h		;09e6
	jr z,l0a0ch		;09e8
	dec a			;09ea
	jr nz,l09f4h		;09eb

	call sub_0744h		;09ed

l09f0h:	ld hl,(02002h)		;09f0
	jp (hl)			;09f3

l09f4h:	ld hl,02000h		;09f4
	ld (hl),0a5h		;09f7
	ld hl,02010h		;09f9
	ld (0fc8ah),hl		;09fc
	ld (0fc8eh),hl		;09ff
	call sub_1b70h		;0a02
	ld hl,l19b4h		;0a05
	ld (02002h),hl		;0a08

	ret			;0a0b

l0a0ch:	jp sub_0744h		;0a0c

	;; Check if valid ROM
	;;
	;; At this point A = 0xD3
l0a0fh:	xor 0a5h		;0a0f - %10100101
sub_0a11h:
	jp z,l0750h		;0a11 - Jump if recognised ROM ???
	dec a			;0a14 - A=0x74
	ret nz			;0a15 - Return, if not recognised ???

	call l0750h		;0a16

	jr l09f0h		;0a19 - Jump to address in 0x2002

	;; Main loop (Context 1 - ???)
C1_MAIN_LOOP:
	call CHECK_STACKP	; Check for Parameter Stack underflow
	call READ_FROM_KIB	; Read next value from KIB (carry set if none)

	call nc,PARSE_KIB_ENTRY	;0a21 - Switch context, if new entry in
				;       keyboard buffer (and if FLAG(3)
				;       is set)

	call sub_058ah		;0a24

	ld hl,(P_MTASK)		;0a27
	call jump_to_hl		;0a2a

	ld hl,(0fc54h)		;0a2d
	inc hl			;0a30
	ld (0fc54h),hl		;0a31
	jr C1_MAIN_LOOP		;0a34

	;; Part of processing word entered at keyboard (e.g., called
	;; from 0x07F3)
	;;
	;; Possible push token as a number to stack
sub_0a36h:
	push hl			;0a36
	push de			;0a37
	push bc			;0a38

	call CDUP		;0a39 - Duplicate token
	xor a			;0a3c
	ld c,a			;0a3d
	rst 10h			;0a3e - Pop string length from stack to HL

l0a3fh:	or l			;0a3f - Check if A and L are zero
	jr z,l0a79h		;0a40 - Skip forward if so
	
	ld b,a			;0a42 
	ld h,c			;0a43
	ld l,c			;0a44
	ld d,c			;0a45
	rst 8			;0a46 - Push HL onto param stack
	rst 8			;0a47 - Push HL onto stack
	ld hl,(BASE)		;0a48 - Retrieve base
	ex de,hl		;0a4b - DE = base

l0a4ch:	rst 20h			;0a4c - Pop from character stack into A
	call ATOI		;0a4d
	cp e			;0a50 - Check if exceeds base
	jr nc,l0a7eh		;0a51   Exit, if exceeds
	
	ex de,hl		;0a53
	rst 8			;0a54 - Push HL onto param stack
	ex de,hl		;0a55
	ld l,a			;0a56
	call sub_0c13h		;0a57
	call DSTAR		;0a5a
	rst 8			;0a5d - Push HL onto param stack
	call sub_0c13h		;0a5e
	call sub_1305h		;0a61

	djnz l0a4ch		;0a64

l0a66h:	call sub_0b9bh		;0a66
	bit 0,c		;0a69
	call nz,sub_0b2dh		;0a6b
	call l0d6bh		;0a6e
	bit 7,c		;0a71
	ex de,hl			;0a73
	call nz,sub_0008h		;0a74
	ex de,hl			;0a77
	rst 8			;0a78
l0a79h:	ld a,c			;0a79

	pop bc			;0a7a
	pop de			;0a7b
	pop hl			;0a7c

	ret			;0a7d
l0a7eh:
	cp 0f7h		;0a7e
	jr z,l0a90h		;0a80
	inc c			;0a82
	cp 0f6h		;0a83
	jr z,l0a92h		;0a85
	rst 18h			;0a87
l0a88h:
	rst 20h			;0a88
	djnz l0a88h		;0a89
	rst 10h			;0a8b
	rst 10h			;0a8c
	scf			;0a8d
	jr l0a79h		;0a8e
l0a90h:
	set 7,c		;0a90
l0a92h:
	djnz l0a4ch		;0a92
	jr l0a66h		;0a94
sub_0a96h:
	push hl			;0a96
	push de			;0a97
	push bc			;0a98
	xor a			;0a99
	ld c,001h		;0a9a
	call sub_0b9bh		;0a9c
	call sub_0b28h		;0a9f
	push af			;0aa2
l0aa3h:
	call STACK_DE_HL		;0aa3
	ld hl,(BASE)		;0aa6
	rst 8			;0aa9
	call sub_0c13h		;0aaa
	call 00cc5h		;0aad
	rst 10h			;0ab0
	ld a,l			;0ab1
	rst 10h			;0ab2
	call sub_091eh		;0ab3
	rst 18h			;0ab6
	inc c			;0ab7
	call sub_0b9bh		;0ab8
	call sub_0d93h		;0abb
	jr nz,l0aa3h		;0abe
	pop af			;0ac0
	rra			;0ac1
	jr nc,l0ac8h		;0ac2
	ld a,02dh		;0ac4
	rst 18h			;0ac6
	inc c			;0ac7
l0ac8h:
	ld a,020h		;0ac8
	rst 18h			;0aca
	ld h,000h		;0acb
	ld l,c			;0acd
	rst 8			;0ace
	pop bc			;0acf
	pop de			;0ad0
	pop hl			;0ad1
	ret			;0ad2
sub_0ad3h:
	push hl			;0ad3
	push de			;0ad4
	push bc			;0ad5
	ld c,001h		;0ad6
	rst 10h			;0ad8
	call sub_0b19h		;0ad9
	push af			;0adc
l0addh:
	rst 8			;0add
	ld hl,(BASE)		;0ade
	rst 8			;0ae1
	call sub_0bf2h		;0ae2
	rst 10h			;0ae5
	ld a,l			;0ae6
	call sub_091eh		;0ae7
	rst 18h			;0aea
	inc c			;0aeb
	rst 10h			;0aec
	call sub_0d98h		;0aed
	jr nz,l0addh		;0af0
	pop af			;0af2
	jr z,l0ac8h		;0af3
	ld a,02dh		;0af5
	rst 18h			;0af7
	inc c			;0af8
	jr l0ac8h		;0af9

	;; Start of built-in FORTH dictionary.
	;; 
	;; Word check in
	;; https://github.com/monsonite/Z80_Forth/blob/master/h4th_source_2.asm

	;; Forth word M*
	db 0x02, 0x4D, 0x2A
	db 0x64, 0x00

sub_0b00h:
	push hl			;0b00
	push de			;0b01
	xor a			;0b02
	rst 10h			;0b03
	call sub_0b19h		;0b04
	ex de,hl			;0b07
	rst 10h			;0b08
	call sub_0b19h		;0b09
	call sub_0b3eh		;0b0c
	or a			;0b0f
	call po,sub_0b2dh		;0b10
	call STACK_DE_HL		;0b13
	pop de			;0b16
	pop hl			;0b17

	ret			;0b18

sub_0b19h:
	bit 7,h		;0b19
	ret z			;0b1b
	scf			;0b1c
	rla			;0b1d
sub_0b1eh:
	push de			;0b1e
	ld de,l0000h		;0b1f
	or a			;0b22
	ex de,hl			;0b23
	sbc hl,de		;0b24
	pop de			;0b26
	ret			;0b27
sub_0b28h:
	bit 7,d		;0b28
	ret z			;0b2a
	scf			;0b2b
	rla			;0b2c
sub_0b2dh:
	call sub_0b1eh		;0b2d
	ex de,hl			;0b30
	jr nc,l0b34h		;0b31
	inc hl			;0b33
l0b34h:
	call sub_0b1eh		;0b34
	ex de,hl			;0b37
	ret			;0b38

	;; Stack DE and HL  ( -- HL DE )
	;; On entry:
	;;
	;; On exit:
	;; 
STACK_DE_HL:
	;; Push DE onto parameter stack
	ex de,hl		;0b39
	rst 8			;0b3a - Push HL onto Parameter stack

	;; Push HL onto parameter stack
	ex de,hl		;0b3b
	rst 8			;0b3c - Push DE onto stack
	
	ret			;0b3d

sub_0b3eh:
	push ix		;0b3e
l0b40h:
	push bc			;0b40
	ld ix,l0000h		;0b41
	ld b,010h		;0b45
	or a			;0b47
	jr l0b4ch		;0b48
l0b4ah:
	add ix,ix		;0b4a
l0b4ch:
	adc hl,hl		;0b4c
	jr nc,l0b55h		;0b4e
	add ix,de		;0b50
	jr nc,l0b55h		;0b52
	inc hl			;0b54
l0b55h:
	djnz l0b4ah		;0b55
	push ix		;0b57
	pop de			;0b59
	ex de,hl			;0b5a
	pop bc			;0b5b
	pop ix		;0b5c
	ret			;0b5e
	ld bc,l0f2ah		;0b5f
	nop			;0b62
	push hl			;0b63
	call sub_0b00h		;0b64
	rst 10h			;0b67
	push hl			;0b68
	rst 10h			;0b69
	pop hl			;0b6a
	rst 8			;0b6b
	pop hl			;0b6c
	ret			;0b6d
	ld (bc),a			;0b6e
	ld c,l			;0b6f
	cpl			;0b70
	ld d,a			;0b71
	nop			;0b72
sub_0b73h:
	push hl			;0b73
	push de			;0b74
	push bc			;0b75
	xor a			;0b76
	rst 10h			;0b77
	call sub_0b19h		;0b78
	push hl			;0b7b
	pop bc			;0b7c
	call sub_0b9bh		;0b7d
	call sub_0b28h		;0b80
	call sub_0ba0h		;0b83
	or a			;0b86
	jr z,l0b94h		;0b87
	inc a			;0b89
	rra			;0b8a
	call nc,sub_0b1eh		;0b8b
	ex de,hl			;0b8e
	rra			;0b8f
	call c,sub_0b1eh		;0b90
	ex de,hl			;0b93
l0b94h:
	call STACK_DE_HL		;0b94
	pop bc			;0b97
	pop de			;0b98
	pop hl			;0b99
	ret			;0b9a
sub_0b9bh:
	rst 10h			;0b9b
	ex de,hl			;0b9c
	rst 10h			;0b9d
	ex de,hl			;0b9e
	ret			;0b9f
sub_0ba0h:
	push ix		;0ba0
	push bc			;0ba2
	ld b,011h		;0ba3
	push hl			;0ba5
	ex de,hl			;0ba6
	pop ix		;0ba7
	pop de			;0ba9
	jr l0bb5h		;0baa
l0bach:
	adc hl,hl		;0bac
	or a			;0bae
	sbc hl,de		;0baf
	ccf			;0bb1
	jr c,l0bb9h		;0bb2
	add hl,de			;0bb4
l0bb5h:
	add ix,ix		;0bb5
	jr l0bbdh		;0bb7
l0bb9h:
	add ix,ix		;0bb9
	inc ix		;0bbb
l0bbdh:
	djnz l0bach		;0bbd
	push ix		;0bbf
	pop de			;0bc1
	pop ix		;0bc2
	ret			;0bc4
	dec b			;0bc5
	ld hl,(04d2fh)		;0bc6
	ld c,a			;0bc9
	ld b,h			;0bca
	ld de,0d700h		;0bcb
	call sub_0b00h		;0bce
	rst 8			;0bd1
	call sub_0b73h		;0bd2
	ret			;0bd5
	ld (bc),a			;0bd6
	ld hl,(00a2fh)		;0bd7
	nop			;0bda
	call 00bcdh		;0bdb
	rst 10h			;0bde
	ret			;0bdf
	ld bc,sub_0b2dh+2		;0be0
	nop			;0be3
	push hl			;0be4
	call sub_0bf2h		;0be5
	rst 10h			;0be8
	pop hl			;0be9
	ret			;0bea
	inc b			;0beb
	cpl			;0bec
	ld c,l			;0bed
	ld c,a			;0bee
	ld b,h			;0bef
	ld (de),a			;0bf0
	nop			;0bf1
sub_0bf2h:
	push hl			;0bf2
	rst 10h			;0bf3
	call sub_0c13h		;0bf4
	rst 8			;0bf7
	call sub_0b73h		;0bf8
	pop hl			;0bfb
	ret			;0bfc
	inc bc			;0bfd
	ld c,l			;0bfe
	ld c,a			;0bff
	ld b,h			;0c00
	rrca			;0c01
	nop			;0c02
	call sub_0bf2h		;0c03
	call sub_0e41h		;0c06
	jp l137ah		;0c09
	inc b			;0c0c
	ld d,e			;0c0d
	dec l			;0c0e
	ld a,044h		;0c0f
	ld d,000h		;0c11

	;; ( XX -- XX FLAG )
sub_0c13h:
	push de			;0c13
	push hl			;0c14
	
	rst 10h			;0c15 - Pop from stack into HL
	ld a,h			;0c16 - Move upper byte to A
	rla			;0c17 - Bit 7 into carry
	ex de,hl		;0c18
	sbc hl,hl		;0c19 - 0000 or FFFF, depending on carry
	ex de,hl		;0c1b
	call STACK_DE_HL	;0c1c

	pop hl			;0c1f
	pop de			;0c20

l0c21h:	ret			;0c21
	
	;; Forth word ROT
	inc bc			;0c22
	ld d,d			;0c23
	ld c,a			;0c24
	ld d,h			;0c25
	ld (de),a			;0c26
	nop			;0c27
	rst 10h			;0c28
	ex de,hl			;0c29
	rst 10h			;0c2a
	push hl			;0c2b
	rst 10h			;0c2c
	ex (sp),hl			;0c2d
	rst 8			;0c2e
l0c2fh:
	ex de,hl			;0c2f
	rst 8			;0c30
	pop hl			;0c31
	rst 8			;0c32
	ret			;0c33
	inc bc			;0c34
	ld c,l			;0c35
	ld b,h			;0c36
	cpl			;0c37
	ld b,c			;0c38
	nop			;0c39
l0c3ah:
	push ix		;0c3a
	push hl			;0c3c
	push de			;0c3d
	push bc			;0c3e
	push iy		;0c3f
	pop de			;0c41
	ld hl,sub_0008h		;0c42
	add hl,de			;0c45
	push de			;0c46
	pop ix		;0c47
	ld bc,02008h		;0c49
l0c4ch:
	push bc			;0c4c
	push ix		;0c4d
	or a			;0c4f
l0c50h:
	rl (ix+004h)		;0c50
	inc ix		;0c54
	dec c			;0c56
	jr nz,l0c50h		;0c57
	pop ix		;0c59
	call sub_12c3h		;0c5b
	jr c,l0c67h		;0c5e
	call sub_12e7h		;0c60
	set 0,(ix+004h)		;0c63
l0c67h:
	pop bc			;0c67
	djnz l0c4ch		;0c68
	pop bc			;0c6a
	pop de			;0c6b
	rst 10h			;0c6c
	rst 10h			;0c6d
	call 00da3h		;0c6e
	pop hl			;0c71
	pop ix		;0c72
	ret			;0c74

	;; Forth word MD*
	db 0x03, 0x4D, 0x44, 0x2A
	db 0x4B, 0x00
MDSTAR:
	push ix		;0c7b
	push hl			;0c7d
	push de			;0c7e
	push bc			;0c7f
	push iy		;0c80
	rst 8			;0c82
	rst 8			;0c83
	ld bc,sub_0008h		;0c84
	push iy		;0c87
	pop de			;0c89
	pop hl			;0c8a
	call sub_036ah		;0c8b
	push de			;0c8e
	ld b,004h		;0c8f
	pop ix		;0c91
l0c93h:
	ld (ix+008h),000h		;0c93
	inc hl			;0c97
	inc ix		;0c98
	djnz l0c93h		;0c9a
	ld bc,02008h		;0c9c
l0c9fh:
	push bc			;0c9f
	or a			;0ca0
	bit 0,(ix+000h)		;0ca1
	call nz,sub_12d7h		;0ca5
	ld b,c			;0ca8
	push ix		;0ca9
l0cabh:
	rr (ix+007h)		;0cab
	dec ix		;0caf
	djnz l0cabh		;0cb1
	pop ix		;0cb3
	pop bc			;0cb5
	djnz l0c9fh		;0cb6
	rst 10h			;0cb8
	rst 10h			;0cb9
	pop bc			;0cba
	pop de			;0cbb
	pop hl			;0cbc
	pop ix		;0cbd
	ret			;0cbf

	;; Forth Word D/
	;;
	;;
	db 0x02, 0x44, 0x2F
	db 0x11, 0x00

	call 0x0B9B
	call sub_0cfbh		;0cc8
	call STACK_DE_HL		;0ccb
	jp l0c3ah		;0cce

	;; Forth Word D*
	db 0x02, 0x44, 0x2A
	db 0x16, 0x00
DSTAR:
	push hl			;0cd6
	push de			;0cd7
	call MDSTAR		;0cd8
	call sub_0b9bh		;0cdb
	call TWODROP		;0cde
	call STACK_DE_HL	;0ce1
	pop de			;0ce4
	pop hl			;0ce5

	ret			;0ce6

	;; Forth word 2DROP
	;;
	;; 
	db 0x05, 0x32, 0x44, 0x52, 0x4F, 0x50
	db 0x0D, 0x00
TWODROP:
	push hl			;0cef
	rst 10h			;0cf0
	rst 10h			;0cf1
	pop hl			;0cf2
	ret			;0cf3
	inc b			;0cf4
	ld b,h			;0cf5
	dec l			;0cf6
	ld a,051h		;0cf7
	rra			;0cf9
	nop			;0cfa
sub_0cfbh:
	push hl			;0cfb
	push de			;0cfc
	call sub_0b9bh		;0cfd
	call STACK_DE_HL		;0d00
	push iy		;0d03
	call STACK_DE_HL		;0d05
	pop hl			;0d08
	push hl			;0d09
	rl d		;0d0a
	pop de			;0d0c
	call sub_12e7h		;0d0d
	pop de			;0d10
	pop hl			;0d11
	ret			;0d12
	ld bc,l0e2bh		;0d13
	nop			;0d16
	push hl			;0d17
	push de			;0d18
	rst 10h			;0d19
	ex de,hl			;0d1a
	rst 10h			;0d1b
	add hl,de			;0d1c
	rst 8			;0d1d
	pop de			;0d1e
	pop hl			;0d1f
	ret			;0d20
	ld bc,l102dh		;0d21
	nop			;0d24
	push hl			;0d25
	push de			;0d26
	rst 10h			;0d27
	ex de,hl			;0d28
	rst 10h			;0d29
	or a			;0d2a
	sbc hl,de		;0d2b
	rst 8			;0d2d
	pop de			;0d2e
	pop hl			;0d2f
	ret			;0d30
	dec b			;0d31
	ld c,l			;0d32
	ld c,c			;0d33
	ld c,(hl)			;0d34
	ld d,l			;0d35
	ld d,e			;0d36
	djnz l0d39h		;0d37
l0d39h:
	push hl			;0d39
	rst 10h			;0d3a
	call sub_0b1eh		;0d3b
	rst 8			;0d3e
	pop hl			;0d3f
	ret			;0d40

	;; Forth word CDUP
	;;
	;; Duplicate string on character stack
	;; 
	db 0x04, 0x43, 0x44, 0x55, 0x50
	db 0x22, 0x00

	;; Save registers
CDUP:	push af			;0d48
	push hl			;0d49

	;; Effectively DUP
	rst 10h			;0d4a - Pop HL (length of string) onto
				;       parameter stack
	rst 8			;0d4b - Push HL from parameter stack
	rst 8			;0d4c - Push HL from parameter stack

	;; Get length of word and check if null
	ld a,l			;0d4d
	and %00111111		;0d4e
	jr z,CDUP_DONE		;0d50
	
	push bc			;0d52

	ld b,a			;0d53 - B is length of string

	;;  Point HL to end of current string on stack
	ld hl,(P_STACKC)	;0d54 
	dec a			;0d57
	add a,l			;0d58
	ld l,a			;0d59

CDUP_LOOP:
	ld a,(hl)		;0d5a - Retrieve character
	rst 18h			;0d5b - Push Character
	dec hl			;0d5c - Next character
	djnz CDUP_LOOP		;0d5d - Repeat unil done

	pop bc			;0d5f

CDUP_DONE:
	pop hl			;0d60
	pop af			;0d61

	ret			;0d62

	dec b			;0d63
	ld b,e			;0d64
	ld b,h			;0d65
	ld d,d			;0d66
	ld c,a			;0d67
	ld d,b			;0d68
	ld d,000h		;0d69
l0d6bh:
	push hl			;0d6b
	rst 10h			;0d6c
	ld a,l			;0d6d
	and 03fh		;0d6e
	jr z,l0d77h		;0d70
	ld l,a			;0d72
l0d73h:
	rst 20h			;0d73
	dec l			;0d74
	jr nz,l0d73h		;0d75
l0d77h:
	pop hl			;0d77
	ret			;0d78
	inc b			;0d79
	ccf			;0d7a
	ld b,h			;0d7b
	ld d,l			;0d7c
	ld d,b			;0d7d
	ld c,000h		;0d7e
	rst 10h			;0d80
	rst 8			;0d81
	ld a,l			;0d82
	or h			;0d83
	ret z			;0d84
	rst 8			;0d85
	ret			;0d86
	inc bc			;0d87
	ld b,h			;0d88
	ld d,l			;0d89
	ld d,b			;0d8a
	inc d			;0d8b
	nop			;0d8c
sub_0d8dh:
	push hl			;0d8d
	rst 10h			;0d8e
	rst 8			;0d8f
	rst 8			;0d90
	pop hl			;0d91
	ret			;0d92
sub_0d93h:
	ld a,l			;0d93
	or h			;0d94
	or e			;0d95
	or d			;0d96
	ret			;0d97
sub_0d98h:
	ld a,l			;0d98
	or h			;0d99
	ret			;0d9a
	dec b			;0d9b
	ld b,h			;0d9c
	ld d,e			;0d9d
	ld d,a			;0d9e
	ld b,c			;0d9f
	ld d,b			;0da0
	ld hl,0e500h		;0da1
	push de			;0da4
	push bc			;0da5
	rst 10h			;0da6
	push hl			;0da7
	rst 10h			;0da8
	push hl			;0da9
	rst 10h			;0daa
	push hl			;0dab
	rst 10h			;0dac
	pop bc			;0dad
	pop de			;0dae
	ex de,hl			;0daf
	rst 8			;0db0
	pop hl			;0db1
	rst 8			;0db2
	ex de,hl			;0db3
	rst 8			;0db4
	push bc			;0db5
	pop hl			;0db6
	rst 8			;0db7
	pop bc			;0db8
	pop de			;0db9
	pop hl			;0dba
	ret			;0dbb
	add a,d			;0dbc
	ld b,h			;0dbd
	ld c,a			;0dbe
	ld hl,02100h		;0dbf
	adc a,00dh		;0dc2
	call sub_07d3h		;0dc4
	ld hl,(0fc8ah)		;0dc7
	push hl			;0dca
	call l0881h		;0dcb
	pop de			;0dce
	ld bc,08000h		;0dcf
	call sub_0e41h		;0dd2
	rst 10h			;0dd5
	add hl,bc			;0dd6
	push hl			;0dd7
	rst 10h			;0dd8
	add hl,bc			;0dd9
	push hl			;0dda
	ex de,hl			;0ddb
	jp (hl)			;0ddc
	add a,l			;0ddd
	dec hl			;0dde
	ld c,h			;0ddf
	ld c,a			;0de0
	ld c,a			;0de1
	ld d,b			;0de2
	jr z,l0de5h		;0de3
l0de5h:
	pop hl			;0de5
	pop hl			;0de6
	pop hl			;0de7
	ld hl,l0deeh		;0de8
	jp l0e12h		;0deb
l0deeh:
	pop bc			;0dee
	pop de			;0def
	rst 10h			;0df0
	bit 7,h		;0df1
	jr nz,l0dffh		;0df3
	add hl,de			;0df5
	ex de,hl			;0df6
	pop hl			;0df7
	push hl			;0df8
	push de			;0df9
l0dfah:
	scf			;0dfa
	sbc hl,de		;0dfb
	push bc			;0dfd
	ret			;0dfe
l0dffh:
	add hl,de			;0dff
	pop de			;0e00
	push de			;0e01
	push hl			;0e02
	jr l0dfah		;0e03
	add a,h			;0e05
	ld c,h			;0e06
	ld c,a			;0e07
	ld c,a			;0e08
	ld d,b			;0e09
	daa			;0e0a
	nop			;0e0b
	pop hl			;0e0c
	pop hl			;0e0d
	pop hl			;0e0e
	ld hl,l0e21h		;0e0f
l0e12h:
	call sub_07d3h		;0e12
	pop hl			;0e15
	ld a,0d2h		;0e16
	call l07d5h		;0e18
	ld hl,0e1e1h		;0e1b
	jp sub_07c4h		;0e1e
l0e21h:
	pop de			;0e21
	pop bc			;0e22
	pop hl			;0e23
	push hl			;0e24
	inc bc			;0e25
	push bc			;0e26
	scf			;0e27
	sbc hl,bc		;0e28
	ex de,hl			;0e2a
l0e2bh:
	jp (hl)			;0e2b
	dec b			;0e2c
	ld c,h			;0e2d
	ld b,l			;0e2e
	ld b,c			;0e2f
	ld d,(hl)			;0e30
	ld b,l			;0e31
	ld c,000h		;0e32
	pop hl			;0e34
	pop de			;0e35
	pop bc			;0e36
	push de			;0e37
	push de			;0e38
	jp (hl)			;0e39
l0e3ah:
	inc b			;0e3a
	ld d,e			;0e3b
	ld d,a			;0e3c
	ld b,c			;0e3d
	ld d,b			;0e3e
	inc de			;0e3f
	nop			;0e40
sub_0e41h:
	push hl			;0e41
	push de			;0e42
	rst 10h			;0e43
	ex de,hl			;0e44
	rst 10h			;0e45
	ex de,hl			;0e46
	rst 8			;0e47
	ex de,hl			;0e48
l0e49h:
	rst 8			;0e49
l0e4ah:
	pop de			;0e4a
	pop hl			;0e4b
	ret			;0e4c
	ld bc,CHECK_STACKP+2	;0e4d
	nop			;0e50
	call sub_06e3h		;0e51
	rst 8			;0e54
	ret			;0e55
	ld bc,l0e3ah		;0e56
	nop			;0e59
	call sub_0866h		;0e5a
	ld hl,l0878h		;0e5d
	push hl			;0e60
	call l0881h		;0e61
	add a,c			;0e64
	dec sp			;0e65
	inc c			;0e66
	nop			;0e67
l0e68h:
	call sub_07e5h		;0e68
	pop hl			;0e6b
	pop hl			;0e6c
l0e6dh:
	pop hl			;0e6d
	pop hl			;0e6e
	ret			;0e6f
	add a,d			;0e70
	dec sp			;0e71
	ld b,e			;0e72
	rlca			;0e73
	nop			;0e74
	jr l0e6dh		;0e75
	ld bc,00a2eh		;0e77
	nop			;0e7a
l0e7bh:
	call sub_0ad3h		;0e7b
	jp PRINT_STRING		;0e7e
	ld (bc),a			;0e81
	ld b,h			;0e82
	ld l,00bh		;0e83
	nop			;0e85
	call sub_0a96h		;0e86
	jp PRINT_STRING		;0e89
	ld bc,l0e49h		;0e8c
	nop			;0e8f
	pop de			;0e90
	pop hl			;0e91
	push hl			;0e92
	push de			;0e93
l0e94h:
	ld de,08000h		;0e94
	add hl,de			;0e97
	rst 8			;0e98
	ret			;0e99
	ld bc,l0e4ah		;0e9a
	nop			;0e9d
	ld hl,l0005h+1		;0e9e
	add hl,sp			;0ea1
l0ea2h:
	ld a,(hl)			;0ea2
	inc hl			;0ea3
	ld h,(hl)			;0ea4
	ld l,a			;0ea5
	jr l0e94h		;0ea6
	ld (bc),a			;0ea8
	ld c,(hl)			;0ea9
	ld c,c			;0eaa
	inc c			;0eab
	nop			;0eac
	rst 10h			;0ead
	add hl,hl			;0eae
	inc hl			;0eaf
	add hl,hl			;0eb0
	add hl,sp			;0eb1
	jr l0ea2h		;0eb2
	inc b			;0eb4
	ld d,b			;0eb5
	ld c,c			;0eb6
	ld b,e			;0eb7
	ld c,e			;0eb8
	inc de			;0eb9
	nop			;0eba
	push iy		;0ebb
	rst 10h			;0ebd
	add hl,hl			;0ebe
	pop de			;0ebf
	add hl,de			;0ec0
	ld e,(hl)			;0ec1
	inc hl			;0ec2
	ld d,(hl)			;0ec3
	ex de,hl			;0ec4
	rst 8			;0ec5
	ret			;0ec6
	ld (bc),a			;0ec7
	ld b,e			;0ec8
	ld d,d			;0ec9
	ex af,af'			;0eca
	nop			;0ecb
	jp sub_06dah		;0ecc
	inc b			;0ecf
	ld b,l			;0ed0
	ld c,l			;0ed1
	ld c,c			;0ed2
	ld d,h			;0ed3
	inc c			;0ed4
	nop			;0ed5
	rst 10h			;0ed6
	ld a,l			;0ed7
	jp PRINT_A		;0ed8
	ld bc,l0c21h		;0edb
	nop			;0ede
	rst 10h			;0edf
	ex de,hl			;0ee0
	rst 10h			;0ee1
	ex de,hl			;0ee2
	ld (hl),e			;0ee3
	inc hl			;0ee4
	ld (hl),d			;0ee5
	ret			;0ee6
	ld bc,l0b40h		;0ee7
	nop			;0eea
sub_0eebh:
	rst 10h			;0eeb
	ld e,(hl)			;0eec
	inc hl			;0eed
	ld d,(hl)			;0eee
	ex de,hl			;0eef
	rst 8			;0ef0
	ret			;0ef1
	ld (bc),a			;0ef2
	ld d,e			;0ef3
	ld d,b			;0ef4
	ld a,(bc)			;0ef5
	nop			;0ef6
	ld a,020h		;0ef7
	jp PRINT_A		;0ef9
	rlca			;0efc
	inc a			;0efd
	ld b,d			;0efe
	ld d,l			;0eff
l0f00h:
	ld c,c			;0f00
	ld c,h			;0f01
	ld b,h			;0f02
	ld d,e			;0f03
	rla			;0f04
	nop			;0f05
	call sub_0866h		;0f06
	call sub_07d3h		;0f09
	ld hl,(0fc8ah)		;0f0c
	dec hl			;0f0f
	dec hl			;0f10
	ex (sp),hl			;0f11
	jp (hl)			;0f12
	add a,l			;0f13
	ld b,h			;0f14
	ld c,a			;0f15
	ld b,l			;0f16
	ld d,e			;0f17
	ld a,01ah		;0f18
	nop			;0f1a
	ld hl,l0f27h		;0f1b
	call sub_07d3h		;0f1e
	ld hl,0cfe1h		;0f21
	jp sub_07c4h		;0f24
l0f27h:
	pop de			;0f27
	pop hl			;0f28
	ld (hl),e			;0f29
l0f2ah:
	inc hl			;0f2a
	ld (hl),d			;0f2b
	ret			;0f2c
	dec b			;0f2d
	ld b,c			;0f2e
	ld c,h			;0f2f
	ld c,h			;0f30
	ld c,a			;0f31
	ld d,h			;0f32
	ld (de),a			;0f33
	nop			;0f34
sub_0f35h:
	ld hl,(0fc8ah)		;0f35
	ex de,hl			;0f38
	rst 10h			;0f39
	add hl,de			;0f3a
	ld (0fc8ah),hl		;0f3b
	ret			;0f3e
	add a,h			;0f3f
	ld b,e			;0f40
	ld c,a			;0f41
	ld b,h			;0f42
	ld b,l			;0f43
	ld a,(0cd00h)		;0f44
	ld a,a			;0f47
	rrca			;0f48
	jr l0f4eh		;0f49
l0f4bh:
	call sub_07d3h		;0f4b
l0f4eh:
	call sub_0782h		;0f4e
	jr c,l0f69h		;0f51
	call l0d6bh		;0f53
	bit 7,(hl)		;0f56
	push af			;0f58
	call sub_082ah		;0f59
	pop af			;0f5c
	jr z,l0f4bh		;0f5d
	ld de,l0878h		;0f5f
	push de			;0f62
	call jump_to_hl		;0f63
	pop de			;0f66
	jr l0f4eh		;0f67
l0f69h:
	call sub_0a36h		;0f69
	ld a,048h		;0f6c
	jp c,l0878h		;0f6e
	rst 10h			;0f71
	ld a,l			;0f72
	call sub_07b9h		;0f73
	jp l0f4eh		;0f76
	inc bc			;0f79
	ld c,b			;0f7a
	ld b,l			;0f7b
	ld e,b			;0f7c
	dec c			;0f7d
	nop			;0f7e
	ld hl,l0010h		;0f7f
	ld (BASE),hl		;0f82
	ret			;0f85
	inc bc			;0f86
	ld b,c			;0f87
	ld c,(hl)			;0f88
	ld b,h			;0f89
	ld de,0d700h		;0f8a
	ex de,hl			;0f8d
	rst 10h			;0f8e
	ld a,l			;0f8f
	and e			;0f90
	ld l,a			;0f91
	ld a,h			;0f92
	and d			;0f93
	ld h,a			;0f94
	rst 8			;0f95
	ret			;0f96
	ld (bc),a			;0f97
	ld c,a			;0f98
	ld d,d			;0f99
	djnz l0f9ch		;0f9a
l0f9ch:
	rst 10h			;0f9c
	ex de,hl			;0f9d
	rst 10h			;0f9e
	ld a,l			;0f9f
	or e			;0fa0
	ld l,a			;0fa1
	ld a,h			;0fa2
	or d			;0fa3
	ld h,a			;0fa4
	rst 8			;0fa5
	ret			;0fa6
	inc bc			;0fa7
	ld e,b			;0fa8
	ld c,a			;0fa9
	ld d,d			;0faa
	ld de,0d700h		;0fab
	ex de,hl			;0fae
	rst 10h			;0faf
	ld a,l			;0fb0
	xor e			;0fb1
	ld l,a			;0fb2
	ld a,h			;0fb3
	xor d			;0fb4
	ld h,a			;0fb5
	rst 8			;0fb6
	ret			;0fb7
	inc b			;0fb8
	ld (04156h),a		;0fb9
	ld d,d			;0fbc
	ld c,000h		;0fbd
	call sub_0fd1h		;0fbf
	rst 10h			;0fc2
	jp sub_07c4h		;0fc3
	ex af,af'			;0fc6
	ld d,(hl)			;0fc7
	ld b,c			;0fc8
	ld d,d			;0fc9
	ld c,c			;0fca
	ld b,c			;0fcb
	ld b,d			;0fcc
	ld c,h			;0fcd
	ld b,l			;0fce
	dec de			;0fcf
	nop			;0fd0
sub_0fd1h:
	call sub_0866h		;0fd1
	ld hl,l0fdeh		;0fd4
	call sub_07d3h		;0fd7
	rst 10h			;0fda
	jp sub_07c4h		;0fdb
l0fdeh:
	pop hl			;0fde
	rst 8			;0fdf
	ret			;0fe0

	;; Forth word TASK
	inc b			;0fe1
	ld d,h			;0fe2
	ld b,c			;0fe3
	ld d,e			;0fe4
	ld c,e			;0fe5
	ld c,b			;0fe6
	nop			;0fe7

	call sub_0866h		;0fe8
	ld hl,(START_OF_DICT)		;0feb
	set 6,(hl)		;0fee
	ld hl,(0fc8ah)		;0ff0
	ld de,l0005h		;0ff3
	add hl,de			;0ff6
	push hl			;0ff7
	call sub_07c4h		;0ff8
	ld hl,l1190h		;0ffb
	call sub_07d3h		;0ffe
	ld hl,l000dh		;1001
	rst 8			;1004
	call sub_0f35h		;1005
	ld bc,sub_0b00h		;1008
	pop hl			;100b
	push hl			;100c
l100dh:
	ld (hl),c			;100d
	inc hl			;100e
	djnz l100dh		;100f
	ex de,hl			;1011
	call sub_06e3h		;1012
	ex de,hl			;1015
	ld (hl),e			;1016
	inc hl			;1017
	ld (hl),d			;1018
	pop de			;1019
	ld hl,(0fc92h)		;101a
	out (0fdh),a		;101d
	ld (hl),e			;101f
	inc hl			;1020
	ld (hl),d			;1021
	out (0feh),a		;1022
	ex de,hl			;1024
	ld (0fc92h),hl		;1025
	ret			;1028
	inc b			;1029
	ld b,c			;102a
	ld d,l			;102b
	ld d,h			;102c
l102dh:
	ld c,a			;102d
	dec hl			;102e
	nop			;102f
	ld hl,FLAGS3		;1030
	res 7,(hl)		;1033
	ret			;1035
sub_1036h:
	ld hl,FLAGS3		;1036
	bit 7,(hl)		;1039
	ret nz			;103b
	ld de,l0000h		;103c
	ld hl,(0fc54h)		;103f
	ex de,hl			;1042
	ld (0fc54h),hl		;1043
	ex de,hl			;1046
	call sub_0d98h		;1047
	jr z,l1060h		;104a
	ld de,l0100h		;104c
	sbc hl,de		;104f
	jr nc,l1073h		;1051
	ret			;1053
	inc b			;1054
	ld b,(hl)			;1055
	ld b,c			;1056
	ld d,e			;1057
	ld d,h			;1058
	inc de			;1059
	nop			;105a
	ld hl,FLAGS3		;105b
	set 7,(hl)		;105e
l1060h:
	ld hl,l0057h		;1060
	ld (P_RUN_DISP),hl		;1063
	ret			;1066
	inc b			;1067
	ld d,e			;1068
	ld c,h			;1069
	ld c,a			;106a
	ld d,a			;106b
	inc de			;106c
	nop			;106d

	;; Set the location of main display routine
l106eh:	ld hl,FLAGS3		;106e
	set 7,(hl)		;1071

l1073h:	ld hl,RUN_DISPLAY	;1073
	ld (P_RUN_DISP),hl	;1076
	
	ret			;1079


	ld (bc),a			;107a
	ld b,h			;107b
	ld b,b			;107c
	ld hl,0dd00h		;107d
	push hl			;1080
	rst 10h			;1081
	push hl			;1082
	pop ix		;1083
	call sub_108eh		;1085
	call STACK_DE_HL		;1088
	pop ix		;108b
	ret			;108d
sub_108eh:
	ld l,(ix+000h)		;108e
	ld h,(ix+001h)		;1091
	ld e,(ix+002h)		;1094
	ld d,(ix+003h)		;1097
	ret			;109a
	ld (bc),a			;109b
	ld b,h			;109c
	ld hl,l0021h		;109d
	push ix		;10a0
	rst 10h			;10a2
	push hl			;10a3
	pop ix		;10a4
	call sub_0b9bh		;10a6
	call sub_10afh		;10a9
	pop ix		;10ac
	ret			;10ae
sub_10afh:
	ld (ix+000h),l		;10af
	ld (ix+001h),h		;10b2
	ld (ix+002h),e		;10b5
	ld (ix+003h),d		;10b8
	ret			;10bb
	add a,d			;10bc
	ld c,c			;10bd
	ld b,(hl)			;10be
	rra			;10bf
	nop			;10c0
	ld hl,l10d7h		;10c1
	call sub_07d3h		;10c4
	ld hl,(0fc8ah)		;10c7
	inc hl			;10ca
	push hl			;10cb
	ld a,0cah		;10cc
	call l07d5h		;10ce
	call l0881h		;10d1
	jp l0878h		;10d4
l10d7h:
	rst 10h			;10d7
	jp sub_0d98h		;10d8
	add a,h			;10db
	ld b,l			;10dc
	ld c,h			;10dd
	ld d,e			;10de
	ld b,l			;10df
	ld e,000h		;10e0
	pop hl			;10e2
	pop hl			;10e3
	pop hl			;10e4
	pop de			;10e5
	call sub_07cfh		;10e6
	ld hl,(0fc8ah)		;10e9
	ex de,hl			;10ec
	ld (hl),e			;10ed
	inc hl			;10ee
	ld (hl),d			;10ef
	dec de			;10f0
	dec de			;10f1
	push de			;10f2
	call l0881h		;10f3
	jp l0878h		;10f6
	add a,h			;10f9
	ld d,h			;10fa
	ld c,b			;10fb
	ld b,l			;10fc
	ld c,(hl)			;10fd
	inc de			;10fe
	nop			;10ff
	pop hl			;1100
	pop hl			;1101
	pop hl			;1102
	pop de			;1103
	ld hl,(0fc8ah)		;1104
	ex de,hl			;1107
	ld (hl),e			;1108
	inc hl			;1109
	ld (hl),d			;110a
	ret			;110b
	inc bc			;110c
	ld d,d			;110d
	ld b,l			;110e
	ld d,(hl)			;110f
	dec d			;1110
	nop			;1111
	ld de,l0005h+1		;1112
	rst 10h			;1115
	add hl,de			;1116
	ld a,080h		;1117
	xor (hl)			;1119
	ld (hl),a			;111a
	inc hl			;111b
	ld a,080h		;111c
	xor (hl)			;111e
	ld (hl),a			;111f
	ret			;1120
	inc b			;1121
	ld c,h			;1122
	ld c,a			;1123
	ld b,e			;1124
	ld c,e			;1125
	db 0x11,0x00

	;; Some kind of multi-tasking service routine. When multitasking
	;; is off, $FC98 points to ROM, so this routine does nothing.
	ld hl,($FC98)		;1128 - Initially contains 0x0000
	ld de,$000A		; 
	add hl,de		;112e
	set 7,(hl)		;112f
	
	ret			;1131

	ld b,055h		;1132
	ld c,(hl)			;1134
	ld c,h			;1135
	ld c,a			;1136
	ld b,e			;1137
	ld c,e			;1138
	inc de			;1139
	nop			;113a
	ld hl,(0fc98h)		;113b
l113eh:
	ld de,l000ah		;113e
	add hl,de			;1141
	res 7,(hl)		;1142
	ret			;1144
	inc b			;1145
	ld d,e			;1146
	ld d,h			;1147
	ld c,a			;1148
	ld d,b			;1149
	ld c,000h		;114a
	ld hl,l0002h+1		;114c
	rst 8			;114f
	rst 8			;1150
	rst 8			;1151
	ret			;1152
	dec b			;1153
	ld d,e			;1154
	ld d,h			;1155
	ld b,c			;1156
	ld d,d			;1157
	ld d,h			;1158
	rrca			;1159
	nop			;115a
	ld hl,l0002h+2		;115b
	rst 8			;115e
	rst 8			;115f
	rst 8			;1160
	ret			;1161
	ld (bc),a			;1162
	ld c,c			;1163
	ld c,(hl)			;1164
	ld a,(bc)			;1165
	nop			;1166
	ld hl,l0000h		;1167
	rst 8			;116a
	ret			;116b
	dec b			;116c
	ld b,l			;116d
	ld d,(hl)			;116e
	ld b,l			;116f
	ld d,d			;1170
	ld e,c			;1171
	dec c			;1172
	nop			;1173
	ld hl,l0000h+1		;1174
	rst 8			;1177
	ret			;1178
	ld (bc),a			;1179
	ld b,c			;117a
	ld d,h			;117b
	ld a,(bc)			;117c
	nop			;117d
	ld hl,l0002h		;117e
	rst 8			;1181
	ret			;1182
	inc bc			;1183
	ld d,d			;1184
	ld d,l			;1185
	ld c,(hl)			;1186
	ld (hl),d			;1187
	nop			;1188
	ld hl,l0005h		;1189
	rst 8			;118c
	rst 8			;118d
	rst 8			;118e
	ret			;118f
l1190h:
	ex (sp),ix		;1190
	call sub_0b9bh		;1192
	call sub_0b2dh		;1195
	push de			;1198
	push hl			;1199
	rst 10h			;119a
	ld de,0fffah		;119b
	add hl,de			;119e
	jr c,l11a9h		;119f
	add hl,hl			;11a1
	ld de,011bah		;11a2
	add hl,de			;11a5
	jp JP_ADDR_HL		;11a6
l11a9h:
	pop hl			;11a9
	pop hl			;11aa
	pop ix		;11ab
	ret			;11ad
	jp nz,0ce11h		;11ae
	ld de,l12a8h		;11b1
	push de			;11b4
	ld de,l11deh		;11b5
	rst 20h			;11b8
	ld de,0fdd3h		;11b9
	call sub_10afh		;11bc
	out (0feh),a		;11bf
	ret			;11c1
l11c2h:
	inc ix		;11c2
	inc ix		;11c4
l11c6h:
	pop hl			;11c6
	pop de			;11c7
	call 011bah		;11c8
	pop ix		;11cb
	ret			;11cd
	ld de,l0005h+1		;11ce
	add ix,de		;11d1
	jr l11c6h		;11d3
	pop hl			;11d5
	pop hl			;11d6
	set 6,(ix+00ah)		;11d7
	pop ix		;11db
	ret			;11dd
l11deh:
	pop hl			;11de
	pop hl			;11df
	ld (ix+00ah),000h		;11e0
	pop ix		;11e4
	ret			;11e6
	pop hl			;11e7
	pop hl			;11e8
	bit 6,(ix+00ah)		;11e9
	jr nz,l11f2h		;11ed
	inc (ix+00ah)		;11ef
l11f2h:
	pop ix		;11f2
	ret			;11f4
	ld (bc),a			;11f5
	ld d,h			;11f6
	ld d,h			;11f7
	ex af,af'			;11f8
	nop			;11f9
	jp sub_0c13h		;11fa
	ld (bc),a			;11fd
	ld d,h			;11fe
	ld d,e			;11ff
	inc c			;1200
	nop			;1201
	ld hl,00032h		;1202
	rst 8			;1205
	jp sub_0b00h		;1206
	ld (bc),a			;1209
	ld d,h			;120a
	ld c,l			;120b
	inc c			;120c
	nop			;120d
	ld hl,00bb8h		;120e
	rst 8			;1211
	jp sub_0b00h		;1212
	ld (bc),a			;1215
	ld d,h			;1216
	ld c,b			;1217
	inc d			;1218
	nop			;1219
	call sub_0c13h		;121a
	ld de,l0002h		;121d
	ld hl,0bf20h		;1220
l1223h:
	call STACK_DE_HL		;1223
	jp DSTAR		;1226
	ld (bc),a			;1229
	ld d,h			;122a
	ld b,h			;122b
	djnz l122eh		;122c
l122eh:
	call sub_0c13h		;122e
	ld de,0x0041		;1231
	ld hl,0eb00h		;1234
	jr l1223h		;1237
	ld (bc),a			;1239
	ld d,h			;123a
	ld d,a			;123b
	djnz l123eh		;123c
l123eh:
	call sub_0c13h		;123e
	ld de,l01cdh		;1241
	ld hl,06d00h		;1244
	jr l1223h		;1247
	ld (bc),a			;1249
	ld d,h			;124a
	ld e,c			;124b
	djnz l124eh		;124c
l124eh:
	call sub_0c13h		;124e
	ld de,05dfch		;1251
	ld hl,l0f00h		;1254
	jr l1223h		;1257
	add a,h			;1259
	ld b,e			;125a
	ld b,c			;125b
	ld d,e			;125c
	ld b,l			;125d
	and a			;125e
	nop			;125f
	ld hl,l128bh		;1260
	call sub_07d3h		;1263
	ld hl,(0fc8ah)		;1266
	push hl			;1269
	call sub_07c4h		;126a
	ld de,l0e68h		;126d
l1270h:
	call sub_06e3h		;1270
	call sub_12a2h		;1273
	jr z,l127dh		;1276
	call sub_07c4h		;1278
	jr l1270h		;127b
l127dh:
	ld hl,(0fc8ah)		;127d
	dec hl			;1280
	dec hl			;1281
	pop de			;1282
	or a			;1283
	sbc hl,de		;1284
	ex de,hl			;1286
	ld (hl),e			;1287
	inc hl			;1288
	ld (hl),d			;1289
	ret			;128a
l128bh:
	pop hl			;128b
	ld e,(hl)			;128c
	inc hl			;128d
	ld d,(hl)			;128e
	inc hl			;128f
	push hl			;1290
	add hl,de			;1291
	ex (sp),hl			;1292
	push hl			;1293
	rst 10h			;1294
	add hl,hl			;1295
	call sub_12a2h		;1296
	jr nc,l12a0h		;1299
	pop de			;129b
	add hl,de			;129c
	jp JP_ADDR_HL		;129d
l12a0h:
	pop hl			;12a0
	ret			;12a1
sub_12a2h:
	push hl			;12a2
	or a			;12a3
	sbc hl,de		;12a4
	pop hl			;12a6
	ret			;12a7
l12a8h:
	ld hl,l0000h		;12a8
	add hl,sp			;12ab
l12ach:
	ld de,0fc5ch		;12ac
	or a			;12af
	call sub_12d7h		;12b0
	jp nc,l11c2h		;12b3
	call sub_12e7h		;12b6
	ld e,068h		;12b9
	call sub_12e7h		;12bb
	jr l12ach		;12be
sub_12c0h:
	call sub_12f8h		;12c0
sub_12c3h:
	push hl			;12c3
	push de			;12c4
	ld bc,l0002h+2		;12c5
	add hl,bc			;12c8
	ex de,hl			;12c9
	add hl,bc			;12ca
	ld b,c			;12cb

l12cch:	dec hl			;12cc
	dec de			;12cd
	ld a,(de)			;12ce
	cp (hl)			;12cf
	jr nz,l12d4h		;12d0
	djnz l12cch		;12d2

l12d4h:	pop de			;12d4
	pop hl			;12d5
	ret			;12d6

	;; Handles system clock?
sub_12d7h:
	push hl			;12d7
	push de			;12d8
	push bc			;12d9
	ld b,004h		;12da
l12dch:	ld a,(de)			;12dc
	adc a,(hl)			;12dd
	ld (hl),a			;12de
	inc hl			;12df
	inc de			;12e0
	djnz l12dch		;12e1
	pop bc			;12e3
	pop de			;12e4
	pop hl			;12e5
	ret			;12e6
sub_12e7h:
	push hl			;12e7
	push de			;12e8
	push bc			;12e9
	ld b,004h		;12ea
	ex de,hl			;12ec
l12edh:
	ld a,(de)			;12ed
	sbc a,(hl)			;12ee
	ld (de),a			;12ef
	inc hl			;12f0
	inc de			;12f1
	djnz l12edh		;12f2
	pop bc			;12f4
	pop de			;12f5
	pop hl			;12f6
	ret			;12f7
sub_12f8h:
	push iy		;12f8
	pop de			;12fa
	ld hl,l0002h+2		;12fb
	add hl,de			;12fe
	ret			;12ff
	ld (bc),a			;1300
	ld b,h			;1301
	dec hl			;1302
	ld (de),a			;1303
	nop			;1304
sub_1305h:
	push hl			;1305
	push de			;1306
	call sub_12f8h		;1307
	call sub_12d7h		;130a
	rst 10h			;130d
	rst 10h			;130e
	pop de			;130f
	pop hl			;1310
	ret			;1311
	ld (bc),a			;1312
	ld b,h			;1313
	dec l			;1314
	ld (de),a			;1315
	nop			;1316
	push hl			;1317
	push de			;1318
	call sub_12f8h		;1319
	call sub_12e7h		;131c
	rst 10h			;131f
	rst 10h			;1320
	pop de			;1321
	pop hl			;1322
	ret			;1323
	inc bc			;1324
	ld b,e			;1325
	ld c,h			;1326
	ld d,e			;1327
	dec bc			;1328
	nop			;1329
	ld a,00ch		;132a
	jp PRINT_A		;132c
	inc bc			;132f
	ld b,c			;1330
	ld b,d			;1331
	ld d,e			;1332
	inc c			;1333
	nop			;1334
	rst 10h			;1335
	call sub_0b19h		;1336
	rst 8			;1339
	ret			;133a
	ld b,044h		;133b
	ld c,l			;133d
	ld c,c			;133e
	ld c,(hl)			;133f
	ld d,l			;1340
	ld d,e			;1341
	ld (de),a			;1342
	nop			;1343
	call sub_0b9bh		;1344
	call sub_0b2dh		;1347
	jp STACK_DE_HL		;134a
	inc b			;134d
	ld b,h			;134e
	ld b,c			;134f
	ld b,d			;1350
	ld d,e			;1351
	djnz l1354h		;1352
l1354h:
	call sub_0b9bh		;1354
	call sub_0b28h		;1357
	jp STACK_DE_HL		;135a
	ld (bc),a			;135d
	ld b,e			;135e
	ld b,b			;135f
	dec bc			;1360
	nop			;1361
	rst 10h			;1362
	ld l,(hl)			;1363
	ld h,000h		;1364
	rst 8			;1366
	ret			;1367
	ld (bc),a			;1368
	ld b,e			;1369
	ld hl,l000ah+1		;136a
	rst 10h			;136d
	ex de,hl			;136e
	rst 10h			;136f
	ld a,l			;1370
	ld (de),a			;1371
	ret			;1372
	inc b			;1373
	ld b,h			;1374
	ld d,d			;1375
	ld c,a			;1376
	ld d,b			;1377
	add hl,bc			;1378
	nop			;1379
l137ah:
	rst 10h			;137a
	ret			;137b
	ld b,042h		;137c
	ld c,h			;137e
	ld b,c			;137f
	ld c,(hl)			;1380
	ld c,e			;1381
	ld d,e			;1382
	rrca			;1383
	nop			;1384
	rst 10h			;1385
	ex de,hl			;1386
	ld c,000h		;1387
	jr l1396h		;1389
	inc b			;138b
	ld b,(hl)			;138c
	ld c,c			;138d
	ld c,h			;138e
	ld c,h			;138f
	inc d			;1390
	nop			;1391
	rst 10h			;1392
	ld c,l			;1393
	rst 10h			;1394
	ex de,hl			;1395
l1396h:
	rst 10h			;1396
l1397h:
	ld (hl),c			;1397
	inc hl			;1398
	dec de			;1399
	ld a,e			;139a
	or d			;139b
	jr nz,l1397h		;139c
	ret			;139e
	inc b			;139f
	ld b,e			;13a0
	ld c,a			;13a1
	ld d,b			;13a2
	ld e,c			;13a3
	dec c			;13a4
	nop			;13a5
	ld hl,l0200h		;13a6
	push hl			;13a9
	jr l13b6h		;13aa
	inc b			;13ac
	ld c,l			;13ad
	ld c,a			;13ae
	ld d,(hl)		;13af
	ld b,l			;13b0
	ld b,e			;13b1
	nop			;13b2
	rst 10h			;13b3
	add hl,hl		;13b4
	push hl			;13b5

l13b6h: rst 10h			;13b6
	ex de,hl		;13b7
	rst 10h			;13b8
	pop bc			;13b9
	jp sub_036ah		;13ba

	call sub_13d2h		;13bd
	call CHECK_KIB		;13c0
	call z,sub_048dh	;13c3
	call sub_18fah		;13c6
	call sub_1036h		;13c9

	;; Set to indicate warm restart is possible
	ld hl,F_WARM_RESTART	;13cc
	ld (hl),0xFF		;13cf

	ret			;13d1

sub_13d2h:
	push hl			;13d2
	push de			;13d3
	ld hl,0fc58h		;13d4
	ld de,0xFC60		;13d7
	call sub_12d7h		;13da
	ld l,05ch		;13dd
	ld e,064h		;13df
	call sub_12d7h		;13e1
	ld e,068h		;13e4
	call sub_12c3h		;13e6
	call nc,sub_12e7h		;13e9
	pop de			;13ec
	pop hl			;13ed
	ret			;13ee

	ld b,053h		;13ef
	ld b,e			;13f1
	ld d,d			;13f2
	ld b,l			;13f3
	ld b,l			;13f4
	ld c,(hl)			;13f5
	inc l			;13f6
	nop			;13f7
	call sub_0866h		;13f8
	ld hl,l0fdeh		;13fb
	call sub_07d3h		;13fe
	ld hl,l0000h		;1401
	push hl			;1404
	call sub_07c4h		;1405
	rst 10h			;1408
	ld d,l			;1409
	rst 10h			;140a
	ld e,l			;140b
	rst 10h			;140c
	ld a,l			;140d
	rst 10h			;140e
	ld h,a			;140f
	call sub_07c4h		;1410
	ex de,hl		;1413
	call sub_07c4h		;1414
	pop hl			;1417
	jp sub_07c4h		;1418
	ld (bc),a		;141b
	ld l,043h		;141c
	ld de,0d700h		;141e
	push hl			;1421
	ex (sp),ix		;1422
	rst 10h			;1424
	ld a,l			;1425
	call SCR_PR_CHR		;1426
	pop ix			;1429
	ret			;142b
	ld (bc),a		;142c
	ld l,057h		;142d
	ld a,(de)		;142f
	nop			;1430
	rst 10h			;1431
	push hl			;1432
	ex (sp),ix		;1433
	rst 10h			;1435
	ld a,l			;1436
	and 03fh		;1437
	jr z,l1443h		;1439
	ld l,a			;143b

l143ch:	rst 20h			;143c
	call SCR_PR_CHR		;143d
	dec l			;1440
	jr nz,l143ch		;1441

l1443h:	pop ix			;1443
	ret			;1445
	ld bc,00723h		;1446
	nop			;1449
	jp sub_0ad3h		;144a
	ld (bc),a		;144d
	ld b,h			;144e
	inc hl			;144f
	ex af,af'		;1450
	nop			;1451
	jp sub_0a96h		;1452
	ld (bc),a		;1455
	ld b,l			;1456
	ld b,h			;1457
	ld a,(bc)		;1458
	nop			;1459
	ld hl,SCR_INFO_ED	;145a
	rst 8			;145d
	ret			;145e
	ld (bc),a		;145f
	ld b,e			;1460
	ld c,a			;1461
	ld a,(bc)			;1462
	nop			;1463
	ld hl,SCR_INFO_CO	;1464
	rst 8			;1467
	ret			;1468
	ld (bc),a			;1469
	ld d,a			;146a
	ld b,b			;146b
	add hl,bc			;146c
	nop			;146d
	rst 10h			;146e
	jp PUSH_STRING		;146f
	inc bc			;1472
	ld b,e			;1473
	ld a,04eh		;1474
	jr l1478h		;1476
l1478h:
	ld de,0ffffh		;1478
	xor a			;147b
	rst 10h			;147c
	add hl,de			;147d
	jr c,l1483h		;147e
	inc hl			;1480
	jr l1484h		;1481
l1483h:
	rst 20h			;1483
l1484h:
	rst 8			;1484
	ld h,000h		;1485
	ld l,a			;1487
	rst 8			;1488
	ret			;1489
	inc bc			;148a
	ld c,(hl)			;148b
	ld a,043h		;148c
	dec c			;148e
	nop			;148f
	rst 10h			;1490
	ld a,l			;1491
	rst 18h			;1492
	rst 10h			;1493
	inc hl			;1494
	rst 8			;1495
	ret			;1496
	add a,c			;1497
	ld (0003bh),hl		;1498
	ld hl,l14beh		;149b
l149eh:
	call sub_07d3h		;149e
	ld hl,(0fc8ah)		;14a1
	ld a,0ffh		;14a4
l14a6h:
	call sub_07b9h		;14a6
	inc (hl)			;14a9
	or a			;14aa
	call sub_14c5h		;14ab
	cp 022h		;14ae
	jr nz,l14a6h		;14b0
	ret			;14b2
sub_14b3h:
	pop de			;14b3
	pop hl			;14b4
	push de			;14b5
	ld e,(hl)			;14b6
	ld d,000h		;14b7
	ex de,hl			;14b9
	add hl,de			;14ba
	inc hl			;14bb
	ex de,hl			;14bc
	ret			;14bd
l14beh:
	call sub_14b3h		;14be
	push de			;14c1
	jp PUSH_STRING		;14c2

sub_14c5h:
	call SWITCH_TO_MSTACK1	;14c5
	and 07fh		;14c8
	call PRINT_A		;14ca
	cp 01fh		;14cd
	jr z,sub_14c5h		;14cf
	ret			;14d1
	add a,d			;14d2
	ld l,022h		;14d3
	ld de,02100h		;14d5
	call c,sub_1814h		;14d8
	jp nz,0b3cdh		;14db
	inc d			;14de
	push de			;14df
	jp PRINT_STR_HL		;14e0
	add a,(hl)			;14e3
	ld b,c			;14e4
	ld b,d			;14e5
	ld c,a			;14e6
	ld d,d			;14e7
	ld d,h			;14e8
	ld (0x0086),hl		;14e9 - ??? ROM
	ld hl,l14f1h		;14ec
	jr l149eh		;14ef
l14f1h:
	call sub_14b3h		;14f1
	push de			;14f4
	push hl			;14f5
	rst 10h			;14f6
	ld a,l			;14f7
	or h			;14f8
	pop hl			;14f9
	ret z			;14fa
	call PRINT_STR_HL		;14fb
	jp WARM_RESTART		;14fe
sub_1501h:
	push af			;1501
	in a,(0feh)		;1502
	ld a,010h		;1504
l1506h:
	dec a			;1506
	jr nz,l1506h		;1507
	out (0ffh),a		;1509
	ld a,04bh		;150b
l150dh:
	dec a			;150d
	jr nz,l150dh		;150e
	pop af			;1510
	ret			;1511
sub_1512h:
	push bc			;1512
	ld b,072h		;1513
l1515h:
	djnz l1515h		;1515
	pop bc			;1517
	ret			;1518
sub_1519h:
	push af			;1519
	push bc			;151a
	ld b,00ah		;151b
	ld a,(hl)			;151d
	scf			;151e
l151fh:
	call sub_1501h		;151f
	call c,sub_1501h		;1522
	call nc,sub_1512h		;1525
	add a,a			;1528
	djnz l151fh		;1529
	pop bc			;152b
	pop af			;152c
	ret			;152d
sub_152eh:
	push bc			;152e
	push hl			;152f
	ld hl,l153dh		;1530
	ld b,07dh		;1533
l1535h:
	call sub_1519h		;1535
	djnz l1535h		;1538
	pop hl			;153a
	pop bc			;153b
	ret			;153c
l153dh:
	nop			;153d
l153eh:
	and l			;153e
sub_153fh:
	ld de,l0200h		;153f
	push bc			;1542
	push hl			;1543
	ld bc,0ffffh		;1544
l1547h:
	call sub_1519h		;1547
	inc hl			;154a
	ex de,hl			;154b
	add hl,bc			;154c
	ex de,hl			;154d
	jr c,l1547h		;154e
	pop hl			;1550
	pop bc			;1551
	ret			;1552
sub_1553h:
	push hl			;1553
	ld hl,l153eh		;1554
	call sub_1519h		;1557
	pop hl			;155a
	ret			;155b
sub_155ch:
	push hl			;155c
	ld hl,0fc52h		;155d
	call sub_1519h		;1560
	inc hl			;1563
	call sub_1519h		;1564
	pop hl			;1567
	ret			;1568
	dec b			;1569
	ld d,e			;156a
	ld d,h			;156b
	ld c,a			;156c
	ld d,d			;156d
	ld b,l			;156e
	sub a			;156f
	nop			;1570
	ld hl,FLAGS		;1571
	bit 6,(hl)		;1574
	ret z			;1576
	rst 10h			;1577
	ld (0fc52h),hl		;1578

l157bh:	out (0fdh),a		;157b

	ld hl,(0fc76h)		;157d
	call sub_152eh		;1580
	call sub_1553h		;1583
	call sub_155ch		;1586
	call sub_153fh		;1589
	call sub_152eh		;158c

	out (0feh),a		;158f

	ret			;1591
l1592h:
	call sub_15a5h		;1592
	jr c,l159ch		;1595
	cp 0a5h		;1597
	jr nz,l1592h		;1599
	ret			;159b
l159ch:
	ld a,07fh		;159c
	in a,(0feh)		;159e
	rrca			;15a0
	ccf			;15a1
	jr nc,l1592h		;15a2
	ret			;15a4
sub_15a5h:
	push de			;15a5
	push bc			;15a6
	ld e,000h		;15a7
l15a9h:
	ld bc,l0000h		;15a9
l15ach:
	ld a,07fh		;15ac
	in a,(0feh)		;15ae
	bit 0,a		;15b0
	jr z,l15b9h		;15b2
	rlca			;15b4
	jr c,l15bdh		;15b5
	djnz l15ach		;15b7
l15b9h:
	pop bc			;15b9
	pop de			;15ba
	scf			;15bb
	ret			;15bc
l15bdh:
	ld b,03eh		;15bd
l15bfh:
	djnz l15bfh		;15bf
	ld b,025h		;15c1
l15c3h:
	in a,(0feh)		;15c3
	rlca			;15c5
	ld a,c			;15c6
	adc a,000h		;15c7
	ld c,a			;15c9
	djnz l15c3h		;15ca
	cp 004h		;15cc
	ccf			;15ce
	rl e		;15cf
	jr nc,l15a9h		;15d1
	pop bc			;15d3
	ld a,e			;15d4
	pop de			;15d5
	or a			;15d6
	ret			;15d7
sub_15d8h:
	ld b,000h		;15d8
	call sub_15ddh		;15da
sub_15ddh:
	ld (hl),000h		;15dd
	inc hl			;15df
	djnz sub_15ddh		;15e0
	ret			;15e2
sub_15e3h:
	push de			;15e3
	push hl			;15e4
l15e5h:
	call sub_15a5h		;15e5
	jr c,l15f2h		;15e8
	ld (hl),a			;15ea
	inc hl			;15eb
	dec e			;15ec
	jr nz,l15e5h		;15ed
	dec d			;15ef
	jr nz,l15e5h		;15f0
l15f2h:
	pop hl			;15f2
	pop de			;15f3
	ret			;15f4
sub_15f5h:
	ld b,000h		;15f5
	call sub_15fah		;15f7
sub_15fah:
	call sub_15a5h		;15fa
	djnz sub_15fah		;15fd
	ret			;15ff
	inc b			;1600
	ld c,h			;1601
	ld c,a			;1602
	ld b,c			;1603
	ld b,h			;1604
	ld d,h			;1605
	nop			;1606
	ld hl,FLAGS		;1607
	bit 6,(hl)		;160a
	ret z			;160c
	rst 10h			;160d
	ld (0fc52h),hl		;160e
l1611h:
	out (0fdh),a		;1611
	ld hl,(0fc76h)		;1613
	push hl			;1616
	call sub_15d8h		;1617
	call l1592h		;161a
	jr c,l1651h		;161d
	call sub_15a5h		;161f
	ld l,a			;1622
	call sub_15a5h		;1623
	ld h,a			;1626
	or l			;1627
	jr nz,l162dh		;1628
	ld (0fc52h),hl		;162a
l162dh:
	ex de,hl			;162d
	ld hl,(0fc52h)		;162e
	ld a,l			;1631
	or h			;1632
	jr nz,l163bh		;1633
	ex de,hl			;1635
	ld (0fc52h),hl		;1636
	jr l163dh		;1639
l163bh:
	sbc hl,de		;163b
l163dh:
	pop hl			;163d
	jr nz,l164ch		;163e
	ld de,l0200h		;1640
	call sub_15e3h		;1643
	call sub_168ch		;1646
l1649h:
	out (0feh),a		;1649
	ret			;164b
l164ch:
	call sub_15f5h		;164c
	jr l1611h		;164f
l1651h:
	pop hl			;1651
	jr l1649h		;1652
	inc bc			;1654
	dec l			;1655
	dec l			;1656
	ld a,00bh		;1657
	nop			;1659
	call sub_166bh		;165a
	jr l1611h		;165d
	inc bc			;165f
	inc a			;1660
	dec l			;1661
	dec l			;1662
	dec h			;1663
	nop			;1664
	call sub_166bh		;1665
	jp l157bh		;1668
sub_166bh:
	ld hl,(0fc52h)		;166b
	ld a,l			;166e
	or h			;166f
	ret z			;1670
	inc hl			;1671
	ld (0fc52h),hl		;1672
	ret			;1675
sub_1676h:
	push hl			;1676
	push de			;1677
	ld a,h			;1678
	xor d			;1679
	bit 7,a		;167a
	jr z,l167fh		;167c
	ex de,hl			;167e
l167fh:
	sbc hl,de		;167f
	pop de			;1681
	pop hl			;1682
	ret			;1683
	inc bc			;1684
	ld b,e			;1685
	ld d,b			;1686
	ld c,h			;1687
	inc d			;1688
	nop			;1689
	jr l1692h		;168a
sub_168ch:
	ld hl,FLAGS3		;168c
	bit 2,(hl)		;168f
	ret z			;1691
l1692h:
	ld hl,FLAGS3		;1692
	set 1,(hl)		;1695
	ret			;1697
	inc bc			;1698
	ld b,e			;1699
	ld c,a			;169a
	ld c,(hl)			;169b
	inc c			;169c
	nop			;169d
	ld hl,FLAGS3		;169e
	set 2,(hl)		;16a1
	ret			;16a3
	inc b			;16a4
	ld b,e			;16a5
	ld c,a			;16a6
	ld b,(hl)			;16a7
	ld b,(hl)			;16a8
	dec c			;16a9
	nop			;16aa
	ld hl,FLAGS3		;16ab
	res 2,(hl)		;16ae
	ret			;16b0
	ld bc,l113eh		;16b1
	nop			;16b4
	rst 10h			;16b5
	ex de,hl			;16b6
	rst 10h			;16b7
	ex de,hl			;16b8
l16b9h:
	call sub_1676h		;16b9
l16bch:
	ccf			;16bc
l16bdh:
	sbc hl,hl		;16bd
	inc hl			;16bf
	rst 8			;16c0
	ret			;16c1
	ld bc,0093ch		;16c2
	nop			;16c5
	rst 10h			;16c6
	ex de,hl			;16c7
	rst 10h			;16c8
	jr l16b9h		;16c9
	ld bc,00f3dh		;16cb
	nop			;16ce
	rst 10h			;16cf
	ex de,hl			;16d0
	rst 10h			;16d1
	call sub_1676h		;16d2
l16d5h:
	jr z,l16bdh		;16d5
	or a			;16d7
	jr l16bch		;16d8
	ld (bc),a			;16da
	ld b,e			;16db
	dec a			;16dc
	dec bc			;16dd
	nop			;16de
	rst 10h			;16df
	ld a,l			;16e0
	rst 10h			;16e1
	cp l			;16e2
	jr l16d5h		;16e3
	ld bc,l0a3fh		;16e5
	nop			;16e8
	call sub_0eebh		;16e9
	jp l0e7bh		;16ec
	ld (bc),a			;16ef
	dec hl			;16f0
	ld hl,l0010h+2		;16f1
	rst 10h			;16f4
	push hl			;16f5
	ld e,(hl)			;16f6
	inc hl			;16f7
	ld d,(hl)			;16f8
	rst 10h			;16f9
	add hl,de			;16fa
	ex de,hl			;16fb
	pop hl			;16fc
	ld (hl),e			;16fd
	inc hl			;16fe
	ld (hl),d			;16ff
	ret			;1700
	ld (bc),a			;1701
	dec hl			;1702
	dec l			;1703
	inc d			;1704
	nop			;1705
	rst 10h			;1706
	bit 7,h		;1707
	push af			;1709
	rst 10h			;170a
	call sub_0b19h		;170b
	pop af			;170e
	ret z			;170f - BUG fix - `nop`
	call sub_0b1eh		;1710 - BUG fix - `call nz, ...`
	rst 8			;1713
	ret			;1714
	ld bc,0082ch		;1715
	nop			;1718
	rst 10h			;1719
	jp sub_07c4h		;171a
	ld (bc),a			;171d
	ld b,e			;171e
	inc l			;171f
	ld a,(bc)			;1720
	nop			;1721
	rst 10h			;1722
	ld a,l			;1723
	jp sub_07b9h		;1724
	inc bc			;1727
	ld d,e			;1728
	ld d,b			;1729
	ld b,b			;172a
	dec bc			;172b
	nop			;172c
	push iy		;172d
	pop hl			;172f
	rst 8			;1730
	ret			;1731
	dec b			;1732
	ld d,(hl)			;1733
	ld c,h			;1734
	ld c,c			;1735
	ld d,e			;1736
	ld d,h			;1737
	rra			;1738
	nop			;1739
	ld hl,(START_OF_DICT)		;173a
	ld de,sub_0008h		;173d
l1740h:
	call PUSH_STRING	;1740
	call sub_1758h		;1743
	call FIND_NEXT_WORD		;1746
	call PRINT_STRING		;1749
	xor a			;174c
	cp h			;174d
	jr nz,l1740h		;174e
	ret			;1750
	ld (bc),a			;1751
	ld d,a			;1752
	ld a,020h		;1753
	nop			;1755
	rst 10h			;1756
	ex de,hl			;1757
sub_1758h:
	push hl			;1758
	push de			;1759
	rst 10h			;175a
	call sub_1676h		;175b
	jr nc,l176eh		;175e
	ex de,hl			;1760
	rst 8			;1761
	ccf			;1762
	sbc hl,de		;1763
	ld a,020h		;1765
l1767h:
	rst 18h			;1767
	dec l			;1768
	jr nz,l1767h		;1769
l176bh:
	pop de			;176b
	pop hl			;176c
	ret			;176d
l176eh:
	rst 8			;176e
	jr l176bh		;176f
	add a,e			;1771
	ld c,c			;1772
	ld c,l			;1773
	ld c,l			;1774
	inc c			;1775
	nop			;1776
l1777h:
	ld hl,(0fc8eh)		;1777
	set 7,(hl)		;177a
	ret			;177c

	;; Forth word INTEGER
	db 0x07, 0x49, 0x4E, 0x54, 0x45, 0x47, 0x45, 0x52
	db 0x38, 0x00

l1787h:	call sub_0866h		;1787
	ld hl,l1797h		;178a
	call sub_07d3h		;178d
	rst 10h			;1790
	call sub_07c4h		;1791
	jp l1777h		;1794

	
	;; Jump here from definition of BASE
l1797h:	ld hl,FLAGS3		;1797 - FLAGs
	bit 5,(hl)		;179a
	res 5,(hl)		;179c
	pop hl			;179e
	jr z,l17abh		;179f
	ld a,0d7h		;17a1
	call sub_07b9h		;17a3
	ld a,022h		;17a6
	jp l07d5h		;17a8

l17abh:	ld a,02ah		;17ab
	call l07d5h		;17ad
	ld a,0cfh		;17b0
	jp sub_07b9h		;17b2


	add a,d			;17b5
	ld d,h			;17b6
	ld c,a			;17b7
	dec bc			;17b8
	nop			;17b9
	ld hl,FLAGS3		;17ba
	set 5,(hl)		;17bd
	ret			;17bf
	add a,l			;17c0
	ld b,d			;17c1
	ld b,l			;17c2
	ld b,a			;17c3
	ld c,c			;17c4
	ld c,(hl)		;17c5
	rrca			;17c6
	nop			;17c7
	ld hl,(0fc8ah)		;17c8
	push hl			;17cb
	call l0881h		;17cc
	add a,l			;17cf
	ld b,c			;17d0
	ld b,a			;17d1
	ld b,c			;17d2
	ld c,c			;17d3
	ld c,(hl)		;17d4
	rrca			;17d5
	nop			;17d6
	pop hl			;17d7
	pop hl			;17d8
	pop hl			;17d9
	pop hl			;17da
	jp sub_07cfh		;17db
	add a,l			;17de
	ld d,a			;17df
	ld c,b			;17e0
	ld c,c			;17e1
	ld c,h			;17e2
	ld b,l			;17e3
	ld e,000h		;17e4
	pop de			;17e6
	pop hl			;17e7
	pop hl			;17e8
	ld hl,l182bh		;17e9
	call sub_07d3h		;17ec
	ld hl,(0fc8ah)		;17ef
	ld a,0c2h		;17f2
	call l07d5h		;17f4
	inc hl			;17f7
	push hl			;17f8
	push hl			;17f9
	ex de,hl			;17fa
	jp (hl)			;17fb
	add a,(hl)			;17fc
	ld d,d			;17fd
	ld b,l			;17fe
	ld d,b			;17ff
	ld b,l			;1800
	ld b,c			;1801
	ld d,h			;1802
	jr l1805h		;1803
l1805h:
	pop hl			;1805
	pop hl			;1806
	pop de			;1807
	pop hl			;1808
l1809h:
	call sub_07cfh		;1809
	ld hl,(0fc8ah)		;180c
	ex de,hl			;180f
	ld (hl),e			;1810
	inc hl			;1811
	ld (hl),d			;1812
	ret			;1813
sub_1814h:
	add a,l			;1814
	ld d,l			;1815
	ld c,(hl)			;1816
	ld d,h			;1817
	ld c,c			;1818
	ld c,h			;1819
	dec e			;181a
	nop			;181b
	pop hl			;181c
	pop hl			;181d
	pop hl			;181e
	ld hl,l182bh		;181f
	call sub_07d3h		;1822
	pop hl			;1825
	ld a,0cah		;1826
	jp l07d5h		;1828
l182bh:
	rst 10h			;182b
	xor a			;182c
	cp l			;182d
	ret nz			;182e
	cp h			;182f
	ret			;1830
	inc bc			;1831
	ld c,e			;1832
	ld b,l			;1833
	ld e,c			;1834
	add hl,de			;1835
	nop			;1836
l1837h:
	ld a,01fh		;1837
	call PRINT_A		;1839
	call SWITCH_TO_MSTACK1	;183c
	and 07fh		;183f
	cp 01fh		;1841
	jr z,l1837h		;1843
	ld h,000h		;1845
	ld l,a			;1847
	rst 8			;1848
	ret			;1849
	add a,c			;184a
	jr z,l185bh		;184b
	nop			;184d
l184eh:
	call sub_14c5h		;184e
	and 07fh		;1851
	cp 029h		;1853
	jr nz,l184eh		;1855
	ret			;1857
	ld (bc),a			;1858
	jr nc,$+62		;1859
l185bh:
	dec bc			;185b
	nop			;185c
	rst 10h			;185d
	rlc h		;185e
	jp l16bch		;1860
	ld (bc),a			;1863
	jr nc,$+64		;1864
	dec bc			;1866
	nop			;1867
	rst 10h			;1868
	rlc h		;1869
l186bh:
	jp l16bdh		;186b
	ld (bc),a			;186e
	jr nc,l18aeh		;186f
	dec c			;1871
	nop			;1872
	rst 10h			;1873
	xor a			;1874
	cp l			;1875
	jr c,l186bh		;1876
	cp h			;1878
	jr l186bh		;1879
	ld (bc),a			;187b
	ld (l092ah),a		;187c
	nop			;187f
	rst 10h			;1880
	add hl,hl			;1881
	rst 8			;1882
	ret			;1883
	ld (bc),a			;1884
	ld (l0c2fh),a		;1885
	nop			;1888
	rst 10h			;1889
	sra h		;188a
	rr l		;188c
	rst 8			;188e
	ret			;188f
	ld (bc),a			;1890
	ld d,a			;1891
	ld hl,sub_0008h+1		;1892
	rst 10h			;1895
	jp sub_0687h		;1896
	inc b			;1899
	ld b,l			;189a
	ld c,a			;189b
	ld b,(hl)			;189c
	ld b,(hl)			;189d
	ld d,000h		;189e
	ld hl,FLAGS		;18a0
	res 6,(hl)		;18a3
	ld l,0b9h		;18a5
	ld a,(hl)			;18a7
	ld (hl),000h		;18a8
	ld l,0b7h		;18aa
	add a,(hl)			;18ac
	ld (hl),a			;18ad
l18aeh:
	ret			;18ae

	;; FORTH word BASE 
	db 0x84, 0x42, 0x41, 0x53, 0x45
	db 0x0E, 0x00
	
	ld hl,BASE		;18b6
	push hl			;18b9
	jp l1797h		;18ba


	rlca			;18bd
	ld b,h			;18be
	ld b,l			;18bf
	ld b,e			;18c0
	ld c,c			;18c1
	ld c,l			;18c2
	ld b,c			;18c3
	ld c,h			;18c4
	djnz l18c7h		;18c5
l18c7h:
	ld hl,BASE		;18c7
	ld (hl),00ah		;18ca
	ret			;18cc
	inc b			;18cd
	ld b,d			;18ce
	ld b,c			;18cf
	ld b,e			;18d0
	ld c,e			;18d1
	ld c,000h		;18d2
	call sub_06e3h		;18d4
	ld (P_MTASK),hl		;18d7
	ret			;18da

	db 0x03, 0x4D, 0x45, 0x4D	; MEM
	db 0x3D, 0x00
	call sub_18e6h		;18e1
	rst 8			;18e4
	ret			;18e5

sub_18e6h:
	ld hl,(0fc8ah)		;18e6
	ex de,hl		;18e9
	ld hl,(RAM_SIZE)	;18ea
	push iy			;18ed
	or a			;18ef
	sbc hl,de		;18f0
	pop de			;18f2
	add hl,de			;18f3
	ex de,hl			;18f4
	ld hl,(RAM_START)		;18f5
	add hl,de			;18f8
	ret			;18f9

sub_18fah:
	ld hl,FLAGS3		;18fa
	bit 3,(hl)		;18fd
	ret nz			;18ff
	call sub_18e6h		;1900
	ld de,0ffe0h		;1903
	add hl,de			;1906
	ex de,hl			;1907
	ld hl,(RAM_SIZE)		;1908
	sbc hl,de		;190b
	ret nc			;190d
	ld hl,FLAGS3		;190e
	set 3,(hl)		;1911
	ld a,04dh		;1913
	jp sub_06c9h		;1915
	dec b			;1918
	ld b,(hl)			;1919
	ld b,l			;191a
	ld c,(hl)			;191b
	ld b,e			;191c
	ld b,l			;191d
	ld e,000h		;191e
	call sub_1927h		;1920
	ld (0fc72h),hl		;1923
	ret			;1926
sub_1927h:
	call sub_0782h		;1927
	push af			;192a
	call l0d6bh		;192b
	pop af			;192e
	ret nc			;192f
	pop hl			;1930
	ld a,055h		;1931
	jp sub_06c9h		;1933
	ld b,046h		;1936
	ld c,a			;1938
	ld d,d			;1939
	ld b,a			;193a
	ld b,l			;193b
	ld d,h			;193c
	ld c,e			;193d
	nop			;193e
	call sub_1927h		;193f
	ex de,hl			;1942
	ld hl,(0fc72h)		;1943
	call sub_12a2h		;1946
	ld a,046h		;1949
	jp nc,sub_06c9h		;194b
	ld hl,(P_MTASK)		;194e
	call sub_12a2h		;1951
	jr c,l195ch		;1954
	ld hl,l19b4h		;1956
	ld (P_MTASK),hl		;1959
l195ch:
	ld hl,(0fc70h)		;195c
l195fh:
	push hl			;195f
	pop bc			;1960
	ld (0fc92h),hl		;1961
	ld a,(hl)		;1964
	inc hl			;1965
	ld h,(hl)		;1966
	ld l,a			;1967
	or h			;1968
	jr z,l1974h		;1969
	call sub_12a2h		;196b
	jr c,l195fh		;196e
	xor a			;1970
	ld (bc),a		;1971
	inc bc			;1972
	ld (bc),a		;1973
l1974h:
	ex de,hl		;1974
	ld (0fc8eh),hl		;1975
	call FIND_NEXT_WORD	;1978
	ld (START_OF_DICT_DEF),hl ;197b
	jp sub_0765h		;197e

	;; FORTH word TOFF
	db 0x04, 0x54, 0x4F, 0x46, 0x46
	db 0x0D, 0x00

	ld hl,FLAGS3		;1988
	res 6,(hl)		;198b

	ret			;198d

	;; FORTH word TON
	db 0x03, 0x54, 0x4F, 0x4E
	db 0x0C, 0x00

	ld hl,FLAGS3		;1994
	set 6,(hl)		;1997

	ret			;1999

	inc b			;199a
	ld d,a			;199b
	ld b,c			;199c
	ld d,d			;199d
	ld c,l			;199e
	ld a,(bc)			;199f
	nop			;19a0
	jp WARM_RESTART		;19a1
	inc b			;19a4
	ld b,e			;19a5
	ld c,a			;19a6
	ld c,h			;19a7
	ld b,h			;19a8
	ld a,(bc)			;19a9
	nop			;19aa
	jp COLD_RESTART		;19ab
	inc bc			;19ae
	ld c,(hl)			;19af
	ld d,l			;19b0
	ld c,h			;19b1
	rlca			;19b2
	nop			;19b3
l19b4h:
	ret			;19b4
	inc b			;19b5
	ld c,b			;19b6
	ld b,l			;19b7
	ld d,d			;19b8
	ld b,l			;19b9
	inc c			;19ba
	nop			;19bb
	ld hl,(0fc8ah)		;19bc
	rst 8			;19bf
	ret			;19c0
	ld bc,l0948h		;19c1
	nop			;19c4
	ld hl,0fc8ah		;19c5
	rst 8			;19c8
	ret			;19c9
	ld bc,COLD_RESTART+1		;19ca
	nop			;19cd
	ld hl,START_OF_DICT		;19ce
	rst 8			;19d1
	ret			;19d2
	ld (bc),a			;19d3
	ld d,e			;19d4
	ld b,b			;19d5
	ex af,af'			;19d6
	nop			;19d7
	jp READ_TOKEN		;19d8
	ld (bc),a			;19db
	ld a,023h		;19dc
	dec bc			;19de
	nop			;19df
	call sub_0a36h		;19e0
	jp l16bdh		;19e3
	inc bc			;19e6
	ld l,043h		;19e7
	ld c,a			;19e9
	ld (de),a			;19ea
	nop			;19eb
	rst 10h			;19ec
	xor a			;19ed
	or l			;19ee
	ret z			;19ef
l19f0h:
	rst 20h			;19f0
	call sub_08f8h		;19f1
	dec l			;19f4
	jr nz,l19f0h		;19f5
	ret			;19f7
	inc b			;19f8
	ld l,043h		;19f9
	ld d,b			;19fb
	ld d,l			;19fc
	dec c			;19fd
	nop			;19fe
	ld hl,01d6eh		;19ff
	jp PRINT_STR_HL		;1a02
	ld (bc),a			;1a05
	ld d,l			;1a06
	ld hl,(l000dh+1)		;1a07
	call sub_0b9bh		;1a0a
	call sub_0b3eh		;1a0d
	jp STACK_DE_HL		;1a10
	dec b			;1a13
	ld d,l			;1a14
	cpl			;1a15
	ld c,l			;1a16
	ld c,a			;1a17
	ld b,h			;1a18
	inc d			;1a19
	nop			;1a1a
sub_1a1bh:
	rst 10h			;1a1b
	push hl			;1a1c
	pop bc			;1a1d
	call sub_0b9bh		;1a1e
	call sub_0ba0h		;1a21
	jp STACK_DE_HL		;1a24
	inc b			;1a27
	ld d,l			;1a28
	ld c,l			;1a29
	ld c,a			;1a2a
	ld b,h			;1a2b
	rrca			;1a2c
	nop			;1a2d
	call sub_1a1bh		;1a2e
	call sub_0e41h		;1a31
	rst 10h			;1a34
	ret			;1a35
	ld (bc),a			;1a36
	ld d,l			;1a37
	inc a			;1a38
	ld c,000h		;1a39
	call sub_0b9bh		;1a3b
	or a			;1a3e
	sbc hl,de		;1a3f
	jp l16bdh		;1a41
	inc bc			;1a44
	ld c,l			;1a45
	ld c,c			;1a46
	ld c,(hl)			;1a47
	ld de,0cd00h		;1a48
	sbc a,e			;1a4b
	dec bc			;1a4c
	call sub_1676h		;1a4d
l1a50h:
	jr c,l1a53h		;1a50
	ex de,hl			;1a52
l1a53h:
	rst 8			;1a53
	ret			;1a54
	inc bc			;1a55
	ld c,l			;1a56
	ld b,c			;1a57
	ld e,b			;1a58
	rrca			;1a59
	nop			;1a5a
	call sub_0b9bh		;1a5b
	call sub_1676h		;1a5e
	ccf			;1a61
	jr l1a50h		;1a62

	db 0x03, 0x50, 0x41, 0x44 ; PAD
	db 0x0b, 0x00

	ld hl,PAD		;1a6a
	rst 8			;1a6d - UPUSH
	ret			;1a6e

	
	ld (bc),a			;1a6f
	ld d,l			;1a70
	inc hl			;1a71
	djnz l1a74h		;1a72
l1a74h:
	rst 10h			;1a74
	ld de,l0000h		;1a75
	ex de,hl			;1a78
	rst 8			;1a79
	ex de,hl			;1a7a
	rst 8			;1a7b
	jp sub_0a96h		;1a7c
	ld (bc),a			;1a7f
	ld d,l			;1a80
	ld l,00bh		;1a81
	nop			;1a83
	call l1a74h		;1a84
	jp PRINT_STRING		;1a87
	ld (bc),a			;1a8a
	ld b,h			;1a8b
	dec a			;1a8c
	inc de			;1a8d
	nop			;1a8e
	call sub_12c0h		;1a8f
	rst 10h			;1a92
	rst 10h			;1a93
	rst 10h			;1a94
	rst 10h			;1a95
l1a96h:
	scf			;1a96
	jr nz,l1a9ah		;1a97
	ccf			;1a99
l1a9ah:
	jp l16bdh		;1a9a
	inc bc			;1a9d
	ld b,h			;1a9e
	jr nc,l1adeh		;1a9f
	ld c,000h		;1aa1
	call sub_0b9bh		;1aa3
	call sub_0d93h		;1aa6
	jr l1a96h		;1aa9
	inc b			;1aab
	ld b,h			;1aac
	ld c,l			;1aad
	ld c,c			;1aae
	ld c,(hl)			;1aaf
	djnz l1ab2h		;1ab0
l1ab2h:
	call sub_12c0h		;1ab2
	call nc,00da3h		;1ab5
	rst 10h			;1ab8
	rst 10h			;1ab9
	ret			;1aba
	inc b			;1abb
	ld b,h			;1abc
	ld c,l			;1abd
	ld b,c			;1abe
	ld e,b			;1abf
	djnz l1ac2h		;1ac0
l1ac2h:
	call sub_12c0h		;1ac2
	call c,00da3h		;1ac5
	rst 10h			;1ac8
	rst 10h			;1ac9
	ret			;1aca
	ld (bc),a		;1acb
	ld d,a			;1acc
	dec a			;1acd
	ld a,(bc)		;1ace
	nop			;1acf
	call sub_069dh		;1ad0
	jr l1a96h		;1ad3
	ld (bc),a		;1ad5
	ld d,e			;1ad6
	dec a			;1ad7
	dec bc			;1ad8
	nop			;1ad9
	rst 10h			;1ada
	call MATCH_STRING	;1adb
l1adeh:
	jr l1a96h		;1ade
	inc b			;1ae0
	ld d,h			;1ae1
	ld c,c			;1ae2
	ld c,l			;1ae3
	ld b,l			;1ae4
	inc c			;1ae5
	nop			;1ae6
	ld hl,0fc5ch		;1ae7
	rst 8			;1aea
	ret			;1aeb
	inc bc			;1aec
	ld d,b			;1aed
	ld b,l			;1aee
	ld d,d			;1aef
	dec bc			;1af0
	nop			;1af1
	ld hl,0fc68h		;1af2
	rst 8			;1af5
	ret			;1af6
	inc b			;1af7
	dec hl			;1af8
	ld c,a			;1af9
	ld d,d			;1afa
	ld b,a			;1afb
	ld c,000h		;1afc
	ld de,0fc50h		;1afe
	rst 10h			;1b01
	add hl,de			;1b02
	rst 8			;1b03
	ret			;1b04
	add a,e			;1b05
	ld b,d			;1b06
	ld c,h			;1b07
	ld c,e			;1b08
	dec c			;1b09
	nop			;1b0a
	ld hl,0fc76h		;1b0b
l1b0eh:
	push hl			;1b0e
	jp l1797h		;1b0f
	add a,h			;1b12
	ld d,b			;1b13
	ld b,c			;1b14
	ld b,a			;1b15
	ld b,l			;1b16
	inc c			;1b17
	nop			;1b18
	ld hl,0fc52h		;1b19
	jr l1b0eh		;1b1c
	add a,e			;1b1e
	ld e,e			;1b1f
	ld e,a			;1b20
	ld e,l			;1b21
	inc c			;1b22
	nop			;1b23
	call sub_06e3h		;1b24
	jp sub_07d3h		;1b27
	inc bc			;1b2a
	ld c,b			;1b2b
	ld a,041h		;1b2c
	djnz l1b30h		;1b2e
l1b30h:
	rst 10h			;1b30
	ld a,l			;1b31
	call sub_091eh		;1b32
	ld h,000h		;1b35
	ld l,a			;1b37
	rst 8			;1b38
	ret			;1b39
	inc bc			;1b3a
	ld b,c			;1b3b
	ld a,048h		;1b3c
	djnz l1b40h		;1b3e
l1b40h:
	rst 10h			;1b40
	ld a,l			;1b41
	call ATOI		;1b42
	ld h,000h		;1b45
	ld l,a			;1b47
	rst 8			;1b48
	ret			;1b49
	inc b			;1b4a
	ld c,b			;1b4b
	ld b,l			;1b4c
	ld b,c			;1b4d
	ld b,h			;1b4e
	ld a,(bc)			;1b4f
	nop			;1b50
	jp sub_0866h		;1b51
	inc b			;1b54
	ld c,c			;1b55
	ld c,(hl)			;1b56
	ld c,c			;1b57
	ld d,h			;1b58
	inc de			;1b59
	nop			;1b5a
	call sub_06e3h		;1b5b
	ld (02002h),hl		;1b5e
	ld hl,02000h		;1b61
	ld (hl),0a4h		;1b64
	ret			;1b66
	ld b,055h		;1b67
	ld d,b			;1b69
	ld b,h			;1b6a
	ld b,c			;1b6b
	ld d,h			;1b6c
	ld b,l			;1b6d
	inc e			;1b6e
	nop			;1b6f
sub_1b70h:
	ld hl,(0fc8ah)		;1b70
	ld (02006h),hl		;1b73
	ld hl,(START_OF_DICT)	;1b76
	ld (02004h),hl		;1b79
	ld hl,(0fc98h)		;1b7c
	ld (02008h),hl		;1b7f
	ret			;1b82
	add a,h			;1b83
	ld b,(hl)			;1b84
	ld b,d			;1b85
	ld d,l			;1b86
	ld b,(hl)			;1b87
	dec c			;1b88
	nop			;1b89
	ld hl,P_DBUFFER		;1b8a
	jp l1b0eh		;1b8d
	inc b			;1b90
	ld d,b			;1b91
	ld b,c			;1b92
	ld d,e			;1b93
	ld d,e			;1b94
	ld a,(bc)			;1b95
	nop			;1b96
	jp l09a9h		;1b97
	inc b			;1b9a
	ld c,a			;1b9b
	ld d,(hl)			;1b9c
	ld b,l			;1b9d
	ld d,d			;1b9e
	djnz l1ba1h		;1b9f
l1ba1h:
	rst 10h			;1ba1
	ex de,hl			;1ba2
	rst 10h			;1ba3
	rst 8			;1ba4
	ex de,hl			;1ba5
	rst 8			;1ba6
	ex de,hl			;1ba7
	rst 8			;1ba8
	ret			;1ba9

	;; FORTH word PRINT
	db 0x05, 0x50, 0x52, 0x49, 0x4E, 0x54
	db 0x0F, 0x00

	call sub_06e3h		;1bb2
	ld (PRINT_DRVR),hl	;1bb5

	ret			;1bb8

	;; FORTH word P
	db 0x01, 0x50
	db 0x0F, 0x00

	ld hl,FLAGS2		;1bbd
	bit 0,(hl)		;1bc0
	res 0,(hl)		;1bc2
	ret nz			;1bc4
	set 0,(hl)		;1bc5
	ret			;1bc7
	inc b			;1bc8
	ld d,b			;1bc9
	ld d,d			;1bca
	ld d,h			;1bcb
	ld d,d			;1bcc
	inc a			;1bcd
	nop			;1bce

	;; Default printer driver for Sinclair ZX Printer (or compatible)
	push hl			;1bcf
	rst 10h			;1bd0
	ld a,l			;1bd1
	ld hl,PAD		;1bd2
	and 07fh		;1bd5
	cp 060h		;1bd7
	jr c,l1bddh		;1bd9
	add a,0e0h		;1bdb
l1bddh:	cp 020h		;1bdd
	jr nc,l1bf6h		;1bdf
	cp 008h		;1be1
	jr nz,l1bech		;1be3
	xor a			;1be5
	or (hl)			;1be6
	jr z,l1beah		;1be7
	dec (hl)			;1be9
l1beah:	pop hl			;1bea

	ret			;1beb

l1bech:	cp 00dh		;1bec
	jr z,l1beah		;1bee
	cp 00ah		;1bf0
	ld a,02eh		;1bf2
	jr z,l1bfdh		;1bf4
l1bf6h:
	call STR_ADD_CHR	;1bf6
	bit 5,(hl)		;1bf9
	jr z,l1c02h		;1bfb
l1bfdh:
	call sub_1c09h		;1bfd
	ld (hl),000h		;1c00
l1c02h:
	pop hl			;1c02
	ret			;1c03
	ld (bc),a			;1c04
	ld l,050h		;1c05
	adc a,h			;1c07
	nop			;1c08
sub_1c09h:
	xor a			;1c09
	out (0fdh),a		;1c0a
	out (0fbh),a		;1c0c
	push de			;1c0e
	push hl			;1c0f
	ld hl,PAD		;1c10
	ld a,01fh		;1c13
	sub (hl)			;1c15
	jr c,l1c26h		;1c16
	push hl			;1c18
	inc a			;1c19
	ld e,a			;1c1a
	ld a,(hl)			;1c1b
	inc hl			;1c1c
	add a,l			;1c1d
	ld l,a			;1c1e
l1c1fh:
	ld (hl),020h		;1c1f
	inc hl			;1c21
	dec e			;1c22
	jr nz,l1c1fh		;1c23
	pop hl			;1c25
l1c26h:
	inc hl			;1c26
	ex de,hl			;1c27
	ld hl,(STACKP_BASE)	;1c28
	call sub_1c37h		;1c2b
	pop hl			;1c2e
	pop de			;1c2f
	ld a,004h		;1c30
	out (0fbh),a		;1c32
	out (0feh),a		;1c34
	ret			;1c36
sub_1c37h:
	push bc			;1c37
	ld c,000h		;1c38
l1c3ah:
	push de			;1c3a
	push hl			;1c3b
	ld b,020h		;1c3c
l1c3eh:
	push hl			;1c3e
	ld h,000h		;1c3f
	ld a,(de)			;1c41
	sub 020h		;1c42
	and 03fh		;1c44
	ld l,a			;1c46
	add hl,hl			;1c47
	add hl,hl			;1c48
	add hl,hl			;1c49
	ld a,i			;1c4a
	or h			;1c4c
	ld h,a			;1c4d
	ld a,c			;1c4e
	or l			;1c4f
	ld l,a			;1c50
	ld a,(hl)			;1c51
	pop hl			;1c52
	ld (hl),a			;1c53
	inc hl			;1c54
	inc de			;1c55
	djnz l1c3eh		;1c56
	pop hl			;1c58
	ld a,c			;1c59
	cp 006h		;1c5a
	sbc a,a			;1c5c
	inc a			;1c5d
	add a,a			;1c5e
	ld e,a			;1c5f
	call sub_1c6bh		;1c60
	pop de			;1c63
	inc c			;1c64
	bit 3,c		;1c65
	jr z,l1c3ah		;1c67
	pop bc			;1c69
	ret			;1c6a
sub_1c6bh:
	push hl			;1c6b
	push de			;1c6c
	push bc			;1c6d
l1c6eh:
	in a,(0fbh)		;1c6e
	rlca			;1c70
	jr nc,l1c6eh		;1c71
	ld c,020h		;1c73
l1c75h:
	ld b,008h		;1c75
	ld d,(hl)			;1c77
	inc hl			;1c78
l1c79h:
	in a,(0fbh)		;1c79
	rrca			;1c7b
	jr nc,l1c79h		;1c7c
	rlc d		;1c7e
	ld a,e			;1c80
	jr nc,l1c85h		;1c81
	or 080h		;1c83
l1c85h:
	out (0fbh),a		;1c85
	djnz l1c79h		;1c87
	dec c			;1c89
	jr nz,l1c75h		;1c8a
	pop bc			;1c8c
	pop de			;1c8d
	pop hl			;1c8e
	ret			;1c8f
	inc bc			;1c90
	ld l,043h		;1c91
	ld c,(hl)			;1c93
	add hl,bc			;1c94
	nop			;1c95
	jp PRINT_STRING		;1c96
	ld (bc),a			;1c99
	ld b,h			;1c9a
	ld a,00ah		;1c9b
	nop			;1c9d
	call 00da3h		;1c9e
	jr l1ca8h		;1ca1
	ld (bc),a			;1ca3
	ld b,h			;1ca4
	inc a			;1ca5
	ld e,l			;1ca6
	ex (sp),hl			;1ca7
l1ca8h:
	call sub_12c0h		;1ca8
	rst 10h			;1cab
	rst 10h			;1cac
	rst 10h			;1cad
	rst 10h			;1cae
	ccf			;1caf
	jp l16bdh		;1cb0
	nop			;1cb3
	nop			;1cb4
	nop			;1cb5
	nop			;1cb6
	nop			;1cb7
	nop			;1cb8
	nop			;1cb9
	nop			;1cba
	nop			;1cbb
	nop			;1cbc
	nop			;1cbd
	nop			;1cbe
	nop			;1cbf

	;; Jump table of 32 service routines, corresponding to special
	;; key presses
SPECIAL_CHAR_TABLE:
	dw 0x0050		; 00 - No action, RET
	dw 0x0297		; 01 - Home
	dw 0x0050		; 02 - No action, RET
	dw 0x0050		; 03 - No action, RET
	dw 0x0050		; 04 - No action, RET
	dw 0x0050		; 05 - No action, RET
	dw 0x0050		; 06 - No action, RET
	dw 0x0050		; 07 - No action, RET

	dw 0x0308		; 08 - Left
	dw 0x02E3		; 09 - Right
	dw 0x02C6		; 0A - Down
	dw 0x02FF		; 0B - Up
	dw 0x0283		; 0C - Clear screen
	dw 0x029F		; 0D
	dw 0x0050		; 0E - No action, RET
	dw 0x0050		; 0F - No action, RET

	dw 0x0050		; 10 - No action, RET
	dw 0x03EE		; 11 - Put PAD
	dw 0x041D		; 12 -
	dw 0x0441		; 13 -
	dw 0x044B		; 14 - Fetch PAD
	dw 0x0050		; 15 - No action, RET
	dw 0x0050		; 16 - No action, RET
	dw 0x0050		; 17 - No action, RET
	
	dw 0x0050		; 18 - No action, RET
	dw 0x0050		; 19 - No action, RET
	dw 0x03A3		; 1A - Delete line
	dw 0x0385		; 1B - Rubout
	dw 0x03DA		; 1C - Graphics mode
	dw 0x03BD		; 1D - 
	dw 0x0455		; 1E - Edit
	dw 0x047C		; 1F - ? Flashing cursor

	;; ZX81-FORTH BY DAVID HUSBAND  COPYRIGHT (C) 1983
COPYRIGHT_MSG:
	db 0x33, 0x0C
	dm "ZX81-FORTH BY DAVID HUSBAND"
	db 0x0D, 0x0A
	dm "COPYRIGHT (C) 1983"
	db 0x0D, 0x0A, 0x0A
	

l1d34h:	db 0x02, 0x0D, 0x0A
	
l1d37h: db 0x20
KEY_CODES:
	db 0x00
l1d39h: db "A", "Q", "1", "0", "P", 0x0D," ", "Z"
	db "S", "W", "2", "9", "O", "L", ".", "X"
	db "D", "E", "3", "8", "I", "K", "M"
l1d50h: db "C"
	db "F"
l1d52h: db "R", "4", "7"
l1d55h: db "U", "J", "N", "V"
	db "G", "T", "5", "6", "Y", "H"
l1d5fh: db "B"
l1d60h: db 0x05 		; First 40 keys

	db " ", "O", "K"
l1d64h:	db 0x0D, 0x0A
l1d66h: db 0x07, " ", "E", "R", "R"
	db "O", "R", " ", 0x09, 0x0D, 0x0A, "Z", "X"
	db "-", "Z", "8", "0", " "

	;; Shifted versions of keys
	db 0x00
l1d79h: db 0x0C, 0x18, 0x1E, 0x1B 	; A (CLS), Q (Compile), 1
					; (Edit), 0 (Rubout)
	db 0x22, 0x01, 0x80, ":"	; P, Enter, Space, Z
	db "%", "!", 0x14	 	; S, W, 2 (Fetch PAD)
l1d84h: db 0x1C, ")", "=", ",", 0x3B 	; 9 (Graphics), O, L, ., X
	db "'", "@"			; D, E (STEP)

	db 0x11, 0x09, "(", "+", ">", "?", 0x5C,  "[" ; 3 (Put PAD), 8
						      ; (Right), I, K,
						      ; M, C, F, R
	db 0x1A, 0x0B, 0x24, 0x2D; 4 (Del Line), 7 (Up), U, J
l1d97h: db  "<", "/", "^"	 ;  N, V, G
l1d9ah:	db "_"			 ; T
l1d9bh:	db 0x08			 ; 5 (Left)
	db 0x0A			; 6 (Down)
	db "]", "#", "*"	; Y, H, B

	;; Copy of system variables 1DA0--1DFF
DEFVARS:
	db 0x00, 0x00
	nop			;1da2
	nop			;1da3
	rrca			;1da4
	nop			;1da5
	nop			;1da6
l1da7h:	nop			;1da7
	nop		;1da8 - PERiod - tic limit at which clock resets (see Ch 15 of manual)
	ld a,(de)			;1da9
	ld c,a			;1daa
	nop			;1dab
	nop			;1dac
	ld b,b			;1dad
l1daeh:
	nop			;1dae
	defb 0fdh,098h,0fch	;illegal sequence		;1daf
l1db2h:
	nop			;1db2
	jr nz,l1d84h		;1db3
	dec de			;1db5
	nop			;1db6
	defb 0fdh,0ffh,000h	;illegal sequence		;1db7
	ld (hl),00ah		;1dba
	ld d,b			;1dbc
	nop			;1dbd
	add a,b			;1dbe
	add a,b			;1dbf
	dw RUN_VSYNC		; Initial display routine to call points 
	ld (hl),c		;1dc2
	nop			;1dc3
	ret nz			;1dc4
	call m,l000ah		;1dc5
	ei			;1dc8
	ld a,(bc)			;1dc9
	nop			;1dca
	ld b,b			;1dcb
	ei			;1dcc
	ld a,(bc)			;1dcd
	nop			;1dce
	ld b,b			;1dcf
	add a,b			;1dd0
	jp m,0fc98h		;1dd1
	nop			;1dd4
	nop			;1dd5
	ld (hl),0fch		;1dd6
	nop			;1dd8
	nop			;1dd9
	nop			;1dda
	nop			;1ddb
	nop			;1ddc
	nop			;1ddd
	pop af			;1dde
	rst 38h			;1ddf
	rst 38h			;1de0
	rst 38h			;1de1
	nop			;1de2
	cp l			;1de3
	inc de			;1de4
	nop			;1de5
	rst 8			;1de6
	inc b			;1de7
	ret m			;1de8
	ex af,af'			;1de9
	inc bc			;1dea
	nop			;1deb
	add a,b			;1dec
	add a,b			;1ded
	nop			;1dee
	nop			;1def
	nop			;1df0
	nop			;1df1
	rra			;1df2
	rrca			;1df3
	nop			;1df4
	nop			;1df5
	nop			;1df6
	nop			;1df7
	nop			;1df8
	nop			;1df9
	rra			;1dfa
	rla			;1dfb
	nop			;1dfc
	nop			;1dfd
	nop			;1dfe
	rrca			;1dff

	;; Character ROM (64 characters * 8 pixel-rows per character)
	;; Needs to be a the start of a RAM page -- that is, 0x??00.
	;;
	;; When displayed on screen character code is code-20
CHARS:	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ; 00 Space
	db 0x00, 0x08, 0x08, 0x08, 0x08, 0x00, 0x08, 0x00 ; 01 Exclamation
	db 0x00, 0x12, 0x12, 0x00, 0x00, 0x00, 0x00, 0x00 ; 02 Quotes
	db 0x00, 0x24, 0x7E, 0x24, 0x24, 0x7E, 0x24, 0x00 ; 03 Hash
	db 0x00, 0x08, 0x3E, 0x48, 0x3C, 0x12, 0x7C, 0x10 ; 04 Dollar
	db 0x00, 0x62, 0x64, 0x08, 0x10, 0x26, 0x46, 0x00 ; 05 Percentage
	db 0x00, 0x18, 0x24, 0x18, 0x28, 0x44, 0x3E, 0x02 ; 06 Ampersand
	db 0x00, 0x04, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00 ; 07 Quote
	db 0x00, 0x04, 0x08, 0x08, 0x08, 0x08, 0x04, 0x00 ; 08 Left parenthesis
	db 0x00, 0x20, 0x10, 0x10, 0x10, 0x10, 0x20, 0x00 ; 09 Right parenthesis
	db 0x00, 0x00, 0x08, 0x2A, 0x1C, 0x2A, 0x08, 0x00 ; 0A Asterix
	db 0x00, 0x00, 0x08, 0x08, 0x3E, 0x08, 0x08, 0x00 ; 0B Plus
	db 0x00, 0x00, 0x00, 0x00, 0x06, 0x06, 0x02, 0x00 ; 0C Comma
	db 0x00, 0x00, 0x00, 0x00, 0x3C, 0x00, 0x00, 0x00 ; 0D Minus
	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x06, 0x00 ; 0E Period
	db 0x00, 0x04, 0x04, 0x08, 0x10, 0x20, 0x20, 0x00 ; 0F Slash
	db 0x00, 0x3C, 0x46, 0x4A, 0x52, 0x62, 0x3C, 0x00 ; 10 '0'
	db 0x00, 0x04, 0x0C, 0x04, 0x04, 0x04, 0x0E, 0x00 ; 11 '1'
	db 0x00, 0x3C, 0x42, 0x02, 0x3C, 0x40, 0x7E, 0x00 ; 12 '2'
	db 0x00, 0x3C, 0x42, 0x04, 0x02, 0x42, 0x3C, 0x00 ; 13 '3'
	db 0x00, 0x04, 0x0C, 0x14, 0x24, 0x7E, 0x04, 0x00 ; 14 '4'
	db 0x00, 0x7E, 0x40, 0x7C, 0x02, 0x42, 0x3C, 0x00 ; 15 '5'
	db 0x00, 0x3C, 0x40, 0x7C, 0x42, 0x42, 0x3C, 0x00 ; 16 '6'
	db 0x00, 0x7E, 0x02, 0x04, 0x08, 0x10, 0x20, 0x00 ; 17 '7'
	db 0x00, 0x3C, 0x42, 0x3C, 0x42, 0x42, 0x3C, 0x00 ; 18 '8'
	db 0x00, 0x3C, 0x42, 0x42, 0x3E, 0x02, 0x3C, 0x00 ; 19 '9'

	nop			;1ed0
	ld b,006h		;1ed1
	nop			;1ed3
	nop			;1ed4
	ld b,006h		;1ed5
	nop			;1ed7
	nop			;1ed8
	ld b,006h		;1ed9
	nop			;1edb
	ld b,006h		;1edc
	ld (bc),a			;1ede
l1edfh:
	nop			;1edf
	nop			;1ee0
	nop			;1ee1
	inc b			;1ee2
	ex af,af'			;1ee3
	djnz l1eeeh		;1ee4
	inc b			;1ee6
	nop			;1ee7
	nop			;1ee8
	nop			;1ee9
	nop			;1eea
	inc a			;1eeb
	nop			;1eec
	inc a			;1eed
l1eeeh:
	nop			;1eee
	nop			;1eef
	nop			;1ef0
	nop			;1ef1
	djnz l1efch		;1ef2
	inc b			;1ef4
	ex af,af'			;1ef5
	djnz l1ef8h		;1ef6
l1ef8h:
	nop			;1ef8
	inc a			;1ef9
	ld b,d			;1efa
	ld (bc),a			;1efb
l1efch:
	inc c			;1efc
	nop			;1efd
	ex af,af'			;1efe
	nop			;1eff
	nop			;1f00
	inc a			;1f01
	ld b,d			;1f02
	ld e,h			;1f03
	ld d,d			;1f04
	ld b,h			;1f05
	ld a,000h		;1f06
	nop			;1f08
	inc a			;1f09
	ld b,d			;1f0a
	ld b,d			;1f0b
	ld a,(hl)			;1f0c
	ld b,d			;1f0d
	ld b,d			;1f0e
	nop			;1f0f
	nop			;1f10
	ld a,h			;1f11
	ld b,d			;1f12
	ld a,h			;1f13
	ld b,d			;1f14
	ld b,d			;1f15
	ld a,h			;1f16
	nop			;1f17
	nop			;1f18
	inc a			;1f19
	ld b,d			;1f1a
	ld b,b			;1f1b
	ld b,b			;1f1c
	ld b,d			;1f1d
	inc a			;1f1e
	nop			;1f1f
	nop			;1f20
	ld a,h			;1f21
	ld b,d			;1f22
	ld b,d			;1f23
	ld b,d			;1f24
	ld b,d			;1f25
	ld a,h			;1f26
	nop			;1f27
	nop			;1f28
	ld a,(hl)			;1f29
	ld b,b			;1f2a
	ld a,h			;1f2b
	ld b,b			;1f2c
	ld b,b			;1f2d
	ld a,(hl)			;1f2e
	nop			;1f2f
	nop			;1f30
	ld a,(hl)			;1f31
	ld b,b			;1f32
	ld a,h			;1f33
	ld b,b			;1f34
	ld b,b			;1f35
	ld b,b			;1f36
	nop			;1f37
	nop			;1f38
	inc a			;1f39
	ld b,d			;1f3a
	ld b,b			;1f3b
	ld b,(hl)			;1f3c
	ld b,d			;1f3d
	inc a			;1f3e
	nop			;1f3f
	nop			;1f40
	ld b,d			;1f41
	ld b,d			;1f42
	ld a,(hl)			;1f43
	ld b,d			;1f44
	ld b,d			;1f45
	ld b,d			;1f46
	nop			;1f47
	nop			;1f48
	inc e			;1f49
	ex af,af'			;1f4a
	ex af,af'			;1f4b
	ex af,af'			;1f4c
	ex af,af'			;1f4d
	inc e			;1f4e
	nop			;1f4f
	nop			;1f50
	ld (bc),a			;1f51
	ld (bc),a			;1f52
	ld (bc),a			;1f53
	ld (bc),a			;1f54
	ld b,d			;1f55
	inc a			;1f56
	nop			;1f57
	nop			;1f58
	ld b,d			;1f59
	ld b,h			;1f5a
	ld a,b			;1f5b
	ld c,b			;1f5c
	ld b,h			;1f5d
	ld b,d			;1f5e
	nop			;1f5f
	nop			;1f60
	ld b,b			;1f61
	ld b,b			;1f62
	ld b,b			;1f63
	ld b,b			;1f64
	ld b,b			;1f65
	ld a,(hl)			;1f66
	nop			;1f67
	nop			;1f68
	ld b,d			;1f69
	ld h,(hl)			;1f6a
	ld e,d			;1f6b
	ld b,d			;1f6c
	ld b,d			;1f6d
	ld b,d			;1f6e
	nop			;1f6f
	nop			;1f70
	ld b,d			;1f71
	ld h,d			;1f72
	ld d,d			;1f73
	ld c,d			;1f74
	ld b,(hl)			;1f75
	ld b,d			;1f76
	nop			;1f77
	nop			;1f78
	inc a			;1f79
	ld b,d			;1f7a
	ld b,d			;1f7b
	ld b,d			;1f7c
	ld b,d			;1f7d
	inc a			;1f7e
	nop			;1f7f
	nop			;1f80
	ld a,h			;1f81
	ld b,d			;1f82
	ld b,d			;1f83
	ld a,h			;1f84
	ld b,b			;1f85
	ld b,b			;1f86
	nop			;1f87
	nop			;1f88
	inc a			;1f89
	ld b,d			;1f8a
	ld b,d			;1f8b
	ld b,d			;1f8c
	ld c,d			;1f8d
	ld a,000h		;1f8e
	nop			;1f90
	ld a,h			;1f91
	ld b,d			;1f92
	ld b,d			;1f93
	ld a,h			;1f94
	ld b,h			;1f95
	ld b,d			;1f96
	nop			;1f97
	nop			;1f98
	inc a			;1f99
	ld b,d			;1f9a
	jr nc,l1fa9h		;1f9b
	ld b,d			;1f9d
	inc a			;1f9e
	nop			;1f9f
	nop			;1fa0
	ld a,008h		;1fa1
	ex af,af'			;1fa3
	ex af,af'			;1fa4
	ex af,af'			;1fa5
	ex af,af'			;1fa6
	nop			;1fa7
	nop			;1fa8
l1fa9h:
	ld b,d			;1fa9
	ld b,d			;1faa
	ld b,d			;1fab
	ld b,d			;1fac
	ld b,d			;1fad
	inc a			;1fae
	nop			;1faf
	nop			;1fb0
	ld b,d			;1fb1
	ld b,d			;1fb2
	ld b,d			;1fb3
	ld b,d			;1fb4
	inc h			;1fb5
	jr l1fb8h		;1fb6
l1fb8h:
	nop			;1fb8
	ld b,d			;1fb9
	ld b,d			;1fba
	ld b,d			;1fbb
	ld e,d			;1fbc
	ld e,d			;1fbd
	inc h			;1fbe
	nop			;1fbf
	nop			;1fc0
	ld b,d			;1fc1
	inc h			;1fc2
	jr l1fddh		;1fc3
	inc h			;1fc5
	ld b,d			;1fc6
	nop			;1fc7
	nop			;1fc8
	ld b,d			;1fc9
	inc h			;1fca
	jr $+10		;1fcb
	ex af,af'			;1fcd
	ex af,af'			;1fce
	nop			;1fcf
	nop			;1fd0
	ld a,(hl)			;1fd1
	inc b			;1fd2
	ex af,af'			;1fd3
	djnz l1ff6h		;1fd4
	ld a,(hl)			;1fd6
	nop			;1fd7
	nop			;1fd8
	inc c			;1fd9
	ex af,af'			;1fda
	ex af,af'			;1fdb
	ex af,af'			;1fdc
l1fddh:
	ex af,af'			;1fdd
	inc c			;1fde
	nop			;1fdf
	nop			;1fe0
	jr nz,$+34		;1fe1
	djnz l1fedh		;1fe3
	inc b			;1fe5
	inc b			;1fe6
	nop			;1fe7
	nop			;1fe8
	jr nc,l1ffbh		;1fe9
	djnz l1ffdh		;1feb
l1fedh:
	djnz $+50		;1fed
	nop			;1fef
	nop			;1ff0
	ex af,af'			;1ff1
	inc e			;1ff2
	ex af,af'			;1ff3
	ex af,af'			;1ff4
	ex af,af'			;1ff5
l1ff6h:
	ex af,af'			;1ff6
	nop			;1ff7
	nop			;1ff8
	nop			;1ff9
	nop			;1ffa
l1ffbh:
	nop			;1ffb
	nop			;1ffc
l1ffdh:
	nop			;1ffd
	nop			;1ffe
	rst 38h			;1fff
