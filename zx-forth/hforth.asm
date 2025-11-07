; z80dasm.1.6
; command line: z80dasm -g 0 -a -l -o hforth.asm husband_forth.bin

	;; To do
	;; - Reinstate memory-checking routine
	;; - Initialise IY before parameter stack is called
	;;
	;; 
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
	;; The system variable, NEXT_DISP_ROUT, is used to determine
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
	;; 
	;;   Maskable interrupt is generated whenever A6 goes
	;;   low. Therefore, in normal operation it should be
	;;   disabled. The mechanism for triggering maskable interrupts
	;;   is when the the R register causes A6 to go low at the end
	;;   of executing a display line.
	;; 
	;; 
	;; Memory map
	;; 
	;; FB80 - Top of machine stack 0
	;; FB80--FBBF - Keyboard Input Buffer
	;; FBC0 - PAD
	;; FC3E - top of machine stack 1
	;; FCC0--FCFF -- Character stack (wraps around)
	;; FD00--FFFF - video RAM

	;; Set build configuration options. You can choose between:
	;; - Original ZX81 version (MINSTREL3=0, NTSC=0, MINSTREL4=0)
	;; - Original TS1000 version (MINSTREL3=0, NTSC=1, MINSTREL4=0)
	;; - Updated MINSTREL3 version (MINSTREL3=0, NTSC=?, MINSTREL4=0)
	;; - Port to Ace/ Minstrel 4th (MINSTREL3=0, NTSC=0, MINSTREL4=1)
	;;
	;; Do not set both MINSTREL3 and MINSTREL4 at same time. Jupiter
	;; Ace version requires 32k RAM pack.
	;; 
	;; You can also choose to fix several bugs/ non-standard
	;; elements of the original programme
MINSTREL3:	equ 0x00
NTSC:		equ 0x00
FIXBUG:		equ 0x01
MINSTREL4:	equ 0x01

	;; 	include "zx81_chars.asm"
	include "hforth_chars.asm"

	;; ROM configuration options
	if MINSTREL3+MINSTREL4>0
OFFSET:		equ 0x7B00 	; Offset to system memory

	if NTSC=1
FRAMES:			equ 60 	; Display frames per second
TOP_BORDER_LINES:	equ 31	; H_FORTH uses 4Ah (30d) / better 56
BOT_BORDER_LINES:	equ 34	; H_FORTH uses 1Eh (74d) / better 55
	else
FRAMES:			equ 50 	; Display frames per second
TOP_BORDER_LINES:	equ 55	; H_FORTH uses 4Ah (30d) / better 56
BOT_BORDER_LINES:	equ 58	; H_FORTH uses 1Eh (74d) / better 55
	endif

	else
OFFSET:		equ 0xFB00 	; Offset to system memory
	if NTSC=1
FRAMES:			equ 60 	; Display frames per second
TOP_BORDER_LINES:	equ 31	; H_FORTH uses 4Ah (30d) / better 56
BOT_BORDER_LINES:	equ 34	; H_FORTH uses 1Eh (74d) / better 55
	else
FRAMES:			equ 50 	; Display frames per second
TOP_BORDER_LINES:	equ 31	; H_FORTH uses 4Ah (30d) / better 56
BOT_BORDER_LINES:	equ 73	; H_FORTH uses 1Eh (74d) / better 55
	endif
	endif

	;; Screen configuration options
DISP_WIDTH:	equ 0x20	; Length of row in display buffer
DISP_HEIGHT:	equ 0x18	; Number of rows in display buffer
DBUFFER:	equ 0xFD00	; Address of display buffer in
				; upper-memory (Minstrel 3 only)
	
	;; H Forth  Memory Map (System Variables and Stacks)
PSTACK_BASE:	equ OFFSET-0x0080 	; Base of Parameter Stack
STACK0_BASE:	equ OFFSET+0x0080 	; Start of System Execution
					; Stack (stack 0)
PAD:		equ OFFSET+0x00C0	; Start of PAD
STACK1_BASE:	equ OFFSET+0x013E	; Start of System Editor stack
					; (stack 1)
VARS:		equ OFFSET+0x0140	; Start of system variables
STACKC_BASE:	equ OFFSET+0x01C0	; Base of Character stack

	;; System variables
VARS_BASE:	equ OFFSET+0x0150	; System variable base addr
SCREEN_NUM:	equ OFFSET+0x0152	; Stores screen number during
					; load/ store
AUTO_CNT:	equ OFFSET+0x0154	; Counter for auto-mode
RAM_SIZE:	equ OFFSET+0x0156	; Total RAM
UNKNOWN4:	equ OFFSET+0x0158	; ???
TIME:		equ OFFSET+0x015C	; Time variable (see Ch 15 of manual)


UNKNOWN5:	equ OFFSET+0x0160	; ???
TIC_COUNTER:	equ OFFSET+0x0164	; Increment counter for timer
PER:		equ OFFSET+0x0168	; System clock limit value
RAM_START:	equ OFFSET+0x016C	; Start of RAM
P_DBUFFER:	equ OFFSET+0x016E	; Address of display buffer. Set
					; to BD00/ FD00 during
					; initialisation.
PMTASK_LIST_HD:	equ OFFSET+0x0170	; Pointer to head of task list
FENCE:		equ OFFSET+0x0172	; Location of any fence set
PRINT_DRVR:	equ OFFSET+0x0174	; Address of printer driver
P_EDIT_SCREEN:	equ OFFSET+0x0176	; Pointer to Editor screen
					; (usually start of display
					; buffer)
F_WARM_RESTART:	equ OFFSET+0x0178 	; Set to FF during restart, to
					; indicate warm restart is
					; possible
PARSE_NUM_ROUT:	equ OFFSET+0x017A 	; Routine to parse number from
					; character buffer
P_BACKTASK:	equ OFFSET+0x017C	; Location of multitasking scheduler
KIB_R_OFFSET:	equ OFFSET+0x017E	; Offset to most recently read
					; entry in Keyboard Input Buffer
KIB_W_OFFSET:	equ OFFSET+0x017F	; Offset to most recently
					; written value in keyboard
					; input buffer
NEXT_DISP_ROUT:	equ OFFSET+0x0180	; Next display-handling routine
P_RUN_DISP:	equ OFFSET+0x0182	; Address of routine to produce
					; main display (points to RET,
					; in FAST mode)
P_STACKC:	equ OFFSET+0x0184	; Address of next entry in Character
					; Stack
BASE:		equ OFFSET+0x0186	; 16-bit base for processing numbers
PSTART_DICT:	equ OFFSET+0x0188	; Special string (used with
					; expansion ROM)
P_HERE:		equ OFFSET+0x018A 	; Store for current entry point in
					; dictionary
START_DICT_DEF:	equ OFFSET+0x018C
UNKNOWN2:	equ OFFSET+0x018E 	; Start of currently being
					; defined word???
STACKP_BASE:	equ OFFSET+0x0190	; Base location for Paramater
					; Stack
PCUR_TASK_STRUCT:	equ OFFSET+0x0192	; ???
MSTACK0:	equ OFFSET+0x0194	; Pointer to machine-stack 0
MSTACK1:	equ OFFSET+0x0196	; Pointer to machine-stack 1
MTASK_TAIL:	equ OFFSET+0x0198	; ???
FLAGS3:		equ OFFSET+0x01A5	; 0 - Set if PRINT OK required;
					; 1 - Set if compile-screen req'ed
					; 2 - Auto-compile on LOAD
					; 4 - Set if RAM at 2000--3FFF
					; 5 - ??? Disabled for PAL
					; 6 - Reset to stop multitasking
					;     on restart/ set to continue
					;     multitasking
					; 7 - Speed setting (0 = AUTO; 1
					;     = Manual (FAST/SLOW))


KBD_ROUT: 	equ OFFSET+0x01A6	; Address of key handler for
					; Editor/ Execution context
					; (usually, PROCESS_KEY)
CO_KBD_ROUT:	equ OFFSET+0x01A8	; Keyboard handler
POSSIBLE_KEY:	equ OFFSET+0x01AB
LAST_KEY:	equ OFFSET+0x01AC 	
FLAGS:		equ OFFSET+0x01AD	; System flags:
					; 0 - Execution / Editor mode
					; 1 - Editor screen print lock
					; 2 - Console print lock
					; 3 - Something to do with memory ???
					; 4 - Machine stack in use (0/1)
					; 5 - Switch-machine-stack lock
					; 6 - Editor screen visible
					; 7 - cursor-inversion state
SCR_INFO_ED:	equ OFFSET+0x01AE 	; Screen info (editor)
					; +00 - current col value
					; +01 - current row value
					; +02 - leftmost column (window)
					; +03 - top-most row (window)
					; +04 - rightmost column (window)
					; +05 - bottommost row (window)
					; +06 - Character bitmap mask
					; +07 - Blank character
SCR_INFO_CO:	equ OFFSET+0x01B6 	; Screen info (Console)
					; +00 - current col value
					; +01 - current row value
					; +02 - leftmost column (window)
					; +03 - top-most row (window)
					; +04 - rightmost column (window)
					; +05 - bottommost row (window)
					; +06 - Character bitmap mask
					; +07 - Blank character
FLAGS2:		equ OFFSET+0x01BE	; Further flags
					; 0 - printer disabled/ enabled
TOKEN_LN:	equ OFFSET+0x01BF	; Track length of currently
					; being entered token
	
	org	0x0000		; Start of ROM address space

	;;
	;; RST 0x00 - Cold or warm restart
	;; 
RESTART:
	if MINSTREL4=1
	nop
	nop
	else
	out (0xFD),a		;0000 - Disable NMI Generator
	endif
	
	ld sp,STACK0_BASE	;0002 - Reset stack pointer

	if MINSTREL3+MINSTREL4>0
l0005h:	jp RESTART_NEW		;0005 - Continue with augmented reset,
				;       for Minstrel 3

	else
l0005h:	jp RESTART_CONT		;0005 - Continue with reset
	endif

	;;
	;; RST 0x08 - Push HL onto Parameter Stack
	;; 
	;; IY points to Parameter Stack
HL_TO_PSTACK:
	dec iy			;0008

	ld (iy+000h),h		;000a
l000dh:	jr HTP_CONT		;000d

	
	rst 38h			;000f - Not sure this is ever used

	;;
	;; RST 0x10 - Pop from Parameter Stack into HL
	;; 
	;; IY points to Parameter Stack
PSTACK_TO_HL:
	ld l,(iy+000h)		;0010
	inc iy			;0013
	jr PTH_CONT		;0015

	
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

	if MINSTREL4=1
	di
	push hl
	jp RUN_VSYNC

	else
	dec c			; (4) Decrement scan-line counter

	jp nz, I_NEXT_SCANLINE	; (10) Skip forward if more scan lines
				; to produce (Use JP to ensure
				; consistent timing with/ without a
				; branch).

	pop hl			; (10) Retrieve return address (next
				; character to execute in display
				; buffer)
	endif

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
HTP_CONT:
	dec iy			;004b
	ld (iy+000h),l		;004d

NO_ACTION:
	ret			;0050 - also used for null routine (just
				;       RET)

	;; Continuation of POP_HL routine (RST 10)
PTH_CONT:
	ld h,(iy+000h)		;0051
	inc iy			;0054
	
	ret			;0056

	;; Routine to transition between display modes
SKIP_DISPLAY:
	if NTSC=1
	ld a,0xE4
	else
	ld a,0xFF		;0057 - E4h in 60 Hz version
	endif
	
	ex af,af'		;0059
	ld hl,RUN_VSYNC		;005a - Set routine for video handling
	ld (NEXT_DISP_ROUT),hl	;005d

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
	;;   (NEXT_DISP_ROUT) - address of routine to call, when countdown
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
	push hl 		; Save register - restored at end of
				; corresponding display routine

	ld hl,(NEXT_DISP_ROUT) 	; Proceed to next routine (either
	jp (hl)	     		; RUN_VSYNC or RUN_DISPLAY)

	if MINSTREL4=1
RUN_DISPLAY:	
SCREEN_CONT:
	ld hl, 0x2000		; Screen attributes (char mask and _SPACE)
	
	jp DICT_ADD_HL

	else
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

	ld bc,DISP_HEIGHT*0x0100+0x0009	; 24 rows and 8 scan lines + 1
	ld hl, RUN_VSYNC	; Store address of next but one display
	ld (NEXT_DISP_ROUT), hl ; routine (VSync)

	if MINSTREL3+MINSTREL4>0
	ld hl, DBUFFER

	else
	ld hl,(P_DBUFFER)	; Point to start of display buffer
				; (execution address in upper 32kB of
				; memory)
	;; set 7,h		; Switch address to upper memory
	endif
	
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

	pop hl			; (10) - Corresponds to `push hl` in
				;        NMI_DONE

	ret			; (10) Return to main program: display
				; generation will continue with bottom
				; border, when next NMI pulse is
				; generated.

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
	endif
	
	ds 0x0098-$		; Spacer for Minstrel 4th

	;; ----------------------------------------------------------------
	;; Video handling routine (VSync and read keyboard)
RUN_VSYNC:
	if MINSTREL4=0
BB_OFF:	out (0xFD),a		;0098 - Disable NMI Generator
VS_ON:	in a,(0xFE)		;009a - Turn on VSync

	;;
	;; Vsync duration is 11 + 32 + 1,255 + 11 = 1,310 (including final OUT)
	;;                or 1,304 (400 us) (if no key press)
	;; 		  or 1,312 if shifted key press
	;; 
	ld a,TOP_BORDER_LINES-1	; (7) 009c - Set counter for top margin,
	ex af,af'		; (4) 009e and store for next NMI
				; cycle. Note that one top-border scan
				; line is executed by RUN_DISPLAY, hence
				; the '-1'
	;; Set next display routine
	ld hl,(P_RUN_DISP)	; (16) 009f - Usually contains 0071h
	ld (NEXT_DISP_ROUT),hl 	; (16)

	endif
	
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

	;; Normalise H and L on zero (also drops shift, which has been
	;; dealt with)
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
			;  same half-row)
	
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
	add a,c		; (4) 00e2 - Needed to pick up Shift
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
	if MINSTREL4=1
	nop
	nop
	nop
	nop
	else
VS_OFF:	out (0ffh),a	; (11) 0100 - Disable VSync REPLACE - was at 0x100
TB_ON:	out (0feh),a	; (11) 0102 - Enable NMI Generator
	endif
	
	jr nz,l0109h	; (12/7) 0104 - Jump forward if key other than
			;       shift pressed

	;; No real keys pressed, so set LAST_KEY to zero and skip
	;; forward
	ld (hl),a		;0106
	jr l0119h		;0107

	;; For real key presses, need to debounce
l0109h:	ld a,c			; (4) 0109 - Retrieve keypress into A
	bit 7,(hl)		; (14) 010a - Check if change of key
				;      detected on previous iteration
	jr nz,l0119h		; (12/7) 010c   Jump forward if so

	xor (hl)		;010e - Check if same key pressed as on
				;previous iteration
	jr z,l011eh		;010f   Jump forward if so

	cp c			;0111 - Check if first detection of new
				;       key press, and no keypress on previous
				;       iteraton

	jr nz,l0117h		;0112 - Jump forward if different key from
				;       than previously detected

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

	;; ================================================================
	;; Handle multitasking
	;; ================================================================

	;;  Service the timing routines for active tasks
l012eh:	ld hl,(PMTASK_LIST_HD)	;012e - Retrieve address of link field
	push hl			;0131   for task at head of task list list
				;       and save it
	jr l0139h		;0132

	;; Entry point from 0x0171 (end of routine to increment task
	;; timer). At this point, the start of the task's counter is on
	;; the stack and needs discarding
l0134h:	ex (sp),ix		;0134 - Discard top of stack (with
				;       subsequent POP command) without
				;       affecting any registers

	;; Entry point from 0x015E and 0x0169 (having completed
	;; maintenance of task's counter)
l0136h:	pop ix			;0136 - Restore IX 
	pop hl			;0138 - Retrieve address of link field
				;       for next task

l0139h:	push hl			;0139 - Save address of current task
				;       link field

	;; Retrieve address of link field of next task in list. Current
	;; link-field address could be zero, in which case the address
	;; received will be nonesense. However, a check is made later to
	;; see if the link field address is zero and the value read here
	;; is ignored, if so
	ld a,(hl)		;013a
	inc hl			;013b   
	ld h,(hl)		;013c   
	ld l,a			;013d

	;; Check if current task is end of list (i.e., link field
	;; contains zero)
	xor a			;013e - Clear A 

	;; Place address of next link field on stack (which could be
	;; nonesense) and restore current link field
	ex (sp),hl		;013f - Retrieve address in MTASK

	cp h			;0140 - If high byte is zero, assume zero
	jr z,l0178h		;0141   and skip forward, if so

	inc hl			;0143 - Otherwise advance to byte 2
	inc hl			;0144   of task structure

	ld bc,004ffh		;0145 - B is counter for timer update
				;       and C is a status flag

	;; Check if current task's counter (bytes 2, ..., 5) is zero
	;; (Note, A is zero from earlier)
	push hl			;0148 - Save address of start of counter
l0149h:	cp (hl)			;0149 - Check if byte is zero
	jr nz,l016bh		;014a   and skip forward, if so
	inc hl			;014c - Try next digit, unless done
	djnz l0149h		;014d

	;; Counter is zero
l014fh:	ex (sp),ix		;014f - Retrieve address of start of
				;       task counter into IX, saving IX
				;       in the process

	;; Reset counter (copying start value from bits 6,...,9 of
	;; structure into bits 2,...,5
	ld b,004h		;0151
l0153h:	ld a,(ix+004h)		;0153
	ld (ix+000h),a		;0156
	inc ix			;0159
	djnz l0153h		;015b

	inc c			;015d - Check if ready to move on to
				;       next task
	jr z,l0136h		;015e

	;; Check if change to counter limit is required ???
	bit 6,(ix+004h)		;0160 - Check if task is stopped
	jr nz,l0136h		;0164	and move on, if so
	inc (ix+004h)		;0166 - Increment task priority
	
	jr l0136h		;0169

	;; Task's time is non-zero, so increment and done
l016bh:	pop hl			;016b - Retrieve base address of task's timer
	push hl			;016c
	ld bc,l0400h		;016d - Timer is 32-bit, LSB, so will
				;       have up to four bytes to increment
 
l0170h:	inc (hl)		;0170 - Increase digit
	jr nz,l0134h		;0171   Done, if not rolled over
	inc hl			;0173 - Move to next digit
	djnz l0170h		;0174 - Repeat, if not done

	jr l014fh		;0176 - All digits have rolled over, so
				;       reset counter?

	;; Arrive here, once at the end of task list (indicated by
	;; current task's link field address being zero)
l0178h:	pop hl			;0178 - Discard address of next-task's
				;       link field, as is nonsense
l0179h:	pop hl			;0179 - Retrieve address of first/ next
	push hl			;017a   task and save it again

	;; Retrieve address of link field to next task
	ld a,(hl)		;017b
	inc hl			;017c
	ld h,(hl)		;017d
	ld l,a			;017e

	;; Retrieve current link-field (saving address of next link
	;; field to stack)
	ex (sp),hl		;017f - Retrieve MTASK base

	;;  Advance to status field of current task (w/o corrupting DE
	;;  or BC)
	ld a,l			;0180 
	add a,00ah		;0181
	ld l,a			;0183
	ld a,000h		;0184
	adc a,h			;0186
	ld h,a			;0187
	
	or a			;0188 - Check if high byte of address is
				;       zero, in which case we've reached
	jr z,l01dbh		;0189 - the end of the list, so can move
				;       on to check for keypress, if so

	;; Check if task is scheduled/ locked/ etc. 
l018bh:	ld a,(hl)		;018b - Retrieve task status flag

	bit 7,a			;018c Check if Task Locking is set and
	jr nz,l01dbh		;018e move on to check for keypress, if
				;     so

	bit 6,a			;0190 - Check if Task is stopped and
	jr nz,l0179h		;0192   move on to next task, if so

	;; Check task's priority
	or a			;0194 - Move on if task's
	jr z,l0179h		;0195   priority is zero

	;; Service task
	dec (hl)		;0197 - Decrement priority (Bits 6 and 7
				;       are guaranteed to be reset at
				;       this point)
	set 7,(hl)		;0198 - Set task locking to prevent
				;       other tasks running.

	;; Save registers
	push hl			;019a
	push de			;019b
	exx			;019c
	push hl			;019d
	push de			;019e
	push bc			;019f
	push ix			;01a0
	exx			;01a2

	;; Retrieve task address
	inc hl			;01a3
	ld a,(hl)		;01a4
	inc hl			;01a5
	ld h,(hl)		;01a6
	ld l,a			;01a7

	;; Check which machine stack is in use
	push hl			;01a8
	ld hl,FLAGS		;01a9
	bit 4,(hl)		;01ac
	jr nz,l01b6h		;01ae - Jump forward, if Stack 1

	;; Execute task's action word
	pop hl			;01b0
	call jump_to_hl		;01b1

	jr l01cfh		;01b4

l01b6h:	res 4,(hl)		;01b6 - Set to Execution Context
	set 5,(hl)		;01b8 - Lock context switching
	ex (sp),hl		;01ba - Extract task's action address
				;       into HL, saving HL at the same
				;       time.
	
	;; Switch from Stack 1 to Stack 0
	ld (MSTACK1),sp		;01bb 
	ld sp,(MSTACK0)		;01bf

	;; Execute task's action word
	call jump_to_hl		;01c3 - Execute task
	
	ld sp,(MSTACK1)		;01c6 - Switch back to Stack 1 (discard
				;       Stack 0 pointer)

	pop hl			;01ca - Restore HL
	res 5,(hl)		;01cb - Unlock context switching
l01cdh:	set 4,(hl)		;01cd - Reinstate System Editor Context
				;       as active

	;; Restore registers
l01cfh:	pop ix			;01cf
	pop bc			;01d1
	pop de			;01d2
	pop hl			;01d3
	exx			;01d4
	pop de			;01d5
	pop hl			;01d6
	res 7,(hl)		;01d7 - Disable task locking

	jr l018bh		;01d9 - Continue to service task
				;       (assuming priority has not
				;       reached zero)

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

	ld hl,(KBD_ROUT)	;01f2 - Usually $04CF for both Editor
				;       Context/ Execution Context
	call jump_to_hl		;01f5

	pop hl			;01f8

	jr nc,l01fdh		;01f9 - Skip forward, if key
				;       successfully processed
	res 7,(hl)		;01fb - Otherwise, reset to try again
				;       next time

l01fdh:	pop af			;01fd - End of RUN_VSYNC routine
	pop bc			;01fe
	pop hl			;01ff- Corresponds to `push hl` in NMI_DONE

	if MINSTREL4=1
	ei
	endif
l0200h:	ret			;0200

	ds $0201-$		; Spacer for Minstrel 4th (cut-down)
				; version of RUN_VSYNC

	;; Continuation of PUSHC_A (rst 18)
	;; 
	;; At this point
	;; - HL contains PSTACK_C
	;; - Character is top entry on machine stack
l0201h:	ld a,(hl)		;0201 - Retrieve current offset 
	dec a			;0202 - Decrement ready to add new value
	or 0xC0			;0203 - Wrap round from 0xBF to 0xFF
	ld (hl),a		;0205 - Store pointer
	ld l,a			;0206 - Move character-stack pointer
				;       into L to construct 16-bit address
				;       (high byte is always FCh) 
	pop af			;0207 - Retrieve character
	ld (hl),a		;0208 - Store in stack
	pop hl			;0209 - Restore registers

	ret			;020a - Done

	;; Continuation of POPC_A (RST 20)
	;;
	;; At this point:
	;;   HL contains PSTACK_C
	;;   A contains current offset pointer (from PSTACK_C)
l020bh:	ld l,a			; Move character-stack pointer
				; into L to construct 16-bit address
				; (high byte is always FCh)
	inc a			;020c - Increase character stack pointer
	or 0c0h			;020d - Wrap around from 0x00 to 0xC0
	ld (P_STACKC),a		;020f - Store new pointer value
	ld a,(hl)		;0212 - Retrieve character from stack
	pop hl			;0213 - Restore registers

	ret			;0214 - Done

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

	;; Retrieve offset of either top-left corner of current screen
	;; or of current cursor position (depending on IX).
	;;
	;; For current cursor offset (the screen-info pointer is
	;; incremented twice, so this routine works with cursor
	;; coordinates rather than top-left of screen)
	;;
	;; On entry:
	;;   IX - pointer to coordinates of current location
	;;
	;; On exit:
	;;   HL - offset
GET_SCR_OFFSET:
	push af			;022d - Save A

	ld a,(ix+001h)		;022e - Retrieve row coordinate and
	rrca			;0231   multiply by 32 (equiv of five
	rrca			;0232   rotate-left operations)
	rrca			;0233
	ld h,a			;0234 - At this point, bits 5-7 are in
				;       their correct place and bits
				;       0--4 need to be moved to high
				;       byte. Largest row number is
				;       23d=17h which. when multipled by
				;       32 gives 2E0 (which is
				;       represented by E2 at the end of
				;       the above rotations)

	;; Include column offset
	and %11100000		;0235 - Zero low five bits, which were
				;       overflow into high byte
	or (ix+000h)		;0237   and integrate column offset (max
				;       of 31d = %00011111)
	ld l,a			;023a

	;; Normalise high-byte of address (only need low two bits)
	ld a,h			;023b
	and 003h		;023c
	ld h,a			;023e
	
	pop af			;023f - Restore A
	
	ret			;0240 - Done

	;; Retrieve address of current screen location
	;;
	;; On entry:
	;;   IX - current-screen information
	;;
	;; On exit:
	;;   HL - address of current screen location
	;;   All other registers preserved
GET_SCR_ADDR:
	push ix			;0241
	push de			;0243

	;; Update IX to point to current-location information for screen
	ld de,0x0002		;0244
	add ix,de		;0247

	;; Retrieve address of current screen into DE (preserving HL)
	ex de,hl		;0249
	ld hl,(P_DBUFFER)	;024a
	ex de,hl		;024d

	call GET_SCR_OFFSET	;024e - Retrieve offset of current
				;       location into HL. Note by
				;       changing IX we effectively work
				;       with two subsequent bytes of
				;       screen information
	
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
				;       into HL
	ex de,hl		;025a - Move to DE
	call GET_SCR_OFFSET	;025b - Retrieve offset to current
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
	ld bc,0x0000		;0261 - Required adjustment (to screen
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
	xor %10000000		;027d
	ld (hl),a		;027f

	pop hl			;0280
	pop af			;0281

	ret			;0282


	;; Clear screen
	;;
	;; On entry:
	;;   IX - screen info
CLS:	call GET_SCR_SIZE	;0283 - BC = width, height of screen
	ret c			;0286 - Return if zero-sized screen
	
	call GET_SCR_ADDR	;0287 - HL = start of screen

	ld de,DISP_WIDTH	;028a - DE = row length

	ld a,(ix+007h)		;028d - Retrieve blank character for screen
CLS_LOOP:
	call SCR_BLANK_ROW	;0290
	add hl,de		;0293 - Advance to next row

	dec c			;0294 - Check if more rows to deal with
	jr nz,CLS_LOOP		;0295 

	;; Proceed to move cursor to home position
	
	;; Move cursor to home position
	;;
	;; On entry:
	;;   IX - screen info
	;;
	;; On exit:
GO_HOME:
	call CR			;0297 - Advance to start of next line
				;       (*** could save space by
				;       dropping this call and simply
				;       removing subsequent return
				;       statement)
	ld (ix+001h),0x00	;029a - Set current row to zero
	
	ret			;029e - Done

	;; Carriage return (no line feed)
CR:	ld (ix+000h),000h	;029f
	ret			;02a3

	;; Deal with cursor moving off bottom of screen
SCROLL_SCRN:
	push hl			;02a4
	push de			;02a5
	push bc			;02a6

	call GET_SCR_SIZE	;02a7 - Retrieve screen size into BC
	jr c,l02c2h		;02aa - Done if screen is negative sized
	call GET_SCR_ADDR	;02ac

	ld de,DISP_WIDTH		;02af - Set DE to width of display

	;; Scroll screen
SS_KERNEL:
	dec c			;02b2 - Rows to scroll is screen height-1
	jr z,l02bch		;02b3 - Jump forward if only one row on
				;       screen

	;; Move current row up by one
SS_LOOP:
	call SCR_ROW_SCROLL	;02b5

	;; Advance to next row
	add hl,de		;02b8

	dec c			;02b9 - Repeat if more rows to scroll
	jr nz,SS_LOOP		;02ba

l02bch:	ld a,(ix+007h)		;02bc
	call SCR_BLANK_ROW	;02bf

l02c2h:	pop bc			;02c2
	pop de			;02c3
	pop hl			;02c4

	ret			;02c5


	;; Move cursor down (prefix for LINE_FEED)
DOWN:	push hl			;02c6 - Save registers
	push de			;02c7   
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

	;; Advance current character position (also used for cursor
	;; right)
	;;
	;; On entry:
	;;   IX - screen information
	;; 
	;; On exit:
	;; 
ADVANCE_CUR:
RIGHT:	
	push hl			;02e3
	push de			;02e4
	push bc			;02e5

	call GET_SCR_SIZE	;02e6 - BC = width/ height
	jr c,AC_DONE		;02e9 - Done if zero-size screen
	
	ld a,(ix+000h)		;02eb - Get current column 
	inc a			;02ee - Advance right
	cp b			;02ef - Compare to screen width
	jr c,AC_CONT		;02f0 - Jump forward if not past end of screen
	
	call CR			;02f2 - Reset current column

	jp LINE_FEED		;02f5 - Advance to next row and done

AC_CONT:
	ld (ix+000h),a		;02f8 - Store current column and done

AC_DONE:
	pop bc			;02fb
	pop de			;02fc
	pop hl			;02fd

	ret			;02fe

	;; Move cursor up one screen row (assuming not at top)
UP:	ld a,(ix+001h)		;02ff - Retrieve current row 
	or a			;0302 - No action, if top of screen
	ret z			;0303
	
	dec (ix+001h)		;0304 - Otherwise decrement

	ret			;0307

	;;
	;; Move cursor left
	;;
	;; On entry:
	;;   IX - screen info
	;; 
	;; On exit:
	;; 
LEFT:	ld a,(ix+000h)		;0308 - Retrieve current column
	or a			;030b - Check if beginning of line
	ld b,a			;030c
	jr nz,LEFT_CONT		;030d - and skip forward if not
	
	call UP			;030f - Start of line, so move up
	nop			;0312
	call GET_SCR_SIZE	;0313 - Set B to last column
	ret c			;0316 - Return if zero-sized screen

LEFT_CONT:
	dec b			;0317 - Decrease column index 
	ld (ix+000h),b		;0318

	ret			;031b - Done
	
	;; Not sure if this is ever called
	call GET_SCR_SIZE	;031c
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
	if MINSTREL4=1
	and %01111111		;032a - Mask off bits 7
	else
	and %00111111		;032a - Mask off bits 7 and 6
	endif
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
	push af			;0333 - Save registers
	push hl			;0334
	push de			;0335
	push bc			;0336

	call SCR_INV_CUR		;0337 - (Un-)invert character at
				;	current screen location (as
				;	cursor highlight will move on)

	;; Convert character to 7-bit ASCII (lowercase letters replaced
	;; by uppercase)
	and %01111111		;033a - Mask off Bit 7 of character to
				;       print
	cp "`"			;033c - Is it < "£" symbol (same as
				;       ASCII "`" or 60h)
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
	cp _SPACE		;0342 - Is it >= " " (printable
				;       ASCII char)
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
	if MINSTREL4=1
	nop
	nop
	else
	add a,0xE0		;0357 - Shift character code to 00--3F
	endif
	
	call GET_SCR_POSN	;0359 - Get current screen location
				;       (into HL)
	xor (ix+006h)		;035c - Apply cursor mask

	ld (hl),a		;035f - Write character to screen

	call ADVANCE_CUR	;0360 - Advance cursor

	jr SPC_DONE		;0363 - Done

	;; Jump to address to which HL points
	;; 
	;; On entry:
	;;   HL - location of address
	;;
	;; On exit
	;;   A corrupt
	;;   HL = HL+1
JP_ADDR_HL:
	ld a,(hl)		;0365
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
INSERT_CHAR:
	push hl			;036a
	push de			;036b
	push bc			;036c
	push hl			;036d

	;; Check if inserting/ deleting character
	or a			;036e
	sbc hl,de		;036f
	jr nc,IC_DEL		;0371 - Jump forward, if deleting

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

	jr IC_DONE		;037c - Done

	;; Shift tail of line left
IC_DEL:	pop hl			;037e

	ldir			;037f

IC_DONE:
	pop bc			;0381
	pop de			;0382
	pop hl			;0383

	ret			;0384

	;; Delete character under cursor (Editor mode)
	;;
	;; On entry:
	;;   IX - screen information
RUBOUT:	call GET_SCR_POSN	;0385 - HL contains address of cursor
	call GET_SCR_SIZE	;0388 - BC = width, height

	ld a,b			;038b - Retrieve width
	ld b,000h		;038c - Zero upper byte of BC for later
	sub (ix+000h)		;038e - Subtract current column value to
	dec a			;0391   give number of chars to end of
				;       line
	ld c,a			;0392 - BC contains remaining character
	jr z,RO_CONT		;0393 - Jump forward if at end of line

	push hl			;0395 - Move current position into 
	pop de			;0396   DE and set HL to next position
	inc hl			;0397
	call INSERT_CHAR	;0398 - Will delete character

	;; Inset rubout character
RO_CONT:
	ld a,c			;039b - Length of line end to A
	add a,e			;039c
	ld l,a			;039d
	ld a,(ix+007h)		;039e - Retrieve rubout character
	ld (hl),a		;03a1   and replace
	
	ret			;03a2 - Done

DEL_LINE:	ld a,(ix+003h)		;03a3
	push af			;03a6
	add a,(ix+001h)		;03a7
	ld (ix+003h),a		;03aa
	call SCROLL_SCRN	;03ad
	pop af			;03b0
	ld (ix+003h),a		;03b1
	ret			;03b4

CHECK_EDIT_MODE:
	push hl			;03b5

	ld hl,FLAGS		;03b6
	bit 0,(hl)		;03b9

	pop hl			;03bb

	ret			;03bc

ED_SCROLL:
	;; Save registers
	push hl			;03bd
	push de			;03be
	push bc			;03bf

	call GET_SCR_SIZE	;03c0 - Retrieve screen size into BC
	ld a,(ix+003h)		;03c3 - Retrieve top-most row of window
	push af			;03c6 - Save it
	ld a,(ix+005h)		;03c7 - Retrieve bottom-most row 
	ld (ix+003h),a		;03ca   and set as top row
	call GET_SCR_ADDR	;03cd - Get start of final row

	pop af			;03d0 - Restore top row value
	ld (ix+003h),a		;03d1

	ld de,0xFFE0		;03d4 - -32d (length of a screen row)

	jp SS_KERNEL		;03d7 - Proceed with screen-scroll
				;       routine

	;; Insert line on editor screen
INS_LINE:
	ld a,(ix+003h)		;03da - Retrieve top row of window
	push af			;03dd - Store it
	add a,(ix+001h)		;03de - Retrieve current row 
	ld (ix+003h),a		;03e1 - Set as top of window
				;       (temporarily)
	call ED_SCROLL		;03e4

	;; Restore screen dimensions
	pop af			;03e7 - Retrieve top row of window
	ld (ix+003h),a		;03e8 - and store it

	jp CR			;03eb

	;; Insert current line from (editor) screen into display pad.
	;;
	;; Note this is different from PAD
	;;
	;; On entry:
	;;
	;; On exit:
	;; 
PUT_DPAD:
	;; Put pad only works in editor mode
	call CHECK_EDIT_MODE	;03ee - 0 = System Execution Context/ 1
				;       = System Editor Contaxt
	ret z			;03f1 - No action, if System Execution
				;       context
	
	call CR			;03f2
	call GET_SCR_POSN	;03f5 - Get current screen position
	push hl			;03f8 - Save it
	
	ld hl,(P_DBUFFER)	;03f9 - Retrieve start of display buffer
	ld de,16*DISP_WIDTH	;03fc - 16 screen lines?
	add hl,de		;03ff - Point to display copy of PAD

l0400h:	ex de,hl		;0400 - DE points to display copy of pad
	pop hl			;0401 - HL points to current screen position
	call GET_SCR_SIZE	;0402 - B=width; C=height
	ld c,b			;0405 - BC = width
	ld b,000h		;0406
	call INSERT_CHAR	;0408 - This routine will copy current
				;       line into display copy of PAD

	ex de,hl		;040b - DE points to current screen
				;       position; HL points to display
				;       copy of pad
	ld b,c			;040c - B contains length of screen line
	call INVERT_LINE	;040d
	
	ret			;0410

	;; Invert a sequence of cells of display buffer
	;;
	;; On entry:
	;;   HL - address of start of sequence
	;;   B  - number of characters
	;;
	;; On exit:
	;;   A - corrupted
INVERT_LINE:
	push hl			;0411
	push bc			;0412

IL_LOOP:
	ld a,(hl)		;0413
	xor %10000000		;0414
	ld (hl),a		;0416
	inc hl			;0417
	djnz IL_LOOP		;0418
	
	pop bc			;041a
	pop hl			;041b

	ret			;041c

l041dh:	call CHECK_EDIT_MODE	;041d 
	ret z			;0420 - Return if in System Execution Context
	call GET_SCR_SIZE	;0421 - Retrieve window size into BC
	ld hl,(P_DBUFFER)	;0424

	;; Advance to row 16
	ld de,16*0x20		;0427 - Sixteen rows
	add hl,de		;042a
	call INVERT_LINE	;042b

	ex de,hl		;042e
	call CR			;042f
	call GET_SCR_POSN	;0432
	ex de,hl		;0435
	ld c,b			;0436
	ld b,000h		;0437
	call INSERT_CHAR	;0439
	ld b,c			;043c
	call INVERT_LINE	;043d

	ret			;0440

	call CHECK_EDIT_MODE	;0441
	ret z			;0444
	call PUT_DPAD		;0445
	jp DEL_LINE		;0448

	call CHECK_EDIT_MODE	;044b
	ret z			;044e
	call INS_LINE		;044f
	jp l041dh		;0452


	;; Switch between editor and console screens and, if not
	;; previously activated, activate editor screen.
EDIT:	push hl			;0455

	ld hl,FLAGS		;0456
	bit 0,(hl)		;0459 - Check if console/ editor mode
	jr nz,E_TO_CONSOLE	;045b - Jump forward if editor

	set 0,(hl)		;045d - Set new mode to be editor
	bit 6,(hl)		;045f - Check if Editor screen is visible
	jr nz,E_DONE		;0461 - Jump forward, if so

	set 6,(hl)		;0463
	
	ld l,(SCR_INFO_CO & 0x00FF) + 0x03
				;0465 - HL points into Console Screen
				;       info   (would be IX+3)
	ld (hl),0x11		;0467 - Set top row of console to 17d

	ld a,_PUTPAD		;0469 - Copy first line of screen
				;       (copyright) into PAD
	call PROCESS_KEY	;046b

	ld a,_CLS		;046e
	call PROCESS_KEY	;0470

	call SCR_PR_CHR		;0473

E_DONE:	pop hl			;0476

	ret			;0477

E_TO_CONSOLE:
	res 0,(hl)		;0478

	pop hl			;047a

	ret			;047b

	;; ================================================================
	;; Handle special character _FLASH (0x1F)
	;; Flip Bit 7 of FLAGS to invert/ uninvert character at cursor
	;; ================================================================
	;; On entry:
	;;   IX - points to screen information for current screen
	;;
	;; On exit:
	;; ================================================================
FLASH_CURSOR:	
	push hl			;047c - Save registers

	ld hl,FLAGS		;047d - Check if cursor needs inverting
	bit 7,(hl)		;0480

	set 7,(hl)		;0482 - Assume not, and set to invert
				;       next time

	jr z,FC_DONE		;0484 - If not to invert, all done

	res 7,(hl)		;0486 - Otherwise invert cursor
	call INVERT_CUR_CHAR	;0488

FC_DONE:
	pop hl			;048b - Restore registers

	ret			;048c - Done

PRINT_FLASH:
	ld a,_FLASH		;048d
	ld hl,(KBD_ROUT)	;048f

	jp (hl)			;0492


	;; ================================================================
	;; Print character (inc. control chars) to screen and printer
	;; ================================================================
	;; On entry:
	;;   A = character to print
	;;
	;; On exit:
	;;   CF - reset = success ; set = nothing printed
	;; ================================================================
PRINT_A:
	push hl			;0493 - Save HL
	
	ld hl,FLAGS2		;0494

	;; Check for special case of cursor flash (which is periodically
	;; inserted into KIB, during periods of inactivity). Skip over
	;; printer check, if so, as not needed.
	cp _FLASH		; 
	jr z,PA_CONT		; Jump forward if so, as not for
				; printer

	;; Check if printer is enabled, and jump forward if not
	bit 0,(hl)		;049b - Bit 0 of FLAGS2 
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
	
	cp _EDIT		;04b4 Check for EDIT (character 0x1E)
	jr z,PR_PRINT_A		;04b6 Jump forward, if so

	bit 2,(hl)		;04b8 If FLAGS(2) is non-zero, then do
				;     not print
	set 2,(hl)		;04ba Block further printing until done  

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

	
	;; Handle keypress
	;;
	;; If System Execution mode is enabled, keypress is handled by
	;; CO_KBD_ROUT. Otherwise, (if System Editor), write to ED
	;; screen.
	;; 
	;; On entry, A contains ASCII code of key press
	;; 
PROCESS_KEY:
	push hl			;04cf
	ld hl,FLAGS		;04d0 - Check which input screen is
	bit 0,(hl)		;04d3 - active (System Execution or
				;       Editor)
	scf			;04d5
	jr nz,PK_EDITOR		;04d6 - Jump forward if editor mode

	ld hl,(CO_KBD_ROUT)	;04d8 - Likely 0x08f8 - Add to keyboard
	call jump_to_hl		;04db   input buffer ???

PK_DONE:
	pop hl			;04de
	
	ret			;04df

	;; Handle keypress in Editor mode
PK_EDITOR:
	bit 1,(hl)		;04e0 - Check FLAGS(1) and skip printing
				;       if set
	set 1,(hl)		;04e2
	jr nz,PK_DONE		;04e4
	
	push ix			;04e6

	ld ix,SCR_INFO_ED	;04e8
	cp _SPACE		;04ec - Check for printable character
	jr c,PK_E_CTRL		;04ee   and jump forward if not
	call PK_E_CHR_PRNT	;04f0

PK_NEARLY_DONE:
	res 1,(hl)		;04f3

	pop ix			;04f5
	
	or a			;04f7 - Reset carry
	
	jr PK_DONE		;04f8 - Done

	;; Handle control characters in Editor mode
PK_E_CTRL:
	cp _ENTER		;04fa - Check for Enter
	jr nz,PK_E_CONT		;04fc   and jump forward if not

	call SCR_PR_CHR		;04fe
	ld a,_DOWN		;0501
PK_E_PRNT:
	call SCR_PR_CHR		;0503
	jr PK_NEARLY_DONE	;0506

PK_E_CONT:
	cp _COMPILE		;0508 - Check for Shift-Q (Compile Line - 018h)
	jr nz,PK_E_PRNT		;050a   and jump if not

	call COMPILE_LN		;050c

	jr PK_NEARLY_DONE	;050f - Done

	
	;; Print character to editor window
PK_E_CHR_PRNT:
	push hl			;0511
	push de			;0512
	push bc			;0513
	push af			;0514

	call SCR_INV_CUR	;0515
	call GET_SCR_POSN	;0518 - HL = screen position
	call GET_SCR_SIZE	;051b - B = width ; C = height

	;; Set BC to be number of characters to right of cursor
	ld a,b			;051e - Retrieve width
	ld b,000h		;051f

	;; Check if end of line
	sub (ix+000h)		;0521 - Current column value
	dec a			;0524
	ld c,a			;0525 - BC set to right-length of row
	jr z,PK_E_CONT2		;0526

	;; Set DE to next cursor position
	push hl			;0528
	pop de			;0529
	inc de			;052a

	;; Insert character in current line
	call INSERT_CHAR	;052b

PK_E_CONT2:
	call INVERT_CUR_CHAR	;052e

	pop af			;0531

	call SCR_PR_CHR		;0532

	pop bc			;0535
	pop de			;0536
	pop hl			;0537

	ret			;0538


	;; Shift-Q -- Compile line (editor mode) by copying it into the
	;; KIB
	;;
	;; On entry:
	;;   IX - screen info
COMPILE_LN:
	push hl			;0539
	push de			;053a
	push bc			;053b

	call SCR_INV_CUR	;053c - Uninvert cursor and move 
	call CR			;053f   to start of line
	call GET_SCR_POSN	;0542 - HL point to start of current line
	call GET_SCR_SIZE	;0545 - B = width ; C = height

	dec b			;0548 - Decrement width
	push hl			;0549 - Save screen position

	;; Update HL to point to end of line
	ld a,b			;054a - Set HL to end of line
	add a,l			;054b
	ld l,a			;054c
	jr nc,CL_CONT		;054d
	inc h			;054f

CL_CONT:
	ld a,(hl)		;0550 - Retrieve character at end of
	and 07fh		;0551   line and check if actionable
	jr nz,CL_FULL_LINE		;0553 - Skip forward if so

	;; Scan left along line to be compiled until find an actionable
	;; character
CL_LOOP:
	dec hl			;0555
	ld a,(hl)		;0556
	and 07fh		;0557
	jr nz,CL_COPY_LINE	;0559
	djnz CL_LOOP		;055b

CL_COPY_LINE:
	pop hl			;055d - Retrieve start of line
	call CL_LINE_TO_KIB	;055e
	ld a,_ENTER		;0561 - Add carriage return
	call CL_WRITE_TO_KIB	;0563
	jr CL_DONE		;0566

	;; Last character cell contains an actionable value, so line is
	;; full (no ENTER character needed)
CL_FULL_LINE:
	pop hl			;0568
	inc b			;0569 - Increment row length
	call CL_LINE_TO_KIB	;056a

CL_DONE:
	call INVERT_CUR_CHAR	;056d
	
	pop bc			;0570
	pop de			;0571
	pop hl			;0572

	ret			;0573

	;; Transfer line to KIB
CL_LINE_TO_KIB:
	xor a			;0574 - Check if done?
	cp b			;0575
	ret z			;0576
l0577h:	ld a,(hl)		;0577
	if MINSTREL4=1
	nop
	nop
	else
	add a,020h		;0578
	endif
	
	call CL_WRITE_TO_KIB		;057a - Write to KKIB
	inc hl			;057d
	djnz l0577h		;057e
	
	ret			;0580

CL_WRITE_TO_KIB:
	push hl			;0581

	ld hl,(CO_KBD_ROUT)	;0582 - Retrieve address of routine to
				;       write to keyboard input buffer
	call jump_to_hl		;0585

	pop hl			;0588

	ret			;0589

	;; Compile the contents of the Editor screen
COMPILE_SCREEN:
	;; Check if editor mode (i.e., not execution mode) and return,
	;; if so
	call CHECK_EDIT_MODE	;058a 
	ret nz			;058d

	ld hl,FLAGS3		;058e
	bit 1,(hl)		;0591 - Check ???
	res 1,(hl)		;0593
	ret z			;0595 - Return, if not

	ld b,00fh		;0596 - Maximum 15 lines in editor window???
	ld a,_EDIT		;0598 - Switch to editor window
	call COMP_PROC_KIB	;059a
	ld a,_HOME		;059d - Move cursor to home position
	call PROCESS_KEY	;059f

COMP_LOOP:
	ld a,_COMPILE		;05a2 - Compile line
	call PROCESS_KEY	;05a4
	ld a,_DOWN		;05a7 - Advance to next line
	call COMP_PROC_KIB	;05a9
	djnz COMP_LOOP		;05ac
	
	ld a,_COMPILE		;05ae
	call COMP_PROC_KIB	;05b0
	ld a,_EDIT		;05b3

COMP_PROC_KIB:
	call PROCESS_KEY	;05b5
	jp FLUSH_KIB		;05b8


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

	;; Restore registers and done (note return will be to other
	;; System context, if switch was possible)
SMS_EXIT:
	pop ix			;05e5
	pop bc			;05e7
	pop de			;05e8
	pop hl			;05e9

	ret			;05ea

	;; Startup routine
INIT_MSTACKS:
	ld hl,FLAGS		;05eb
	res 4,(hl)		;05ee - Using System Execution Stack
	res 5,(hl)		;05f0 - Confirm stack switch not in
				;       progress

	ld hl,C1_MAIN_LOOP	; Insert address of main loop at
	ld (STACK1_BASE),hl	; head of System Editor Stack

	ld hl,STACK1_BASE-8	; Set pointer to fourth word on
	ld (MSTACK1),hl		; machine Editor Stack. Will balance the
				; first time that we switch to System
				; Editor mode

	ret			;05fe

	;; Handle most recent Keyboard Input Buffer Entry
	;;
	;; On entry:
	;;   A - keyboard entry
	;;
	;; On exit:
	;;   
PARSE_KIB_ENTRY:
	cp _EDIT		;05ff - Check if Edit key pressed
	jp z,PRINT_A		;0601   and jump forward, if so
	set 7,a			;0604 - Confirm key has been handled?
	
	push hl			;0606 - Save KIB offset

	;; Check which context is active and, unless FLAG 3 is set,
	;; switch to Context 0
	ld hl,FLAGS		;0607
	bit 4,(hl)		;060a - Check which stack context is active
	jr z,PKE_EXIT		;060c - Jump forward, if Context 0

	bit 3,(hl)		;060e
	set 3,(hl)		;0610
	jr nz,PKE_EXIT		;0612

	call SWITCH_MSTACK	;0614 - Change execution context

	res 3,(hl)		;0617
	or a			;0619
	bit 7,a			;061a
	jr z,PKE_DONE		;061c

PKE_EXIT:
	scf			;061e

PKE_DONE:
	pop hl			;061f

	ret			;0620


	;; Switch to Context 1
	;;
	;; On entry:
	;;   Carry significant ???
	;;
	;; On exit:
	;;   Depends on what happens in Stack 1 context
SWITCH_TO_MSTACK1:
	ld a,(FLAGS)		; Check which machine stack is active
	bit 4,a		

	ret nz			; Nothing to do if Stack 1 is already
				; active

	;; Know Stack 0 is active
	sbc a,a			; A = 0, if carry clear, otherwise A = 0xFF

	call SWITCH_MSTACK	; Switch stack
	
	ret			; 062b - Return once context has
				; switched back to Execution Stack

NEWLINE_AND_OK:
	push hl			;062c

	ld hl,FLAGS3		;062d
	set 0,(hl)		;0630

	call PRINT_NEW_LINE	;0632

	pop hl			;0635
	
	ret			;0636


	;; ================================================================
	;; Push string onto character stack (in reverse order)
	;; ================================================================
	;; On entry:
	;;   HL - pointer to countable string
	;;
	;; On exit:
	;;   Length of string on parameter stack
	;;   String (reversed) on character stack
	;;   AF - corrupted
	;; ================================================================
PUSH_STRING:
	push hl			;0637 - Save address of string

	ld a,(hl)		;0638 - Retrieve string length and limit
	ld h,000h		;0639   to 63 bytes maximum
	and 03fh		;063b - Also sets Z, if zero-length string
	ld l,a			;063d - HL = length of string
	
	rst 8			;063e - Push length (that is, HL) onto
				;Param stack

	jr z,CS_DONE		;063f - Jump forward if zero-length string

	pop hl			;0641 - Retrieve string address
	push hl			;0642

	push bc			;0643 - Save BC
	ld b,a			;0644 - Set B to length of string

	;;  Find address of last character of string
	add a,l			;0645 - Set HL to point to end of string
	ld l,a			;0646   taking account of potential carry-over
	jr nc,CS_LOOP		;0647   from low byte to high
	inc h			;0649

	;;  Push string onto stack in reverse order (end of string is
	;;  lowest on stack), so can be pop'ed in right order
CS_LOOP:
	ld a,(hl)		;064a - Retrieve next character
	rst 18h			;064b - Push A onto character stack
	dec hl			;064c - Decrement pointer and repeat,
	djnz CS_LOOP		;064d   if necessary

	pop bc			;064f - Restore registers
CS_DONE:
	pop hl			;0650

	ret			;0651 - Done

	;; ================================================================
	;; Print string from character stack
	;;
	;; On entry:
	;;   - String stored on character stack (first character at TOS)
	;;   - Length of string stored on parameter stack (max 63 chars)
	;;
	;; On exit:
	;;   - String removed from character stack (and length
	;;     removed from parameter stack)
	;;   - All registers preserved
	;; ================================================================
PRINT_STRING:
	push af			;0652 - Save registers
	push hl			;0653
	
	rst 10h			;0654 - Retrieve string length from
				;       parameter stack into HL
	ld a,l			;0655 - Max length of string is 63 characters
	and 03fh		;0656   (assumes H is zero)

	jr z,PS_DONE		;0658 - Exit routine, if empty string

	ld l,a			;065a - HL is length of string

PS_LOOP:
	rst 20h			;065b - Retrieve next character from
				;       Character Stack into A
	call PRINT_A		;065c - Print to screen

	dec l			;065f - Decrement counter and repeat,
	jr nz,PS_LOOP		;0660   if necessary

PS_DONE:
	pop hl			;0662 - Restore registers
	pop af			;0663

	ret			;0664 - Done


	;; ================================================================
	;; Print counted string pointed to by HL register (via character
	;; stack)
	;; ================================================================
	;;
	;; On entry:
	;; 	HL - address of counted string
	;;
	;; On exit:
	;;      HL - corrupted
	;; ================================================================

PRINT_STR_HL:
	push af			;0665 - Save registers

	call PUSH_STRING	;0666 - Push string onto character stack
	call PRINT_STRING	;0669 - Print string from character stack

	pop af			;066c - Restore registers

	ret			;066d - Done

	
PRINT_OK:
	push hl			;066e
	ld hl,OK_MSG		;066f
	call PRINT_STR_HL	;0672
	pop hl			;0675
	ret			;0676


	;; Convert a character into a digit, in current base
	;;
	;; Assumes digits are 0, 1, 2, ..., 9, A, B, ..., up to limit
	;; for current base
	;;
	;; On entry:
	;;   A - ASCII character code to convert
	;;
	;; On exit:
	;;   A - numerical value
	;; 
ATOI:	and %01111111		;0677 - Mask off inverse flag
	cp 060h			;0679 - Check for lower-case letter
	jr c,l067fh		;067b
	add a,0e0h		;067d - Convert to upper case (Subtract 0x20)
l067fh:	add a,0d0h		;067f - Subtract 0x30 ('0' mapped to 0,
				;       and so on)
	cp 010h			;0681 - Check for digit and return, if so
	ret c			;0683

	add a,0f9h		;0684 - Subtract 7 (so that 'A' = 10, and so on)

	ret			;0686

	;; Pop string from character stack and write to memory (usually
	;; into dictionary)
	;; 
	;; On entry:
	;; - HL - Destination address for string (usually HERE in dictionary)
	;; - Token is on Character Stack (with length on Parameter Stack)
	;;
	;; On exit:
	;; - String has been removed from Character Stack
STRING_TO_MEM:
	push af			;0687 - Save registers
	push hl			;0688
	push de			;0689
	
	ex de,hl		;068a - Move destination address to DE
	rst 10h			;068b - Pop string length into HL

	ld a,l			;068c - Maximum string length is 0x3F (63d)
	and %00111111		;068d
	
	ld (de),a		;068f - Store string length
	jr z,TOM_DONE		;0690 - Check if done (zero flag set by
				;       `and` command above)

	ld l,a			;0692 - String length to L

TOM_LOOP:
	inc de			;0693 - Advance dictionary pointer
	
	rst 20h			;0694 - Pop from character stack into A
	ld (de),a		;0695 - Store to dictionary
	dec l			;0696 - Loop until done
	jr nz,TOM_LOOP		;0697

TOM_DONE:
	pop de			;0699
	pop hl			;069a
	pop af			;069b

	ret			;069c


MATCH_STACK_STRINGS:
	push hl			;069d - Save registers
	push de			;069e
	push bc			;069f
	
	rst 10h			;06a0 - Retrieve TOS into DE
	ex de,hl		;06a1
	rst 10h			;06a2 - Retrieve 2OS into HL
	ld a,(hl)		;06a3 - Retrieve string length and
	inc hl			;06a4   advance pointer

	jr l06b1h		;06a5

	;; Match string
	;;
	;; On entry:
	;; - HL - points to candidate match (counted string)
	;; - String on Character stack and string-length on Parameter
	;;   Stack
	;;
	;; On exit:
	;; - Character Stack and Parameter Stack preserved
	;; - Z = match (or zero match string)/ NZ = no match
	;; - A corrupted
MATCH_STRING:
	push hl			;06a7
	push de			;06a8
	push bc			;06a9

	ex de,hl		;06aa - DE points to match string

	;; Retrieve length of token into A (should be one byte) via HL
	rst 10h			;06ab - Pop HL from Param stack
	rst 8			;06ac - Push HL to Param stack (to
				;       preserve stack)
	ld a,l			;06ad 

	ld hl,(P_STACKC)	;06ae - Retrieve character-stack current
				;       position

l06b1h:	and %0111111		;06b1 - Max 127-byte match
	ld b,a			;06b3 - Set loop counter to length of string
	ld a,(de)		;06b4 - Retrieve length of match string
	and %0111111		;06b5 - Max 127
	cp b			;06b7 - Check if strings are same length
	jr nz,MS_DONE		;06b8 - Skip forward, if not, as no match
	or a			;06ba - Check if match-string is zero-length
	jr z,MS_DONE		;06bb - Skip forward if so, as no match

MS_LOOP:
	inc de			;06bd - Advance to next character
	ld a,(de)		;06be - Retrieve and ...
	cp (hl)			;06bf - ... compare to corresponding in string
	jr nz,MS_DONE		;06c0 - Exit if not same

	inc hl			;06c2 - Check again
	djnz MS_LOOP		;06c3 - Repeat for every character

MS_DONE:
	pop bc			;06c5 - Restore registers
	pop de			;06c6
	pop hl			;06c7
	
	ret			;06c8

	;; Routine to handle user error (Part 2)
PRINT_ERR_MSG:
	push hl			;06c9

	ld hl,ERR_MSG		;06ca
	call PRINT_STR_HL	;06cd
	call PRINT_A		;06d0
	ld a,_SPACE		;06d3
	call PRINT_A		;06d5

	pop hl			;06d8

	ret			;06d9

PRINT_NEW_LINE:
	push hl			;06da
	
	ld hl,NEW_LINE_MSG	;06db - Message 02, 0D, 0A
	call PRINT_STR_HL	;06de

	pop hl			;06e1

	ret			;06e2


	;; Check for valid word and, assuming so, find start of its
	;; parameter field
	;;
	;; On entry:
	;;
	;; On exit:
	;;   HL - address of word's parameter field
	;;   Error restart, if no valid word
TICK_WORD:
	call GET_WORD		;06e3

	ld a,_U			;06e6 - Undefined word
	jp c,ERR_RESTART	;06e8

	call FIND_PARAM_FIELD	;06eb

	jp UNSTACK_STRING	;06ee

	;; Read key sequence (corresponding to white-space-separated
	;; token) from keyboard and assemble for parsing.
	;;
	;; Read token from keyboard, echoing key entries to screen
	;;
	;; On exit:
	;;   Token is on Character Stack (length on Parameter Stack)
	;;   A - corrupted
READ_TOKEN:
	push hl			;06f1
	
	ld hl,TOKEN_LN		;06f2 - Zero current-token length
	ld (hl),0x00		;06f5

RT_GET_CHAR:
	or a			;06f7 - Clear carry for next subroutine

	call SWITCH_TO_MSTACK1	;06f8 - Activate Context 1 (check for
				;       keyboard input and perform
				;       system house-keeping)

	;; At this point, A contains most recent entry in KIB + 0x80
	and %01111111		;06fb - Mask off bit 7

	cp _EDIT		; Check if control character (other than
	jr c,RT_CTRL		; EDIT (1E) or FLASH (1F): branch if so

	call PRINT_A		;0701 - Print character

	cp _SPACE		;0704 - Check for SPACE (triggers parser)
	jr z,RT_PARSE_TOKEN	;0706 - Parse token
	
	call nc,STR_ADD_CHR	;0708 - If printable character, add to
				;       token string

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
	cp _ENTER		;0717 - Check for carriage return
	jr nz,RT_CONT		;0719 - Jump forward, if not

	call NEWLINE_AND_OK	;071b

	jr RT_DONE		;071e

RT_CONT:
	cp _RUBOUT		;0720 - Check for Rubout
	jr z,RT_RUBOUT		;0722 - Jump forward, if so
	call STR_ADD_CHR	;0724
	ld a,_PERIOD		;0727 - Display as "."

RT_PRINT_CHAR:
	call PRINT_A		;0729
	jr RT_GET_CHAR		;072c

RT_RUBOUT:
	ld a,(hl)		;072e - Retrieve current-token length
	or a			;072f - Skip forward if zero-length
	jr z,RT_GET_CHAR	;0730   as nothing to rub out

	dec (hl)		;0732
	ld a,_LEFT		;0733
	jr RT_PRINT_CHAR	;0735

	
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
	ld (MTASK_TAIL),hl		;0747
	ld hl,(02006h)		;074a
	ld (P_HERE),hl		;074d

	;; Deal with recognised ROM
l0750h:	ld hl,(02004h)		;0750 - Jump address in ROM
	ld (PSTART_DICT),hl		;0753

sub_0756h:
	push hl			;0756
	ld hl,(PSTART_DICT)	;0757
	ld (START_DICT_DEF),hl	;075a
	ld hl,(P_HERE)		;075d
	ld (UNKNOWN2),hl		;0760
	pop hl			;0763

	ret			;0764

INIT_DICT:
	push hl			;0765

	ld hl,(START_DICT_DEF)		;0766
	ld (PSTART_DICT),hl		;0769
	ld hl,(UNKNOWN2)		;076c
	ld (P_HERE),hl		;076f

	pop hl			;0772

	ret			;0773

	;; Compute and add link field to word definition
	;; GOT THIS FAR
ADD_LINK_FIELD:
	push de			;0774
	
	ex de,hl		;0775 - Move entry point to DE
	ld hl,(PSTART_DICT)	;0776 - Head of existing dictionary

	or a			;0779 
	sbc hl,de		;077a

	call DICT_ADD_HL	;077c
	ex de,hl		;077f - Restore HL
	
	pop de			;0780 - Restore DE

	ret			;0781

	;; Read token from keyboard and check if matches to word in
	;; dictionary
	;;
	;; On entry:
	;;
	;; On exit:
	;;   Word in Character Stack (length on Parameter Stack)
	;;   CF - reset (matched) / set (no match)
	;;   HL - matching word / corrupt
GET_WORD:
	call READ_TOKEN ;0782

	;; Check if token in string buffer corresponds to Forth word in
	;; dictionary.
	;;
	;; On exit:
	;;   CF - reset (matched) / set (no match)
	;;   HL - matching word / corrupt
	;;   A  - corrupted
CHECK_FOR_WORD:
	ld hl,(PSTART_DICT)	;0785

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
	;;  HL - address of start of current word
	;;
	;; On exit:
	;;  HL - address of start of next word
	;;  A  - corrupted
FIND_NEXT_WORD:
	push de			;0795 - Save DE

	;; Move address of word into DE
	ld d,h			;0796 
	ld e,l			;0797

	;; Retrieve word-name-length (masking off word-type modifiers
	ld a,(de)		;0798 
	and %00111111		;0799

	;;  Advance HL to word-length field
	add a,l			;079b
	ld l,a			;079c
	jr nc,FNW_CONT		;079d - Skip forward if no carry
	inc h			;079f

FNW_CONT:
	inc hl			;07a0 - One more increment to account
				;       for word-name-length field

	;; Retrieve contents of link field
	ld a,(hl)		;07a1
	inc hl			;07a2
	ld h,(hl)		;07a3
	ld l,a			;07a4

	;; ... and add to address of current word to get address of next
	;; work
	add hl,de		;07a5

	pop de			;07a6 - Retrieve DE
	
	ret			;07a7

	
TOKEN_TO_DICT:
	ld hl,(P_HERE)		;07a8
	call DUP		;07ab - Duplicate token length (on
				;       Parameter Stack)
	call STRING_TO_MEM	;07ae
	ex de,hl		;07b1 - DE = HERE
	rst 10h			;07b2 - Pop token length
	add hl,de		;07b3 - Update dictionary pointer to
	inc hl			;07b4   end of token
	ld (P_HERE),hl		;07b5 - Store it

	ret			;07b8

	;; Add byte to dictionary
	;; 
	;; On entry:
	;;   A - byte to add
	;;
	;; On exit:
	;; 
DICT_ADD_BYTE:
	push hl			;07b9

	ld hl,(P_HERE)		;07ba
	ld (hl),a			;07bd
	inc hl			;07be
	ld (P_HERE),hl		;07bf

	pop hl			;07c2

	ret			;07c3

	;;  Add 16-bit number to dictionary
	;; 
	;; On entry:
	;;   HL - word to add
	;;
	;; On exit:
	;; 
DICT_ADD_HL:
	push af			;07c4

	ld a,l			;07c5 - Retrieve low byte 
	call DICT_ADD_BYTE	;07c6 - Add to dictionary
	ld a,h			;07c9 - Retrieve high byte
	call DICT_ADD_BYTE	;07ca - Add to dictionary

	pop af			;07cd

	ret			;07ce

DICT_ADD_JP_HL:
	ld a,0xC3		;07cf
	jr DICT_ADD_CHAR_AND_HL	;07d1

DICT_ADD_CALL_HL:
	ld a,0xCD		;07d3 - Opcode for 'CALL'

DICT_ADD_CHAR_AND_HL:
	call DICT_ADD_BYTE	;07d5
	jp DICT_ADD_HL		;07d8

DICT_ADD_NUMBER:
	ld a,0x21		;07db - Opcode 'ld hl,NNNN'
	call DICT_ADD_CHAR_AND_HL		;07dd
	
	ld a,0xCF		;07e0 - Opcode 'rst 08h'
	jp DICT_ADD_BYTE	;07e2 - Add byte and return


DICT_ADD_RET:
	ld a,0xC9		;07e5 - Opcode 'ret'
	jp DICT_ADD_BYTE	;07e7


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
	jr nc,PT_WORD		;07ee - Jump forward if match found (HL
				;       points to word)

	;; Attempt to parse as a number
	ld hl,(PARSE_NUM_ROUT)
				;07f0 - Contains 0A36
	call jump_to_hl		;07f3

	jr nc,PT_DONE		;07f6 - Skip forward if done

	;; Deal with error
	call PRINT_NEW_LINE	;07f8
	call PRINT_STRING	;07fb

	ld a,_U			;07fe - Error - Undefined Word
l0800h:	call PRINT_ERR_MSG	;0800

PT_DONE:
	pop hl			;0803

	ret			;0804

	;; Process word (pointed to by HL)
PT_WORD:
	call UNSTACK_STRING	;0805 - Discard word from Character Stack
	bit 7,(hl)		;0808 - Check if immediate word ??? (`;`
				;       is an example of word with bit 7
				;       set)

	push af			;080a
	call FIND_PARAM_FIELD	;080b - Advance HL to start of parameter
				;       field
	pop af			;080e

	jr nz,PT_IMM_WORD	;080f - Jump forward, if immediate word

	call jump_to_hl		;0811 - Execute word

	call sub_0756h		;0814

	pop hl			;0817

	ret			;0818

PT_IMM_WORD:
	call jump_to_hl		;0819

	call DICT_ADD_RET	;081c

	ld hl,(UNKNOWN2)	;081f
	call jump_to_hl		;0822

	call INIT_DICT		;0825

	pop hl			;0828

	ret			;0829

	;; Advance to parameter field in word
	;;
	;; On entry:
	;;   HL - start of word
	;;
	;; On exit
	;;   HL - parameter field of word
FIND_PARAM_FIELD:
	ld a,(hl)		;082a - Retrieve word-name-length
	and 03fh		;082b   Isolate length
	bit 6,(hl)		;082d - Check for task word ???
	jr z,FPF_CONT		;082f   and skip forward if not
	add a,002h		;0831

FPF_CONT:
	add a,003h		;0833 - Word-name-length field plus link
				;       field
	add a,l			;0835 - Word-name-length
	ld l,a			;0836
	ret nc			;0837
	inc h			;0838
	ret			;0839

	
	ld a,(hl)		;083a
	bit 6,a			;083b
	scf			;083d
	ret z			;083e
	and 03fh		;083f
	add a,003h		;0841
	add a,l			;0843
	ld l,a			;0844
	jr nc,l0848h		;0845
	inc h			;0847
l0848h:	ld a,(hl)			;0848
	inc hl			;0849
	ld h,(hl)			;084a
	ld l,a			;084b

	ret			;084c


	;; Routine to handle user error (Part 1)
OUTPUT_ERR:
	push af			;084d - Save error code

	call PRINT_NEW_LINE	;084e
	call CDUP		;0851 - Duplicate string on character stack
	call PRINT_STRING	;0854

	pop af			;0857 - Retrieve error code
	
	jp PRINT_ERR_MSG		;0858
	

	;; Read word from keyboard (terminated by white-space) and check
	;; if it is in dictionary
	;; 
	;; On entry:
	;;
	;; On exit:
	;; - Word on Character Stack (length on Parameter Stack)
	;; - Carry Set/ Reset - no match/ match in dictionary (plus
	;;     throws error)
GET_NEW_WORD:
	call GET_WORD		;085b
	ret c			;085e - Exit if no match (Carry set) --
				;       i.e., new word

	;; Otherwise throw error
	ld a,_R			;085f - Redundant - word already in
				;       dictionary

	call OUTPUT_ERR		;0861

	or a			;0864 - Reset Carry -- failed

	ret			;0865

	;; GOT THIS FAR
INIT_NEW_WORD:
	ld hl,(P_HERE)		;0866 - Retrieve current entry point for
				;       dictionary

	push hl			;0869 - Save it

	call GET_NEW_WORD	;086a - Read word from keyboard
	call TOKEN_TO_DICT	;086d   and enter into dictionary

	pop hl			;0870 - Retrieve HERE (prior to token
				;       being added)

	call ADD_LINK_FIELD	;0871

	ld (PSTART_DICT),hl	;0874 - Update head of dictionary

	ret			;0877

	;; Handle user error
	;;
	;; On entry:
	;;   A - error code
	;;
	;; On exit:
	;;   
ERR_RESTART:
	call OUTPUT_ERR		;0878
	jp WARM_RESTART		;087b

DAW_ADD_CALL:
	call DICT_ADD_CALL_HL		;087e

	;; Add a sequence of predefined words and numbers to the
	;; dictionary
	;;
	;; On entry:

DICT_ADD_WORDS:
	call GET_WORD		;0881
	jr c,DAW_CHECK_NUM	;0884 - Jump forward if not word, to see
				;       if is number

	;; HL points to matched word in dictionary, character stack
	;; contains word (with length on parameter stack)
	call UNSTACK_STRING	;0886 - Discard word 
	bit 7,(hl)		;0889 - Check if immediate word

	push af			;088b
	call FIND_PARAM_FIELD	;088c - Advance to parameter field of
				;       word (in HL)
	pop af			;088f

	jr z,DAW_ADD_CALL	;0890 - If not immediate word, jump to
				;add `call HL` to dictionary, which
				;immediately preceeds this function, so
				;will effectively loop back to
				;DICT_ADD_WORDS

	;; Execute immediate word
	ld de,ERR_RESTART	;0892
	push de			;0895
	call jump_to_hl		;0896
	pop de			;0899

	jr DICT_ADD_WORDS	;089a and repeat

DAW_CHECK_NUM:	rst 10h		;089c - Retrieve token length

	xor a			;089d - Check if zero-length and 
	or l			;089e   loop if so
	jr z,DICT_ADD_WORDS	;089f

	rst 8			;08a1 - Push token length back onto
				;       Parameter Stack
	ld hl,(PARSE_NUM_ROUT)	;08a2 - Retrieve address of and call
	call jump_to_hl		;08a5   PARSE_NUM routine

	call DAW_ADD_NUM	;08a8

	jr DICT_ADD_WORDS	;08ab

DAW_ADD_NUM:
	bit 7,a			;08ad - Check for double-precision
				;       number ???

	;; Error, if not number
	ld a,_U			;08af - Error: Undefined word
	jp c,ERR_RESTART	;08b1

	jr z,l08bbh		;08b4 - Skip, if single-precision
	rst 10h			;08b6 - Double-precision???
	call l08bbh		;08b7
	rst 8			;08ba

l08bbh:	push hl			;08bb
	rst 10h			;08bc - Pop number from stack
	call DICT_ADD_NUMBER	;08bd
	pop hl			;08c0

	ret			;08c1

	
INIT_FLAGS:
	ld hl,FLAGS		;08c2 - Flags
	ld a,(hl)		;08c5
	and %01110000		;08c6 - Set System Execution mode;
				;       Disable print locks, ???,
				;       cursor-invertion off
	ld (hl),a		;08c8
	
	ld hl,FLAGS3		;08c9
	ld a,(hl)		;08cc

	if NTSC=1
	and %11010000		;08cd - Disable Print OK; ???
	else
	and %11000000		;08cd
	endif
	ld (hl),a		;08cf

	ld hl,FLAGS2		;08d0
	ld a,(hl)		;08d3
	and %11111100		;08d4 - Disable printer, ???
	ld (hl),a		;08d6

	ret			;08d7

	;; Kernel of context 0 loop
C0_KERNEL:
	call READ_TOKEN		;08d8 - Read and parse token from
				;       keyboard input buffer
	rst 10h			;08db - Pop string length from Parameter
				;       stack into HL

	;; Check if token length is zero and skip forward if so
	xor a			;08dc - Token length zero?
	cp l			;08dd
	jr z,EX_SKIP		;08de

	rst 8			;08e0 - Push token length back onto
				;       Parameter stack (restore
				;       previous state)
	
	call PROCESS_TOKEN	;08e1

EX_SKIP:
	ld hl,FLAGS3		;08e4
	bit 0,(hl)		;08e7
	res 0,(hl)		;08e9

	call nz,PRINT_OK	;08eb

	ret			;08ee

	;; Flush KIB
FLUSH_KIB:
	call READ_FROM_KIB	;08ef - retrieve next entry 
	ret c			;08f2 - return if done
	call PARSE_KIB_ENTRY	;08f3 - Process entry

	jr FLUSH_KIB		;08f6

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
WRITE_TO_KIB:
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
				;       write-offset, setting zero
				;       according

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

	;; Convert digit to ASCII character (with base)
	;; 
	;; On entry:
	;;   A - digit to convert
	;;
	;; On exit:
	;;   A - ASCII code of digit
BYTE_TO_ASCII:
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

	ld a,_S			;092f - Error: Parameter Stack underflow
	jp ERR_RESTART		;0931


RESET_TASKS:
	ld hl,FLAGS3		;0934 - Check if multitasking is enabled
	bit 6,(hl)		;0937   (as set by TON)
	ret nz			;0939 - Return if so

	;; Disable background task
	ld hl,NO_ACTION		;093a - Points to RET statement 
	ld (P_BACKTASK),hl	;093d

	call LOCK		;0940 - Lock multi-tasking

	jp SLOW			;0943 - Switch to slow mode (and return)

	;; 
	;; Continuation of RST 0x00 routine
	;;
RESTART_CONT:
	;;  Check if Shift key is pressed (user-requested cold restart)
	ld a,0x7F		;0946
l0948h:	in a,(0xFE)		;0948 - Read keyboard half-row for
				;       'Shift', 'z', ..., 'v' (also,
				;       turns on VSync)
	rrca			;094a - Rotate 'shift key' into carry
	jr nc,COLD_RESTART	;094b - Branch, if shift pressed (to
				;       force cold restart)

	;; Check if warm restart is possible
l094dh:	ld hl,F_WARM_RESTART	;094d - Check if warm restart is
	inc (hl)		;       possible (if address contains
				;       0xFF)
	jr z,WARM_RESTART	;0951 - Jump forward, if warm restart
				;       possible

	;; Continuation of cold-restart routine
COLD_RESTART:
	if MINSTREL4
	nop
	nop
	else
	out (0xFD),a		;0953 - Disable NMI Generator
	endif
	
	;; 
	;; Check memory configuation of machine (i.e., how much memory
	;; there is)
	;;
	if MINSTREL4=1
		;; Populate Ace character RAM
	ld de,0x2C00+0x20*8	; Start of character set in ROM
	ld hl,CHARS
	ld bc, 0x40*8
	ldir
	
	ld hl, 0x6000		; Set RAM size to be 16k ($4000--$7FFF)
				; -- we will add the lower RAM (in
				; $2000--$3FFF) later)

	ds 0x0964-$ 		; This is four bytes short of the end of
				; the original routine, to allow extra
				; space for zeroing memory

	else
	if MINSTREL3+MINSTREL4>0
	ld hl, 0x6000		; Set RAM size to be 16k ($4000--$7FFF)
				; -- we will add the lower RAM (in
				; $2000--$3FFF) later)
	
	ds 0x0964-$ 		; This is four bytes short of the end of
				; the original routine, to allow extra
				; space for zeroing memory

	else
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

	endif
	endif
	
	;; Save current registers
	exx			;0968

 	;; Zero RAM (up to 0x8000)
	if MINSTREL3+MINSTREL4>0 		
	ld hl,0x4000
	ld de,0x4001
	ld bc,0x3FFE
	xor a
	ld (hl),a
	ldir

	else
	ld hl,04000h		;0969
	ld de,0x0001		;097c
	xor a			;096f
l0970h:	ld (hl),a		;0970
	add hl,de
	
	jr nc,l0970h		;0972
	endif
	
	;; Initialise (second part of) system variables
	ld hl,DEFVARS		;0974
	ld de,VARS+0x20		;0977
	ld bc,0x0060		;097a
	ldir			;097d

	;; Padding (for Minstrel 3 version)
	if MINSTREL3+MINSTREL4>0
	ds 0x097f-$
	endif
	
	;; Restore registers
	exx			;097f

	ld (RAM_SIZE),hl	;0980 - Store RAM size

	;; Check how much memory was found and set location of display
	;; buffer accordingly

	;; Check if 48kB RAM or more
	ld a,0xBF		;0983
	cp h			;0985
	
	jr nc,l0991h		;0986 - Skip forward if <= 48kB RAM

	;; Otherwise move display buffer (from default location)
	ld hl,0xBD00		;0988 - Need to adjust memory
				;       configuration for 48k RAM.
	ld (P_DBUFFER),hl	;098b - By default, these variables
	ld (P_EDIT_SCREEN),hl	;098e   contain FD00, so only need to
				;       change if largest RAM
				;       configuration

	if MINSTREL4=1
l0991h:	nop
	nop
	nop
	else
l0991h:	call CHECK_FP_ROM	;0991 - check if ROM/ RAM in 2000--3FFF
	endif
	
	;; Display copyright message
	ld hl,COPYRIGHT_MSG	;0994
	call PRINT_STR_HL	;0997

	;; ============================================================
	;; Warm restart
	;; ============================================================
WARM_RESTART:
	ld sp,STACK0_BASE	; Reset Execution Stack
	
	ld hl,STACKC_BASE	; Reset character stack
	ld (P_STACKC),hl	;

	;; Initialise KIB (32-byte circular buffer at FB80--FBBF)
	ld hl,0x8080		;09a3 - Initial offset for both
	ld (KIB_R_OFFSET),hl	;09a6 - last-read and lastwrite offset are
				;       stored in consecutive bytes

l09a9h:	call RESET_TASKS	;09a9 - Disable multitasking and switch
				;       to SLOW mode (unless TON)
	call INIT_MSTACKS	;09ac - Initialise two machine stacks
	call INIT_DICT		;09af - Initialise some variables
	call INIT_FLAGS		;09b2 - Set/ reset some flags

	ld iy,(STACKP_BASE)	;09b5 - Reset parameter stack

	;; Set I register to point to page containing character set
	if MINSTREL4=1
	xor a
	ld (0x2700),a
	ei
	ds 0x09BF-$
	else
	ld a,CHARS>>8		;09b9 - High byte of character-map addr
	ld i,a			;09bb
	
	;; Initiate display production
	out (0xFE),a		;09bd - Enable NMI Generator, to start
				;       display handling (Note AF' (used
				;       to count NMI signals) has not
				;       been set). Initially,
				;       NEXT_DISP_ROUT is set to be
				;       0x0098
	endif
	
	;; Check if system variables are corrupted (assumed if value at
	;; FC40 is non-zero, as is zeroed as part of cold restart)
	;;
	;; NOTE: Would it be better to do this before starting display
	;; routine?
	ld a,(VARS)		;09bf - Check for zero at start of Sys
	or a			;09c2   Variables. Otherwise assume
				;       corrupt and Cold Restart is
	jr nz,COLD_RESTART	;09c3 - required

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
	ld hl,RAM_SIZE+1		;09d6 - High byte of RAM size

	;; Update RAM start
	ld a,020h		;09d9 - 32 256-byte pages / also high
				;       byte of RAM_START
	ld (RAM_START+1),a	;09db

	;; Increase RAMSIZE by 0x2000
	add a,(hl)		;09de - Increase count by 2k
	ld (hl),a		;09df

	ld hl,FLAGS3		;09e0
	set 4,(hl)		;09e3

	pop af			;09e5

	;; Check for known RAM content?
	xor 0xA5		;09e6
	jr z,l0a0ch		;09e8
	dec a			;09ea
	jr nz,l09f4h		;09eb

	call sub_0744h		;09ed

l09f0h:	ld hl,(02002h)		;09f0
	jp (hl)			;09f3

l09f4h:	ld hl,02000h		;09f4
	ld (hl),0a5h		;09f7 - Store marker byte

	;; Initialise dictionary
	ld hl,02010h		;09f9
	ld (P_HERE),hl		;09fc
	ld (UNKNOWN2),hl	;09ff

	;; Set up background task
	call sub_1b70h		;0a02
	ld hl,NUL		;0a05
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

	;; Context 1 main loop
C1_MAIN_LOOP:
	call CHECK_STACKP	;0a1b - Check for Parameter Stack underflow
	call READ_FROM_KIB	; Read next value from KIB (carry set if none)

	call nc,PARSE_KIB_ENTRY	;0a21 - Switch context, if new entry in
				;       keyboard buffer (and if FLAG(3)
				;       is set)

	call COMPILE_SCREEN	;0a24 - If necessary, compile screen

	;; Run background task
	ld hl,(P_BACKTASK)	;0a27
	call jump_to_hl		;0a2a

	;; Service auto counter
	ld hl,(AUTO_CNT)	;0a2d
	inc hl			;0a30
	ld (AUTO_CNT),hl	;0a31
	
	jr C1_MAIN_LOOP		;0a34

	;; Attempt to convert string on Character Stack to number on
	;; Parameter Stack (in current base)
	;;
	;; On exit:
	;;   C - reset okay/ set = not number
STR_TO_NUM:
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
	call S_TO_D		;0a57
	call DSTAR		;0a5a
	rst 8			;0a5d - Push HL onto param stack
	call S_TO_D		;0a5e
	call D_PLUS		;0a61

	djnz l0a4ch		;0a64

l0a66h:	call UNSTACK_DEHL		;0a66
	bit 0,c		;0a69
	call nz,NEG_DEHL		;0a6b
	call UNSTACK_STRING		;0a6e
	bit 7,c		;0a71
	ex de,hl			;0a73
	call nz,HL_TO_PSTACK		;0a74
	ex de,hl			;0a77
	rst 8			;0a78
l0a79h:	ld a,c			;0a79

	pop bc			;0a7a
	pop de			;0a7b
	pop hl			;0a7c

	ret			;0a7d

l0a7eh:	cp 0f7h		;0a7e
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

	;; Convert 32-bit number on Parameter Stack into counted string
	;; on Character Stack
D_TOS_TO_STRING:
	push hl			;0a96
	push de			;0a97
	push bc			;0a98

	xor a			;0a99
	ld c,001h		;0a9a - Set length of string

	call UNSTACK_DEHL	;0a9c - Retrieve number
	call ABS_SGN_DEHL	;0a9f - Separate sign and magnitude

	push af			;0aa2
DTTS_LOOP:	call STACK_DEHL		;0aa3
	ld hl,(BASE)		;0aa6
	rst 8			;0aa9 - Push base onto Parameter Stack
	call S_TO_D		;0aaa   and convert to double
	call D_SLASH		;0aad - Divide
	rst 10h			;0ab0 - Retrieve low bytes of remainder,
				;       which will be no bigger than
				;       255d.
	ld a,l			;0ab1 - Move into A
	rst 10h			;0ab2 - Retrieve high byte of remainder
				;       (effectively, discard)
	call BYTE_TO_ASCII	;0ab3 - Convert remainder (digit) to 
	rst 18h			;0ab6   ASCII and add to Character Stack
	inc c			;0ab7 - Increase string length
	call UNSTACK_DEHL	;0ab8 - Retrieve quotient
	call CHECK_DEHL_ZERO	;0abb - Check if zero (i.e., done)
	jr nz,DTTS_LOOP		;0abe - Repeat, if not
	
	pop af			;0ac0 - Check for negative
	rra			;0ac1
	jr nc,DTTS_CONT		;0ac2 - Skip forward, if positive
	ld a,_MINUS		;0ac4 - Add "-" to beginning of string
	rst 18h			;0ac6
	inc c			;0ac7 - Increase string length

DTTS_CONT:	ld a,_SPACE		;0ac8 - Pad with space
	rst 18h			;0aca

	ld h,000h		;0acb - Move string length to HL
	ld l,c			;0acd   and store to stack
	rst 8			;0ace

	pop bc			;0acf - Restore registers
	pop de			;0ad0
	pop hl			;0ad1

	ret			;0ad2

	;; Convert number to string
TOS_TO_STRING:
	push hl			;0ad3 - Save registers
	push de			;0ad4
	push bc			;0ad5

	ld c,001h		;0ad6 
	rst 10h			;0ad8 - Retrieve number

	call ABS_SGN_HL		;0ad9 - Separate sign (HL = ABS(HL))

	push af			;0adc - Save sign
l0addh:	rst 8			;0add - Save ABS(HL) onto Parameter Stack

	ld hl,(BASE)		;0ade
	rst 8			;0ae1 - Stack number base
	call DIV_TO_MDIV	;0ae2 - Divide ABS(HL) by BASE
	rst 10h			;0ae5 - Pop quotient
	ld a,l			;0ae6
	call BYTE_TO_ASCII	;0ae7

	rst 18h			;0aea - Push to character stack
	inc c			;0aeb - Increase character count
	rst 10h			;0aec - Pop remainder

	call CHECK_HL_ZERO	;0aed - Check if zero and repeat
	jr nz,l0addh		;0af0   if not

	pop af			;0af2
	
	jr z,DTTS_CONT		;0af3 - Jump if sign is positive
	ld a,_MINUS		;0af5
	rst 18h			;0af7 - Add "-"
	inc c			;0af8 - Increase string length

	jr DTTS_CONT		;0af9

	
	;; Start of built-in FORTH dictionary. (0AFBh)
	;; 
	;; Word check in
	;; https://github.com/monsonite/Z80_Forth/blob/master/h4th_source_2.asm

	;; Forth word M*
DICT_START:
	db 0x02, 0x4D, 0x2A
	db 0x64, 0x00

MSTAR:
	push hl			;0b00
	push de			;0b01
	xor a			;0b02
	rst 10h			;0b03
	call ABS_SGN_HL		;0b04
	ex de,hl			;0b07
	rst 10h			;0b08
	call ABS_SGN_HL		;0b09
	call MULT_DE_HL		;0b0c
	or a			;0b0f
	call po,NEG_DEHL		;0b10
	call STACK_DEHL		;0b13
	pop de			;0b16
	pop hl			;0b17

	ret			;0b18

	;; Separate signed integer in HL into sign and magnitude
	;;
	;; On entry:
	;;   HL - signed integer
	;;
	;; On exit
	;;   HL - ABS(HL)
	;;   Z - set for positive/ reset for negative
ABS_SGN_HL:
	bit 7,h			;0b19 - Check if negative
	ret z			;0b1b

	scf			;0b1c - Set bit 0 of A to be
	rla			;0b1d   sign (1=-ve; 0=+ve)

NEG_HL:	push de			;0b1e
	
	ld de,0x0000		;0b1f
	or a			;0b22
	ex de,hl			;0b23
	sbc hl,de		;0b24

	pop de			;0b26

	ret			;0b27

	;; Separate sign and magnitude of 32-bit number is DEHL.
	;;
	;; On entry:
	;;   DEHL - 32-bit, signed integer
	;;
	;; On exit
	;;   DEHL - ABS(DEHL) 
	;;   A(1)  - set if positive/ reset if negative.
ABS_SGN_DEHL:
	bit 7,d			;0b28
	ret z			;0b2a
	scf			;0b2b
	rla			;0b2c

NEG_DEHL:
	call NEG_HL		;0b2d
	ex de,hl		;0b30
	jr nc,ND_CONT		;0b31
	inc hl			;0b33

ND_CONT:
	call NEG_HL		;0b34
	ex de,hl		;0b37

	ret			;0b38

	;; Stack DE and HL ( -- HL DE ). Typically, this is used to
	;; stack a double-precision (32-bit) number onto the Parameter
	;; Stack.
	;; 
	;; On entry:
	;; - DEHL - 32-bit number to be stacked.
	;;
	;; On exit:
	;; 
STACK_DEHL:
	;; Push DE onto parameter stack
	ex de,hl		;0b39
	rst 8			;0b3a - Push onto Parameter Stack

	;; Push HL onto parameter stack
	ex de,hl		;0b3b
	rst 8			;0b3c - Push onto Parameter Stack
	
	ret			;0b3d

	;; Multiply HL by DE
MULT_DE_HL:
	push ix			;0b3e
l0b40h:	push bc			;0b40
	
	ld ix,0x0000		;0b41
	ld b,010h		;0b45
	or a			;0b47
	jr l0b4ch		;0b48
l0b4ah:	add ix,ix		;0b4a
l0b4ch:	adc hl,hl		;0b4c
	jr nc,l0b55h		;0b4e

	add ix,de		;0b50
	jr nc,l0b55h		;0b52
	inc hl			;0b54
l0b55h:	djnz l0b4ah		;0b55

	push ix			;0b57
	pop de			;0b59

	ex de,hl		;0b5a

	pop bc			;0b5b
	pop ix			;0b5c

	ret			;0b5e

	;; Forth word *
	;; 
	db 0x01, _ASTERISK
	dw 0x000F

	push hl			;0b63

	call MSTAR		;0b64

	rst 10h			;0b67

	push hl			;0b68

	rst 10h			;0b69

	pop hl			;0b6a

	rst 8			;0b6b

	pop hl			;0b6c

	ret			;0b6d

	;; Forth word M/
	db 0x02, _M, _SLASH
	dw 0x0057
MSLASH:
	push hl			;0b73
	push de			;0b74
	push bc			;0b75

	xor a			;0b76
	rst 10h			;0b77
	call ABS_SGN_HL		;0b78
	push hl			;0b7b
	pop bc			;0b7c
	call UNSTACK_DEHL		;0b7d
	call ABS_SGN_DEHL		;0b80
	call sub_0ba0h		;0b83
	or a			;0b86
	jr z,l0b94h		;0b87
	inc a			;0b89
	rra			;0b8a
	call nc,NEG_HL		;0b8b
	ex de,hl			;0b8e
	rra			;0b8f
	call c,NEG_HL		;0b90
	ex de,hl			;0b93
l0b94h:	call STACK_DEHL		;0b94

	pop bc			;0b97
	pop de			;0b98
	pop hl			;0b99

	ret			;0b9a

UNSTACK_DEHL:
	rst 10h			;0b9b - Pop 
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

	;; Forth word */MOD
	dec b			;0bc5
	ld hl,(04d2fh)		;0bc6
	ld c,a			;0bc9
	ld b,h			;0bca
	ld de,0d700h		;0bcb
	call MSTAR		;0bce
	rst 8			;0bd1
	call MSLASH		;0bd2
	ret			;0bd5
	ld (bc),a			;0bd6
	ld hl,(00a2fh)		;0bd7
	nop			;0bda
	call 00bcdh		;0bdb
	rst 10h			;0bde
	ret			;0bdf
	ld bc,NEG_DEHL+2		;0be0
	nop			;0be3
	push hl			;0be4
	call DIV_TO_MDIV		;0be5
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

	;; Perform 16-bit division, firstly promoting dividend to 32-bit
DIV_TO_MDIV:
	push hl			;0bf2

	rst 10h			;0bf3 - Pop divisor into HL

	call S_TO_D		;0bf4 - Convert dividend to double-precision
	
	rst 8			;0bf7 - Push divisor to Parameter Stack
	
	call MSLASH		;0bf8

	pop hl			;0bfb

	ret			;0bfc

	;; Forth word MOD
	db 0x03, _M, _O, _D
	dw 0x000F

	call DIV_TO_MDIV	;0c03 - Do division
	call SWAP		;0c06 - Switch to TOS = Remainder; 2OS =
				;       Quotient
	jp DROP			;0c09 - Drop remainder and return
	
	;; Forth word S->D
	;; Convert single-precision to double-precision number
	;; 
	db 0x04, _S, _MINUS, _GREATERTHAN, _D
	dw 0x0016

S_TO_D:	push de			;0c13
	push hl			;0c14
	
	rst 10h			;0c15 - Pop from stack into HL
				;(determines frequency)

	;; Check if negative
	ld a,h			;0c16 - Move upper byte to A
	rla			;0c17 - Bit 7 into carry
	ex de,hl		;0c18
	sbc hl,hl		;0c19 - 0000/ FFFF for positive/ negative
	ex de,hl		;0c1b
	call STACK_DEHL	;0c1c

	pop hl			;0c1f
	pop de			;0c20

	ret			;0c21

	
	;; Forth word ROT ( A B C -- B C A )
	;;
	db 0x03, _R, _O, _T
	dw 0x0012

	rst 10h			;0c28 - Retrieve TOS into DE
	ex de,hl		;0c29
	rst 10h			;0c2a - Retrieve 2OS into machine stack
	push hl			;0c2b
	rst 10h			;0c2c - Retrieve 3OS into HL
	
	ex (sp),hl		;0c2d - HL = 2OS, machine stack holds 3OS
	rst 8			;0c2e - Push 2OS onto Parameter Stack
	ex de,hl		;0c2f - HL = TOS
	rst 8			;0c30 - Push TOS onto stack
	pop hl			;0c31 - Retrieve 3OS
	rst 8			;0c32 - Push 3OS onto stack
	
	ret			;0c33 - Done

	
	;; Forth word MD/
	;; 
	db 0x03, _M, _D, _SLASH
	dw 0x0041
	
l0c3ah:	push ix			;0c3a
	push hl			;0c3c
	push de			;0c3d
	push bc			;0c3e
	push iy			;0c3f
	pop de			;0c41
	ld hl,HL_TO_PSTACK	;0c42
	add hl,de		;0c45
	push de			;0c46
	pop ix			;0c47
	ld bc,02008h		;0c49
l0c4ch:
	push bc			;0c4c
	push ix		;0c4d
	or a			;0c4f
l0c50h:
	rl (ix+004h)		;0c50
	inc ix			;0c54
	dec c			;0c56
	jr nz,l0c50h		;0c57
	pop ix			;0c59
	call COMPARE_DNUM	;0c5b
	jr c,l0c67h		;0c5e
	call HL_MINUS_DE		;0c60
	set 0,(ix+004h)		;0c63
l0c67h:
	pop bc			;0c67
	djnz l0c4ch		;0c68
	pop bc			;0c6a
	pop de			;0c6b
	rst 10h			;0c6c
	rst 10h			;0c6d
	call DSWAP		;0c6e
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
	ld bc,HL_TO_PSTACK		;0c84
	push iy		;0c87
	pop de			;0c89
	pop hl			;0c8a
	call INSERT_CHAR		;0c8b
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
	call nz,TIC		;0ca5
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
	;; 32-bit division of TOS by 2OS
	db 0x02, 0x44, 0x2F
	db 0x11, 0x00

D_SLASH:
	call UNSTACK_DEHL
	call sub_0cfbh		;0cc8
	call STACK_DEHL		;0ccb
	jp l0c3ah		;0cce

	;; Forth Word D*
	db 0x02, 0x44, 0x2A
	db 0x16, 0x00
DSTAR:
	push hl			;0cd6
	push de			;0cd7
	call MDSTAR		;0cd8
	call UNSTACK_DEHL		;0cdb
	call TWODROP		;0cde
	call STACK_DEHL	;0ce1
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
	call UNSTACK_DEHL		;0cfd
	call STACK_DEHL		;0d00
	push iy		;0d03
	call STACK_DEHL		;0d05
	pop hl			;0d08
	push hl			;0d09
	rl d		;0d0a
	pop de			;0d0c
	call HL_MINUS_DE	;0d0d
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
	ld bc,102dh		;0d21
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
	call NEG_HL		;0d3b
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

	;; Remove counted string from Character Stack
	;; 
	;; On entry
	;;   Character Stack contains string (with length on Parameter Stack)
	;; 
UNSTACK_STRING:
	push hl			;0d6b - Save HL
	
	rst 10h			;0d6c - Retrieve length of word into HL

	ld a,l			;0d6d - Isolate low 6 bits
	and 0x3F		;0d6e

	jr z,US_DONE		;0d70 - Skip forward if zero length
	ld l,a			;0d72

US_LOOP:
	rst 20h			;0d73 - Pop from character stack
	dec l			;0d74 - Repeat until character
	jr nz,US_LOOP		;0d75   stack is cleared

US_DONE:
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

	;; Forth word DUP (0x0D87)
	db 0x03, _D, _U, _P
	dw 0x0014

DUP:
	push hl			;0d8d
	rst 10h			;0d8e - Pop Parameter Stack to HL
	rst 8			;0d8f - Push HL to Parameter Stack 
	rst 8			;0d90 - Push HL to Parameter Stack
	pop hl			;0d91
	
	ret			;0d92

CHECK_DEHL_ZERO:
	ld a,l			;0d93
	or h			;0d94
	or e			;0d95
	or d			;0d96
	ret			;0d97


CHECK_HL_ZERO:
	ld a,l			;0d98
	or h			;0d99
	ret			;0d9a

	;; Forth word DSWAP (0x0D9B)
	db 0x05, _D, _S, _W, _A, _P
	dw 0x0021

DSWAP:	push hl			;0da3 - Save registers
	push de			;0da4
	push bc			;0da5

	;; Retrieve four bytes (two words) from stack
	rst 10h			;0da6 - Pop parameter stack to HL
	push hl			;0da7   and save it
	rst 10h			;0da8 - Pop parameter stack to HL
	push hl			;0da9   and save it
	rst 10h			;0daa - Pop parameter stack to HL
	push hl			;0dab   and save it
	rst 10h			;0dac - Pop parameter stack to HL

	pop bc			;0dad - 2OS in BCHL
	pop de			;0dae - TOS(L) in DE
	ex de,hl		;0daf - 2OS in BCDE
	rst 8			;0db0 - Push TOS(L) onto Parameter stack
	pop hl			;0db1 - Retrieve TOS(H)
	rst 8			;0db2 - Push TOS(H) onto Parameter stack
	ex de,hl		;0db3 - 2OS in BCHL
	rst 8			;0db4 - Push 2OS(L) onto Parameter stack
	push bc			;0db5 - Move 2OS(H) into HL
	pop hl			;0db6
	rst 8			;0db7 - Push 2OS(H) onto Parameter stack

	pop bc			;0db8 - Restore registers
	pop de			;0db9
	pop hl			;0dba

	ret			;0dbb

	;; Forth Word DO (0x0DBC)
	db 0x02+0x80, _D, _O
	dw 0x0021

	ld hl,0x0DCE		;0dc1
	call DICT_ADD_CALL_HL		;0dc4
	ld hl,(P_HERE)		;0dc7
	push hl			;0dca
	call DICT_ADD_WORDS		;0dcb
	pop de			;0dce
	ld bc,08000h		;0dcf
	call SWAP		;0dd2
	rst 10h			;0dd5
	add hl,bc		;0dd6
	push hl			;0dd7
	rst 10h			;0dd8
	add hl,bc		;0dd9
	push hl			;0dda
	ex de,hl		;0ddb
	jp (hl)			;0ddc

	;; Forth word +LOOP (0x0DDD)
	db 0x05+0x80, _PLUS, _L, _O, _O, _P
	dw 0x0028

l0de5h:	pop hl			;0de5
	pop hl			;0de6
	pop hl			;0de7
	ld hl,l0deeh		;0de8
	jp l0e12h		;0deb
l0deeh:	pop bc			;0dee
	pop de			;0def
	rst 10h			;0df0
	bit 7,h			;0df1
	jr nz,l0dffh		;0df3
	add hl,de		;0df5
	ex de,hl		;0df6
	pop hl			;0df7
	push hl			;0df8
	push de			;0df9
l0dfah:
	scf			;0dfa
	sbc hl,de		;0dfb
	push bc			;0dfd
	ret			;0dfe
l0dffh:
	add hl,de		;0dff
	pop de			;0e00
	push de			;0e01
	push hl			;0e02
	jr l0dfah		;0e03

	;; Forth word (LOOP) (0x0E05)
LOOP:	db 0x04+0x80, _L, _O, _O, _P
	dw 0x0027

	pop hl			;0e0c
	pop hl			;0e0d
	pop hl			;0e0e
	ld hl,l0e21h		;0e0f
l0e12h:	call DICT_ADD_CALL_HL		;0e12
	pop hl			;0e15
	ld a,0d2h		;0e16
	call DICT_ADD_CHAR_AND_HL		;0e18
	ld hl,0e1e1h		;0e1b
	jp DICT_ADD_HL		;0e1e
l0e21h:	pop de			;0e21
	pop bc			;0e22
	pop hl			;0e23
	push hl			;0e24
	inc bc			;0e25
	push bc			;0e26
	scf			;0e27
	sbc hl,bc		;0e28
	ex de,hl		;0e2a
l0e2bh:	jp (hl)			;0e2b


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

	;; Forth word SWAP
	;;
l0e3ah:	db 0x04, _S, _W, _A, _P
	dw 0x0013
	
SWAP:	push hl			;0e41 - Save registers
	push de			;0e42

	rst 10h			;0e43 - Retrieve TOS into DE
	ex de,hl		;0e44
	rst 10h			;0e45 - Retrieve 2OS into HL

	ex de,hl		;0e46 - Swap
	rst 8			;0e47 - Push TOS

	ex de,hl		;0e48 - Push 2OS
l0e49h:	rst 8			;0e49

l0e4ah:	pop de			;0e4a - Restore registers
	pop hl			;0e4b

	ret			;0e4c

	;; Forth word '
	db 0x01, _QUOTE
	dw 0x0009
	
	call TICK_WORD		;0e51 - Retrieve word and find parameter field
	rst 8			;0e54 - Push onto parameter stack

	ret			;0e55

	;; Forth word :
	;; 
	db 0x01, _COLON
	dw 0x000E

	call INIT_NEW_WORD	;0e5a
	ld hl,ERR_RESTART	;0e5d
	push hl			;0e60
	call DICT_ADD_WORDS	;0e61 - Assume returns via ';'

	;; Forth word ;
	;; 
	db 0x01+0x80, _SEMICOLON
	dw 0x000C

END_COLON_DEF:	call DICT_ADD_RET	;0e68

	;; Balance stack, without returning to DICT_ADD_WORDS
	pop hl			;0e6b
	pop hl			;0e6c
EXIT_CDEF:
	pop hl			;0e6d
	pop hl			;0e6e

	ret			;0e6f

	;; Forth word ;C
	;;
	;; Used to terminate a CODE definition
	db 0x02+0x80, _SEMICOLON, _C
	dw 0x0007

	jr EXIT_CDEF		;0e75

	;; Forth word .
	;; 
	db 0x01, _PERIOD
	dw 0x000A
	
l0e7bh:	call TOS_TO_STRING	;0e7b
	jp PRINT_STRING		;0e7e

	;; Forth word D.
	db 0x02, _D, _PERIOD
	dw 0x000B

	call D_TOS_TO_STRING		;0e86
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
	jp PRINT_NEW_LINE	;0ecc
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

	;; Forth word ! (store to memory)
	db 0x01, _EXCLAMATION
	dw 0x000C
	
	rst 10h			;0edf
	ex de,hl		;0ee0
	rst 10h			;0ee1
	ex de,hl		;0ee2
	ld (hl),e		;0ee3
	inc hl			;0ee4
	ld (hl),d		;0ee5
	ret			;0ee6
	
	ld bc,l0b40h		;0ee7
	nop			;0eea
sub_0eebh:
	rst 10h			;0eeb
	ld e,(hl)		;0eec
	inc hl			;0eed
	ld d,(hl)		;0eee
	ex de,hl		;0eef
	rst 8			;0ef0
	ret			;0ef1
	ld (bc),a			;0ef2
	ld d,e			;0ef3
	ld d,b			;0ef4
	ld a,(bc)			;0ef5
	nop			;0ef6
	ld a,020h		;0ef7
	jp PRINT_A		;0ef9

	;; Forth word <BUILDS
	;;
	db 0x07, _LESSTHAN, _B, _U, _I, _L, _D, _S
	dw 0x0017

	call INIT_NEW_WORD	;0f06
	call DICT_ADD_CALL_HL	;0f09
	ld hl,(P_HERE)		;0f0c
	dec hl			;0f0f
	dec hl			;0f10
	ex (sp),hl		;0f11
	jp (hl)			;0f12

	;; Forth word DOES>
	;; 
	db 0x80+0x05, _D, _O, _E, _S, _GREATERTHAN
	dw 0x001A

	ld hl, 0x0F27
	call DICT_ADD_CALL_HL	;0f1e
	ld hl,0xCFE1		;0f21 `rst 08h`, `pop hl`
	jp DICT_ADD_HL		;0f24
	
l0f27h:	pop de			;0f27
	pop hl			;0f28
	ld (hl),e		;0f29
l0f2ah:	inc hl			;0f2a
	ld (hl),d		;0f2b

	ret			;0f2c

	;; Forth word ALLOT
	;;
	db 0x05, _A, _L, _L, _O, _T
	dw 0x0012
	
DICT_ALLOT:
	ld hl,(P_HERE)		;0f35 - Retrieve HERE
	ex de,hl		;0f38   into DE
	rst 10h			;0f39 - Retrieve number of bytes to
				;       allocate from Parameter Stack
	add hl,de		;0f3a - Advance HERE and 
	ld (P_HERE),hl		;0f3b   save

	ret			;0f3e

	;; Forth word CODE
	;;
	;; Allows insertion of Z80 opcodes into a colon definition
	db 0x80 + 0x04, _C, _O, _D, _E
	dw 0x003A

	call HEX 		; Switch to HEX mode
	jr CO_LOOP		;0f49

C_ADD_WORD:
	call DICT_ADD_CALL_HL	;0f4b - Add word to dictionary
				;       definition
CO_LOOP:
	call GET_WORD		;0f4e - Input token (should be either a
				;       number (Opcode) or an immediate
				;       word)
	jr c,C_CHECKNUM		;0f51 - Move on if new word

	call UNSTACK_STRING	;0f53 - Remove from Character Stack

	bit 7,(hl)		;0f56 - Check if immediate word
	push af			;0f58
	call FIND_PARAM_FIELD	;0f59
	pop af			;0f5c
	jr z,C_ADD_WORD		;0f5d - Step back, if not immediate
				;       word, and add to dictionary
				;       definition
	ld de,ERR_RESTART	;0f5f - Otherwise execute word
	push de			;0f62
	call jump_to_hl		;0f63
	pop de			;0f66
	jr CO_LOOP		;0f67

C_CHECKNUM:
	call STR_TO_NUM		;0f69 - Convert token to number (leaving
				;       on Parameter Stack)

	;; Throw error if not a number
	ld a,_H			;0f6c - Error: token not found nor hex
	jp c,ERR_RESTART	;0f6e

	rst 10h			;0f71 - Pop value off Parameter Stack
	ld a,l			;0f72 - Add (byte) to dictionary
	call DICT_ADD_BYTE	;0f73
	
	jp CO_LOOP		;0f76

	
	;; Forth word HEX
	;;
	;; Set BASE to hexadecimal
	db 0x03, _H, _E, _X
	dw 0x000D

HEX:	ld hl,0x10		;0f7f
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

	;; Forth word 2VAR
	db 0x04, _2, _V, _A, _R
	dw 0x000E

	call VARIABLE		;0fbf
	rst 10h			;0fc2
	jp DICT_ADD_HL		;0fc3

	;; Forth word VARIABLE
	db 0x08, _V, _A, _R, _I, _A, _B, _L, _E
	dw 0x001B

VARIABLE:	
	call INIT_NEW_WORD	;0fd1
	
	ld hl,V_GET_ADDR	;0fd4
	call DICT_ADD_CALL_HL	;0fd7
	rst 10h			;0fda
	jp DICT_ADD_HL		;0fdb

V_GET_ADDR:
	pop hl			;0fde - Retrieve address in dictionary
				;       of variable's storage
	rst 8			;0fdf   and push onto parameter stack
	ret			;0fe0

	;; Forth word TASK
	db 0x04, _T, _A, _S, _K
	dw 0x0048

	call INIT_NEW_WORD	;0fe8 - Request name for task word and
				;       instantiate in dictionary

	;; Set word to be a task
	ld hl,(PSTART_DICT)	;0feb - Indicates a Task word
	set 6,(hl)		;0fee

	ld hl,(P_HERE)		;0ff0 - Skip forward five bytes
	ld de,0x0005		;0ff3
	add hl,de		;0ff6

	;; Populate task word header
	push hl			;0ff7
	call DICT_ADD_HL	;0ff8
	ld hl,SCHED_TASK		;0ffb
	call DICT_ADD_CALL_HL	;0ffe

	;; Advance dictionary pointer by 14 bytes
	ld hl,0x000d		;1001 - Push 0x0D onto stack
	rst 8			;1004
	call DICT_ALLOT		;1005

	;; Zero next 11 bytes
	ld bc,0x0B00		;1008
	pop hl			;100b - Retrieve start of
	push hl			;100c   parameter field

	;; Initialise task info (14 bytes initialised to zero)
l100dh:	ld (hl),c		;100d
	inc hl			;100e
	djnz l100dh		;100f

	ex de,hl		;1011 - Save address of next entry in
				;       parameter field to DE

	call TICK_WORD		;1012 - Retrieve action word for task
				;       (parameter field address) into HL

	;; Store address into task word
	ex de,hl		;1015
	ld (hl),e		;1016
	inc hl			;1017
	ld (hl),d		;1018

	;; Retrieve address of parameter field
	pop de			;1019 - Retrieve start of task Parameter Field

	;; Store address of task's Parameter Field in (PCUR_TASK_STRUCT)
	;; and update PCUR_TASK_STRUCT to point to
	ld hl,(PCUR_TASK_STRUCT)	;101a

	;; Temporarily disable NMI generator
	out (0fdh),a		;101d

	;; Store address of parameter field in previous task field
	ld (hl),e		;101f
	inc hl			;1020
	ld (hl),d		;1021

	;; Reenable NMI generator
	out (0feh),a		;1022

	;; Update location of current task parameter field to newly
	;; created task
	ex de,hl		;1024
	ld (PCUR_TASK_STRUCT),hl	;1025

	ret			;1028

	;; Forth word AUTO
	db 0x04, _A, _U, _T, _O
	dw 0x002B

	ld hl,FLAGS3		;1030
	res 7,(hl)		;1033

	ret			;1035

	;;  Service AUTO mode counter???
SERVICE_AUTO:
	;; Check if auto-mode enabled and exit if not
 	ld hl,FLAGS3		;1036
	bit 7,(hl)		;1039
	ret nz			;103b

	;; Retrieve 32-bit counter into DEHL
	ld de,0x0000		;103c
	ld hl,(AUTO_CNT)	;103f
	ex de,hl		;1042
	ld (AUTO_CNT),hl	;1043

	;; Check if high word is zero
	ex de,hl		;1046
	call CHECK_HL_ZERO	;1047
	jr z,FA_DISABLE_DISP	;104a - Disable display (as for FAST mode)

	;; Check if high word > 0100 
	ld de,0x0100		;104c
	sbc hl,de		;104f
	jr nc,SL_ENABLE_DISP	;1051 - Enable display (as for SLOW mode)

	ret			;1053

	;; Forth word FAST
	db 0x04, _F, _A, _S, _T
	dw 0x0013
	
FAST:	ld hl,FLAGS3		;105b
	set 7,(hl)		;105e
FA_DISABLE_DISP:
	ld hl,SKIP_DISPLAY	;1060
	ld (P_RUN_DISP),hl	;1063
	ret			;1066


	;; Forth word SLOW
	db 0x04, _S, _L, _O, _W
	dw 0x0013

SLOW:	ld hl,FLAGS3		;106e
	set 7,(hl)		;1071
SL_ENABLE_DISP:
	ld hl,RUN_DISPLAY	;1073
	ld (P_RUN_DISP),hl	;1076
	
	ret			;1079


	ld (bc),a		;107a
	ld b,h			;107b
	ld b,b			;107c
	ld hl,0dd00h		;107d
	push hl			;1080
	rst 10h			;1081
	push hl			;1082
	pop ix		;1083
	call sub_108eh		;1085
	call STACK_DEHL		;1088
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
	call UNSTACK_DEHL		;10a6
	call STORE_DEHL		;10a9
	pop ix		;10ac
	ret			;10ae

	;; Write 32-bit number to memory (Little Endian)
	;;
	;; On entry:
	;;   DEHL - number to be written
	;;   IX - address to write to
	;;
	;; On exit:
	;;   IX - memory location immedately after where number is written
STORE_DEHL:
	ld (ix+000h),l		;10af
	ld (ix+001h),h		;10b2
	ld (ix+002h),e		;10b5
	ld (ix+003h),d		;10b8

	ret			;10bb

	;; Forth word IF
	;;
	;; Conditionally expect sequence of instructions based on
	;; boolean test of TOS
	;; 
	db 0x80+0x02, _I, _F
	dw 0x001F

	;; Add check on TOS
	ld hl,IF_TEST		;10c1
	call DICT_ADD_CALL_HL	;10c4

	ld hl,(P_HERE)		;10c7 - Retrieve current dictionary
	inc hl			;10ca   entry point and advance one
				;       element (which will be the
				;       address parameter of conditional
				;       jump statement entered next)
	push hl			;10cb - Save it for use in future ELSE/
				;       THEN word
	
	ld a,0xCA		;10cc - Z80 opcode JP Z, NN
	call DICT_ADD_CHAR_AND_HL	;10ce

	;; Proceed to fill in body of IF block, until reach an ELSE or
	;; THEN statement
	call DICT_ADD_WORDS	;10d1 - Only returns, if error.

	jp ERR_RESTART		;10d4

IF_TEST:
	rst 10h			;10d7 - Retrieve parameter from TOS
	jp CHECK_HL_ZERO	;10d8 - Check if boolean

	
	;; Forth word ELSE
	;;
	;; Indicate alternative command sequence, if an IF condition
	;; fails
	;; 
	db 0x80+0x04, _E, _L, _S, _E
	dw 0x001E
	
	pop hl			;10e2 - Discard top two entries from
	pop hl			;10e3   stack (as exiting DICT_ADD_WORDS)

	pop hl			;10e4 - Retrieve return address (should
				;       be 0x10D4, in code field for IF
				;       statement)
	pop de			;10e5 - Retrieve pointer to branch
				;       address for JP Z,nn command,
				;       used if IF condition test fails
	call DICT_ADD_JP_HL	;10e6 - Jump command, initial with dummy
				;       address, which will be updated as part
				;       of THEN word later
	ld hl,(P_HERE)		;10e9 - Retrieve current location into DE,
	ex de,hl		;10ec   moving IF condition test address
				;       into HL
	ld (hl),e		;10ed - Replace with address of ELSE
	inc hl			;10ee   command body
	ld (hl),d		;10ef

	dec de			;10f0 - Set to call address of
	dec de			;10f1   dictionary entry above)

	push de			;10f2 - Save it

	call DICT_ADD_WORDS	;10f3

	jp ERR_RESTART		;10f6

	
	;; Forth word THEN
	;;
	;; Used to signify end of IF conditional code
	;; 
	db 0x80+0x04, _T, _H, _E, _N
	dw 0x0013

	pop hl			;1100 - Discard top two entries from stack
	pop hl			;1101
	pop hl			;1102 - Retrieve return address from
				;       DICT_ADD_WORDS - should be in IF or
				;       ELSE code field
	pop de			;1103 - Retrieve address field of branch
				;       instruction
	ld hl,(P_HERE)		;1104 - Retrieve current dictionary
	ex de,hl		;1107   into DE, swapping DE to HL
	
	ld (hl),e		;1108 - Replace branch address with
	inc hl			;1109   current location in dictionary
	ld (hl),d		;110a

	ret			;110b - Done

	
	;; Forth word REV
	;;
	db 0x03, _R, _E, _V
	dw 0x0015

	ld de,0x0006		;1112 - Set offset to mask character
	rst 10h			;1115 - Retrieve screen id from
				;       Parameter Stack (effectively
				;       pointer to screen info)
	
	add hl,de		;1116 - Point to mask character
	ld a,080h		;1117   and set REV mask

	xor (hl)		;1119 - Update mask character
 	ld (hl),a		;111a
	
	inc hl			;111b - Point to space character
	ld a,080h		;111c   and set REV mask
	
	xor (hl)		;111e - Update space character
	ld (hl),a		;111f
	
	ret			;1120


	;; Forth word LOCK
	db 0x04, _L, _O, _C, _K
	dw 0x0011

LOCK:	ld hl,(MTASK_TAIL)	;1128 - Set HL to point to tail of task
				;       list
	ld de,$000A		;       Skip forward to byte 10
	add hl,de		;112e
	set 7,(hl)		;112f - Set lock flag
	
	ret			;1131 - Done

	;; Forth word UNLOCK
	;;
	db 0x06, _U, _N, _L, _O, _C, _K
	dw 0x0013
	
	ld hl,(MTASK_TAIL)	;113b - Set HL to point to tail of task
				;       list
l113eh:	ld de,0x000A		;113e - Skip forward 10 bytes
	add hl,de		;1141
	res 7,(hl)		;1142 - Reset lock flag

	ret			;1144

	
	;; Forth word STOP
	;; 
	db 0x04, _S, _T, _O, _P
	dw 0x000E

	;; Set scheduler mode to '3'
	ld hl,0x0003		;114c

	;; Populate parameter stack with scheduler mode and (dummy)
	;; 32-bit time interval
	rst 8			;114f
	rst 8			;1150
	rst 8			;1151

	ret			;1152

	;; Forth word START
	;;
	db 0x05, _S, _T, _A, _R, _T
	dw 0x000F

	;; Set scheduler mode to be '4'
	ld hl,0x0004		;115b

	;; Populate parameter stack with scheduling mode and (dummy)
	;; 32-bit timer
	rst 8			;115e 
	rst 8			;115f
	rst 8			;1160
	
	ret			;1161

	;; Forth word IN
	;;
	;; Schedule task to execute after a time interval
	db 0x02, _I, _N
	dw 0x000A

	;; Push flag value '0' onto Parameter Stack
	ld hl,0x0000		;1167
	rst 8			;116a
	
	ret			;116b

	
	;; Forth word EVERY
	db 0x05, _E, _V, _E, _R, _Y
	dw 0x000D

	;; Push 0x0001 onto the Parameter Stack
	ld hl,0x0001		;1174
	rst 8			;1177
	
	ret			;1178

	;; Forth word AT
	db 0x02, _A, _T
	dw 0x000A

	;; Push 0x0002 onto the Parameter Stack
	ld hl,0x0002		;117e
	rst 8			;1181
	
	ret			;1182

	;; Forth word RUN
	db 0x03, _R, _U, _N
	dw 0x0072

	;; Put 5 on the stack three times (once for scheduler flag and
	;; twice for timing (which is ignored))
	ld hl,0x0005		;1189
	rst 8			;118c
	rst 8			;118d
	rst 8			;118e

	ret			;118f

	;; Run task ( FLAG TIME -- )
	;;
	;; Implementation of each task word, used to schedule the
	;; contained action word. This routine is accessed from code
	;; field of task, which contains `call SCHED_TASK`. Thus, on
	;; entry, the stack contains the address of the start of the
	;; task's parameter field.
	;;
	;; To function correctly, user must have inserted two items onto
	;; the Parameter Stack: TOS is a double-length time-interval and
	;; 2OS is a code (0--5) indicating the scheduling mode (see
	;; SCHED_MODE for details).
SCHED_TASK:
	ex (sp),ix		;1190 - Retrieve return address (which
				;       is the start of the task's
				;       parameter field) and save
				;       IX. Doing in this way, means we
				;       discard this address when
				;       RET-urning (using `pop ix`).
	call UNSTACK_DEHL	;1192 - Retrieve (double-length) time
				;       interval (frames) into DE (high
				;       byte) and HL (now byte)
	call NEG_DEHL		;1195 - Negate value (DEHL =
				;       0x10000-DEHL), which is used to
				;       set starting value for task
				;       counter, which then counts up to
				;       0x10000

	;; Save time value
	push de			;1198
	push hl			;1199

	;; Retrieve scheduler mode and turn into look-up address in
	;; SCHED_MODE table (that is SCHED_MODE+2*(mode-6).
	;;
	;; N.B. This seems an odd way to do this. One could set
	;; SCHED_MODE to point to start of lookup table and add two
	;; times mode to get offset. However, this way incorporates a
	;; check for out-of-range mode values.
	rst 10h			;119a - Retrieve mode value into HL

	ld de,0xFFFA		;119b - That is, -6
	add hl,de		;119e - Flag-6
	jr c,l11a9h		;119f   Abandon if Mode >=6
	add hl,hl		;11a1   (FLAG-6)*2

	;; Compute table offset for jump instruction
	ld de,SCHED_MODE+12	;11a2 - Note, this is end of taebl
	add hl,de		;11a5   HL=(SCHED_MODE+12)+2*(FLAG-6)
	jp JP_ADDR_HL		;11a6

l11a9h:	pop hl			;11a9 - Balance stack (remove
	pop hl			;11aa   time interval and
	pop ix			;11ab   restore IX)

	ret			;11ad

	;; Table of possible scheduling modes
SCHED_MODE:
	dw SCHEDULE_IN		; 0 = IN
	dw SCHEDULE_EVERY	; 1 = EVERY
	dw SCHEDULE_AT		; 2 = AT
	dw SCHEDULE_STOP	; 3 = STOP
	dw SCHEDULE_START	; 4 = START
	dw SCHEDULE_RUN		; 5 = RUN

	;; Write 32-bit timer value to a task's parameter field
	;;
	;; On entry:
	;;   DEHL - 32-bit timer value to write
	;;   IX - address (usually, in task's parameter field) to which
	;;        to store number
UPDATE_TIME_LIMIT:
	out (0xFD),a		;11ba - Disable NMI

	call STORE_DEHL		;11bc

	out (0feh),a		;11bf - Enable NMI

	ret			;11c1

	;; Scheduler for IN command. Also used for finishing up the EVERY
	;; scheduler
	;;
	;; On entry:
	;;   Stack contains 32-bit time interval
	;;   IX - address of parameter field of task
	;;
	;; On exit:
	;;   A, BC, DE, HL - corrupted
SCHEDULE_IN:
	inc ix			;11c2 - Advance to byte 2 of Task's
	inc ix			;11c4   parameter field (that is, current
				;       counter)

	;; (Entry point, from SCHEDULE_EVERY)
SE_CONT:
	pop hl			;11c6 - Retrieve time interval into DEHL
	pop de			;11c7

	call UPDATE_TIME_LIMIT	;11c8 - and write to time-counter field
				;       of task (Param_field(2,...,5)

	pop ix			;11cb - Restore IX (and, as started with
				;       `ex (sp),ix`, discard return
				;       address)

	ret			;11cd   Return to PROCESS_WORD

	;; Scheduler for EVERY command.
	;;
	;; On entry:
	;;   Stack contains 32-bit time value
	;;   IX - address of parameter field of task
	;;
	;; On exit:
	;;   A, BC, DE, HL - corrupted
SCHEDULE_EVERY:
	;; Point IX to time-limit field
	ld de,0x0006		;11ce
	add ix,de		;11d1

	jr SE_CONT		;11d3 - Write time value and done

	
	;; Scheduler for STOP command.
	;;
	;; On entry:
	;;   Stack contains 32-bit (dummy) time value
	;;   IX - address of parameter field of task
	;;
	;; On exit:
	;;   A, BC, DE, HL - corrupted
SCHEDULE_STOP:	
	pop hl			;11d5 - Discard time value
	pop hl			;11d6

	set 6,(ix+0x0A)		;11d7 - Set task status to Stopped

	pop ix			;11db - Restore IX
	
	ret			;11dd

	;; Scheduler for START command.
	;;
	;; On entry:
	;;   Stack contains 32-bit (dummy) time value
	;;   IX - address of parameter field of task
	;;
	;; On exit:
	;;   A, BC, DE, HL - corrupted
SCHEDULE_START:
	pop hl			;11de - Discard time value
	pop hl			;11df

	ld (ix+0x0A),000h	;11e0 - Clear task Stopped flag

	pop ix			;11e4 - Restore IX
	
	ret			;11e6

	;; Scheduler for RUN command.
	;;
	;; On entry:
	;;   Stack contains 32-bit (dummy) time value
	;;   IX - address of parameter field of task
	;;
	;; On exit:
	;;   A, BC, DE, HL - corrupted
	;; Implement 'Run' scheduler
SCHEDULE_RUN:	
	pop hl			;11e7 - Discard time value
	pop hl			;11e8

	bit 6,(ix+0x0A)		;11e9 - Check if task is stopped
	jr nz,SR_DONE		;11ed   Skip forward, if so

	inc (ix+0x0A)		;11ef - Increase priority

SR_DONE:
	pop ix			;11f2 - Restore IX

	ret			;11f4

	
	;; Forth word TT (returns (double-length) time interval in
	;; ticks)
	db 0x02, _T, _T
	dw 0x0008
	
	jp S_TO_D		;11fa

	;; Forth word TS ( secs -- frames )
	;;
	;; Compute number of frames in TOS seconds and push onto
	;; Parameter Stack
	db 0x02, _T, _S
	dw 0x000C

	;; Work out frames in one second
	if NTSC=1
	ld hl,FRAMES		;1202 - 60 frames in a second
	else
	ld hl,FRAMES		;1202 - 50 frames in a second
	endif

	rst 8			;1205 - Push onto Parameter Stack
	
	jp MSTAR		;1206 - Jump to `M*` to work out product
				;       as a double

	;; Forth word TM ( mins -- frames )
	;; 
	;; Compute number of frames in TOS minutes and push onto
	;; Parameter Stack
	db 0x02, _T, _M
	dw 0x000C

	;; Work out frames in one minutes
	if NTSC
	ld hl,60*FRAMES		;120e
	else
	ld hl,60*FRAMES		;120e
	endif

	rst 8			;1211 - Push onto parameter stack
	
	jp MSTAR		;1212 - Jump to `M*` to work out product
				;       as a double


	;; Forth word TH ( hours -- frames )
	;; 
	;; Compute number of frames in TOS hours and push onto
	;; Parameter Stack
	db 0x02, _T, _H
	dw 0x0014
	
	call S_TO_D		;121a

	if NTSC=1
	ld de,0x0003		;121d
	ld hl,0x4BC0		;1220
	else
	ld de,0x0002		;121d
	ld hl,0xBF20		;1220
	endif
	
l1223h:	call STACK_DEHL	;1223
	jp DSTAR		;1226

	;; Forth word TD (d days -- frames )
	;; 
	;; Compute number of frames in TOS days and push onto
	;; Parameter Stack.
	db 0x02, _T, _D
	dw 0x0010
	
l122eh:	call S_TO_D		;122e

	if NTSC=1
	ld de,0x004F		;1231
	ld hl,01A00h		;1234
	else
	ld de,0x0041		;1231
	ld hl,0eb00h		;1234
	endif

	jr l1223h		;1237


	;; Forth word TW ( weeks -- frames )
	;; 
	;; Compute number of frames in TOS weeks and push onto
	;; Parameter Stack.
	db 0x02, _T, _W
	dw 0x0010
	
l123eh:	call S_TO_D		;123e
	
	if NTSC=1
	ld de,0x0229		;1241
	ld hl,0xB600		;1244
	else
	ld de,0x01CD		;1241
	ld hl,0x6D00		;1244
	endif
	
	jr l1223h		;1247

	;; Forth word TY ( years -- frames )
	;; 
	;; Compute number of frames in TOS years and push onto
	;; Parameter Stack
	db 0x02, _T, _Y
	dw 0x0010

l124eh:	call S_TO_D		;124e - Convert TOS to double

	;; Stack number of ticks in a year (as a double)
	if NTSC=1
	ld de,0x70C8		;1251
	ld hl,0x1200		;1254
	else
	ld de,0x5DFC		;1251
	ld hl,0x0F00		;1254
	endif
	
	jr l1223h		;1257 - Stack HLDE and D*


	;; Forth word CASE
	;;
	;; Execute (n-1)th word, based on TOS
	;; 
	db 0x80+0x04, _C, _A, _S, _E
	dw 0x00A7

	ld hl,CASE_EXEC		;1260 
	call DICT_ADD_CALL_HL	;1263

	ld hl,(P_HERE)		;1266 - Retrieve pointer to current
	push hl			;1269   dictionary position and save it
	call DICT_ADD_HL	;126a - Store in dictionary: will be
				;       updated later to otherwise
				;       option.
	ld de,END_COLON_DEF	;126d - Code field for `;`, which will
				;       indicate end of CASE word list

CASE_LOOP:
	call TICK_WORD		;1270 - Wait for word
	call DE_CMP_HL		;1273 - Check if `;` for end of case
	jr z,CASE_DONE		;1276   options, and jump forward if is
	call DICT_ADD_HL	;1278 - Add code-field address of word to
	jr CASE_LOOP		;127b   dictionary

CASE_DONE:
	ld hl,(P_HERE)		;127d - Retrieve current dictionary
				;       location
	dec hl			;1280 - Step back to start of last
	dec hl			;1281   CASE word

	pop de			;1282 - Retrieve pointer to otherwise
	or a			;1283   offset
	sbc hl,de		;1284

	ex de,hl		;1286 - Store offset to final CASE word
	ld (hl),e		;1287
	inc hl			;1288
	ld (hl),d		;1289

	ret			;128a - Done

CASE_EXEC:
	pop hl			;128b - Retrieve return address (start
				;       of CASE list)
	ld e,(hl)		;128c - Store OTHERWISE offset into DE
	inc hl			;128d
	ld d,(hl)		;128e
	inc hl			;128f - Advance to first word
	push hl			;1290   and save
	add hl,de		;1291 - Work out end of CASE list
	ex (sp),hl		;1292   and set as default return address
				;       (retrieving pointer to start of
				;       CASE list into HL)
	push hl			;1293 - Save it
	rst 10h			;1294 - Retrieve TOS
	add hl,hl		;1295   and multiply by 2

	call DE_CMP_HL		;1296 - Check if beyond end of list
	jr nc,l12a0h		;1299 - Skip forward to user otherwise
				;       case, if so
	pop de			;129b - Retrieve word address
	add hl,de		;129c
	jp JP_ADDR_HL		;129d - and jump to it (return address
				;       is on stack)

l12a0h:	pop hl			;12a0 - Balance stack
	
	ret			;12a1

	
	;; Check if DE>HL
	;;
	;; On entry:
	;;   DE and HL populated appropriately
	;;
	;; On exit:
	;;   Z  - set, if DE=HL
	;;   CF - set, if DE>HL, reset otherwise
DE_CMP_HL:
	push hl			;12a2
	
	or a			;12a3 - Reset carry
	sbc hl,de		;12a4
	
	pop hl			;12a6
	
	ret			;12a7

	;; Process AT
SCHEDULE_AT:	ld hl,0x0000		;12a8
	add hl,sp		;12ab - HL points to top of stack (which
				;hold   time interval specified, when
				;       scheduling task)

l12ach:	ld de,TIME		;12ac - Point to current time
	or a			;12af - Reset carry
	call TIC		;12b0 - Work out system time - offset
	jp nc,SCHEDULE_IN	;12b3 - Move on, if okay

	call HL_MINUS_DE	;12b6 - Otherwise ???
	ld e,PER & 0x00FF	;12b9 - Set DE to point to PER
	call HL_MINUS_DE	;12bb
	jr l12ach		;12be

COMPARE_STACK_DNUM:
	call FIND_2DP_NUMS	;12c0 - DE and HL point to top and
				;       second double precision numbers
				;       on the stack

	;; Compare the 32-bit numbers pointed to by HL and DE
	;;
	;; On exit
	;;   Z - set for match, reset otherwise
	;;   BC - corrupted
COMPARE_DNUM:
	push hl			;12c3
	push de			;12c4

	ld bc,0x0004		;12c5
	add hl,bc		;12c8 - Advance to TIME + 04 (put in DE)
	ex de,hl		;12c9
	add hl,bc		;12ca - Advance to PER + 04
	ld b,c			;12cb

	;; Check if each digit match, one at a time, starting with final
	;; digit and working down
CD_LOOP:
	dec hl			;12cc
	dec de			;12cd
	ld a,(de)		;12ce
	cp (hl)			;12cf
	jr nz,CD_DONE		;12d0
	djnz CD_LOOP		;12d2

CD_DONE:
	pop de			;12d4
	pop hl			;12d5

	ret			;12d6

	;; Increment time counter
TIC:	push hl			;12d7 - Save registers
	push de			;12d8
	push bc			;12d9

	ld b,004h		;12da - Time held in four bytes
TIC_LOOP:
	ld a,(de)		;12dc
	adc a,(hl)		;12dd
	ld (hl),a		;12de
	inc hl			;12df
	inc de			;12e0
	djnz TIC_LOOP		;12e1

	pop bc			;12e3
	pop de			;12e4
	pop hl			;12e5

	ret			;12e6

	;; Subtract 32-bit number pointed to by DE from 32-bit number
	;; pointed to by HL and store result in location pointed to by
	;; HL
HL_MINUS_DE:
	push hl			;12e7
	push de			;12e8
	push bc			;12e9

	ld b,004h		;12ea - Four bytes
	ex de,hl		;12ec

HMD_LOOP:
	ld a,(de)		;12ed
	sbc a,(hl)		;12ee
	ld (de),a		;12ef
	inc hl			;12f0
	inc de			;12f1
	djnz HMD_LOOP		;12f2

	pop bc			;12f4
	pop de			;12f5
	pop hl			;12f6

	ret			;12f7

	;; Retrieve pointers to two double-precision numbers on
	;; Parameter Stack
FIND_2DP_NUMS:
	;; Copy pointer to parameter stack into DE
	push iy			;12f8
	pop de			;12fa

	;; Advance four bytes
	ld hl,0x0004		;12fb
	add hl,de		;12fe

	;; Done
	ret			;12ff


	;; Forth word D+
	;;
	;; 32-bit addition
	db 0x02, _D, _PLUS
	dw 0x0012

D_PLUS:	push hl			;1305 - Save registers
	push de			;1306

	call FIND_2DP_NUMS	;1307 - DE and HL point to top and
				;       second double precision numbers
				;       on the stack
	call TIC		;130a - Increment
	rst 10h			;130d
	rst 10h			;130e

	pop de			;130f - Restore registers
	pop hl			;1310

	ret			;1311

	;; Forth word D-
	;;
	;; 32-bit subtraction
	db 0x02, _D, _MINUS
	dw 0x0012

	push hl			;1317
	push de			;1318

	call FIND_2DP_NUMS	;1319 - DE and HL point to top and
				;       second double precision numbers
				;       on the stack
	call HL_MINUS_DE	;131c
	rst 10h			;131f
	rst 10h			;1320

	pop de			;1321
	pop hl			;1322

	ret			;1323

	;; GOT THIS FAR
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
	call ABS_SGN_HL		;1336
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
	call UNSTACK_DEHL		;1344
	call NEG_DEHL		;1347
	jp STACK_DEHL		;134a
	inc b			;134d
	ld b,h			;134e
	ld b,c			;134f
	ld b,d			;1350
	ld d,e			;1351
	djnz l1354h		;1352

l1354h:	call UNSTACK_DEHL	;1354
	call ABS_SGN_DEHL	;1357
	jp STACK_DEHL		;135a
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
	ld hl,0x000B		;136a
	rst 10h			;136d
	ex de,hl			;136e
	rst 10h			;136f
	ld a,l			;1370
	ld (de),a			;1371
	ret			;1372

	;; Forth word DROP
	db 0x04, _D, _R, _O, _P
	dw 0x0009

DROP:	rst 10h			;137a - Pop value from Parameter Stack

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
l1396h:	rst 10h			;1396

l1397h:	ld (hl),c			;1397
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
	jp INSERT_CHAR		;13ba

	;; Default task that is executed every 15 frames
DEF_TASK:
	call SERVICE_CLOCK	;13bd - Update clock
	call CHECK_KIB		;13c0 - Check for keyboard input
	call z,PRINT_FLASH	;13c3 - Flash cursor, if no key pressed
	
	call CHECK_MEM		;13c6 - Check for out-of-memory
	call SERVICE_AUTO	;13c9 - If AUTO mode, service counter

	;; Set to indicate warm restart is possible
	ld hl,F_WARM_RESTART	;13cc
	ld (hl),0xFF		;13cf

	ret			;13d1

	
SERVICE_CLOCK:
	push hl			;13d2
	push de			;13d3

	;; Service timer ???
	ld hl,UNKNOWN4		;13d4
	ld de,UNKNOWN5		;13d7
	call TIC		;13da

	;; Update DE and HL to point to clock (relies on them being in
	;; same page)
	ld l,TIME & 0x00FF	;13dd
	ld e,TIC_COUNTER & 0x00FF	;13df
	call TIC		;13e1

	;; Check if timer has reached limit (in PER)
	ld e,PER & 0x00FF	;13e4
	call COMPARE_DNUM	;13e6
	call nc,HL_MINUS_DE	;13e9 - If TIME>PER, TIME=TIME-PER

	pop de			;13ec
	pop hl			;13ed

	ret			;13ee

	;; Forth word SCREEN
	db 0x06, _S, _C, _R, _E, _E, _N
	dw 0x002C

	call INIT_NEW_WORD	;13f8 - Code adds new screen's parameter
	ld hl,V_GET_ADDR	;13fb   field address to Parameter stack
	call DICT_ADD_CALL_HL	;13fe

	ld hl,0x0000		;1401 - Initialise current location 
	push hl			;1404   to (0,0)

	call DICT_ADD_HL	;1405 - Retrieve upper limit into D
	rst 10h			;1408
	ld d,l			;1409 

	rst 10h			;140a - Retrieve left limit into E
	ld e,l			;140b
	rst 10h			;140c

	ld a,l			;140d - Retrieve lower limit into H
	rst 10h			;140e   and right limit into L
	ld h,a			;140f
	
	call DICT_ADD_HL	;1410 - Add coordinates to dictionary
	ex de,hl		;1413
	call DICT_ADD_HL	;1414

	pop hl			;1417 - Done

	if MINSTREL4=1
	jp SCREEN_CONT
	else
	jp DICT_ADD_HL		;1418 *** BUG *** Screen initialisation
				;     does not set the masking character
				;     and blanking character (assuming
				;     the memory locations will contain
				;     zero already)
	endif
	
	;; Forth word .C
	db 0x02, _PERIOD, _C
	dw 0x0011
	
	rst 10h			; Pop TOS into HL

	push hl			;1421 - Save it
	ex (sp),ix		;1422 - Move HL into IX

	rst 10h			;1424 - Pop TOS into HL
	ld a,l			;1425
	call SCR_PR_CHR		;1426

	pop ix			;1429 - Restore IX
	
	ret			;142b

	;; Forth word .W ( LEN SCR -- , STRING C-C -- )
	;;
	;; Print string to screen
	db 0x02, _PERIOD, _W
	dw 0x001A
	
	rst 10h			;1431 - Retrieve screen id from
	push hl			;1432   parameter stack and save it

	ex (sp),ix		;1433 - Move screen id into IX (saving
				;       IX to stack at same time)
	rst 10h			;1435 - Retrieve string length
	ld a,l			;1436
	and 03fh		;1437
	
	jr z,l1443h		;1439 - Jump forward if empty string
	ld l,a			;143b

l143ch:	rst 20h			;143c - Retrieve next character
	call SCR_PR_CHR		;143d   and print

	dec l			;1440 - Decrement character count
	jr nz,l143ch		;1441

l1443h:	pop ix			;1443 - Retrieve previous value of IX
	
	ret			;1445 - Done

	
	ld bc,00723h		;1446
	nop			;1449
	jp TOS_TO_STRING	;144a
	
	ld (bc),a		;144d
	ld b,h			;144e
	inc hl			;144f
	ex af,af'		;1450
	nop			;1451
	jp D_TOS_TO_STRING		;1452

	;; Forth word ED
	;;
	db 0x02, _E, _D
	dw 0x000A
	ld hl,SCR_INFO_ED	;145a - Retrieve pointer to editor
				;       screen info
	rst 8			;145d   Push to parameter stack
	
	ret			;145e

	;; Forth word CO
	db 0x02, _C, _O
	dw 0x000A

	ld hl,SCR_INFO_CO	;1464 - Retrieve pointer to console
				;       screen info
	rst 8			;1467 - Push to parameter stack

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
	ld de,0xFFFF		;1478 - Offset of -1
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

	;; Forth word "
	;;
	;; 
	db 0x01+0x80, _QUOTES
	dw 0x003B

	ld hl,QUOTE		;149b
l149eh:	call DICT_ADD_CALL_HL	;149e
	ld hl,(P_HERE)		;14a1
	ld a,0xFF		;14a4
l14a6h:	call DICT_ADD_BYTE	;14a6
	inc (hl)		;14a9
	or a			;14aa
	call sub_14c5h		;14ab
	cp 022h			;14ae
	jr nz,l14a6h		;14b0

	ret			;14b2

Q_FIND_STR:
	pop de			;14b3 - Retrieve second value from
	pop hl			;14b4   parameter stack, which is
	push de			;14b5   start of string, into HL

	;; Retrieve the length of the string
	ld e,(hl)		;14b6
	ld d,000h		;14b7
	ex de,hl		;14b9 

	;; Point HL to end of string
	add hl,de		;14ba
	inc hl			;14bb
	ex de,hl		;14bc - HL points to string; DE points
				;to next command

	ret			;14bd

	;; Runtime part of " word
QUOTE:	call Q_FIND_STR		;14be
	push de			;14c1 - Set return address

	jp PUSH_STRING		;14c2

                                                                                sub_14c5h:
	call SWITCH_TO_MSTACK1	;14c5
	and 07fh		;14c8
	call PRINT_A		;14ca
	cp 01fh		;14cd
	jr z,sub_14c5h		;14cf
	ret			;14d1

	;; Forth word ."
	db 0x02+0x80, _PERIOD, _QUOTES
	dw 0x0011
	
	ld hl, DOTQ
	jr 0x149E

	;; Execution kernel for ."
DOTQ:	call 0x14B3		;14dc
	push de			;14df
	jp PRINT_STR_HL		;14e0


	;; Forth word ABORT"
	;;
	;;
	db 0x06+0x80, _A, _B, _O, _R, _T, _QUOTES
	dw 0x0086
	
	ld hl,l14f1h		;14ec
	jr l149eh		;14ef

l14f1h:	call Q_FIND_STR		;14f1
	push de			;14f4
	push hl			;14f5
	rst 10h			;14f6
	ld a,l			;14f7
	or h			;14f8
	pop hl			;14f9
	ret z			;14fa
	call PRINT_STR_HL		;14fb
	jp WARM_RESTART		;14fe

	;; Write bit (based on carry)
WRITE_PULSE:
	push af			;1501

	;;  Generate one pulse of leader tone
	in a,(0feh)		;1502 - Set Cassette Out to low
	ld a,010h		;1504
TOL_LOOP:
	dec a			;1506
	jr nz,TOL_LOOP		;1507

	out (0ffh),a		;1509 - Set Cassette Out to high

	;; Wait
	ld a,04bh		;150b
TOL_WAIT:
	dec a			;150d
	jr nz,TOL_WAIT		;150e

	pop af			;1510

	ret			;1511

WRITE_GAP:
	push bc			;1512
	ld b,072h		;1513
l1515h:	djnz l1515h		;1515
	pop bc			;1517
	ret			;1518

WRITE_BYTE:
	push af			;1519
	push bc			;151a

	;; Write data bit
	ld b,00ah		;151b
	ld a,(hl)		;151d
	scf			;151e - Start with one
l151fh:	call WRITE_PULSE	;151f - Generate pulse
	call c,WRITE_PULSE	;1522   Double-length for bit=one
	call nc,WRITE_GAP	;1525 - Gap for zero
	add a,a			;1528 - Next bit to carry
	djnz l151fh		;1529 - Repeat (with end with zero?)

	pop bc			;152b
	pop af			;152c

	ret			;152d

WRITE_LEADER:
	push bc			;152e
	push hl			;152f

	ld hl,P_NULL		;1530
	ld b,0x7D		;1533
l1535h:	call WRITE_BYTE		;1535
	djnz l1535h		;1538

	pop hl			;153a
	pop bc			;153b
	ret			;153c

P_NULL:	db 0x00			;153d

FILE_ID:	db 0xA5		;153e - Identifier used for ZX-Forth
				;       files

WRITE_SCREEN:
	ld de,16*32		;153f - Size of editor screen
	push bc			;1542
	push hl			;1543
	ld bc,0xFFFF		;1544 - -1

l1547h:	call WRITE_BYTE		;1547
	inc hl			;154a - Advance to next byte

	;; Check if finished
	ex de,hl		;154b - Retrieve counter
	add hl,bc		;154c - Subtract 1
	ex de,hl		;154d - Store counter
	jr c,l1547h		;154e - Repeat if not done

	pop hl			;1550
	pop bc			;1551

	ret			;1552

WRITE_ID:
	push hl			;1553
	ld hl,FILE_ID		;1554
	call WRITE_BYTE		;1557
	pop hl			;155a
	ret			;155b

WRITE_SCR_NUM:
	push hl			;155c
	
	ld hl,SCREEN_NUM	;155d
	call WRITE_BYTE		;1560
	inc hl			;1563
	call WRITE_BYTE		;1564

	pop hl			;1567
	ret			;1568

	;; Forth word STORE ( SCREEN_NUM -- )
	;;
	;; Save screen to cassette.
	;;
	;; On entry:
	;;   TOS - screen number
	;;
	;; On exit
	db 0x05, _S, _T, _O, _R, _E
	dw 0x0097

	;; Check if Editor screen is visible, and return if not
	ld hl,FLAGS		;1571
	bit 6,(hl)		;1574
	ret z			;1576

	;; Retrieve screen number (used as filename) from TOS and store
	;; it for later
	rst 10h			;1577
	ld (SCREEN_NUM),hl	;1578

	;; Disable NMI generation (important to have accurate timing)
ST_CONT:	out (0xFD),a		;157b

	ld hl,(P_EDIT_SCREEN)	;157d - Retrieve address of editor screen
	call WRITE_LEADER	;1580 - Write to cassette port
	call WRITE_ID		;1583 - Write id code (used to confirm
				;       is a ZX-Forth file, when loading back
				;       into computer)
	call WRITE_SCR_NUM	;1586 - Write screen number
	call WRITE_SCREEN	;1589 - Write content of screen
	call WRITE_LEADER	;158c - Will be address of editor
				;       screen, as all of the WRITE_*
				;       routines preserve HL

	;; Enable NMI generation
	out (0feh),a		;158f

	ret			;1591

LOAD_HEADER:
	call LOAD_BYTE		;1592
	jr c,l159ch		;1595 - Skip forward if failed
	cp 0xA5			;1597 - Check for valid file id
	jr nz,LOAD_HEADER	;1599 - and repeat if invalid

	ret			;159b

	;; Check for Space
l159ch:	ld a,07fh		;159c
	in a,(0feh)		;159e
	rrca			;15a0
	ccf			;15a1
	jr nc,LOAD_HEADER	;15a2

	ret			;15a4

	;; Attempt to read a byte from tape
	;;
	;; On exit:
	;;  Carry reset, if succeeds (value in A)
	;;  Carry set, if failed
LOAD_BYTE:
	push de			;15a5
	push bc			;15a6

	ld e,000h		;15a7
l15a9h:	ld bc,0x0000		;15a9

	;; Check for tape signal
l15ach:	ld a,07fh		;15ac
	in a,(0feh)		;15ae
	bit 0,a			;15b0 - 
	jr z,l15b9h		;15b2
	rlca			;15b4 - Rotate signal into Carry
	jr c,l15bdh		;15b5 - Move on if signal
	djnz l15ach		;15b7 - Repeat, if not, unless timed out

l15b9h:	pop bc			;15b9
	pop de			;15ba

	scf			;15bb

	ret			;15bc

l15bdh:	ld b,03eh		;15bd - Pause
l15bfh:	djnz l15bfh		;15bf
	
	;; Read bit?
	ld b,025h		;15c1
l15c3h:	in a,(0feh)		;15c3 - Read tape signal
	rlca			;15c5 - Move into carry
	ld a,c			;15c6 - Add to current byte
	adc a,000h		;15c7
	ld c,a			;15c9
	djnz l15c3h		;15ca

	cp 004h			;15cc
	ccf			;15ce
	rl e			;15cf
	jr nc,l15a9h		;15d1

	pop bc			;15d3

	ld a,e			;15d4

	pop de			;15d5

	or a			;15d6 - Reset carry

	ret			;15d7

	;; Clear editor buffer
CLEAR_EDIT_BUFFER:
	ld b,000h		;15d8
	call ZERO_BYTE		;15da

	;; 
ZERO_BYTE:
	ld (hl),000h		;15dd
	inc hl			;15df
	djnz ZERO_BYTE		;15e0

	ret			;15e2

	;; Read DE bytes from tape into buffer starting at HL
LOAD_BLOCK:
	push de			;15e3
	push hl			;15e4
l15e5h:	call LOAD_BYTE		;15e5
	jr c,l15f2h		;15e8 - Exit, if failed
	ld (hl),a		;15ea
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
	call LOAD_BYTE		;15fa
	djnz sub_15fah		;15fd
	ret			;15ff

	;; Forth word LOAD
	;;
	;; Load screen from tape
	db 0x04, _L, _O, _A, _D
	dw 0x0054

	;; Check if editor screen is visible, and return if not
LOAD_SCREEN:
	ld hl,FLAGS		;1607
	bit 6,(hl)		;160a
	ret z			;160c

	;; Retrieve requested screen number and store
	rst 10h			;160d
	ld (SCREEN_NUM),hl	;160e

	;; Disable NMI (ensures consistent timing)
LS_CONT:	out (0fdh),a		;1611

	ld hl,(P_EDIT_SCREEN)	;1613 - Retrieve address of editor
				;       screen
	push hl			;1616
	call CLEAR_EDIT_BUFFER	;1617 - Clear editor window
	call LOAD_HEADER	;161a - Load valid header
	jr c,LS_FAIL		;161d - Jump forward if failed

	;; Retrieve screen number
	call LOAD_BYTE		;161f
	ld l,a			;1622
	call LOAD_BYTE		;1623
	ld h,a			;1626
	or l			;1627
	jr nz,l162dh		;1628 - Jump forward if screen number is
				;       non-zero
	ld (SCREEN_NUM),hl	;162a

l162dh:	ex de,hl		;162d - Move screen number (from file)
				;       to DE
	ld hl,(SCREEN_NUM)	;162e - Retrieve requested file number

	ld a,l			;1631 - Check is non-zero
	or h			;1632
	jr nz,l163bh		;1633

	ex de,hl		;1635 - DE = requested file number; HL =
				;       read file number
	ld (SCREEN_NUM),hl	;1636
	jr l163dh		;1639
l163bh:	sbc hl,de		;163b - Check if screen numbers match

l163dh:	pop hl			;163d - Retrieve start of editor window
	jr nz,l164ch		;163e - Repeat, if no screen-number match 

	ld de,16*32		;1640 - Set DE to size of editor window in memory

	call LOAD_BLOCK		;1643
	call LOAD_COMPILE	;1646

l1649h:	out (0feh),a		;1649 - Enable NMI

	ret			;164b - Done

l164ch:	call sub_15f5h		;164c
	jr LS_CONT		;164f - Load screen

LS_FAIL:
	pop hl			;1651
	jr l1649h		;1652

	;; Forth word -->
	;;
	;; Load next screen
	db 0x03, _MINUS, _MINUS, _GREATERTHAN
	dw 0x000B
	
	call INC_SCREEN_NUM	;165a - Increment screen number

	jr LS_CONT		;165d - Load screen

	;; Forth word <--
	;;
	;; Store next screen
	db 0x03, _LESSTHAN, _MINUS, _MINUS
	dw 0x0025

	call INC_SCREEN_NUM	;1665 - Increment screen number

	jp ST_CONT		;1668 - Store screen


INC_SCREEN_NUM:
	ld hl,(SCREEN_NUM)	;166b
	ld a,l			;166e
	or h			;166f
	ret z			;1670
	inc hl			;1671
	ld (SCREEN_NUM),hl	;1672

	ret			;1675

	;; Compute DE > HL (assuming signed 16-bit numbers)
	;;
	;; On entry:
	;;   DE and HL contain two numbers to compare
	;;
	;; On exit:
	;;   CF set if DE> HL; CF clear otherwise
	;;   A corrupted
DE_GT_HL:
	push hl			;1676
	push de			;1677

	;; Check if two numbers have same sign
	ld a,h			;1678
	xor d			;1679
	bit 7,a			;167a
	jr z,l167fh		;167c - Jump forward if so, otherwise swap

	ex de,hl		;167e
l167fh:	sbc hl,de		;167f

	pop de			;1681
	pop hl			;1682

	ret			;1683

	;; Forth word CPL
	;;
	;; Compile editor screen
	db 0x03, _C, _P, _L
	dw 0x0014

	jr l1692h		;168a

	;; Check for auto-compile
LOAD_COMPILE:
	ld hl,FLAGS3		;168c - Check if auto-compile enabled
	bit 2,(hl)		;168f
	ret z			;1691 - Return, if not

l1692h:	ld hl,FLAGS3		;1692 - Set compile-screen flag
	set 1,(hl)		;1695

	ret			;1697

	;; Forth word CON
	;;
	;; Turn on auto-compile
	db 0x03, _C, _O, _N
	dw 0x000C

	ld hl,FLAGS3		;169e
	set 2,(hl)		;16a1
	ret			;16a3

	;; Forth word COFF
	;; 
	;; Disable auto-compile mode
	db 0x04, _C, _O, _F, _F
	dw 0x000D

	ld hl,FLAGS3		;16ab
	res 2,(hl)		;16ae
	ret			;16b0

	;; Forth word >
	db 0x01, _GREATERTHAN
	dw 0x0011
	rst 10h			;16b5 - Retrieve TOS into DE
	ex de,hl		;16b6
	rst 10h			;16b7 - Retrieve 2OS into HL
	ex de,hl		;16b8 - HL = TOS; DE=20S
GT_COMP:
	call DE_GT_HL		;16b9 - Carry clear if TOS > 20S
GT_CONT_1:
	ccf			;16bc - Carry set if TOS > 2OS
GT_CONT_2:
	sbc hl,hl		;16bd - HL = 0x0000 if TOS > 20S
	inc hl			;16bf - HL = 0x0001 if TOS > 2OS

	rst 8			;16c0 - Push answer onto Parameter Stack
	
	ret			;16c1

	;; Forth word <
	db 0x01, _LESSTHAN
	dw 0x0009
	
	rst 10h			;16c6 - Retrieve TOS to DE
	ex de,hl		;16c7
	rst 10h			;16c8 - Retrieve 2OS to HL
	jr GT_COMP		;16c9 - Continue as for ">" but with
				;numbers reversed

	;; Forth word =
	;; 
	db 0x01, _EQUALS
	dw 0x000F

	rst 10h			;16cf - Retrieve TOS to DE
	ex de,hl		;16d0
	rst 10h			;16d1 - Retrieve 2OS to HL 
	call DE_GT_HL		;16d2 - Check if TOS > 2OS
l16d5h:	jr z,GT_CONT_2		;16d5 - Jump if TOS <= 20S  
	or a			;16d7 - Reset carry
	jr GT_CONT_1		;16d8

	;; Forth word C=
	;;
	db 0x02, _C, _EQUALS
	dw 0x000B
	
	rst 10h			;16df
	ld a,l			;16e0
	rst 10h			;16e1
	cp l			;16e2
	jr l16d5h		;16e3


	db 0x01, _QUESTIONMARK
	dw 0x000A

	call sub_0eebh		;16e9

	jp l0e7bh		;16ec

	
	;; Forth word +! ( INC ADDR -- )
	;;
	;; Add 2OS to address pointed to by TOS
	db 0x02, _PLUS, _EXCLAMATION
	dw 0x0012

	rst 10h			;16f4 - Retrieve address

	push hl			;16f5 - Save it

	ld e,(hl)		;16f6 - Retrieve (HL) into DE
	inc hl			;16f7
	ld d,(hl)		;16f8
	
	rst 10h			;16f9 - Retrieve addition into HL
	add hl,de		;16fa

	ex de,hl		;16fb - Put sum into DE

	pop hl			;16fc - Retrieve address

	ld (hl),e		;16fd - Store new value
	inc hl			;16fe
	ld (hl),d		;16ff

	ret			;1700

	;; Forth word +- 
	;;
	;; Swap sign - apply sign of TOS to 2OS
	db 0x02, _PLUS, _MINUS
	dw 0x0014
	
	rst 10h			;1706 - Retrieve TOS and check if negative (NZ)
	bit 7,h			;1707

	push af			;1709 - Save flag

	rst 10h			;170a - Retrieve 2OS
	call ABS_SGN_HL		;170b - Drop sign

	pop af			;170e

	if FIXBUG=1
	call nz,NEG_HL		; If TOS was negative, negate number

	rst 8			;1713 - Push result onto Parameter Stack

	ret			;1714

	nop			; Padding
	
	else			; *** BUG: result not put on stack, if positive!

	ret z			;170f - BUG fix - `nop`
	call NEG_HL		;1710 - BUG fix - `call nz, ...`

	rst 8			;1713 - Push result onto Parameter Stack

	ret			;1714

	endif

	;; Forth word ,
	db 0x01, _COMMA
	dw 0x0008
	
	rst 10h			;1719 - Pop word from parameter stack into HL
	jp DICT_ADD_HL		;171a

	;; Forth word C,
	;;
	;; Add byte to dictionary
	db 0x02, _C, _COMMA
	dw 0x000A

	;; Retrieve LSB of TOS
	rst 10h			;1722
	ld a,l			;1723
	jp DICT_ADD_BYTE	;1724

	;; Forth word SP@
	;;
	;; Retrieve current parameter-stack value (as prior to running
	;; this command)
	db 0x03, _S, _P, _AT
	dw 0x000B

	;; Copy parameter-stack parameter into HL and push onto
	;; parameter stack (which, of course, changes the
	;; parameter-stack pointer!)
	push iy			;172d
	pop hl			;172f
	
	rst 8			;1730 - Push HL to stack

	ret			;1731

	;; Forth word VLIST
	db 0x05, _V, _L, _I, _S, _T
	dw 0x001F
	
VLIST:	ld hl,(PSTART_DICT)	;173a
	ld de,0x0008		;173d - Routine to push HL onto
				;       Parameter Stack

VL_NEXT_WORD:
	call PUSH_STRING	;1740 - Add next word name to
					;character stack
	call PAD_WORD		;1743
	call FIND_NEXT_WORD	;1746
	call PRINT_STRING	;1749

	;; Check if end of dictionary (that is, if address returned by
	;; FIND_NEXT_WORD has MSB=0) and repeat if not
	xor a			;174c
	cp h			;174d
	jr nz,VL_NEXT_WORD	;174e

	ret			;1750

	;; Forth word W>
	db 0x02, _W, _GREATERTHAN
	dw 0x0020
	
	rst 10h			;1756
	ex de,hl		;1757

PAD_WORD:
	push hl			;1758
	push de			;1759

	rst 10h			;175a - Pop length of current word from
				;       stack into HL

	call DE_GT_HL		;175b - Check if length < 8 ???

	jr nc,l176eh		;175e - Skip forward, if not
	ex de,hl		;1760 - Set HL to be 8
	rst 8			;1761 - Push new length onto Parameter stack

	;; Padd to eight characters
	ccf			;1762 
	sbc hl,de		;1763
	ld a,_SPACE		;1765
l1767h:	rst 18h			;1767 -Push A onto Character Stack
	dec l			;1768
	jr nz,l1767h		;1769

l176bh:	pop de			;176b
	pop hl			;176c

	ret			;176d

	
l176eh:
	rst 8			;176e
	jr l176bh		;176f

	;; Forth word IMM
	;;
	db 0x80+0x03, _I, _M, _M
	dw 0x000C

l1777h:	ld hl,(UNKNOWN2)	;1777
	set 7,(hl)		;177a
	
	ret			;177c

	
	;; Forth word INTEGER
	db 0x07, 0x49, 0x4E, 0x54, 0x45, 0x47, 0x45, 0x52
	db 0x38, 0x00

l1787h:	call INIT_NEW_WORD		;1787
	ld hl,l1797h		;178a
	call DICT_ADD_CALL_HL		;178d
	rst 10h			;1790
	call DICT_ADD_HL	;1791
	jp l1777h		;1794

	
	;; Jump here from definition of BASE (and other things)
l1797h:	ld hl,FLAGS3		;1797 - FLAGs
	bit 5,(hl)		;179a
	res 5,(hl)		;179c
	pop hl			;179e
	jr z,l17abh		;179f
	ld a,0d7h		;17a1
	call DICT_ADD_BYTE		;17a3
	ld a,022h		;17a6
	jp DICT_ADD_CHAR_AND_HL		;17a8

l17abh:	ld a,02ah		;17ab
	call DICT_ADD_CHAR_AND_HL		;17ad
	ld a,0cfh		;17b0
	jp DICT_ADD_BYTE		;17b2


	add a,d			;17b5
	ld d,h			;17b6
	ld c,a			;17b7
	dec bc			;17b8
	nop			;17b9
	ld hl,FLAGS3		;17ba
	set 5,(hl)		;17bd
	ret			;17bf

	;; Forth word BEGIN
	db 0x80+0x05, _B, _E, _G, _I, _N
	dw 0x000F
	
	ld hl,(P_HERE)		;17c8 - Retrieve dictionary entry point
	push hl			;17cb   and save it
	
	call DICT_ADD_WORDS	;17cc - Will not return here

	;; Forth word AGAIN
	db 0x80+0x05, _A, _G, _A, _I, _N
	dw 0x000F

	;; Balance stack by dropping:
	;; - return address to DICT_ADD_WORDS
	;; - previous value of DE
	;; - return address from BEGIN
	pop hl			;17d7
	pop hl			;17d8
	pop hl			;17d9

	;; Retrieve dictionary instruction immediately after BEGIN and
	;; add jump to dictionary
	pop hl			;17da
	jp DICT_ADD_JP_HL	;17db - Returns to command entry

	
	;; Forth word WHILE
	db 0x80+0x05, _W, _H, _I, _L, _E
	dw 0x001E
	
	;; Balance stack by dropping:
	;; - previous value of DE
	;; Retrieve return address to DICT_ADD_WORDS
	pop de			;17e6

	;; Discard previous value of DE
	pop hl			;17e7

	;; Discard return address from BEGIN, as is meaningless
	pop hl			;17e8

	;; Add check that TOS is zero to dictionary
	ld hl,TOS_ZERO_CHECK	;17e9
	call DICT_ADD_CALL_HL	;17ec

	;; Add conditional jump to next word (will be updated as part of
	;; RETURN)
	ld hl,(P_HERE)		;17ef - Retrieve current dictionary
				;       entry point

	if FIXBUG=1		; WHILE condition test on ZX-FORTH is
				; non-standard
	ld a,0xCA		; Z80 op code for JP Z, NN
	else
	ld a,0xC2		;17f2 - Z80 op code for JP NZ,NN
	endif

	call DICT_ADD_CHAR_AND_HL	;17f4
	
	inc hl			;17f7
	push hl			;17f8
	push hl			;17f9
	ex de,hl		;17fa - Move return address to
				;       DICT_ADD_WORDS to HL ready to
				;       return to it
	jp (hl)			;17fb

	;; Forth word REPEAT
	db 0x80+0x06, _R, _E, _P, _E, _A, _T
	dw 0x0018
	
l1805h:	pop hl			;1805 - Discard return address
	pop hl			;1806 - Discard ???
	pop de			;1807 - Retrieve location of WHILE
				;       jump-to address
	pop hl			;1808 - Retrieve address of BEGIN
l1809h:	call DICT_ADD_JP_HL	;1809 - Add JP BEGIN to dictionary
	ld hl,(P_HERE)		;180c - Retrieve current dictionary location
	ex de,hl		;180f - Move to DE, moving address of
				;       WHILE jump to HL
	ld (hl),e		;1810 - Update jump-to address to be next
	inc hl			;1811   dictionary location 
	ld (hl),d		;1812
	
	ret			;1813 - Return to wrap-up of immediate word

	;; Forth word UNTIL
	db 0x80+0x05, _U, _N, _T, _I, _L
	dw 0x001D
	
	pop hl			;181c - Discard return address
	pop hl			;181d - Discard previous value of DE
	pop hl			;181e - Discard return address for
				;       BEGIN, as is meaningless
	ld hl,TOS_ZERO_CHECK	;181f - Add a check for TOS=0 to
	call DICT_ADD_CALL_HL	;1822   dictionary
	pop hl			;1825 - Ratrieve address of BEGIN in
				;       dictionary
	ld a,0xCA		;1826 - Set up conditional loop using
	jp DICT_ADD_CHAR_AND_HL	;1828   Z80 op code for JP Z, NN

TOS_ZERO_CHECK:
	rst 10h			;182b - Retrieve TOS

	;; Check if HL zero
	xor a			;182c
	cp l			;182d
	ret nz			;182e
	cp h			;182f
	
	ret			;1830

	;; Forth word KEY
	db 0x03, _K, _E, _Y
	dw 0x0019
	
l1837h:	ld a,_FLASH		;1837
	call PRINT_A		;1839
	call SWITCH_TO_MSTACK1	;183c

	and 07fh		;183f
	cp _FLASH		;1841
	jr z,l1837h		;1843

	ld h,000h		;1845
	ld l,a			;1847
	rst 8			;1848
	
	ret			;1849

	;; Forth word (
	db 0x80+0x01, _LEFTPARENTH
	dw 0x000E
l184eh:	call sub_14c5h		;184e
	and 07fh		;1851
	cp 029h		;1853
	jr nz,l184eh		;1855
	ret			;1857
	ld (bc),a			;1858
	jr nc,$+62		;1859
l185bh:	dec bc			;185b
	nop			;185c
	rst 10h			;185d
	rlc h			;185e
	jp GT_CONT_1		;1860
	ld (bc),a		;1863
	jr nc,$+64		;1864
	dec bc			;1866
	nop			;1867
	rst 10h			;1868
	rlc h		;1869

l186bh:	jp GT_CONT_2		;186b
	
	;; Forth word 0=
	;;
	;; Check if TOS is 0
	db 0x02, _0, _EQUALS
	dw 0x000D

	rst 10h			;1873 - Retrieve TOS

	xor a			;1874
	cp l			;1875 - Check LSB
	jr c,l186bh		;1876
	cp h			;1878 - Check MSB
	jr l186bh		;1879

	;; Forth word 2*
	db 0x02, _2, _ASTERISK
	dw 0x0009
	
	rst 10h			;1880 - Retrieve TOS
	add hl,hl		;1881 - Multiply by 2
	rst 8			;1882 - Push to Parameter Stack

	ret			;1883

	;; Forth word 2/
	;;
	;; Divide TOS by 2
	db 0x02, _2, _SLASH
	dw 0x000C

	rst 10h			;1889 - Retrieve TOS into HL

	sra h			;188a - Divide by 2
	rr l			;188c

	rst 8			;188e - Return to Parameter Stack
	
	ret			;188f

	;; Forth word W!
	;;
	;; Store character string to address on TOS
	;; 
	db 0x02, _W, _EXCLAMATION
	dw 0x0009
	
	rst 10h			;1895 - Retrieve destination address
	
	jp STRING_TO_MEM	;1896


	;; Forth word EOFF
	;; 
	db 0x04, _E, _O, _F, _F
	dw 0x0016
	
	ld hl,FLAGS		;18a0
	res 6,(hl)		;18a3

	ld l,0xB9		;18a5 - Console Screen status
	ld a,(hl)		;18a7 - Retrieve top-row value
	ld (hl),0x00		;18a8 - Set top-row to be zero
	ld l,0xB7		;18aa - Point to row count
	add a,(hl)		;18ac - Expand to fill space previously
	ld (hl),a		;18ad   occupied by Editor Screen

EO_DONE:
	ret			;18ae - Done

	;; FORTH word BASE 
	db 0x84, _B, _A, _S, _E
	dw 0x000E
	
	ld hl,BASE		;18b6
	push hl			;18b9
	jp l1797h		;18ba


	;; Forth word DECIMAL
	;; 
	db 0x07, _D, _E, _C, _I, _M, _A, _L
	dw 0x0010
	
DECIMAL:
	ld hl,BASE		;18c7
	ld (hl),0x0A		;18ca

	ret			;18cc

	;; Forth word BACK
	;;
	db 0x04, _B, _A, _C, _K
	dw 0x000E

	;; Update background text to point to code field of selected
	;; word
	call TICK_WORD		;18d4
	ld (P_BACKTASK),hl	;18d7

	ret			;18da

	;; Forth word MEM
	db 0x03, _M, _E, _M	; MEM
	dw 0x003D

	call FREE_MEM		;18e1
	
	rst 8			;18e4

	ret			;18e5

FREE_MEM:
	if MINSTREL3+MINSTREL4>0
	ld de,(P_HERE)		; Retrieve end of dictionary
	push iy			; Retrieve head (bottom) of Parameter
	pop hl			; Stack

	and a			; Free space is difference
	sbc hl,de

	ds 0x18F9-$
	
	else
	ld hl,(P_HERE)		;18e6
	ex de,hl		;18e9
	ld hl,(RAM_SIZE)	;18ea
	push iy			;18ed
	or a			;18ef
	sbc hl,de		;18f0
	pop de			;18f2
	add hl,de		;18f3
	ex de,hl		;18f4
	ld hl,(RAM_START)	;18f5
	add hl,de		;18f8

	endif
	
	ret			;18f9

CHECK_MEM:
	ld hl,FLAGS3		;18fa
	bit 3,(hl)		;18fd
	ret nz			;18ff
	call FREE_MEM		;1900
	ld de,0xFFE0		;1903 - That is, -32
	add hl,de		;1906 - HL = HL-32

	if MINSTREL3+MINSTREL4>0
	ret c
	
	ds 0x190E-$

	else
	ex de,hl		;1907
 	ld hl,(RAM_SIZE)	;1908
	sbc hl,de		;190b

	ret nc			;190d

	endif
	
	;; Out of memory
	ld hl,FLAGS3		;190e
	set 3,(hl)		;1911
	ld a,_M			;1913 - Error: Out of memory
	jp PRINT_ERR_MSG	;1915
	

	;; Forth word FENCE
	db 0x05, _F, _E, _N, _C, _E
	dw 0x001E

	call GET_WORD_AND_UNSTACK		;1920
	ld (FENCE),hl		;1923

	ret			;1926

GET_WORD_AND_UNSTACK:
	call GET_WORD		;1927 - Carry set, if no match in dictionary
	push af			;192a
	call UNSTACK_STRING	;192b
	pop af			;192e
	ret nc			;192f - Return, if match
	
	pop hl			;1930 - Dicard return address
	ld a,_U			;1931 - Error: Undefined word
	jp PRINT_ERR_MSG	;1933

	;; Forth word FORGET
	db 0x06, _F, _O, _R, _G, _E, _T
	dw 0x004B
	
	call GET_WORD_AND_UNSTACK	;193f

	;; Check if will cross fence boundary
	ex de,hl		;1942
	ld hl,(FENCE)		;1943
	call DE_CMP_HL		;1946
	ld a,_F			;1949 - Error: Fenced word
	jp nc,PRINT_ERR_MSG	;194b
	
	;; Check if will delete background task
	ld hl,(P_BACKTASK)	;194e
	call DE_CMP_HL		;1951
	jr c,l195ch		;1954 - Jump forward, if not
	ld hl,NUL		;1956 - Otherwise, disable background
	ld (P_BACKTASK),hl	;1959   task

	;; Check if need to remove items from task list
l195ch:	ld hl,(PMTASK_LIST_HD)	;195c
l195fh:	push hl			;195f
	pop bc			;1960
	ld (PCUR_TASK_STRUCT),hl
				;1961
	ld a,(hl)		;1964
	inc hl			;1965
	ld h,(hl)		;1966
	ld l,a			;1967
	or h			;1968
	jr z,l1974h		;1969
	call DE_CMP_HL		;196b
	jr c,l195fh		;196e
	xor a			;1970
	ld (bc),a		;1971
	inc bc			;1972
	ld (bc),a		;1973

l1974h:	ex de,hl		;1974
	ld (UNKNOWN2),hl	;1975

	call FIND_NEXT_WORD	;1978
	ld (START_DICT_DEF),hl ;197b

	jp INIT_DICT		;197e

	
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

	;; Forth word WARM
	db 0x04, _W, _A, _R, _M
	dw 0x000A
	
	jp WARM_RESTART		;19a1

	db 0x04, _C, _O, _L, _D
	dw 0x000A
	
	jp COLD_RESTART		;19ab

	;; Forth word NUL
	db 0x03, _N, _U, _L
	dw 0x0007
	
NUL:	ret			;19b4

	;; Forth word HERE (19b5h)
	db 0x04, _H, _E, _R, _E
	dw 0x000C

HERE:	ld hl,(P_HERE)		;19bc
	rst 8			;19bf
	ret			;19c0

	;; Forth Word H
H:	db 0x01, _H
	dw 0x0009

	ld hl,P_HERE		;19c5 - Retrieve address of system variable
	rst 8			;19c8 - Push onto Parameter Stack
	
	ret			;19c9

	;; Forth Word T
	db 0x01, _T
	dw 0x0009
	
T:	ld hl,PSTART_DICT	;19ce - Retrieve address
	rst 8			;19d1 - Push onto Parameter Stack

	ret			;19d2

	
	ld (bc),a		;19d3
	ld d,e			;19d4
	ld b,b			;19d5
	ex af,af'			;19d6
	nop			;19d7
	jp READ_TOKEN		;19d8


	ld (bc),a		;19db
	ld a,023h		;19dc
	dec bc			;19de
	nop			;19df
	call STR_TO_NUM		;19e0
	jp GT_CONT_2		;19e3


	db 0x03, _PERIOD, _C, _O
	dw 0x0012

	rst 10h			;19ec
	xor a			;19ed
	or l			;19ee
	ret z			;19ef

l19f0h:	rst 20h			;19f0
	call WRITE_TO_KIB	;19f1
	dec l			;19f4
	jr nz,l19f0h		;19f5
	ret			;19f7

	;; Forth word .CPU
	db 0x04, _PERIOD, _C, _P, _U
	dw 0x000D

	ld hl,CPU_MSG		;19ff
	jp PRINT_STR_HL		;1a02

	;; Forth word U*
	;;
	db 0x02, _U, _ASTERISK
	dw 0x000E
	
	call UNSTACK_DEHL	;1a0a
	call MULT_DE_HL		;1a0d
	jp STACK_DEHL		;1a10

	;; Forth word U/MOD
	;;
	;; Divide 32-bit number (2OS) by 16-bit number (TOS), returning
	;; quotient (2OS) and remainder (TOS)
	db 0x05, _U, _SLASH, _M, _O, _D
	dw 0x0014
	
U_SLASH_MOD:
	rst 10h			;1a1b
	push hl			;1a1c
	pop bc			;1a1d
	call UNSTACK_DEHL	;1a1e
	call sub_0ba0h		;1a21
	jp STACK_DEHL		;1a24

	;; Forth word UMOD
	;;
	;; Divide 32-bit number (2OS) by 16-bit number (TOS), returning
	;; quotient (2OS) and remainder (TOS)
	db 0x04, _U, _M, _O, _D
	dw 0x000F
	
	call U_SLASH_MOD	;1a2e
	call SWAP		;1a31 - Swap remainder and quotient

	rst 10h			;1a34 - Discard quotient

	ret			;1a35

	
	;; Forth word U<
	;;
	db 0x02, _U, _LESSTHAN
	dw 0x000E

	call UNSTACK_DEHL	;1a3b
	or a			;1a3e
	sbc hl,de		;1a3f
	jp GT_CONT_2		;1a41

	;; Forth word MIN
	db 0x03, _M, _I, _N
	dw 0x0011
	
	call UNSTACK_DEHL	;1a4a	
	call DE_GT_HL		;1a4d

MIN_CONT:
	jr c,l1a53h		;1a50 - Swap if DE > HL
	ex de,hl		;1a52

l1a53h:	rst 8			;1a53 - Push HL onto stack
	
	ret			;1a54


	;; Forth word MAX
	db 0x03, _M, _A, _X	
	dw 0x000F
	
	call UNSTACK_DEHL	;1a5b - Retrieve TOS and 20S into HL and DE
	call DE_GT_HL		;1a5e - Carry indicates if DE > HL
	ccf			;1a61 - Complement, so flag indicates HL > DE
	jr MIN_CONT		;1a62 - Continue as for MIN

	;; Forth word PAD
	db 0x03, _P, _A, _D  	; 1a64
	dw 0x000B

	ld hl,PAD		;1a6a
	rst 8			;1a6d - UPUSH

	ret			;1a6e


	;; Forth word U# (1a6f)
	;;
	;; Convert (unsigned) integer to string
	db 0x02, _U, _HASH
	dw 0x0010

	;; Convert TOS to string (treating as unsigned integer)
U_HASH:	rst 10h			;1a74 - Retrieve TOS into HL
	ld de,0x0000		;1a75
	ex de,hl		;1a78

	;; Turn into 32-bit integer, by prepending with 0x0000
	rst 8			;1a79 - Push 0x0000 onto stack
	ex de,hl		;1a7a - Retrieve number
	rst 8			;1a7b   and restore to stack
	jp D_TOS_TO_STRING	;1a7c - Convert to string


	;; Forth word U.
	;; 
	db 0x02, _U, _PERIOD
	dw 0x000B

	call U_HASH		;1a84
	jp PRINT_STRING		;1a87


	;; Forth word D=
	;; 
	;; Compare two double-precision numbers from Parameter Stack
	db 0x02, _D, _EQUALS
	dw 0x0013
	
	call COMPARE_STACK_DNUM	;1a8f
	rst 10h			;1a92
	rst 10h			;1a93
	rst 10h			;1a94
	rst 10h			;1a95
	
l1a96h:	scf			;1a96
	jr nz,l1a9ah		;1a97
	ccf			;1a99

l1a9ah:	jp GT_CONT_2		;1a9a


	;; Forth word D0=
	;;
	;; Check if double-precision number on TOS is zero
	db 0x03, _D, _0, _EQUALS
	dw 0x000E
	
	call UNSTACK_DEHL	;1aa3
	call CHECK_DEHL_ZERO	;1aa6

	jr l1a96h		;1aa9

	;; Forth word DMIN
	;; 
	db 0x04, _D, _M, _I, _N
	dw 0x0010
	
l1ab2h:	call COMPARE_STACK_DNUM	;1ab2 - Compare numbers
	call nc,DSWAP		;1ab5   Swap, if bigger number is TOS

	rst 10h			;1ab8 - Discard double on TOS (smaller of 
	rst 10h			;1ab9   two numbers)

	ret			;1aba

	;; Forth word DMAX
	;; 
	db 0x04, _D, _M, _A, _X
	dw 0x0010
	
l1ac2h:	call COMPARE_STACK_DNUM	;1ac2 - Compare numbers
	call c,DSWAP		;1ac5 - Swap, if smaller is TOS

	rst 10h			;1ac8 - Discard double on TOS (larger of
	rst 10h			;1ac9   two numbers)

	ret			;1aca

	;; Forth word W=
	;;
	;; Compare (counted) strings pointed to by TOS and 2OS
	;; 
	db 0x02, _W, _EQUALS
	dw 0x000A
	
	call MATCH_STACK_STRINGS ;1ad0
	jr l1a96h		;1ad3


	;; Forth word S=
	;; 
	db 0x02, _S, _EQUALS
	dw 0x000B
	
	rst 10h			;1ada
	call MATCH_STRING	;1adb

SEQ_CONT:	
	jr l1a96h		;1ade

	;; Forth word TIME
	;; 
	db 0x04, _T, _I, _M, _E
	dw 0x000C

	ld hl,TIME		;1ae7
	rst 8			;1aea - Push HL onto Parameter Stack

	ret			;1aeb

	;; Forth word PER
	db 0x03, _P, _E, _R
	dw 0x000B
	
	ld hl,PER		;1af2 - Retrieve PER and push onto
	rst 8			;1af5   Parameter Stack

	ret			;1af6

	;; Forth word +ORG ( OFFSET -- ADDR )
	;;
	db 0x04, _PLUS, _O, _R, _G
	dw 0x000E
	
	ld de,VARS_BASE		;1afe

	rst 10h			;1b01 - Retrieve offset
	add hl,de		;1b02 - Add to start of sys vars
	rst 8			;1b03

	ret			;1b04

	;; Forth word BLK
	;;
	;; Integer variable containing the address from which/ to which
	;; tape operations move data
	db 0x80+0x03, _B, _L, _K
	dw 0x000D
	
	ld hl,P_EDIT_SCREEN	;1b0b
l1b0eh:	push hl			;1b0e

	jp l1797h		;1b0f


	;; Forth work PAGE
	;;
	;; Most recent page number stored to/ loaded from cassette
	db 0x80+0x04, _P, _A, _G, _E
	dw 0x000C
	
	ld hl,SCREEN_NUM	;1b19
	jr l1b0eh		;1b1c

	;; Forth word [_]
	;;
	;; Suppress execution of the subsequent immediate word
	db 0x80+0x03, _LEFTSQBRACKET, _UNDERSCORE, _RIGHTSQBRACKET
	dw 0x000C
	
	call TICK_WORD		;1b24
	jp DICT_ADD_CALL_HL	;1b27


	
	inc bc			;1b2a
	ld c,b			;1b2b
	ld a,041h		;1b2c
	djnz l1b30h		;1b2e
l1b30h:
	rst 10h			;1b30
	ld a,l			;1b31
	call BYTE_TO_ASCII	;1b32
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
	ld a,(bc)		;1b4f
	nop			;1b50
	jp INIT_NEW_WORD	;1b51
	inc b			;1b54
	ld c,c			;1b55
	ld c,(hl)			;1b56
	ld c,c			;1b57
	ld d,h			;1b58
	inc de			;1b59
	nop			;1b5a
	call TICK_WORD		;1b5b
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
	ld hl,(P_HERE)		;1b70
	ld (02006h),hl		;1b73
	ld hl,(PSTART_DICT)	;1b76
	ld (02004h),hl		;1b79
	ld hl,(MTASK_TAIL)		;1b7c
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

	call TICK_WORD		;1bb2
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
PRINT_DRV:
	push hl			;1bcf
	rst 10h			;1bd0
	ld a,l			;1bd1
	ld hl,PAD		;1bd2
	and 07fh		;1bd5
	cp 060h			;1bd7
	jr c,l1bddh		;1bd9
	add a,0e0h		;1bdb
l1bddh:	cp 020h			;1bdd
	jr nc,l1bf6h		;1bdf
	cp 008h			;1be1
	jr nz,l1bech		;1be3
	xor a			;1be5
	or (hl)			;1be6
	jr z,l1beah		;1be7
	dec (hl)		;1be9
l1beah:	pop hl			;1bea

	ret			;1beb

l1bech:	cp 00dh		;1bec
	jr z,l1beah		;1bee
	cp 00ah		;1bf0
	ld a,02eh		;1bf2
	jr z,l1bfdh		;1bf4
l1bf6h:	call STR_ADD_CHR	;1bf6
	bit 5,(hl)		;1bf9
	jr z,l1c02h		;1bfb
l1bfdh:	call sub_1c09h		;1bfd
	ld (hl),000h		;1c00

l1c02h:	pop hl			;1c02
	ret			;1c03


	ld (bc),a		;1c04
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
l1c1fh:	ld (hl),020h		;1c1f
	inc hl			;1c21
	dec e			;1c22
	jr nz,l1c1fh		;1c23
	pop hl			;1c25
l1c26h:	inc hl			;1c26
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
l1c3ah:	push de			;1c3a
	push hl			;1c3b
	ld b,020h		;1c3c
l1c3eh:	push hl			;1c3e
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

	;; Forth word .CN
	db 0x03, _PERIOD, _C, _N
	dw 0x0009
	
	jp PRINT_STRING		;1c96

	;; Forth word D>
	;; Double-precision comparison
	db 0x02, _D, _GREATERTHAN
	dw 0x000A

D_GREATERTHAN:	
	call DSWAP		;1c9e
	jr D_LESSTHAN		;1ca1

	;; Forth word D<
	;; Double-precision comparison
	db 0x02, _D, _LESSTHAN
	dw $10000-$1CA3		; Length => next word starts at 0x0000,
				; which is a special case, indicating
				; the end of the dictionary
	
D_LESSTHAN:
	call COMPARE_STACK_DNUM	;1ca8 - Carry and Zero indicate result

	;; Balance stack, removing two double-precision numbers
	rst 10h			;1cab
	rst 10h			;1cac
	rst 10h			;1cad
	rst 10h			;1cae

	ccf			;1caf - Complement carry

	jp GT_CONT_2		;1cb0 - Push resulm onto stack and done

	
	if MINSTREL3+MINSTREL4>0
RESTART_NEW:
	im 1			; System relies on Interrupt Mode 1
				; (interrupt-service routine at 0x0038)
	ld iy,  PSTACK_BASE	; Initialise Parameter Stack (original
				; ROM does not do this meaning initially
				; data is loaded into top of memory)

	jp RESTART_CONT

	endif

SPARE:	ds 0x1CC0-$
	
	;; Jump table of 0x20 service routines, corresponding to special
	;; key presses
SPECIAL_CHAR_TABLE:		; 1CC0h
	dw NO_ACTION		; 00 - No action, RET
	dw GO_HOME		; 01 - Home
	dw NO_ACTION		; 02 - No action, RET
	dw NO_ACTION		; 03 - No action, RET
	dw NO_ACTION		; 04 - No action, RET
	dw NO_ACTION		; 05 - No action, RET
	dw NO_ACTION		; 06 - No action, RET 
	dw NO_ACTION		; 07 - No action, RET

	dw LEFT			; 08 - Left
	dw RIGHT		; 09 - Right
	dw DOWN			; 0A - Down
	dw UP			; 0B - Up
	dw CLS			; 0C - Clear screen
	dw CR			; 0D - Carriage return
	dw NO_ACTION		; 0E - No action, RET
	dw NO_ACTION		; 0F - No action, RET

	dw NO_ACTION		; 10 - No action, RET
	dw PUT_DPAD		; 11 - Put PAD
	dw 0x041D		; 12 - ???
	dw 0x0441		; 13 -
	dw 0x044B		; 14 - Fetch PAD
	dw NO_ACTION		; 15 - No action, RET
	dw NO_ACTION		; 16 - No action, RET
	dw NO_ACTION		; 17 - No action, RET
	
	dw NO_ACTION		; 18 - No action, RET
	dw NO_ACTION		; 19 - No action, RET
	dw DEL_LINE		; 1A - Delete line
	dw RUBOUT		; 1B - Rubout
	dw INS_LINE		; 1C - Graphics mode
	dw 0x03BD		; 1D - 
	dw EDIT			; 1E - Edit
	dw FLASH_CURSOR		; 1F - ? Flashing cursor

	;; ZX81-FORTH BY DAVID HUSBAND  COPYRIGHT (C) 1983
COPYRIGHT_MSG:
	if NTSC=1
	db 0x33, _CLS, _T, _R, _E, _E, _MINUS, _F
	else
	db 0x33, _CLS, _Z, _X, _8, _1, _MINUS, _F
	endif
	db _O, _R, _T, _H, _SPACE, _B, _Y, _SPACE
	if NTSC=1
	db _T, _R, _E, _E, _SPACE, _S, _Y, _S
	db _T, _E, _M, _S, _SPACE, _ENTER, _DOWN, _C
	else
	db _D, _A, _V, _I, _D, _SPACE, _H, _U
	db _S, _B, _A, _N, _D, _ENTER, _DOWN, _C
	endif
	db _O, _P, _Y, _R, _I, _G, _H, _T
	db _SPACE, _LEFTPARENTH, _C, _RIGHTPARENTH, _SPACE, _1, _9, _8
	db _3, _ENTER, _DOWN, _DOWN
	
NEW_LINE_MSG:
	db 0x02, _ENTER, _DOWN
	db _SPACE		; Not used

	;; Keyboard mapping (unshifted)
KEY_CODES:
	if MINSTREL4=1
	db _NULL, _A, _Q, _1, _0, _P, _ENTER, _SPACE
	db _PERIOD, _S, _W, _2, _9, _O, _L, _M
	db _Z, _D, _E, _3, _8, _I, _K, _N
	db _X, _F, _R, _4, _7, _U, _J, _B
	db _C, _G, _T, _5, _6, _Y, _H, _V
	else
	db _NULL, _A, _Q, _1, _0, _P, _ENTER, _SPACE
	db _Z, _S, _W, _2, _9, _O, _L, _PERIOD
	db _X, _D, _E, _3, _8, _I, _K, _M
	db _C, _F, _R, _4, _7, _U, _J, _N
	db _V, _G, _T, _5, _6, _Y, _H, _B
	endif
	
OK_MSG: db 0x05, _SPACE, _O, _K, _ENTER, _DOWN
	
ERR_MSG:
	db 0x07, _SPACE, _E, _R, _R, _O, _R, _SPACE

CPU_MSG:
	db 0x09, _ENTER, _DOWN, _Z, _X, _MINUS, _Z, _8
	db _0, _SPACE

L1d78:	;; Keyboard mapping (shifted)
	if MINSTREL4=1
	db _NULL, _CLS, _COMPILE, _EDIT
	db _RUBOUT, _QUOTES, _HOME, _BREAK
	db _COMMA, _PERCENT, _EXCLAMATION, _FETCHPAD
	db _INSERTLINE, _RIGHTPARENTH, _EQUALS, _GREATERTHAN
	db _COLON, _QUOTE, _AT, _PUTPAD
	db _RIGHT, _LEFTPARENTH, _PLUS, _LESSTHAN
	db _SEMICOLON, _BACKSLASH, _LEFTSQBRACKET, _DELETELINE
	db _DOWN, _DOLLAR, _MINUS, _ASTERISK
	db  _QUESTIONMARK, _POWER, _UNDERSCORE, _LEFT
	db _UP, _RIGHTSQBRACKET, _HASH, _SLASH
	else
	db _NULL, _CLS, _COMPILE, _EDIT
	db _RUBOUT, _QUOTES, _HOME, _BREAK
	db _COLON, _PERCENT, _EXCLAMATION, _FETCHPAD
	db _INSERTLINE, _RIGHTPARENTH, _EQUALS, _COMMA
	db _SEMICOLON, _QUOTE, _AT, _PUTPAD
	db _RIGHT, _LEFTPARENTH, _PLUS, _GREATERTHAN
	db _QUESTIONMARK, _BACKSLASH, _LEFTSQBRACKET, _DELETELINE
	db _UP, _DOLLAR, _MINUS, _LESSTHAN
	db  _SLASH, _POWER, _UNDERSCORE, _LEFT
	db _DOWN, _RIGHTSQBRACKET, _HASH, _ASTERISK
	endif
	
	;; Default values of system variables 0x1DA0--0x1DFF
DEFVARS:
	db 0x00, 0x00, 0x00, 0x00	; UNKNOWN5 (7C60)
	db 0x0F, 0x00, 0x00, 0x00 	; TIC_COUNTER (7C64) - 15 frames
	db 0x00, 0x1A, 0x4F, 0x00	; PER (7C68)
	dw 0x4000			; RAM_START
	if MINSTREL4=1
	dw 0x2400
	else
	dw OFFSET+0x0200		; P_DBUFFER
	endif
	dw OFFSET+0x0198		; PMTASK_LIST_HD (1db0)
	dw 0x2000			; FENCE
	dw PRINT_DRV			; PRINTER DRIVER
	if MINSTREL4=1
	dw 0x2400
	else
	dw OFFSET+0x0200		; DISP_2
	endif
	db 0ffh,000h			; F_WARM_RESTART
	dw STR_TO_NUM			; PARSE_NUM_ROUTINE
	dw NO_ACTION			; P_BACKTASK
	db 0x80, 0x80			; KIB_R_OFFSET, KIB_W_OFFSET
	dw RUN_VSYNC			; NEXT_DISP_ROUT (1DC0)
	dw RUN_DISPLAY			; P_RUN_DISPLAY
	dw OFFSET+0x01C0		; PSTACK_C
	dw 0x000a			; BASE
	dw DICT_START			; PSTART_DICT
	dw 0x4000			; P_HERE
	dw DICT_START			; START_DICT_DEF
	dw 0x4000			; UNKNOWN2 (1DCE)
	dw OFFSET-0x0080		; STACKP_BASE
	dw OFFSET+0x0198 		; PCUR_TASK_STRUCT
	dw 0x0000			; MSTACK0
	dw OFFSET+0x0136		; MSTACK1
	db 0x00, 0x00, 0x00, 0x00	; MTASK_TAIL
	db 0x00, 0x00, 0xF1, 0xFF
	db 0xFF, 0xFF, 0x00
	dw DEF_TASK
	db 0x00				; FLAGS3 (1DE5)
	dw PROCESS_KEY			; KBD_ROUT
	dw WRITE_TO_KIB			; CO_KBD_ROUT
	db 0x03				; (1DEA)
	db 0x00				; POSSIBLE_KEY (1DEB)
	db 0x80				; LAST_KEY
	db 0x80				; FLAGS
	if MINSTREL4
	db 0x00, 0x00, 0x00, 0x00	; SCREEN_INFO_EE (1DEE)
	db 0x1F, 0x0F, 0x00, 0x20
	db 0x00, 0x00, 0x00, 0x00	; SCREEN_INFO_CO (1DF6)
	db 0x1F, 0x17, 0x00, 0x20
	else
	db 0x00, 0x00, 0x00, 0x00	; SCREEN_INFO_EE (1DEE)
	db 0x1F, 0x0F, 0x00, 0x00
	db 0x00, 0x00, 0x00, 0x00	; SCREEN_INFO_CO (1DF6)
	db 0x1F, 0x17, 0x00, 0x00
	endif
	
	db 0x00				; FLAGS2
	db 0x0F				; TOKEN_LN

	;; Character ROM (64 characters * 8 pixel-rows per character)
	;; Needs to be a the start of a RAM page -- by default, it is
	;; 0x1E00.
	;;
	;; When displayed on screen, character code is code-0x20
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
	db 0x00, 0x00, 0x08, 0x2A, 0x1C, 0x2A, 0x08, 0x00 ; 0A Asterisk
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
	db 0x00, 0x06, 0x06, 0x00, 0x00, 0x06, 0x06, 0x00 ; 1A Colon
	db 0x00, 0x06, 0x06, 0x00, 0x06, 0x06, 0x02, 0x00 ; 1B Semicolon
	db 0x00, 0x00, 0x04, 0x08, 0x10, 0x08, 0x04, 0x00 ; 1C Less than
	db 0x00, 0x00, 0x00, 0x3C, 0x00, 0x3C, 0x00, 0x00 ; 1D Equals
	db 0x00, 0x00, 0x10, 0x08, 0x04, 0x08, 0x10, 0x00 ; 1E Greater than
	db 0x00, 0x3C, 0x42, 0x02, 0x0C, 0x00, 0x08, 0x00 ; 1F Question mark
	db 0x00, 0x3C, 0x42, 0x5C, 0x52, 0x44, 0x3E, 0x00 ; 20 At char
	db 0x00, 0x3C, 0x42, 0x42, 0x7E, 0x42, 0x42, 0x00 ; 21 'A'
	db 0x00, 0x7C, 0x42, 0x7C, 0x42, 0x42, 0x7C, 0x00 ; 22 'B'
	db 0x00, 0x3C, 0x42, 0x40, 0x40, 0x42, 0x3C, 0x00 ; 23 'C'
	db 0x00, 0x7C, 0x42, 0x42, 0x42, 0x42, 0x7C, 0x00 ; 24 'D'
	db 0x00, 0x7E, 0x40, 0x7C, 0x40, 0x40, 0x7E, 0x00 ; 25 'E'
	db 0x00, 0x7E, 0x40, 0x7C, 0x40, 0x40, 0x40, 0x00 ; 26 'F'
	db 0x00, 0x3C, 0x42, 0x40, 0x46, 0x42, 0x3C, 0x00 ; 27 'G'
	db 0x00, 0x42, 0x42, 0x7E, 0x42, 0x42, 0x42, 0x00 ; 28 'H'
	db 0x00, 0x1C, 0x08, 0x08, 0x08, 0x08, 0x1C, 0x00 ; 29 'I'
	db 0x00, 0x02, 0x02, 0x02, 0x02, 0x42, 0x3C, 0x00 ; 2A 'J'
	db 0x00, 0x42, 0x44, 0x78, 0x48, 0x44, 0x42, 0x00 ; 2B 'K'
	db 0x00, 0x40, 0x40, 0x40, 0x40, 0x40, 0x7E, 0x00 ; 2C 'L'
	db 0x00, 0x42, 0x66, 0x5A, 0x42, 0x42, 0x42, 0x00 ; 2D 'M'
	db 0x00, 0x42, 0x62, 0x52, 0x4A, 0x46, 0x42, 0x00 ; 2E 'N'
	db 0x00, 0x3C, 0x42, 0x42, 0x42, 0x42, 0x3C, 0x00 ; 2F 'O'
	db 0x00, 0x7C, 0x42, 0x42, 0x7C, 0x40, 0x40, 0x00 ; 30 'P'
	db 0x00, 0x3C, 0x42, 0x42, 0x42, 0x4A, 0x3E, 0x00 ; 31 'Q'
	db 0x00, 0x7C, 0x42, 0x42, 0x7C, 0x44, 0x42, 0x00 ; 32 'R'
	db 0x00, 0x3C, 0x42, 0x30, 0x0C, 0x42, 0x3C, 0x00 ; 33 'S'
	db 0x00, 0x3E, 0x08, 0x08, 0x08, 0x08, 0x08, 0x00 ; 34 'T'
	db 0x00, 0x42, 0x42, 0x42, 0x42, 0x42, 0x3C, 0x00 ; 35 'U'
	db 0x00, 0x42, 0x42, 0x42, 0x42, 0x24, 0x18, 0x00 ; 36 'V'
	db 0x00, 0x42, 0x42, 0x42, 0x5A, 0x5A, 0x24, 0x00 ; 37 'W'
	db 0x00, 0x42, 0x24, 0x18, 0x18, 0x24, 0x42, 0x00 ; 38 'X'
	db 0x00, 0x42, 0x24, 0x18, 0x08, 0x08, 0x08, 0x00 ; 39 'Y'
	db 0x00, 0x7E, 0x04, 0x08, 0x10, 0x20, 0x7E, 0x00 ; 3A 'Z'
	db 0x00, 0x0C, 0x08, 0x08, 0x08, 0x08, 0x0C, 0x00 ; 3B '['
	db 0x00, 0x20, 0x20, 0x10, 0x08, 0x04, 0x04, 0x00 ; 3C '\'
	db 0x00, 0x30, 0x10, 0x10, 0x10, 0x10, 0x30, 0x00 ; 3D ']'
	db 0x00, 0x08, 0x1C, 0x08, 0x08, 0x08, 0x08, 0x00 ; 3E '^'
	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF ; 3F '_'

HFORTH_END:
	end
