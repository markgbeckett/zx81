# Running Husband Forth on the Minstrel 3

Husband Forth was an alternative ROM image for the ZX81, which replaced the default Sinclair BASIC monitor with a Forth-based programming environment.

Forth was relatively popular in the early 1980s. Forth programs are compact and fast (when compared to BASIC), making Forth well-suited to the limited resources of a microcomputer. Husband Forth looks to be a particularly advanced Forth monitor which, for example, provides multi-tasking support -- very unusual at the time.

Husband Forth is likely to be a port of a Timex Sinclair 1000 ROM (referred to by several names, including [Tree Forth](https://www.timexsinclair.com/article/tree-forth/), Multi-Forth, and Pluri-Forth). The Timex Sinclair website suggests there were several versions of Tree Forth. However, all the Tree Forth ROMs I have found on in the Internet are identical (and do not contain version information). Tree Forth was onfigured for an NTSC display, as was typically used on the TS1000. The port to Husband Forth was likely to involve adjusting the code to work with a PAL display (as used in the UK). Comparing the ROM images of Tree Forth and Husband Forth highlights around 30 byte-level differences, with substantive differences in the code that renders the display.

Versions of both the Husband Forth ROM (and Tree Forth ROM) are distributed with the [EightyOne emulator](https://sourceforge.net/projects/eightyone-sinclair-emulator/). ROMs are also available from other sites, but they look to be identical to the versions distributed with EightyOne.

The Tynemouth Minstrel 3 is a modern-day, ZX81-compatible microcomputer with 32kB of RAM. I hoped to be able to run Husband Forth on the Minstrel 3. Sadly, neither Husband Forth ROM nor Tree Forth ROM, distributed with EightyOne, runs correctly on the Minstrel 3. I wanted to understand why this was and whether I could get Husband Forth to work, so I began to disassemble and study the ROM.

My expectation was that the display-handling code was not (quite) compatible with the Minstrel 3. A [discussion on a retrocomputing forum](https://forum.tlienhard.com/phpBB3/viewtopic.php?t=1438) suggests that the ROMs distributed with EightyOne have been tweaked to work correctly with the emulator, so perhaps these tweaks had rendered the ROMs unusable on real hardware.

However, my investigations to date have identified a number of issues that seem to prevent Husband Forth running on the Minstrel 3 as I describe below.

## Husband Forth Display Handling

Building on its predecessor, the ZX81 relied on the Z80 CPU (with some custom electronics) to generate a display. This made the ZX81 very low cost, but also meant that the ZX81 potentially spent much (in fact, up to 80%) of its time producing a display, rather than running user programs.

Husband Forth uses very similar code to the ZX81 8K BASIC ROM for creating the display, though with some differences. A good explanation of how the ZX81 generates its display is provided on the [Tynemouth Software blog](http://blog.tynemouthsoftware.co.uk/2023/10/how-the-zx81-generates-video.html). Here I summarise how Husband Forth generates a display, though only going into detail on the differences from the ZX81 display.

Both Husband Forth and ZX81 8K BASIC generate the display in four phases:

* A Vsync is produced to initiate the start of a new frame. This is a fixed-length signal during which Husband Forth scans the keyboard and does some initial processing of key presses.

* A top border is produced, using the NMI mechanism to allow the Forth monitor to be run, with perioidic interruptions for creating each scan line in the border.

* The main display (where text is shown) is produced by "executing" the display and relying on a clever configuration of the ZX81 hardware to turn this into a valid display signal.

* The bottom border is generated, again using the NMI mechanism to the Forth monitor to be run.

As you can see, most of the Forth monitor code runs during the top- and bottom- border generation, with specific keyboard-scanning operations hidden inside the VSync stage.

### Border Generation

The top and bottom borders of the display are generated via the Z80's NMI support. On the ZX81, when active, an NMI pulse is generated every 64 microseconds (the length of time to produce a scan line on a PAL display). The NMI routine in Husband Forth (at address 0x066h) simply decrements a counter and, unless it reaches zero, returns to the previously running routine. The counter is held in the A' register, which is initiated to the desired number of scanlines for the border.

Slightly unusually, a standard `ret` instruction is used to exit the NMI routine (wheras, more normally, a `retn` would be used). Using `ret` means the previousstate of the maskable interrupt will be lost and the maskable interrupts will always be disabled.

When the counter reaches zero, the border has been produced, so the computer moves onto the next phase of the display (either producing the Vsync, after the bottom border is produced, or producing the main display after the top border is produced). This is done by jumping to a routine whose address is stored in memory -- either the start of a Vsync or main-display routine.

```
NMI:	ex af, af'		; Retrieve and decrement cycle counter
	dec a			

	jr z, NMI_DONE		; Move on to next phase of display, if
				; done

	ex af, af'		; Otherwise, restore counter and ...

	ret			; ... return to calling program, with
				; maskable interrupt disabled (that is,
				; not using RETN here.

NMI_DONE: ; 0x006C
	push hl 		; Save register

	ld hl,(NEXT_DISP_ROUTINE) ; Proceed to next routine (either
	jp (hl)			  ; RUN_VSYNC or RUN_DISPLAY)
```

By default, Husband Forth sets the top border to have 30 lines and the bottom border to have 74 lines, so the main screen is not centred vertically.


## Vsync Generation

Generation of the Vsync signal is relatively straightforward (via the ZX81's built-in ULA). One can turn on Vsync from software using `in (0xFE),a` and turn off Vsync using `out (<any_address>),a`.

The Vsync routine in Husband Forth (starting at address 0x98h) first disables the NMI signal, then activates the Vsync signal.

It them proceeds with a carefully timed sequence of instructions that, first of all, sets up the NMI paramaters (initial value of A' and NEXT_DISPLAY_ROUTINE) for the top border and then reads the keyboard. The keyboard reading routine is configured to be relatively constant in length. Combining this with the setup steps for the next NMI cycle, gives a Vsync time of 1,304--1,312 T states, depending on whether a key is pressed or not.

After the Vsync signal is deactivated, the NMI signal is reenabled, to produce the bottom border of the display. The keyboard scanning routine continues, while NMI is enabled, and -- after that -- the multitasking scheduler is serviced, before returning to user code.

### Generation of the main display

The most complicated part of display generation is the main screen area, in which text is displayed. This involves "executing" each row of the display as a carefully crafted instruction sequence that is terminated by a maskable interrupt.

I have put "executing" in quotes because this is not completely correct. The Z80 is instructed to run a row of the mirror of the display buffer (in the upper 32K of the ZX81 memory space) with a `jp` instruction. However, the Z80 never receives the character codes that are stored in the display: they are characters not instructions, so would be meaningless. Instead the Z80 receives `nop` instructions and the character codes are intercepted by other ZX81 hardware and turned into a display signal.

The ZX81 replaces the instruction on the database by `nop` when the Z80 tries to retrieve an instruction from an address in upper memory (address bit 15 is high) for which bit 6 of the byte at the address is low. The requirement on bit 6 being low is significant, as I will explain shortly. First, note that the ZX81's character set contains 64 characters with codes in the range of 0 to 63 (all of which have bit 6 low) plus inverted versions of these characters, between 128 and 191 (again, all of which have bit 6 low). 

While the Z80 receives a `nop` instruction, the actual byte read from memory is visible on part of the databuse to a character latch, which interprets it as a pixel pattern (a row of a character) to be displayed. This is done by creating an address, combined the contents of the lower seven bits of the I register, with the lower six bits of the byte read, and augmenting to a three-bit counter to produce a 15-bit address, as follows:

*   3-bit counter - A0--A2
*   lower six bits of character code - A3--A8
*   lower seven bits of I register - A9--A15

A pixel bitmap is read from that address and transmitted bit-by-bit to the screen at the expected cadence of the display.

Husband Forth stores the pixel patterns of the 64 characters it recognises at address 0x1E00. For example, row 0 of the space character (with character code 0) is stored at 0xE00, row 1 is at 0x1E01, and so on. Then row 0 of the exclamation mark (with character code 1) is stored at 0x1E08, row 1 at 0x1E09, and so on. During startup, the I register is initialised to hold 0x1E.

The three bit counter steps through 0, 1, ..., 7, incrementing once per scanline, so a character row of the display is produced by executing the same row of the display buffer eight times (with the 3-bit counter ensuring the correct pixel row is read and displayed).

For this mechanism to work, there needs to be a way to interrupt execution of the display buffer at the end of each character row. Husband Forth uses a different technique to ZX81 BASIC to do this, so we will briefly describe both approaches.

ZX81 BASIC relies on an newline character to terminate execution of the display buffer. To accommodate this, the ZX81 BASIC display buffer allows up to 33 characters per line (up to 32 printable characters and and one newline character). Because the new-line character (code 0x76) has bit 6 set, it is not substituted by `nop` but is passed to the Z80. Fortuitously, code 0x76 corresponds to the `halt` instructions, so causes the Z80 to stop and wait for a maskable interrupt.

Husband Forth does not use a newline character and instead relies on every row of the display buffer having 32 characters. Both ZX81 BASIC and Husband Forth then rely on careful timing to ensure that a maskable interrupt is computed at the end of the display row.

The ZX81 does not have a typical maskable interrupt. Instead, the interrupt line is configured so it is triggered whenever bit 6 of the refresh register R goes low. The Z80 refresh register supports the use of dynamic RAM with the Z80 exploiting the time when the Z80 is processing an instruction to do a dummy memory access. The R register is incremented after each instruction fetch, so by setting an intial value for R correctly, one can ensure the bit 6 of the R register goes low when the Z80 gets to the 32nd character of the current display-buffer row.

The following code segment, from Husband Forth, shows how this works:
```
EXEC_DISPLAY:
	ld a, 0xDD              ; 32 characters plus two additional instructions
	...
	ld r,a			; Reset refresh counter (to 0xDD)
	ei			; Enable maskable interrupts

	jp (hl) 		; Execute next display line
```

The R register incremeents to 0xDE after the Z80 executes `ei`, then increments to 0xDF after the Z80 executes `jp (hl)`, and then increments 32 more times as the Z80 executes the 32 characters in the current row of the display buffer, before it wraps around and triggers the maskable interrupt.

In Husband Forth (and ZX81 BASIC), interrupt mode 1 is used to execute the routine at address 0x38. This routine loops through the 24 character rows of the display and eight pixel rows per character row, to generate the display.

This routine is reproduced below:

```
M_INT:	dec c			; Decrement scan-line counter

	jp nz, NEXT_SCANLINE	; Skip forward if more scan lines
				; to produce (Use JP to ensure
				; consistent timing with/ without a
				; branch)

	pop hl			; Retrieve return address (next
				; character to execute in display
				; buffer)

	dec b			; Decrement row counter

	ret z			; Exit, if done

	set 3,c			; Reset scan-line counter to 8

EXEC_DISPLAY:
	ld r,a			; Reset refresh counter
	ei			; Enable interrupts and trigger HSync

	jp (hl) 		; Execute next display line

NEXT_SCANLINE:
	pop de			; Discard return address as we need
				; to re-run current row (address still
				; in HL))
	
	ret z			; Timing-related: this is never
	 			; satisfied, so always adds 5 T states
	 			; to the code to delay start of HSYNC

	jr EXEC_DISPLAY		; Continue to execute display line

```

When setting up to produce the main display, the BC pair is initialised to 9 pixels per row (one more than needed as B is decremented at the start of the interrupt) and 24 rows.

There are two significant paths through the interrupt routine (the `jp` instruction is the branch point): one when progressing through a character row, and one when advancing to the next character row. It is very important that both paths take the same amount of time to complete, to provide a stable display. Using a `jp` rather than `jr` instruction and inserting a dummy `ret z` ensures both paths take 57 T states to prepare to execute the display buffer. This time is reflected by the left-hand border on the screen display.
