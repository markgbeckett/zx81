# Running Husband Forth on the Minstrel 3

## Background

Husband Forth was an alternative ROM image for the ZX81, which replaced the default Sinclair BASIC monitor with a Forth-based programming environment.

Forth was relatively popular in the early 1980s. Forth programs are compact and fast (when compared to BASIC), making Forth well-suited to the limited resources of a microcomputer. Better still, being ROM-based, Husband Forth does not take any precious RAM for the monitor code, so can run with as little as 2 kB of RAM.

Released in early 1984, Husband Forth looks to be a particularly advanced Forth monitor which, for example, provides multi-tasking support -- something not common on microcomputers, at the time.

Husband Forth is likely to be a port of a Timex Sinclair 1000 ROM (referred to by several names, including [Tree Forth](https://www.timexsinclair.com/article/tree-forth/), Multi-Forth, and Pluri-Forth). The Timex Sinclair website suggests there were several versions of Tree Forth. However, all the Tree Forth ROMs I have found on in the Internet are identical (and do not contain version information). Tree Forth was configured for an NTSC display, as was typically used on the TS1000. The port to Husband Forth made the monitor run on a PAL system (as used in the UK) and adjusted the clock and timing routines to 50 Hz. 

Versions of both the Husband Forth ROM (and Tree Forth ROM) are distributed with the [EightyOne emulator](https://sourceforge.net/projects/eightyone-sinclair-emulator/). ROMs are also available from other sites, but they look to be identical to the versions distributed with EightyOne.

The Tynemouth Minstrel 3 is a modern-day, ZX81-compatible microcomputer with 32kB of RAM. I hoped to be able to run Husband Forth on the Minstrel 3. Sadly, neither the Husband Forth nor the Tree Forth ROMs, distributed with EightyOne, run on the Minstrel 3. I wanted to understand why this was and whether I could get Husband Forth to work, so I began to disassemble and study the ROM.

![Tree Forth on a Minstrel 3](tree_forth.png)

My expectation was that the display-handling code was not (quite) compatible with the Minstrel 3. A [discussion on a retrocomputing forum](https://forum.tlienhard.com/phpBB3/viewtopic.php?t=1438) suggests that the ROMs distributed with EightyOne have been tweaked to work correctly with the emulator, so perhaps these tweaks had rendered the ROMs unusable on real hardware.

However, as I explain below, the problems were somewhat more subtle than that.

Happy Forth programming!


## Running Husband Forth on a Minstrel 3

You can run either Husband Forth or Tree Forth on the Minstrel 3, but copying one of the ROM images [hforth_pal.rom](hforth_pal,rom) or [hforth_ntsc.rom](hforth_ntsc.rom) onto a suitable EPROM and installing in your Minstrel 3.

Make sure to set the appropriate jumpers for either NTSC or PAL, as appropriate to your chosen ROM, and you should be able to boot into the Forth system console screen.

You can download a PDF copy of the [Husband Forth user guide](https://www.retrocomputers.gr/media/kunena/attachments/169/zx81-forth-manual.pdf) (which is also suitable for Tree Forth)

## Husband Forth Display Handling

The ZX81 produces a PAL-compatible display signal. It's sibling, the Timex Simclair 1000, which was sold mostly in North America produced an NTSC-compatible display. The two standards are very similar, but with different timings. Here I am looking at the PAL system, though these notes are relevant to NTSC with specific timings adjusted appropriately.

The official [PAL display format](https://martin.hinner.info/vga/pal.html) produces a TV frame consisting of 625 scanlines and the frame is updated 25 times per second. Each frame is transmitted as two fields of 312.5 scan lines, and pairs of fields are interlaced to produce a picture with less flicker. Not every scanline is visible: a small number are used for image synchronisation, to ensure the display is stable.

The first component of image synchronisation, generated at the beginning of each field, is called the vertical synchronisation (Vsync, for short). Its length alternates between 7 and 8 scanlines (448 and 512 us), which provides the interlacing effect. Following the Vsync pulse, a series of 64us scanlines follows, containing the visible part of the image. However, not all 64us of each horizontal scanline contains image data: the first 12us is used for a horizontal pulse (4us) and backporch signal (8us), referred to together as Hsync, which helps align the line and callibrate the brightness.

Instead of an interlaced 625-line signal, the ZX81 is designed to produce a simplified field of around 312 lines at a rate of 50 Hz (20ms/ fram), with a consistent Vsync pulse to prevent interlacing. From now on, we will refer to the fields produced by the ZX81 as frames.

Building on its predecessor, the ZX81 relied heavily on the Z80 CPU (with some help from custom electronics) to generate the display signal. This made the ZX81 very low cost, but also meant that the micro potentially spent much (in fact, up to 80%) of its time producing a display, rather than running user programs.

Husband Forth uses very similar code to the ZX81 8K BASIC ROM for creating the display, though with some differences. A good explanation of how the ZX81 generates its display is provided on the [Tynemouth Software blog](http://blog.tynemouthsoftware.co.uk/2023/10/how-the-zx81-generates-video.html). Here I summarise how Husband Forth generates a display, though only going into detail on the differences from the ZX81 display.

Both Husband Forth and ZX81 8K BASIC generate the display in four phases:

* A constant-length Vsync signal is produced to initiate the start of a new frame. 

* A top border, consisting of blank scanlines, is produced, using the NMI mechanism to allow the Forth monitor to be run with periodic interruptions for creating each scan line in the border.

* The main display (where text is shown) is produced by "executing" the display and relying on a clever configuration of the ZX81 hardware to turn this into a valid display signal.

* The bottom border is generated, again using the NMI mechanism and allowing the Forth monitor code to make progress.

As you can see, most of the Forth monitor code runs during the top- and bottom- border generation, though there is also some keyboard-scanning and multitasking scheduling code hidden inside the VSync stage, as we will describe below.

### Border Generation

The top and bottom borders of the display are generated via the Z80's NMI support. On the ZX81, when active, an NMI pulse is generated every 64 microseconds (the length of time to produce a scan line on a PAL display). The NMI routine in Husband Forth (at address 0x066h) simply decrements a counter and, unless it reaches zero, returns to the previously running routine. The counter is held in the A' register, which is initiated to the desired number of scanlines for the border.

When the counter reaches zero, the border has been produced, so the computer moves on to the next phase of the display (either producing the Vsync, after the bottom border is produced, or producing the main display after the top border is produced). This is done by jumping to a routine whose address has been previously stored in memory -- either the start of a Vsync or main-display routine.

The kernel of the NMI routine to generate the top and bottom borders is reproduced below.

```
NMI:	ex af, af'		; Retrieve and decrement cycle counter
	dec a			

	jr z, NMI_DONE		; Move on to next phase of display, if
				; done

	ex af, af'		; Otherwise, restore counter and ...

	ret			; ... return to calling program, with
				; maskable interrupt disabled (that is,
				; not using RETN here)

NMI_DONE: ; 0x006C
	push hl 		; Save register

	ld hl,(NEXT_DISP_ROUTINE) ; Proceed to next routine (either
	jp (hl)			  ; RUN_VSYNC or RUN_DISPLAY)
```

Slightly unusually, a standard `ret` instruction is used to exit the NMI routine (wheras, more normally, a `retn` would be used). Using `ret` means the previousstate of the maskable interrupt is lost and the maskable interrupts is always be disabled.

By default, Husband Forth sets the top border to have 31 lines and the bottom border to have 74 lines, so the main screen is not centred vertically. It has been suggested that the unequal borders are related to the way it was ported from the NTSC-compatible Tree Forth (adding rows to the bottom border to produce sufficient scanlines for a PAL display).

### Vsync Generation

The Vsync signal is controlled by the ZX81s ULA. The Vsync signal is turned on, from software, using `in (0xFE),a` and turned off using `out (<any_address>),a`. Other than activating and deactivating the signal, the Z80 has little to do during this phase. However, the Z80 is responsible for getting the timing right and on both ZX81 BASIC and Husband Forth, the time between the start and end of the Vsync signal is used to do something useful: to scan the keyboard. To work, the code to scan the keyboard has to take a fixed amount of time.

The Vsync routine in Husband Forth (starting at address 0x98h) first disables the NMI signal, then activates the Vsync signal. It them proceeds with a carefully timed sequence of instructions that, first of all, sets up the NMI paramaters (initial value of A' and NEXT_DISPLAY_ROUTINE) for the top border and then reads the keyboard. The combined sequence of actions takes 1,304--1,312 T states (about 402ms or 6 scanlines). This is a little shorter than te ideal Vsync of 7--8 scanlines.

After the Vsync signal is deactivated, the NMI signal is reenabled, to trigger the production of the top border of the display, though before returning to user code, the keyboard scanning routine is wrapped up and the multitasking scheduler is serviced. This further reduces the time available for user code.

### Generation of the main display

The most complicated part of display generation is the main screen area, in which text is displayed. This involves "executing" each row of the display as a carefully crafted instruction sequence that is terminated by a maskable interrupt.

I have put "executing" in quotes because this is not completely correct. The Z80 is instructed to run a row of the mirror of the display buffer (in the upper 32K of the ZX81 memory space) with a `jp` instruction. However, the Z80 never receives the character codes that are stored in the display: they are characters not instructions, so would be meaningless. Instead the Z80 receives `nop` instructions and the character codes are intercepted by other ZX81 hardware and turned into a display signal.

On the ZX81, when the Z80 tries to retrieve an instruction from an address in upper memory (address bit 15 is high) for which bit 6 of the byte at the byte at the address is low, the bytes is replaced by `nop`. The requirement on bit 6 being low is significant, as I will explain shortly. First, note that the ZX81's character set contains 64 characters with codes in the range of 0 to 63 (all of which have bit 6 low) plus inverted versions of these characters, between 128 and 191 (again, all of which have bit 6 low). 

While the Z80 receives a `nop` instruction, the actual byte read from memory is visible on part of the databuse to a character latch, which interprets it as a pixel pattern (a row of a character) to be displayed. This is done by creating an address, combined the contents of the lower seven bits of the I register, with the lower six bits of the byte read, and augmenting to a three-bit counter to produce a 15-bit address, as follows:

*   3-bit counter - A0--A2
*   lower six bits of character code - A3--A8
*   lower seven bits of I register - A9--A15

The sequence of eight pixels in the character row is read from that derived memory address and transmitted bit-by-bit to the screen in the 4 T states (1.2us) needed for the Z80 to execute `nop`.

Husband Forth stores the pixel patterns of the 64 characters it recognises at address 0x1E00. For example, row 0 of the space character (with character code 0) is stored at 0xE00, row 1 is at 0x1E01, and so on. Then row 0 of the exclamation mark (with character code 1) is stored at 0x1E08, row 1 at 0x1E09, and so on. During startup, the I register is initialised to hold 0x1E.

The three bit counter steps through 0, 1, ..., 7, incrementing once per scanline, so a character row of the display is produced by executing the same row of the display buffer eight times (with the 3-bit counter ensuring the correct pixel row is read and displayed).

For this mechanism to work, there needs to be a way to interrupt execution of the display buffer at the end of each character row. Husband Forth uses a different technique to ZX81 BASIC to do this, so we will briefly describe both approaches.

ZX81 BASIC relies on an newline character to terminate execution of a row of the display buffer. To accommodate this, the ZX81 BASIC display buffer allows up to 33 characters per line (up to 32 printable characters and a newline character). Because the new-line character (code 0x76) has bit 6 set, it is not substituted by `nop` but is passed to the Z80. Fortuitously(!), code 0x76 corresponds to the `halt` instructions, so causes the Z80 to stop and wait for a maskable interrupt. Husband Forth does not use a newline character but instead relies on every row of the display buffer having 32 characters.

Both ZX81 BASIC and Husband Forth then rely on careful timing to ensure that a maskable interrupt is generated at the appropriate time to end the scanline.

The ZX81 does not have a typical maskable interrupt. Instead, the interrupt line is configured so it is triggered whenever bit 6 of the refresh register R goes low. The Z80 refresh register is intended to support the use of dynamic RAM exploiting the time when the Z80 is processing an instruction to put a dummy memory address on the bus and triggering a fake read. To step through the memory space, the R register is incremented after each instruction fetch. The ZX81 does not support DRAM and, instead uses the R register to interrupt the Z80 at the end of each main-display scanline. Effectively, one initiates the R register to ensure the bit 6 of the register goes low when the Z80 gets to the 32nd character of the current display-buffer row.

The following code segment, from Husband Forth, shows how this works:
```
EXEC_DISPLAY:
	ld a, 0xDD              ; FFh - 22h (32 characters plus
	                        ; three additional instructions)
	...
	ld r,a			; Reset refresh counter (to 0xDD)
	ei			; Enable maskable interrupts

	jp (hl) 		; Execute next display line
```

The R register increments to 0xDE after the Z80 executes `ei`, then increments to 0xDF after the Z80 executes `jp (hl)`, and then increments 32 more times as the Z80 executes the 32 characters in the current row of the display buffer, before it wraps around and triggers the maskable interrupt.

In Husband Forth (and ZX81 BASIC), interrupt mode 1 is used to execute the routine at address 0x38. This routine loops through the 24 character rows of the display (tracked in the C register) and eight pixel rows per character row (tracked in the B register), to generate the main display.

This maskable-interrupt routine is reproduced below:

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
	ei			; Enable interrupts

	jp (hl) 		; Execute next display line

NEXT_SCANLINE:
	pop de			; Discard return address as we need
				; to re-run current row (address still
				; in HL))
	
	ret z			; Timing-related: this is never
	 			; satisfied, so always adds 5 T states
	 			; delay 

	jr EXEC_DISPLAY		; Continue to execute display line

```

When setting up to produce the main display, the BC pair is initialised to 24 rows and 9 pixels per row (one more than needed as B is decremented at the start of the interrupt).

There are two significant paths through the interrupt routine (the `jp` instruction is the branch point): one when progressing through a character row, and one when advancing to the next character row. It is very important that both paths take the same amount of time to complete, to provide a stable display. Using a `jp` rather than `jr` instruction and inserting a dummy `ret z` ensures both paths take 62 T states (20.3 us) to prepare to execute the display buffer. This time is reflected by the left-hand border on the screen display.

The maskable interrupt is also responsible for triggering the Hsync signal. This happens in response to the Z80's IORQ signal, which is asserted as part of the fetch cycle of the first instruction of the interrupt routine (the maskable interrupt takes 13 T states to assert). Minstrel 3 circuitry means a Hsync signal is enabled during the fetch cycle of the next instruction. The Hysn signal is subsequently disabled during the fetch cycle of the fourth instruction.

At the end of the main display, the NMI circuitry is enabled (having previously set it up to display the bottom border). The routine then exits (back to user code) and relies on the NMI circuitry to interrupt the Z80 at the end of each bottom-border display line.

### Husband Forth Timing

As noted above, the Husband Forth ROM does not produce a stable display on the Minstrel 3, so the first thing I wanted to do was check the timing of display generation in the ROM, based on the deduction above.

A Husband Forth frame consists of a VSync signal (taking 402us), 105 border lines (each taking 64 us) and 192 main display lines, each taking 62.4us (based on 64+32*4+11 T states). This give a frame length of 18.1ms. Both the frame length and the main-display scanline length are shorter than idea, potentially leading to an undisplayable signal.

However, when investigating the Husband Forth display code, I also spotted that the Z80 is never set to interrupt mode 1. This means it will operate in Interrupt Mode 0, which retrieves and executes an instruction from the data bus when an interrupt is generated.

### 

8 scan lines = 512 us = 1,664 T states

Would like scanlines to be 12 T states longer

Scanline:

16 T states (Hsync) + 42 T states (left) +  128 (text) + 13+4+16  (interrupt and IORQ) = 207 T states

## Dictionary Layout

All Forth environments rely on a dictionary of words, which the user adds to to provide the functionality they need. On Husband Forth, the core of the dictionary is held in ROM and users add to it (with a colon-defined word, for example) in RAM.

A typical H Forth word has three components:
* It begins with a one-byte name-length-field, which is primarily used to hold the length of the word name. The lowest six bits hold the word-name length. The remaining tow bits are used to hold word properties.
* The name-length-field is followed immediately by the name (as a sequence of bytes)
* Third, a two-byte, word-length field which records the total size of the word in memory.
* Fourth, the body of the word -- that is, its parameter field.

A task word is slightly more complicate. It has four components:
* It begins with a one-byte name-length-field, like a colon definition.
* The name-length-field is followed immediately by the name.
* Third, a two-byte, word-length field which records the total size of the word in memory.
* Fourth, the address of the parameter field is stored, which is later in the defintion
* Fifth is a sixteen-byte data structure containing key information about the task.
* Finally, there is the parameter field, which contains a link to the parameter field of the word that the task actions.

Further, the beginning (tail) and end (head) of the dictionary are stored in two system variables. New words are added to the head of the dictionary.

The ROM part of the dictionary is actually backwards. The last word in the user's dictionary has a word-length field that effectively points to the head of the ROM dictionary: that is, to `M*`.

## Character Stack

In the default memory configuration, the character stack is located between FCC0h and FCFFh (that is, space for 64 characters) and grows down from FCFFh. The top of stack is held in a one-byte system-variable at FC84h (label `PSTACK_C`) and initially contains the value C0h.

The restart routines at 18h and 20h are used to push/ pop characters onto/ off the stack, respectively. There are also more advanced routines, which use character stack, including:

* PUSH_STRING (0637h) - which pushes a countable string in memory onto character stack (also pushing length of string onto parameter stack)

* PRINT_STRING (0652h) - Print string from character stack (with length on parameter stack) to current screen

* MATCH STRING (06A7h)

