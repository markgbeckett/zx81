# Running ZX81-Forth on the Minstrel 3

## Background

ZX81-Forth (also, sometimes known as Husband Forth) was an alternative ROM image for the ZX81, which replaced the default Sinclair BASIC monitor with a Forth-based programming environment.

Forth was relatively popular in the early 1980s. Forth programs are compact and fast (when compared to BASIC), making Forth well-suited to the limited resources of a microcomputer. Better still, being ROM-based, ZX81-Forth does not take any precious RAM for the monitor code, so can run with as little as 2 kB of RAM.

Released in early 1984, ZX81-Forth looks to be a particularly advanced Forth monitor which, for example, provides multi-tasking support -- something not common on microcomputers, at the time.

As acknowledged in the [manual](https://www.retrocomputers.gr/media/kunena/attachments/169/zx81-forth-manual.pdf), ZX81-Forth is a port of a Timex Sinclair 1000 ROM (referred to by several names, including [Tree Forth](https://www.timexsinclair.com/article/tree-forth/), Multi-Forth, and Pluri-Forth). The Timex Sinclair website suggests there were several versions of Tree Forth. However, all the Tree Forth ROMs I have found on in the Internet are identical (and do not contain version information). Tree Forth was configured for an NTSC display, as was typically used on the TS1000. The port to ZX81-Forth made the monitor run on a PAL system (as used in the UK) and adjusted the clock and timing routines to 50 Hz. 

Versions of both the ZX81-Forth ROM (and Tree Forth ROM) are distributed with the [EightyOne emulator](https://sourceforge.net/projects/eightyone-sinclair-emulator/). ROMs are also available from other sites, but they look to be identical to the versions distributed with EightyOne.

The Tynemouth Minstrel 3 is a modern-day, ZX81-compatible microcomputer with 32kB of RAM. I hoped to be able to run ZX81-Forth on the Minstrel 3. Sadly, neither the ZX81-Forth nor the Tree Forth ROMs, distributed with EightyOne, run on the Minstrel 3. I wanted to understand why this was and whether I could get ZX81-Forth to work, so I began to disassemble and study the ROM.

![Tree Forth on a Minstrel 3](tree_forth.png)

My expectation was that the display-handling code was not (quite) compatible with the Minstrel 3. A [discussion on a retrocomputing forum](https://forum.tlienhard.com/phpBB3/viewtopic.php?t=1438) suggests that the ROMs distributed with EightyOne have been tweaked to work correctly with the emulator, so perhaps these tweaks had rendered the ROMs unusable on real hardware. (Note: I have found no evidence that the ROMS have been changed for EightyOne.)

However, as I explain below, the problems were somewhat more subtle than that.

ZX81-Forth (and Tree Forth) do not work, in their unmodified form, on the Minstrel 3 for three reasons:

* First, ZX81-Forth does not enable Z80 interrupt mode 1, which is critical to the correct operation of the monitor. The display routine in ZX81-Forth relies on the maskable interrupt routine at 0x38 to display text and, to use this routine, the Z80 needs to be configured for `IM 1`. This puzzled me at first, as I did not understand how the monitor worked on the ZX81 (when using the default interrupt mode 0). However, it turns out that the ZX81 has pull-ups fitted to the address bus, so will have the value 0xFF on the databus when a maskable interrupt is generated, and 0xFF is the Z80 instruction for `rst 0x38`. The Minstrel 3 does not have pull-up resistors on the databus (though could be modified to do so). To correct this, I inserted an `im 1` command into the initialisation code.

* Second, ZX81-Forth (usually) stores its state at the top of the address space (addresses 0xFB00--0xFFFF). On a ZX81 with at least 2kB of RAM, this will contain a shadow copy of the top five pages of RAM. The ZX81 hardware intercepts attempts to execute code in upper memory as part of its display-handling code. However, it does not intercept read and write operations, so this is okay. In contrast, the Minstrel 3 supports read and write operations to addresses up to 0x9FFF (which is the end of the 32K RAM). Read and write to the top of the address space (specifically, 0xFB00--0xFFFF) is not possible. To fix this, I refactored the code so that it used only the lower half of memory, with state being located in 0x7B00--0x7FFF.

* Third, ZX81-Forth tries to compute how much RAM a computer has at boot time, to allow it to accommodate a range of RAM configurations. The routine effectively counts down from the top of memory (0xFFFF) until it finds the end of the last shadow copy of RAM, and relies on memory being mirror across the address space in a certain way. However, because of the way memory on the Minstrel 3 is configured (same as the default configuration of the ZXpand) and because of the memory-access difference noted in the previous point, the routine reports the wrong amount of RAM. This is also a problem when using the ZXpand and may mean ZX81-Forth will not work with a ZXpand connected (I have not checked this). Of course, neither the Minstrel 3 nor the ZXpand existed when ZX81-Forth was developed, so this is understandable. I have addressed this by hardwiring the value for upper memory to be 16kB (which, plus the RAM in 0x2000--0x3FFF) means ZX81-Forth on the Minstrel 3 has 24kB of RAM to work with, in the range 0x2000--0x7FFF. The dictionary grows up from 0x2000 and the system state is stored  in 0x7B00--0x7FFF.

Whilst the display routine would work as well on the Minstrel 3 as it does on the ZX81, I do not think it is a very good match to the PAL standard (nor the NTSC standard, for Tree Forth). In particular, it produces too few scanlines and the end of main-display scanlines does not properly contain a right-hand border. I addressed the frame-size issue by increasing the border slightly, in the hope of producing a more stable display. I have yet to tackle the right-hand border issue, as this requires more significant reworking. At present, the right-hand end of each line of text is likely to be inverted and may be corrupted. Because of this, the display is not as crisp as I would like, especially when using NTSC mode.

## Running ZX81-Forth on a Minstrel 3

You can run either ZX81-Forth or Tree Forth on the Minstrel 3, by copying one of the ROM images [hforth_pal.rom](hforth_pal.rom) or [hforth_ntsc.rom](hforth_ntsc.rom) onto a suitable EPROM and installing in your Minstrel 3.

Make sure to set the appropriate jumpers for either NTSC or PAL, as appropriate to your chosen ROM, and you should be able to boot into the Forth system console screen.

You can download a PDF copy of the [ZX81-Forth user guide](https://www.retrocomputers.gr/media/kunena/attachments/169/zx81-forth-manual.pdf) (which is also suitable for Tree Forth)

I am also disassembling the ZX81-Forth ROM and it is possible to build the ROM from source should you wish. I have written the source in generic Z80 assembly language, which should hopefully work with most assemblers. I use the [z80asm](https://www.nongnu.org/z80asm/).

Near the start of the source code, you will find two variables that can be used to create different versions of the ROM. The variable `MINSTREL3` (set to zero or one) controls whether or not to patch the ROM for the Minstrel 3, and the variable `NTSC` (set to zero or one) determins whether to assemble for PAL displays (Husband variant) or for NTSC displays (Tree Forth original).

If you set `MINSTREL3=0`, then you will assemble the original ZX81-Forth (with `NTSC=0`) or Tree Forth (with `NTSC=1`) ROM. 

Happy Forth programming!

## ZX81-Forth Display Handling

The ZX81 produces a PAL-compatible display signal. It's sibling, the Timex Simclair 1000, which was sold mostly in North America, produced an NTSC-compatible display. The two standards are very similar, but with different timings. Here I am looking at the PAL system, though these notes are relevant to NTSC with specific timings adjusted appropriately.

The official [PAL display format](https://martin.hinner.info/vga/pal.html) produces a TV frame consisting of 625 scanlines and the frame is updated 25 times per second. Each frame is transmitted as two fields of 312.5 scan lines, and pairs of fields are interlaced to produce a picture with less flicker. Not every scanline is visible: a small number are used for image synchronisation, to ensure the display is stable.

The first component of image synchronisation, generated at the beginning of each field, is called the vertical synchronisation (Vsync, for short). Its length typically alternates between 7 and 8 scanlines (448 and 512 microseconds (us)), which provides the interlacing effect. Following the Vsync pulse, a series of 64us scanlines follows, containing the visible part of the image. However, not all 64us of each horizontal scanline contains image data: the first 12us is used for a horizontal pulse (4us) and backporch signal (8us), referred to together as Hsync, which helps align the line and callibrate the brightness.

Instead of an interlaced 625-line signal, the ZX81 is designed to produce a simplified field of around 312 lines at a rate of 50 Hz (20ms/ frame), with a consistent Vsync pulse to prevent interlacing. From now on, we will refer to the fields produced by the ZX81 as frames.

Building on its predecessor, the ZX81 relied heavily on the Z80 CPU (with some help from custom electronics) to generate the display signal. This made the ZX81 very low cost, but also meant that the micro potentially spent much (in fact, up to 80%) of its time producing a display, rather than running user programs.

ZX81-Forth uses very similar code to the ZX81 8K BASIC ROM for creating the display, though with some differences. A good explanation of how the ZX81 generates its display is provided on the [Tynemouth Software blog](http://blog.tynemouthsoftware.co.uk/2023/10/how-the-zx81-generates-video.html). Here I summarise how ZX81-Forth generates a display, though only going into detail on the differences from the ZX81 display.

Both ZX81-Forth and ZX81 8K BASIC generate the display in four phases:

* A constant-length Vsync signal is produced to initiate the start of a new frame. During this phase, the keyboard is read and the multitasking scheduler is serviced, though the task spills over into the top-border generation.

* A top border, consisting of blank scanlines, is produced, using the NMI mechanism to allow the Forth monitor to be run with periodic interruptions for creating each scan line in the top border.

* The main display (where text is shown) is produced by "executing" the display and relying on a clever configuration of the ZX81 hardware to turn this into a valid display signal.

* The bottom border is generated, again using the NMI mechanism in the same way as the top border, again allowing the Forth monitor code to make progress.

At the end of the bottom-border generation, the monitor returns to the first phase (producing the Vsync signal).

As you can see, most of the Forth monitor code runs during the top- and bottom- border generation, though there is also some keyboard-scanning and multitasking scheduling code hidden inside the VSync stage, as we will describe below.

### Border Generation

The top and bottom borders of the display are generated via the Z80's NMI support. On the ZX81, when active, an NMI pulse is generated every 64 microseconds (the length of time to produce a scan line on a PAL display). The NMI routine in ZX81-Forth (at address 0x066h) simply decrements a counter and, unless it reaches zero, returns to the previously running routine (either user code or the monitor's screen and multitasking code. The counter is held in the A' register, which is initiated to the desired number of scanlines for the border, during the previous phase (that is, Vsync generation or main-display generation).

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

NMI_DONE: 
	push hl 		; Save register

	ld hl,(NEXT_DISP_ROUTINE) ; Proceed to next routine (either
	jp (hl)			  ; RUN_VSYNC or RUN_DISPLAY)
```

Slightly unusually, a standard `ret` instruction is used to exit the NMI routine (wheras, more normally, a `retn` would be used). Using `ret` means the previousstate of the maskable interrupt is lost and the maskable interrupts is always disabled.

By default, ZX81-Forth sets the top border to have 31 lines and the bottom border to have 74 lines, so the main screen is not centred vertically. It has been suggested that the unequal borders are related to the way it was ported from the NTSC-compatible Tree Forth (adding rows to the bottom border to produce sufficient scanlines for a PAL display).

### Vsync Generation

The Vsync signal is controlled by the ZX81s ULA. The Vsync signal is turned on, from software, using `in (0xFE),a` and turned off using `out (<any_address>),a`. Other than activating and deactivating the signal, the Z80 has little to do during this phase. However, the Z80 is responsible for getting the timing right and on both ZX81 BASIC and ZX81-Forth, the time between the start and end of the Vsync signal is used to do something useful: to scan the keyboard. To work, the code to scan the keyboard has to take a fixed amount of time.

The Vsync routine in ZX81-Forth (starting at address 0x0098) first disables the NMI signal, then activates the Vsync signal. It then proceeds with a carefully timed sequence of instructions that, first of all, sets up the NMI paramaters (initial value of A' and NEXT_DISPLAY_ROUTINE) for the top border and then reads the keyboard. The combined sequence of actions takes 1,304--1,312 T states (about 402ms or 6 scanlines). This is a little shorter than te ideal Vsync of 7--8 scanlines.

After the Vsync signal is deactivated, the NMI signal is reenabled, to trigger the production of the top border of the display, though before returning to user code, the keyboard scanning routine is wrapped up and the multitasking scheduler is serviced. This further reduces the time available for user code.

### Generation of the main display

The most complicated part of display generation is the main screen area, in which text is displayed. This involves "executing" each row of the display as a carefully crafted instruction sequence that is terminated by a maskable interrupt.

I have put "executing" in quotes because this is not completely correct. The Z80 is instructed to run a row of the mirror of the display buffer (in the upper 32K of the ZX81 memory space) with a `jp` instruction. However, the Z80 never receives the character codes that are stored in the display: they are characters not instructions, so would be meaningless. Instead the Z80 receives `nop` instructions and the character codes are intercepted by other ZX81 hardware and turned into a display signal.

On the ZX81, when the Z80 tries to execute an instruction from an address in upper memory (address bit 15 is high) for which bit 6 of the byte at the address is low, the bytes is replaced by `nop`. The requirement on bit 6 being low is significant, as I will explain shortly. First, note that the ZX81's character set contains 64 characters with codes in the range of 0 to 63 (all of which have bit 6 low) plus inverted versions of these characters, between 128 and 191 (again, all of which have bit 6 low). 

While the Z80 receives a `nop` instruction, the actual byte read from memory is visible on part of the data bus connected to a character latch, which interprets it as a pixel pattern (a row of a character) to be displayed. This is done by creating an address, combined the contents of the lower seven bits of the I register, with the lower six bits of the byte read, and augmenting to a three-bit counter to produce a 15-bit address, as follows:

*   3-bit counter - A0--A2
*   lower six bits of character code - A3--A8
*   lower seven bits of I register - A9--A15

The sequence of eight pixels in the character row is read from that derived memory address and transmitted bit-by-bit to the screen in the 4 T states (1.2us) needed for the Z80 to execute `nop`.

ZX81-Forth stores the pixel patterns of the 64 characters it recognises at address 0x1E00. For example, row 0 of the space character (with character code 0) is stored at 0x1E00, row 1 is at 0x1E01, and so on. Then row 0 of the exclamation mark (with character code 1) is stored at 0x1E08, row 1 at 0x1E09, and so on. During startup, the I register is initialised to hold 0x1E.

The three-bit counter steps through 0, 1, ..., 7, incrementing once per scanline, so a character row of the display is produced by executing the same row of the display buffer eight times (with the 3-bit counter ensuring the correct pixel row is read and displayed).

For this mechanism to work, there needs to be a way to interrupt execution of the display buffer at the end of each character row. ZX81-Forth uses a different technique to ZX81 BASIC to do this, so we will briefly describe both approaches.

ZX81 BASIC relies on an newline character to terminate execution of a row of the display buffer. To accommodate this, the ZX81 BASIC display buffer allows up to 33 characters per line (up to 32 printable characters and a newline character). Because the new-line character (code 0x76) has bit 6 set, it is not substituted by `nop` but is passed to the Z80. Fortuitously(!), code 0x76 corresponds to the `halt` instructions, so causes the Z80 to stop and wait for a maskable interrupt. ZX81-Forth does not use a newline character but instead relies on every row of the display buffer having 32 characters.

Both ZX81 BASIC and ZX81-Forth then rely on careful timing to ensure that a maskable interrupt is generated at the appropriate time to end the scanline.

The ZX81 does not have a typical maskable interrupt. Instead, the interrupt line is configured so it is triggered whenever bit 6 of the refresh register R goes low. The Z80 refresh register is intended to support the use of dynamic RAM: exploiting the time when the Z80 is processing an instruction to put a dummy memory address on the bus and triggering a fake read. To step through the memory space, the R register is incremented after each instruction fetch. The ZX81 does not support DRAM and, so instead uses the R register to interrupt the Z80 at the end of each main-display scanline. Effectively, the R register is initiated to ensure the bit 6 of the register goes low when the Z80 gets to the 32nd character of the current display-buffer row.

The following code segment, from ZX81-Forth, shows how this works:
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

In ZX81-Forth (and ZX81 BASIC), interrupt mode 1 is used to execute the routine at address 0x38. This routine loops through the 24 character rows of the display (tracked in the C register) and eight pixel rows per character row (tracked in the B register), to generate the main display.

This maskable-interrupt routine is reproduced below (with timings measured in T states in brackets):

```
M_INT:	dec c			; (4) Decrement scan-line counter

	jp nz, NEXT_SCANLINE	; (10) Skip forward if more scan
				; lines to produce (Use JP to ensure
				; consistent timing with/ without a
				; branch)

	pop hl			; (10) Retrieve return address (next
				; character to execute in display
				; buffer)

	dec b			; (4) Decrement row counter

	ret z			; (11/5) Exit, if done

	set 3,c			; (8) Reset scan-line counter to 8

EXEC_DISPLAY:
	ld r,a			; (9) Reset refresh counter
	ei			; (4) Enable interrupts

	jp (hl) 		; (4) Execute next display line

NEXT_SCANLINE:
	pop de			; (10) Discard return address as we need
				; to re-run current row (address still
				; in HL))
	
	ret z			; (11/5) Timing-related: this is never
	 			; satisfied, so always adds 5 T states
	 			; delay 

	jr EXEC_DISPLAY		; (12) Continue to execute display line

```

When setting up to produce the main display, the BC pair is initialised to 24 rows and 9 pixels per row (one more than needed as B is decremented at the start of the interrupt).

There are two significant paths through the interrupt routine (the `jp` instruction is the branch point): one when progressing through a character row, and one when advancing to the next character row. It is very important that both paths take the same amount of time to complete, to provide a stable display. Using a `jp` rather than `jr` instruction and inserting a dummy `ret z` ensures both paths take 62 T states (20.3 us) to prepare to execute the display buffer. This time is reflected by the left-hand border on the screen display.

The maskable interrupt is also responsible for triggering the Hsync signal. This happens in response to the Z80's IORQ signal, which is asserted as part of the fetch cycle of the first instruction of the interrupt routine (the maskable interrupt takes 13 T states to assert). Minstrel 3 circuitry means a Hsync signal is enabled during the fetch cycle of the next instruction. The Hysnc signal is subsequently disabled during the fetch cycle of the fourth instruction.

This leads to the following scanline timing:

16 T states (Hsync) + 42 T states (left) +  128 (text) + 13+4+16  (interrupt and IORQ) = 207 T states



At the end of the main display, the NMI circuitry is enabled (having previously set it up to display the bottom border). The routine then exits (back to user code) and relies on the NMI circuitry to interrupt the Z80 at the end of each bottom-border display line.

### ZX81-Forth Timing

As noted above, the ZX81-Forth ROM does not produce a stable display on the Minstrel 3, so the first thing I wanted to do was check the timing of display generation in the ROM, based on the deduction above.

A ZX81-Forth frame consists of a VSync signal (taking 402us), 105 border lines (each taking 64us) and 192 main display lines, each taking 62.4us (based on 64+32*4+11 T states). This give a frame length of 18.1ms. Both the frame length and the main-display scanline length are shorter than idea, potentially leading to an undisplayable signal.

However, when investigating the ZX81-Forth display code, I also spotted that the Z80 is never set to interrupt mode 1. This means it will operate in Interrupt Mode 0, which retrieves and executes an instruction from the data bus when an interrupt is generated.

### 

8 scan lines = 512 us = 1,664 T states

Would like scanlines to be 12 T states longer

## Dictionary Layout

All Forth environments rely on a dictionary of words, which the user adds to to provide the functionality they need. On ZX81-Forth, the core of the dictionary is held in ROM and users add to it (with a colon-defined word, for example) in RAM.

A typical H Forth word has three components:
* It begins with a one-byte name-length-field, which is primarily used to hold the length of the word name. The lowest six bits hold the word-name length. The remaining tow bits are used to hold word properties (bit 6 indicates a task word and bit 7 indicates an immediate word).
* The name-length-field is followed immediately by the name (as a sequence of ASCII characters)
* Third, a two-byte, link field which records the offset to the next word in the dictionary (in a linked-list structure). This is used by the monitor to step through the dictionary -- e.g., when searching for a Forth word. 
* Fourth, the body of the word -- that is, its parameter field. For colon-defined words, this is a Z80 machine-code routine executed when the word is actioned.

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

## Multitasking

ZX81-Forth has a relatively sophisticated preemptive, multitasking capability, which allows you to schedule tasks to occur at certain times, with certain regularities, or as soon as possible.

Tasks are ordered in a linked list and on each frame (fifty or sixty times per second), the monitor will walk through the list and execute any tasks that are due.

The task list always contains at least one task, which is a monitor-service routine that will update the clock, check for out-of-memory error, and flash the cursor.

A task is described by a 13-byte structure, with the following elements:
- Bytes 0 and 1 are the link field pointing to the next task in the list (or holding 0x0000, if it is the end of the list).
- Bytes 2 through 5 contain a counter that is updated each frame and is used to determine when a task should be scheduled to run.
- Bytes 6 through 9 contain a base time, which is used to reset the counter when it has run down.
- Byte 10 is the task status byte. The lower six bits contain a backlog counter of task instances to be run. If the lower six bits are non-zero then the task is due to run (potentially multiple runs, if the scheduler is overloaded with tasks). Bit 6 is a flag indicating if the task is stopped. Bit 7 is a flag indicating if this and subsequent task in the list are locked (see manual for info).
- Bytes 11 and 12 contain a link to the code field of the Forth word that contains the action of the task.

The system task's data structure is held within the system variables (usually, starting at address 0x7C98 on Minstrel 3 (or 0xFC98 on ZX81). Subsequent tasks are held within Forth words in the dictionary, created with the `TASK` command.

Task words have bit 6 of their name-length field set. The task's data structure is the inserted int the parameter field of the task word.
