# Running ZX81-Forth on the Minstrel 3

## Background

ZX81-Forth (also, sometimes known as Husband Forth) was an alternative ROM image for the ZX81, which replaced Sinclair BASIC with a Forth-based programming environment.

Forth was relatively popular in the early 1980s. Forth programs are compact and fast (when compared to BASIC), making Forth well-suited to the limited resources of a microcomputer. Better still, being ROM-based, ZX81-Forth does not take any precious RAM for the monitor code, so can be used with as little as 2 kB of RAM.

Released in early 1984, ZX81-Forth looks to be a particularly advanced Forth monitor which included preemptive, multi-tasking support -- something not common on microcomputers, at the time.

As acknowledged in the [manual](https://www.retrocomputers.gr/media/kunena/attachments/169/zx81-forth-manual.pdf), ZX81-Forth is a port of a Timex Sinclair 1000 ROM (referred to by several names, including [Tree Forth](https://www.timexsinclair.com/article/tree-forth/), Multi-Forth, and Pluri-Forth). Tree Forth was configured for an NTSC display, as was typically used on the TS1000. The port to ZX81-Forth made the monitor run on a PAL system (as used in the UK) and adjusted the clock and timing routines to work at 50 Hz. 

Versions of both the ZX81-Forth ROM and Tree Forth ROM images are distributed with the [EightyOne emulator](https://sourceforge.net/projects/eightyone-sinclair-emulator/). ROMs are also available from other sites, but they look to be identical to the versions distributed with EightyOne.

The Timex Sinclair website suggests there were several versions of Tree Forth. However, all the Tree Forth ROMs I have found on in the Internet are identical (and do not contain version information). Interestingly, the ZX81-Forth ROM, distributed with EightyOne, has a bug (in the implementation of `+-`), which is fixed in the Tree Forth ROM also distributed with EightyOne, suggesting the Tree Forth ROM is a later release than the ZX81-Forth ROM.

The Tynemouth Minstrel 3 is a modern-day, ZX81-compatible microcomputer with 32kB of RAM. I hoped to be able to run ZX81-Forth on the Minstrel 3 though, sadly, neither the ZX81-Forth nor the Tree Forth ROM images, distributed with EightyOne, run on the Minstrel 3. I wanted to understand why this was and whether I could get ZX81-Forth to work, so I began to disassemble and study the ROM.

![Tree Forth on a Minstrel 3](tree_forth.png)

My expectation was that the display-handling code was not (quite) compatible with the Minstrel 3. A [discussion on a retrocomputing forum](https://forum.tlienhard.com/phpBB3/viewtopic.php?t=1438) suggests that the ROMs distributed with EightyOne have been tweaked to work correctly with the emulator, so I thought these tweaks could have rendered the ROMs unusable on real hardware. (Note: In disassembling the ROM, I have found no evidence that the ROMS have been changed for EightyOne, and do not think this is the case.) However, as I explain below, the problems were somewhat more subtle than that.

I found four issues that stopped ZX81-Forth (and Tree Forth) from working, in their unmodified form, on the Minstrel 3:

* First, ZX81-Forth does not enable Z80 interrupt mode 1, which is critical to the correct operation of the ROM. The display routine in ZX81-Forth relies on the maskable interrupt routine at address 0x38 to display text and, to use this routine, the Z80 needs to be configured for `IM 1`. This puzzled me at first, as I did not understand how the monitor could work on the original ZX81 (when using the default interrupt mode 0). However, it turns out that the ZX81 has pull-up resistors fitted to the data-bus, so reports the value 0xFF on the data-bus when a maskable interrupt is generated: 0xFF is the Z80 opcode for `rst 0x38`, hence does the right thing. The Minstrel 3 does not have pull-up resistors on the data-bus (though could be modified to do so). To correct this, I inserted an `im 1` command into the initialisation code.

* Second, ZX81-Forth (usually) stores its state at the top of the Z80's address space (addresses 0xFB00--0xFFFF). On a ZX81 with at least 2kB of RAM, this will contain a shadow copy of the top five pages of the physical RAM. The ZX81 hardware intercepts attempts to execute code in upper memory as part of its display-handling code. However, it does not intercept read and write operations, so these complete as expected. In contrast, the Minstrel 3 supports read and write operations to addresses up to 0x9FFF (which is the end of the 32K RAM). Read and write to the top of the address space (specifically, 0xFB00--0xFFFF) is not possible, and so caused ZX81-Forth to fail. To fix this, I refactored the monitor code so that it used only the lower half of memory, with state information being located in 0x7B00--0x7FFF.

* Third, ZX81-Forth tries to compute how much RAM a computer has at boot time, to allow it to accommodate a range of RAM configurations. The routine effectively counts down from the top of memory (0xFFFF) until it finds the end of the last-but-one shadow copy of RAM, and relies on memory being mirror across the address space in a certain way. However, because of the way memory on the Minstrel 3 is configured (same as the default configuration of the ZXpand) and because of the memory-access difference noted in the previous point, the routine reports the wrong amount of RAM. This is also a problem when using the ZXpand and may mean ZX81-Forth will not work with a ZXpand connected (I have not checked this). Of course, neither the Minstrel 3 nor the ZXpand existed when ZX81-Forth was developed, so this is understandable. I have addressed this by hardwiring the value for upper memory to be 16kB (which, plus the RAM in 0x2000--0x3FFF) means ZX81-Forth on the Minstrel 3 has 24kB of RAM to work with, in the range 0x2000--0x7FFF. The dictionary grows up from 0x2000 and the system state is stored in 0x7B00--0x7FFF.

* Finally, ZX81-Forth uses the Forth parameter stack before it is initialised. The Parameter Stack grows down in memory from 0xFB80 (original) or 0x7B80 (my port) and the address of the top (actually, bottom) of the stack is held in the IY index register. In the original ROM, the Parameter Stack is used before IY is initialised, meaning IY holds the value 0x0000 and values are stored at end of the address space -- at 0xFFFE, 0xFFFC, etc. On the ZX81, this leads to a momentary corruption of the display buffer, but is otherwise okay. On the Minstrel 3, accesses to addresses on or above 0xA000 are intercepted by the display hardware, so are not supported. I have added an initialisation of the IY register to the early part of the restart routine, before the Parameter Stack is used, to address this.

I also discovered that the ZX81-Forth display routine is a simplified version of what is used in Sinclair BASIC. Most notably, the buffer holds 32 bytes per row and does not use an end-of-line `halt` character. Having 32 bytes per display row makes it possible to optimise display-handling code but also means there is no real way to encode time to generate the right-hand border of the display. This can lead to some corruption at the right end of each display line, though the severity depends on the specific monitor you are using.  Whilst the display routine works as well on the Minstrel 3 as it does on the ZX81, I do not think it is a very good match to the PAL standard (neither is the Tree Forth version for the NTSC standard). As well as the end-of-line issue noted above, the code generates too few scanlines, so the frame duration is shorter than it should be. I addressed the frame-length issue by increasing the border slightly, in the hope of producing a more stable display. This also has the advantage of centring the ZX81-Forth text display (on the original it is shifted noticably upwards). I have yet to tackle the right-hand border issue, as this requires more significant reworking. At present, the right-hand end of each line of text is likely to be inverted and may be corrupted. This does not impact on the functionality of the monitor, though does slightly affect usability.

## Running ZX81-Forth on a Minstrel 3

You can run either ZX81-Forth or Tree Forth on the Minstrel 3, by copying one of the ROM images [zx-forth_pal.rom](zx-forth_pal.rom) or [zx-forth_ntsc.rom](zx-forth_ntsc.rom) onto a suitable EPROM and installing it in your Minstrel 3.

Make sure to set the display jumper for either NTSC or PAL, as appropriate to your chosen ROM, and the address-bus jumpers to expose the correct 16kB RAM block. Having done this, you should be able to boot into the Forth system console screen and start using the system.

You can download a PDF copy of the [ZX81-Forth user guide](https://www.retrocomputers.gr/media/kunena/attachments/169/zx81-forth-manual.pdf) (which is also suitable for Tree Forth)

## Building ZX81-Forth from Source

It is possible to build the ROM from source, should you wish. I have created a disassembly of the ZX81-Forth ROM from EightyOne, have generalised it to support several different platforms, and am in the process of documenting the source.

I have used generic Z80 assembly language for this, so it should be possible to assemble with most Z80 assemblers. I use the [z80asm](https://www.nongnu.org/z80asm/).

Near the start of the source code, you will find four variables that can be used to create different versions of the ROM:
* The variable `MINSTREL3` (set to zero or one) controls whether or not to patch the ROM for the Minstrel 3
* The variable `NTSC` (set to zero or one) determines whether to assemble for PAL displays (ZX81-Forth variant) or for NTSC displays (Tree Forth original).
* The `FIXBUG` variable (set to zero or one) determines whether or not to fix several bugs or oddities that I have found (see next section for details).
* The variable MINSTREL4 (set to zero or one) determines whether or not to patch the ROM to work on the Minstrel 4th, which is a Jupiter Ace compatible board (see below for more details).

Note that you  should set at most one of the `MINSTREL3` and `MINSTREL4` variables, when assembling the source.

The original ROMs (as shipped with EightyOne) can be produced by setting `MINSTREL3=0`, `MINSTREL4=0`, `FIXBUG=0`, and then choose the value of `NTSC` for ZX81-Forth (with `NTSC=0`) or Tree Forth (with `NTSC=1`). 

Happy Forth programming!

## Bugs and Anomolies

I have discovered a few bugs and anomolies in the course of disassembling the ROM and, where reasonable to do so, I have tried to fix these. You can accept my fixes and changes by setting `FIXBUG=1` when assembling from source.

The bugs and anomolies I have found, so far, are:
* As noted above there is a bug in the implementation of `+-`, which means it does not work with a positive modifier. For example `2 3 +- .` will throw a stack error. The Tree Forth ROM already has a fix for this, which I have copied over to the ZX81-Forth ROM.
* The word `0>` is not implemented as you might expect. It will report `1` for any non-negative value, including zero. Usually, `0>` would report `0` in response to zero.
* On ZX81-Forth, the screen is not centred vertically and has a very small top margin. This looks to be an artefact of how the ROM was converted from NTSC to PAL, requiring a reduction in the number of scan lines. I have changed the border sizes, so the ZX81-Forth display is properly centred.
* The implementation of `BEGIN`, `WHILE`, `REPEAT` is non-standard in that the `WHILE` condition will be satisfied only if a zero is on the Top of Stack (whereas the standard specifies the code should repeat if TOS is non-zero). This is confusing for experienced Forth programmers. I have corrected the behaviour though note this means that examples in the user guide that use While loops need to be updated accordingly.
* When first activating the Editor screen (using Shift+1), if the cursor is in the lower part of the screen, then a memory location a little beyond the end of the display is written to incorrectly. On the ZX81, this memory location is almost always likely  to be a shadow copy of ROM, so has no effect. On the Minstrel 4th, it is likely to be in the Character RAM, so will lead to corruption of the font. I have fixed this bug only in the Minstrel 4th version, as it requires extra code, which I have located in the extended ROM area on the Minstrel 4th. As noted, on the ZX81 and Minstrel 3, the bug should not have any significant effect.
* When switching between the Editor screen and the Console screen, it is possible that a cursor blob will be left in the Editor screen. If you then `STORE` the screen to cassette, the cursor blob is also stored. This does not have an affect, as the blob is treated as a Space character by the compiler, though I find it distracting. I have not yet fixed this issue.


## ZX81-Forth Disassembly

I am in the process of disassembling ZX81-Forth, in part because it seems a genuinely interesting piece of software to study and in part because I would like to try to run it on other Z80-based systems.

You can get a sense of my progress by browsing the [source code](zx-forth.asm). I have worked through around three quarters of the program, though my understanding has grown as I have progressed, so some of my earlier comments may be ambiguous or even wrong. It will take a couple more iterations to get a reasonable and complete, commented assembly-language source code.

In addition, I have described below some of what I think are the key elements of ZX81-Forth, to help people follow the source code. Again, this is a work in progress.

### ZX81-Forth High-level Flowchart

I have sketched out a flowchart that explains how I think ZX81-Forth works.

![High-level flow of the monitor](zx-forth_flowchart.png)

As with ZX81 BASIC, quite a lot of time is spent producing a screen display. This is time-critical and has to, effectively, be the controlling process.

During the top and border generation, the Z80 is mostly idle and this is where the monitor code and user code runs. The monitor is reasonably complex and likely to take a reasonable amount of this idle time, leaving less time for actual user code. Further, the monitor runs in two different contexts (with different machine stacks and register contents).

The monitor starts in what I have called Context 0. This is where user code is actually executed, either actioning Forth words and numbers entered into the Console window or scheduled by the multitasker. Alongside Context 0 is Context 1 which handles some of the monitor's house-keeping tasks, such as checking stack underflow, running the background task and, when the Console is in focus, parsing keyboard input.

As with ZX81 BASIC, the keyboard is read as part of the Vsync routine. It runs in a near-constant number of clock cycles and, if a key is pressed, it is either added to a keyboard input buffer (if the Console is in focus) or is printed to the screen, if the Editor is in focus. Key presses are actually processed in Context 1, asynchronous from the keyboard scanning routine. Processing key presses effetively equates to reading in tokens (strings of ASCII characters representing either Forth words of numbers). When a complete token has been received (indicated by a space character or newline character), control is passed back to Context 0 which then processes the token (running a Forth word or adding a number to the Parameter Stack). The creation of new dictionary entries using compiling words, such as `:`,  is handled in Context 0 as part of the execution of the corresponding compiling word.

Multitasking is achieved via a scheduler which runs at the end of each frame's Vsync signal is generated. If any tasks are due to run, these are handled by a temporary switch to Context 0, temporarily interrupting the parsing of tokens (Context 0) and the monitor's housekeeping functions (Context 1).

Because it is a multitasking monitor, it is not possible to describe a linear path through the monitor code. The monitor switches between different tasks constantly and there are various system variables that support locking to prevent race conditions between conflicting monitor actions.


### ZX81-Forth Display Handling

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

## Dictionary Layout

All Forth environments rely on a dictionary of words, which the user adds to to provide the functionality they need. On ZX81-Forth, the core of the dictionary is held in ROM and users add to it (with a colon-defined word, for example) in RAM.

A typical ZX-Forth word has three components:
* It begins with a one-byte name-length-field, which is primarily used to hold the length of the word name. The lowest six bits hold the word-name length. The remaining two bits are used to hold word properties (bit 6 indicates a task word and bit 7 indicates an immediate word).
* The name-length-field is followed immediately by the name (as a sequence of ASCII characters).
* Third, a two-byte, link field which records the offset to the next word in the dictionary (in a linked-list structure). This is used by the monitor to step through the dictionary -- e.g., when searching for a Forth word. 
* Fourth, the body of the word -- that is, its parameter field. For colon-defined words, this is a Z80 machine-code routine executed when the word is actioned.

A task word is slightly more complicate. It has four components:
* It begins with a one-byte name-length-field, like a colon definition.
* The name-length-field is followed immediately by the name.
* Third, a two-byte, word-length field which records the total size of the word in memory.
* Fourth, the address of the parameter field is stored, which is later in the definition
* Fifth is a sixteen-byte data structure containing key information about the task.
* Finally, there is the parameter field, which contains a link to the parameter field of the word that the task actions.

Further, the beginning (tail) and end (head) of the dictionary are stored in two system variables, at address 0x7C88 and 0x7C8A, respectively. New words are added to the head of the dictionary.

The ROM part of the dictionary is actually backwards. The last word in the user's dictionary has a word-length field that effectively points to the head of the ROM dictionary -- that is, to `M*` -- which is the first word in the dictionary.

## Character Stack

In the default memory configuration, the character stack is located between FCC0h and FCFFh (that is, space for 64 characters) and grows down from FCFFh. The top of stack is held in a one-byte system-variable at FC84h (label `PSTACK_C`) and initially contains the value C0h.

The restart routines at 18h and 20h are used to push/ pop characters onto/ off the stack, respectively. There are also more advanced routines, which use character stack, including:

* PUSH_STRING (0637h) - which pushes a countable string in memory onto character stack (also pushing length of string onto parameter stack)

* PRINT_STRING (0652h) - Print string from character stack (with length on parameter stack) to current screen

* MATCH STRING (06A7h)

## Multitasking

ZX81-Forth has a relatively sophisticated, preemptive, multitasking capability, which allows you to schedule tasks to occur at certain times, with certain regularities, or as soon as possible.

Tasks are ordered in a linked list. On each frame (fifty or sixty times per second), the monitor will walk through the list and execute any tasks that are due.

The task list always contains at least one task, which is a monitor-service routine that will update the clock, check for an out-of-memory error, and flash the cursor.

A task is described by a 13-byte structure, with the following elements:
- Bytes 0 and 1 are the link field pointing to the next task in the list (or holding 0x0000, if the task is at the end of the list).
- Bytes 2 through 5 contain a counter that is incremented each frame and is used to determine when a task is due to run. This happens when the counter rolls over to 0x00000000, which causes the task status counter to be incremented.
- Bytes 6 through 9 contain a base time, which is used to reset the counter when it has rolled over.
- Byte 10 is the task status byte. The lower six bits contain a backlog counter of task instances to be run. If the lower six bits are non-zero then the task is due to run (potentially multiple runs, if the scheduler is overloaded with tasks). Bit 6 is a flag indicating if the task is stopped. Bit 7 is a flag indicating if this and subsequent task in the list are locked (see manual for info).
- Bytes 11 and 12 contain a link to the code field of the Forth word that contains the action of the task.

The system task's data structure is held within the system variables (usually, starting at address 0x7C98 on the Minstrel 3 (or address 0xFC98 on ZX81). Subsequent tasks are held within Forth words in the dictionary, created with the `TASK` command.

Task words have bit 6 of their name-length field set. The task's data structure is then inserted into the parameter field of the task word.

## Cassette storage

ZX-Forth allows the user to save the contents of the Editor screen  o cassette for future use. The format used for this is specific to ZX-Forth and consists of the following elements:

- A leader tone of 7Dh zero bytes
- Identifier byte (has value 0xA5), which is used to check for a ZX-Forth file during loading.
- Screen number (as specified by the user (2 bytes)
- Screen contents (512 bytes)
- Another leader tone of 7Dh zero bytes

When loading back from tape, ZX-Forth searchers for the identifier byte 0xA5 and then loads and checks the screen number. If all is as should be, it then populates the screen from the cassette buffer. If auto-compile is enabled, the screen is compiled once loaded.

Note that the Editor Screen must be visible for `LOAD` to work and that the Editor Screen is cleared at the beginning of the `LOAD` operation, so any previous content will be lost. 

# Porting ZX-Forth to Minstrel 4th

ZX-Forth is designed to run on the ZX81, which has many similarities to the Minstrel 4th (a modern-day Jupiter Ace compatible). In some ways, the Jupiter Ace was  (and, hence, Minstrel 4th is) a successor to the ZX81. Notable for here, the Ace (and 4th) does not require significant time from the Z80 to generate a display signal and has an improved tape-storage library. Potentially, ZX-Forth would be an excellent candidate to run on a Jupiter Ace (Minstrel 4th), with significantly improved performance and more reliable screen handling, when compared to the ZX81.

It transpired that a basic port to the Minstrel 4th is relatively straightforward, involving the following high-level tasks:
- The display buffer needs to be located at 0x2400 (which is where Ace hardware expects to find it). The display buffer format is almost suitable already, though the Ace display expects characters to be stored with their ASCII encoding, whereas on the ZX81 the characters are encoded with ASCII code - 0x20.
- During initialisation, the character RAM needs to be populated.
- The memory checking code needs to be updated, to reflect the different memory layout on the Ace (upper memory is not a copy of lower memory and addresses during 0x2000-0x3FFF can not be used as general-purpose RAM).
- The display-generation code, used for ZX81, needs to be disabled.
- The keyboard/ multitasking routine needs to be triggered appropriately - that is, using the maskable interrupt.

Note that the current port only works on the Minstrel 4th (and not on a Jupiter Ace). My port relies on the extra ROM at address 0x2800--0x4000 to hold a new tape interface, plus other extra routines. I do not expect this is a significant limitation, since it is unlikely anyone who has an original Jupiter Ace would want to swap out the ROM.

The ZX-Forth tape storage routines have been replaced by the Ace Forth equivalents. The Ace Forth routines are, I believe, much more robust than the Ace Forth ones. This means you cannot share tapes between a ZX81 and a Minstrel 4th though you can share files between ZX-Forth on the Minstrel 4th and Ace Forth (screens are saved a code blocks, which can then be loaded into Ace Forth using `BLOAD`).


## Differences

I think ZX-Forth runs well on the Minstrel 4th, but there are a few differences to be aware of:
- The key meanings (especially, some of the symbols) are different on ZX-Forth from Ace Forth, so you can not rely on the keyboard legends for some characters. If in doubt, consult the final page of the ZX81-Forth [manual](https://www.retrocomputers.gr/media/kunena/attachments/169/zx81-forth-manual.pdf), which lists all key presses. Also, there is no Period key on the Minstrel 4th. The Minstrel 4th's Symbol Shift key (which is not needed is mapped to the ZX81's period key).
- The `SLOW`, `FAST`, and `AUTO` words are implemented but do nothing on the Minstrel 4th.
- As noted above, `LOAD` and `STORE` work with Jupiter Ace tape-file format. Specifically, screens are stored as binary code blocks, which could be loaded into Ace Forth with `BLOAD` though can not be intepretted by Ace Forth.

## Speed

As noted, a potential advantage of running on the Minstrel 4th is the speed advantage (as the Minstrel 4th does not need time from the Z80 to display the screen). To test how significant this speedup is, I have written a simple benchmark based on the Sieve of Eratosthenes:

```
: ?PRIME HERE + C@ 0= ;
: COMPOSITE! HERE + 1 SWAP C! ;
: 2DUP OVER OVER ;
: SIEVE HERE OVER 0 FILL
  2 BEGIN
    2DUP DUP * > WHILE
      DUP ?PRIME IF
        2DUP DUP DO
          I COMPOSITE! DUP +LOOP
      THEN 1 +
    REPEAT
  DROP ." PRIMES: " 2 DO
    I ?PRIME IF I . THEN LOOP
;
```

Using thie program to compute all prime numbers in the first 1,000 integers takes 11 seconds on the ZX81 (slow) or 4 second in auto mode. On the Minstrel 4th, the same benchmark takes a little under 3 seconds.

