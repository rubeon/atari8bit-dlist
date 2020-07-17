# Equates

```
DLISTL  = $D402			; display list lo
DLISTH  = $D403			; display list hi
CHACTL  = $D401			; Character control
CHBASE  = $d409
sDLISTL = $05
sDLISTH = $06
DMACTL  = $D400			; DMA control
sDMACTL	= $07			; DMA Control Shadow
NMIEN   = $D40E			; NMI enable
WSYNC   =	$D40A
COLOR0  = 	$0c
COLOR1  =	$0d			; Color 1 shadow
COLOR2  =	$0e			; Color 2 shadow
COLOR3	=	$0f
```

# Main

at ORG $4000 (5200 cart)

* Point IRQ vectors to BIOS routines:

BIOS routine = $fc03
IRQ vector is stored at $200,$201

```
lda	#$03			; point IRQ vector
sta	$200			; to BIOS routine
lda	#$FC
sta	$201
```

* Point VBI vector at the BIOS routine

BIOS routine = $fcb8
VBI vector is stored at $202,$203

```
lda	#$B8			; point VBI vector
sta	$202			; to BIOS routine
lda	#$FC
sta	$203
```

* Point deferred VBI  to BIOS routine

BIOS routine = $fcb2
Deferred VBI vector is stored at $204,$205

```
lda	#$B2			; point deferred VBI
sta	$204			; to BIOS routine
lda	#$FC
sta	$205
```

* Set foreground color (COLOR1), background color (COLOR2) for mode 3, and the foreground color for mode 7 (COLOR0)

```
lda #$ef
sta COLOR0
lda	#$0F
sta	COLOR1
lda	#$84
sta	COLOR2
```

* Set the the shadow DLIST (sDLISTL, sDLISTH) pointers to the display list (called dlist)

```
lda	#<dlist			; set Display List Pointer
sta	sDLISTL

lda #>dlist
sta	sDLISTH

```
* Set the character set base address to the default ($f8)

```
lda	#$F8			; set Character Set Base
sta	CHBASE
```
* turn on DMA via the shadow register
```
7 - Unused
6 - Unused
5 - Display List DMA          ; $0 no DL, $20 DL
4 - Player Missile Resolution ; $0 double , $10 single
3 - Player DMA                ; $0 Disable P/M, $4 missile
2 - Missile DMA               ; $8 Player, $11 Player and Missile
1 - Playfield Width           ; $0 disable playfield, $1 Narrow
0 - Playfield Width           ; $2 Normal (160 cclk), $3 Wide (192 cclk)

$22 = 0b00100010 -> DLIST + normal width
$2e = 0b00101110 -> DLIST + normal width + missile DMA + player DMA + PM double res
```


lda #$22
sta sDMACTL

* Turn on NMI interrupts for VBI ($40, or %01000000)
```
Reset	$20	Enable Reset key interrupt
VBI	  $40	Enable Vertical Blank Interrupt
DLI	  $80	Enable Display List Interrupt
```


```
lda #$40
sta NMIEN
```
* Enter a forever loop

```
forever
    jmp forever

```

# Data

Pick an origin in the ROM space ($4000-$bfff), and define your dlist and its data source(s) in there.

For example:

at Origin $a000, define the dlist

```
dlist
    .byte $70,$70,$70
    .byte $43
    .word splash
    .byte $3,$3,$3
    .byte $70,$70,$70
    .byte $7
    .byte $41
    .word dlist

```

At origin $b000, put the "splash" label with some appropriately-formatted strings (check the mode line specs for widths)

```

splash
  .sbyte  "          ANOTHER DLIST TEST! (1)       "
  .sbyte  "          ANOTHER DLIST TEST! (2)       "
  .sbyte  "          ANOTHER DLIST TEST! (3)       "
  .sbyte  "          ANOTHER DLIST TEST! (4)       "
  .sbyte             "   DLISTS ROCK!!!   "

```

# Display list interrupts

The display list mode byte determines if a DLI is triggered:

Bits 7:4 are modifiers for Playfield Mode instructions in bits 3:0. Bit value 1 Enables the modifier, and 0 disables the modifier.

```
Bit 3:Bit 0 - Playfield Mode Instruction.
Values $00, and $01 are special instructions.
Mode values $02 through $0F specify Playfield Character and Map modes.
Bit 4 - $10 - Horizontal Scroll.
Bit 5 - $20 - Vertical Scroll.
Bit 6 - $40 - Load Memory Scan.
Bit 7 - $80 - Display List Interrupt.

vscroll with DLI and LMS:
$20 + $40 + $80 =

``


# Atari 5200 housekeeping

The 5200 looks to $bfe8 to set the title, logo screen display, and entry point:

```
; title is 20 bytes wide
* = $bfe8
cartitle
  .sbyte "    DLI TEST 02     "

```

Then it looks to $bfcc for the Copyright Date

```
* = $bffc
  .byte $57,$50  ; $57='7', $50='0'
                 ; display copyright 1970
```

Add the following to overwrite $bffd with $ff (disables atari logo on startup):

```
* = $bffd
  .byte $ff
```
Finally, set the entry point

```
* = $bffe
  .word init
```

# Display Lists

Display list modes are defined in a .byte for each mode (and pseudo-modes)




Mode      FGCOLOR     BGCOLOR
2         COLOR1      COLOR2
3         

Antic
Mode	    Basic
Mode	           Colors	         Lines	    Width	      Bytes per
Line	           Screen Ram
(Bytes)
2	        0	     2	8	40	40	960
3	        N/A	   2	10	40	40	760
4	        N/A	   4	8	40	40	960
5	        N/A	   4	16	40	40	480
6	        1	     5	      8	20	20	480
7	        2	5	16	20	20	240
8	        3	4	8	40	10	240
9	        4	2	4	80	10	480
A	        5	4	4	80	20	960
B	        6	2	2	160	20	1920
C	        N/A	2	1	160	20	3840
D	        7	4	2	160	40	3840
E	        N/A	4	1	160	40	7680
F	        8	2	1	320	40	7680

Display Mode Line
```
7 6 5 4 3 2 1 0
-----------------
|I|R|H|V|M|M|M|M|
-----------------
 | | | | \      /
 | | | |  ------
 | | | |    |
 | | | |    2-F = display one line of graphics in
 | | | |          ANTIC mode 2-F
 | | | 1 = horizontal scroll enabled
 | | |
 | | 1 = vertical scroll enabled
 | |
 | 1 = reload memory scan counter with next two bytes
 |
 1 = display list interrupt, all instructions

```
```
7 6 5 4 3 2 1 0
-----------------
|I|n|n|n|0|0|0|0|
-----------------
  \   / \      /
   ---   ------
    |      |
    |      0 = display blank lines
    |
    0-7 = number of blank lines (1-8)

```

```
7 6 5 4 3 2 1 0
-----------------
|I|W| | |0|0|0|1|
-----------------
   |     \      /
   |      ------
   |        |
   |        1 = jump (3 byte instruction)
   |
   0 = jump and display one blank line
   1 = jump and wait for vertical blank
```

# VBI
Writing a deferred VBI vector:
