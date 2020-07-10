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

at ORG 4000 (5200 cart)

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

lda #$22
sta sDMACTL

* Turn on NMI interrupts for VBI ($40, or %01000000)
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
