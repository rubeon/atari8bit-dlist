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

COLBK	= $c01a
COLBK_SHADOW = $02C8
; colors?
COLPF0 = $c016
COLPF1 = COLPF0+1
COLPF2 = COLPF0+2
COLPF3 = COLPF0+3

;COLOR0 = $02c4


  * = $4000
init

  sei     ; disable interrupts
  cld     ; clear decimal mode
  ldy	#0
	cld

bgset
  	lda	#$08
	sta 	COLBK
  	;sta	COLBK_SHADOW


  lda #<dlist
  sta sDLISTH

  lda #>dlist
  sta sDLISTL
  lda	#$22			; Enable DMA
	sta	sDMACTL

	lda	#$40			; enable NMI
	sta	NMIEN

  lda	#$03			; point IRQ vector
	sta	$200			; to BIOS routine
  lda	#$FC
	sta	$201
  lda	#$FC
	sta	$201
	lda	#$B8			; point VBI vector
	sta	$202			; to BIOS routine

  lda	#$FC
  sta	$203
  lda	#$B2			; point deferred VBI
  sta	$204			; to BIOS routine
  lda	#$FC
	sta	$205
  lda	#$03
	sta	CHACTL			; set Character Control
  lda	#$84			; set color PF2
	sta	COLOR2
;	sta	COLPF1

	lda	#$0F			; set color PF1
;	sta	COLOR1
 	 sta	COLOR1
  lda	#$c8
;	sta COLPF0
	sta COLOR3
;	sta COLPF1
;	sta COLPF2
;	sta COLPF3

  lda	#$00
	sta COLOR0

  lda	#00			; set Display List Pointer
	sta	sDLISTL

  lda #$a0
	sta	sDLISTH

  lda	#$F8			; set Character Set Base
	sta	CHBASE

  lda	#$22			; Enable DMA
	sta	sDMACTL

  lda	#$40			; enable NMI
	sta	NMIEN

  ldy	#0
	cld



forever
  jmp forever

  * = $a000

dlist
  .byte $70,$70,$70  ; 24 blank lines
  .byte $46,        ; Mode 6 + LMS, setting screen memory to $4000
  .word title
  .byte 6            ; Mode 6
  .byte $70          ; 8 blank lines
  .byte $07,$07,$07,$07,$07; 5 lines of Mode 7
  .byte $70          ; 8 blank lines
  .byte 2            ; single line of Mode 2
  .byte $70,$70,$70  ; 24 blank lines
  .byte 2,4          ; Mode 2 followed by mode 4
  .byte $70          ; 8 blank lines
  .byte 2,5          ; Mode 2 followed by mode 5
  .byte $41,<dlist,>dlist ; JVB, restart same display list on next frame

  ; .word dlist

  * = $b000
title
  ; .sbyte "             THIS IS A LINE             "
  .sbyte "   player"                                  ; 9
  .byte $4f ; slash using lower-case color              10
  .sbyte           "missile   "                       ; 20
  .sbyte "  podcast presents  "                       ; 40
  .sbyte +$c0,"   A CRASH COURSE   "                       ; 60
  .sbyte +$c0,"         ON         "                       ; 80
  .sbyte +$c0,"      ADVANCED      "                       ; 100
  .sbyte +$c0,"    DISPLAY LIST    "                       ; 120
  .sbyte +$c0,"     INTERRUPTS     "                       ; 140
  ;       0123456789012345678901234567890123456789
  .sbyte " Available at http://playermissile.com  "   ; 180

  .sbyte " Here's some ANTIC mode 4:              "   ; 220
  .sbyte "0123456789012345678901234567890123456789"   ; 260

  .sbyte " And here's some ANTIC mode 5:          "   ; 280
  .sbyte "0123456789012345678901234567890123456789"   ; 300

  * = $b900
scoreline
  .sbyte "PLAYER 1:              PLAYER 2:        "

  * = $bfe8
carttitle
  ;       "01234567890123456789"
  .sbyte  "    hello ehw io    "
  ; .sbyte  "    hello ehw io    "
  * =	$bffc
	.byte	$57,$ff ; "70"

  * = $bffe
	.word	init			; start code at $4000
