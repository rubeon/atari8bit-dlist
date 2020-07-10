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

  * = $4000
init
  lda	#$03			; point IRQ vector
	sta	$200			; to BIOS routine
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

  lda #$ef      ; set FG color for mode 7
  sta COLOR0
  lda	#$0F			; set FG color for mode 3
  sta	COLOR1
  lda	#$80			; set FG color for mode 3
  sta	COLOR2

  lda	#<dlist			; set Display List Pointer
	sta	sDLISTL

  lda #>dlist
	sta	sDLISTH


  lda	#$F8			; set Character Set Base
	sta	CHBASE

  lda #$22
  ; sta sDMACTL
  sta sDMACTL

  lda	#$40			; enable NMI
  sta	NMIEN


forever

    jmp forever

    * = $a000
dlist
    .byte $70,$70,$70   ; 24 blank lines                                    24
    .byte $43,           ; LMS mode 3                                        10
    .word splash
    .byte $3,$3,$3   ; 4 x mode 3                                        40
    .byte $70,$70,$70
    .byte $7
    .byte $41
    .word dlist

    * = $b000
splash
    .sbyte  "          ANOTHER DLIST TEST! (1)       "
    .sbyte  "          ANOTHER DLIST TEST! (2)       "
    .sbyte  "          ANOTHER DLIST TEST! (3)       "
    .sbyte  "          ANOTHER DLIST TEST! (4)       "
    .sbyte  "   DLISTS ROCK!!!   "
  * = $bfe8
cartitle
  .sbyte "    dli test 02     "

  * = $bffc
  .byte $57,$50

  ; * = $bffd
  ; .byte $FF

  * = $bffe
  .word init
