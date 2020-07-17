DLISTL  = $D402			; display list lo
DLISTH  = $D403			; display list hi
CHACTL  = $D401			; Character control
CHBASE  = $d409
CHBASE_DEF  = $d409

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
COUNTER = $3000

VVBLKD = $224
VDSLST = $200

; SETVBV = $e54c
NMIEN_DLI = $80
NMIEN_VBI = $40


  * = $4000
init
  ; disable interrupts
  sei
  ; lda	#$03			; point IRQ vector
	; sta	$200			; to BIOS routine
  ; lda	#$fc
	; sta	$201

  ; lda #<dli1
  ; sta $200
  ; lda #>dli1
  ; sta $201


	; lda	#$b8			; point VBI vector
	; sta	$202			; to BIOS routine
  ; lda	#$fc
  ; sta	$203

  lda #<vbi1
  sta $202
  lda #>vbi1
  sta $203


  lda	#$b2			; point deferred VBI
  sta	VVBLKD			; to BIOS routine
  lda	#$fc
	sta	VVBLKD+1

  lda #$ef      ; set FG color for mode 7
  sta COLOR0
  lda	#$0f			; set FG color for mode 3
  sta	COLOR1
  lda	#$80			; set FG color for mode 3
  sta	COLOR2

  lda	#<dlist			; set Display List Pointer
	sta	sDLISTL

  lda #>dlist
	sta	sDLISTH


  lda	#$f8			; set Character Set Base
	sta	CHBASE

  lda #$22
  ; sta sDMACTL
  sta sDMACTL

  ; lda	#$40			; enable NMI for VBI and DLI ($40 + $80)
  ; sta	NMIEN


  ; set the dli vector ?
  lda #$07  ; deferred
  ldx #<dli1
  ldy #>dli1
  brk
  jsr init_dli

forever
    jmp forever


init_dli
  ; load display list interrupt address
  sty VDSLST
  stx VDSLST + 1

  ; activate disiplay list interrupt
  lda #NMIEN_VBI | NMIEN_DLI
  sta NMIEN
  rts


    * = $9000
dli1
  pha
  inc COLPF0
  brk
  sta WSYNC

  pla
  ; rti
  jsr $fc03

vbi1
  ; inc COLBK
  brk
  ; rti
  jsr $fcb8
  rti
    * = $a000

dlist
    .byte $70,$70,$70   ; 24 blank lines                                    24
    .byte $c3           ; LMS mode 3                                        10
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
  .sbyte "    dli test 03     "

  * = $bffc
  .byte $57,$50

  * = $bffd
  .byte $FF

  * = $bffe
  .word init
