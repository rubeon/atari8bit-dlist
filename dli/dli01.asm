DLISTL  = $d402			; display list lo
DLISTH  = $d403			; display list hi
CHACTL  = $d401			; Character control
CHBASE  = $d409
sDLISTL = $05       ; dlist low shadow
sDLISTH = $06       ; dlist high shadow

DMACTL  = $d400			; DMA control
sDMACTL	= $07			  ; DMA Control Shadow

NMIEN   = $d40e			; NMI enable
WSYNC   =	$d40a     ;

COLOR0  = $0c
COLOR1  = COLOR0+1
COLOR2  = COLOR0+2
COLOR3  = COLOR0+3
COLOR4  = COLOR0+4

PCOLR0   = $02c0
PCOLR1   = $02c1
PCOLR2   = $02c2
PCOLR3   = $02c3

COLPM0   = $c012
COLPM1   = $c013
COLPM2   = $c014
COLPM3   = $c015

COLBK	= $c01a

COLPF0   = $c016
COLPF1   = COLPF0+1
COLPF2   = COLPF0+2
COLPF3   = COLPF0+3

NMIEN_DLI = $80
NMIEN_VBI = $40

BLACK   = $00
SALMON  = $38
SKY     = $ae
LIME    = $d8
UMBER   = $f8

  * = $4000

init
  sei
  cld
  lda #0
  ldx #0
  ldy #0
  lda #BLACK
  sta COLBK


vectors
  lda #$03    ; point IRQ vector
  sta $200    ; to BIOS routine
  lda #$fc
  sta $201

  lda #$b8    ; point VBI vector
  sta $202    ; to BIOS routine
  lda #$fc
  sta $203

  lda #$b2    ; point deferred VBI
  sta $204    ; to BIOS routine
  lda #$fc
  sta $205



dma
  lda #$03
  sta CHACTL
colset
  lda #$08
  sta COLBK
  lda #$00
  sta COLOR0
  lda #$84
  sta COLOR1
  lda #$c8
  sta COLOR3
  lda #SKY
  sta COLOR4

dlist_setup
  lda #>dlist
  sta sDLISTL
  lda #<dlist
  sta sDLISTH

  lda #$f8
  sta CHBASE
  lda #$22
  sta sDMACTL

nmi_setup
  lda #$40
  sta NMIEN
  ; lda #$22
  ; sta sDMACTL
  cld


forever
  ; inc COLOR0
  inc COLOR1
  ; inc COLOR2
  ; inc COLOR3
  ; inc COLOR4
  ; inc COLPF0
  ; inc COLPF1
  ; inc COLPF2
  ; inc COLPF3
  ; inc COLBK
  jmp forever


  * = $9000

dlist
    .byte $70,$70,$70   ; 24 blank lines                                    24
    .byte $46,           ; LMS mode 4                                        10
    .word splash
    .byte $3,$3,$3   ; 4 x mode 3                                        40
    .byte $70,$70,$70
    .byte $41
    .word dlist

  * = $b000


splash
    .sbyte  "          ANOTHER DLIST TEST! (1)       "
    .sbyte  "          ANOTHER DLIST TEST! (2)       "
    .sbyte  "          ANOTHER DLIST TEST! (3)       "
    .sbyte  "          ANOTHER DLIST TEST! (4)       "


  * = $bfe8
cartitle
  .sbyte "    DLI TEST 01     "

  * = $bffc
  .byte $57,$50

  * = $bffd
  .byte $FF

  * = $bffe
  .word init
