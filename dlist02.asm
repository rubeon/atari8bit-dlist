; Written in 2019 by Rob McMullen, https://playermissile.com/dli_tutorial/
; Copyright and related rights waived via CC0: https://creativecommons.org/publicdomain/zero/1.0/

 ; .include
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

; sDMACTL = TSTDAT
VSCROL  = $d405
VDSLST  = $0200
XITVBV   = $e462
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
; COLBK_SHADOW = $02c8

COLPF0   = $c016
COLPF1   = COLPF0+1
COLPF2   = COLPF0+2
COLPF3   = COLPF0+3

NMIEN_DLI = $80
NMIEN_VBI = $40
; color defs
BLACK   = $00
SALMON  = $38
SKY     = $ae
LIME    = $d8
UMBER   = $f8

INTTEMP = $0f

  * = $4000

  ; .include "hardware.s"
  ; .include "util_dli.s"

init
  sei     ; disable interrupts
  cld     ; clear decimal mode

  ldy #0
  cld



vectors
  ; lda #$40
  ; sta NMIEN
  ;
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
  lda #BLACK
  sta COLBK
  lda #$f8
  sta CHBASE
  lda #$22
  sta DMACTL
  lda #$40      ; enable NMI
  ; ora #NMIEN_DLI
  ; ora #NMIEN_VBI
  sta NMIEN
  lda #$22
  sta sDMACTL

  ; set the screen colors
  ; lda #UMBER
  ; sta COLBK

  lda #SKY
  sta COLOR0
  ;
  lda #$ff
  sta COLOR1
  ;
  lda #$08
  sta COLOR2

  ; lda #UMBER
  ; sta COLOR3
  ; lda #2

  lda #00
  sta COLOR4
  ; sta VSCROL
  lda #<dlist2
  sta sDLISTL
  lda #>dlist2
  sta sDLISTH


  ldx #0
forever
  ; inc COLBK
  ; inc COLOR2

  jmp forever




  * = $9000

dlist2
  .byte $70,$70,$70   ; 24 blank lines                                    24
  .byte $43          ; Mode 2 + LMS                                        8
  .word splash       ; set to RAM version of scoreline
  ; ; .word scoreline     ; with memeory address at $title
  .byte $70,$70,$70   ; 24 blank lines                                    24
  .byte $7            ;                                                   16
  .byte $70,$70,$70   ; 24 blank lines                                    24
  ; .byte $7,$7         ;                                                   32
  .byte $3,$3,$3      ;                                                   30
  .byte $2            ;                                                   16
  .byte $70,$70,$70   ; 24 blank lines                                    24
  .byte $7
  .byte $70,$70  ; 8 blank lines                                               8
  .byte $3,$3                  ;                                             20
  ; .byte $07,$07,$07,$07
  ; .byte $07,$07,$07,$07
  ; ; .byte $42,$00,$31
  .byte $02
  .byte $41
  .word dlist2



  * = $a000
dlist
  .byte $70,$70,$70  ; 24 blank lines
  .byte $46,        ; Mode 6 + LMS, setting screen memory to $title
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
  .byte $41          ;,<dlist,>dlist ; JVB, restart same display list on next frame
  .word dlist



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
splash
        ;"0123456789012345678901234567890123456789"
  .sbyte "          THIS 5200 PROGRAM BY          "
  .sbyte          "       EHW.IO!      "
  .sbyte $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
  .sbyte $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
  .sbyte $0e,$0e,$0e,$0e,$0e,$0e
  .sbyte "   IF YOU WOULD LIKE TO SEE MORE FROM   "
  .sbyte "        OUR TALENTED DEVELOPERS         "
  .sbyte "             VISIT OUR BBS              "
  .sbyte          "     RUBE'S NEST    "
  .sbyte "         404 926 5824 (ATLANTA)         "
  .sbyte "    1200/2400/9600 BAUD DOORS XMODEM    "


  * = $bfe8
carttitle
  ;       "0123456789012345   6   789"
  .sbyte  "    hello ehw",$4f,"io    "
  ; .sbyte  "    hello ehw io    "
  * =	$bffc
	.byte	$57,$50 ; "70"

  * =	$bffd
	.byte	$FF			; don't display atari logo
  * = $bffe
	.word	init			; start code at $4000
