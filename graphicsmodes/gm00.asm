  .include 5200.s

MODETEXT=$3000
SCROLLV=2 ; speed of scrolling
  * = $4000

init
  lda #$03          ; IRQ vector
  sta $200
  lda #$fc
  sta $201

  lda #$b8          ; vbi vector
  sta $202
  lda #$fc
  sta $203

  lda #$b2
  sta $204
  lda #$fc
  sta $205

  ; set foreground color
  ; lda #$ef
  ; sta COLOR0
  lda #$0f
  sta COLOR1
  lda #$03
  sta COLOR2
  sta COLPF4
  ; lda #$84
  ; sta COLOR3
  ; sta COLPF0
  ; sta COLPF1
  ; sta COLPF2
  ; sta COLPF3

  ; shadow DLISTH/L
  lda #<dlist1
  sta sDLISTL
  lda #>dlist1
  sta sDLISTH

  ; default CHBASE
  lda #$f8
  sta $D409

  ; flip on DMA
  lda #$22
  sta sDMACTL

  ; NMI enable for VBI
  lda #$40
  sta NMIEN

  ; load up the table
  ldx #0
  jsr loadtext

  ; set scrolling


forever
  lda #SCROLLV
  sta HSCROL
  jmp forever

loadtext
  clc
  txa
  sta MODETEXT,x
  inx
  cpx #$ff
  bne loadtext
  rts

dli


  * = $5000
; display list

dlist1
  .byte BLANK8,BLANK8,BLANK8  ; 24 lines : 3 bytes
  .byte BLANK8,BLANK8,BLANK8  ; 24 lines : 3 bytes
  .byte $42                  ; mode 2 8x8*40 (one color, two lines)
  .word mode2text
  .byte $43                  ; mode 2 8x8*40 (one color, two lines)
  .word mode3text
  .byte $44                  ; mode 2 8x8*40 (one color, two lines)
  .word mode4text
  .byte $45                  ; mode 2 8x8*40 (one color, two lines)
  .word mode5text
  .byte $46                  ; mode 2 8x8*40 (one color, two lines)
  .word mode6text
  .byte $56
  .word MODETEXT+64
  ; .byte $c6  ;,6,6,6
  .byte $47                  ; mode 2 8x8*40 (one color, two lines)
  .word mode7text
  ; .byte $42
  ; .word MODETEXT
  ; .byte 2,2,2,2,2,2
  .byte $41
  .word dlist1
  * = $6000
;
mode2text
  .sbyte "            hi there mode 2             "
mode3text
  .sbyte "            hi there mode 3             "
mode4text
  .sbyte "            hi there mode 4             "
mode5text
  .sbyte "            hi there mode 5             "
mode6text

  .sbyte "  hi there mode "
  .byte  $56
  .sbyte "   "
mode7text
  .sbyte "  hi there mode 7"
  .byte $57
  .sbyte "   "

ATARTB
  .BYTE   $0D2,$4E,$42,$97,$52,$0
  .BYTE   $0C4,$3E,$34,$79,$42,$0
  .BYTE   $0B6,$2E,$26,$5B,$32,$0
  .BYTE   $0B6,$82,$86,$53,$43,$22,$0
  .BYTE   $0A3,$03,$72,$73,$03,$42,$63,$12,$0
  .BYTE   $0A2,$22,$72,$72,$22,$42,$72,$12,$0
  .BYTE   $0A2,$22,$72,$72,$22,$42,$72,$12,$0
  .BYTE   $92,$42,$62,$62,$42,$32,$62,$22,$0
  .BYTE   $92,$42,$62,$62,$42,$32,$52,$32,$0
  .BYTE   $92,$42,$62,$62,$42,$32,$06,$42,$0
  .BYTE   $83,$43,$52,$53,$43,$22,$05,$52,$0
  .BYTE   $82,$62,$52,$52,$62,$22,$05,$52,$0
  .BYTE   $82,$62,$52,$52,$62,$22,$42,$42,$0
  .BYTE   $7E,$42,$4E,$12,$42,$42,$0
  .BYTE   $7E,$42,$4E,$12,$52,$32,$0
  .BYTE   $7E,$42,$4E,$12,$52,$32,$0
  .BYTE   $63,$83,$32,$33,$83,$02,$62,$22,$0
  .BYTE   $62,$0A2,$32,$32,$0A2,$02,$62,$22,$0
  .BYTE   $62,$0A2,$32,$32,$0A2,$02,$72,$12,$0
  .BYTE   $62,$0A2,$32,$32,$0A2,$02,$72,$12,$0

    * = $bfe8
romtitle ;01234567890123456789
  .sbyte "     GM TEST 02     "
  .byte $57,$ff
  .word init
