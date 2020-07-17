
  * = $4000

init
  .include 5200.s
  jsr set_vectors


  lda #>dlist_df
  sta sDLISTL
  lda #<dlist_df
  sta sDLISTH

  ; default CHBASE
  lda #$f8
  sta $D409

  ; flip on DMA
  lda #$22
  sta sDMACTL
  ; lda #$1
  ; sta CHACTL


setup_fonts
  ldx #0

loader1

  lda 0
  sta $110a,x
  inx
  cpx #20
  bne loader1
  ldx #0
;
loader2
  lda l2,x
  sta $11a0,x
  inx
  cpx #40
  bne loader2
  ldx #0
;
loader3
  lda $d409,x
  sta $1240,x
  inx
  cpx #20
  bne loader3
  ldx #0
;
loader4
  inx
  lda #0
  sta $1128,x
  cpx #40
  bne loader4
  ldx #0

;
loader5
  lda #0
  sta $1150,x
  inx
  cpx #20
  bne loader5
  ldx #0

;
loader6
  lda #0
  sta $1178,x
  inx
  cpx #40
  bne loader6
  ldx #0

;
loader7
  lda l7,x
  sta $1628,x
  inx
  cpx #160
  bne loader7
  ldx #0

;
loader8
  inx
  lda l8,x
  sta $1254,x
  cpx #20
  bne loader8
  ldx #0

;
loader9
  lda l9,x
  sta $1268,x
  inx
  cpx #40
  bne loader9
  ldx #0

;
loader10

  lda l10,x
  sta $1588,x
  inx
  cpx #40
  bne loader10
  ldx #0

;
loader11

  lda l11,x
  sta $15b0,x
  inx
  cpx #40
  bne loader11
  ldx #0

;
loader12
  lda l12,x
  sta $15db,x
  inx
  cpx 40
  bne loader12
  ldx #0

;
loader13
  lda l13,x
  sta $1600,x
  inx
  cpx #40
  bne loader13
  ldx #0


;
loader14
  inx
  lda $d409,x
  sta $1600,x
  cpx #160
  bne loader14
  ldx #0

; enable NMI

  lda #$40
  sta NMIEN

;

setup_colors
  lda #$58
  sta COLPM0
  ; sta COLOR1
  lda #$A8
  sta COLPM1
  ; sta COLOR2
  lda #$44
  sta COLPM2
  lda #$0e
  sta COLPM3
  lda #$ac
  sta COLPF0
  lda #$0c
  sta COLPF1
  lda #$ff
  sta COLPF2
  lda #$7c
  sta COLPF3
  lda #$00
  sta COLBK




;
loop
        jmp loop

l2
  .byte $00, $00, $24, $32, $25, $21, $24, $2E
  .byte $21, $35, $27, $28, $34, $33, $00, $00
  .byte $00, $00, $00, $00

l3
  .byte 00, 00, 00, 00, $10, 00, 00, 00
  .byte 00, 00, 00, 00, 00, 00, $C0, $D9
  .byte $D3, 00, 00, 00


l7
  .byte 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
  .byte 00, 00, $CA, $CD, $C8, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
  .byte 00, 00, 00, 00, 00, 00, 00, 00,

l8
  .byte 00, 00, 00, 00, 00, 00, 00, 00
  .byte 00, 00, $55, $55, $40, 00, 00, 00
  .byte 00, 00, 00, 00

l9
  .byte 00, 00, 00, 00, 00, 00, 00, 00, $28, 00, 00, 00, $80, $28, 00, 00
  .byte 00, 00, 00, $0A, 00, $A0, $28, 00, 00, 00, 00, 00, 00, 00, $20, 00
  .byte 00, 00, $A0, $0A, $28, 00, 00, 00, 00

l10
  .byte 00, 00, 00, 00, 00, $A0, 00, $28, $0A, 00, $28, 00, 00, 00, 00, 00
  .byte 00, 00, 00, 00, 00, $20, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
  .byte $80, $80, $A8, 00, 00, 00, 00, 00,

l11
  .byte $28, 00, 00, 00, 00, $0A, $28, 00, $AA, 00, 00, 00, 00, 00, 00, 00,
  .byte 00, 00, 00, 00, 00, 00, $20, 00, 00, 00, 00, 00, 00, 00, 00, 00,
  .byte $80, $02, 00, 00, 00, 00, 00, 00,

l12
  .byte 00, 00, 00, $88, $2A, $28, $A8, 00, 00, 00, 00, 00, 00, 00, 00, 00
  .byte 00, 00, 00, 00, $20, 00, 00, 00, 00, 00, 00, 00, 00, $20, 00, 00
  .byte 00, 00, 00, 00, 00, 00, 00, $28,

l13
  .byte 00, 00, $28, $80, 00, 00, 00, $A0, $28, 00, $A0, 00, 00, 00, 00, 00
  .byte 00, 00, 00, 00, 00, 00, 00, 00, $A0, 00, 00, 00, 00, 00, 00, 00
  .byte $20, 00, 00, 00, 00, 00, $A0, 00,

  * = $5000
  .include vector_set.s


        * = $a2a2
dlist_df
; A2A2: 3x 8 BLANK
  .byte $70,$70,$70
; A2A5: LMS 110A MODE 6, 20 bytes (l1)
  .byte $46
  ; .word $110a
  .word testtext
; A2A8: LMS 11A0 MODE 2, 40 bytes (l2)
  .byte $42
  .word $11a0
; A2AB: LMS 1240 MODE 6, 20 bytes (l3)
  .byte $46
  .word $1240
; A2AE: 8 BLANK
  .byte $70
; A2AF: LMS 1128 MODE 2, 40 bytes (l4)
  .byte $42
  .word $1128
; A2B2: LMS 1150 MODE 7, 28 bytes (l5)
  .byte $47
  .word $1150
; A2B5: MODE 7
  .byte $07
; A2B6: LMS 1178 MODE 2, 40 bytes (l6)
  .byte $42
  .word $1178
; A2B9: 4x 8 BLANK
  .byte $70,$70,$70,$70
; A2BD: LMS 1628 MODE 2, 40 bytes (l7)
  .byte $42
  .word $1628
; A2C0: 8 BLANK
  .byte $70
; A2C1: MODE 2
  .byte $02
; A2C2: 2x 8 BLANK
  .byte $70,$70
; A2C4: LMS 1254 MODE B, 20 bytes (l8)
  .byte $4b
  .word $1254
; A2C7: DLI 6 BLANK
  .byte $d0
; A2C8: LMS 1268 MODE E, 40 bytes (l9)
  .byte $4e
  .word $1268
; A2CB: 5x MODE E
  .byte $0e,$0e,$0e,$0e,$0e
; A2D0: 15x MODE D
  .byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,
  .byte $0d,$0d,$0d,$0d,$0d,$0d,$0d
; A2DF: LMS 1588 MODE E, 40 bytes (l10)
  .byte $4e
  .word $1588
; A2E2: MODE D
  .byte $0d
; A2E3: LMS 15B0 MODE E, 40 bytes (l11)
  .byte $4e
  .word $15b0
; A2E6: MODE D
  .byte $0d
; A2E7: LMS 15D8 MODE E, 40 bytes (l12)
  .byte $4e
  .word $15db
; A2EA: MODE D
  .byte $0d
; A2EB: LMS 1600 MODE E, 40 bytes (l13)
  .byte $4e
  .word $1600
; A2EE: JVB A2A2
  .byte $41
  .word dlist_df

dlist_battle
  ; DLI 1 BLANK
  ; 2 BLANK
  ; DLI LMS 2C92 VSCROL HSCROL MODE 5
    .byte $45
    .word $2c92
  ; DLI LMS 2D12 VSCROL HSCROL MODE 5
    .byte $45
    .word 2d12
  ; DLI LMS 2D92 VSCROL HSCROL MODE 5
  .byte $45
  .word 2d92
  ; DLI LMS 2E12 VSCROL HSCROL MODE 5
  .byte $45
  ; DLI LMS 2E92 VSCROL HSCROL MODE 5
  .byte $45
  ; DLI LMS 2F12 VSCROL HSCROL MODE 5
  .byte $45
  ; DLI LMS 2F92 VSCROL HSCROL MODE 5
  .byte $45
  ; DLI LMS 3012 VSCROL HSCROL MODE 5
  .byte $45
  ; DLI LMS 3092 VSCROL HSCROL MODE 5
  .byte $45
  ; DLI LMS 3112 VSCROL HSCROL MODE 5
  .byte $45
  ; DLI LMS 3192 VSCROL HSCROL MODE 5
  .byte $45
  ; DLI LMS 3212 VSCROL HSCROL MODE 5
  .byte $45
  ; DLI LMS 3292 VSCROL HSCROL MODE 5
  .byte $45
  ; DLI LMS 3312 VSCROL HSCROL MODE 5
  .byte $45
  ; LMS 3392 HSCROL MODE 5
  .byte $45
  ; JVB 1789


testtext
  .sbyte  "  THIS IS A TEST YOU FUCKER! "

  * = $bfe8
romtitle ;01234567890123456789
  .sbyte "     GM TEST 03     "
  .byte $57,$ff
  .word init
