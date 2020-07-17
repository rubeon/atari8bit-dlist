; show a grey bar a la De Re Atari

  * = $4000

  .include 5200.s
text = $3000
line = $20
init
      sei                     ;Disable interrupts
      cld                     ;Clear decimal mode


;************** Clear zero page and hardware ******

        ldx     #$00
        lda     #$00
crloop1
        sta     $00,x           ;Clear zero page
        sta     $D400,x         ;Clear ANTIC
        sta     $C000,x         ;Clear GTIA
        sta     $E800,x         ;Clear POKEY
        dex
        bne     crloop1

;************* Clear RAM **************************

        ldy     #$00            ;Clear Ram
        lda     #$02            ;Start at $0200
        sta     $81
        lda     #$00
        sta     $80
crloop3
        lda     #$00
        sta     ($80),y         ;Store data
        iny                     ;Next byte
        bne     crloop3         ;Branch if not done page
        inc     $81             ;Next page
        lda     $81
        cmp     #$40            ;Check if end of RAM
        bne     crloop3         ;Branch if not

;************* Setup display list *******************

;************* Setup display list *******************


        ldx     #$21            ;Number of bytes in list
dlloop                          ;Copy display list to RAM
        lda     dlist,x         ;Get byte
        sta     $1000,x         ;Copy to RAM
        dex                     ;next byte
        bpl     dlloop

  ; .include vector_set.s

  lda     #$03            ;point IRQ vector
  sta     $200            ;to BIOS routine
  lda     #$FC
  sta     $201
  lda     #$B8            ;point VBI vector
  sta     $202            ;to BIOS routine
  lda     #$FC
  sta     $203
  lda     #$B2            ;point Deferred VBI
  sta     $204            ;to BIOS routine
  lda     #$FC
  sta     $205
  lda     #<dli           ;point DLI vector
  sta     $206            ;to custom routine
  lda     #>dli
  sta     $207
  lda     #$00
  sta     line

  ; set dli

  ; lda #<dli
  ; sta $205
  ;
  ; lda #>dli
  ; sta $206


; Background:  00
; Playfield 1: 02
; Playfield 2: 0A
; Playfield 3: 0C
  lda #$0c
  sta COLOR0
  lda #$02
  sta COLPF1
  sta COLOR1
  lda #$0a
  sta COLPF2
  lda #$0c
  sta COLPF3

  lda #<dlist
  sta sDLISTL
  sta DLISTL
  lda #>dlist
  sta sDLISTH
  sta DLISTH


  lda #$f8
  sta CHBASE
  jsr load_tmptxt
  lda #$22
  sta sDMACTL

  lda #$c0
  sta NMIEN
  lda #$ef
  sta COLOR0
  lda	#$0F
  sta	COLOR1
  lda	#$84
  sta	COLOR2


forever
  jsr waitvb
  lda #$00
  sta line

  jmp forever

dli
  pha
  phx
  inc line
  lda #line
  ; sta COLOR2
  ; lda #$ff
  ; sta COLOR1

dli_done
  plx
  pla
  rti

load_tmptxt
  ldx #0
load_tmptxt_loop
  lda tmptxt,x
  sta $3500
  ; turn off the last couple of bits
  ; and #~11111100
  sta $3501
  sta text,x
  inx
  cpx #240
  bne load_tmptxt_loop
  rts

waitvb
        lda     $02     ;Read timer (this is incremented during VB)
waitvb2
        cmp     $02         ;Did it change?
        beq     waitvb2     ;If not keep waiting
        rts


  * = $6000
mytext
  ;       0123456789012345678901234567890123456789
  .sbyte "             HI THERE MO FO             "
  .sbyte "             HI THERE MO FO             "

tmptxt
  ;      "01234564789012345647890123456478901234564789"
  ; .sbyte "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  ; .byte +$00,"0123456789012345"
  .sbyte "abcdefghijklmnopqrstuvwxyz"
  .sbyte +$14,"0123456789012345"
  .sbyte "abcdefghijklmnopqrstuvwxyz"
  .sbyte +$14,"0123456789012345"
  .sbyte "abcdefghijklmnopqrstuvwxyz"
  .sbyte +$14,"0123456789012345"
  .sbyte "abcdefghijklmnopqrstuvwxyz"
  .sbyte +$14,"0123456789012345"
  .sbyte "abcdefghijklmnopqrstuvwxyz"
  .sbyte +$14,"0123456789012345"



taritxt
  .byte  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $05, $50, $55, $55, $05, $50, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  .byte  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $05, $50, $55, $55, $05, $50, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00


;
;  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
;3000 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
;3028 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
;3050 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 05 50 55 55 05 50 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
;30x0 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 05 50 55 55 05 50 00



;
;************* Display list data ****************************
        * = $b000
dlist   .byte     $70,$70,$70      ;24 blank scanlines
        .byte     $c3      ;Mode 8 and Load memory scan $3000
        .word     mytext
        ; .byte     $84,$84,$84,$84,$84,$84,$84   ;23 more line of mode 8
        ; .byte     $84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84,$84
        ; .byte     $84,$84,$84
        .byte     $41      ;Jump back to start at $1000
        .word dlist

;
;************* Player shape *********************************

pm1     .byte     ~00111100
        .byte     ~01000010
        .byte     ~10100101
        .byte     ~10000001
        .byte     ~10100101
        .byte     ~10011001
        .byte     ~01000010
        .byte     ~00111100



  * = $bfe8
  ;       012345678901234567890
  .sbyte "    HI THERE MOFO    "

  * = $bffc
  .byte $57,$ff

  .word init
