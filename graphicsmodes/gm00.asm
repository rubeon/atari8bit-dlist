  .include 5200.s

horz_scroll = $91       ; variable used to store HSCROL value
horz_scroll_max = $ff     ; ANTIC mode 4 has 4 color clocks


delay = 2
MODETEXT=$3000
SCROLLV=2 ; speed of scrolling
HORZ_SCROLL = $91
COUNTER=$92
  * = $4000

init
  lda #$03          ; IRQ vector
  sta $200
  lda #$fc
  sta $201

  ; lda #<dli
  ; sta $200
  ; lda #>dli
  ; sta $201

  lda #$b8          ; vbi vector
  sta $202
  lda #$fc
  sta $203

  lda #$b2          ; deferred VBI
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
  lda #<dlist_coarse_mode4
  sta sDLISTL
  lda #>dlist_coarse_mode4
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

  lda #$1
  sta CHACTL
  lda #0          ; initialize horizontal scrolling value
  sta horz_scroll
  sta HSCROL      ; initialize hardware register

; forever
;   ldx #15         ; number of VBLANKs to wait
; ?start
;   lda RTCLOK+1    ; check fastest moving RTCLOCK byte
; ?wait
;   cmp RTCLOK+1    ; VBLANK will update this
;   beq ?wait       ; delay until VBLANK changes it
;   dex             ; delay for a number of VBLANKs
;   bpl ?start
; ;   ; enough time has passed, scroll one color clock
;   ; brk
;   jsr fine_scroll_left
;   jmp forever
;
;
loop
        ldx #15         ; number of VBLANKs to wait
?start  lda RTCLOK+1    ; check fastest moving RTCLOCK byte
?wait   cmp RTCLOK+1    ; VBLANK will update this
        beq ?wait       ; delay until VBLANK changes it
        dex             ; delay for a number of VBLANKs
        bpl ?start

        ; enough time has passed, scroll one line
        jsr coarse_scroll_down

        jmp loop


; scroll one color clock left and check if at HSCROL limit
fine_scroll_left
        inc horz_scroll
        lda horz_scroll
        cmp #horz_scroll_max ; check to see if we need to do a coarse scroll
        bcc ?done       ; nope, still in the middle of the character
        ; jsr coarse_scroll_left ; yep, do a coarse scroll...
        lda #0          ;  ...followed by reseting the HSCROL register
        sta horz_scroll
?done   sta HSCROL      ; store vertical scroll value in hardware register
        rts


loadtext
  clc
  txa
  sta MODETEXT,x
  inx
  cpx #$ff
  bne loadtext
  rts


dli
  ldx VCOUNT
  lda #HSCROL
  ; ina
  sta WSYNC
  sta HSCROL
  stx MODETEXT+65
  rti

  * = $5000
; display list
coarse_scroll_down
        clc
        lda dlist_coarse_address
        adc #40
        sta dlist_coarse_address
        lda dlist_coarse_address+1
        adc #0
        sta dlist_coarse_address+1
        rts

dlist1
  .byte BLANK8,BLANK8,BLANK8  ; 24 lines : 3 bytes
  ; .byte BLANK8,BLANK8,BLANK8  ; 24 lines : 3 bytes
  .byte $d2                  ; mode 2 8x8*40 (one color, two lines)
  .word mode2text
  ; .byte MODETEXT

  .byte $43                  ; mode 2 8x8*40 (one color, two lines)
  .word mode3text
  .byte $44                  ; mode 2 8x8*40 (one color, two lines)
  .word mode4text
  .byte $45                  ; mode 2 8x8*40 (one color, two lines)
  .word mode5text
  .byte $46                  ; mode 2 8x8*40 (one color, two lines)
  .word mode6text
  .byte $d6
  .word MODETEXT+64
  .byte $d6
  .word MODETEXT+64
  ; .byte $c6  ;,6,6,6
  .byte $c7                  ; mode 2 8x8*40 (one color, two lines)
  .word mode7text
  ; .byte $42
  ; .word MODETEXT
  ; .byte 2,2,2,2,2,2
  .byte $41
  .word dlist1

  * = $5200
;

coarse_scroll_up
        sec
        lda dlist_coarse_address
        sbc #40
        sta dlist_coarse_address
        lda dlist_coarse_address+1
        sbc #0
        sta dlist_coarse_address+1
        rts

;
dlist_coarse_mode4
        .byte $70,$70,$70       ; 24 blank lines
        .byte $44               ; Mode 4 + LMS
dlist_coarse_address
        .byte $b0,$84           ; screen address
        .byte 4,4,4,4,4,4,4,4   ; 21 more Mode 4 lines
        .byte 4,4,4,4,4,4,4,4
        .byte 4,4,4,4,4
        .byte $42,<static_text, >static_text ; 2 Mode 2 lines + LMS + address
        .byte $2
        .byte $41,<dlist_coarse_mode4,>dlist_coarse_mode4 ; JVB ends display list
        ;             0123456789012345678901234567890123456789
static_text
        .sbyte +$80, " ANTIC MODE 2, NOT SCROLLED, FIRST LINE "
        .sbyte       " ANTIC MODE 2, NOT SCROLLED, SECOND LINE"

;
.include "util_font.s"
.include "util_scroll.s"
.include "font_data_antic4.s"
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

    * = $bfe8
romtitle ;01234567890123456789
  .sbyte "     GM TEST 02     "
  .byte $57,$ff
  .word init
