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
COLBK	= $c01a

COLOR1  =	$0d			; Color 1 shadow
COLOR2  =	$0e			; Color 2 shadow
COLOR3	=	$0f
VDSLST  = $200
NMIEN_DLI = $80
NMIEN_VBI = $40

COLPF0 = $c016
COLPF1 = COLPF0+1
COLPF2 = COLPF0+2
COLPF3 = COLPF0+3

COUNTER=$3000

SETVBV   = $e45c
VCOUNT   = $d40b
XITVBV   = $e462

start_color = $80
text_color = $36


  * = $4000

init
  sei
  ; lda #$03
  ; sta $200
  ; lda #$fc
  ; sta $201
  ;
  lda #$b8
  sta $202
  lda #$fc
  sta $203

  ; lda #>vbi1
  ; sta $202
  ; lda #<vbi1
  ; sta $203

  lda #$b2
  sta $204
  lda #$fc
  sta $205

  ; lda #$ef
  ; sta COLOR0

  lda #1
  sta start_color
  lda #$text_color
  sta COLOR0

  lda #$0f
  sta COLOR1
  lda #$84
  sta COLOR2
  lda #<dlistrb
  sta sDLISTL

  lda #>dlistrb
  sta sDLISTH

  lda #$f8
  sta CHBASE

  lda #$22
  sta sDMACTL

  ldx #>dli
  ldy #<dli
  jsr init_dli

  ldx #>vbi1
  ldy #<vbi1
  jsr init_vbi

  ; lda #$40
  ; sta NMIEN

forever
  jmp forever

;
init_dli
  sty VDSLST
  stx VDSLST + 1

  ; activate disiplay list interrupt
  lda #NMIEN_VBI | NMIEN_DLI
  sta NMIEN
  rts

;
init_vbi
        ; load deferred VBI address
        lda #7
        jsr SETVBV
        rts


.LOCAL
dli     pha             ; save A & X registers to stack
        txa
        pha
        ldx #32         ; make 32 color changes
        lda start_color ; initial color
        sta WSYNC       ; first WSYNC gets us to start of scan line we want
?loop   sta COLPF0      ; change text color for UPPERCASE characters in gr2
        ; clc
        ; adc #$1         ; change color value, making brighter
        sec
        sbc #$1         ; change color value, making darker

        dex             ; update iteration count
        sta WSYNC       ; sta doesn't affect processor flags
        bne ?loop       ; we are still checking result of dex
        lda #text_color ; reset text color to normal color
        sta COLPF0
        dec start_color ; change starting color for next time
        pla             ; restore X & A registers from stack
        tax
        pla
        ; rti             ; always end DLI with RTI!
        jsr $fc03
        rti



; dli     pha             ; save A & X registers to stack
;         txa
;         pha
;         ldx #16         ; make 16 color changes
;         lda #$a         ; initial color
;         sta WSYNC       ; first WSYNC gets us to start of scan line we want
; ?loop   sta COLBK       ; change background color
;         clc
;         adc #$10        ; change color value, luminance remains the same
;         dex             ; update iteration count
;         sta WSYNC       ; make it the color change last ...
;         sta WSYNC       ;   for two scan lines
;         bne ?loop       ; sta doesn't affect flags so this still checks result of dex
;         lda #$00        ; reset background color to black
;         sta COLBK
;         pla             ; restore X & A registers from stack
;         tax
;         pla
;         ; rti             ; always end DLI with RTI!
;         jsr $fc03
;         rti
; dli
;   pha
;   txa
;   pha
;   ldx #16         ; make 16 color changes
;   lda #$a        ; new background color
;   sta WSYNC
; ?loop
;   sta COLBK       ; store it in the hardware register
;   clc
;   adc #$10
;   dex
;   sta WSYNC
;   sta WSYNC
;   bne ?loop
;   lda #$00
;   sta COLBK
;   pla
;   tax
;   pla             ; restore the A register
;   rti             ; always end DLI with RTI!
;
.LOCAL
vbi1    lda VCOUNT
;         bne vbi1         ; just wait for top of screen
;         ldx #0
; ?top    inx             ; on top of screen
;         stx COLBK
;         lda VCOUNT
;         cmp #50         ; work until 100th scan line
;         bcc ?top
;         lda #0          ; reset background color to zero
;         sta COLBK
        jmp XITVBV


  * = $a000

;
dlist
  .byte $70,$70,$70  ; 24 blank lines
  .byte $46,        ; Mode 6 + LMS, setting screen memory to $4000
  .word title
  .byte $84            ; Mode 4
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



dlist2
  .byte $70,$70,$70       ; 24
  .byte $43
  .word splash
  .byte $3,$3,$83          ; 30 (54)
  .byte $70,$70,$70       ; 24 (78)
  .byte $7                ; 16 (94)
  .byte $7                ; 16 (110)
  ; .word COUNTER
  .byte $70,$70,$70       ; 24 (134)
  .byte $70,$70,$70       ; 24 (158)
  .byte $70,$70,$70       ; 24 (182)
  .byte $3                ; 10 (192)
  .byte $41
  .word dlist2
;
dlistrb   .byte $70,$70,$70,$70,$70,$70  ; 48 blank lines
        .byte $46,<text,>text ; Mode 6 + LMS, setting screen memory to text
        .byte 6            ; Mode 6
        .byte $70,$70      ; 16 blank lines
        .byte 7,7,7        ; 3 lines of Mode 7
        .byte $70          ; 8 blank lines
        .byte $f0          ; 8 blank lines + DLI on last scan line
        .byte 7,7          ; 2 lines of Mode 7
        .byte $41,<dlistrb,>dlistrb ; JVB, restart same display list on next frame

;

  * = $b000
;
text
  .sbyte "   player"
  .byte $4f ; slash using lower-case color
  .sbyte           "missile   "
  .sbyte "  podcast presents  "

  .sbyte +$c0, "    ATARI  8-BIT    "
  .sbyte +$c0, "    DISPLAY LIST    "
  .sbyte +$c0, "     INTERRUPTS     "

  .sbyte "   A COMPLETE(ISH)  "
  .sbyte "      TUTORIAL      "



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


splash
.sbyte  "          ANOTHER DLIST TEST! (1)       "
.sbyte  "          ANOTHER DLIST TEST! (2)       "
.sbyte  "          ANOTHER DLIST TEST! (3)       "
.sbyte  "          ANOTHER DLIST TEST! (4)       "
.sbyte             "   DLISTS ROCK!!!   "
.sbyte             "   but they hard!   "
.sbyte  "          ANOTHER DLIST TEST! (4)       "
.sbyte  "          ANOTHER DLIST TEST! (4)       "
.sbyte  "          ANOTHER DLIST TEST! (4)       "
.sbyte  "          ANOTHER DLIST TEST! (4)       "
.sbyte  "          ANOTHER DLIST TEST! (4)       "

; title is 20 bytes wide
  * = $bfe8

cartitle
  .sbyte "    DLI TEST 04     "

  * = $bffc
  .byte $57,$50  ; $57='7', $50='0'
                 ; display copyright 1970

  * = $bffd
  .byte $ff

  * = $bffe
  .word init
