; Demo screen showing text being written and scrolling with
; a blinking cursor

; setup equates
POKMSK = $0
; vectors
VIMIRQ    = $200
VVBLKI    = $202
VVBLKD    = $204
VBREAK    = $206
VKYBDI    = $208
VKPD      = $20a

XITVBV   =  $e462       ;BIOS vblank IRQ exiter
XITVBL    = $fcb2       ;BIOS kbd IRQ exiter


ANTIC   = $d400
DMACTL    = ANTIC

CHACTL    = ANTIC+$1
DLISTL    = ANTIC+$2
DLISTH    = ANTIC+$3
HSCROL    = ANTIC+$4
VSCROL    = ANTIC+$5
PMBASE    = ANTIC+$7
CHBASE    = ANTIC+$9
WSYNC     = ANTIC+$a
VCOUNT    = ANTIC+$b
NMIEN     = ANTIC+$e
; shadow antics
sDLISTL   = $05
sDLISTH   = $06
sDMACTL   = $07
sCOLPM0   = $08
sCOLPM1   = $09
sCOLPM2   = $0A
sCOLPM3   = $0B
sCOLOR0   = $0C
sCOLOR1   = $0D
sCOLOR2   = $0E
sCOLOR3   = $0F
sCOLBK    = $10

RTCLOK    = $1

; GIT registers
GTIA      = $c000

HPOSP0    = GTIA             ; (w) horiz. position of player 0
HPOSP1    = GTIA+$1          ; (w) position of player 1
HPOSP2    = GTIA+$2          ; (w) horiz. position of player 2
HPOSP3    = GTIA+$3          ; (w) horiz. position of player 3
HPOSM0    = GTIA+$4          ; (w) horiz. position of missile 0
HPOSM1    = GTIA+$5          ; (w) horiz. position of missile 1
HPOSM2    = GTIA+$6          ; (w) horiz. position of missile 2
HPOSM3    = GTIA+$7          ; (w) horiz. position of missile 3

M0PF      = GTIA             ; (r) Missile 0 to PF collisions
M1PF      = GTIA+$1          ; (r) Missile 1 to PF collisions
M2PF      = GTIA+$2          ; (r) Missile 2 to PF collisions
M3PF      = GTIA+$3          ; (r) Missile 3 to PF collisions

P0PF      = GTIA+$4          ; (r) player 0 to playfield collisions
P1PF      = GTIA+$5          ; (r) player 1 to playfield collisions
P2PF      = GTIA+$6          ; (r) player 2 to playfield collisions
P3PF      = GTIA+$7          ; (r) player 3 to playfield collisions

M0PL      = GTIA+$8          ; (r) Missile 0 to player collisions
M1PL      = GTIA+$9          ; (r) Missile 1 to player collisions
M2PL      = GTIA+$a          ; (r) Missile 2 to player collisions
M3PL      = GTIA+$b          ; (r) Missile 3 to plahyer collisions


SIZEP0    = GTIA+$8          ; (w) size of player 0
SIZEP1    = GTIA+$9          ; (w) size of player 1
SIZEP2    = GTIA+$a          ; (w) size of player 2
SIZEP3    = GTIA+$b          ; (w) size of player 3
SIZEM     = GTIA+$c          ; (w) size of all missiles

P0PL      = GTIA+$c          ; (r) player 0 to player collisions
P1PL      = GTIA+$d          ; (r) player 1 to player collisions
P2PL      = GTIA+$e          ; (r) player 2 to player collisions
P3PL      = GTIA+$f          ; (r) player 3 to player collisions

GRAFP0    = GTIA+$d          ; (w) graphics pattern for player 0 $d00d :-)
GRAFP1    = GTIA+$e          ; (w) graphics pattern for player 1
GRAFP2    = GTIA+$f          ; (w) graphics pattern for player 2
GRAFP3    = GTIA+$10         ; (w) graphics pattern for player 3
GRAFM     = GTIA+$11         ; (w) graphics pattern for all missiles

TRIG0     = GTIA+$10         ;(r) P1 trigger
TRIG1     = GTIA+$11         ;(r) P2 trigger
TRIG2     = GTIA+$12         ;(r) P3 trigger
TRIG3     = GTIA+$13         ;(r) P4 trigger

PRIOR     = GTIA+$1b         ; (w) Priority selection, fifth player, and GTIA modes
GRACTL    = GTIA+$1d         ; (w) Graphics Control.
HITCLR    = GTIA+$1e         ; (w) Clear collisions
CONSPK    = GTIA+$1f         ; (w) console speaker

CONSOL    = GTIA+$1f         ; (r) Console Keys

COLPM0    = GTIA+$12         ; (w) Color/luminance of Player and Missile 0.
COLPM1    = GTIA+$13         ; (w) Color/luminance of Player and Missile 1.
COLPM2    = GTIA+$14         ; (w) Color/luminance of Player and Missile 2.
COLPM3    = GTIA+$15         ; (w) Color/luminance of Player and Missile 3.
COLPF0    = GTIA+$16         ; (w) Color/luminance of Playfield 0.
COLPF1    = GTIA+$17         ; (w) Color/luminance of Playfield 1.
COLPF2    = GTIA+$18         ; (w) Color/luminance of Playfield 2.
COLPF3    = GTIA+$19         ; (w) Color/luminance of Playfield 3.
COLBK     = GTIA+$1a         ; (w) Color/luminance of Playfield background.


POKEY   = $e800
POT0      = POKEY           ; P1 X
POT1      = POKEY+$1        ; P1 Y
POT2      = POKEY+$2        ; P2 X
POT3      = POKEY+$4        ; P2 Y
POT4      = POKEY+$4        ; P3 X
POT5      = POKEY+$5        ; P3 Y
POT6      = POKEY+$6        ; P4 X
POT7      = POKEY+$7        ; P4 Y
RANDOM    = POKEY+$a
POTGO     = POKEY+$b
IRQEN     = POKEY+$e
IRQSTAT   = POKEY+$e
SKCTL     = POKEY+$f

; shadow color registers
COLOR0    = $0c
COLOR1    = $0d
COLOR2    = $0e
COLOR3    = $0f
COLOR4    = $10
; shadow pokeys
PADDL0    = $11
PADDL1    = PADDL0+1
PADDL2    = PADDL0+2
PADDL3    = PADDL0+3
PADDL4    = PADDL0+4
PADDL5    = PADDL0+5
PADDL6    = PADDL0+6
PADDL7    = PADDL0+7

; constants
DL_HSCROLL      = $10
DL_VSCROLL      = $20
DL_LMS          = $40
DL_DLI          = $80
CURDELAY        = $1
CURDELAYLONG    = $ff
CURGRAF         = $59       ; solid block
; zero page
CURPOS          = $80
TXTPOS          = $82
; variables
  * = $1000
CURX            .ds $1
CURY            .ds $1
CURBLINK        .ds $1
CURTIMER        .ds $1
MAINSCREENL     .ds $1
MAINSCREENH     .ds $1
hexcode         .ds $2
; screen RAM
sscoreline      .ds $28
chbase_cur      .ds $1

  * = $1200
smainscreen
  * = $1600
smainscreen2
; initialization routines

  * = $4000

init
  sei                         ; clear interrupts
  cld                         ; clear decimal mode
  lda #$0
  sta NMIEN                   ; turn off NMIs

  ldx #0

crloop1                       ; clear out ram
  sta $00,x                   ; write 0 to HW[0-255]
  sta ANTIC,x
  sta GTIA,x
  sta POKEY,x
  dex
  bne crloop1

  ldy #0
  lda #$02                    ; put starting $0200 address
  sta $81                     ; into $80,$81
  lda #$00
  sta $80

crloop3
  lda #0
  sta ($80),y
  iny
  bne crloop3                 ; loop ghrough $0200-$2ff

  inc $81                     ; $0300-$3fff
  lda $81
  cmp #$40
  bne crloop3

  lda #$22
  sta sDMACTL                 ; DLISTs
  lda #0
  sta GRACTL                  ; no missile/player


  lda #$0f                    ; colors
  sta COLPF0
  sta sCOLOR0
  lda #$0c
  sta COLPF1
  sta sCOLOR1
  lda #$00
  sta COLPF2
  sta sCOLOR2
  lda #$46
  sta COLPF3
  sta sCOLOR3
  lda #$0
  sta sCOLBK
  sta COLBK

  lda #$f8                  ; set the default atari chset
  sta CHBASE

  lda #<dlist               ; setup DLIST pointer
  sta sDLISTL
  sta DLISTL
  lda #>dlist
  sta sDLISTH
  sta DLISTH

  lda #$03                  ; setup bios interrupt handler
  sta VIMIRQ
  lda #$fc
  sta VIMIRQ+1

  lda #$b8                  ; setup bios vblank immediate handler
  sta VVBLKI
  lda #$fc
  sta VVBLKI+1

  lda #<vbi_deferred        ; add my vblank deferred handler
  sta VVBLKD
  lda #>vbi_deferred
  sta VVBLKD+1

  lda #<dli                 ; add my DLI handlers
  sta VBREAK
  lda #>dli
  sta VBREAK+1

  lda #$c0                  ; VBI + DLI
  sta NMIEN

  lda #$40                  ; turn on controllers
  sta POKMSK
  sta IRQEN

  lda #$2                   ; enable kb scanning
  sta SKCTL

  cli

  lda #1
  sta CURX
  sta CURY
  lda #$1
  sta CURTIMER
  lda #$ff
  sta CURBLINK
  lda #<smainscreen
  sta MAINSCREENL
  sta CURPOS
  lda #>smainscreen
  sta MAINSCREENH
  sta CURPOS+1
  lda #<mainscreen
  sta TXTPOS
  lda #>mainscreen
  sta TXTPOS+1

  lda #$fc
  sta chbase_cur

forever
  jmp forever

draw_byte
  ; acc has value, y has offset

draw_status
  lda CURX
  jsr a2hexcode
  lda hexcode
  sta sscoreline+24
  lda hexcode+1
  sta sscoreline+25
  lda CURY
  jsr a2hexcode
  lda hexcode
  sta sscoreline+27
  lda hexcode+1
  sta sscoreline+28

  rts

;
a2hexcode
  ; converts byte in accumulator to two hex characters
  pha           ; save a
  ldx #0
  lsr
  lsr
  lsr
  lsr           ; convert first byte to 0-16
?start
  tay
  lda numbers,y
  sta hexcode,x
  inx
  cpx #2
  beq ?done
  pla
  and #$0f
  jmp ?start
?done
  rts


draw_cursor
draw_cursor_char
  clc
  ; lda CURPOS        ; load current position
  ; adc #1            ; increment it
  ; sta CURPOS
  ; lda CURPOS+1
  ; adc #0            ; add carry, if any
  ; sta CURPOS+1

  lda TXTPOS
  adc #1
  sta TXTPOS
  lda TXTPOS+1
  adc #0
  sta TXTPOS+1
  ldx CURX
  cpx #38
  bne up_x
  jsr newline
  jmp draw_continue
up_x
  inx
  stx CURX
draw_continue
  jsr xy_to_screen
  ldx #0
  lda (TXTPOS,x)
  cmp linesep
  beq draw_newline
  cmp scenesep
  beq draw_scenesep
  cmp pausechar
  beq draw_pausechar
  jmp draw_it
draw_newline
  jsr newline
  jmp draw_cursor_done
draw_scenesep
  ldy #0

  sty CURY
  sty CURX
  jsr xy_to_screen
  ldx #<dlist2
  stx sDLISTL
  stx DLISTL
  ldx #>dlist2
  stx sDLISTH
  stx DLISTH
  ldx #<smainscreen2
  stx MAINSCREENL
  ldx #>smainscreen2
  stx MAINSCREENH
  ldx #$f8
  stx chbase_cur
  jmp draw_cursor_done
draw_pausechar
  ldy CURDELAYLONG
  sty CURTIMER
  jmp draw_cursor_done
draw_it
  sta (CURPOS,x)
draw_cursor_done
  rts

xy_to_screen
  clc
  ldy CURY
  ; lda #<smainscreen
  lda MAINSCREENL
  sta CURPOS
  ; lda #>smainscreen
  lda MAINSCREENH
  sta CURPOS+1
  cpy #0
  bne addrow
  jmp addx
addrow
  lda CURPOS
  adc #40
  sta CURPOS
  lda CURPOS+1
  adc #0
  sta CURPOS+1
  dey
  bne addrow
  clc
  ldx CURX
  bne addx
  jmp xy_done
addx
  lda CURPOS
  adc #1
  sta CURPOS
  lda CURPOS+1
  adc #0
  sta CURPOS+1
  dex
  bne addx
xy_done
  rts

.LOCAL
newline
  inc CURY
  lda CURY
  cmp #20
  beq newpage
  sta CURY
  lda #0
  sta CURX
  jmp newline_done
newpage
  lda #0
  sta CURY
  ldx #0
  stx CURX
  ldx #40
  ldy #24
; clearpage
;   sty CURY
;   stx CURX
;   phx
;   ldx #0
;   jsr xy_to_screen
;   lda #0
;   sta (CURPOS,x)
;   plx
;   dex
;   bne clearpage
;   dey
;   bne clearpage

newline_done
  rts


vbi_deferred
  lda #$f8
  sta CHBASE
  dec CURTIMER
  bne vbi_skip_cursor
  lda #CURDELAY
  sta CURTIMER
  jsr draw_cursor
vbi_skip_cursor

  jsr draw_status
  jmp $fcb2
dli
  pha
  lda VCOUNT
  cmp #$50                   ; past the 20th scanline?
  bge dli_bottom
  lda #$c8
  sta COLBK
  lda chbase_cur
  sta CHBASE
  jmp dli_done
dli_bottom
  lda #0
  sta COLBK
  lda #$f8
  sta CHBASE

dli_done
  pla
  rti

;******************  DISPLAY LIST ******************
  * = $5000
tmode   = $2
dlist
  .byte $70,$70,$70          ; 24 blank lines
  .byte $7|DL_LMS                  ; mode 7 LMS
  .word topline
  .byte $3|DL_LMS|DL_DLI
  .word sscoreline
  .byte tmode|DL_LMS
  .word smainscreen
  .byte tmode,tmode,tmode,tmode,tmode,tmode,tmode,tmode ; 20 lines of mode 3
  .byte tmode,tmode,tmode,tmode,tmode,tmode,tmode,tmode
  .byte tmode,tmode,tmode,tmode
  .byte $41
  .word dlist

;
dlist2
  .byte $70,$70,$70          ; 24 blank lines
  .byte $7|DL_LMS                  ; mode 7 LMS
  .word topline2
  .byte $3|DL_LMS|DL_DLI
  .word sscoreline
  .byte tmode|DL_LMS
  .word smainscreen2
  .byte tmode,tmode,tmode,tmode,tmode,tmode,tmode,tmode ; 20 lines of mode 3
  .byte tmode,tmode,tmode,tmode,tmode,tmode,tmode,tmode
  .byte tmode,tmode,tmode,tmode
  .byte $41
  .word dlist



;***************** SCREEN STUFF *******************
  * = $6000
topline
  .sbyte " TERMINAL ACTIVATED "
topline2
  .sbyte " terminal  acquired "
scoreline
  .sbyte "      CONNECTION: 1200,N,8,1 xx:xx      "
mainscreen
  .sbyte "5 REM PMSAMPLE2}"
  .sbyte "10 DIM PM$(128)}"
  .sbyte "20 DIM SHIP$(16),CLEAR$(128) }"
  .sbyte "30 FOR ROW=1 TO 16}"
  .sbyte "40 READ DOTS}"
  .sbyte "50 SHIP$(ROW,ROW)=CHR$(DOTS) }"
  .sbyte "60 NEXT ROW}"
  .sbyte "70 DATA 0,0,0,0,0}"
  .sbyte "80 DATA 16,56,56,124,108,68 }"
  .sbyte "90 DATA 0,0,0,0,0}"
  .sbyte "100 FOR ROW=1 TO 128}"
  .sbyte "110 CLEAR$(ROW,ROW)=CHR$(0) }"
  .sbyte "120 NEXT ROW}"
  .sbyte "130 A=4*(INT(PEEK(742)/4)-1) }"
  .sbyte "140 POKE 54279,A}"
  .sbyte "150 VSA=256*PEEK(135)+PEEK(134) }"
  .sbyte "160 BOA=256*PEEK(141)+PEEK(140) }"
  .sbyte "170 PM=256*A+512}"
  .sbyte "180 DISP=PM-BOA }"
  .sbyte "190 ADD=2}"
  .sbyte "200 FOR T=1 TO 1}"
  .sbyte "210 PMHIGH=INT(DISP/256) }"
  .sbyte "220 PMLOW=DISP-256*PMHIGH }"
  .sbyte "230 POKE VSA+ADD,PMLOW }"
  .sbyte "240 POKE VSA+ADD+1,PMHIGH }"
  .sbyte "_"
titletext
  .sbyte "}}" ; trailing space for dumb bug

  .sbyte ">>> |#47 WAKE UP}|"
  .sbyte "...}>>> |"
  .sbyte "#47 YOUR PROGRAM HAS }"
  .sbyte "        BEEN UPDATED}|"
  .sbyte "...}>>> |"
  .sbyte "YOU MUST REINITIALIZE}|"
  .sbyte "ok}>>> |"
  .sbyte "YOU MUST RESTORE POWER}|"
  .sbyte "ok}>>> |"
  .sbyte "YOU MUST REBUILD}|"
  .sbyte "ok}|"
  .sbyte "...}|"
  .sbyte "REBUILD?}|"




rows
  .byte 0,40,80,120,130,140,160,200,240


linesep
  .sbyte "}"
scenesep
  .sbyte "_"
pausechar
  .sbyte "|"

numbers
  .sbyte "0123456789abcdef"
;*****************  5200 STUFF ********************
  * = $bfe8
         ;01234567890123456789
  .sbyte "   SCREEN DEMO "
  .byte                 $56
  .sbyte                   "  "
  * = $bffc
  .byte $57,$50
  * = $bffd
  .byte $ff
  * = $bffe
  .word init
