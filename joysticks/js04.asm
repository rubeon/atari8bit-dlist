POKEY     = $e800
GTIA      = $c000
HPOSP0    = GTIA
HPOSP1    = GTIA+$1
HPOSP2    = GTIA+$2
HPOSP3    = GTIA+$3
HPOSM0    = GTIA+$4
HPOSM1    = GTIA+$5
HPOSM2    = GTIA+$6
HPOSM3    = GTIA+$7

NMIEN_DLI = $80
NMIEN_VBI = $40

VDSLST  = $200

PRIOR     = GTIA+$1b
GRACTL    = GTIA+$1d
CONSOL    = GTIA+$1f

RTCLOK    = $01

ANTIC     = $d400
CHACTL    = ANTIC+$1
HSCROL    = ANTIC+$4
VSCROL    = ANTIC+$5
PMBASE    = ANTIC+$7
CHBASE    = ANTIC+$9

WSYNC     = ANTIC+$0a
NMIEN     = ANTIC+$0e
DLISTL    = ANTIC+2
DLISTH    = ANTIC+3
sDLISTL   = $05
sDLISTH   = $06
DMACTL    = ANTIC
sDMACTL   = $07

POKMSK    = $0

VVBLKI    = $202
VVBLKD    = $204
VKYBDI    = $208

VKPD      = $20a
XITVBL    = $fcb2


COLPM0    = GTIA+$12
COLPM1    = COLPM0+1
COLPM2    = COLPM1+1
COLPM3    = COLPM2+1

COLPF0     = GTIA+$16
COLPF1     = COLPF0+1
COLPF2     = COLPF1+1
COLPF3     = COLPF2+1
COLBK      = COLPF3+1

COLOR0    = $0c
COLOR1    = $0d
COLOR2    = $0e
COLOR3    = $0f

SETVBV   = $e45c
VCOUNT   = $d40b
XITVBV   = $e462

; COLOR0    = $0c
; COLOR1    = $0d
; COLOR2    = $0e
; COLOR3    = $0f


POT0      = POKEY           ; P1 X
POT1      = POKEY+$1        ; P1 Y
POT2      = POKEY+$2        ; P2 X
POT3      = POKEY+$4        ; P2 Y
POT4      = POKEY+$4        ; P3 X
POT5      = POKEY+$5        ; P3 Y
POT6      = POKEY+$6        ; P4 X
POT7      = POKEY+$7        ; P4 Y
RANDOM    = POKEY+$a
IRQEN     = POKEY+$e

; shadow registers for the paddles
; maybe this will work on the 5200?
PADDL0    = $11
PADDL1    = PADDL0+1
PADDL2    = PADDL0+2
PADDL3    = PADDL0+3
PADDL4    = PADDL0+4
PADDL5    = PADDL0+5
PADDL6    = PADDL0+6
PADDL7    = PADDL0+7

POTGO     = POKEY+$b

IRQSTAT   = POKEY+$e
SKCTL     = POKEY+$f

TRIG0     = GTIA+$10        ;P1 trigger
TRIG1     = GTIA+$11        ;P2 trigger
TRIG2     = GTIA+$12        ;P3 trigger
TRIG3     = GTIA+$13        ;P4 trigger

DL_HSCROLL = $10
DL_VSCROLL = $20
DL_LMS     = $40
DL_DLI     = $80
delay      = $1

deadzone_up     = 90
deadzone_down   = 130
deadzone_left   = 90
deadzone_right  = 130
playerx_max     = $c9
playerx_min     = $2f
playery_max     = $57
playery_min     = $1b
; zero page variables

; ram variables
  * = $1000
vbicounter  .ds $1
ch          .ds $1
hscroll_ctr .ds $1
hscroll_max .ds $1
hexcode     .ds $2
stopline    .ds $14
potshow     .ds $28
sscoreline  .ds $28
sdlist      .ds $60
playerx     .ds $1
playery     .ds $1
playery_old .ds $1
is_paused   .ds $1  0
keypad_debounce .ds $1

  * = $1200
pmgraphics
  * = $2000
chartable

  * = $4000

init
  sei
  lda #$0
  sta NMIEN     ; disable interrupts
  ; sta DMACTL

  cld

  ; jsr set_dlist_vectors
;
cptopline
  lda topline,x
  sta stopline,x
  inx
  cpx #20
  bne cptopline

  ldx #0
cpscoreline
  ;
  lda scoreline,x
  sta sscoreline,x
  inx
  cpx #$ff
  bne cpscoreline

  ; ldx #0
  ; ldy #<vblankd
  ; sty VVBLKD
  ; ldx #>vblankd
  ; stx VVBLKD+1

  ; keyboard interrupts?
  ; ignore for now
  ldx #0
cpdlist
  lda dlist,x
  sta sdlist,x
  inx
  cpx #36
  bne cpdlist

.LOCAL
fudgepmgraphics
  ldx #$ff
  lda #$ff
?loop
  sta pmgraphics,x
  eor #$ff
  dex
  bne ?loop
;
.LOCAL
cppmgraphics
  ldx #0
  ldy #$ff
?loop
  lda pmgraphics_rom,x
  sta pmgraphics,y
  iny
  inx
  cpx #8
  bne ?loop

  ; lda #$4
  lda ~00001100
  sta CONSOL           ; enable POT 01/00 (0b00000100)

  ; turn on DMA
  lda #$2e              ; 2e = DLIST + Normal Width + missile + player + pm
  sta sDMACTL           ; double resolution
                        ;

  lda #$3               ; 0b00000011 -> enable players / enable missiles
  sta GRACTL            ;

  lda #$00              ; 0b00010001 -> bit 4: 5th player / bit 0: priority 0
  sta PRIOR             ;

  lda #>pmgraphics
  sta PMBASE                  ; PM graphics base starts at 0800

  set the colors
  lda #$ef
  ; sta COLOR0
  sta COLPM0                  ; player0 color


  lda #$f0
  ; sta COLPM1
  ; sta COLOR1

  lda #$84
  ; sta COLPM2
  ; sta COLOR2

  lda #$f8
  ; lda #>charset_city
  sta CHBASE

  ; setup display list

  lda #<sdlist
  sta sDLISTL
  sta DLISTL
  lda #>sdlist
  sta sDLISTH
  sta DLISTH

  ; reset NMIs etc.
  lda #$03    ; point interrupt vector to
  sta $200    ; bios handler ($fc03)
  lda #$fc
  sta $201

  ; lda #<vbi_routine
  ; sta $202
  ; lda #>vbi_routine
  ; sta $203
  lda #$b8
  sta VVBLKI
  lda #$fc
  sta VVBLKI+1

  ; ; vbi deferred vector
  lda #<vbi_deferred
  sta VVBLKD
  lda #>vbi_deferred
  sta VVBLKD+1

  lda #$02
  sta VKYBDI
  lda #$fd
  sta VKYBDI+1

  ; vkpd routine? crashes unforto
  lda #<kbd_handler
  sta VKPD
  lda #>kbd_handler
  sta VKPD+1
  ;
  ;
  ;

  ldx #>dli
  ldy #<dli
  jsr init_dli

  ; setup NMI
  lda #$c0       ; VBI + DLI
  sta NMIEN
  ;
  ; enable interrupts
  ; what's 40?
  lda #$40
  sta POKMSK
  sta IRQEN
  lda #2
  sta SKCTL
  cli

  ; create a table
  ldx #$ff
.LOCAL
?loop
  dex
  txa
  sta chartable,x
  cpx #0
  bne ?loop


  lda #16
  sta hscroll_max
.LOCAL
forever
; check for pause
PAUSE
  lda is_paused
  cmp #$ff
  beq PAUSE

  ; jsr player_movex
  lda playerx
  jsr a2hexcode
  lda hexcode
  ldx #5
  sta potshow,x
  lda hexcode+1
  inx
  sta potshow,x
  inx
  inx
  ; jsr player_movey
  lda PADDL1
  jsr a2hexcode
  lda hexcode
  ldx #1
  sta potshow,x
  inx
  lda hexcode+1
  sta potshow,x
  lda playerx
  jsr a2hexcode
  ldx #$c
  lda hexcode
  sta sscoreline,x
  inx
  lda hexcode+1
  sta sscoreline,x
  lda playery
  jsr a2hexcode
  ldx #$15
  lda hexcode
  sta potshow,x
  inx
  lda hexcode+1
  sta potshow,x
  ; get last character pressed
  lda ch
  jsr a2hexcode
  lda hexcode
  sta potshow+$15
  lda hexcode+1
  sta potshow+$16


  ldx #delay
?loop
  lda RTCLOK+1
?wait
  cmp RTCLOK+1
  beq ?wait
  dex
  bne ?loop
  jsr fine_scroll_left
  jmp forever
;
.LOCAL
kbd_handler
  ; brk
  ldx keypad_debounce
  ldy #0
  sty keypad_debounce ; reset debounce
  cpx #$05            ; counter < 5?
  bcc ?done           ; skip key
  sta ch
  ; pause button?
  cmp #$0d
  beq ?togglepause
  cmp #$0e
  beq ?go_init
  jmp ?done
?go_init
  jmp init
?togglepause
  lda is_paused
  eor #$ff
  sta is_paused
?done
  jmp XITVBL


.LOCAL
a2hexcode
  ; converts byte in accumulator to two hex characters
  pha           ; save a
  ldx #0
?start
  lsr
  lsr
  lsr
  lsr           ; convert first byte to 0-16
  tay
  lda numbers,y
  sta hexcode,x
  inx
  pla
  and #$0f
  ; jmp ?start
  tay
  lda numbers,y
  sta hexcode,x
?done
  rts

;
init_dli
  sty VDSLST + 6
  stx VDSLST + 7

  ; activate disiplay list interrupt
  lda #NMIEN_VBI | NMIEN_DLI
  sta NMIEN
  rts

vbi_deferred
;
  ; fix colors
  lda #$ba
  sta COLOR0

  lda #$28
  sta COLOR2

  lda #$3a
  sta COLPF2
  ; reset CHBASE
  lda #$f8
  sta CHBASE
  inc COLBK
  ; lda RANDOM
  ; ldx #13
  ; sta potshow,x

  ; keypad debouncing
  lda keypad_debounce
  bmi ?nodebounce
  inc keypad_debounce
?nodebounce
  jsr player_movex
  jsr player_movey
  jsr print_trig1
  jsr print_trig2

  ; jsr player_movex
  ; jsr a2hexcode
  ; lda hexcode
  ; ldx #5
  ; sta potshow,x
  ; lda hexcode+1
  ; inx
  ; sta potshow,x
  ; inx
  ; inx
  ; jsr player_movey
  ; sta playery

  ; lda PADDL1
  ; jsr a2hexcode
  ; lda hexcode
  ; ldx #1
  ; sta potshow,x
  ; inx
  ; lda hexcode+1
  ; sta potshow,x
  ; lda playerx
  ; jsr a2hexcode
  ; ldx #$c
  ; lda hexcode
  ; sta sscoreline,x
  ; inx
  ; lda hexcode+1
  ; sta sscoreline,x
  ; lda playery
  ; jsr a2hexcode
  ; ldx #$15
  ; lda hexcode
  ; sta potshow,x
  ; inx
  ; lda hexcode+1
  ; sta potshow,x
  ; ; get last character pressed
  ; lda ch
  ; jsr a2hexcode
  ; lda hexcode
  ; sta potshow+$15
  ; lda hexcode+1
  ; sta potshow+$16
  jmp $fcb2

.LOCAL
fine_scroll_left
  inc hscroll_ctr
  lda hscroll_ctr
  cmp #4
  bne ?scroll
  jsr coarse_scroll_left
  lda #0
  sta hscroll_ctr
?scroll
  sta HSCROL
  rts

.LOCAL
coarse_scroll_left
  ldy #$6       ; max line
  ldx #$a       ; position of lms low byte
?loop
  dec sdlist,x
  inx
  inx
  inx
  dey
  bne ?loop
  rts

.LOCAL
clearpmgraphics
  pha
  phx
  ldx #$ff
  lda #0
?loop
  sta pmgraphics,x
  dex
  bne ?loop
  plx
  pla
  rts
print_trig1
  ldx TRIG0
  lda firetext,x
  sta sscoreline+36
  rts
print_trig2
  lda SKCTL
  ldx #2
  and #$08
  cmp #$08
  bne ?done
  ldx #3
?done
  lda firetext,x
  sta sscoreline+37
  rts

.LOCAL
player_movex
  lda PADDL0
  cmp #playerx_min
  bcc ?setmin
  cmp #playerx_max
  bcs ?setmax
  jmp ?writegraphics
?setmin
  lda #playerx_min
  jmp ?writegraphics
?setmax
  lda #playerx_max
  jmp ?writegraphics
?writegraphics
  sta HPOSP0
  sta playerx
  rts

.LOCAL
player_movey
  lda PADDL1                ; get current pot reading
  lsr                       ; divide by 2
  cmp #playery_min          ;
  bcc ?setmin               ; less than player_min? floor it
  cmp #playery_max
  bcs ?setmax               ; greater than player_max? ceiling it
  sta playery               ; otherwise, take as is
  jmp ?writegraphics        ; refresh the player graphics

?setmin
  lda #playery_min
  sta playery
  jmp ?writegraphics

?setmax
  lda #playery_max
  sta playery
  jmp ?writegraphics

?writegraphics
  jsr clearpmgraphics
  ldy playery
?writeloop
  lda pmgraphics_rom,x
  sta pmgraphics,y
  inx
  iny
  cpx #8
  bne ?writeloop
  jmp ?done

?done
  lda playery
  jsr a2hexcode
  lda hexcode
  ldx #$1b
  sta sscoreline,x
  inx
  lda hexcode+1
  sta sscoreline,x
  rts


  * = $5000
dlist
  .byte $70,$70,$70                   ; 0,1,2
  .byte $7|DL_LMS                     ; 3
  .word stopline                      ; 4,5
  .byte $3|DL_LMS|DL_DLI              ; 6
  .word sscoreline                    ; 7,8
  ; .byte $2|DL_DLI
  ; .byte $40|DL_LMS|DL_HSCROLL|DL_DLI
  ; .word chartable
  .byte $2|DL_LMS|DL_HSCROLL         ; 9
  .word chartable                    ; a,b
  .byte $3|DL_LMS|DL_HSCROLL         ;c
  .word chartable                    ;d,e
  .byte $4|DL_LMS|DL_HSCROLL         ;f
  .word chartable                    ;$10,11
  .byte $5|DL_LMS|DL_HSCROLL         ;$12
  .word chartable                    ;$13,$14
  .byte $6|DL_LMS|DL_HSCROLL         ;$15
  .word chartable                    ;$16,$17
  .byte $7|DL_LMS|DL_HSCROLL         ;$18
  .word chartable                    ;$19,$20
  ; .byte $8|DL_LMS|DL_HSCROLL         ;$21
  ; .word chartable                    ;$22,23
  ; .byte $8|DL_LMS|DL_HSCROLL|DLI         ;$24
  ; .word chartable                    ;$25,$26
  ; .byte $8|DL_LMS|DL_HSCROLL         ;$27
  ; .word chartable                    ;$28,$29
  ; .byte $8|DL_LMS|DL_HSCROLL         ;$2a
  ; .word chartable                    ;$2b,$2c
  ; .byte $8|DL_LMS|DL_HSCROLL         ;$2d
  ; .word chartable                    ;$2e,30
  ; .byte $9|DL_LMS|DL_HSCROLL
  ; .word chartable
  ; .byte $a|DL_LMS|DL_HSCROLL
  ; .word chartable
  ; .byte $b|DL_LMS|DL_HSCROLL
  ; .word chartable
  ; .byte $c|DL_LMS|DL_HSCROLL
  ; .word chartable
  ; .byte $d|DL_LMS|DL_HSCROLL
  ; .word chartable
  ; .byte $e|DL_LMS|DL_HSCROLL
  ; .word chartable
  ; .byte $f|DL_LMS|DL_HSCROLL
  ; .word chartable


  ;
  .byte $45
  .word blanks
  .byte $5,$5,$5                          ;,$5,$5,$5,$5,$5
  .byte $3|DL_LMS                               ;31
  .word potshow                           ;32,33
  .byte $41                               ;34
  .word sdlist                             ;35,36
;

.LOCAL
dli
  ; dli for lines
  pha             ; save A & X registers to stack
  txa
  pha
  sta WSYNC       ; first WSYNC gets us to start of scan line we want
  ; lda #$7a        ; new background color
  ; sta COLBK       ; store it in the hardware register
	; lda #>charset_city
  ; setup colors
  ; lda #$84
  ; sta COLBK
  ; lda #$08
  ; sta COLPF0
  ; lda #$00
  ; sta COLPF1
  ; lda #$86
  ; sta COLPF2
  ; lda #$56
  ; sta COLPF3
	; sta CHBASE
  ; ldx VCOUNT
  ; cpx #$40
  ; bcc ?tophalf
  ; lda #$ff
  ; sta COLPF0
  ; ;
  ; lda #$28
  ; sta COLPF1
  ; ;
  ; lda #$3a
  ; sta COLPF2
  ;
  ; lda #$0
  ; sta COLBK
  jmp ?done
?tophalf
  ; lda #$c0
  ; sta COLOR0
  ; sta COLPF0
  ; lda #$d0
  ; sta COLPF1
  ; lda #$e0
  ; sta COLPF2
  ; lda #$ff
  ; sta COLBK
  ; lda #>charset_city
  ; sta CHBASE
  ; lda numbers,x
  ; sta sscoreline
  ; sta WSYNC       ; sta doesn't affect processor flags
  ; bne ?loop       ; we are still checking result of dex
?done
  pla             ; restore X & A registers from stack
  tax
  pla
  rti             ; always end DLI with RTI!
  ; jsr $fc03
        ;rti




;
topline
  .sbyte "   JOYSTICK DEMO    "
scoreline
         ;0123456789012345678901234567890123456789
  .sbyte " JOYSTICK X:    JOYSTICK Y:   FIRE:     "
blanks
  .sbyte "                                      "
  .sbyte "                                        "
  .sbyte "                                        "
  .sbyte "                                        "
  .sbyte "                                        "
  .sbyte "                                        "
  .sbyte "                                        "
  .sbyte "                                        "
  .sbyte "                                        "
  .sbyte "                                        "
  .sbyte "                                        "

numbers
  .sbyte "0123456789ABCDEF"
skyline
  ; .byte $ac,$ae,$ae,$ae,$ac,$ae,$ae,$ae,$ac,$ae
  ; .byte $ac,$ae,$ae,$ae,$ac,$ae,$ae,$ae,$ac,$ae
  ; .byte $ac,$ae,$ae,$ae,$ac,$ae,$ae,$ae,$ac,$ae
  ; .byte $ac,$ae,$ae,$ae,$ac,$ae,$ae,$ae,$ac,$ae
  .sbyte "AAABBBCCCCDDDDEEEEFFFFGGGGHHHHJJJKKKLLLLMMMNNNNIIIIIJJJJ"


  * = $6000
pmgraphics_rom
  .byte   ~00000000
  .byte   ~00011000
  .byte   ~00011000
  .byte   ~01111110
  .byte   ~01111110
  .byte   ~00011000
  .byte   ~00011000
  .byte   ~00000000

firetext
  .byte $11,$0 ; "1"," "
  .byte $12,$0 ; "2"," "


  * = $6600

charset_city
  .include charset.asm
  * = $bfe8

  .sbyte "  JOYSTICK DEMO 1   "

  * = $bffc
  .byte $57,$50
  * = $bffd
  .byte $ff

  * = $bffe
  .word init
