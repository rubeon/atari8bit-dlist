; setup equates
POKMSK = $0
; vectors
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
GTIA      = $c000
TRIG0     = GTIA+$10        ;P1 trigger
TRIG1     = GTIA+$11        ;P2 trigger
TRIG2     = GTIA+$12        ;P3 trigger
TRIG3     = GTIA+$13        ;P4 trigger
PRIOR     = GTIA+$1b
GRACTL    = GTIA+$1d
CONSOL    = GTIA+$1f

COLPM0    = GTIA+$12
COLPM1    = GTIA+$13
COLPM2    = GTIA+$14
COLPM3    = GTIA+$15
COLPF0    = GTIA+$16
COLPF1    = GTIA+$17
COLPF2    = GTIA+$18
COLPF3    = GTIA+$19
COLBK     = GTIA+$1a


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

; some constants
DL_HSCROLL = $10
DL_VSCROLL = $20
DL_LMS     = $40
DL_DLI     = $80

; some data space in RAM
  * = $1000
stopline    .dc 20
spotshow    .dc 40
ch          .dc 1
hexcode     .dc 2
sscoreline  .dc 255 $70
playerx     .dc 1
playery     .dc 1

; pm graphics
  * = $2000
pmgraphics0   .dc $ff $ff
pmgraphics1   .dc $ff $ff

  * = $4000                             ; atari 5200 base address

init
  sei                                   ; clear interrupts
  cld                                   ; clear decimal?
  lda #$0
  sta NMIEN

  ldx #0
  lda #0

;
crloop1                                 ; clear the ram
  sta $00,x                             ; clear zero page
  sta ANTIC,x                           ; clear the ANTIC
  sta GTIA,x                            ; clear the GTIA
  sta POKEY,x                           ; clear the pokey
  dex
  bne crloop1

  ldy #$00
  lda #$02
  sta $81
  lda #$00
  sta $80                               ; $00,$02 $0200
crloop3
  sta ($80),y                           ; sta $0200-$02ff
  iny                                   ; clears out interrupt vectors
  bne crloop3

  inc $81                               ; =$03
  lda $81
  cmp #$40                              ; not at $4000 yet?
  bne crloop3
;
  lda #~00000100
  sta CONSOL            ; enable POT 01/00 (0b00000100)

  lda #~00101110        ;
  sta sDMACTL           ; double resolution
  lda #$3               ; 0b00000011 -> enable players / enable missiles
  sta GRACTL            ;
  lda #$01              ; 0b00010001 -> bit 4: 5th player / bit 0: priority 0
  sta PRIOR             ;
  ; set the colors
  lda #$ef
  sta COLOR0
  sta COLPM0                  ; player0 color


  lda #$ff
  sta COLPM1
  sta COLOR1

  lda #$84
  sta COLPM2
  sta COLOR2

  lda #$f8
  sta CHBASE

  ; setup display list
  lda #<dlist
  sta sDLISTL
  sta DLISTL
  lda #>dlist
  sta sDLISTH
  sta DLISTH

; setup vectors
; reset NMIs etc.
  lda #$03    ; point interrupt vector to
  sta $200    ; bios handler ($fc03)
  lda #$fc
  sta $201

  lda #$b8
  sta VVBLKI
  lda #$fc
  sta VVBLKI+1
  ; DLI handler
  lda #<dli
  sta VBREAK
  lda #>dli
  sta VBREAK+1
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

  ; setup NMI
  lda #$c0       ; VBI + DLI
  sta NMIEN

  lda #>pmgraphics
  sta PMBASE

  ; enable interrupts
  lda #$40
  sta POKMSK
  sta IRQEN

  lda #2
  sta SKCTL
  cli


; cache dynamic strings in RAM
; copy topline
.LOCAL
  ldx #19
topline_loop
  lda topline,x
  sta stopline,x
  dex
  bpl topline_loop

; copy scoreline
.LOCAL
  ldx #0
scoreline_loop
  lda scoreline,x
  sta sscoreline,x
  inx
  cpx #40
  bcc scoreline_loop
  ;
?done
; copy potshow
.LOCAL
  ldx #39
?loop
  dex
  lda potshow,x
  sta spotshow,x
  cpx #0
  bne ?loop

forever
  jmp forever

vbi_deferred
  ; restore default background color
  ; lda #$84
  ; sta COLOR2
  ; place player

  lda PLAYERX
  sta HPOSP0

  jmp $fcb2

.LOCAL
write_player
  ldy PLAYERY
  sta pmgraphics,y
  rts
.LOCAL
dli
  lda VCOUNT
  ; cmp #20
  ; bcc ?top
  ; lda #$c0
  ; sta COLOR2
  lda PADDL0
  sta playerx
  jsr a2hexcode
  lda hexcode
  sta COLOR2
  ldy #8
  sta spotshow,y
  lda hexcode+1
  sta spotshow+1,y
  lda PADDL1
  sta playery
  jsr a2hexcode
  lda hexcode
  ldy #22
  sta spotshow,y
  lda hexcode+1
  sta spotshow+1,y

  jmp ?done
; ?top
;   lda #
?done
  rti

; utility

.LOCAL
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


kbd_handler
  sta ch
  jmp XITVBL

  * = $5000
;
dlist
  .byte $70,$70,$70
  .byte $47
  .word stopline
  .byte $2|DL_LMS
  .word sscoreline
  ; .byte $5,$5,$5,$5,$5,$5,$5,$5,$5,$5
  .byte 3,3,3,3,3,3,3,3
  .byte $3|DL_DLI|DL_LMS
  .word spotshow
  .byte $41
  .word dlist

topline
  .sbyte "  JOYSTICK DEMO 6   "

scoreline
         ;0123456789012345678901234567890123456789
  .sbyte " JOYSTICK X:$00 JOYSTICK Y:$00 FIRE:    "
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

potshow
         ;0123456789012345678901234567890123456789
  .sbyte "PADDL0:       PADDL1:                   "
  .sbyte "Following on that..."


  * = $5200
pmgraphics_rom
  .byte   ~00000000
  .byte   ~00011000
  .byte   ~00011000
  .byte   ~01111110
  .byte   ~01111110
  .byte   ~00011000
  .byte   ~00011000
  .byte   ~00000000
;

; lonely
pmgrapics_lonely
  .byte   ~000000000
  .byte   ~000110000
  .byte   ~001111000
  .byte   ~011010100
  .byte   ~011111100
  .byte   ~111111111
  .byte   ~110011001
  .byte   ~110011001

numbers
  .sbyte "0123456789ABCDEF"

; atari cart housekeeping
  * = $bfe8
         ;01234567890123456789
  .sbyte "  JOYSTICK DEMO "
  .byte                 $56
  .sbyte                   "  "

  * = $bffc
  .byte $57,$50
  * = $bffd
  .byte $ff

  * = $bffe
  .word init
