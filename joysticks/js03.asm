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


PRIOR     = GTIA+$1b
GRACTL    = GTIA+$1d
CONSOL    = GTIA+$1f


ANTIC     = $d400
CHACTL    = ANTIC+$1
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


; zero page variables

; ram variables
  * = $1000
vbicounter  .ds $1
ch          .ds $1
hexcode     .ds $2
stopline    .ds $14
potshow     .ds $28
sscoreline  .ds $28
  * = $1200
pmgraphics  .ds $800

  * = $4000

init
  sei
  lda #$0
  sta NMIEN     ; disable interrupts

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
  cpx #40
  bne cpscoreline
;
  ; ldx #0
  ; ldy #<vblankd
  ; sty VVBLKD
  ; ldx #>vblankd
  ; stx VVBLKD+1

  ; keyboard interrupts?
  ; ignore for now

.LOCAL
fudgepmgraphics
  ldx #2047
  lda #$ff
?loop
  sta pmgraphics,x
  eor #$ff
  dex
  bne ?loop

.LOCAL
cppmgraphics
  ldx #0
  ldy #1200
?loop
  lda pmgraphics_rom,x
  sta pmgraphics,y
  iny
  inx
  cpx #8
  bne ?loop



  lda #$4
  sta CONSOL           ; enable POT 01/00 (0b00000100)

  ; turn on DMA
  lda #$2e              ; 2e = DLIST + Normal Width + missile + player + pm
  sta sDMACTL           ; double resolution
                        ;

  lda #$3               ; 0b00000011 -> enable players / enable missiles
  sta GRACTL            ;

  lda #$01              ; 0b00010001 -> bit 4: 5th player / bit 0: priority 0
  sta PRIOR             ;

  lda #>pmgraphics
  sta PMBASE                  ; PM graphics base starts at 0800

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
  ; setup NMI
  lda #$c0       ; VBI + DLI
  sta NMIEN

  ; enable interrupts
  lda #$40
  sta POKMSK
  sta IRQEN

  lda #2
  sta SKCTL
  cli

forever
  jmp forever

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
  cpx #2
  beq ?done
  pla
  and #$0f
  jmp ?start
?done
  rts





vbi_deferred


  ; inc vbicounter
  ; lda vbicounter
  inc COLBK
  lda RANDOM
  ldx #13
  sta potshow,x
  lda PADDL0
  sta HPOSP0
  jsr a2hexcode
  lda hexcode
  ldx #5
  sta potshow,x
  lda hexcode+1
  inx
  sta potshow,x
  inx
  inx
  lda PADDL1
  jsr a2hexcode
  lda hexcode
  sta potshow,x
  inx
  lda hexcode+1
  sta potshow,x
  jmp $fcb2

kbd_handler
  sta ch
  jmp XITVBL

  * = $5000
dlist
  .byte $70,$70,$70
  .byte $47
  .word stopline
  .byte $43
  .word sscoreline
  .byte $5,$5,$5,$5,$5,$5,$5,$5,$5,$5
  .byte $43
  .word potshow
  .byte $41
  .word dlist

;
topline
  .sbyte "   JOYSTICK DEMO    "
scoreline
         ;0123456789012345678901234567890123456789
  .sbyte " JOYSTICK X:    JOYSTICK Y:   FIRE:     "
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

  * = $5200
pmgraphics_rom
  .byte   '00000000
  .byte   '00011000
  .byte   '00011000
  .byte   '01111110
  .byte   '01111110
  .byte   '00011000
  .byte   '00011000
  .byte   '00000000

  * = $bfe8

  .sbyte "  JOYSTICK DEMO 1   "

  * = $bffc
  .byte $57,$50
  * = $bffd
  .byte $ff

  * = $bffe
  .word init
