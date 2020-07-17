; demo of reading joysticks, as I was unable to make this work previous

POKEY     = $e800
GTIA      = $c000
ANTIC     = $d400
WSYNC     = ANTIC+$0a
NMIEN     = ANTIC+$0e
sDLISTL   = $05
sDLISTH   = $06
DMACTL    = ANTIC
sDMACTL   = $07

CHACTL    = ANTIC+$1
CHBASE    = ANTIC+$09

COLOR0    = $0c
COLOR1    = $0d
COLOR2    = $0e
COLOR3    = $0f

POT0      = POKEY           ; P1 X
POT1      = POKEY+$1        ; P1 Y
POT2      = POKEY+$2        ; P2 X
POT3      = POKEY+$4        ; P2 Y
POT4      = POKEY+$4        ; P3 X
POT5      = POKEY+$5        ; P3 Y
POT6      = POKEY+$6        ; P4 X
POT7      = POKEY+$7        ; P4 Y

TRIG0     = GTIA+$10        ;P1 trigger
TRIG1     = GTIA+$11        ;P2 trigger
TRIG2     = GTIA+$12        ;P3 trigger
TRIG3     = GTIA+$13        ;P4 trigger

stopline  = $1000
sscoreline= stopline+20
z_as      = sscoreline+14
z_h       = sscoreline+28

  * = $4000

init
  sei
  clc

  ; shadowfy topline
  ldx #0
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



  lda #$03    ; point interrupt vector to
  sta $200    ; bios handler ($fc03)
  lda #$fc
  sta $201

  lda #$b8    ; point VBI fector to
  sta $202    ; BIOS handler ($fcb8)
  lda #$fc
  sta $203

  lda #$b2    ; point deferred VBI vector
  sta $204    ; to BIOS routine
  lda #$fc
  sta $205

  lda #$ef
  sta COLOR0
  lda #$0f
  sta COLOR1
  lda #$84
  sta COLOR2

  lda #<dlist   ; set DLIST pointer to our dlist
  sta sDLISTL
  lda #>dlist
  sta sDLISTH

  lda #$f8
  sta CHBASE    ; set pointer to default charset

  lda #$22
  sta sDMACTL   ; set DMA to DLIST + normal playfield width

  lda #$40
  sta NMIEN     ; enable DLI

forever
  jsr waitvb
  jsr Player_ReadControlsDual
  jmp forever

;
waitvb
        lda     $02     ;Read timer (this is incremented during VB)
waitvb2
        cmp     $02         ;Did it change?
        beq     waitvb2     ;If not keep waiting
        rts


Player_ReadControlsDual
  lda TRIG0
  sta z_as
  lda POT0
  jsr Player_ReadControlsProcessAnalog
  lda POT1
  jsr Player_ReadControlsProcessAnalog

  lda #'11100000
  ora z_as
  sta z_h

; convert analog to digital
Player_ReadControlsProcessAnalog
  ; Accumulator contains address to process (e.g., POT0)
  cmp #$ff-$40
  bcs Player_ReadControlsProcessHigh
  cmp #$40
  bcc Player_ReadControlsProcessLow
  sec
  rol z_h
  sec
  rol z_h
  rts
Player_ReadControlsProcessHigh    ; U/R
  clc
  rol z_as
  sec
  rol z_as
  rts
Player_ReadControlsProcessLow     ; D/L
  sec
  rol z_as
  clc
  rol z_as
  rts

  * = $5000
dlist
  .byte $70,$70,$70
  .byte $47
  .word stopline
  .byte $43
  .word sscoreline
  .byte $5,$5,$5,$5,$5,$5,$5,$5,$5,$5
  .byte $43
  .word POKEY
  .byte $41
  .word dlist

topline
  .sbyte "   JOYSTICK DEMO    "
scoreline
  .sbyte " JOYSTICK X: 00 JOYSTICK Y:00 FIRE: OFF "
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


  * = $bfe8

  .sbyte "  JOYSTICK DEMO 1   "

  * = $bffc
  .byte $57,$50
  * = $bffd
  .byte $ff

  * = $bffe
  .word init
