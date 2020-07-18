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

POTGO     = POKEY+$b

SKCTL     = POKEY+$f

TRIG0     = GTIA+$10        ;P1 trigger
TRIG1     = GTIA+$11        ;P2 trigger
TRIG2     = GTIA+$12        ;P3 trigger
TRIG3     = GTIA+$13        ;P4 trigger

stopline  = $1000
sscoreline= stopline+20

playerx         = sscoreline + 13
playery         = sscoreline + 27
playertrig      = sscoreline + 36

player_config   = $1100      ; 32(?) bytes for player configuration

deadzone_right  = player_config
deadzone_left   = player_config+1
deadzone_up     = player_config+2
deadzone_down   = player_config+3

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

  ; init POKEY
  lda #$03
  sta SKCTL
  lda #$ff
  sta POTGO
  sta SKCTL


  ; setup deadzones
  lda #90
  sta deadzone_left
  sta deadzone_down
  lda #130
  sta deadzone_right
  sta deadzone_up

forever
  lda #$03
  jsr waitvb
  jsr Player_ReadControlsDual
  lda #$ff
  sta POTGO
  jmp forever


;
waitvb
        lda     $02     ;Read timer (this is incremented during VB)
waitvb2
        cmp     $02         ;Did it change?
        beq     waitvb2     ;If not keep waiting
        rts

firetext
  .byte $11,$0               ; "1"," "
  .byte $12,$0               ; "2"

.LOCAL
print_trig2
  lda SKCTL
  ldx #2
  and #$08
  cmp #$08                   ; if SKCTL==$8 then fire2?
  bne ?done
  ldx #3
?done
  lda firetext,x
  ; ldx playertrig+1
  sta playertrig+1
  rts

.LOCAL
print_playerleft
  lda POT0
  cmp deadzone_left
  bcc ?is_left
  lda #0
  jmp ?done
?is_left
  lda #$5e
?done
  sta playerx
  rts

;
.LOCAL
print_playerright
  lda POT0
  cmp deadzone_right
  bcs ?is_right
  lda #0
  jmp ?done
?is_right
  lda #$5f
?done
  sta playerx
  rts

;
.LOCAL
print_playerdown
  lda POT1
  cmp deadzone_down
  bcc ?is_down
  lda #0
  jmp ?done
?is_down
  lda #$5c
?done
  sta playery
  rts
;
.LOCAL
print_playerup
  lda POT1
  cmp deadzone_up
  bcs ?is_up
  lda #0
  jmp ?done
?is_up
  lda #$5d
?done
  sta playery
  rts


Player_ReadControlsDual
  ldx TRIG0
  lda firetext,x            ; it works!
  sta playertrig
  jsr print_trig2

;x
.LOCAL
  lda POT0
  cmp #110
  bcs goright
  jsr print_playerleft
  jmp ?done
goright
  jsr print_playerright
?done

.LOCAL
  lda POT1
  cmp #110
  bcs goup
  jsr print_playerdown
  jmp ?done
goup
  jsr print_playerup
?done
  ; all done with the input handler
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
         ;0123456789012345678901234567890123456789
  .sbyte " JOYSTICK X: 00 JOYSTICK Y:00 FIRE: -   "
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
