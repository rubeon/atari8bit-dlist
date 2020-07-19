; demo of reading joysticks, as I was unable to make this work previous

POKEY     = $e800
GTIA      = $c000
CONSOL    = GTIA+$1f
ANTIC     = $d400
WSYNC     = ANTIC+$0a
NMIEN     = ANTIC+$0e
sDLISTL   = $05
sDLISTH   = $06
DMACTL    = ANTIC
sDMACTL   = $07

CHACTL    = ANTIC+$1
CHBASE    = ANTIC+$9

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

stopline  = $1000
sscoreline= stopline+20

playerx         = sscoreline + 13
playery         = sscoreline + 27
playertrig      = sscoreline + 36

potshow = $1500

player_config   = $2000      ; 32(?) bytes for player configuration

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
;
  ldx #0
cpscreenlines
  ;
  lda scoreline+60,x
  sta sscoreline+60,x
  inx
  cpx #200
  bne cpscreenlines



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
  ; lda #$ff
  ; sta POTGO
  ; sta SKCTL
  ; turn on the pots in CONSOL
  ; xxxx3210
  ; js1 = $0
  ; enablepots = $4
  lda #'00001100
  sta CONSOL


  ; setup deadzones
  lda #90
  sta deadzone_left
  sta deadzone_down
  lda #130
  sta deadzone_right
  sta deadzone_up

forever
  lda #$ff
  sta POTGO
  jsr waitvb
  jsr Player_ReadControlsDual
  lda #$ff
  sta SKCTL
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
  ldx #2                     ; '2'
  and #$08
  cmp #$08                   ; if SKCTL==$8 then fire2
  bne ?done
  ldx #3                     ;' ' otherwise, no fire2
?done
  lda firetext,x
  ; ldx playertrig+1
  sta playertrig+1
  rts

.LOCAL
print_playerleft
  lda PADDL0
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
  lda PADDL0
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
  lda PADDL1
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
  lda PADDL1
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
  lda PADDL0
  cmp #110
  bcs goright
  jsr print_playerleft
  jmp ?done
goright
  jsr print_playerright
?done

.LOCAL
  lda PADDL1
  cmp #110
  bcs goup
  jsr print_playerdown
  jmp ?done
goup
  jsr print_playerup
?done
  lda PADDL0
  ldx #0
  jsr showpot
  ; all done with the input handler
  lda PADDL1
  ldx #3
  jsr showpot
  rts

;
; Print the BCD value in A as two ASCII digits
showpot
  ; x has the offset to print to
  pha             ;Save the BCD value
  lsr A           ;Shift the four most significant bits
  lsr A           ;... into the four least significant
  lsr A
  lsr A
  tay
  lda numbers,y
  ; ora #'0'        ;Make an ASCII digit
  ; jsr PRINT       ;... and print it
  sta potshow,x
  pla             ;Recover the BCD value
  and #$0F        ;Mask out all but the bottom 4 bits
  tay
  lda numbers,y
  ; ora #'0'        ;Make an ASCII digit
  ; jsr PRINT       ;... and print it
  inx
  sta potshow,x
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
  .word potshow
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

numbers
  .sbyte "0123456789ABCDEF"

carttitle
  * = $bfd4
  .sbyte   "@@@HcI@RPQT@sioR@@@@"
  .sbyte  "@@@*/934)#+@4%34@@@@"
  .BYTE   $B5


  * = $bfe8

  .sbyte "  JOYSTICK DEMO 1   "

  * = $bffc
  .byte $57,$50
  * = $bffd
  .byte $ff

  * = $bffe
  .word init
