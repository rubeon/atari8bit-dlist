; demo of reading joysticks, as I was unable to make this work previous

POKEY     = $e800
GTIA      = $c000
PRIOR     = GTIA+$1b
GRACTL    = GTIA+$1d
CONSOL    = GTIA+$1f

ANTIC     = $d400
CHACTL    = ANTIC+$1
PMBASE    = ANTIC+$7
CHBASE    = ANTIC+$9

WSYNC     = ANTIC+$0a
NMIEN     = ANTIC+$0e
sDLISTL   = $05
sDLISTH   = $06
DMACTL    = ANTIC
sDMACTL   = $07

POKMSK    = $0

VVBLKD    = $204
VKPD      = $20a

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
  lda #$00
  sta NMIEN

;
; move screen data to ram
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

  lda #$04
  sta CONSOL

  lda #$2e
  sta sDMACTL

  lda #3
  sta GRACTL

  lda #$11
  sta PRIOR

  lda #$08
  sta PMBASE

  lda #<dlist
  sta sDLISTL

  lda #>dlist
  sta sDLISTH

  ; set the colors
  lda #$ef
  sta COLOR0

  lda #$0f
  sta COLOR1

  lda #$84
  sta COLOR2

  lda #$f8
  sta CHBASE
  ;
  ; lda #$be
  ; sta VVBLKD
  ;
  ; lda #$aa
  ; sta VVBLKD+1
  ;
  lda #<vbid
  sta VKPD
  lda #>vbid
  sta VKPD+1

  ; reset NMIs etc.
  lda #$03    ; point interrupt vector to
  sta $200    ; bios handler ($fc03)
  lda #$fc
  sta $201

  lda #$b8    ; point VBI vector to
  sta $202    ; BIOS handler ($fcb8)
  lda #$fc
  sta $203

  lda #$b2    ; point deferred VBI vector
  sta $204    ; to BIOS routine
  lda #$fc
  sta $205

  ; ; VKPD gonna get it
  ; lda #<vkpd_thing
  ; sta VKPD
  ; lda #>vkpd_thing
  ; sta VKPD+1

  lda #$c0
  sta NMIEN

  lda #$40
  sta POKMSK      ; shadow
  sta IRQEN       ; hw register

  cli             ; clear interrupt disable

  lda #$00
  sta CONSOL

  sta $3c
  lda $02
  ; adc #$0a
  lda #$02
  sta SKCTL

padloop
  cmp $02
  beq padloop     ; waiting for vblank

  lda PADDL0         ; paddl0
  cmp #$e4           ; trackball?

  inc $3c
  lda PADDL0
  sta $e9
  lda PADDL1
  sta $ea
  lda $3c             ; this can all be skipped until I figure shit out

forever
  jsr zeropots
  jsr waitvb
  ; jsr Player_ReadControlsDual
  jsr handle_playerx
  jsr showpot
  jmp forever

; VBlank
waitvb
        lda     $02     ;Read timer (this is incremented during VB)
waitvb2
        cmp     $02         ;Did it change?
        beq     waitvb2     ;If not keep waiting
        rts

exitvbv
  PLA
  TAY
  PLA
  TAX
  PLA
  RTI


vkpd_thing
  ; handles keyboard interrupts maybe?
  ; ldx #10
  ; sta potshow,x
  ; jmp exitvbv
  rti


vbid
  rti

;
firetext
  .byte $11,$0               ; "1"," "
  .byte $12,$0               ; "2"

.LOCAL
handle_trig2
  lda SKCTL
  ldx #2
  and #$08
  cmp #$08                   ; if SKCTL==$8 then fire2
  bne ?done
  ldx #3
?done
  lda firetext,x
  sta playertrig+1
  rts


.LOCAL
handle_playerx
  ; brk
  ; takes care of POT0 (player left/right)
  lda POT0
  ; is it below deadzone left?
  cmp deadzone_left
  bcc ?is_left
  ; or is it above deadzone right?
  cmp deadzone_right
  bcs ?is_right
  lda #0
  jmp ?done
?is_left
  lda #$5e        ; left arrow
  jmp ?done
?is_right
  lda #$5f      ; right arrow
?done
  sta playerx
  rts

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
  jsr handle_trig2
  jsr handle_playerx
  ; jsr showpot
  rts


;x
.LOCAL
  lda PADDL0
  cmp #110
  bcs goright
  ; jsr print_playerleft
  jmp ?done
goright
  ; jsr print_playerright
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
  ldx #3
  pha             ;Save the BCD value
  lsr A           ;Shift the four most significant bits
  lsr A           ;... into the four least significant
  lsr A
  lsr A
  tay
  lda numbers,y
  sta potshow,x
  pla             ;Recover the BCD value
  and #$0F        ;Mask out all but the bottom 4 bits
  tay
  lda numbers,y
  inx
  sta potshow,x
  rts

.LOCAL
zeropots
  ldy #$07
  lda #$00
?loop
  sta POKEY,y
  dey
  bpl ?loop
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

  * = $bfe8

  .sbyte "  JOYSTICK DEMO 1   "

  * = $bffc
  .byte $57,$50
  * = $bffd
  .byte $ff

  * = $bffe
  .word init
