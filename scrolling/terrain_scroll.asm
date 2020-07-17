DLISTL  = $D402			; display list lo
DLISTH  = $D403			; display list hi
CHACTL  = $D401			; Character control
HSCROL  = $d404
VSCROL  = $d405
CHBASE  = $d409
CHBAS   = $02f4
sDLISTL = $05
sDLISTH = $06
DMACTL  = $D400			; DMA control
sDMACTL	= $07			; DMA Control Shadow
NMIEN   = $D40E			; NMI enable
WSYNC   =	$D40A
COLOR0  = 	$0c
COLOR1  =	$0d			; Color 1 shadow
COLOR2  =	$0e			; Color 2 shadow
COLOR3	=	$0f
RTCLOK = $01
RTCLOKH = $01
RTCLOKL = $02
XITVBV = $fcb2
SCRBASE = $3000
DLBASE  = $3a00

GTIA = $c000
TRIG0=GTIA+$a
TRIG1=GTIA+$b
TRIG2=GTIA+$c
TRIG3=GTIA+$d



COLPF0 = GTIA+16
COLPF1 = GTIA+17
COLPF2 = GTIA+18
COLPF3 = GTIA+19


STRIG0=$284
STRIG1=$285
STRIG2=$286
STRIG3=$287
; pokeys
POKEY=$e800
POT0=POKEY
POT1=POKEY+1
POT2=POKEY+2
POT3=POKEY+3
POT4=POKEY+4
POT5=POKEY+5
POT6=POKEY+6
POT7=POKEY+7
SKCTL=POKEY+$a

sPOT0=$270
sPOT1=sPOT0+1
sPOT2=sPOT1+1
sPOT3=sPOT2+1
sPOT4=sPOT3+1
sPOT5=sPOT4+1
sPOT6=sPOT5+1
sPOT7=sPOT6+1

stopline = $3c00

;vars
vert_scroll = $90       ; variable used to store VSCROL value
vert_scroll_max = 8     ; ANTIC mode 4 has 8 scan lines
delay = $5
horz_scroll_max = 4     ; ANTIC mode 4 has 4 color clocks
delay_count = $80       ; counter for scrolling updates

vert_scroll = $90       ; variable used to store VSCROL value
horz_scroll = $91       ; variable used to store HSCROL value

pressed = $a0           ; user still pressing button?
latest_joystick = $a1   ; last joystick direction processed
joystick_y = $a2        ; down = 1, up=$ff, no movement = 0
joystick_x = $a3        ; right = 1, left=$ff, no movement = 0
vscroll_x2 = $a4        ; twice vertical scrolling? no = 0, yes = $ff


  * = $4000
init
  sei
  ; point IRQ vector to bios routine
  lda #$03
  sta $200
  lda #$fc
  sta $201

  ; point vbi vector to bios routine
  lda #$b8
  sta $202
  lda #$fc
  sta $203

  ; point deferred vbi vector to bios routine
  ; lda #$b2
  lda #<vbi
  sta $204
  lda #>vbi
  ; lda #$fc
  sta $205

  ; set the foreground color, bg color,  and color for mode 7
  lda #$ef
  sta COLOR0
  lda #$0f
  sta COLOR1
  lda #$84
  sta COLOR2

  lda #$da
  sta COLPF0
  lda #$a4
  sta COLPF1
  lda #$06
  sta COLPF2
  lda #$a8
  sta COLPF3


  ; default charset
  lda #$f8
  sta CHBASE

  ; point to the DLI
  lda #<dli
  sta $206
  lda #>dli
  sta $207
  ;
  ; turn on DMA via the shadow register
  ; $22 = DLIST & NARROW PF
  lda #$2e
  sta sDMACTL

  ; turn on NMI
  ; $40 = VBI
  ; $80 = DLI
  ; $40 + $80 = $c0
  lda #$c0
  sta NMIEN

  ldx #0
cpdloop
  lda dlist,x
  sta DLBASE,x
  inx
  cpx #41
  bne cpdloop

  ; dlist pointers
  lda #<DLBASE
  sta sDLISTL
  lda #>DLBASE
  sta sDLISTH

  ; generate screen memory
  ldx #0
  lda #0          ; initialize vertical scrolling value
  sta vert_scroll
  sta VSCROL      ; initialize hardware register

cploop
  lda terrain_charset,x
  sta SCRBASE,x
  inx
  cpx #$ff
  bne cploop

  ldx #0
  lda #0
  sta horz_scroll
  sta HSCROL      ; initialize hardware register

  lda #0          ; initialize vertical scrolling value
  sta vert_scroll
  sta VSCROL      ; initialize hardware register

  lda #0
  sta pressed

  lda #delay      ; number of VBLANKs to wait
  sta delay_count
  sta latest_joystick
  sta joystick_y
  sta joystick_x

  lda #2
  sta SKCTL
;   ; mainloop
; forever
;   jmp forever
  ldx #0
stloop
  lda topline,x
  sta stopline,x
  cpx #40
  bne stloop

loop    ldx #delay      ; number of VBLANKs to wait
?start  lda RTCLOK+1    ; check fastest moving RTCLOCK byte
?wait   cmp RTCLOK+1    ; VBLANK will update this
        beq ?wait       ; delay until VBLANK changes it
        dex             ; delay for a number of VBLANKs
        bpl ?start

        ; enough time has passed, scroll one scan line
        jsr fine_scroll_down

        jmp loop

  * = $b000

dlist
  .byte $70,$70,$70         ; 3/ 24 blank lines
  .byte $c3                 ; 4/ DLI LMS Mode 3 ($40 + $80)
  .word stopline            ; 6/ address of title text
  .byte $f5                 ; 7/ LMS Mode 4
  .word scr1                ; 9/ screen text mem location
  .byte $f5                 ;10/
  .word scr1+$80            ;12
  .byte $f5                 ;13
  .word scr1+$100           ;15
  .byte $f5                 ;16
  .word scr1+$180           ;18
  .byte $f5                 ;19
  .word scr1+$200           ;21
  .byte $f5                 ;22
  .word scr1+$280           ;23
  .byte $f5                 ;24
  .word scr1+$300           ;26
  .byte $f5,                ;27
  .word scr1+$380           ;29
  .byte $f5                 ;30
  .word scr1+$400           ;32
  .byte $f5                 ;33
  .word scr1+$480           ;35

  .byte $41                 ;36  jump
  .word DLBASE               ;38 to top of dlist

dli
  pha
  phx
  lda #>terrain_charset
  ; lda #$f8
  sta CHBASE
  ; brk
  lda #3
  sta VSCROL
dli_done
  plx
  pla
  rti

; vbi
  ; lda #$f8
  ; sta CHBASE
; vbi_done
  ; jmp XITVBV
  ; restore default CHBASE
;
fine_scroll_down
        inc vert_scroll
        lda vert_scroll
        cmp #vert_scroll_max ; check to see if we need to do a coarse scroll
        bcc ?done       ; nope, still in the middle of the character
        ; jsr coarse_scroll_down ; yep, do a coarse scroll...
        lda #0          ;  ...followed by reseting the vscroll register
        sta vert_scroll
?done   sta VSCROL      ; store vertical scroll value in hardware register
        ; brk
        rts


;
.LOCAL
fine_scroll_right
        dec horz_scroll
        lda horz_scroll
        bpl ?done       ; if non-negative, still in the middle of the character
        ; jsr coarse_scroll_right ; wrapped to $ff, do a coarse scroll...
        lda #horz_scroll_max-1  ;  ...followed by reseting the HSCROL register
        sta horz_scroll
?done   rts

; move viewport one byte to the right by pointing each display list start
; address to one byte higher in memory

; scroll one color clock left and check if at HSCROL limit, returns
; HSCROL value in A
.LOCAL
fine_scroll_left
        inc horz_scroll
        lda horz_scroll
        cmp #horz_scroll_max ; check to see if we need to do a coarse scroll
        bcc ?done       ; nope, still in the middle of the character
        ; jsr coarse_scroll_left ; yep, do a coarse scroll...
        lda #0          ;  ...followed by reseting the HSCROL register
        sta horz_scroll
?done   rts


;
.LOCAL
vbi
  ; jsr check_console ; handle OPTION & SELECT keys for control changes
  ; brk
  jsr record_joystick ; check joystick for scrolling direction
  dec delay_count ; wait for number of VBLANKs before updating
  bne ?exit       ;   fine/coarse scrolling

  jsr process_joystick ; update scrolling position based on current joystick direction

  lda #delay      ; reset counter
  sta delay_count
?exit
  jmp XITVBV      ; exit VBI through operating system routine

;
.LOCAL
record_joystick
        ; lda STICK0      ; check joystick
        lda sPOT0
        ; brk
        cmp #4
        bcs ?fast       ; only store if a direction is pressed
        sta latest_joystick
?fast   lda STRIG0      ; easter egg: check trigger
        bne ?done       ; not pressed
        lda #1          ; pressed = ludicrous speed!
        sta delay_count
?done   rts

.LOCAL
process_joystick
        lda #0                  ; clear joystick movement vars
        sta joystick_x
        sta joystick_y
        lda latest_joystick     ; bits 3 - 0 = right, left, down, up
        ror a                   ; put bit 0 (UP) in carry
        bcs ?down               ; carry clear = up, set = not pressed
        dec joystick_y
?down   ror a                   ; put bit 1 (DOWN) in carry
        bcs ?left
        inc joystick_y
?left   ror a                   ; put bit 2 (LEFT) in carry
        bcs ?right
        dec joystick_x
?right  ror a                   ; put bit 3 (RIGHT) in carry
        bcs ?next
        inc joystick_x
?next   lda #0
        sta latest_joystick     ; reset joystick
        lda joystick_x  ; check horizontal scrolling
        beq ?updown     ; zero means no movement, move on to vert
        bmi ?left1      ; bit 7 set ($ff) means left
        jsr fine_scroll_right ; otherwise, it's right
        jmp ?storeh
?left1  jsr fine_scroll_left
?storeh sta HSCROL      ; store vertical scroll value in hardware register

        clc             ; convert scroll value...
        adc #$90        ;   to ATASCII text and...
        sta stopline+29 ;   store on screen

?updown lda joystick_y  ; check vertical scrolling
        beq ?done       ; zero means no movement, we're done
        bmi ?up1        ; bit 7 set ($ff) means up
        jsr fine_scroll_down ; otherwise, it's down
        jmp ?storev
?up1    jsr fine_scroll_up
?storev sta VSCROL      ; store vertical scroll value in hardware register
        clc             ; convert scroll value...
        adc #$90        ;   to ATASCII text and...
        sta stopline+38 ;   store on screen
?done   rts

.LOCAL
; scroll one scan line up and check if at VSCROL limit
fine_scroll_up
        dec vert_scroll
        lda vert_scroll
        bpl ?done       ; if non-negative, still in the middle of the character
        ; jsr coarse_scroll_up   ; wrapped to $ff, do a coarse scroll...
        lda #vert_scroll_max-1 ;  ...followed by reseting the vscroll register
        sta vert_scroll
?done
        lda #vert_scroll_max
        sta VSCROL      ; store vertical scroll value in hardware register
        rts

  * = $5000
scr1
  ;
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$64,$10,$11
  .byte $61,$61,$28,$29,$74,$71,$71,$71
  .byte $70,$70,$70,$75,$28,$29,$60,$60
  .byte $10,$11,$65,$5c,$5c,$5c,$5d,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5d,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5d
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5d,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$dc,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$dc,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5d,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$2c,$2e,$2f,$2d,$5c,$5d
  .byte $5c,$5c,$2c,$2e,$2f,$2d,$5c,$5c
  .byte $5c,$5c,$2c,$2e,$2f,$2d,$5c,$5c
  .byte $5c,$5c,$2c,$2e,$2f,$2d,$5d,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5d,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5d
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5d,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$62,$61,$61,$61,$61,$61
  .byte $61,$61,$30,$32,$33,$31,$61,$61
  .byte $61,$61,$30,$32,$33,$31,$61,$61
  .byte $60,$60,$30,$32,$33,$31,$60,$60
  .byte $60,$60,$30,$32,$33,$31,$60,$60
  .byte $60,$60,$60,$60,$60,$63,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5e
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5e,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$61,$61,$54,$58,$59,$55
  .byte $61,$72,$71,$71,$71,$71,$71,$71
  .byte $71,$71,$71,$71,$71,$71,$71,$71
  .byte $70,$70,$70,$70,$70,$70,$70,$70
  .byte $70,$70,$70,$70,$70,$70,$73,$60
  .byte $54,$58,$59,$55,$60,$60,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$dc,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$dc
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5d,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$61,$61,$0c,$0d,$0c,$0d
  .byte $61,$71,$56,$5a,$5b,$57,$71,$71
  .byte $71,$26,$27,$71,$86,$87,$88,$8a
  .byte $8b,$89,$86,$87,$70,$26,$27,$70
  .byte $70,$70,$56,$5a,$5b,$57,$70,$60
  .byte $0c,$0d,$0c,$0d,$60,$60,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5d,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5d,$5c,$5c,$5c,$5c,$5e,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5e,$5c,$5c,$5c,$5c
  .byte $5c,$dc,$64,$61,$61,$61,$28,$29
  .byte $61,$71,$0e,$0f,$0e,$0f,$71,$f5
  .byte $e1,$e1,$e1,$e1,$e1,$e1,$e1,$e1
  .byte $e0,$e0,$e0,$e0,$e0,$e0,$e0,$e0
  .byte $f4,$70,$0e,$0f,$0e,$0f,$70,$60
  .byte $28,$29,$60,$60,$60,$65,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5e,$5c,$5c,$5c,$5c
  .byte $5c,$dc,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$61,$61,$61,$2a,$2b
  .byte $61,$74,$71,$71,$71,$71,$71,$f3
  .byte $90,$91,$90,$91,$e1,$e1,$e1,$e1
  .byte $e0,$e0,$e0,$e0,$90,$91,$90,$91
  .byte $f2,$70,$70,$70,$70,$70,$75,$60
  .byte $2a,$2b,$60,$60,$60,$5c,$5c,$5c
  .byte $5c,$5d,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$dc,$5c,$5c
  .byte $5c,$5d,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5d,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$64,$02,$03,$61,$61
  .byte $28,$29,$74,$71,$71,$71,$71,$71
  .byte $71,$71,$f3,$e1,$e1,$e1,$e1,$e1
  .byte $e0,$e0,$e0,$e0,$e0,$f2,$70,$70
  .byte $70,$70,$70,$70,$70,$75,$28,$29
  .byte $60,$60,$02,$03,$65,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5e,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5e,$5c,$5c,$5c
  .byte $5c,$5c,$5e,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5e
  .byte $5c,$5c,$5c,$dc,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5d,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5e,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$64,$61,$61,$61
  .byte $2a,$2b,$61,$74,$12,$13,$71,$71
  .byte $71,$71,$71,$f3,$90,$91,$90,$91
  .byte $90,$91,$90,$91,$f2,$70,$70,$70
  .byte $70,$70,$12,$13,$75,$60,$2a,$2b
  .byte $60,$60,$60,$65,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5e,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5e,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$dc,$5c,$5c,$5c,$5c,$5c,$dc
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$dc,$5c,$5c,$5c,$5c,$dc,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$64,$10,$11
  .byte $61,$61,$28,$29,$74,$71,$56,$5a
  .byte $5b,$57,$71,$71,$71,$26,$27,$71
  .byte $70,$26,$27,$70,$70,$70,$56,$5a
  .byte $5b,$57,$70,$75,$28,$29,$60,$60
  .byte $10,$11,$65,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$dc,$5c,$5c,$5c,$5c,$dc,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5d,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5d,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$64,$61
  .byte $61,$61,$2a,$2b,$61,$74,$0e,$0f
  .byte $0e,$0f,$71,$71,$71,$71,$71,$71
  .byte $70,$70,$70,$70,$70,$70,$0e,$0f
  .byte $0e,$0f,$75,$60,$2a,$2b,$60,$60
  .byte $60,$65,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $dc,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5e
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$64
  .byte $00,$01,$61,$61,$28,$29,$74,$71
  .byte $71,$71,$71,$71,$86,$87,$88,$8a
  .byte $8b,$89,$86,$87,$70,$70,$70,$70
  .byte $70,$75,$28,$29,$60,$60,$00,$01
  .byte $65,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5d,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5e
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5d,$5c
  .byte $5c,$5c,$5c,$dd,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$dd,$5c,$5c,$5e,$5c
  .byte $64,$61,$61,$61,$2a,$2b,$61,$74
  .byte $71,$71,$12,$13,$71,$f3,$e1,$e1
  .byte $e0,$e0,$f2,$70,$12,$13,$70,$70
  .byte $75,$60,$2a,$2b,$60,$60,$60,$65
  .byte $5c,$5c,$5c,$5c,$dc,$5c,$5d,$5c
  .byte $5c,$5c,$5c,$5d,$dc,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5d,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$dc,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$dc,$5c,$5d,$5c
  .byte $5c,$5c,$5c,$5d,$dc,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$64,$10,$11,$61,$61,$28,$29
  .byte $74,$71,$71,$71,$71,$71,$f3,$fe
  .byte $ff,$f2,$70,$70,$70,$70,$70,$75
  .byte $28,$29,$60,$60,$10,$11,$65,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5d,$5c,$5c,$5e,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5d,$5c,$5c,$5e,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$64,$61,$61,$61,$2a,$2b
  .byte $61,$74,$71,$71,$71,$71,$71,$71
  .byte $70,$70,$70,$70,$70,$70,$75,$60
  .byte $2a,$2b,$60,$60,$60,$65,$5c,$5c
  .byte $5c,$5c,$5c,$5d,$5e,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5e,$5c,$5c,$5c,$5c,$5c,$5c,$5e
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5d,$5e,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$64,$02,$03,$61,$61
  .byte $28,$29,$74,$71,$12,$13,$56,$5a
  .byte $5b,$57,$12,$13,$70,$75,$28,$29
  .byte $60,$60,$02,$03,$65,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5d,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$dc,$5c,$5c
  .byte $5e,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5d,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$64,$61,$61,$61
  .byte $2a,$2b,$61,$74,$71,$71,$0e,$0f
  .byte $0e,$0f,$70,$70,$75,$60,$2a,$2b
  .byte $60,$60,$60,$65,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$dc,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5d,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$64,$61,$61
  .byte $61,$2a,$2b,$61,$74,$12,$13,$12
  .byte $13,$75,$60,$2a,$2b,$60,$60,$60
  .byte $65,$5c,$5e,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5e,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$dd
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$64,$00
  .byte $01,$61,$61,$28,$29,$76,$78,$79
  .byte $77,$28,$29,$60,$60,$00,$01,$65
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$dc,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5e,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$dc,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$dc,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5e,$5c,$5c
  .byte $5d,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5d
  .byte $5c,$5c,$5c,$5c,$5e,$5c,$5c,$64
  .byte $61,$61,$61,$2a,$2b,$61,$24,$25
  .byte $60,$2a,$2b,$60,$60,$60,$65,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5d,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5d
  .byte $5c,$5c,$5c,$5c,$5e,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5e,$5c,$5c,$5c,$5c,$5c
  .byte $dc,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $64,$10,$11,$61,$61,$61,$61,$60
  .byte $60,$60,$60,$10,$11,$65,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5e,$5c,$5c,$5c,$5c,$5c
  .byte $dc,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$dc,$5c,$5c,$5c
  .byte $5d,$64,$61,$61,$61,$54,$58,$59
  .byte $55,$60,$60,$60,$65,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5d,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$dc,$5c,$5c,$5c
  .byte $5d,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5d,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5e,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$dd,$5c,$5c,$5c,$5c
  .byte $5c,$5e,$64,$10,$11,$0c,$0d,$0c
  .byte $0d,$10,$11,$65,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$dc,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5d,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5e,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5e,$5c,$5c,$5c,$5c
  .byte $5c,$5e,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$dc,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5d,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5e,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$dd
  .byte $5c,$5c,$5c,$5c,$5c,$dd,$5c,$5c
  .byte $5c,$5c,$5c,$64,$61,$61,$61,$60
  .byte $60,$60,$65,$5c,$5c,$5c,$5c,$5c
  .byte $dc,$5c,$5c,$5c,$5c,$5c,$dc,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5e,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5e,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $dc,$5c,$5c,$5c,$5c,$5c,$dc,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $dc,$5c,$5c,$5c,$5c,$dc,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5d,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$64,$10,$11,$10
  .byte $11,$65,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5d
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $dc,$5c,$5c,$5c,$5c,$dc,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5d,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5d
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5e,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$dc
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$64,$6e,$6f
  .byte $65,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5e,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5e,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$dc
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5e,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5d,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5d,$5c,$5c
  .byte $5c,$5c,$dd,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$dd,$5c,$5c,$5e,$5c,$5d
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$dd,$5c,$5c,$5c
  .byte $5c,$dc,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$dc,$5c,$5d,$5c,$5c
  .byte $5c,$5c,$5d,$dc,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5e,$5c,$5d
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$dc,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$dc,$5c,$5d,$5c,$5c
  .byte $5c,$5c,$5d,$dc,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5d
  .byte $5c,$5c,$5e,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5d
  .byte $5c,$5c,$5e,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5e
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5d,$5e,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5e
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5d,$5e,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$dd,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$dc,$5c,$5c,$5e
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5d,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$dc,$5c,$5c,$5e
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5d,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$dc,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5e,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$dc,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5e,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5d,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5d,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5d,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5d,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5d,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5d,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5e,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5e,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$dd
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$dd,$5c,$5c,$5c
  .byte $5c,$dc,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$dc,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5e,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$dc,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$dc,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5e,$5c,$5c
  .byte $5d,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5d
  .byte $5c,$5c,$5c,$5c,$5e,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5d,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5d
  .byte $5c,$5c,$5c,$5c,$5e,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5e,$5c,$5c,$5c,$5c,$5c
  .byte $dc,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5e,$5c,$5c,$5c,$5c,$5c
  .byte $dc,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$dc,$5c,$5c,$5c
  .byte $5d,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5d,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$dc,$5c,$5c,$5c
  .byte $5d,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5d,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5e,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$dd,$5c,$5c,$5c,$5c
  .byte $5c,$5e,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$dc,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5d,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5e,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5e,$5c,$5c,$5c,$5c
  .byte $5c,$5e,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$dc,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5d,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5e,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$dd
  .byte $5c,$5c,$5c,$5c,$5c,$dd,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5e,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $dc,$5c,$5c,$5c,$5c,$5c,$dc,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5e,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5e,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $dc,$5c,$5c,$5c,$5c,$5c,$dc,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $dc,$5c,$5c,$5c,$5c,$dc,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5d,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5d
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $dc,$5c,$5c,$5c,$5c,$dc,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5d,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5d
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5e,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$dc
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5e,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5e,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$dc
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5e,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5d,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5d,$5c,$5c
  .byte $5c,$5c,$dd,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$dd,$5c,$5c,$5e,$5c,$5d
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$dd,$5c,$5c,$5c
  .byte $5c,$dc,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$dc,$5c,$5d,$5c,$5c
  .byte $5c,$5c,$5d,$dc,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5e,$5c,$5d
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$dc,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$dc,$5c,$5d,$5c,$5c
  .byte $5c,$5c,$5d,$dc,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5d
  .byte $5c,$5c,$5e,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5d
  .byte $5c,$5c,$5e,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5e
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5d,$5e,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5e
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5d,$5e,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$dd,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$dc,$5c,$5c,$5e
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5d,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$dc,$5c,$5c,$5e
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5d,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$dc,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5e,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$dc,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5e,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5d,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5d,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5d,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5d,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5d,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5d,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5e,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5e,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$dc,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$dc,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5e,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$dc,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$dc,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5e,$5c,$5c
  .byte $5d,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5d
  .byte $5c,$5c,$5c,$5c,$5e,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5d,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5d
  .byte $5c,$5c,$5c,$5c,$5e,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5e,$5c,$5c,$5c,$5c,$5c
  .byte $dc,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5e,$5c,$5c,$5c,$5c,$5c
  .byte $dc,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$dc,$5c,$5c,$5c
  .byte $5d,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5d,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$dc,$5c,$5c,$5c
  .byte $5d,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5d,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5e,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5e,$5c,$5c,$5c,$5c
  .byte $5c,$5e,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$dc,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5d,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5e,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5e,$5c,$5c,$5c,$5c
  .byte $5c,$5e,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  .byte $5c,$5c,$dc,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5d,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5e,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5e,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $dc,$5c,$5c,$5c,$5c,$5c,$dc,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5e,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $5e,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  .byte $dc,$5c,$5c,$5c,$5c,$5c,$dc,$5c
  .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c



  * = $b200                 ; new page for screen data

topline
  ;       0123456789012345678901234567890123456789
  .sbyte "  WELCOME TO THE SCROLLING TUTORIAL 1   "

terrain_map
  .byte 0,0,0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0,0,0
; screentext
;   .sbyte "A1A2A3A4A5A6A7A8A9A0A1A2A3A4A5A6A7A8A9A0"
;   .sbyte "B1B2B3B4B5B6B7B8B9B0B1B2B3B4B5B6B7B8B9B0"
;   .sbyte "C1C2C3C4C5C6C7C8C9C0C1C2C3C4C5C6C7C8C9C0"
;   .sbyte "D1D2D3D4D5D6D7D8D9D0D1D2D3D4D5D6D7D8D9D0"
;   .sbyte "E1E2E3E4E5E6E7E8E9E0E1E2E3E4E5E6E7E8E9E0"
;   .sbyte "F1F2F3F4F5F6F7F8F9F0F1F2F3F4F5F6F7F8F9F0"
;   .sbyte "G1G2G3G4G5G6G7G8G9G0G1G2G3G4G5G6G7G8G9G0"
;   .sbyte "H1H2H3H4H5H6H7H8H9H0H1H2H3H4H5H6H7H8H9H0"
;   .sbyte "I1I2I3I4I5I6I7I8I9I0I1I2I3I4I5I6I7I8I9I0"
;   .sbyte "J1J2J3J4J5J6J7J8J9J0J1J2J3J4J5J6J7J8J9J0"
;   .sbyte "K1K2K3K4K5K6K7K8K9K0K1K2K3K4K5K6K7K8K9K0"
;   .sbyte "L1L2L3L4L5L6L7L8L9L0L1L2L3L4L5L6L7L8L9L0"
;   .sbyte "M1M2M3M4M5M6M7M8M9M0M1M2M3M4M5M6M7M8M9M0"
;   .sbyte "N1N2N3N4N5N6N7N8N9N0N1N2N3N4N5N6N7N8N9N0"
;   .sbyte "O1O2O3O4O5O6O7O8O9O0O1O2O3O4O5O6O7O8O9O0"
;   .sbyte "P1P2P3P4P5P6P7P8P9P0P1P2P3P4P5P6P7P8P9P0"
;   .sbyte "Q1Q2Q3Q4Q5Q6Q7Q8Q9Q0Q1Q2Q3Q4Q5Q6Q7Q8Q9Q0"
;   .sbyte "R1R2R3R4R5R6R7R8R9R0R1R2R3R4R5R6R7R8R9R0"
;   .sbyte "S1S2S3S4S5S6S7S8S9S0S1S2S3S4S5S6S7S8S9S0"
;   .sbyte "T1T2T3T4T5T6T7T8T9T0T1T2T3T4T5T6T7T8T9T0"
;   .sbyte "U1U2U3U4U5U6U7U8U9U0U1U2U3U4U5U6U7U8U9U0"
;   .sbyte "V1V2V3V4V5V6V7V8V9V0V1V2V3V4V5V6V7V8V9V0"
;   .sbyte "W1W2W3W4W5W6W7W8W9W0W1W2W3W4W5W6W7W8W9W0"
;   .sbyte "X1X2X3X4X5X6X7X8X9X0X1X2X3X4X5X6X7X8X9X0"
;   .sbyte "Y1Y2Y3Y4Y5Y6Y7Y8Y9Y0Y1Y2Y3Y4Y5Y6Y7Y8Y9Y0"
;   .sbyte "Z1Z2Z3Z4Z5Z6Z7Z8Z9Z0Z1Z2Z3Z4Z5Z6Z7Z8Z9Z0"


  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$64,$10,$11
  ; .byte $61,$61,$28,$29,$74,$71,$71,$71
  ; .byte $70,$70,$70,$75,$28,$29,$60,$60
  ; .byte $10,$11,$65,$5c,$5c,$5c,$5d,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5d,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5d
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5d,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5e
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$64,$61
  ; .byte $61,$61,$2a,$2b,$61,$74,$12,$13
  ; .byte $12,$13,$75,$60,$2a,$2b,$60,$60   ; compare $2a+$2b and $10+$11
  ; .byte $60,$65,$5c,$5e,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5e
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5e,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $dd,$5c,$5c,$5c,$5c,$5c,$5c,$64
  ; .byte $00,$01,$61,$61,$28,$29,$76,$78   ; $28+$29 => big launcher
  ; .byte $79,$77,$28,$29,$60,$60,$00,$01   ;
  ; .byte $65,$5c,$5c,$5c,$5c,$5c,$5c,$dc
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$dc,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$dc
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5e,$5c
  ; .byte $5c,$5d,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5d,$5c,$5c,$5c,$5c,$5e,$5c,$5c
  ; .byte $64,$61,$61,$61,$2a,$2b,$61,$24
  ; .byte $25,$60,$2a,$2b,$60,$60,$60,$65
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5d,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5d,$5c,$5c,$5c,$5c,$5e,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5e,$5c,$5c,$5c,$5c
  ; .byte $5c,$dc,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$64,$10,$11,$61,$61,$61,$61
  ; .byte $60,$60,$60,$60,$10,$11,$65,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; .byte $5c,$5c,$5c,$5e,$5c,$5c,$5c,$5c
  ; .byte $5c,$dc,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$dc,$5c,$5c
  ; ; .byte $5c,$5d,$64,$61,$61,$61,$54,$58
  ; ; .byte $59,$55,$60,$60,$60,$65,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5d,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$dc,$5c,$5c
  ; ; .byte $5c,$5d,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5d,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5e,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$dd,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5e,$64,$10,$11,$0c,$0d
  ; ; .byte $0c,$0d,$10,$11,$65,$5c,$5c,$5e
  ; ; .byte $5c,$5c,$5c,$dc,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5d,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5e,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5e,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5e,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5e
  ; ; .byte $5c,$5c,$5c,$dc,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5d,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5e,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $dd,$5c,$5c,$5c,$5c,$5c,$dd,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$64,$61,$61,$61
  ; ; .byte $60,$60,$60,$65,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$dc,$5c,$5c,$5c,$5c,$5c,$dc
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5e,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5e,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$dc,$5c,$5c,$5c,$5c,$5c,$dc
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$dc,$5c,$5c,$5c,$5c,$dc,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5d,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$64,$10,$11
  ; ; .byte $10,$11,$65,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5d,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$dc,$5c,$5c,$5c,$5c,$dc,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5d,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5d,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ; .byte $5c,$5c,$5c,$5c,$5c,$5c,$5c,$5c
  ; ;




  ;
  * = $b800
  .include terrain.asm

; title is 20 bytes wide
  * = $bfe8
cartitle
  .sbyte "   scroll test 1    "

  * = $bffc
  .byte $57,$50

  * = $bffd
  .byte $ff

  * = $bffe
  .word init
