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

; some constants
DL_HSCROLL      = $10
DL_VSCROLL      = $20
DL_LMS          = $40
DL_DLI          = $80
deadzone_up     = 90
deadzone_down   = 130
deadzone_left   = $60
deadzone_right  = 120
playerx_min     = $2f
playerx_max     = $c9
player_graphic  = $86
vert_scroll_max = $10
vymax           = $a
vxmax           = $5
physics_timer_default = $10
gravity         = $1
; some data space in RAM
  * = $1000
stopline              .ds 20
ch                    .ds 1
hexcode               .ds 2
playerx               .ds 1
playery               .ds 1
player_accel          .ds 1        ; current acceleration
player_dir            .ds 1        ; current direction
player_updown         .ds 1        ; $0=neutral,$8=up, $f=down
sscoreline            .ds 40
feet_countdown        .ds 1
blink_countdown       .ds 1
blink_countdown_exit  .ds 1
last_color            .ds 1
delay                 .ds 1
vert_scroll           .ds 1         ; current vert_scroll
; vert_scroll_max       .ds 1         ; number of scanlines to scroll
                                    ; mode 5 has 16
scroll_dir            .ds 1
vx                    .ds 1
vy                    .ds 1

physics_timer         .ds 1

  * = $1400
mainscreen
  * = $1500
spotshow
  * = $1600
sdlist

; pm graphics
  * = $2000
pmgraphics   .ds $ff 0
  * = $2300
pm_missiles
  * = $2400
pm_player0
  * = $2500
pm_player1
  * = $2600
pm_player2
  * = $2700
pm_player3


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
  lda #0
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

  ; lda #~00111110        ; normal width (0,1), missiles (2), players (3),
  lda #$3e
  sta sDMACTL           ; single resolution (4), DLIST (5)
  lda #$3               ; 0b00000011 -> enable players / enable missiles
  sta GRACTL            ;
  lda #$00              ; 0b00010001 -> bit 4: 5th player / bit 0: priority 0
  sta PRIOR             ;


; player colors
  lda #$5c
  sta sCOLPM0
  sta COLPM0

  lda #$5c
  sta sCOLPM1
  sta COLPM1

  lda #$36
  sta sCOLPM2
  sta COLPM2

  lda #$36
  sta sCOLPM3
  sta COLPM3

; playfield colors

  lda #$f8
  sta COLPF0
  sta sCOLOR0
  lda #$0
  sta COLPF1
  sta sCOLOR1
  lda #$b4
  sta COLPF2
  sta sCOLOR2
  lda #$86
  sta COLPF3
  sta sCOLOR3
  lda #$d0
  sta sCOLBK
  sta COLBK


  lda #$f8                        ; set the default character set
  sta CHBASE

; copy dlist to RAM
  ldx #0
dlist_loop
  lda dlist,x
  sta sdlist,x
  inx
  cpx #31
  bne dlist_loop
?done

; setup display list
  lda #<sdlist
  sta sDLISTL
  sta DLISTL

  lda #>sdlist
  sta sDLISTH
  sta DLISTH

; setup vectors
; reset NMIs etc.
  lda #$03                      ; point interrupt vector to
  sta VIMIRQ                    ; bios handler ($fc03)
  lda #$fc
  sta VIMIRQ+1

  lda #$b8                      ; point vblank interrupt to bios
  sta VVBLKI                    ; handler ($fcb8)
  lda #$fc
  sta VVBLKI+1

  ; DLI handler
  lda #<dli                     ; add my dli handler to the
  sta VBREAK                    ; dli continuation vector
  lda #>dli
  sta VBREAK+1

  ; ; vbi deferred vector
  lda #<vbi_deferred            ; here I can put my VBI (deferred) handler
  sta VVBLKD                    ; to the vector used by the VBI BIOS
  lda #>vbi_deferred            ; routine
  sta VVBLKD+1

  lda #$02                      ; set the keyboard interrupt handler to
  sta VKYBDI                    ; the BIOS routine ($fd02)
  lda #$fd
  sta VKYBDI+1

  ; vkpd routine - called after a keyboard interrupt
  lda #<kbd_handler             ; load my kbd_handler to this
  sta VKPD
  lda #>kbd_handler
  sta VKPD+1

  ; setup NMI
  lda #$c0                      ; VBI + DLI
  sta NMIEN

  ; enable interrupts
  lda #$40                      ;  $40 (64) is bit 5 and 3
  sta POKMSK                    ; 5: Serial input data ready interrupt enabled
  sta IRQEN                     ; 3: Serial out transmission finished interrupt
                                ; is enabled.

; enable keyboard scanning
  lda #$2                       ; bit 1 enables kb scanning
  sta SKCTL
  cli

; cache dynamic strings in RAM
; copy topline
  ldx #19
topline_loop
  lda topline,x
  sta stopline,x
  dex
  bpl topline_loop

; copy scoreline
  ldx #0
scoreline_loop
  lda scoreline,x
  sta sscoreline,x
  inx
  cpx #40
  bcc scoreline_loop

; copy bottom status line
  ldx #0
spotshow_loop
  lda potshow,x
  sta spotshow,x
  inx
  cpx #40
  bcc spotshow_loop

; copy player-missile graphics address to PMBASE
  lda #>pmgraphics
  sta PMBASE

; center the player
  lda #$5f
  sta playerx
  lda #10
  sta playery

; zero out scroll registers
  lda #0
  sta vert_scroll
  sta VSCROL

  lda #16
  sta vert_scroll_max

; add a delay
  lda #155
  sta delay

; reset physics
  ldx #$0
  stx vx
  stx vy
  ; ldx #5
  ; stx vxmax
  ldx #10
  ; stx vymax
  ldx physics_timer_default
  stx physics_timer


; *************************************
; loop forever
forever
  jmp forever

;
.LOCAL
vbi_deferred
  cld
  lda #$f8
  sta CHBASE
  dec physics_timer
  bne ?skip_physics
  lda #6
  sta physics_timer
  jsr move_player_physics
?skip_physics
  jsr write_player
  lda playerx
  sta HPOSP0
  lda player_updown
  cmp #0
  beq ?done
  cmp #$f
  beq ?down
  jsr fine_scroll_up
  jmp ?done
?down
  jsr fine_scroll_down
  jmp ?done
?neutral
  lda #0
  sta vert_scroll
?done
  lda vert_scroll
  sta VSCROL
  jsr show_scroll
  jmp $fcb2
;
move_player_physics
  lda #~00000001      ; color0 collision
  and P0PF
  bne collision
  ; account for gravity
  clc
  lda vy
  adc #gravity
  cmp #vymax
  bge terminal_vy
  sta vy
  jmp accel
collision
  lda playery
  sbc vy
  sta playery
  lda #0              ; set vy to 0, we've hit something
  sta vy
  sta HITCLR          ; also clear the hit reg

  jmp accel
terminal_vy
  lda #vymax
  sta vy
accel
  lda playery
  adc vy
physics_done
  sta playery
  rts




.LOCAL
fine_scroll_down
  inc vert_scroll
  lda vert_scroll
  cmp #vert_scroll_max    ; check if we've reached the max scroll?
  bne ?done               ; nope, still in the cell
  jsr coarse_scroll_down  ; reached the last scan line, move a  line
  lda #0
  sta vert_scroll
?done
  sta VSCROL
  ; jsr a2hexcode
  ; lda hexcode
  ; sta spotshow+10
  ; lda hexcode+1
  ; sta spotshow+11
  rts

.LOCAL
fine_scroll_up
  dec vert_scroll
  lda vert_scroll
  bpl ?done
  jsr coarse_scroll_up
  lda #vert_scroll_max-1
  sta vert_scroll
?done
  rts



.LOCAL
coarse_scroll_down
  clc
  lda sdlist+11
  adc #40           ; down a line in memory
  sta sdlist+11
  lda sdlist+12
  adc #0
  cmp #$6d
  beq ?reset
  sta sdlist+12
  jmp ?done
?reset
  lda #>cmpf
  sta sdlist+12
  lda #$0
  sta sdlist+11
?done

  rts

;
coarse_scroll_up
  sec
  lda sdlist+11
  sbc #40
  sta sdlist+11
  lda sdlist+12
  sbc #0
  sta sdlist+12
  rts

show_scroll
  lda vert_scroll
  jsr a2hexcode
  lda hexcode
  sta spotshow+8
  lda hexcode+1
  sta spotshow+9
  ; lda numbers,x
  ; sta spotshow+8
;
  lda sdlist+12
  jsr a2hexcode
  lda hexcode
  sta spotshow+19
  lda hexcode+1
  sta spotshow+20

  lda sdlist+11
  jsr a2hexcode
  lda hexcode
  sta spotshow+21
  lda hexcode+1
  sta spotshow+22

  lda vx
  jsr a2hexcode
  lda hexcode
  sta spotshow+26
  lda hexcode+1
  sta spotshow+27
  lda vy
  jsr a2hexcode
  lda hexcode
  sta spotshow+33
  lda hexcode+1
  sta spotshow+34
  ldy gravity
  lda numbers,y
  sta spotshow+38
  rts

; coarse_scroll_down
  ; clc
  ; lda dlist_coarse_address
  ; adc #40
  ; sta dlist_coarse_address
  ; lda dlist_coarse_address+1
  ; adc #0
  ; sta dlist_coarse_address+1
  ; rts

.LOCAL
clear_player
  ; clears out the player before re-writing him
  ldx playery
  ldy #14 ; player height
  lda #0
?clearloop
  sta pm_player0,x
  inx
  dey
  bne ?clearloop
  rts

.LOCAL
write_player
  jsr clear_player
empty player
  lda #0
  ldy #$ff
?emptyloop
  sta pm_player0,y
  dey
  bne ?emptyloop
  ldx playery
  ldy #0
?loop
  lda (player_graphic),y
  sta pm_player0,x
  inx
  iny
  cpy #14
  beq ?checkanim
  jmp ?loop
?checkanim
  lda player_dir
  ora player_updown
  cmp #0
  beq ?check_blink
?animfeet
  ldx playery
  lda RANDOM
  ; right legs
  sta pm_player0+10,x
  sta pm_player0+11,x
  sta pm_player0+12,x
  sta pm_player0+13,x
  sta feet_countdown
  lda #$0
  ; sta SIZEP0
?check_blink
  ; check if blinking
  lda blink_countdown_exit
  cmp #0
  bne ?blinking
  dec blink_countdown

  bne ?done ; countdown isn't finished
  jmp ?blink

?blinking
  dec blink_countdown_exit
  ldx playery
  lda #~01111110
  sta pm_player0+4,x
  sta pm_player0+5,x
  lda RANDOM
  ora #~00000111
  sta blink_countdown
  jmp ?done


  ; lda #'a
  ; sta sscoreline+12
  ; lda blink_countdown_exit
  ; cmp #0                    ; are we currently blinking?
  ; bne ?blinking                ; no, let's
  ; dec blink_countdown       ; not blinking, let's check the countdown
  ; lda blink_countdown       ;
  ; cmp #0                    ; zero yet?
  ; beq ?blink                ; then blink (further)
  jmp ?done
?blink
  ; dbg
  lda #5
  sta blink_countdown_exit
  lda #'b
  ; sta potshow+20
  ldx playery
  lda #~01111110
  sta pm_player0+4,x
  sta pm_player0+5,x
  jmp ?done

; ?blinking
;   dec blink_countdown_exit
;   ldx blink_countdown_exit
;   cpx #0
;   bne ?blink
?done

  rts


.LOCAL
; first handle e/w
move_player
  lda PADDL0      ; x/y
  cmp #deadzone_left
  blt ?moveleft
  cmp #deadzone_right
  bge ?moveright
  jmp ?neutral
?moveleft
  ldx #0
  dec feet_countdown

  lda dirtxt,x
  ; sta spotshow+38
  ; load the left-facing player
  ldx #<pmgraphics_lonely_l
  stx player_graphic
  ldx #>pmgraphics_lonely_l
  stx player_graphic+1
  stx player_dir
  dec playerx
  lda playerx
  cmp #playerx_min
  blt ?stopleft
  jmp ?northsouth
?moveright
  dec feet_countdown
  ldx #1
  lda dirtxt,x
  ; sta spotshow+38
  ldx #<pmgraphics_lonely_r
  stx player_graphic
  ldx #>pmgraphics_lonely_r
  stx player_graphic+1
  inc playerx
  lda playerx
  ldx #$ff
  sta player_dir
  cmp #playerx_max
  bge ?stopright
  jmp ?northsouth
?stopleft
  lda #playerx_min
  sta playerx
  jmp ?northsouth
?stopright
  lda #playerx_max
  sta playerx
  jmp ?northsouth
?neutral
  lda #'n
  ; sta spotshow+38
  lda feet_countdown
  ; sta spotshow+39
  lda #0
  sta player_dir
?northsouth
  lda PADDL1
  cmp #deadzone_up
  blt ?moveup
  cmp #deadzone_down
  bge ?movedown
  lda #0
  jmp ?done
?moveup
  dec feet_countdown
  lda #$8
  jmp ?done
?movedown
  dec feet_countdown
  lda #$f
?done
  sta player_updown
  ; lda #$72
  ; sta playerx
  ; jsr a2hexcode
  ; lda hexcode
  ; sta spotshow+20
  ; lda hexcode+1
  ; sta spotshow+21
  rts

.LOCAL
dli
  lda VCOUNT
  cmp #$50
  bge ?bottom
  lda #>cmchar
  sta CHBASE
  jsr move_player
  ; lda PADDL0
  ; jsr a2hexcode
  ; lda hexcode
  ; ; sta COLOR2
  ; ldy #8
  ; ; sta spotshow,y
  ; lda hexcode+1
  ; ; sta spotshow+1,y
  ; lda PADDL1
  ; lda #$92
  ; sta playery
  ; jsr a2hexcode
  ; lda hexcode
  ; ldy #22
  ; ; sta spotshow,y
  lda hexcode+1
  ; sta spotshow+1,y
  lda playerx
  jsr a2hexcode
  ldy #30
  ; sta spotshow,y
  lda hexcode+1
  ; sta spotshow+1,y
  jmp ?done
; ?top
;   lda #
?bottom
  lda #$f8
  sta CHBASE
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
  .byte $2|DL_LMS|DL_DLI
  .word sscoreline
  .byte $0                        ; blank line
  .byte $5|DL_LMS|DL_VSCROLL
  .word cmpf
  .byte $5|DL_VSCROLL,$5|DL_VSCROLL,$5|DL_VSCROLL,$5|DL_VSCROLL,
  .byte $5|DL_VSCROLL,$5|DL_VSCROLL,$5|DL_VSCROLL,$5|DL_VSCROLL,
  .byte $5|DL_VSCROLL
  .byte $5                        ; buffer for vscroll
  .byte $0|DL_DLI
  .byte $5|DL_LMS
  .word spotshow
  .byte $41
  .word dlist
  .byte $ff ; stopme!

topline
         ;012345678901234567890
  .sbyte "   PLAYER DEMO 3    "

scoreline
         ;0123456789012345678901234567890123456789
  .sbyte "       PM GRAPHICS WITH PHYSICS        "
blanks
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
  * = $5200
potshow
         ;0123456789012345678901234567890123456789
  .sbyte "VSCROL:       LMS:     VX:    VY:   A:  "
  .sbyte "Following on that..."
dirtxt
  .sbyte "LR"
; pmgraphics_rom
;   .byte   ~00000000
;   .byte   ~00011000
;   .byte   ~00011000
;   .byte   ~01111110
;   .byte   ~01111110
;   .byte   ~00011000
;   .byte   ~00011000
;   .byte   ~00000000
; ;
pmgraphics_lonely_hat
  .byte   ~00111100
  .byte   ~00111100
  .byte   ~00111100
  .byte   ~11111111
  .byte   ~00000000
pmgraphics_lonely_r
  .byte   ~00011000
  .byte   ~00011000
  .byte   ~00111100
  .byte   ~00111100
  .byte   ~01101010
  .byte   ~01101010
  .byte   ~01111110
  .byte   ~01111110
  .byte   ~01111110
  .byte   ~01111110
  .byte   ~01010010
  .byte   ~01010010
  .byte   ~01010010
  .byte   ~01010010
;lookin' left
pmgraphics_lonely_l
  .byte   ~00011000
  .byte   ~00011000
  .byte   ~00111100
  .byte   ~00111100
  .byte   ~01010110
  .byte   ~01010110
  .byte   ~01111110
  .byte   ~01111110
  .byte   ~01111110
  .byte   ~01111110
  .byte   ~01010010
  .byte   ~01010010
  .byte   ~01010010
  .byte   ~01010010

numbers
  .sbyte "0123456789abcdef"

  * = $6000
cmchar
  .incbin cmchar.dat
  * = (* & $ff00) + 256
cmpf
  .incbin cmpf.dat


; atari cart housekeeping
  * = $bfe8
         ;01234567890123456789
  .sbyte "   PLAYER DEMO 2    "
  .byte                 $56
  .sbyte                   "  "

  * = $bffc
  .byte $57,$50
  * = $bffd
  .byte $ff

  * = $bffe
  .word init
