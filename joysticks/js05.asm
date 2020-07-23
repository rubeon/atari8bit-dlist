; setup equates
POKMSK = $0
; vectors
VVBLKI    = $202
VVBLKD    = $204
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
; shadow color registers
COLOR0    = $0c
COLOR1    = $0d
COLOR2    = $0e
COLOR3    = $0f

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
; create variable storage
  ; space for shadowed charset, etc. $1000-$17ff
  * = $1000
charset_base
  * = $1800
pmgraf_base
  * = $3000
screentext
; init

  * = $4000                             ; atari 5200 base address

init
  sei                                   ; clear interrupts
  cld
                               ; clear decimal?
  ldx #0
  lda #0
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

  ; default chbase
  lda #$f8
  sta CHBASE

  ; setup vectors
  lda #$03                              ; BIOS interrupt handler @ $fc03
  sta $200
  lda #$fc
  sta $201

  lda #$b8                              ; BIOS VBI handler @ $fcb8
  sta VVBLKI                            ; $202
  lda #$fc
  sta VVBLKI+$1                         ; $203

  ; vbi deferred vector
  lda #<vbi_deferred
  sta VVBLKD
  lda #>vbi_deferred
  sta VVBLKD+1

  lda #$02                              ; BIOS keyboard IRQ handler @ $fd02
  sta VKYBDI                            ; without this, keypad will crash 5200
  lda #$fd                              ; $208,$209
  sta VKYBDI+1

  lda #<kbd_handler                     ; handle keypresses
  sta VKPD                              ; $20a,$20b
  lda #>kbd_handler
  sta VKPD+1

  lda #<dli
  sta $206
  lda #>dli
  sta $207


  lda ~00001100
  sta CONSOL           ; enable POT 01/00 (0b00000100)

  ; turn on DMA
  lda #$2e              ; 2e = DLIST + Normal Width + missile + player + pm
  sta sDMACTL           ; double resolution
                          ;
  lda #$3               ; 0b00000011 -> enable players / enable missiles
  sta GRACTL            ;

  lda #$80              ; 0b00010001 -> bit 4: 5th player / bit 0: priority 0
  sta PRIOR             ;


  ; setup initial display list
  lda #<dlist
  sta sDLISTL
  sta DLISTL

  lda #>dlist
  sta sDLISTH
  sta DLISTH

  jsr set_default_colors

  ; Serial port activiate
  ; B Dec Hex Function
  ; 7 128 80  Serial Break
  ; 6  64 40  Serial Mode2
  ; 5  32 20  Serial Mode1
  ; 4  16 10  Serial Mode0
  ; 3   8  8  Serial Two-Tone
  ; 2   4  4  Fast Pot Scan
  ; 1   2  2  Enable KB Scan
  ; 0   1  1  KB debounce (apparently doesn't work in 5200?)
  ; setup NMI
  lda #$c0                              ; VBI + DLI ($40 + $80=$c0)
  sta NMIEN
  ; enable interrupts - what' $40?
  ; 0b01000000
  ; enables the keyboard interrupt
  ; B Decimal                Function
  ; 7 128  BREAK key enable
  ; 6  64  KEYBOARD interrupt enable
  ; 5  32  Serial data input ready (VSERIN)
  ; 4  16  Serial data output request (VSEROR)
  ; 3   8  Serial data output complete (VSEROC)
  ; 2   4  POKEY timer 4 interrupt enable
  ; 1   2  POKEY timer 2
  ; 0   1  POKEY timer 1
  lda #$40
  sta IRQEN
  sta POKMSK
  lda #$2
  sta SKCTL
  cli                                   ; clear interrupt flag

forever
  jmp forever

vbi_deferred
  jmp $fcb2
; keyboard interrupt handler
kbd_handler
  jmp XITVBL

dli
  pha             ; save A & X registers to stack
  txa
  pha
  sta WSYNC       ; first WSYNC gets us to start of scan line we want
  pla             ; restore X & A registers from stack
  tax
  pla
  rti             ; always end DLI with RTI!

set_default_colors
  lda #$cf
  sta COLBK
  lda #$ff
  sta sCOLOR1


  rts

  * = $6000
dlist
  .byte $70,$70,$70               ; 24 blank lines
  .byte $7|DL_LMS                       ; mode 7 LMS
  .word topline
  .byte $3|DL_LMS                       ; mode 3 LMS
  .word scoreline
  .byte $3|DL_LMS|DL_DLI                ; mode 3 LMS
  .word screentext
  .byte $41
  .word dlist                           ; back to top


topline
         ;0123456789012345678901234567890123456789
  .sbyte "  JOYSTICK DEMO 5   "
scoreline
  .sbyte " PLAYERX:$00     PLAYERY:$00    FIRE:   "

  ; 5200 housekeeping
  * = $bfe8
  ; cart title
         ;01234567890123456789
  .sbyte "  joystick demo "
  .byte                 $55
  .sbyte                  "   "

  ; cart copyright 1970
  * = $bffc
  .byte $57,$50
  * = $bffd
  .byte $ff
  * = $bffe
  .word init
