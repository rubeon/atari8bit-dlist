; This will setup a vertical scrolling screen with a static
; top and bottom line
;
DLISTL  = $D402			; display list lo
DLISTH  = $D403			; display list hi
CHACTL  = $D401			; Character control
CHBASE  = $d409
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

COUNTER = $3000   ; counter byte
COUNTER_TENS = COUNTER+40
COUNTER_ONES = COUNTER+41
DLIST_SHADOW = $1000
num          = $2000
pad          = $2010
MYSCORE      = $2100
result       = $2200
value        = $2300
  * = $4000       ; cartridge start

init              ; setup the hardware
  sei
  ; cld

  ; .include 5200.s
  .include clearram.s
  .include vector_set.s

  lda #$ef
  sta COLOR0
  lda #$0f
  sta COLOR1
  lda #$84
  sta COLOR2

  ; set the default chbase
  lda #$f8
  ; lda #>font_data
  sta CHBASE

  ; setup my dli routine
  lda #<dli1
  sta $206
  lda #>dli1
  sta $207

  ; turn on DMA for DLI, normal playfield width
  lda #$22
  sta sDMACTL

  ; enable VBI and DLI
  lda #$40
  sta NMIEN

  ; setup counter
  ldx #0
cploop
  lda counter_text,x
  sta COUNTER,x
  inx
  cpx #24
  bne cploop

; move DLIST to ram
  ldx #0
cpdli

  lda dlist,x
  sta DLIST_SHADOW,x
  inx
  cpx #27
  bne cpdli

  ; setup dlist pointer
  lda #<DLIST_SHADOW
  sta sDLISTL
  lda #>DLIST_SHADOW
  sta sDLISTH
  ldx #4
cpvalue
  lda start_value,x
  sta value,x
  inx
  cpx #4
  bne cpvalue




; main loop
loop
  ldx #15         ; number of VBLANKs to wait
?start
  lda RTCLOK+1    ; check fastest moving RTCLOCK byte
?wait
  cmp RTCLOK+1    ; VBLANK will update this
  beq ?wait       ; delay until VBLANK changes it
  dex             ; delay for a number of VBLANKs
  bpl ?start

  ; enough time has passed, scroll one line
  jsr coarse_scroll_down
  ; clc
  ; sed
  ; update counter
  ldx COUNTER_ONES
  cpx #9
  blt update_ones
update_tens
  lda #0
  sta COUNTER_ONES
  ldx #0
  ldy #6
  lda numbers,x
  sta COUNTER,y
  ldx COUNTER_TENS
  inx
  stx COUNTER_TENS
  ldy #5
  lda numbers,x
  sta COUNTER,y
  jmp counter_done

update_ones
  inx
  stx COUNTER_ONES
  lda numbers,x
  ldy #6
  sta COUNTER,y

update_lmi
  ldy #7
  ldx DLIST_SHADOW,y
  lda numbers,x
  ldx $d
  sta COUNTER,x

  lda COUNTER
  sta value
  jsr printdec
counter_done

; LoadStatsToScreen2
;   lda MYSCORE
;   sta pad 		; work variable / memory location
;   ; jsr Convert2Bcd
; ;   lda <MYSCORE ;$381		; HI number
; ;   sta $300e		; screen memory
; ;   lda >MYSCORE ;$382		; LO number
; ;   sta $300f		; screen memory
; ;
; ;
  jmp loop



; scroll down a  line (coarse)
coarse_scroll_down
        lda #0
        ldx #0
        clc
        ldx #7
        lda DLIST_SHADOW,x
        adc #40
        bvs coarse_scroll_down_roll
        jmp coarse_scroll_down_cont
coarse_scroll_down_roll
        lda #0
coarse_scroll_down_cont
        sta DLIST_SHADOW,x
        inx
        lda DLIST_SHADOW,x
        adc #0
        sta DLIST_SHADOW,x
        rts

  * = $5000

dlist
  .byte $70,$70,$70       ; 24 blank lines
  .byte $c7               ; mode 7 title LMS DLI
  .byte <title
  .byte >title
  .byte $43               ; mode 3 DLI vscroll LMS
dlist_coarse_address
  .byte $00,$52
  ; .byte 0,0
  .byte $3,$3,$3
  .byte $70               ; 8 blank lines
  .byte $47               ; mode 7 DLI
  .word dlists_rock
  .byte $70,$70,$70,$70
  .byte $43
  .word COUNTER
  .byte $41               ; loop
  .word DLIST_SHADOW
;
  * = $5200
splash    ;0123456789012345678901234567890123456789
  .sbyte  "          another dlist test! (1)       "
  .sbyte  "          ANOTHER DLIST TEST! (2)       "
  .sbyte  "          ANOTHER DLIST TEST! (3)       "
  .sbyte  "          ANOTHER DLIST TEST! (4)       "
  .sbyte  "          ANOTHER DLIST TEST! (1)       "
  .sbyte  "          ANOTHER DLIST TEST! (2)       "
  .sbyte  "          ANOTHER DLIST TEST! (3)       "
  .sbyte  "          ANOTHER DLIST TEST! (4)       "
  .sbyte  "          ANOTHER DLIST TEST! (1)       "
  .sbyte  "          ANOTHER DLIST TEST! (2)       "
  .sbyte  "          ANOTHER DLIST TEST! (3)       "
  .sbyte  "          ANOTHER DLIST TEST! (4)       "
  .sbyte  "          ANOTHER DLIST TEST! (1)       "
  .sbyte  "          ANOTHER DLIST TEST! (2)       "
  .sbyte  "          ANOTHER DLIST TEST! (3)       "
  .sbyte  "          ANOTHER DLIST TEST! (4)       "
  .sbyte  "          ANOTHER DLIST TEST! (1)       "
  .sbyte  "          ANOTHER DLIST TEST! (2)       "
  .sbyte  "          ANOTHER DLIST TEST! (3)       "
  .sbyte  "          ANOTHER DLIST TEST! (4)       "
  .sbyte  "          ANOTHER DLIST TEST! (1)       "
  .sbyte  "          ANOTHER DLIST TEST! (2)       "
  .sbyte  "          ANOTHER DLIST TEST! (3)       "
  .sbyte  "          ANOTHER DLIST TEST! (4)       "
  .sbyte  "          ANOTHER DLIST TEST! (1)       "
  .sbyte  "          ANOTHER DLIST TEST! (2)       "
  .sbyte  "          ANOTHER DLIST TEST! (3)       "
  .sbyte  "          ANOTHER DLIST TEST! (4)       "
  .sbyte  "          ANOTHER DLIST TEST! (1)       "
  .sbyte  "          ANOTHER DLIST TEST! (2)       "
  .sbyte  "          ANOTHER DLIST TEST! (3)       "
  .sbyte  "          ANOTHER DLIST TEST! (4)       "
  .sbyte  "          ANOTHER DLIST TEST! (1)       "
  .sbyte  "          ANOTHER DLIST TEST! (2)       "
  .sbyte  "          ANOTHER DLIST TEST! (3)       "
  .sbyte  "          ANOTHER DLIST TEST! (4)       "
  .sbyte  "          ANOTHER DLIST TEST! (1)       "
  .sbyte  "          ANOTHER DLIST TEST! (2)       "
  .sbyte  "          ANOTHER DLIST TEST! (3)       "
  .sbyte  "          ANOTHER DLIST TEST! (z)       "

  * = $5e00
dlists_rock
  .sbyte             "   DLISTS ROCK!!!   "

numbers
  .sbyte "0123456789"


counter_text
  ;      "0123456789abcdef01234567"
  .sbyte "RTC2:   LMI1:   RTC0:   "

title
  .sbyte "  SCROLLING DEMO 1  "

  * = $6000
; dlist data
start_value
  .byte $ff,$ff,$ff,$ff
start_result
  .byte 0,0,0,0,0,0,0,0,0,0

  * = $7000

dli1
  pha
  phx



return
  plx
  pla
  rti

  *= $7400
  ; .include font_data_antic_4.s
;

;
; *** AdvII XE addition 7/7/2011 ***************************************************************************** XE
; *** 380 = work variable which will result in the converted bcd value - Ex: "$37"
; *** 381 = HI digit - Ex: "$3"
; *** 382 = LO digit - Ex: "$7"
; *** 383 = put the hexadecimal number you want converted to bcd in 4383. Ex: "$25" (which is decimal 37)

Convert2Bcd
   lda #0
   sta pad;$380	;BcdValue
   ldx #8
   sed
Convert2BcdLoop
   asl MYSCORE; $383	;input variable
   lda pad          ;$380 	;BcdValue
   adc pad           ;$380 	;BcdValue
   sta pad           ;$380 	;BcdValue
   dex
   bne Convert2BcdLoop
   cld
; ; Convert to displayable characters.
;    lda pad          ;$380 	;BcdValue
;    lsr a
;    lsr a
;    lsr a
;    lsr a
;    clc
;    AND #'00001111		;strip off left nibble
;    ORA #'00010000		;Convert to ASCII or a font (whatever you need to do).
;    sta <MYSCORE ; $381			;store HI digit
; ;
;    lda pad             ;$380			;BcdValue
;    and #$0F
;    ORA #'00010000		;modify to ascii value for screen
;    sta >MYSCORE     ; $382			;store LO digit
;
Covert2BcdExit
rts

; prints a 32 bit value to the screen
printdec
  jsr hex2dec
  ldx #9
l1
  lda result,x
  bne l2
  dex             ; skip leading zeros
  bne l1

l2
  lda result,x
  ora #$30
  jsr $ffd2
  dex
  bpl l2
  rts

; converts 10 digits (32 bit values have max. 10 decimal digits)
hex2dec
  ldx #0
l3
  jsr div10
  sta result,x
  inx
  cpx #10
  bne l3
  rts

; divides a 32 bit value by 10
; remainder is returned in akku
div10
  ldy #32         ; 32 bits
  lda #0
  clc
l4
  rol
  cmp #10
  bcc skip
  sbc #10
skip
  rol value
  rol value+1
  rol value+2
  rol value+3
  dey
  bpl l4
  rts

; value   .byte $ff,$ff,$ff,$ff
;
; result  .byte 0,0,0,0,0,0,0,0,0,0
;



  * = $bfe8

cart
         ;01234567890123456789
  .sbyte "   SCROLL TEST 1    "
  ; cart copyright
  * = $bffc
  .byte $57,$50

  * = $bffd
  .byte $ff

  ; entrypoint
  * = $bffe
  .word init
