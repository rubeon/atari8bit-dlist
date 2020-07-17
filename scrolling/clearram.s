;************** Clear zero page and hardware ******

        ldx     #$00
        lda     #$00
crloop1
        sta     $00,x           ;Clear zero page
        sta     $D400,x         ;Clear ANTIC
        sta     $C000,x         ;Clear GTIA
        sta     $E800,x         ;Clear POKEY
        dex
        bne     crloop1

;************* Clear RAM **************************

        ldy     #$00            ;Clear Ram
        lda     #$02            ;Start at $0200
        sta     $81
        lda     #$00
        sta     $80
crloop3
        lda     #$00
        sta     ($80),y         ;Store data
        iny                     ;Next byte
        bne     crloop3         ;Branch if not done page
        inc     $81             ;Next page
        lda     $81
        cmp     #$40            ;Check if end of RAM
        bne     crloop3         ;Branch if not
