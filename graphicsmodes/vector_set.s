set_vectors
  lda #$03          ; IRQ vector
  sta VDSLST
  lda #$fc
  sta VDSLST+1

  lda #$b8          ; vbi vector
  sta $202
  lda #$fc
  sta $203

  lda #$b2          ; deferred VBI
  sta $204
  lda #$fc
  sta $205
  rts
