  .include 5200.s

  * = $4000

init
  .include vector_set.s

  lda #$0f
  sta COLOR1
  lda #$03
  sta COLOR2
  sta COLPF4

  lda #<dlist1
  sta sDLISTL
  lda #>dlist1
  sta sDLISTH

  lda #$f8
  sta $d409
  lda #00
  sta $d40a
