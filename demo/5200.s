; dlist defines
BLANK8  = $70

DLISTL  = $D402			; display list lo
DLISTH  = $D403			; display list hi
CHACTL  = $D401			; Character control
CHBASE  = $f800
sDLISTL = $05

sDLISTH = $06
DMACTL  = $D400			; DMA control
sDMACTL	= $07			; DMA Control Shadow
NMIEN   = $D40E			; NMI enable
WSYNC   =	$D40A
COLOR0  = $0c
COLOR1  =	$0d			; Color 1 shadow
COLOR2  =	$0e			; Color 2 shadow
COLOR3	=	$0f
ANTIC  = $d400
HSCROL = ANTIC+4
VSCROL = ANTIC+5
VCOUNT = ANTIC+$b
RTCLOK = $01
RTCLOKH = $01
RTCLOKL = $02

VDSLST = $200

GTIA   = $c000
COLPF0 = GTIA + $16
COLPF1 = COLPF0+1
COLPF2 = COLPF0+2
COLPF3 = COLPF0+3
COLPF4 = COLPF0+4

COLPM0 = GTIA + $12
COLPM1 = COLPM0 + 1
COLPM2 = COLPM0 + 2
COLPM3 = COLPM0 + 3
COLBK = GTIA + $1a
