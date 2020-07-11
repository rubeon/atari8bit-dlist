; dlist defines
BLANK8  = $70

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
COLOR0  = $0c
COLOR1  =	$0d			; Color 1 shadow
COLOR2  =	$0e			; Color 2 shadow
COLOR3	=	$0f
COLPF0 = $c016
COLPF1 = COLPF0+1
COLPF2 = COLPF0+2
COLPF3 = COLPF0+3
COLPF4 = COLPF0+4
ANTIC  = $d400
HSCROL = ANTIC+4
VSCROL = ANTIC+5
