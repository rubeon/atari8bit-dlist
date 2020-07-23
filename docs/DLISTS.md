# Missile Command
## Title Screen

Weird that there aren't the usual blank lines at the top?

First line is mode 6 (5 colors, 8 lines, 20 chars/line) and shows
MISSILE COMMAND in blue
Second line is mode6 and shows 1PLAYER SKIP0 BONUS

The rest of the screen is mode D (4 colors, 2 lines, 160 pixels/line)

Main game screen memory starts at 1178 on line 3 (mode D)

BF00: DLI LMS 1010 MODE 6
BF03: MODE 6
BF04: LMS 1178 MODE D
BF07: 23x MODE D
BF1E: DLI MODE D
BF1F: 28x MODE D
BF3B: DLI MODE D
BF3C: 29x MODE D
BF59: DLI MODE D
BF5A: 9x MODE D
BF63: LMS 2000 MODE D
BF66: 9x MODE D
BF6F: DLI MODE D
BF70: 7x MODE D
BF77: DLI MODE D
BF78: JVB BF00

When it's running the DLIST remains identical.

DLIs:
1, 27, 56, 76, and 84

# Defender

Has the 24 blank lines at the top

192 lines of mode E (4 colors, 1 scanline, 160 px)
DLI on 2nd E line, then on last.



2000: 3x 8 BLANK
2003: LMS 2218 MODE E
2006: DLI MODE E
2007: 87x MODE E
205E: LMS 3000 MODE E
2061: 100x MODE E
20C5: DLI MODE E
20C6: JVB 2000

# Kangaroo

24 blank lines, standard.

Then 24 lines of mode 4 (8 lines, 4 colors, 40 chrs/line) for 192 visible
lines.



0900: 8 BLANK
0901: 2x DLI 8 BLANK
0903: DLI LMS 2400 MODE 4
0906: 23x DLI MODE 4
091D: JVB 0900

Q*bert

B148: 3x 8 BLANK
B14B: LMS 1000 MODE 4
B14E: 2x MODE 4
B150: DLI MODE 4
B151: 19x MODE 4
B164: DLI MODE 4
B165: MODE 6
B166: JVB B148

Joystick Test Cart
(customized Atari logo!)

2000: 3x 8 BLANK
2003: LMS 3000 MODE D
2006: DLI MODE D
2007: 78x MODE D
2055: 2x MODE 7
2057: JVB 2000

Missile Command Logo Screen

2000: 3x 8 BLANK
2003: LMS 3000 MODE D
2006: DLI MODE D
2007: 78x MODE D
2055: 2x MODE 7
2057: JVB 2000

# SPACE Dungeon

Title Screen
9F88: 3x 8 BLANK
9F8B: LMS 2100 MODE E (2100 = blank blank blank)
9F8E: 18x MODE E
9FA0: 8 BLANK
9FA1: LMS 1700 MODE 7 (1700 = SPACE DUNGEON PRESS START etc. until DEMONSTRATION)
9FA4: 2 BLANK
9FA5: LMS 909C MODE 4 (909c = (c) TAITO AMERICA etc wonder how that got up below space dungeon?)
9FA8: 2x 8 BLANK
9FAA: LMS 1728 MODE 6  (PRESS START FOR PLAYER GAME )
9FAD: MODE 6 (continues 1 player/2 player etc.)
9FAE: 2x 8 BLANK
9FB0: MODE 6
9FB1: 8 BLANK
9FB2: MODE 6
9FB3: 8 BLANK
9FB4: MODE 6
9FB5: 8 BLANK
9FB6: MODE 6
9FB7: 8 BLANK
9FB8: MODE 6
9FB9: JVB 0046

In-game:

1F00: 3x 8 BLANK
1F03: LMS 2100 MODE E
1F06: 64x MODE E
1F46: DLI MODE E
1F47: 30x MODE E
1F65: LMS 3000 MODE E
1F68: 95x MODE E
1FC7: JVB 1F00
