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

