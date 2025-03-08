100 REM Gamepad Test
105 SET FNKEY 7 TO "run "+ARGS$(0)+CHR$(13)

120 'This array is used to display the * next to pressed controls
125 S(0)=' ':S(1)='*':S(2)=$C$


200 REM Thumb Pad Bit Patterns
210 'Each of the 16 directions of the thumb pat generates a unique 
211 'five bit code contained in bits 4 through 0.
212 'If bits 5 through 7 are 0, then no keys are being pressed and
213 'bits 0 through 4 will accurately reflect the thumb pad state.

220 'This function tests for the corresponding thumb pad direction
221 '
222'M is the bit pattern corresponding to the direction being checked
223 'P must contain the result of JOY() with bits 5 - 7 stripped.
224 '
225 'If the specofoed direction is pressed, the function returns 1,
226 'otherwise it returns 0.
230 DEF FNP(M)=-(P=M)

240 'If a direction and any button are pressed at the same time,
241 'bits 0 through 5 may no longer exactly match the pattern for
242 'that direction.

250 'Bit  43210 
251 D1 = %00010 : '1
252 D2 = %10010 : '2
253 D3 = %10011 : '3
254 D4 = %00011 : '4
255 D5 = %00001 : '5
256 D6 = %10001 : '6
257 D7 = %11001 : '7
258 D8 = %01001 : '8
259 D9 = %01000 : '9
260 DA = %11000 : '10
261 DB = %11100 : '11
262 DC = %01100 : '12
263 DD = %00100 : '13
264 DE = %10100 : '14
265 DF = %10110 : '15
266 DG = %00110 : '16

300 REM Button Bit Patterns
320 DEF FNK(M)=(J AND M)=M


340 '   765          43210
341 H1=%01000000:L1=%00000:M1=H1+L1
342 H2=%10000000:L2=%00100:M2=H2+L2
343 H3=%10100000:L3=%00000:M3=H3+L3
344 H4=%00100000:L4=%00000:M4=H4+L4
345 H5=%10000000:L5=%00010:M5=H5+L5
346 H6=%10000000:L6=%00001:M6=H6+L6


400 CLS
410 PRINT
420 PRINT: 'TAB(2);"00000000";TAB(16);"00000000"
425 PRINT
430 FOR I=1 TO 16
432 IF I<7 THEN PRINT TAB(2);"K%%" % (I);
433 PRINT TAB(7+(I>9));"P%%" % (I);
434 IF I<7 THEN PRINT TAB(16);"K%%" % (I);
435 PRINT TAB(21+(I>9));"P%%" % (I);
436 PRINT
438 NEXT
440 CLEAR CURSOR

500 _main:
510 J=JOY(1):K=SGN(J AND $E0):C=K AND J:P=J AND $1F
515 POKE SCREEN 45,HEX$(J)

521 K1=FNK(M1):POKE SCREEN 166,S(-K1)
522 K2=FNK(M2):POKE SCREEN 206,S(-K2)
523 K3=FNK(M3):POKE SCREEN 246,S(-K3)
524 K4=FNK(M4):POKE SCREEN 286,S(-K4)
525 K5=FNK(M5):POKE SCREEN 326,S(-K5)
526 K6=FNK(M6):POKE SCREEN 366,S(-K6)

641 P1=FNP(%00010):POKE SCREEN 171,S(-P1)
642 P2=FNP(%10010):POKE SCREEN 211,S(-P2)
643 P3=FNP(%10011):POKE SCREEN 251,S(-P3)
644 P4=FNP(%00011):POKE SCREEN 291,S(-P4)
645 P5=FNP(%00001):POKE SCREEN 331,S(-P5)
646 P6=FNP(%10001):POKE SCREEN 371,S(-P6)
647 P7=FNP(%11001):POKE SCREEN 411,S(-P7)
648 P8=FNP(%01001):POKE SCREEN 451,S(-P8)
649 P9=FNP(%01000):POKE SCREEN 491,S(-P9)
650 PA=FNP(%11000):POKE SCREEN 531,S(-PA)
651 PB=FNP(%11100):POKE SCREEN 571,S(-PB)
652 PC=FNP(%01100):POKE SCREEN 611,S(-PC)
653 PD=FNP(%00100):POKE SCREEN 651,S(-PD)
654 PE=FNP(%10100):POKE SCREEN 691,S(-PE)
655 PF=FNP(%10110):POKE SCREEN 731,S(-PF)
656 PG=FNP(%00110):POKE SCREEN 771,S(-PG)

490 GOTO _main

