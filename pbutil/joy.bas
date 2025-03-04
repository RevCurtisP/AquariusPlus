100 REM Gamepad Test
105 SET FNKEY 7 TO "run "+ARGS$(0)+CHR$(13)

110 DEF FNK(M)=(J AND M)=M

120 DIM K(6)=,$40,$84,$A0,$20,$82,$81
125 DIM P(16)=,$2,$12,$13,$3,$1,$11,$19,$9,$8,$18,$1C,$C,$4,$14,$16,$6
130 S(0)=' ':S(1)='*':S(2)=$C6

140 'Button bit patterns
141 H1=%01000000:L1=%00000:M1=H1+L1
142 H2=%10000000:L2=%00100:M2=H2+L2
143 H3=%10100000:L3=%00000:M3=H3+L3
144 H4=%00100000:L4=%00000:M4=H4+L4
145 H5=%10000000:L5=%00010:M5=H5+L5
146 H6=%10000000:L6=%00001:M6=H6+L6


200 CLS
210 PRINT
220 PRINT: 'TAB(2);"00000000";TAB(16);"00000000"
225 PRINT
230 FOR I=1 TO 16
232 IF I<7 THEN PRINT TAB(2);"K%%" % (I);
233 PRINT TAB(7+(I>9));"P%%" % (I);
234 IF I<7 THEN PRINT TAB(16);"K%%" % (I);
235 PRINT TAB(21+(I>9));"P%%" % (I);
236 PRINT
238 NEXT
240 CLEAR CURSOR

300 _main:
310 J=JOY(1):K=SGN(J AND $E0):C=K AND J:P=J AND $1F
315 POKE SCREEN 45,HEX$(J)

321 K1=FNK(M1):POKE SCREEN 166,S(-K1)
322 K2=FNK(M2):POKE SCREEN 206,S(-K2)
323 K3=FNK(M3):POKE SCREEN 246,S(-K3)
324 K4=FNK(M4):POKE SCREEN 286,S(-K4)
325 K5=FNK(M5):POKE SCREEN 326,S(-K5)
326 K6=FNK(M6):POKE SCREEN 366,S(-K6)

341 P1=(P=%00010):POKE SCREEN 171,S(-P1)
342 P2=(P=%10010):POKE SCREEN 211,S(-P2)
343 P3=(P=%10011):POKE SCREEN 251,S(-P3)
344 P4=(P=%00011):POKE SCREEN 291,S(-P4)
345 P5=(P=%00001):POKE SCREEN 331,S(-P5)
346 P6=(P=%10001):POKE SCREEN 371,S(-P6)
347 P7=(P=%11001):POKE SCREEN 411,S(-P7)
348 P8=(P=%01001):POKE SCREEN 451,S(-P8)
349 P9=(P=%01000):POKE SCREEN 491,S(-P9)
350 PA=(P=%11000):POKE SCREEN 531,S(-PA)
351 PB=(P=%11100):POKE SCREEN 571,S(-PB)
352 PC=(P=%01100):POKE SCREEN 611,S(-PC)
353 PD=(P=%00100):POKE SCREEN 651,S(-PD)
354 PE=(P=%10100):POKE SCREEN 691,S(-PE)
355 PF=(P=%10110):POKE SCREEN 731,S(-PF)
356 PG=(P=%00110):POKE SCREEN 771,S(-PG)

490 GOTO _main

915 A=106+C*14
930 FOR R=1 TO 16
935 A=A+40
940 IF R<7 THEN POKE SCREEN A+6,S(-(K=(K(R))))
950 POKE SCREEN A+11,S(-(P=P(R)))
