100 REM Mouse Draw
110 SET SPRITE * CLEAR:SCREEN 4:CLS:POKE PEEK($3801),32:POKE $3400,STRING$(40,$60):POKE $3001,"MouseDraw":POKE $3020,"?=Help"
120 SET TILE 511 TO $"FFF00000F77F0000F777F0000F777F0000F777F0000F777F0000F7F000000F00"
130 DEF SPRITE M$ = 0,0,0
140 DEF TILELIST T$=511
150 SET SPRITE M$ TILE T$ POS 156,96 ON

200 REM Main Loop
210 X=MOUSEX:Y=MOUSEY:B=MOUSEB
220 SET SPRITE M$ POS X,Y
230 IF Y<8 GOTO 210
240 IF B=1 THEN PSET(X/4,(Y-8)*3/8)
250 IF B=2 THEN PRESET(X/4,(Y-8)*3/8)
290 GOTO 210
