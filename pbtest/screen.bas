100 REM SCREEN commands test/demo
110 DIM H(99),L(99),S(99)

200 REM Tests
205 SET COLOR OFF
210 SCREEN 2,,,,1
215 GOSUB 300
220 SCREEN 3
225 GOSUB 300
230 SCREEN 1
235 END

300 REM Fill Entire Screen
305 CLS
310 FILL SCREEN CHR '@'
315 PAUSE
320 FILL SCREEN COLOR 7,0
325 PAUSE
330 FILL SCREEN CHR "*"
335 PAUSE
340 FILL SCREEN CHR 37 COLOR 2,0:REM %
345 PAUSE

400 REM Fill Rectangle
410 FILL SCREEN (10,10)-(30,20) CHR "#$"
415 PAUSE
420 FILL SCREEN (10,10)-(30,20) COLOR 1,7
425 PAUSE
430 FILL SCREEN (5,5)-(20,15) CHR $7E COLOR 3,5:REM ~
435 PAUSE

500 REM Get and Put Screen
510 GET SCREEN CHR (3,3)-(8,8),*H
520 GET SCREEN ATTR (5,5)-(10,10),*L
530 GET SCREEN (5,5)-(30,20),*S 
550 CLS
560 PUT SCREEN CHR (4,5),*H
565 PAUSE
570 PUT SCREEN ATTR (4,5),*L
575 PAUSE
580 PUT SCREEN (7,9),*s
585 PAUSE

900 RETURN

