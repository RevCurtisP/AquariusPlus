100 REM X-Box Controller Test
110 DIM S$(1)=" ","*"

200 CLS
210 PRINT " Left Stick"
212 PRINT "  X      Y      Button  "
214 PRINT "  Trigger     Shoulder  "
216 PRINT
220 PRINT "Right Stick"
222 PRINT "  X      Y      Button  "
224 PRINT "  Trigger     Shoulder  "
226 PRINT
230 PRINT "  A        Up     View  "
232 PRINT "  B      Down    Guide  "
234 PRINT "  X      Left     Menu  "
236 PRINT "  Y     Right    Share  "
240 PRINT
242 PRINT " Raw:"

300 _main:
302 G$=JOY$(0):IF G$="" THEN PRINT "Xbox controller not found":END
304 B=WORD(G$,7)
310 POKE SCREEN 85,PAD$(STR$(BYTE(G$[1])),-4): 'X
312 POKE SCREEN 92,PAD$(STR$(BYTE(G$[2])),-4): 'Y
314 POKE SCREEN 104,S$(-BIT(B,7))            : 'Button
316 POKE SCREEN 131,PAD$(STR$(ASC(G$[5])),-3): 'Trigger
318 POKE SCREEN 144,S$(-BIT(B,9))            : 'Shoulder

320 POKE SCREEN 245,PAD$(STR$(BYTE(G$[3])),-4): 'X 
322 POKE SCREEN 252,PAD$(STR$(BYTE(G$[4])),-4): 'Y 
324 POKE SCREEN 264,S$(-BIT(B,8))             : 'Button
326 POKE SCREEN 291,PAD$(STR$(ASC(G$[6])),-3) : 'Trigger
328 POKE SCREEN 304,S$(-BIT(B,10))            : 'Shoulder

330 POKE SCREEN 365,S$(-BIT(B,0)): 'A
332 POKE SCREEN 405,S$(-BIT(B,1)): 'B
334 POKE SCREEN 445,S$(-BIT(B,2)): 'X
336 POKE SCREEN 485,S$(-BIT(B,3)): 'Y

340 POKE SCREEN 375,S$(-BIT(B,11)): 'Up
342 POKE SCREEN 415,S$(-BIT(B,12)): 'Down
344 POKE SCREEN 455,S$(-BIT(B,13)): 'Left
346 POKE SCREEN 495,S$(-BIT(B,14)): 'Right

350 POKE SCREEN 384,S$(-BIT(B,4)) : 'View
352 POKE SCREEN 424,S$(-BIT(B,5)) : 'Menu
354 POKE SCREEN 464,S$(-BIT(B,6)) : 'Guide
356 POKE SCREEN 504,S$(-BIT(B,15)): 'Share

360 POKE SCREEN 567,HEX$(G$)

390 GOTO _main

