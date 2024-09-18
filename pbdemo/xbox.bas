100 REM X-Box Controller Test

200 CLS
210 PRINT " Left Stick"
212 PRINT "  X  ### Y  ### Button *"
214 PRINT "  Trigger ### Shoulder *"
216 PRINT
220 PRINT "Right Stick"
222 PRINT "  X  ### Y  ### Button *"
224 PRINT "  Trigger ### Shoulder *"
226 PRINT
230 PRINT "  A *      Up *   View *"
232 PRINT "  B *    Down *  Guide *"
234 PRINT "  X *    Left *   Menu *"
236 PRINT "  Y *   Right *  Share *"

300 _main:
305 G$=JOY$(0):IF G$="" THEN END
310 POKE SCREEN 85,PAD$(STR$(ASC(G$[1])),-4) 
312 POKE SCREEN 92,PAD$(STR$(ASC(G$[2])),-4) 
314 POKE SCREEN 104,' '+10*SGN(ASC(G$[7])AND128)
316 POKE SCREEN 131,PAD$(STR$(ASC(G$[5])),-3) 
318 POKE SCREEN 144,' '+10*SGN(ASC(G$[8])AND2)

320 POKE SCREEN 245,PAD$(STR$(ASC(G$[3])),-4) 
322 POKE SCREEN 252,PAD$(STR$(ASC(G$[4])),-4) 
324 POKE SCREEN 264,' '+10*SGN(ASC(G$[8])AND1)
326 POKE SCREEN 291,PAD$(STR$(ASC(G$[6])),-3) 
328 POKE SCREEN 304,' '+10*SGN(ASC(G$[8])AND4)

330 POKE SCREEN 365,' '+10*SGN(ASC(G$[7])AND1)
332 POKE SCREEN 405,' '+10*SGN(ASC(G$[7])AND2)
334 POKE SCREEN 445,' '+10*SGN(ASC(G$[7])AND4)
336 POKE SCREEN 485,' '+10*SGN(ASC(G$[7])AND8)

340 POKE SCREEN 375,' '+10*SGN(ASC(G$[8])AND8)
342 POKE SCREEN 415,' '+10*SGN(ASC(G$[8])AND16)
344 POKE SCREEN 455,' '+10*SGN(ASC(G$[8])AND32)
346 POKE SCREEN 495,' '+10*SGN(ASC(G$[8])AND64)

350 POKE SCREEN 384,' '+10*SGN(ASC(G$[7])AND16)
352 POKE SCREEN 424,' '+10*SGN(ASC(G$[7])AND32)
354 POKE SCREEN 464,' '+10*SGN(ASC(G$[7])AND64)
356 POKE SCREEN 504,' '+10*SGN(ASC(G$[8])AND128)

390 GOTO _main
