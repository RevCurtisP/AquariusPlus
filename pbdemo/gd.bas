100 REM Tile Test (Arcade Gauntlet Level 1)
110 CLS:AD$="gauntlet/":
115 LOAD CHRSET AD$+"gaunt.chr":USE CHRSET 2
120 PRINT:PRINT "Gauntlet tilemap demo":PRINT
130 PRINT:PRINT "Requires plusBASIC v0.12q"
140 PRINT "and Aquarius+ System V0.17d"
170 PRINT:INPUT "Level (1-3)";L$
210 N$="gauntlet/level"+L$
220 LOAD N$+".pal",$3860
225 SET PALETTE 1 TO PEEK$($3860,30)
230 LOAD N$+".map",@40,0
235 COPY @40,0,4096 TO @20,0 FAST
240 LOAD N$+".til",@20,4096
250 M=(MOUSEX>=0)
300 SET PALETTE 0 INDEX 15 TO RGB$(0,0,0):SET PALETTE 0 INDEX 4 TO RGB$(1,4,15)
305 CLS 7,0
310 SCREEN 1+16+24:POKE $37FF,15:POKE $33FF,' '
315 FILL SCREEN (25,0)-(39,24) " ",8,15
320 POKE PEEK($3801),' '
325 SS=$3000+66:SC=SS+1024:POKE SS,"Gauntlet Demo"
330 POKE SS+120,"  LEVEL   "+L$
332 POKE SS+160,$"2020202020208081828384"
333 POKE SC+160,STRING$(14,$1F)
334 POKE SS+200,$"202020209B9C9D2020208B8C8D8E"
335 POKE SC+202,STRING$(12,$1F)
336 POKE SS+240,"   3020  10814"
337 POKE SC+240,STRING$(14,$1F)
338 POKE SS+281,$8F:POKE SC+281,$3F
339 POKE SS+293,$9F:POKE SC+293,$4F
342 POKE SS+360,$"2020202020208586878889"
343 POKE SC+360,STRING$(14,$4F)
344 POKE SS+400,$"202020209B9C9D2020208B8C8D8E"
345 POKE SC+402,STRING$(12,$4F)
346 POKE SS+440,"   1795  10543"
347 POKE SC+440,STRING$(14,$4F)
348 POKE SS+481,$8F:POKE SC+481,$3F
349 POKE SS+493,$9F:POKE SC+493,$4F
350 IF M=0 THEN 360
352 POKE SS+560," Use mouse to"
354 POKE SS+600," move tilemap"
358 GOTO 380
360 POKE SS+560,$"06"+" 1 pxl right"
362 POKE SS+600,$"07"+" 1 pxl left"
364 POKE SS+640,$"08"+" 1 pxl up"
366 POKE SS+680,$"09"+" 1 pxl down"
370 POKE SS+720,"PgUp: 8 pxl up"
372 POKE SS+760,"PgDn: 8 pxl dn"
374 POKE SS+800,"Home: 8 pxl lf"
376 POKE SS+840," End: 8 pxl rt"
380 POKE SS+920," Q to quit"
400 TX=0:TY=0
405 SET TILEMAP OFFSET TX,TY
410 _LOOP:IF M THEN K=INKEY:GOTO 420
415 K=GETKEY
420 IF K=81 OR K=113 THEN SCREEN 0:STOP
425 IF M THEN TX=MOUSEX*312/319:TY=MOUSEY*56/199:GOTO 450
430 TX=TX+(K=158)+8*(K=155):IF TX<0 THEN TX=0
435 TX=TX-(K=142)-8*(K=154):IF TX>312 THEN TX=312
440 TY=TY+(K=143)+8*(K=138):IF TY<0 THEN TY=0
445 TY=TY-(K=159)-8*(K=139):IF TY>56 THEN TY=56
450 SET TILEMAP OFFSET TX,TY
480 GOTO _LOOP
