100 REM Test Tiles and Sprites

110 QG$="tx"
120 GOSUB _init

200 GOSUB _title:ARGS "Tiles and Sprites"

210 GOSUB _output:ARGS LIST$(NEXT)
211 DEF RGBLIST R$=0,0,0;1,1,1;2,2,2;3,3,3;4,4,4;5,5,5;6,6,6;7,7,7;8,8,8;9,9,9;10,10,10;11,11,11;12,12,12;13,13,13;14,14,14;15,15,15
216 GOSUB _output:ARGS LIST$(NEXT)
217 SET PALETTE 2 TO R$
218 GOSUB _assert:ARGS "GETPALETTE$(2)=R$"

230 DIM T$(5):FOR L=231 TO 235:GOSUB _output:ARGS LIST$(L):NEXT
231 T$(1)=$"000FF00000F44F0000F44F0000F44F000F4444F00F4444F0F044440FF444444F"
232 T$(2)=$"0000000000000008000000080800000707000007080000070800000708000006"
233 T$(3)=$"08000004087E7E84087E7E7808000E8708000008000000800000000000000060"
234 T$(4)=$"0000000008000000780000001700000817000007170000084700000846000008"
235 T$(5)=$"44000008448E7E78787E7E78478E000878000008008000000000000000600000"

240 GOSUB _output:ARGS LIST$(NEXT)
241 FOR T=1 TO 5:SET TILE 500+T TO T$(T)
242 GOSUB _assert:ARGS "PEEK$(@20,(500+%%)*32,32)=T$(%%)" % (T,T)
244 GOSUB _assert:ARGS "GETTILE$(500+%%)=T$(%%)" % (T,T)
246 NEXT

250 GOSUB _output:ARGS LIST$(NEXT)
251 T9$=$"F0F0F0F00F0F0F0FF0F0F0F00F0F0F0FF0F0F0F00F0F0F0FF0F0F0F00F0F0F0F"
252 GOSUB _output:ARGS LIST$(NEXT)
253 SET TILE 128 TO T9$
254 GOSUB _output:ARGS LIST$(NEXT)
255 FILL TILEMAP TILE 128 PALETTE 2

260 GOSUB _output:ARGS LIST$(NEXT)
261 DIM A$(5):FOR I=1 TO 5:A$(I)=WORD$(400+I)+T$(I):NEXT
262 GOSUB _output:ARGS LIST$(NEXT)
263 SET TILE *A$
266 FOR I=1 TO 5
267 GOSUB _assert:ARGS "GETTILE$(%%)=T$(%%)" % (400+I, I)
268 NEXT

290 GOSUB _output:ARGS LIST$(NEXT)
291 FILL TILEMAP (5,4) - (8,7) TILE 501 PALETTE 2 
292 T0$="802080208020802080208020"
293 T1$="8020F521F521F521F5218020"
294 GOSUB _assert:ARGS "PEEK$(@20,$188,12)=$`%%`" % (T0$)
295 GOSUB _assert:ARGS "PEEK$(@20,$208,12)=$`%%`" % (T1$)
296 GOSUB _assert:ARGS "PEEK$(@20,$288,12)=$`%%`" % (T1$)
297 GOSUB _assert:ARGS "PEEK$(@20,$308,12)=$`%%`" % (T1$)
298 GOSUB _assert:ARGS "PEEK$(@20,$388,12)=$`%%`" % (T1$)
299 GOSUB _assert:ARGS "PEEK$(@20,$408,12)=$`%%`" % (T0$)



300 GOSUB _output:ARGS LIST$(NEXT)
301 FOR T=1 TO 5:SET TILEMAP (40-T*4,T*2) TO TILE 500+T PALETTE 2:NEXT

310 GOSUB _output:ARGS LIST$(NEXT)
311 FILL TILEMAP (20,10)-(27,17) TILE 502 PALETTE 2
312 GOSUB _output:ARGS LIST$(NEXT)
313 DIM A(20):GET TILEMAP (4,3)-(9,8),*A
314 GOSUB _output:ARGS LIST$(NEXT)
315 GET TILEMAP (4,3)-(9,8),^A$
316 'bad test:GOSUB _assert:ARGS "A$=ASC$(`0606`+T0$+T1$+T1$+T1$+T1$+T0$)"

320 GOSUB _output:ARGS LIST$(NEXT)
321 PUT TILEMAP (21,11),*A
322 GOSUB _output:ARGS LIST$(NEXT)
323 PUT TILEMAP (11,11),^A$
330 REM add asserts for PUT *A
340 REM add asserts for PUT ^A$:

350 DIM TC$(5):FOR L=351 TO 355:GOSUB _output:ARGS LIST$(L):NEXT
351 TC$(1)=$"7000007777707777777077777770777777707777777077777770777777777777"
352 TC$(2)=$"7700077777707777777077777770777777707777777077777700077777777777"
353 TC$(3)=$"7077777770777777707777777077777770777777707777777000007777777777"
354 TC$(4)=$"7000007770777777707777777000077770777777707777777000007777777777"
355 TC$(5)=$"7700077770777077707777777700077777777077707770777700077777777777"

360 GOSUB _output:ARGS LIST$(NEXT)
361 C$="TILES":C=LEN(C$)
362 GOSUB _output:ARGS LIST$(NEXT)
363 FOR I=1 TO C:SET TILE 490+I TO CHR C$[I],0,7: NEXT
364 GOSUB _output:ARGS LIST$(NEXT)
365 FOR I=1 TO C:SET TILEMAP (2+I,9) TO TILE 490+I:NEXT
366 FOR I=1 TO C:GOSUB _assert:ARGS "GETTILE$(490+%%)=TC$(%%)" % (I,I):NEXT


380 REM TILEMAPX : TILEMAPY : TILEMAP(x,y)

390 SCREEN 0,1:GOSUB _getkey:SCREEN 3,0

500 GOSUB _output:ARGS LIST$(NEXT)
502 DEF TILELIST T$=501,502,503,504,505
504 GOSUB _assert:ARGS "T$=$`F501F601F701F801F901`"

510 GOSUB _output:ARGS LIST$(NEXT)
512 DEF ATTRLIST R$ = 2,4,6,8,64
514 GOSUB _assert:ARGS "R$=$`0204060840`"

520 GOSUB _output:ARGS LIST$(NEXT)
522 DEF PALETTELIST P$ = 0,1,0,2,3
524 GOSUB _assert:ARGS "P$=$`0001000203`"

530 GOSUB _output:ARGS LIST$(NEXT)
532 DEF SPRITE S$ = 1,0,0;2,16,0;3,8,8;4,0,16;5,16,16
534 GOSUB _assert:ARGS "S$=$`051818010000021000030808040010051010`"

540 GOSUB _output:ARGS LIST$(NEXT)
542 SET SPRITE S$ TILE T$ PALETTE P$ ATTR R$ POS 10,10
544 G$="0A000AF5031A000AF615120012F7070A001AF8291A001AF971"
546 GOSUB  _assert:ARGS "GETSPRITE$(S$)=$`"+G$+"`"

560 GOSUB _output:ARGS LIST$(NEXT)
561 DS$=$"010000020800030008"
562 GOSUB _output:ARGS LIST$(NEXT)
563 DEF SPRITE SS$ = ^DS$
564 GOSUB _assert:ARGS "SS$=$`031010010000020800030008`"

580 GOSUB _output:ARGS LIST$(NEXT)
582 DEF SPRITE SS$ = (2,2),11
584 GOSUB _assert:ARGS "SS$=$`0410100B00000C08000D00080E0808`"
