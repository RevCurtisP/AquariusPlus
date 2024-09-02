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
261 FILL TILEMAP (5,4) - (8,7) TILE 501 PALETTE 2 
262 T0$="802080208020802080208020"
263 T1$="8020F521F521F521F5218020"
264 GOSUB _assert:ARGS "PEEK$(@20,$188,12)=$`%%`" % (T0$)
265 GOSUB _assert:ARGS "PEEK$(@20,$208,12)=$`%%`" % (T1$)
266 GOSUB _assert:ARGS "PEEK$(@20,$288,12)=$`%%`" % (T1$)
267 GOSUB _assert:ARGS "PEEK$(@20,$308,12)=$`%%`" % (T1$)
268 GOSUB _assert:ARGS "PEEK$(@20,$388,12)=$`%%`" % (T1$)
269 GOSUB _assert:ARGS "PEEK$(@20,$408,12)=$`%%`" % (T0$)

300 GOSUB _output:ARGS LIST$(NEXT)
301 FOR T=1 TO 5:SET TILEMAP (40-T*4,T*2) TO TILE 500+T PALETTE 2:NEXT

310 GOSUB _output:ARGS LIST$(NEXT)
311 FILL TILEMAP (20,10)-(27,17) TILE 502 PALETTE 2
312 GOSUB _output:ARGS LIST$(NEXT)
313 DIM A(20):GET TILEMAP (4,3)-(9,8),*A
314 GOSUB _output:ARGS LIST$(NEXT)
315 GET TILEMAP (4,3)-(9,8),^A$
316 GOSUB _assert:ARGS "A$=ASC$(`0606`+T0$+T1$+T1$+T1$+T1$+T0$)"

320 GOSUB _output:ARGS LIST$(NEXT)
321 PUT TILEMAP (21,11),*A
322 GOSUB _output:ARGS LIST$(NEXT)
323 PUT TILEMAP (11,11),^A$
330 REM add asserts for PUT *A
340 REM add asserts for PUT ^A$:

350 GOSUB _output:ARGS LIST$(NEXT)
351 C$="TILES":C=LEN(C$)
352 GOSUB _output:ARGS LIST$(NEXT)
353 FOR I=1 TO C:SET TILE 490+I TO CHR ASC(MID$(C$,I)),0,7: NEXT
354 GOSUB _output:ARGS LIST$(NEXT)
355 FOR I=1 TO C:SET TILEMAP (2+I,9) TO TILE 490+I:NEXT

380 REM TILEMAPX : TILEMAPY : TILEMAP(x,y)

390 SCREEN 0,1:PAUSE:SCREEN 3,0

500 GOSUB _output:ARGS LIST$(NEXT)
502 DEF TILELIST T$=501,502,503,504,505
504 GOSUB _assert:ARGS "T$=$`F501F601F701F801F901`"

510 GOSUB _output:ARGS LIST$(NEXT)
512 DEF ATTRLIST P$ = 0,1,0,2,3
514 GOSUB _assert:ARGS "P$=$`0001000203`"

520 GOSUB _output:ARGS LIST$(NEXT)
522 DEF PALETTELIST P$ = 0,1,0,2,3
524 GOSUB _assert:ARGS "P$=$`0001000203`"

530 GOSUB _output:ARGS LIST$(NEXT)
532 DEF SPRITE S$ = 1,0,0;2,16,0;3,8,8;4,0,16;5,16,16
534 GOSUB _assert:ARGS "S$=$`051818010000021000030808040010051010`"


