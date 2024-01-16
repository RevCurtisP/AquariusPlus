100 REM Test Tiles and Sprites
110 QU=0:REM 0=Screen,1=Printer
120 SET FAST ON
130 SCREEN 3:GOSUB _init
133 SET FNKEY 3 TO \"RUN /au/ts.baq\r"
134 SET FNKEY 4 TO \"goto _dump\r"

200 GOSUB _title:ARGS "Tiles and Sprites"

210 GOSUB _output:ARGS LIST$(NEXT)
211 DEF RGBLIST R$=0,0,0;1,1,1;2,2,2;3,3,3;4,4,4;5,5,5;6,6,6;7,7,7;8,8,8;9,9,9;10,10,10;11,11,11;12,12,12;13,13,13;14,14,14;15,15,15
216 GOSUB _output:ARGS LIST$(NEXT)
217 SET PALETTE 0 TO R$
218 GOSUB _assert:ARGS "GETPALETTE$(0)=R$"

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
248 GOSUB _pause

249 goto 500

250 FOR PL=0 TO 3:P=PL*16:FOR PR=0 TO 1:FOR FL=0 TO 3:A=PR*64+FL*2
251 GOSUB _output:ARGS "FILLINT @40,0,$1000,$01F5+$%%" % (HEX$((P+A)*256))
252 FILLINT @40,0,$1000,501+(P+A)*256
253 GOSUB _output:ARGS LIST$(NEXT)
254 POKE @20,$1000,$FF
255 GOSUB _output:ARGS "FILL TILEMAP TILE 501 ATTR $%% PALETTE %%" % (HEX$(A),PL)
256 FILL TILEMAP TILE 501 ATTR A PALETTE PL
257 GOSUB _assert:ARGS "COMPARE(@20,0,@40,0,$1000)"
258 GOSUB _assert:ARGS "PEEK(@20,$1000)=$FF"
259 NEXT:GOSUB _pause:NEXT:NEXT

260 GOSUB _output:ARGS LIST$(NEXT)
261 FILL TILEMAP (5,4) - (8,7) TILE 128
262 T0$="F577F577F577F577F577F577"
263 T1$="F5778000800080008000F577"
264 GOSUB _assert:ARGS "PEEK$(@20,$188,12)=$`%%`" % (T0$)
265 GOSUB _assert:ARGS "PEEK$(@20,$208,12)=$`%%`" % (T1$)
266 GOSUB _assert:ARGS "PEEK$(@20,$288,12)=$`%%`" % (T1$)
267 GOSUB _assert:ARGS "PEEK$(@20,$308,12)=$`%%`" % (T1$)
268 GOSUB _assert:ARGS "PEEK$(@20,$388,12)=$`%%`" % (T1$)
269 GOSUB _assert:ARGS "PEEK$(@20,$408,12)=$`%%`" % (T0$)

300 REM SET TILEMAP
310 REM GET TILEMAP
320 REM PUT TILEMAP
330 REM TILEMAPX : TILEMAPY : TILEMAP(x,y)


500 GOSUB _output:ARGS LIST$(NEXT)
502 DEF TILELIST T$=501,502,503,504,505
504 GOSUB _assert:ARGS "T$=$`F501F601F701F801F901`"

510 GOSUB _output:ARGS LIST$(NEXT)
512 DEF SPRITE S$ = 1,0,0;2,16,0;3,8,8;4,0,16;5,16,16
514 GOSUB _assert:ARGS "S$=$`051818010000021000030808040010051010`"

520 GOSUB _output:ARGS LIST$(NEXT)
522 DEF PALETTELIST P$ = 0,1,0,2,3
524 GOSUB _assert:ARGS "P$=$`0001000203`"

