100 REM DEF TILE ... TO CHR demo
110 C$="SPRITE"
120 FOR I=1 TO 6
130 SET TILE 500+I TO CHR ASC(MID$(C$,I)),0,7
140 NEXT
150 DEF TILELIST T$=501,502,503,504,505,506
160 DEF SPRITE S$ = [1,2,3,4,5,6]
170 SET SPRITE S$ TILE T$ 

200 SCREEN ,,1
210 X=136:Y=96:XD=RND(0):YD=RND(0)
220 SET SPRITE S$ ON

230 SET SPRITE S$ POS X,Y
240 X=X+XD:Y=Y+YD
250 IF X>272 THEN XD=-RND(0)
260 IF Y>192 THEN YD=-RND(0)
270 IF X<1 THEN XD=RND(0)
280 IF Y<1 THEN YD=RND(0)
290 GOTO 230
