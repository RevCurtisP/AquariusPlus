100 REM Sprite Demo
110 SCREEN 4:CLS 6,0
120 SET TILE 0 TO $"000FF00000F44F0000F44F0000F44F000F4444F00F4444F0F044440FF444444F" 
130 SET TILE 1 TO $"000F488400F488880F488888F4444444F4444444F44FFFF4FFF0110F00011110"
140 DEF SPRITE S$ = 0,4,0 ; 1,0,8 ; 2,8,8
150 DEF TILELIST T$ = 0,1,1
160 DEF ATTRLIST A$ = 0,0,2
170 SET SPRITE S$ TILE T$ ATTR A$
180 SET SPRITE S$ POS 152,180 ON
190 FOR I=180 TO 5 STEP -1:SET SPRITE S$ POS 152,I:FOR D=1 TO 10:NEXT:NEXT