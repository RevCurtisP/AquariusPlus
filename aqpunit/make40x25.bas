10 LOAD TILEMAP "assets/64x32.tmap"
15 LOAD TILESET 128,"assets/tiles.tset"
20 D=0:FOR I=0 TO 25*64*2 STEP 128
30 COPY @20,I,80 TO @32,D
40 D=D+80:NEXT
50 SAVE "assets/40x25.tmap",@32,0,25*40*2
60 FILL BYTES @20,0,4096,$5A
70 LOAD TILEMAP "assets/40x25.tmap"
80 SCREEN 0,1:PAUSE

