1000 REM Freecell Demo
1010 SET FNKEY 3 TO \"run freecell.bas\r"
1020 CLEAR 4096:REM Reserve string space for the tileclips
1030 E=RND(-VAL(TIME$)):REM Set Random seed

1110 REM Card array indexes
1111 REM Suits: 0=Clubs, 1=Hearts, 2=Diamonds,3=Clubs
1112 REM Ranks: 1=Ace ... 13=King
1113 REM Backs: (0,0)=Red, (1,0)=Blue
1114 REM Jokers: (2,0)=Red, (3,0)=Black
1115 REM Blanks: (0,14)=Filled, (1,14)=Unfilled
1116 REM Cursor: (2,14)=Open Hand, (3,14)=Grabbing Hand

1120 REM Card value format: High nybble=Suit, Low nybble=Rank
1122 DIM C$(3,14):REM The tile clips (suit,rank)
1124 DIM D(52):REM The deck before it is distributed
1126 DIM G(7,14): REM Playing grid (column,row).

1130 REM Sprites
1132 DEF SPRITE SC$=[11,12,13,14,15],[16,17,18,19,10],[21,22,23,24,25],[26,27,28,29,30],[31,32,33,34,35],[36,37,38,39,40]
1134 DEF SPRITE SH$=[1,2,3],[4,5,6],[7,8,9]:REM Hand sprite

1200 REM Load palette and tile definitions
1210 LOAD PALETTE 1,"cards.pal"
1220 REM Skip if tiles already loaded
1225 IF PEEK$(@20,$40CC,4)<>$"33320122" THEN LOAD "cards.til",@20,4096

1300 REM Load the Tile clips, substitute for unimplemented
1302 REM LOAD "cards.clip",*C$
1310 REM Skip if clips already loaded
1315 IF PEEK$(@40,0,4)<>$"3E0506B5" THEN LOAD "cards.clp",@40,0
1320 A=0:REM Start at beginning of clips
1330 FOR S=0 TO 3:FOR R=0 TO 14:REM suits, ranks
1340 L=PEEK(@40,A):A=A+1:REM Read string length
1350 C$(S,R)=PEEK$(@40,A,L):A=A+L:REM Read string
1360 NEXT:NEXT:REM Next rank and suit

1400 REM Clear the tilemap
1410 FILL TILEMAP TILE 128 PALETTE 1

1500 REM Set up Cursor Sprite

1900 REM Switch to tilemap mode, sprites enabled
1910 SCREEN 0,1,1


2000 REM Reset the Deck
2001 REM using standard 52 card deck
2002 REM D(1)=Top of stack, D(52)=Bottom of stack
2010 D=1
2020 FOR S=0 TO 3
2030 FOR R=1 TO 13
2040 C=S*16+R
2050 D(D)=C
2060 D=D+1
2070 NEXT
2080 NEXT

2100 REM Shuffle the Deck
2110 FOR D=1 TO 52:REM From top to bottom of deck
2120 E=INT(RND(1)*52)+1:REM Random number from 1 to 52
2130 T=D(D):D(D)=D(E):D(E)=T:REM Swap the cards
2140 NEXT

2200 REM Draw Layout
2210 REM Top Row - Blank Cards
2220 X=0:Y=0:REM Start in upper left hand corner
2230 FOR S=1 TO 0 STEP -1:REM Filled then unfilled
2240 FOR R=0 TO 3:REM Four of each
2250 PUT TILEMAP (X,Y),^C$(S,14):X=X+5:REM Draw the card
2260 NEXT:NEXT

2300 REM Place cards on the grid
2302 REM Grid row 0 starts as empty
2310 GC=0:GR=1:REM Grid column and ror
2315 X=0:Y=6:REM Start below first blank card
2320 FOR D=1 TO 52:REM Loop through the shuffled deck
2330 C=D(D):REM Get card value
2335 G(GC,GR)=C:REM Put it in the grid
2340 S=C/16:REM Get suit from high nybble
2345 R=C AND 15:REM Get rank from low nybble
2350 PUT TILEMAP (X,Y),^C$(S,R):REM Draw the card
2360 GC=GC+1:REM Move to next grid column
2365 X=X+5:REM Move to next tilemap column
2370 REM If end of row, set column to zero, move to next row
2380 IF X>39 THEN GC=0:GR=GR+1:X=0:Y=Y+1
2390 NEXT

2400 REM Show the hand sprite
2410 SET SPRITE SH$ ON

3000 REM Let the game begin

3100 _main:
3110 SET SPRITE SH$ TILECLIP C$(2,14):REM Display open hand
3115 SET SPRITE SH$ ON:REM SET SPRITE ...TILECLIP turns the sprite off
3120 FOR B=0 TO 1:REM Faking WHILE B=0
3130 X=MOUSEX:Y=MOUSEY:B=MOUSEB:REM Read the mouse
3140 SET SPRITE SH$ POS X-12,Y-12:REM Center hand over mouse position
3150 NEXT B:REM Continue looping until button is pressed


3200 REM Pick up the card
3210 SET SPRITE SH$ TILECLIP C$(3,14):REM Display grabbing hand
3215 SET SPRITE SH$ ON:REM Renable sprite

3300 REM Drag the card around
3310 FOR B=1 TO 0 STEP -1:REM Faking WHILE B<>0
3320 X=MOUSEX:Y=MOUSEY:B=MOUSEB:REM Read the mouse
3330 SET SPRITE SH$ POS X-12,Y-12:REM Center hand over mouse position
3380 NEXT B: REM Loop until button is released

3900 GOTO _main
