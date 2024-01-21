1000 REM Freecell Demo
1010 SET FNKEY 3 TO \"run freecell.bas\r"
1020 CLEAR 4096:REM Reserve string space for the tileclips
1030 E=RND(-VAL(TIME$)):REM Set Random seed
1040 SET SPRITE * CLEAR:REM Disable all sprites

1110 REM Card array indexes
1111 REM Suits: 0=Clubs, 1=Hearts, 2=Diamonds, 3=Spades
1112 REM Ranks: 1=Ace ... 13=King
1113 REM Backs: (0,0)=Red, (1,0)=Blue
1114 REM Jokers: (2,0)=Red, (3,0)=Black
1115 REM Blanks: (0,14)=Filled, (1,14)=Unfilled
1116 REM Cursor: (2,14)=Open Hand, (3,14)=Grabbing Hand

1120 REM Card value format: High nybble=Suit, Low nybble=Rank
1122 DIM C$(3,14):REM The tile clips (suit,rank)
1124 DIM D(52):REM The deck before it is distributed
1126 BG=16:DIM G(7,BG): REM Playing grid (column,row).
1128 DIM B(7):REM Bottom card cell in each column
1130 DIM Q(3):Q(1)=1:Q(2)=1:REM Suit Color: 0=Black, 1=Red

1140 REM Sprites
1142 DEF SPRITE SC$=[11,12,13,14,15],[16,17,18,19,10],[21,22,23,24,25],[26,27,28,29,30],[31,32,33,34,35],[36,37,38,39,40]
1144 DEF SPRITE SH$=[41,42,43],[44,45,46],[47,48,49]:REM Hand sprite

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
2325 B(GC)=GR:REM Set bottom row of column
2330 C=D(D):REM Get card value
2335 G(GC,GR)=C:REM Put it in the grid
2340 S=INT(C/16):REM Get suit from high nybble
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

3200 REM Try to pick up a card
3210 TX=INT(X/8):TY=INT(Y/8):REM Calculate tilemap position
3220 GC=INT(TX/5):GX=TX MOD 5:REM Grid column and tile position in column
3230 IF GX<1 OR GX>3 THEN GOTO _nograb:REM Don't grab if too close to edge
3240 IF TY<6 THEN GOTO _toprow:REM Handle top row separately

3300 REM Cursor is below top row of grid
3310 GR=TY-7:IF GR>B(GC) AND GR<B(GC)+2 THEN GR=B(GC)
3320 IF GR<0 THEN GOTO _nograb:REM Calculate 
3330 IF GR>BG THEN GOTO _nograb:REM Can't grab if below last row
3340 C=G(GC,GR):REM Get card value
3350 IF C=0 THEN GOTO _nograb:REM Can't pick up if no card in cell
3360 IF GR=BG THEN GOTO _pickup:REM Can always pick up from bottom row
3370 IF G(GC,GR+1)=0 THEN GOTO _pickup:REM Okay to pick up if no card on top

3400 _nograb:REM Here if we couldn't grab a card
3410 FOR B=1 TO 0 STEP -1:REM Faking WHILE B<>0
3420 X=MOUSEX:Y=MOUSEY:B=MOUSEB:REM Read the mouse
3430 SET SPRITE SH$ POS X-12,Y-12:REM Center hand over mouse position
3480 NEXT B: REM Loop until button is released4
3490 GOTO _main

3500 _toprow:REM Cursor is in top row of grid
3520 GR=0:GY=TY:REM Grid row and tile position in row
3530 IF GY<1 OR GY>4 THEN GOTO _nograb:REM Don't grab if too close to edge
3535 IF GC>3 THEN GOTO _nograb:REM Can't pick up from from foundations
3540 C=G(GC,GR):REM Get card value
3550 IF C=0 THEN GOTO _nograb:REM Can't pick up if no card in cell

4000 _pickup:REM Pick up the card
4010 SET SPRITE SH$ TILECLIP C$(3,14):REM Display grabbing hand
4015 SET SPRITE SH$ ON:REM Renable sprite
4020 C=G(GC,GR):S=INT(C/16):R=C AND 15:Q=Q(S):REM Card value, suit, rank, color
4030 CX=GC*5:CY=0:IF GR THEN CY=GR+5:REM Card tile column and row
4035 SET SPRITE SC$ TILECLIP C$(S,R):REM Set card sprite to selected card
4040 SET SPRITE SC$ POS CX*8,CY*8:REM Place card sprite over card tiles
4045 SET SPRITE SC$ ON:REM Renable sprite
4050 IF GR=0 THEN CY=0:PUT TILEMAP (CX,CY),^C$(1,14):GOTO _drag
4060 CY=GR+5:IF GR=1 THEN CY=6:FILL TILEMAP (CX,CY)-(CX+4,CY+5) TILE 128 PALETTE 1:GOTO _drag
4065 DR=GR-1:DC=G(GC,DR):DS=DC/16:DR=DC AND 15:REM Get card underneath
4070 PUT TILEMAP (CX,CY-1),^C$(DS,DR):REM Redraw card underneath
4080 FILL TILEMAP (CX,CY+5)-(CX+4,CY+5) TILE 128 PALETTE 1

4100 _drag:REM Drag the card around
4110 FOR B=1 TO 0 STEP -1:REM Faking WHILE B<>0
4120 X=MOUSEX:Y=MOUSEY:B=MOUSEB:REM Read the mouse
4130 SET SPRITE SH$ POS X-12,Y-12:REM Center hand over mouse position
4140 SET SPRITE SC$ POS X-20,Y-24:REM Center card over mouse position
4180 NEXT B: REM Loop until button is released4

4200 REM Drop the card
4210 TX=INT(X/8):TY=INT(Y/8):REM Calculate tilemap position
4220 PC=INT(TX/5):PX=TX MOD 5:REM Grid column and tile position in column
4225 NX=PC*5:REM Left tile column of grid position
4230 IF PX<1 OR PX>3 THEN GOTO _putback:REM Put back if too close to edge
4240 IF TY>5 THEN GOTO _piledrop:REM Card is not on top row


4300 REM Try to drop on top row
4305 PB=0:REM Do not update bottom of column
4310 PR=0:NY=0:PY=TY:REM Grid row and tile position in row
4315 IF PY>4 THEN GOTO _putback:REM Don't grab if too close to edge
4320 N=G(PC,PR):REM Get card in new spot
4325 NS=INT(NC/16):NR=NC AND 15:REM Suit and Rank in spot
4330 IF PC>3 THEN GOTO _foundation:REM Foundation has different rules
4340 IF N<>0 THEN GOTO _putback:REM Can't pick up if no card in cell
4345 GOTO _dropcard

4400 _foundation:REM Try to put on foundation
4410 FC=G(PC,PR):FS=INT(FC/16):FR=FC AND 15:REM Card, Suit, Rank in foundation
4420 IF R<>FR+1 THEN GOTO _putback:REM Can only drop next highest card
4430 IF FC=0 THEN GOTO _dropcard:REM Okay to drop if no card in foundation
4435 IF FS=S THEN GOTO _dropcard:REM or card is same suit
4440 GOTO _putback:REM Otherwise put it back

4500 _piledrop
4505 PB=1:REM Do Update bottom of column
4510 PR=B(PC)+1:NY=PR+5:REM Row below bottom of stack, grid column of position
4515 IF PR=1 THEN GOTO _dropcard:REM No cards in stack, okay to drop
4520 IF PR>BG THEN GOTO _putback:REM No more space on stack
4530 N=G(PC,PR-1):REM Get card at bottom of stack
4535 NS=INT(N/16):NR=N AND 15:NQ=Q(NS):REM Suit, Rank, and Color of card
4540 IF NQ=Q THEN GOTO _putback:REM Can't drop if same color
4545 IF R<>NR-1 THEN GOTO _putback:REM Can't drop if not one less

4800 _dropcard:
4810 G(GC,GR)=0:REM Remove card from old grid position
4815 IF GR THEN B(GC)=B(GC)-1:REM If taken from column, update bottom of column
4820 G(PC,PR)=C:REM Put in new position
4825 IF PB THEN B(PC)=PR
4830 CX=NX:CY=NY

4900 _putback:REM Put the card back
4910 PUT TILEMAP (CX,CY),^C$(S,R)
4920 SET SPRITE SC$ OFF
4990 GOTO _main
