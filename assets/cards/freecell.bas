1000 REM Freecell Demo
1005 SET FNKEY 3 TO \"run freecell.baq\r"
1010 SET SPEED 3
1020 CLEAR 4096: 'Reserve string space for the tileclips
1030 E=RND(-VAL(TIME$)/VAL(DATE$)):'Seed random number generator
1040 SET SPRITE * CLEAR: 'Disable all sprites

1090 PRINT "Shuffling the deck"

1110 REM Card array indexes
1111 REM Suits: 0=Clubs, 1=Hearts, 2=Diamonds, 3=Spades
1112 REM Ranks: 1=Ace ... 13=King
1113 REM Backs: (0,0)=Red, (1,0)=Blue
1114 REM Jokers: (2,0)=Red, (3,0)=Black
1115 REM Blanks: (0,14)=Filled, (1,14)=Unfilled
1116 REM Cursor: (2,14)=Open Hand, (3,14)=Grabbing Hand

1120 REM Card value format: High nybble=Suit, Low nybble=Rank
1122 DIM C$(3,14): 'The tile clips (suit,rank)
1124 DIM D(52): 'The deck before it is distributed
1126 BG=16:DIM G(7,BG): 'Playing grid (column,row).
1128 DIM B(7): 'Bottom card cell in each column
1130 DIM Q(3):Q(1)=1:Q(2)=1: 'Suit Color: 0=Black, 1=Red

1140 REM Sprites
1142 DEF SPRITE SC$=(5,6),11: 'Card - spritles 11 - 40
1144 DEF SPRITE SH$=(3,3),49: 'Hand - spritles 41 - 49

1200 REM Load palette and tile definitions
1210 LOAD PALETTE 1,"cards.pal"
1220 REM Skip if tiles already loaded
1225 IF PEEK$(@20,4096,4)<>$"33320122" THEN LOAD "cards.tset",@20,4096

1230 REM Load Sound Clips
1232 SP=41:S1=0:S2=2048
1234 LOAD "cardplace.saq",@SP,S1
1236 LOAD "cardpick.saq",@SP,S2

1300 REM Load the Tile clips, substitute for unimplemented
1302 LOAD "cards.clip",*C$

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
2110 FOR D=1 TO 52: 'From top to bottom of deck
2120 E=INT(RND(1)*52)+1: 'Random number from 1 to 52
2130 T=D(D):D(D)=D(E):D(E)=T: 'Swap the cards
2140 NEXT

2200 REM Draw Layout
2210 REM Top Row - Blank Cards
2220 TX=0:TY=0: 'Start in upper left hand corner
2230 FOR S=1 TO 0 STEP -1: 'Filled then unfilled
2240 FOR R=0 TO 3: 'Four of each
2250 PUT TILEMAP (TX,TY),^C$(S,14):TX=TX+5: 'Draw the card
2260 NEXT:NEXT

2300 REM Place cards on the grid
2302 REM Grid row 0 starts as empty
2310 GC=0:GR=1: 'Grid column and ror
2315 TX=0:TY=6: 'Start below first blank card
2320 FOR D=1 TO 52: 'Loop through the shuffled deck
2325 B(GC)=GR: 'Set bottom row of column
2330 C=D(D): 'Get card value
2335 G(GC,GR)=C: 'Put it in the grid
2340 S=INT(C/16): 'Get suit from high nybble
2345 R=C AND 15: 'Get rank from low nybble
2350 PUT TILEMAP (TX,TY),^C$(S,R): 'Draw the card
2355 PLAY SAMPLE @SP,S1
2360 GC=GC+1: 'Move to next grid column
2365 TX=TX+5: 'Move to next tilemap column
2370 REM If end of row, set column to zero, move to next row
2380 IF TX>39 THEN GC=0:GR=GR+1:TX=0:TY=TY+1
2390 NEXT

2400 REM Show the hand sprite
2410 SET SPRITE SH$ ON

3000 _main: 'Let the game begin

3100 _autobuild:
3110 'FL=13: 'Lowest rank on foundation
3112 'FOR PC=4 TO 7:FR=C(PC,0)AND15:IF FR<FL THEN FL=FR
3114 'NEXT
3116 'IF FL=13 THEN REM Game Over
3120 'FOR GC=0 TO 7:GR=B(GC)
3122 'C=G(GC,B(GR)):S=INT(C/16):R=C AND 15
3124 'FOR PC=4 TO 7:C=C(PC,0):PS=INT(C/16):PR=C AND 15
3126 'IF PR=R AND S=PS+1 THEN GOSUB _automove
3128 'NEXT:NEXT

4100 _hand: 'Move the hand cursor and check for button click
4110 SET SPRITE SH$ TILECLIP C$(2,14): 'Display open hand
4115 SET SPRITE SH$ ON: 'SET SPRITE ...TILECLIP turns the sprite off
4120 FOR MB=0 TO 1: 'Faking WHILE B=0
4130 MX=MOUSEX:MY=MOUSEY:MB=MOUSEB: 'Read the mouse
4140 SET SPRITE SH$ POS MX-12,MY-12: 'Center hand over mouse position
4150 NEXT MB: 'Continue looping until button is pressed

4200 REM Try to pick up a card
4210 TX=INT(MX/8):TY=INT(MY/8): 'Calculate tilemap position
4220 GC=INT(TX/5):GX=TX MOD 5: 'Grid column and tile position in column
4230 IF GX<1 OR GX>3 THEN GOTO _nograb: 'Don't grab if too close to edge
4240 IF TY<6 THEN GOTO _toprow: 'Handle top row separately

4300 REM Cursor is below top row of grid
4310 GR=TY-7:IF GR>B(GC) AND GR<B(GC)+2 THEN GR=B(GC)
4320 IF GR<0 THEN GOTO _nograb: 'Calculate 
4330 IF GR>BG THEN GOTO _nograb: 'Can't grab if below last row
4340 C=G(GC,GR): 'Get card value
4350 IF C=0 THEN GOTO _nograb: 'Can't pick up if no card in cell
4360 IF GR=BG THEN GOTO _pickup: 'Can always pick up from bottom row
4370 IF G(GC,GR+1)=0 THEN GOTO _pickup: 'Okay to pick up if no card on top

4400 _nograb: 'Here if we couldn't grab a card
4410 FOR MB=1 TO 0 STEP -1: 'Faking WHILE B<>0
4420 MX=MOUSEX:MY=MOUSEY:MB=MOUSEB: 'Read the mouse
4430 SET SPRITE SH$ POS MX-12,MY-12: 'Center hand over mouse position
4480 NEXT MB: REM Loop until button is released4
4490 GOTO _hand

4500 _toprow: 'Cursor is in top row of grid
4520 GR=0:GY=TY: 'Grid row and tile position in row
4530 IF GY<1 OR GY>4 THEN GOTO _nograb: 'Don't grab if too close to edge
4535 IF GC>3 THEN GOTO _nograb: 'Can't pick up from from foundations
4540 C=G(GC,GR): 'Get card value
4550 IF C=0 THEN GOTO _nograb: 'Can't pick up if no card in cell

5000 _pickup: 'Pick up the card
5005 PLAY SAMPLE @SP,S2
5010 SET SPRITE SH$ TILECLIP C$(3,14): 'Display grabbing hand
5015 SET SPRITE SH$ ON: 'Reenable sprite
5020 C=G(GC,GR):S=INT(C/16):R=C AND 15:Q=Q(S): 'Card value, suit, rank, color
5030 CX=GC*5:CY=0:IF GR THEN CY=GR+5: 'Card tile column and row
5035 SET SPRITE SC$ TILECLIP C$(S,R): 'Set card sprite to selected card
5040 SET SPRITE SC$ POS CX*8,CY*8: 'Place card sprite over card tiles
5045 SET SPRITE SC$ ON: 'Renable sprite
5050 IF GR=0 THEN CY=0:PUT TILEMAP (CX,CY),^C$(1,14):GOTO _drag
5060 CY=GR+5:IF GR=1 THEN CY=6:FILL TILEMAP (CX,CY)-(CX+4,CY+5) TILE 128 PALETTE 1:GOTO _drag
5065 DR=GR-1:DC=G(GC,DR):DS=DC/16:DR=DC AND 15: 'Get card underneath
5070 PUT TILEMAP (CX,CY-1),^C$(DS,DR): 'Redraw card underneath
5080 FILL TILEMAP (CX,CY+5)-(CX+4,CY+5) TILE 128 PALETTE 1

5100 _drag: 'Drag the card around
5110 FOR MB=1 TO 0 STEP -1: 'Faking WHILE B<>0
5120 MX=MOUSEX:MY=MOUSEY:MB=MOUSEB: 'Read the mouse
5130 SET SPRITE SH$ POS MX-12,MY-12: 'Center hand over mouse position
5140 SET SPRITE SC$ POS MX-20,MY-24: 'Center card over mouse position
5180 NEXT MB: 'Loop until button is released4

5200 REM Drop the card
5210 TX=INT(MX/8):TY=INT(MY/8): 'Calculate tilemap position
5220 PC=INT(TX/5):PX=TX MOD 5: 'Grid column and tile position in column
5225 NX=PC*5: 'Left tile column of grid position
5230 IF PX<1 OR PX>3 THEN GOTO _putback: 'Put back if too close to edge
5240 IF TY>5 THEN GOTO _piledrop: 'Card is not on top row

5300 REM Try to drop on top row
5305 PB=0: 'Do not update bottom of column
5310 PR=0:NY=0:PY=TY: 'Grid row and tile position in row
5315 IF PY>4 THEN GOTO _putback: 'Don't grab if too close to edge
5320 N=G(PC,PR): 'Get card in new spot
5325 NS=INT(NC/16):NR=NC AND 15: 'Suit and Rank in spot
5330 IF PC>3 THEN GOTO _foundation: 'Foundation has different rules
5340 IF N<>0 THEN GOTO _putback: 'Can't pick up if no card in cell
5345 GOTO _dropcard

5400 _foundation: 'Try to put on foundation
5410 FC=G(PC,PR):FS=INT(FC/16):FR=FC AND 15: 'Card, Suit, Rank in foundation
5420 IF R<>FR+1 THEN GOTO _putback: 'Can only drop next highest card
5430 IF FC=0 THEN GOTO _dropcard: 'Okay to drop if no card in foundation
5435 IF FS=S THEN GOTO _dropcard: 'or card is same suit
5440 GOTO _putback: 'Otherwise put it back

5500 _piledrop
5505 PB=1: 'Do Update bottom of column
5510 PR=B(PC)+1:NY=PR+5: 'Row below bottom of stack, grid column of position
5515 IF PR=1 THEN GOTO _dropcard: 'No cards in stack, okay to drop
5520 IF PR>BG THEN GOTO _putback: 'No more space on stack
5530 N=G(PC,PR-1): 'Get card at bottom of stack
5535 NS=INT(N/16):NR=N AND 15:NQ=Q(NS): 'Suit, Rank, and Color of card
5540 IF NQ=Q THEN GOTO _putback: 'Can't drop if same color
5545 IF R<>NR-1 THEN GOTO _putback: 'Can't drop if not one less

5800 _dropcard:
5810 G(GC,GR)=0: 'Remove card from old grid position
5815 IF GR THEN B(GC)=B(GC)-1: 'If taken from column, update bottom of column
5820 G(PC,PR)=C: 'Put in new position
5825 IF PB THEN B(PC)=PR
5830 CX=NX:CY=NY

5900 _putback: 'Put the card back
5905 PLAY SAMPLE @SP,S1
5910 PUT TILEMAP (CX,CY),^C$(S,R)
5920 SET SPRITE SC$ OFF
5990 GOTO _main

8000 _automove:
8010 PLAY SAMPLE @SP,S2
8090 RETURN

9000 _redraw:
9010 IF GR=0 THEN CY=0:PUT TILEMAP (CX,CY),^C$(1,14):GOTO _drag
9020 CY=GR+5:IF GR=1 THEN CY=6:FILL TILEMAP (CX,CY)-(CX+4,CY+5) TILE 128 PALETTE 1:GOTO _drag
9030 DR=GR-1:DC=G(GC,DR):DS=DC/16:DR=DC AND 15: 'Get card underneath
9040 PUT TILEMAP (CX,CY-1),^C$(DS,DR): 'Redraw card underneath
9050 FILL TILEMAP (CX,CY+5)-(CX+4,CY+5) TILE 128 PALETTE 1
