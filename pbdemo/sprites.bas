100 REM 15 Sprite Demo
110 SET SPRITE * CLEAR:SCREEN 4:CLS 6,0
210 FOR T=11 TO 19:SET TILE T TO $"1111111110000001100000011000000110000001100000011000000111111111":NEXT
220 FOR T=21 TO 29:SET TILE T TO $"2222222220000002200000022000000220000002200000022000000222222222":NEXT
230 FOR T=31 TO 39:SET TILE T TO $"3333333330000003300000033000000330000003300000033000000333333333":NEXT
240 FOR T=41 TO 49:SET TILE T TO $"4444444440000004400000044000000440000004400000044000000444444444":NEXT
250 FOR T=51 TO 59:SET TILE T TO $"5555555550000005500000055000000550000005500000055000000555555555":NEXT
260 FOR T=61 TO 69:SET TILE T TO $"6666666660000006600000066000000660000006600000066000000666666666":NEXT
270 FOR T=71 TO 79:SET TILE T TO $"7777777770000007700000077000000770000007700000077000000777777777":NEXT
280 FOR T=81 TO 89:SET TILE T TO $"8888888880000008800000088000000880000008800000088000000888888888":NEXT
290 FOR T=91 TO 99:SET TILE T TO $"9999999990000009900000099000000990000009900000099000000999999999":NEXT

310 A$=STRING$(9,0)
320 S=0:FOR I=1 TO 7:T=I*10:X(I)=T*4:Y(I)=10
330 DEF TILELIST T$=T+1,T+2,T+3,T+4,T+5,T+6,T+7,T+8,T+9
340 DEF SPRITE S$(I)=S,0,0;S+1,8,0;S+2,16,0; S+3,0,8;S+4,8,8;S+5,8,16;S+6,0,16;S+7,16,8;S+8,16,16
350 SET SPRITE S$(I) TILE T$ ATTR A$ POS X(I),Y(I) ON
360 S=S+9:NEXT
370 S1$=S$(1):S2$=S$(2):S3$=S$(3):S4$=S$(4):S5$=S$(5)::S6$=S$(6):S7$=S$(7)

400 FOR Y=10 TO 160
410 SET SPRITE S1$ POS  40,Y; S2$ POS  80,Y; S3$ POS 120,Y; S4$ POS 160,Y; S5$ POS 200,Y; S6$ POS 240,Y; S7$ POS 280,Y
420 NEXT
430 K=GETKEY

                                         