100 'Display all characters
110 W=80:SCREEN 3:CLS 0,7
115 H$=STRING$(8,$AC):V$=$"D6":S$="        ":VS$=V$+S$
120 TL$=$"DE":TM$=$"DC":TR$=$"CE":TH$=H$+TM$
125 BL$=$"CF":BM$=$"CC":BR$=$"DF":BH$=H$+BM$
210 PRINT TL$;TH$;TH$;TH$;TH$;TH$;TH$;TH$;H$;TR$
220 FOR R=0 TO 7
225 :FOR C=0 TO 7
230 ::A=(C+8)*16+R
235 ::PRINT V$;PAD$(STR$(A),-3);" ";HEX$(A);"  ";
240 ::F=W+9+C*9+R*W*2+W
245 ::POKE SCREEN F,A:POKE COLOR F,6
270 :NEXT
275 :PRINT V$
280 :IF R<7 THEN PRINT VS$;VS$;VS$;VS$;VS$;VS$;VS$;VS$;V$
285 NEXT
290 PRINT BL$;BH$;BH$;BH$;BH$;BH$;BH$;BH$;H$;BR$
400 LOCATE 1,20:'K=GETKEY
