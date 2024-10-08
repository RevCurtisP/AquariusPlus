100 'Display all characters
105 SET FNKEY 3 TO \"RUN ASCII_TABLE.BAS\r"
110 DH$(0)=" ":FOR I=1 TO 9:DH$(I)=CHR$(128+I):NEXT
112 DIM DT$(99,1)
114 FOR I=0 TO 99:DT$(I,0)=CHR$(128+I):DT$(I,1)=CHR$(128+I-100*(I<10))
116 NEXT
120 DIM HL$(15),HR$(15)
122 FOR I=0 TO 9:HL$(I)=CHR$(I+128):HR$(I)=CHR$(I+238):NEXT
124 FOR I=10 TO 15:HL$(I)=CHR$(I+6):HR$(I)=CHR$(I+14):NEXT

200 SCREEN 3:CLS 0,7
210 USE CHRSET "doubled-hex.char"

310 FOR R=0 TO 15
315 FOR C=0 TO 15
320 A=C*16+R:H=INT(A/100):T=A MOD 100:HL=INT(A/16):HR=A AND 15
325 PRINT DH$(H);DT$(T,SGN(H));" ";CHR$(127);
330 ';HL$(HL);HR$(HR);"*";
370 NEXT
375 PRINT
380 NEXT

600 END
610 W=80:SCREEN 3:CLS 0,7
615 H$=STRING$(8,$AC):V$=$"D6":S$="        ":VS$=V$+S$
620 TL$=$"DE":TM$=$"DC":TR$=$"CE":TH$=H$+TM$
625 BL$=$"CF":BM$=$"CC":BR$=$"DF":BH$=H$+BM$
710 PRINT TL$;TH$;TH$;TH$;TH$;TH$;TH$;TH$;H$;TR$
720 FOR R=0 TO 7
725 :FOR C=0 TO 7
730 ::A=(C+8)*16+R
735 ::PRINT V$;PAD$(STR$(A),-3);" ";HEX$(A);"  ";
740 ::F=W+9+C*9+R*W*2+W
745 ::POKE SCREEN F,A:POKE COLOR F,6
770 :NEXT
775 :PRINT V$
780 :IF R<7 THEN PRINT VS$;VS$;VS$;VS$;VS$;VS$;VS$;VS$;V$
785 NEXT
790 PRINT BL$;BH$;BH$;BH$;BH$;BH$;BH$;BH$;H$;BR$
800 LOCATE 1,20:'K=GETKEY
