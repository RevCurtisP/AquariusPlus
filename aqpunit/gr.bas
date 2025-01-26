100 REM Miscelleneous Graphics Functions
110 QG$="pp"
130 GOSUB _init

200 GOSUB _title:ARGS "RGB Functions"
210 FOR R=0 TO 13:G=R+1:B=R+2:
212 S$=RIGHT$(HEX$(R),1)+RIGHT$(HEX$(G),1)+RIGHT$(HEX$(B),1)
215 C=R*256+G*16+B:C$=CHR$(G*16+B)+CHR$(R):H$=HEX$(C$)
220 GOSUB _assert:ARGS "RGB(%%,%%,%%)=%%)" % (R,G,B,C)
222 GOSUB _assert:ARGS \"RGB$(%%,%%,%%)=$\"%%\")" % (R,G,B,H$)
224 GOSUB _assert:ARGS \"RGB(\"%%\")=%%)" % (S$,C)
226 GOSUB _assert:ARGS \"RGB$(\"%%\")=$\"%%\")" % (S$,H$)
290 NEXT
