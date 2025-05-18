100 REM Conversion Functions
110 QG$="dr"
130 GOSUB _init



200 GOSUB _title:ARGS "Conversion Functions"

210 GOSUB _title:ARGS "Binary String Functions"

220 GOSUB _title:ARGS "ASC() and CHR$()"
222 FOR I=0 TO 255 STEP 17:C=I+(I=34)-(I=0):C$=CHR$(C)
224 GOSUB _assert:ARGS "ASC(`%%`)=%%" % (C$,C)
226 GOSUB _assert:ARGS "CHR$(%%)=`%%`" % (C,C$)
228 NEXT

240 GOSUB _title:ARGS "ASC(), ASC$() and BYTE()"
242 A$=\"\"0F2D4B6987A5C3E1\""
244 H$="$"+A$
245 goto 270
250 GOSUB _assert:ARGS "ASC(%%)=15" % (H$)
251 GOSUB _assert:ARGS "BYTE(%%)=15" % (H$)
254 FOR I=0 TO 7:A=I*32+15-I*2:B=A:IF B>128 THEN B=B-256
255 GOSUB _assert:ARGS "ASC(%%,%%)=%%" % (H$,I+1,A)
256 GOSUB _assert:ARGS "BYTE(%%,%%)=%%" % (H$,I+1,B)
258 NEXT

270 GOSUB _title:ARGS "HEX$(), DEC() and DEC$()"
272 GOSUB _assert:ARGS "HEX$(%%)=%%" % (H$,A$)
276 DIM D$(7)="-8388608","-1","0","1","255","43981","1193046","8388607"
278 DIM H$(7)="800000","FFFFFF","00","01","FF","ABCD","123456","7FFFFF"
280 FOR I=0 TO 7
282 GOSUB _assert:ARGS "HEX$(%%)=`%%`" % (D$(I),H$(I)) 
284 GOSUB _assert:ARGS "DEC(`%%`)=%%" % (H$(I),D$(I)) 
286 GOSUB _assert:ARGS "DEC(`0%%`)=%%" % (H$(I),D$(I)) 
288 NEXT

300 REM ToDo: INT(), WORD(), and WORD$()

330 REM ToDo: LONG() and LONG$()

360 REM ToDo: FLOAT() and FLOAT$()

400 REM ToDo: STR$() and VAL()
