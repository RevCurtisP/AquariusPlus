100 REM Conversion Functions
110 QG$="dr"
130 GOSUB _init

200 GOSUB _title:ARGS "Conversion Functions"

210 GOSUB _title:ARGS "Binary String Functions"

220 GOSUB _title:ARGS "ASC() and CHR$()"
222 FOR I=0 TO 255 STEP 17:C=I+(I=34)-(I=0):C$="`%%`" % (CHR$(C))
223 IF C<32 OR C>126 THEN C$="$`%%`" % (HEX$(C))
224 GOSUB _assert:ARGS "ASC(%%)=%%" % (C$,C)
226 GOSUB _assert:ARGS "CHR$(%%)=%%" % (C,C$)
228 NEXT

240 GOSUB _title:ARGS "ASC(), ASC$() and BYTE()"
242 U$="7F7E7D7C":S$="FFFEFDFC"
244 GOSUB _assert:ARGS "ASC($`%%`)=127" % (U$)
245 GOSUB _assert:ARGS "ASC($`%%`)=255" % (S$)
246 GOSUB _assert:ARGS "BYTE($`%%`)=127" % (U$)
247 GOSUB _assert:ARGS "BYTE($`%%`)=-1" % (S$)

270 GOSUB _title:ARGS "HEX$(), DEC() and DEC$()"
272 GOSUB _assert:ARGS "HEX$(%%)=%%" % (H$,A$)
276 DIM D$(7)="-8388608","-1","0","1","255","43981","1193046","8388607"
278 DIM H$(7)="800000","FFFFFF","00","01","FF","ABCD","123456","7FFFFF"
280 FOR I=0 TO 7
282 GOSUB _assert:ARGS "HEX$(%%)=`%%`" % (D$(I),H$(I)) 
284 GOSUB _assert:ARGS "DEC(`%%`)=%%" % (H$(I),D$(I)) 
286 GOSUB _assert:ARGS "DEC(`0%%`)=%%" % (H$(I),D$(I)) 
288 NEXT

300 GOSUB _title:ARGS "DEF BYTELIST, ASC(), and BYTE()"
301 GOSUB _output:ARGS LIST$(NEXT)
302 DEF BYTELIST B$=0,1,-1,127
303 GOSUB _output:ARGS LIST$(NEXT)
304 APPEND BYTELIST B$=128,-128,-255,255
308 GOSUB _assert:ARGS "B$=$`0001FF7F808001FF`"

310 DIM B(7)=0,1,-1,127,-128,-128,1,-1
311 DIM A(7)=0,1,255,127,128,128,1,255
314 FOR I=0 TO 7:J=I+1:J$=HEX$(B$[J])
315 GOSUB _assert:ARGS("BYTE(B$,%%)=%%") % (J,B(I))
316 GOSUB _assert:ARGS("ASC(B$,%%)=%%") % (J,A(I))
317 GOSUB _assert:ARGS("CHR$(%%)=$`%%`") % (A(I),J$)
318 NEXT

320 GOSUB _title:ARGS "DEF INTLIST, INT(), WORD(), and WORD$()"
321 GOSUB _output:ARGS LIST$(NEXT)
322 DEF INTLIST I$=0,1,-1,32767
323 GOSUB _output:ARGS LIST$(NEXT)
324 APPEND INTLIST I$=32768,-32768,-65535,65535
328 GOSUB _assert:ARGS "I$=$`00000100FFFFFF7F008000800100FFFF`"

330 DIM I(7)=0,1,-1,32767,-32768,-32768,1,-1
331 DIM W(7)=0,1,65535,32767,32768,32768,1,65535
334 FOR I=0 TO 7:J=I*2+1:J$=HEX$(I$[J TO J+1])
335 GOSUB _assert:ARGS("INT(I$,%%)=%%") % (J,I(I))
336 GOSUB _assert:ARGS("WORD(I$,%%)=%%") % (J,W(I))
337 GOSUB _assert:ARGS("WORD$(%%)=$`%%`") % (W(I),J$)
338 NEXT

340 REM ToDo: LONG() and LONG$()

360 REM ToDo: FLOAT() and FLOAT$()

400 REM ToDo: STR$() and VAL()
