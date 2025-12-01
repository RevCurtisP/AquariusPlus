100 REM Bitwise Statements and Functions
110 QG$="cp"
130 GOSUB _init

200 GOSUB _title:ARGS "Bitwise Functions"

210 GOSUB _title:ARGS "BIT long"
215 N=18:B=1:C=4:T$="BIT(%%,%%)=-1":F$="BIT(%%,%%)=0"
220 FOR I=1 TO 18:'N gets rounded off after this.
221 N$="%"+BIN$(N)
222 GOSUB _assert:ARGS F$ % (N$,B-1)
223 GOSUB _assert:ARGS T$ % (N$,B)
224 GOSUB _assert:ARGS F$ % (N$,B+1)
225 GOSUB _assert:ARGS F$ % (N$,C-1)
226 GOSUB _assert:ARGS T$ % (N$,C)
227 GOSUB _assert:ARGS F$ % (N$,C+1)
230 N=N+N:B=B+1:C=C+1
232 IF QU THEN PRINT ".";
234 NEXT
236 IF QU THEN PRINT

250 GOSUB _title:ARGS "BIT string"
255 T$=\"BIT(\"%%\",%%)=-1":F$=\"BIT(\"%%\",%%)=0"
260 GOSUB _assert:ARGS F$ % ("B",0)
261 GOSUB _assert:ARGS T$ % ("B",1)
262 GOSUB _assert:ARGS F$ % ("B",2)
264 GOSUB _assert:ARGS F$ % ("B",5)
265 GOSUB _assert:ARGS T$ % ("B",6)
266 GOSUB _assert:ARGS F$ % ("B",7)
270 GOSUB _assert:ARGS F$ % ("ABCDE",8)
271 GOSUB _assert:ARGS T$ % ("ABCDE",9)
272 GOSUB _assert:ARGS F$ % ("ABCDE",10)
273 GOSUB _assert:ARGS F$ % ("ABCDE",15)
274 GOSUB _assert:ARGS T$ % ("ABCDE",16)
275 GOSUB _assert:ARGS T$ % ("ABCDE",17)
276 GOSUB _assert:ARGS F$ % ("ABCDE",18)
280 GOSUB _assert:ARGS F$ % ("ABCDE",37)
281 GOSUB _assert:ARGS T$ % ("ABCDE",38)
282 GOSUB _assert:ARGS F$ % ("ABCDE",39)
283 GOSUB _assert_err:ARGS F$ % ("ABCDE",40),5

300 GOSUB _title:ARGS "Bitwise Statements"

310 GOSUB _title:ARGS "SET BIT long"
311 GOSUB _output:ARGS LIST$(NEXT)
312 A=%10111111:SET BIT A,6
313 GOSUB _assert:ARGS "A=%11111111"
315 GOSUB _output:ARGS LIST$(NEXT)
316 A=%11111111:SET BIT A,4
317 GOSUB _assert:ARGS "A=%11111111"
321 GOSUB _output:ARGS LIST$(NEXT)
322 A=0:SET BIT A,22
323 GOSUB _assert:ARGS "A=2^22"
325 GOSUB _output:ARGS LIST$(NEXT)
326 A=7.5:SET BIT A,3
327 GOSUB _assert:ARGS "A=%1111"

350 GOSUB _title:ARGS "RESET BIT long"
351 GOSUB _output:ARGS LIST$(NEXT)
352 A=%10111111:RESET BIT A,7
353 GOSUB _assert:ARGS "A=%111111"
355 GOSUB _output:ARGS LIST$(NEXT)
356 A=%11101111:RESET BIT A,4
357 GOSUB _assert:ARGS "A=%11101111"
361 GOSUB _output:ARGS LIST$(NEXT)
362 A=2^22:RESET BIT A,22
363 GOSUB _assert:ARGS "A=0"
365 GOSUB _output:ARGS LIST$(NEXT)
366 A=7.5:RESET BIT A,0
367 GOSUB _assert:ARGS "A=%110"

