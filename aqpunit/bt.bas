100 REM Bitwise Statements and Functions
110 QG$="cp"
130 GOSUB _init

200 GOSUB _title:ARGS "Bitwise Functions"

300 GOSUB _title:ARGS "BIT long"
305 N=18:B=1:C=4:T$="BIT(%%,%%)=-1":F$="BIT(%%,%%)=0"
310 FOR I=1 TO 18:'N gets rounded off after this.
311 GOSUB _assert:ARGS F$ % (N,B-1)
312 GOSUB _assert:ARGS T$ % (N,B)
313 GOSUB _assert:ARGS F$ % (N,B+1)
314 GOSUB _assert:ARGS F$ % (N,C-1)
315 GOSUB _assert:ARGS T$ % (N,C)
316 GOSUB _assert:ARGS F$ % (N,C+1)
318 N=N+N:B=B+1:C=C+1
319 NEXT

330 GOSUB _title:ARGS "BIT string"
335 T$=\"BIT(\"%%\",%%)=-1":F$=\"BIT(\"%%\",%%)=0"
340 GOSUB _assert:ARGS F$ % ("B",0)
341 GOSUB _assert:ARGS T$ % ("B",1)
342 GOSUB _assert:ARGS F$ % ("B",2)
344 GOSUB _assert:ARGS F$ % ("B",5)
345 GOSUB _assert:ARGS T$ % ("B",6)
346 GOSUB _assert:ARGS F$ % ("B",7)
350 GOSUB _assert:ARGS F$ % ("ABCDE",8)
351 GOSUB _assert:ARGS T$ % ("ABCDE",9)
352 GOSUB _assert:ARGS F$ % ("ABCDE",10)
353 GOSUB _assert:ARGS F$ % ("ABCDE",15)
354 GOSUB _assert:ARGS T$ % ("ABCDE",16)
355 GOSUB _assert:ARGS T$ % ("ABCDE",17)
356 GOSUB _assert:ARGS F$ % ("ABCDE",18)
360 GOSUB _assert:ARGS F$ % ("ABCDE",37)
361 GOSUB _assert:ARGS T$ % ("ABCDE",38)
362 GOSUB _assert:ARGS F$ % ("ABCDE",39)
363 GOSUB _assert_err:ARGS F$ % ("ABCDE",40),5
