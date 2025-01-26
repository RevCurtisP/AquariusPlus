100 REM Program Description
110 QG$="cp"
130 GOSUB _init

200 GOSUB _title:ARGS "Bitwise Functions"

300 GOSUB _title:ARGS "BIT long"
310 N=18:B=1:C=4:T$="BIT(%%,%%)=-1":F$="BIT(%%,%%)=0"
320 FOR I=1 TO 18:'N gets rounded off after this.
330 GOSUB _assert:ARGS F$ % (N,B-1)
332 GOSUB _assert:ARGS T$ % (N,B)
334 GOSUB _assert:ARGS F$ % (N,B+1)
340 GOSUB _assert:ARGS F$ % (N,C-1)
342 GOSUB _assert:ARGS T$ % (N,C)
344 GOSUB _assert:ARGS F$ % (N,C+1)
350 N=N+N:B=B+1:C=C+1
360 NEXT

400 GOSUB _title:ARGS "BIT string"
405 T$=\"BIT(\"%%\",%%)=-1":F$=\"BIT(\"%%\",%%)=0"
410 GOSUB _assert:ARGS F$ % ("B",0)
411 GOSUB _assert:ARGS T$ % ("B",1)
412 GOSUB _assert:ARGS F$ % ("B",2)
414 GOSUB _assert:ARGS F$ % ("B",5)
415 GOSUB _assert:ARGS T$ % ("B",6)
416 GOSUB _assert:ARGS F$ % ("B",7)
420 GOSUB _assert:ARGS F$ % ("ABCDE",8)
421 GOSUB _assert:ARGS T$ % ("ABCDE",9)
422 GOSUB _assert:ARGS F$ % ("ABCDE",10)
423 GOSUB _assert:ARGS F$ % ("ABCDE",15)
424 GOSUB _assert:ARGS T$ % ("ABCDE",16)
425 GOSUB _assert:ARGS T$ % ("ABCDE",17)
426 GOSUB _assert:ARGS F$ % ("ABCDE",18)
430 GOSUB _assert:ARGS F$ % ("ABCDE",37)
431 GOSUB _assert:ARGS T$ % ("ABCDE",38)
432 GOSUB _assert:ARGS F$ % ("ABCDE",39)
440 GOSUB _assert_err:ARGS F$ % ("ABCDE",40),5
