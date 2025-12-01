100 REM DIM and READ
110 QG$="pp"
130 GOSUB _init

200 GOSUB _title:ARGS "DIM Arrays"
202 GOSUB _output:ARGS LIST$(NEXT)
204 DIM R(2),T$(3)
206 GOSUB _assert:ARGS "VARPTR(*R)>=DEEK($38D8)"
208 GOSUB _assert:ARGS "VARPTR(*T$)>=DEEK($38D8)"

220 GOSUB _title:ARGS "DIM Numeric Array Equals"
222 GOSUB _output:ARGS LIST$(NEXT)
224 DIM N(3)=3,6,9,12
226 FOR I=0 TO 3:GOSUB _assert:ARGS "N(%%)=%%" % (I,(I+1)*3)
228 NEXT

230 GOSUB _title:ARGS "DIM String Array Equals"
232 GOSUB _output:ARGS LIST$(NEXT)
234 DIM R$(2)="FOO","BAR","BAZ"
236 FOR I=0 TO 2:Z$=MID$("FOOBARBAZ",I*3+1,3)
238 GOSUB _assert:ARGS "R$(%%)=`%%`" % (I,Z$):NEXT

300 GOSUB _title:ARGS "DIM Simple Variables"
302 GOSUB _output:ARGS LIST$(NEXT)
304 DIM A,S$
306 GOSUB _assert:ARGS "VARPTR(A)<DEEK($38D8)"
308 GOSUB _assert:ARGS "VARPTR(S$)<DEEK($38D8)"

320 'GOSUB _title:ARGS "DIM Simple Variables Errors"
322 'GOSUB _output:ARGS LIST$(NEXT):QE=27:ON ERROR GOTO _yes_err
324 'DIM A=1

400 GOSUB _title:ARGS "READ Simple Variables"
401 GOSUB _output:ARGS LIST$(NEXT)
402 READ D,D$,E,E$:DATA 1,cat,$FF,"Dog"
404 GOSUB _assert:ARGS "D=1"
405 GOSUB _assert:ARGS "D$=`cat`"
406 GOSUB _assert:ARGS "E=255"
407 GOSUB _assert:ARGS "E$=`Dog`"

420 GOSUB _title:ARGS "READ Numeric Array"
421 GOSUB _output:ARGS LIST$(NEXT)
422 DIM F(3):READ *F:DATA $FE,255
423 GOSUB _output:ARGS LIST$(NEXT)
424 DATA $100,257
426 FOR I=0 TO 3:GOSUB _assert:ARGS "F(%%)=%%" % (I,I+254)
428 NEXT

430 GOSUB _title:ARGS "READ String Array"
431 GOSUB _output:ARGS LIST$(NEXT)
432 DIM G$(4):READ *G$:DATA "AB",CD
433 GOSUB _output:ARGS LIST$(NEXT)
434 DATA "EF",GH,"IJ
436 FOR I=0 TO 4:Z$=CHR$('A'+I+I)+CHR$('B'+I+I)
437 GOSUB _assert:ARGS "G$(%%)=`%%`" % (I,Z$)
438 GOSUB _assert:ARGS "INDEX(*G$,`%%`)=%%" % (Z$,I)
439 NEXT

