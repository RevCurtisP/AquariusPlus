100 REM Variables and Literals
120 IF FRE(2)<>$BEFF THEN CLEAR 4096
130 GOSUB _init

200 GOSUB _title:ARGS "Variables and Literals"

300 GOSUB _title:ARGS "Extended Variable Names (tilde)"
305 GOSUB _output:ARGS LIST$(NEXT)
306 A~BCD=1:AB~CD=2:A~BCD$="a":AB~CD$="ab":A~BCD(3)=3:AB~CD$(4)="d"

310 GOSUB _assert:ARGS "A=1"
311 GOSUB _assert:ARGS "A~B=1"
312 GOSUB _assert:ARGS "A~BC=1"
313 GOSUB _assert:ARGS "A~BCD=1"
314 GOSUB _assert:ARGS "A~BCDE=1"
315 GOSUB _assert:ARGS "A~X=1"
316 GOSUB _assert:ARGS "A~XY=1"
317 GOSUB _assert:ARGS "A~XYZ=1"

321 GOSUB _assert:ARGS "AB=2"
322 GOSUB _assert:ARGS "AB~C=2"
323 GOSUB _assert:ARGS "AB~CD=2"
324 GOSUB _assert:ARGS "AB~CDE=2"
325 GOSUB _assert:ARGS "AB~X=2"
326 GOSUB _assert:ARGS "AB~XY=2"
327 GOSUB _assert:ARGS "AB~XYZ=2"

330 GOSUB _assert:ARGS "A$=`a`"
331 GOSUB _assert:ARGS "A~B$=`a`"
332 GOSUB _assert:ARGS "A~BC$=`a`"
333 GOSUB _assert:ARGS "A~BCD$=`a`"
334 GOSUB _assert:ARGS "A~BCDE$=`a`"
335 GOSUB _assert:ARGS "A~X$=`a`"
336 GOSUB _assert:ARGS "A~XY$=`a`"
337 GOSUB _assert:ARGS "A~XYZ$=`a`"

341 GOSUB _assert:ARGS "AB$=`ab`"
342 GOSUB _assert:ARGS "AB~C$=`ab`"
343 GOSUB _assert:ARGS "AB~CD$=`ab`"
344 GOSUB _assert:ARGS "AB~CDE$=`ab`"
345 GOSUB _assert:ARGS "AB~X$=`ab`"
346 GOSUB _assert:ARGS "AB~XY$=`ab`"
347 GOSUB _assert:ARGS "AB~XYZ$=`ab`"

350 GOSUB _assert:ARGS "A(3)=3"
351 GOSUB _assert:ARGS "A~B(3)=3"
352 GOSUB _assert:ARGS "A~BC(3)=3"
353 GOSUB _assert:ARGS "A~BCD(3)=3"
354 GOSUB _assert:ARGS "A~BCDE(3)=3"
355 GOSUB _assert:ARGS "A~X(3)=3"
356 GOSUB _assert:ARGS "A~XY(3)=3"
357 GOSUB _assert:ARGS "A~XYZ(3)=3"

361 GOSUB _assert:ARGS "AB$(4)=`d`"
362 GOSUB _assert:ARGS "AB~C$(4)=`d`"
363 GOSUB _assert:ARGS "AB~CD$(4)=`d`"
364 GOSUB _assert:ARGS "AB~CDE$(4)=`d`"
365 GOSUB _assert:ARGS "AB~X$(4)=`d`"
366 GOSUB _assert:ARGS "AB~XY$(4)=`d`"
367 GOSUB _assert:ARGS "AB~XYZ$(4)=`d`"

400 GOSUB _title:ARGS "Character Literals"
410 GOSUB _assert_nq:ARGS "' '=32"
411 GOSUB _assert_nq:ARGS "'1'=49"
412 GOSUB _assert_nq:ARGS "'@'=64"
413 GOSUB _assert_nq:ARGS "'A'=65"
414 GOSUB _assert_nq:ARGS "'Z'=90"
415 GOSUB _assert_nq:ARGS "'['=91"
416 GOSUB _assert_nq:ARGS "'`'=96"
417 GOSUB _assert_nq:ARGS "'a'=97"
418 GOSUB _assert_nq:ARGS "'z'=122"
419 GOSUB _assert_nq:ARGS "'{'=123"

500 GOSUB _title:ARGS "Escaped Strings"
501 GOSUB _output:ARGS LIST$(NEXT)
502 E$=\"\"\a\b\f\n\r\t\v\"\\":rem Valid escape sequences
504 GOSUB _assert:ARGS "E$=$`2207080C0D0A0D090B225C`"

511 GOSUB _output:ARGS LIST$(NEXT)
512 H$=\"\x00\x0d\x1A\x34\x41\x4e\x5B\x68\x82\x8F\xA9\xb6\xC3\xd0\xdd\xEF\xf7"
514 GOSUB _assert:ARGS "H$=$`000D1A34414E5B68828FA9B6C3D0DDEFF7`"

520 GOSUB _pause
521 GOSUB _output:ARGS LIST$(NEXT):SCREEN 1
522 E1$=\"\vClear Screen\nNewline":PRINT E1$:P1$=PEEKSCREEN$(40,80)
523 SCREEN 3:GOSUB _output:ARGS LIST$(NEXT):SCREEN 1
524 E2$=\"*****\rReturn\tTab":PRINT E2$:P2$=PEEKSCREEN$(120,40)
525 SCREEN 3:GOSUB _output:ARGS LIST$(NEXT):SCREEN 1
526 E3$=\"Backspace*\b":PRINT E3$:P3$=PEEKSCREEN$(160,40)
527 SCREEN 3:GOSUB _output:ARGS LIST$(NEXT):SCREEN 1
528 E4$=\"\aBell\fForm Feed":PRINT E4$:P4$=PEEKSCREEN$(200,40)
529 GOSUB _pause:SCREEN 3

530 GOSUB _output:ARGS "Verifying temporary string buffers deallocated"
532 GOSUB _assert:ARGS "DEEK($3836)=DEEK($38FE)"

540 GOSUB _outnewline
542 GOSUB _assert:ARGS "P1$[1 TO 40]=` Clear Screen`+STRING$(27)"
543 GOSUB _assert:ARGS "P1$[41 TO 80]=` Newline`+STRING$(32)"
544 GOSUB _assert:ARGS "P2$=` Return`+$`09`+`Tab`+STRING$(29)"
546 GOSUB _assert:ARGS "P3$=` Backspace`+STRING$(30)"
548 GOSUB _assert:ARGS "P4$=` Bell`+$`0C`+`Form Feed`+STRING$(25)"

550 GOSUB _outnewline
552 GOSUB _assert:ARGS "E1$=$`0B`+`Clear Screen`+$`0D0A`+`Newline`" 
554 GOSUB _assert:ARGS "E2$=`*****`+$`0D`+`Return`+$`09`+`Tab`"
556 GOSUB _assert:ARGS "E3$=`Backspace*`+$`08`"
558 GOSUB _assert:ARGS "E4$=$`07`+`Bell`+$`0C`+`Form Feed`"

560 GOSUB _pause
562 GOSUB _title:ARGS "% String Substitution"

571 GOSUB _output:ARGS LIST$(NEXT)
572 PRINT \"ARGS$(0)=\"%%\"" % (ARGS$(0))
573 SCREEN 3:GOSUB _output:ARGS LIST$(NEXT):SCREEN 1
574 PRINT \"CD$=\"%%\"" % (CD$)
575 SCREEN 3:GOSUB _output:ARGS LIST$(NEXT):SCREEN 1
576 PRINT \"BIN$(12345)=\"%%\"" % (BIN$(12345))
577 SCREEN 3

580 GOSUB _output:ARGS "Verifying temporary string buffers deallocated"
582 GOSUB _assert:ARGS "DEEK($3836)=DEEK($38FE)"
584 GOSUB _output:ARGS "Verifying temporary strings \deallocated"
586 GOSUB _assert:ARGS "DEEK($38AF)=$38B1"
