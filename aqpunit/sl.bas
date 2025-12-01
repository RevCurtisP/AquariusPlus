100 REM Test Save and Load

110 QG$="sp"
120 GOSUB _init

200 GOSUB _title:ARGS "SAVE, LOAD, and APPEND"
202 SET FILE ERROR OFF
204 WD$="work":MKDIR wd$
208 SET FILE ERROR ON

210 GOSUB _title:ARGS "SAVE/LOAD/APPEND binary"
212 COPY $C000,2000 TO $8000
214 GOSUB _outquoted:ARGS LIST$(NEXT)
216 SAVE "work/binsave.bin",$100,1000
218 GOSUB _outquoted:ARGS LIST$(NEXT)
220 RENAME "work/binsave.bin" TO "work/binload.bin"
222 GOSUB _outquoted:ARGS LIST$(NEXT)
224 LOAD "work/binload.bin",$8000
226 GOSUB _assert:ARGS "COMPARE($100,$8000,1000)"
230 GOSUB _outquoted:ARGS LIST$(NEXT)
232 APPEND "work/binload.bin",1256,1000
234 GOSUB _outquoted:ARGS LIST$(NEXT)
238 COPY $C000,2000 TO $8000
240 RENAME "work/binload.bin" TO "work/binappend.bin"
242 GOSUB _outquoted:ARGS LIST$(NEXT)
244 LOAD "work/binappend.bin",$8000
246 GOSUB _assert:ARGS "COMPARE($100,$8000,2000)"
248 DEL "work/binappend.bin"

262 GOSUB _outquoted:ARGS LIST$(NEXT)
264 SET FILE ERROR OFF:LOAD "work/nofile.bin",$8000:SET FILE ERROR ON
266 GOSUB _assert:ARGS "ERR=50"

300 GOSUB _title:ARGS "SAVE/LOAD paged"
302 COPY $C100,2000 TO @32,100:FILL BYTES @33,0,2256,0
304 GOSUB _outquoted:ARGS LIST$(NEXT)
306 SAVE "work/pagedsave.bin",@32,$100,1000
308 GOSUB _outquoted:ARGS LIST$(NEXT)
310 RENAME "work/pagedsave.bin" TO "work/pagedload.bin"
312 GOSUB _outquoted:ARGS LIST$(NEXT)
314 LOAD "work/pagedload.bin",@33,200
316 GOSUB _assert:ARGS "COMPARE(@32,$100,@33,200,1000)"
320 GOSUB _outquoted:ARGS LIST$(NEXT)
322 APPEND "work/pagedload.bin",@32,1256,1000
324 GOSUB _outquoted:ARGS LIST$(NEXT)
326 RENAME "work/pagedload.bin" TO "work/pagedappend.bin"
328 FILL BYTES @33,0,2256,0
330 GOSUB _outquoted:ARGS LIST$(NEXT)
332 LOAD "work/pagedappend.bin",@33,$200
334 GOSUB _assert:ARGS "COMPARE(@32,$100,@33,$200,2000)"
348 DEL "work/pagedappend.bin"

362 GOSUB _outquoted:ARGS LIST$(NEXT)
364 FILL BYTES @39,0,255,0:LOAD "assets/large.bin",@35,0
366 GOSUB _assert:ARGS "PEEK$(@39,0,5)=$`0102030405`"

370 GOSUB _title:ARGS "SAVE/LOAD page"
372 COPY @1 TO @33
374 GOSUB _outquoted:ARGS LIST$(NEXT)
376 SAVE "work/savepage.bin",@33
378 GOSUB _outquoted:ARGS LIST$(NEXT)
380 RENAME "work/savepage.bin" TO "work/loadpage.bin"
382 GOSUB _outquoted:ARGS LIST$(NEXT)
384 LOAD "work/loadpage.bin",@34
386 GOSUB _assert:ARGS "COMPARE(@33,0,@34,0,16384)"
388 DEL "work/loadpage.bin"

394 GOSUB _outquoted:ARGS LIST$(NEXT)
395 SET FILE ERROR OFF:LOAD "work/nofile.bin",@35:SET FILE ERROR ON
396 GOSUB _assert:ARGS "ERR=50"

400 GOSUB _title:ARGS "SAVE/LOAD array"
402 GOSUB _output:ARGS LIST$(NEXT)
404 DIM A(9),B(9),C(9)
406 GOSUB _output:ARGS "Filling A()"
408 FOR I=0 TO 9:A(I)=I*I:NEXT
410 GOSUB _outquoted:ARGS LIST$(NEXT)
412 SAVE "work/arraysave.caq",*A
414 GOSUB _outquoted:ARGS LIST$(NEXT)
416 RENAME "work/arraysave.caq" TO "work/arrayload.caq"
420 GOSUB _outquoted:ARGS LIST$(NEXT)
422 LOAD "work/arrayload.caq",*B
424 GOSUB _assert:ARGS "COMPARE(*B,*A)"
426 GOSUB _assert:ARGS "NOT COMPARE(*B,*C)"
428 DEL "work/arrayload.caq"

434 GOSUB _outquoted:ARGS LIST$(NEXT)
435 SET FILE ERROR OFF:LOAD "work/nofile.caq",*C:SET FILE ERROR ON
436 GOSUB _assert:ARGS "ERR=50"

440 GOSUB _title:ARGS "SAVE/LOAD string array"
442 GOSUB _output:ARGS LIST$(NEXT)
444 DIM A$(9),B$(9)
446 GOSUB _output:ARGS "Filling A()"
450 A$(0)="Lorem ipsum dolor sit amet, consectetur adipiscing elit, "
451 A$(1)="sed do eiusmod tempor incididunt ut labore et dolore magna "
452 A$(2)="aliqua. Ut enim ad minim veniam, quis nostrud exercitation "
453 A$(3)="ullamco laboris nisi ut aliquip ex ea commodo consequat. "
454 A$(4)="Duis aute irure dolor in reprehenderit in voluptate velit " 
455 A$(5)="esse cillum dolore eu fugiat nulla pariatur. Excepteur sint "
456 A$(6)="occaecat cupidatat non proident, sunt in culpa qui officia "
457 A$(7)=""
458 A$(8)="deserunt mollit anim id est laborum."
460 GOSUB _outquoted:ARGS LIST$(NEXT)
462 SAVE "work/arraysave.star",*A$
464 GOSUB _outquoted:ARGS LIST$(NEXT)
466 RENAME "work/arraysave.star" TO "work/arrayload.star"
470 GOSUB _output:ARGS LIST$(NEXT)
472 FOR I=0 TO 9:B$(I)=STR$(I):NEXT
474 GOSUB _outquoted:ARGS LIST$(NEXT)
476 LOAD "work/arrayload.star",*B$
480 FOR I=0 TO 9
482 GOSUB _assert:ARGS "B$(%%)=A$(%%)" % (I,I)
484 NEXT
486 CLEAR *B$:Z=FRE("")
488 DEL "work/arrayload.star"

494 GOSUB _outquoted:ARGS LIST$(NEXT)
495 SET FILE ERROR OFF:LOAD "work/nofile.star",*C:SET FILE ERROR ON
496 GOSUB _assert:ARGS "ERR=50"

500 GOSUB _title:ARGS "SAVE/LOAD string"
502 GOSUB _outquoted:ARGS LIST$(NEXT)
504 A$="SOS (Save my String)":B$=""
506 GOSUB _outquoted:ARGS LIST$(NEXT)
508 SAVE "work/savestring.asc",^A$
512 GOSUB _outquoted:ARGS LIST$(NEXT)
514 LOAD "work/savestring.asc",^B$
516 GOSUB _assert:ARGS "B$=A$"
518 DEL "work/savestring.asc"

524 GOSUB _outquoted:ARGS LIST$(NEXT)
525 SET FILE ERROR OFF:LOAD "work/nofile.asc",^C$:SET FILE ERROR ON
526 GOSUB _assert:ARGS "ERR=50"

530 GOSUB _title:ARGS "LOAD string array ASCII"
532 GOSUB _outquoted:ARGS LIST$(NEXT)
533 A1$="what is with this code?"
534 GOSUB _outquoted:ARGS LIST$(NEXT)
535 A2$="oh my, looks like I wrote it"
536 GOSUB _outquoted:ARGS LIST$(NEXT)
537 A3$="what was I thinking?"
538 GOSUB _outquoted:ARGS LIST$(NEXT)
539 A$=A1$+$"0D0A"+A2$+$"0D0A"+A3$:SAVE "work/savearray.txt",^A$
540 GOSUB _outquoted:ARGS LIST$(NEXT)
541 DIM C$(5):LOAD "work/savearray.txt",*C$,ASC
542 GOSUB _assert:ARGS "VAL(C$(0))=3"
544 FOR I=1 TO 5
545 IF I<4 THEN GOSUB _assert:ARGS "C$(%%)=A%%$" % (I,I)
546 IF I>3 THEN GOSUB _assert:ARGS "C$(%%)="+$"2222" % (I)
547 NEXT
548 CLEAR *C$:Z=FRE("")
549 DEL "work/savearray.txt"

554 GOSUB _outquoted:ARGS LIST$(NEXT)
555 SET FILE ERROR OFF:LOAD "work/nofile.star",*C$,ASC:SET FILE ERROR ON
556 GOSUB _assert:ARGS "ERR=50"

600 GOSUB _title:ARGS "LOAD DIR"
602 GOSUB _outquoted:ARGS LIST$(NEXT)
603 DIM D$(9),E$(9):LOAD DIR "*.baq",*D$,BIN:LOAD DIR "*.baq",*E$,ASC
604 FOR I=1 TO VAL(D$(0))
605 GOSUB _output:ARGS E$(I)
606 GOSUB _assert:ARGS "MID$(D$(I),10)=MID$(E$(I),23)"
608 NEXT
609 CLEAR *D$,*E$

610 GOSUB _outquoted:ARGS LIST$(NEXT)
611 SET FILE ERROR OFF:LOAD DIR "nodir",*D$:SET FILE ERROR ON
612 GOSUB _assert:ARGS "ERR=50"
615 GOSUB _outquoted:ARGS LIST$(NEXT)
616 SET FILE ERROR OFF:LOAD DIR "nodir",*E$,ASC:SET FILE ERROR ON
617 GOSUB _assert:ARGS "ERR=50"
