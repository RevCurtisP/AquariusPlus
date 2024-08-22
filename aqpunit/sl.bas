100 REM Test Save and Load
110 GOSUB _init
123 SET FNKEY 3 TO \"RUN sl.baq\r"
124 SET FNKEY 4 TO \"RUN ss.baq\r"

190 REM Tests

200 GOSUB _title:ARGS "SAVE and LOAD"
202 SET FILE ERROR OFF
204 WD$="work":MKDIR wd$
208 SET FILE ERROR ON

210 GOSUB _title:ARGS "SAVE/LOAD binary"
212 COPY $C000,2000 TO $8000
214 GOSUB _outquoted:ARGS LIST$(NEXT)
216 SAVE "work/binsave.bin",$100,1000
218 GOSUB _outquoted:ARGS LIST$(NEXT)
220 RENAME "work/binsave.bin" TO "work/binload.bin"
222 GOSUB _outquoted:ARGS LIST$(NEXT)
224 LOAD "work/binload.bin",$8000
226 GOSUB _assert:ARGS "COMPARE($100,$8000,1000)"
228 DEL "work/binload.bin"

234 GOSUB _outquoted:ARGS LIST$(NEXT)
235 SET FILE ERROR OFF:LOAD "work/nofile.bin",$8000:SET FILE ERROR ON
236 GOSUB _assert:ARGS "ERR=50"

240 GOSUB _title:ARGS "SAVE/LOAD paged"
242 COPY $C100,1000 TO @32,100
244 GOSUB _outquoted:ARGS LIST$(NEXT)
246 SAVE "work/pagedsave.bin",@32,$100,1000
248 GOSUB _outquoted:ARGS LIST$(NEXT)
250 RENAME "work/pagedsave.bin" TO "work/pagedload.bin"
252 GOSUB _outquoted:ARGS LIST$(NEXT)
254 LOAD "work/pagedload.bin",@33,200
256 GOSUB _assert:ARGS "COMPARE(@32,$100,@33,200,1000)"
258 DEL "work/pagedload.bin"

264 GOSUB _outquoted:ARGS LIST$(NEXT)
265 SET FILE ERROR OFF:LOAD "work/nofile.bin",@32,100:SET FILE ERROR ON
266 GOSUB _assert:ARGS "ERR=50"

270 GOSUB _title:ARGS "SAVE/LOAD page"
272 COPY @1 TO @33
274 GOSUB _outquoted:ARGS LIST$(NEXT)
276 SAVE "work/savepage.bin",@33
278 GOSUB _outquoted:ARGS LIST$(NEXT)
280 RENAME "work/savepage.bin" TO "work/loadpage.bin"
282 GOSUB _outquoted:ARGS LIST$(NEXT)
284 LOAD "work/loadpage.bin",@34
286 GOSUB _assert:ARGS "COMPARE(@33,0,@34,0,16384)"
288 DEL "work/loadpage.bin"

294 GOSUB _outquoted:ARGS LIST$(NEXT)
295 SET FILE ERROR OFF:LOAD "work/nofile.bin",@35:SET FILE ERROR ON
296 GOSUB _assert:ARGS "ERR=50"

300 GOSUB _title:ARGS "SAVE/LOAD array"
302 GOSUB _output:ARGS LIST$(NEXT)
304 DIM A(9),B(9),C(9)
306 GOSUB _output:ARGS "Filling A()"
308 FOR I=0 TO 9:A(I)=I*I:NEXT
310 GOSUB _outquoted:ARGS LIST$(NEXT)
312 SAVE "work/arraysave.caq",*A
314 GOSUB _outquoted:ARGS LIST$(NEXT)
316 RENAME "work/arraysave.caq" TO "work/arrayload.caq"
320 GOSUB _outquoted:ARGS LIST$(NEXT)
322 LOAD "work/arrayload.caq",*B
324 GOSUB _assert:ARGS "COMPARE(*B,*A)"
326 GOSUB _assert:ARGS "NOT COMPARE(*B,*C)"
328 DEL "work/arrayload.caq"

334 GOSUB _outquoted:ARGS LIST$(NEXT)
335 SET FILE ERROR OFF:LOAD "work/nofile.caq",*C:SET FILE ERROR ON
336 GOSUB _assert:ARGS "ERR=50"

340 GOSUB _title:ARGS "SAVE/LOAD string array"
342 GOSUB _output:ARGS LIST$(NEXT)
344 DIM A$(9),B$(9)
346 GOSUB _output:ARGS "Filling A()"
350 A$(0)="Lorem ipsum dolor sit amet, consectetur adipiscing elit, "
351 A$(1)="sed do eiusmod tempor incididunt ut labore et dolore magna "
352 A$(2)="aliqua. Ut enim ad minim veniam, quis nostrud exercitation "
353 A$(3)="ullamco laboris nisi ut aliquip ex ea commodo consequat. "
354 A$(4)="Duis aute irure dolor in reprehenderit in voluptate velit " 
355 A$(5)="esse cillum dolore eu fugiat nulla pariatur. Excepteur sint "
356 A$(6)="occaecat cupidatat non proident, sunt in culpa qui officia "
357 A$(7)=""
358 A$(8)="deserunt mollit anim id est laborum."
360 GOSUB _outquoted:ARGS LIST$(NEXT)
362 SAVE "work/arraysave.star",*A$
364 GOSUB _outquoted:ARGS LIST$(NEXT)
366 RENAME "work/arraysave.star" TO "work/arrayload.star"
370 GOSUB _output:ARGS LIST$(NEXT)
372 FOR I=0 TO 9:B$(I)=STR$(I):NEXT
374 GOSUB _outquoted:ARGS LIST$(NEXT)
376 LOAD "work/arrayload.star",*B$
380 FOR I=0 TO 9
382 GOSUB _assert:ARGS "B$(%%)=A$(%%)" % (I,I)
384 NEXT
386 CLEAR *B$:Z=FRE("")
388 DEL "work/arrayload.star"

394 GOSUB _outquoted:ARGS LIST$(NEXT)
395 SET FILE ERROR OFF:LOAD "work/nofile.star",*C:SET FILE ERROR ON
396 GOSUB _assert:ARGS "ERR=50"

400 GOSUB _title:ARGS "SAVE/LOAD string"
402 GOSUB _outquoted:ARGS LIST$(NEXT)
404 A$="SOS (Save my String)":B$=""
406 GOSUB _outquoted:ARGS LIST$(NEXT)
408 SAVE "work/savestring.asc",^A$
412 GOSUB _outquoted:ARGS LIST$(NEXT)
414 LOAD "work/savestring.asc",^B$
416 GOSUB _assert:ARGS "B$=A$"
418 DEL "work/savestring.asc"

424 GOSUB _outquoted:ARGS LIST$(NEXT)
425 SET FILE ERROR OFF:LOAD "work/nofile.asc",^C$:SET FILE ERROR ON
426 GOSUB _assert:ARGS "ERR=50"

430 GOSUB _title:ARGS "LOAD string array ASCII"
432 GOSUB _outquoted:ARGS LIST$(NEXT)
433 A1$="what is with this code?"
434 GOSUB _outquoted:ARGS LIST$(NEXT)
435 A2$="oh my, looks like I wrote it"
436 GOSUB _outquoted:ARGS LIST$(NEXT)
437 A3$="what was I thinking?"
438 GOSUB _outquoted:ARGS LIST$(NEXT)
439 A$=A1$+$"0D0A"+A2$+$"0D0A"+A3$:SAVE "work/savearray.txt",^A$
440 GOSUB _outquoted:ARGS LIST$(NEXT)
441 DIM C$(5):LOAD "work/savearray.txt",*C$,ASC
442 GOSUB _assert:ARGS "VAL(C$(0))=3"
444 FOR I=1 TO 5
445 IF I<4 THEN GOSUB _assert:ARGS "C$(%%)=A%%$" % (I,I)
446 IF I>3 THEN GOSUB _assert:ARGS "C$(%%)="+$"2222" % (I)
447 NEXT
448 CLEAR *C$:Z=FRE("")
449 DEL "work/savearray.txt"

454 GOSUB _outquoted:ARGS LIST$(NEXT)
455 SET FILE ERROR OFF:LOAD "work/nofile.star",*C$,ASC:SET FILE ERROR ON
456 GOSUB _assert:ARGS "ERR=50"

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
