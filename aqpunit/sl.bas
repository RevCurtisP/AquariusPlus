100 REM Test Save and Load
110 QU=0:REM 0=Screen,1=Printer
120 SET FAST ON
130 SCREEN 3:GOSUB _init
133 SET FNKEY 3 TO \"RUN /au/sl.baq\r"
134 SET FNKEY 4 TO \"goto _dump\r"

190 REM Tests

200 GOSUB _title:ARGS "SAVE and LOAD"

210 GOSUB _title:ARGS "SAVE/LOAD array"
212 GOSUB _output:ARGS LIST$(NEXT)
214 DIM A(9),B(9),C(9)
216 GOSUB _output:ARGS "Filling A()"
218 FOR I=0 TO 9:A(I)=I*I:NEXT
220 GOSUB _outquoted:ARGS LIST$(NEXT)
222 SAVE "ssa.caq",*A
224 GOSUB _outquoted:ARGS LIST$(NEXT)
226 RENAME "ssa.caq" TO "sla.caq"
230 GOSUB _outquoted:ARGS LIST$(NEXT)
232 LOAD "sla.caq",*B
234 GOSUB _assert:ARGS "COMPARE(*B,*A)"
236 GOSUB _assert:ARGS "NOT COMPARE(*B,*C)"
238 DEL "sla.caq"

240 GOSUB _title:ARGS "SAVE/LOAD binary"
242 COPY $C000,2000 TO $8000
244 GOSUB _outquoted:ARGS LIST$(NEXT)
246 SAVE "ssb.bin",$100,1000
248 GOSUB _outquoted:ARGS LIST$(NEXT)
250 RENAME "ssb.bin" TO "slb.bin"
252 GOSUB _outquoted:ARGS LIST$(NEXT)
254 LOAD "slb.bin",$8000
256 GOSUB _assert:ARGS "COMPARE($100,$8000,1000)"
258 DEL "slb.bin"

300 GOSUB _title:ARGS "SAVE/LOAD paged"
302 COPY $C100,1000 TO @40,100
304 GOSUB _outquoted:ARGS LIST$(NEXT)
306 SAVE "ssp.bpg",@40,$100,1000
308 GOSUB _outquoted:ARGS LIST$(NEXT)
310 RENAME "ssp.bpg" TO "slp.bpg"
312 GOSUB _outquoted:ARGS LIST$(NEXT)
314 LOAD "slp.bpg",@41,200
316 GOSUB _assert:ARGS "COMPARE(@40,$100,@41,200,1000)"
318 DEL "slp.bpg"

320 GOSUB _title:ARGS "SAVE/LOAD page"
322 COPY @1 TO @42
324 GOSUB _outquoted:ARGS LIST$(NEXT)
326 SAVE "sps.bpg",@40
328 GOSUB _outquoted:ARGS LIST$(NEXT)
330 RENAME "sps.bpg" TO "spg.bpg"
332 GOSUB _outquoted:ARGS LIST$(NEXT)
334 LOAD "spg.bpg",@50
336 GOSUB _assert:ARGS "COMPARE(@42,@63)"
338 DEL "spg.bpg"


350 GOSUB _title:ARGS "SAVE/LOAD string array"
352 GOSUB _output:ARGS LIST$(NEXT)
354 DIM A$(9),B$(9),C$(9)
356 GOSUB _output:ARGS "Filling A()"
360 A$(0)="Lorem ipsum dolor sit amet, consectetur adipiscing elit, "
361 A$(1)="sed do eiusmod tempor incididunt ut labore et dolore magna "
362 A$(2)="aliqua. Ut enim ad minim veniam, quis nostrud exercitation "
363 A$(3)="ullamco laboris nisi ut aliquip ex ea commodo consequat. "
364 A$(4)="Duis aute irure dolor in reprehenderit in voluptate velit " 
365 A$(5)="esse cillum dolore eu fugiat nulla pariatur. Excepteur sint "
366 A$(6)="occaecat cupidatat non proident, sunt in culpa qui officia "
367 A$(7)=""
368 A$(8)="deserunt mollit anim id est laborum."
370 GOSUB _outquoted:ARGS LIST$(NEXT)
372 SAVE "ssa.sta",*A$
374 GOSUB _outquoted:ARGS LIST$(NEXT)
376 RENAME "ssa.sta" TO "sla.sta"
380 GOSUB _output:ARGS LIST$(NEXT)
382 FOR I=0 TO 9:B$(I)=STR$(I):NEXT
384 GOSUB _outquoted:ARGS LIST$(NEXT)
386 LOAD "sla.sta",*B$
390 FOR I=0 TO 9
392 GOSUB _assert:ARGS "B$(%%)=A$(%%)" % (I,I)
394 NEXT
398 DEL "sla.sta"
