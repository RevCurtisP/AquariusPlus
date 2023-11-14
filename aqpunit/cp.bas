100 REM Unit Tests for COPY
110 QU=0:REM 0=Screen,1=Printer
130 GOSUB _init

190 REM Tests

200 GOSUB _title:ARGS "COPY"
202 PRINT "Build binary data";
204 A=$8000:FOR I=1 TO 8:FOR J=i TO 260 STEP 7:B=I+J
206 IF B<256 THEN POKE A,B:A=A+1
208 NEXT:PRINT ".";:NEXT:PRINT

220 GOSUB _title:ARGS "Memory/Memory 256 boundary"
222 GOSUB _output:ARGS "COPY $8000,256 TO $9000"
224 COPY $8000,256 TO $9000
226 GOSUB _assert:ARGS "COMPARE($8000,$9000,256)"

230 GOSUB _title:ARGS "Memory/Memory Offset"
232 GOSUB _output:ARGS "COPY $8005,240 TO $9007"
234 COPY   $8005,240 TO $9007
236 GOSUB _assert:ARGS "COMPARE($8005,$9007,240)"

240 GOSUB _title:ARGS "Overlapping Copy"
242 GOSUB _output:ARGS "COPY $9007,240 TO $9005"
244 COPY $9007,240 TO $9005
246 GOSUB _assert:ARGS "COMPARE($8005,$9005,240)"

252 GOSUB _output:ARGS "COPY $9005,240 TO $9007"
254 COPY $9005,240 TO $9007
256 GOSUB _assert:ARGS "COMPARE($8005,$9007,240)"

300 GOSUB _title:ARGS "Memory/Page 256 boundary"
302 GOSUB _output:ARGS "COPY $8000,256 TO @40,0"
304 COPY $8000,256 TO @40,0
306 GOSUB _assert:ARGS "COMPARE($8000,@40,0,256)"

312 GOSUB _output:ARGS "COPY @40,0,256 TO $9100"
314 COPY @40,0,256 TO $9100
316 GOSUB _assert:ARGS "COMPARE(@40,0,$9100,256)"

322 GOSUB _output:ARGS "COPY @40,0,256 TO @50,0"
324 COPY @40,0,256 TO @50,0
326 GOSUB _assert:ARGS "COMPARE(@40,0,@50,0,256)"

330 GOSUB _title:ARGS "Memory/Page Offset"
332 GOSUB _output:ARGS "COPY $8005,256 TO @40,7"
334 COPY $8005,256 TO @40,7
336 GOSUB _assert:ARGS "COMPARE($8005,@40,7,256)"

342 GOSUB _output:ARGS "COPY @40,7,256 TO $9105"
344 COPY @40,7,256 TO $9105
346 GOSUB _assert:ARGS "COMPARE(@40,7,$9105,256)"

352 GOSUB _output:ARGS "COPY @40,7,256 TO @50,5"
354 COPY @40,7,256 TO @50,5
356 GOSUB _assert:ARGS "COMPARE(@40,7,@50,5,256)"

360 GOSUB _title:ARGS "Memory/Page Rollover"
362 GOSUB _output:ARGS "COPY $8000,256 TO @40,$3F80"
364 COPY $8000,256 TO @40,$3F80
366 GOSUB _assert:ARGS "COMPARE($8000,@40,$3F80,256)"

372 GOSUB _output:ARGS "COPY @40,$3F80,256 TO $9200"
374 COPY @40,$3F80,256 TO $9200
376 GOSUB _assert:ARGS "COMPARE(@40,$3F80,$9200,256)"

382 GOSUB _output:ARGS "COPY @40,$3F80,256 TO @50,$3F00"
384 COPY @40,$3F80,256 TO @50,$3F00
386 GOSUB _assert:ARGS "COMPARE(@40,$3F80,@50,$3F00,256)"

400 GOSUB _title:ARGS "Page to Page"
402 GOSUB _output:ARGS "COPY @55 TO @60"
404 COPY @55 TO @60
406 GOSUB _assert:ARGS "COMPARE(@55,0,@60,0,16384)"


498 GOSUB _title:ARGS "Invalid Page Errors"

500 GOSUB _copytp_err:ARGS $8000,256, 0,0,5
502 GOSUB _copytp_err:ARGS $8000,256,19,0,5
504 GOSUB _copytp_err:ARGS $8000,256,22,0,5
506 GOSUB _copytp_err:ARGS $8000,256,31,0,5
508 GOSUB _copytp_err:ARGS $8000,256,64,0,5

518 GOSUB _title:ARGS "Rollover Overflow Errors"
520 GOSUB _copytp_err:ARGS $8000,256,20,$3FFE,6
522 GOSUB _copytp_err:ARGS $8000,256,21,$3FFE,6
524 GOSUB _copytp_err:ARGS $8000,256,63,$3FFE,6

590 GOTO 598
592 _copytp_err:GETARGS PS,PL,PQ,PD,QE:QL=QL+1:QA$="COPY "+STR$(PS)+","+STR$(PL)+" TO @"+STR$(PQ)+","+STR$(PD):ON ERROR GOTO _yes_err:COPY PS,PL TO @PQ,PD:GOTO _no_err
598 REM

599 GOTO 890



