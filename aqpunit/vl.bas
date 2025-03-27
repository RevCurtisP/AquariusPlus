100 REM Variables and Literals
130 GOSUB _init

190 goto 400

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
