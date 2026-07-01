100 REM Screen Fill
110 QG$="sg"
130 GOSUB _init

180 SC=40: 'Screen width

200 GOSUB _title:ARGS "FILL SCREEN"

210 REPEAT
212 GOSUB _title:ARGS "%%-column mode" % (SC)
214 GOSUB _init_screen

300 GOSUB _title:ARGS "FILL SCREEN (x,y)-(x,y) CHR char"

310 GOSUB _output:ARGS LIST$(NEXT):GOSUB _work_screen
312 FILL SCREEN (1,0)-(7,5) CHR "*.."
314 GOSUB _peekscreen

320 GOSUB _assert:ARGS "SP$(0)=`         `"
321 GOSUB _assert:ARGS "SP$(1)=` ******* `"
322 GOSUB _assert:ARGS "SP$(2)=` ******* `"
323 GOSUB _assert:ARGS "SP$(3)=` ******* `"
324 GOSUB _assert:ARGS "SP$(4)=` ******* `"
325 GOSUB _assert:ARGS "SP$(5)=` ******* `"
326 GOSUB _assert:ARGS "SP$(6)=` ******* `"
327 GOSUB _assert:ARGS "SP$(7)=`         `"
329 GOSUB _outnewline

330 GOSUB _assert:ARGS "CP$(0)=$`060606060606060606`"
331 GOSUB _assert:ARGS "CP$(1)=$`060606060606060606`"
332 GOSUB _assert:ARGS "CP$(2)=$`060606060606060606`"
333 GOSUB _assert:ARGS "CP$(3)=$`060606060606060606`"
334 GOSUB _assert:ARGS "CP$(4)=$`060606060606060606`"
335 GOSUB _assert:ARGS "CP$(5)=$`060606060606060606`"
336 GOSUB _assert:ARGS "CP$(6)=$`060606060606060606`"
337 GOSUB _assert:ARGS "CP$(7)=$`060606060606060606`"
339 GOSUB _outnewline

340 GOSUB _output:ARGS LIST$(NEXT):GOSUB _work_screen
342 FILL SCREEN (0,-1)-(6,4) CHR 64
344 GOSUB _peekscreen

350 GOSUB _assert:ARGS "SP$(0)=`@@@@@@@  `"
351 GOSUB _assert:ARGS "SP$(1)=`@@@@@@@* `"
352 GOSUB _assert:ARGS "SP$(2)=`@@@@@@@* `"
353 GOSUB _assert:ARGS "SP$(3)=`@@@@@@@* `"
354 GOSUB _assert:ARGS "SP$(4)=`@@@@@@@* `"
355 GOSUB _assert:ARGS "SP$(5)=`@@@@@@@* `"
356 GOSUB _assert:ARGS "SP$(6)=` ******* `"
357 GOSUB _assert:ARGS "SP$(7)=`         `"
359 GOSUB _outnewline

360 GOSUB _assert:ARGS "CP$(0)=$`060606060606060606`"
361 GOSUB _assert:ARGS "CP$(1)=$`060606060606060606`"
362 GOSUB _assert:ARGS "CP$(2)=$`060606060606060606`"
363 GOSUB _assert:ARGS "CP$(3)=$`060606060606060606`"
364 GOSUB _assert:ARGS "CP$(4)=$`060606060606060606`"
365 GOSUB _assert:ARGS "CP$(5)=$`060606060606060606`"
366 GOSUB _assert:ARGS "CP$(6)=$`060606060606060606`"
367 GOSUB _assert:ARGS "CP$(7)=$`060606060606060606`"
369 GOSUB _outnewline

400 GOSUB _title:ARGS "FILL SCREEN (x,y)-(x,y) COLOR fg,bg"

410 GOSUB _output:ARGS LIST$(NEXT):GOSUB _work_screen
412 FILL SCREEN (1,0)-(7,5) COLOR 7,0
414 GOSUB _peekscreen

420 GOSUB _assert:ARGS "SP$(0)=`@@@@@@@  `"
421 GOSUB _assert:ARGS "SP$(1)=`@@@@@@@* `"
422 GOSUB _assert:ARGS "SP$(2)=`@@@@@@@* `"
423 GOSUB _assert:ARGS "SP$(3)=`@@@@@@@* `"
424 GOSUB _assert:ARGS "SP$(4)=`@@@@@@@* `"
425 GOSUB _assert:ARGS "SP$(5)=`@@@@@@@* `"
426 GOSUB _assert:ARGS "SP$(6)=` ******* `"
427 GOSUB _assert:ARGS "SP$(7)=`         `"
429 GOSUB _outnewline

430 GOSUB _assert:ARGS "CP$(0)=$`060606060606060606`"
431 GOSUB _assert:ARGS "CP$(1)=$`067070707070707006`"
432 GOSUB _assert:ARGS "CP$(2)=$`067070707070707006`"
433 GOSUB _assert:ARGS "CP$(3)=$`067070707070707006`"
434 GOSUB _assert:ARGS "CP$(4)=$`067070707070707006`"
435 GOSUB _assert:ARGS "CP$(5)=$`067070707070707006`"
436 GOSUB _assert:ARGS "CP$(6)=$`067070707070707006`"
437 GOSUB _assert:ARGS "CP$(7)=$`060606060606060606`"
439 GOSUB _outnewline

440 GOSUB _output:ARGS LIST$(NEXT):GOSUB _work_screen
442 FILL SCREEN (0,-1)-(6,4) COLOR 1,8
444 GOSUB _peekscreen

450 GOSUB _assert:ARGS "SP$(0)=`@@@@@@@  `"
451 GOSUB _assert:ARGS "SP$(1)=`@@@@@@@* `"
452 GOSUB _assert:ARGS "SP$(2)=`@@@@@@@* `"
453 GOSUB _assert:ARGS "SP$(3)=`@@@@@@@* `"
454 GOSUB _assert:ARGS "SP$(4)=`@@@@@@@* `"
455 GOSUB _assert:ARGS "SP$(5)=`@@@@@@@* `"
456 GOSUB _assert:ARGS "SP$(6)=` ******* `"
457 GOSUB _assert:ARGS "SP$(7)=`         `"
459 GOSUB _outnewline

460 GOSUB _assert:ARGS "CP$(0)=$`181818181818180606`"
461 GOSUB _assert:ARGS "CP$(1)=$`181818181818187006`"
462 GOSUB _assert:ARGS "CP$(2)=$`181818181818187006`"
463 GOSUB _assert:ARGS "CP$(3)=$`181818181818187006`"
464 GOSUB _assert:ARGS "CP$(4)=$`181818181818187006`"
465 GOSUB _assert:ARGS "CP$(5)=$`181818181818187006`"
466 GOSUB _assert:ARGS "CP$(6)=$`067070707070707006`"
467 GOSUB _assert:ARGS "CP$(7)=$`060606060606060606`"
469 GOSUB _outnewline

500 GOSUB _title:ARGS "FILL SCREEN (x,y)-(x,y) CHR char COLOR fg,bg"

510 GOSUB _output:ARGS LIST$(NEXT):GOSUB _work_screen
512 FILL SCREEN (2,1)-(5,3) CHR '+' COLOR 8,7
514 GOSUB _peekscreen

520 GOSUB _assert:ARGS "SP$(0)=`@@@@@@@  `"
521 GOSUB _assert:ARGS "SP$(1)=`@@@@@@@* `"
522 GOSUB _assert:ARGS "SP$(2)=`@@++++@* `"
523 GOSUB _assert:ARGS "SP$(3)=`@@++++@* `"
524 GOSUB _assert:ARGS "SP$(4)=`@@++++@* `"
525 GOSUB _assert:ARGS "SP$(5)=`@@@@@@@* `"
526 GOSUB _assert:ARGS "SP$(6)=` ******* `"
527 GOSUB _assert:ARGS "SP$(7)=`         `"
529 GOSUB _outnewline

530 GOSUB _assert:ARGS "CP$(0)=$`181818181818180606`"
531 GOSUB _assert:ARGS "CP$(1)=$`181818181818187006`"
532 GOSUB _assert:ARGS "CP$(2)=$`181887878787187006`"
533 GOSUB _assert:ARGS "CP$(3)=$`181887878787187006`"
534 GOSUB _assert:ARGS "CP$(4)=$`181887878787187006`"
535 GOSUB _assert:ARGS "CP$(5)=$`181818181818187006`"
536 GOSUB _assert:ARGS "CP$(6)=$`067070707070707006`"
537 GOSUB _assert:ARGS "CP$(7)=$`060606060606060606`"
539 GOSUB _outnewline

540 GOSUB _output:ARGS LIST$(NEXT):GOSUB _work_screen
542 FILL SCREEN (1,0)-(4,2) CHR '#' COLOR 2,15
544 GOSUB _peekscreen

550 GOSUB _assert:ARGS "SP$(0)=`@@@@@@@  `"
551 GOSUB _assert:ARGS "SP$(1)=`@####@@* `"
552 GOSUB _assert:ARGS "SP$(2)=`@####+@* `"
553 GOSUB _assert:ARGS "SP$(3)=`@####+@* `"
554 GOSUB _assert:ARGS "SP$(4)=`@@++++@* `"
555 GOSUB _assert:ARGS "SP$(5)=`@@@@@@@* `"
556 GOSUB _assert:ARGS "SP$(6)=` ******* `"
557 GOSUB _assert:ARGS "SP$(7)=`         `"
559 GOSUB _outnewline

560 GOSUB _assert:ARGS "CP$(0)=$`181818181818180606`"
561 GOSUB _assert:ARGS "CP$(1)=$`182F2F2F2F18187006`"
562 GOSUB _assert:ARGS "CP$(2)=$`182F2F2F2F87187006`"
563 GOSUB _assert:ARGS "CP$(3)=$`182F2F2F2F87187006`"   
564 GOSUB _assert:ARGS "CP$(4)=$`181887878787187006`"
565 GOSUB _assert:ARGS "CP$(5)=$`181818181818187006`"
566 GOSUB _assert:ARGS "CP$(6)=$`067070707070707006`"
567 GOSUB _assert:ARGS "CP$(7)=$`060606060606060606`"
569 GOSUB _outnewline

700 IF SC=40 THEN SC=80:LOOP
702 END REPEAT          
704 GOTO _finish

710 _init_screen:
712 SET COLOR OFF
714 IF SC=40 THEN SCREEN 2,,,,1:GOSUB _set_screen:SCREEN 3:RETURN
716 STASH SCREEN:SCREEN 3,,,,1:GOSUB _set_screen:SWAP SCREEN:RETURN
718 _set_screen:SET CURSOR OFF:SET COLOR OFF:CLS:RETURN

720 _work_screen:
724 IF SC=40 THEN SCREEN 2:RETURN
726 SWAP SCREEN:RETURN

730 _main_screen:
732 IF SC=40 THEN SCREEN 3:RETURN
734 SWAP SCREEN:RETURN

750 _peekscreen:
751 'Must be in work screen on entry, exits in main screen
752 FOR I=0 TO 7:S=I*SC
753 L$(I)="I=%%:S=%%:"%(I,S)+LIST$(NEXT)
754 SP$(I)=PEEKSCREEN$(S,9):CP$(I)=PEEKCOLOR$(S,9)
755 NEXT:GOSUB _getkey:GOSUB _main_screen:GOSUB _outnewline
756 FOR I=0 TO 5:GOSUB _output:ARGS L$(I):NEXT
758 GOSUB _outnewline:RETURN
