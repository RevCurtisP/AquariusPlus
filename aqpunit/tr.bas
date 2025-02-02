100 REM Test TRIM and PAD functions

110 QG$="ts"
120 GOSUB _init

190 goto 500

200 GOSUB _title:ARGS "String Trim Functions"
201 PRINT LIST$(NEXT)
202 C$="@":S$="<=>":D$="@@@123@@@":D$(0)="@@@@@@":D$(1)="<=>X=Y<=>"

210 GOSUB _title:ARGS "TRIM$"
211 GOSUB _assert:ARGS "TRIM$(`   123   `)=`123`)"
212 GOSUB _assert:ARGS "TRIM$(\` \t1 2 3   \r\n`)=`1 2 3`"
213 GOSUB _assert:ARGS "TRIM$(``)=``"
214 GOSUB _assert:ARGS "TRIM$(`      `)=``"
215 GOSUB _assert:ARGS "TRIM$(D$,`@`)=`123`"
216 GOSUB _assert:ARGS "TRIM$(D$(0),C$)=``"
217 GOSUB _assert:ARGS "TRIM$(`#*#123#*#`,`*#`)=`123`"
218 GOSUB _assert:ARGS "TRIM$(D$(1),S$)=`X=Y`)"
 
230 GOSUB _title:ARGS "TRIML$"
231 GOSUB _assert:ARGS "TRIML$(`   123   `)=`123   `"
232 GOSUB _assert:ARGS "TRIML$(\` \t1 2 3   \r\n`)=\`1 2 3   \r\n``"
233 GOSUB _assert:ARGS "TRIM$(``)=``"
234 GOSUB _assert:ARGS "TRIML$(`      `)=``"
235 GOSUB _assert:ARGS "TRIML$(D$,`@`)=`123@@@`"
236 GOSUB _assert:ARGS "TRIML$(D$(0),C$)=``"
237 GOSUB _assert:ARGS "TRIML$(`*#*#123#*#`,`#*`)=`123#*#`"
238 GOSUB _assert:ARGS "TRIML$(D$(1),S$)=`X=Y<=>`)"

250 GOSUB _title:ARGS "TRIMR$"
251 GOSUB _assert:ARGS "TRIMR$(`   123   `)=`   123`"
252 GOSUB _assert:ARGS "TRIMR$(\`1 2 3   \r\n`)=\`1 2 3`"
253 GOSUB _assert:ARGS "TRIM$(``)=``"
254 GOSUB _assert:ARGS "TRIMR$(`      `)=``"
255 GOSUB _assert:ARGS "TRIMR$(D$,C$)=`@@@123`"
256 GOSUB _assert:ARGS "TRIMR$(D$(0),`@`)=``"
257 GOSUB _assert:ARGS "TRIMR$(`#*#123*#*`,`#*`)=`#*#123`"
258 GOSUB _assert:ARGS "TRIMR$(D$(1),S$)=`<=>X=Y`)"

300 GOSUB _title:ARGS "Filespec Trim and Extract Functions"
301 PRINT LIST$(NEXT)
302 F$="no_ext":F1$="/no_ext":F2 $="a/no_ext":F$(0)="a/b/file.tmp"

310 GOSUB _title:ARGS "FILEEXT$"
311 GOSUB _assert:ARGS "FILEEXT$(`file.ext`)=`ext`"
312 GOSUB _assert:ARGS "FILEEXT$(F$)=``"
313 GOSUB _assert:ARGS "FILEEXT$(F$(0))=`tmp`"

330 GOSUB _title:ARGS "FILEDIR$"
331 GOSUB _assert: ARGS "FILEDIR$(`file.ext`)=``"
332 GOSUB _assert: ARGS "FILEDIR$(F1$)=`/`"
333 GOSUB _assert: ARGS "FILEDIR$(F$(0))=`a/b/`"
334 GOSUB _assert: ARGS "FILEDIR$(`a/`)=`a/`"
335 GOSUB _assert: ARGS "FILEDIR$(`/a/b/`)=`/a/b/`"

350 GOSUB _title:ARGS "TRIMDIR$"
353 GOSUB _assert: ARGS "TRIMDIR$(`file.ext`)=`file.ext`"
354 GOSUB _assert: ARGS "TRIMDIR$(F2$)=`no_ext`"
355 GOSUB _assert: ARGS "TRIMDIR$(F$(0))=`file.tmp`"
356 GOSUB _assert: ARGS "TRIMDIR$(``)=``"
357 GOSUB _assert: ARGS "TRIMDIR$(`a/`)=``"
358 GOSUB _assert: ARGS "TRIMDIR$(`/a/b/`)=``"

370 GOSUB _title:ARGS "TRIMEXT$"
373 GOSUB _assert: ARGS "TRIMEXT$(`file.ext`)=`file`"
374 GOSUB _assert: ARGS "TRIMEXT$(F$)=`no_ext`"
375 GOSUB _assert: ARGS "TRIMEXT$(F$(0))=`a/b/file`"
376 GOSUB _aBVssert: ARGS "TRIMEXT$(``)=``"


500 GOSUB _title:ARGS("PAD$")
501 GOSUB _assert:ARGS "PAD$(`xyz`,9)=`xyz      `"
502 GOSUB _assert:ARGS "PAD$(`xyz`,-10)=`       xyz`"
503 GOSUB _assert:ARGS "PAD$(`xyz`,1)=`x`"
504 GOSUB _assert:ARGS "PAD$(`xyz`,-2)=`yz`"
505 GOSUB _assert:ARGS "PAD$(`xyz`,0)=``"
506 GOSUB _assert:ARGS "PAD$(`xyz`,9,`!`)=`xyz!!!!!!`"
507 GOSUB _assert:ARGS "PAD$(`xyz`,-10,`@`)=`@@@@@@@xyz`"
508 GOSUB _assert:ARGS "PAD$(`xyz`,5,0)=`xyz`+$`0000`"
509 GOSUB _assert:ARGS "PAD$(`xyz`,-6,0)=$`000000`+`xyz`"

510 GOSUB _title:ARGS("PAD$")
511 GOSUB _output:ARGS LIST$(NEXT)
512 P$="xyz"

521 GOSUB _assert:ARGS "PAD$(P$,9)=`xyz      `"
522 GOSUB _assert:ARGS "PAD$(P$,-10)=`       xyz`"
523 GOSUB _assert:ARGS "PAD$(P$,1)=`x`"
524 GOSUB _assert:ARGS "PAD$(P$,-2)=`yz`"
525 GOSUB _assert:ARGS "PAD$(P$,0)=``"
526 GOSUB _assert:ARGS "PAD$(P$,9,`!`)=`xyz!!!!!!`"
527 GOSUB _assert:ARGS "PAD$(P$,-10,`@`)=`@@@@@@@xyz`"
528 GOSUB _assert:ARGS "PAD$(P$,5,0)=`xyz`+$`0000`"
529 GOSUB _assert:ARGS "PAD$(P$,-6,0)=$`000000`+`xyz`"


540 GOSUB _title:ARGS("Multiple PAD$")
541 P1$="1":P2$="22":P3$="333":P4$="4444"
542 PD$="`1   22333444`"
543 T1$=PAD$(P1$,3)
544 T2$=PAD$(P2$,-3)
545 T3$=PAD$(P3$,3)
546 T4$=PAD$(P4$,3)
547 TT$=T1$+T2$+T3$+T4$
548 TS$="T1$+T2$+T3$+T4$"
549 GOSUB _assert:ARGS TS$+"="+PD$