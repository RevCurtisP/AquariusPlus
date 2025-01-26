100 REM Test TRIM and PAD functions

110 QG$="ts"
120 GOSUB _init

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
376 GOSUB _assert: ARGS "TRIMEXT$(``)=``"


410 GOSUB _title:ARGS("PAD$")
411 GOSUB _assert:ARGS "PAD$(`xyz`,9)=`xyz      `"
412 GOSUB _assert:ARGS "PAD$(`xyz`,-10)=`       xyz`"
413 GOSUB _assert:ARGS "PAD$(`xyz`,1)=`x`"
414 GOSUB _assert:ARGS "PAD$(`xyz`,-2)=`yz`"
415 GOSUB _assert:ARGS "PAD$(`xyz`,0)=``"
416 GOSUB _assert:ARGS "PAD$(`xyz`,9,`!`)=`xyz!!!!!!"
417 GOSUB _assert:ARGS "PAD$(`xyz`,-10,`@`)=`@@@@@@@xyz`"
418 GOSUB _assert:ARGS "PAD$(`xyz`,5,0)=`xyz`+$`0000`"
419 GOSUB _assert:ARGS "PAD$(`xyz`,-6,0)=$`000000`+`xyz`"
