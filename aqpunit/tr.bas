100 REM Test TRIM and PAD functions
110 QU=0:REM 0=Screen,1=Printer
120 SCREEN 3:SET FNKEY 3 TO \"run /au/tr.baq\r"
130 GOSUB _init

210 GOSUB _title:ARGS "TRIM$"
211 GOSUB _assert:ARGS "TRIM$(`   123   `)=`123`)"
212 GOSUB _assert:ARGS "TRIM$(\` \t1 2 3   \r\n`)=`1 2 3`"
213 GOSUB _assert:ARGS "TRIM$(`      `)=``"
214 GOSUB _assert:ARGS "TRIM$(`@@@123@@@`,`@`)=`123`"
215 GOSUB _assert:ARGS "TRIM$(`@@@@@@`,`@`)=``"
216 GOSUB _assert:ARGS "TRIM$(`#*#123#*#`,`*#`)=`123`"
 
230 GOSUB _title:ARGS "TRIML$"
231 GOSUB _assert:ARGS "TRIML$(`   123   `)=`123   `"
232 GOSUB _assert:ARGS "TRIML$(\` \t1 2 3   \r\n`)=\`1 2 3   \r\n``"
233 GOSUB _assert:ARGS "TRIML$(`      `)=``"
234 GOSUB _assert:ARGS "TRIML$(`@@@123@@@`,`@`)=`123@@@`"
235 GOSUB _assert:ARGS "TRIML$(`@@@@@@`,`@`)=``"
236 GOSUB _assert:ARGS "TRIML$(`*#*#123#*#`,`#*`)=`123#*#`"

250 GOSUB _title:ARGS "TRIMR$"
251 GOSUB _assert:ARGS "TRIMR$(`   123   `)=`   123`"
252 GOSUB _assert:ARGS "TRIMR$(\`1 2 3   \r\n`)=\`1 2 3`"
253 GOSUB _assert:ARGS "TRIMR$(`      `)=``"
254 GOSUB _assert:ARGS "TRIMR$(`@@@123@@@`,`@`)=`@@@123`"
255 GOSUB _assert:ARGS "TRIMR$(`@@@@@@`,`@`)=``"
256 GOSUB _assert:ARGS "TRIMR$(`#*#123*#*`,`#*`)=`#*#123`"

310 GOSUB _title:ARGS "FILEEXT$"
311 GOSUB _assert:ARGS "FILEEXT$(`file.ext`)=`ext`"
312 GOSUB _assert:ARGS "FILEEXT$(`no_ext`)=``"
313 GOSUB _assert:ARGS "FILEEXT$(`a/b/file.tmp`)=`tmp`"

330 GOSUB _title:ARGS "FILEDIR$"
331 GOSUB _assert: ARGS "FILEDIR$(`file.ext`)=``"
332 GOSUB _assert: ARGS "FILEDIR$(`/no_ext`)=`/`"
333 GOSUB _assert: ARGS "FILEDIR$(`/a/b/file.tmp`)=`/a/b/`"
334 GOSUB _assert: ARGS "FILEDIR$(`a/`)=`a/`"
335 GOSUB _assert: ARGS "FILEDIR$(`/a/b/`)=`/a/b/`"

350 GOSUB _title:ARGS "TRIMDIR$"
351 GOSUB _assert: ARGS "TRIMDIR$(`file.ext`)=`file.ext`"
352 GOSUB _assert: ARGS "TRIMDIR$(`a/no_ext`)=`no_ext`"
353 GOSUB _assert: ARGS "TRIMDIR$(`/a/b/file.tmp`)=`file.tmp`"
354 GOSUB _assert: ARGS "TRIMDIR$(`a/`)=``"
355 GOSUB _assert: ARGS "TRIMDIR$(`/a/b/`)=``"

370 GOSUB _title:ARGS "TRIMEXT$"
371 GOSUB _assert: ARGS "TRIMEXT$(`file.ext`)=`file`"
372 GOSUB _assert: ARGS "TRIMEXT$(`no_ext`)=`no_ext`"
373 GOSUB _assert: ARGS "TRIMEXT$(`a/b/file.tmp`)=`a/b/file`"

400 GOSUB _title:ARGS("PAD$")
401 GOSUB _assert:ARGS "PAD$(`xyz`,9)=`xyz      `"
402 GOSUB _assert:ARGS "PAD$(`xyz`,-10)=`       xyz`"
403 GOSUB _assert:ARGS "PAD$(`xyz`,1)=`x`"
404 GOSUB _assert:ARGS "PAD$(`xyz`,-2)=`yz`"
405 GOSUB _assert:ARGS "PAD$(`xyz`,0)=``"
406 GOSUB _assert:ARGS "PAD$(`xyz`,9,`!`)=`xyz!!!!!!"
407 GOSUB _assert:ARGS "PAD$(`xyz`,-10,`@`)=`@@@@@@@xyz`"
408 GOSUB _assert:ARGS "PAD$(`xyz`,5,0)=`xyz`+$`0000`"
409 GOSUB _assert:ARGS "PAD$(`xyz`,-6,0)=$`000000`+`xyz`"
