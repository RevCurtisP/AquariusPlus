100 REM Test TRIM functions
110 QU=0:REM 0=Screen,1=Printer
120 SCREEN 3:SET FNKEY 3 TO \"run au/tr.baq\r"
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

