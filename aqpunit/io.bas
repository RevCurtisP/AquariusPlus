100 REM File I/O unit tests
110 QG$="pp": 'Next program to run
130 GOSUB _init

200 GOSUB _title:ARGS "File I/O Tests"

210 GOSUB _title:ARGS "OUTPUT#"
212 GOSUB _output:ARGS LIST$(NEXT)
213 OPEN "work/output.txt" FOR OUTPUT AS F

220 GOSUB _output:ARGS LIST$(NEXT)
221 OUTPUT #F,1;"A";2;"B";3;"C";
222 GOSUB _output:ARGS LIST$(NEXT)
223 OUTPUT #F,4;"D";5;"E";6;"F"
224 GOSUB _output:ARGS LIST$(NEXT)
225 OUTPUT #F,7,"G",8,"H",
226 GOSUB _output:ARGS LIST$(NEXT)
227 OUTPUT #F,9,"I",10,"J"
228 GOSUB _output:ARGS LIST$(NEXT)
229 CLOSE #F

230 GOSUB _output:ARGS LIST$(NEXT)
232 DIM A$(3):LOAD "work/output.txt",*A$,ASC  
234 GOSUB _assert:ARGS "VAL(A$(0))=2"
235 GOSUB _assert:ARGS "A$(1)=`1A2B3C4D5E6F`"
236 GOSUB _assert:ARGS "A$(2)=\`7\tG\t8\tH\t9\tI\t10\tJ`"
237 GOSUB _assert:ARGS "A$(3)=``"
