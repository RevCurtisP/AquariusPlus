100 REM Test SPLIT and JOIN statements

110 QG$="sl"
120 GOSUB _init

200 GOSUB _title:ARGS "SPLIT and JOIN":GOSUB _outnewline
202 DIM A$(5)
204 NF$=\"S$=\"%%\":D=%%:C=%%:"
206 PF$=\"S$=\"%%\":D$=\"%%\":C=%%:":'SPLIT listing prefix
209 GOTO _start

210 _out_array:P$="A$()="
212 FOR I=0 TO 5:P$=P$+"[%%]" % (A$(I)):NEXT
214 GOSUB _output: ARGS P$:RETURN 

220 _test_s:
221 GOSUB _outpart:ARGS PF$ % (S$,D$,C):GOSUB _output:ARGS LIST$(NEXT)
222 SPLIT S$ INTO *A$ DEL D$
223 GOSUB _out_array:GOSUB _assert:ARGS "VAL(A$(0))=%%" % (C)
224 PRINT LIST$(NEXT)
225 JOIN *A$ INTO J$ DEL D$
226 GOSUB _output:ARGS \"J$=\"%%\"" % (J$) 
228 T$=TRIMR$(S$,"|"):GOSUB _assert: ARGS "`%%`=`%%`" % (S$,T$)
229 GOSUB _outnewline:RETURN

230 _test_n:
231 GOSUB _outpart:ARGS NF$%(S$,D,C):GOSUB _output:ARGS LIST$(NEXT)
232 SPLIT S$ INTO *A$ DEL D
233 GOSUB _out_array:GOSUB _assert:ARGS "VAL(A$(0))=%%" % (C)
234 PRINT LIST$(NEXT)
235 JOIN *A$ INTO J$ DEL D
236 GOSUB _output:ARGS \"S$=\"%%\":D$=\"%%\":C=%%" % (J$,D$,C) 
238 T$=TRIMR$(S$,"|"):GOSUB _assert: ARGS "`%%`=`%%`" % (S$,T$)
239 GOSUB _outnewline:RETURN

300 _start
310 C=0:S$="":D$="|":GOSUB _test_s
320 C=3:S$="Foo|Bar|Baz":D=124:GOSUB _test_n
325 C=3:S$="Foo|Bar|Baz":D$="|":GOSUB _test_s
330 C=1:S$="Doh":D$="|":GOSUB _test_s
335 C=0:S$="":D$=":":GOSUB _test_s
340 C=5:S$=\"A\tB\tC\tD\tE":D=9:GOSUB _test_n
345 C=1:S$="A,B|C:D":D=0:GOSUB _test_n

