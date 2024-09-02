100 REM Test [] substrings
110 QG$="sj"
120 GOSUB _init

200 GOSUB _title:ARGS "Eval string$[]"
210 GOSUB _output:ARGS LIST$(NEXT)
212 S$="abcd"
214 FOR I=1 TO 4:S$(I)=MID$(S$,I,1):NEXT

220 GOSUB _assert_err:ARGS "S$[0]",30
221 FOR I=1 TO 4
222 GOSUB _assert:ARGS "S$[%%]=`%%`" % (I, S$(I))
224 NEXT
226 GOSUB _assert:ARGS "S$[5]=``"
228 Z=FRE(""):GOSUB _outnewline
 
230 FOR F=0 TO 5:FOR T=0 TO 5:IF QU THEN PRINT ".";
232 IF F=0 OR T=0 OR T<F THEN GOSUB _assert_err:ARGS "S$[%% TO %%]" % (F,T),30:GOTO 236
233 M$=MID$(S$,F,T-F+1)
234 GOSUB _assert:ARGS "S$[%% TO %%]=`%%`" % (F,T,M$)
236 NEXT:Z=FRE(""):GOSUB _outnewline
238 NEXT:IF QU THEN PRINT:QL=QL+1

300 GOSUB _title:ARGS "Assign to string$[]"
310 B$="ab":B=LEN(B$)+1
312 U$=\"\"%%\""
314 A$="S$="+U$+":"
316 C$="S$[%%]="+U$
318 D$="S$[%% TO %%]="+U$

320 FOR F=0 TO B:FOR T=0 TO B:FOR L=1 TO B:IF QU THEN PRINT ".";
322 L$=STRING$(L,"*")
324 P$=A$ % (S$):R$=C$ % (F,L$):T$=D$ % (T,F,L$)
326 E=(F=0 OR F=B OR T<F)
328 IF F<>T THEN GOTO _range

330 IF E THEN QA$=R$:QE=30:ON ERROR GOTO _ss_err
332 S$=B$:Z$=B$:GOSUB _output:ARGS P$+R$
334 S$[F]=L$:IF E THEN GOSUB _no_err:GOTO _next
336 MID$(Z$,F,1)=L$:M$=U$ % (Z$)
338 GOSUB _assert:ARGS "S$=%%" % (M$)

340 _range:IF E THEN QA$=T$:QE=30:ON ERROR GOTO _ss_err
342 S$=B$:Z$=S$:GOSUB _output:ARGS P$+T$
344 S$[F TO T]=L$:IF E THEN GOSUB _no_err:GOTO _next
346 MID$(Z$,F,T-F+1)=L$:M$=U$ % (Z$)
348 GOSUB _assert:ARGS "S$=%%" % (M$)

350 GOTO _next
352 _ss_err:GOSUB _yes_err
360 _next
362 NEXT:GOSUB _outnewline
364 NEXT:GOSUB _outnewline
366 NEXT:IF QU THEN PRINT:QL=QL+1

