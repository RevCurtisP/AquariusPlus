100 REM Load Alternate ROM

200 DIM R$(20),F$(20),N$(20)
210 D$="/system/sysrom/"
220 LOAD D$+"_index",*R$,ASC
230 R=VAL(R$(0))
240 FOR I=1 TO R
250 IF R$(I)="" OR LEFT$(R$(I),1)="#" THEN GOTO _next
260 N=N+1
270 S$=LEFT$(R$(I),14):T$=TRIM$(S$):F$(N)=D$+T$
280 N$(N)=MID$(R$(I),15)
290 _next:NEXT

300 _display:
305 CLS
310 FOR I=1 TO N:I$=STR$(I)
320 PRINT PAD$(I$,-2);" ";N$(I)
330 NEXT
340 PRINT
350 PRINT \"Select ROM (1-%%)" % (R);

400 _select:
410 S=0:INPUT S
420 IF S=0 THEN END
430 IF S>R THEN PRINT CHR$(7);:GOTO _select
450 F$=F$(S):RUN F$
