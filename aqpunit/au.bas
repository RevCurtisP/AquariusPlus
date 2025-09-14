100 REM Run unit tests
105 SET SPEED 3
110 CLEAR 4096,$BFF5:POKE $BFF8,STRING$(8,0)
111 '$BFF6 - Chain Test
112 '$BFF7 - Run Continuous
113 '$BFF8 - Total Passed
114 '$BFFA - Total Failed
115 '$BFFC - Total Errors
116 '$BFFE - Stop on Failure
117 '$BFFF - Output Mode

125 SET FNKEY 5 TO "run"+CHR$(13)
126 SET FNKEY 6 TO "run au.baq"+CHR$(13)

220 PRINT "Enter=Screen, 1=Printer, 2=File: ";
222 K$=GETKEY$:IF K$<"1" OR K$>"2" THEN K$="0"
224 PRINT K$:Q=VAL(K$):POKE $BFFF,Q:'Save Output Mode
230 PRINT "Continous Run? (Y/N) ";
232 GOSUB _get_yn:POKE $BFF7,F:'Continous Run
240 PRINT "Stop on Failure? (Y/N) ";
242 GOSUB _get_yn:POKE $BFFE,F:'Save Fail Flag
250 INPUT "Test program (Enter=All)";P$
252 IF P$="" THEN P$="bt":C=1
254 POKE $BFF6,C

300 CD "/au"
310 IF Q=2 THEN SAVE "out/_results.txt",^Z$:'Create empty file
320 RUN P$+".baq"

900 _get_yn:K$=UPR$(GETKEY$)
902 K=INSTR("NY",K$):IF K=0 THEN PRINT CHR$(7);:GOTO _get_yn
904 F=K-1:PRINT K$:RETURN
