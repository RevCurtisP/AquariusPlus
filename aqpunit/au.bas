100 REM Run unit tests
105 SET SPEED 3
110 CLEAR 4096,$BFF6:POKE $BFF8,STRING$(8,0)
111 '$BFF7 - Run Continuous
112 '$BFF8 - Total Passed
113 '$BFFA - Total Failed
114 '$BFFC - Total Errors
115 '$BFFE - Stop on Failure
116 '$BFFF - Output Mode
120 PRINT "Enter=Screen, 1=Printer, 2=File: ";
122 K$=GETKEY$:IF K$<"1" OR K$>"2" THEN K$="0"
124 PRINT K$:Q=VAL(K$):POKE $BFFF,Q:'Save Output Mode
130 PRINT "Continous Run? (Y/N) ";
132 GOSUB _get_yn:POKE $BFF7,F:'Continous Run
140 PRINT "Stop on Failure? (Y/N) ";
142 GOSUB _get_yn:POKE $BFFE,F:'Save Fail Flag
150 I$="bt"
152 INPUT "Start with test? (Enter=Default)";I$
154 P$=I$+".baq"

200 CD "/au"
210 IF Q=2 THEN SAVE "out/_results.txt",^Z$:'Create empty file
220 RUN P$

900 _get_yn:K$=UPR$(GETKEY$)
902 K=INSTR("NY",K$):IF K=0 THEN PRINT CHR$(7);:GOTO _get_yn
904 F=K-1:PRINT K$:RETURN
