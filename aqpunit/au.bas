100 REM Run unit tests
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
132 K=GETKEY:P=0:IF K='Y' OR K='y' THEN F=1
134 PRINT CHR$(K):POKE $BFF7,F:'Continous Run
140 PRINT "Stop on Failure? (Y/N) ";
142 K=GETKEY:F=0:IF K='Y' OR K='y' THEN F=1
144 PRINT CHR$(K):POKE $BFFE,F:'Save Fail Flag
150 I$="cp"
152 INPUT "Start with test? (Enter=Default)";I$
154 P$=I$+".baq"

200 CD "/au"
210 IF Q=2 THEN SAVE "out/_results.txt",^Z$:'Create empty file
220 RUN P$
