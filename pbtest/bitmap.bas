10 REM Test/Demo Bitmap Statements and Functions
15 SET FNKEY 3 TO "run "+ARGS$(0)+CHR$(13)
30 SCREEN 0,2:CLEAR BITMAP 7,0:
32 FOR I=0 TO 199:PSET (I,I),6,2:NEXT:GOSUB 90
34 PAUSE
36 FOR I=0 TO 199:PRESET (I,I):NEXT:GOSUB 90
38 PAUSE
40 SCREEN 0,3:CLEAR BITMAP
42 FOR I=0 TO 159:PSET (I,I),I AND 15:NEXT:GOSUB 95
44 PAUSE
46 FOR I=0 TO 159:PRESET (I,I):NEXT:GOSUB 95
48 PAUSE

80 END
90 FOR I=0 TO 199 STEP 15:PRINT POINT(I,I);:NEXT:PRINT:RETURN
95 FOR I=0 TO 159 STEP 15:PRINT POINT(I,I);:NEXT:PRINT:RETURN