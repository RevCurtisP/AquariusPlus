100 REM Test RESET array
110 FOR I=0 TO 10:A(I)=11-I:NEXT
120 FOR I=0 TO 10:PRINT A(I);:NEXT:PRINT
130 RESET *A
140 FOR I=0 TO 10:PRINT A(I);:NEXT:PRINT
