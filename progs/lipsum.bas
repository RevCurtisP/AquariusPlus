100 REM Test LOAD string array to ASC
105 SET FNKEY 3 TO \"run progs/lipsum.bas\r"
110 SCREEN 3:CLS

200 DIM A$(5)

300 LOAD "/lipsum.txt",*A$,ASC
310 L=VAL(A$(0))
320 FOR I=0 TO L
330 PRINT "%%: "%(I);A$(i)
340 NEXT
