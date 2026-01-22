100 REM Delete Directory and all it's Contents
105 SET FNKEY 3 TO "RUN "+ARGS$(0)+CHR$(13)
110 CLEAR 8000
120 DIM DI(9):'Directory index
125 DIM DE(9):'Directory entry count
130 DIM DN$(9):'Directory name
135 DIM DC$(99):'Directory contents
140 DIM DS$(9,99):'Directory stack

200 INPUT "Directory to delete";DD$
205 IF DD$="" THEN END
210 PRINT "Type YES to delete ";DD$:INPUT Y$
215 IF Y$<>"YES" THEN END

300 _main:ON ERROR GOTO _error
310 ED$="getting current directory"
320 OD$=CD$:'Original directory
330 DN$(DL)=DD$
340 GOSUB 400
390 END

400 _read_dir:
410 ED$="opening directory "+DN$(DL)
415 LOAD DIR DN$(DL),*DC$,BIN:DE(DL)=VAL(DC$(0))
420 IF DE(DL) THEN GOSUB _proc_dir
430 ED$="deleting directory"+DN$(DL)
440 DEL DN$(DL)
490 RETURN

500 _proc_dir:
510 FOR I=0 TO DE(DL):DS$(DL,I)=DC$(I):NEXT
520 DI(DL)=1
530 _proc_loop:DE$=DS$(DL,DI(DL))
540 FS$=DN$(DL)+"/"+MID$(DE$,10):FT=ASC(DE$[5]) AND 1
550 GOSUB _proc_file
580 DI(DL)=DI(DL)+1:IF DI(DL)<=DE(DL) THEN GOTO _proc_loop
590 RETURN

600 _proc_file:
610 PRINT FS$
620 IF FT=0 THEN DEL FS$:RETURN
630 DL=DL+1:DN$(DL)=FS$:GOSUB _read_dir:DL=DL-1
690 RETURN

900 _error:
910 PRINT ERR$;" error ";ED$
915 RESUME _abort
920 _abort:ON ERROR GOTO 0