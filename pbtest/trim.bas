100 REM Test Extract and Trim functions
110 SET FNKEY 3 TO \"RUN TRIM.BAS\r"

210 N$="file.ext":GOSUB _test
220 N$="no_ext":GOSUB _test
230 N$="/x/":GOSUB _test
240 N$="y/":GOSUB _test
250 N$="/a/no_ext":GOSUB _test
260 N$="a/b/file.tmp":GOSUB _test
290 END

300 _test:
310 F$="FILEDIR$":GOSUB _testfn
320 F$="TRIMDIR$":GOSUB _testfn
330 F$="FILEEXT$":GOSUB _testfn
340 F$="TRIMEXT$":GOSUB _testfn
350 rem F$="FILENAME$":GOSUB _testfn
380 PRINT:PAUSE "Press a key":PRINT
390 RETURN

400 _testfn:
410 Q$=\"%%(\"%%\")"
415 V$=Q$ % (F$,N$)
420 PRINT V$;"=";EVAL(V$)
490 RETURN