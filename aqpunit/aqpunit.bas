
880 _finish: GOSUB _outnewline
882 IF DEEK($38AF)<>$38B1 THEN GOSUB _adderror:ARGS "Error: hanging temp descriptors"
885 _passed: GOSUB _output:ARGS "Passed:"+STR$(QR(1))
886 _failed: GOSUB _output:ARGS "Failed:"+STR$(QR(0))
887 _errors: GOSUB _output:ARGS "Errors:"+STR$(QR(2))
888 GOSUB _outnewline

890 FOR I=0 TO 2:QA=$BFF8+I*2:DOKE QA,DEEK(QA)+QR(I):NEXT
892 QS$=ARGS$(0)+\" - Passed: %%, Failed: %%, Errors: %%\n" % (QR(1), QR(0), QR(2))
894 APPEND "out/_results.txt",^QS$:IF QU<>0 THEN GOSUB _outscreen:ARGS QS$
896 IF QC=0 THEN PRINT "Unit Tests Complete":END
897 IF QM AND QG THEN RUN QG$+".baq"
898 _theend:GOSUB _nlscreen:ARGS "F3=Run Again, F4=Run Next":END
899 _failend:GOSUB _nlscreen:ARGS "Test failed":goto _theend


900 _init:
901 SET FNKEY 3 To "run "+ARGS$(0)+CHR$(13):SET FNKEY 4 TO "run "+QG$+".baq"+CHR$(13)
902 DIM QR(2),QR$(2):QC=LEN(QG$):SET SPEED 3:SCREEN 3:QZ=FRE("")
904 IF FRE(2)=$BFF5 THEN QX=-1:QM=-PEEK($BFF6):QG=PEEK($BFF7):QF=-PEEK($BFFE):QU=PEEK($BFFF):IF QU=0 THEN CLS
906 IF QU=2 THEN QF$=TRIMEXT$(ARGS$(0)):QF$="out/"+QF$+".out":SAVE QF$,^QZ$
908 GOSUB _outscreen:ARGS "Running Tests":QQ$=CHR$(34):QR$(0)="Fail: ":QR$(1)="Pass: ":RETURN

910 _title:GETARGS QT$:GOSUB _outnewline:GOSUB _output:ARGS "Testing "+QT$
912 IF QU<>0 THEN GOTO _outstring
914 RETURN

918 _adderror: QR(2)=QR(2)+1:GETARGS QA$:GOSUB _output:ARGS QA$:GOTO _failcheck
920 _assert_nq:GETARGS QA$:GOTO 922
921 _assert:GETARGS QA$:GOSUB _quotes
922 ON ERROR GOTO 926:QV=-EVAL(QA$):ON ERROR GOTO 0
924 GOSUB _output:ARGS QR$(QV)+QA$:QR(QV)=QR(QV)+1:GOTO _failcheck
926 DOKE $38AF,$38B1:GOSUB _output:ARGS ERR$+" error in "+QA$:RESUME:ON ERROR GOTO 0:QR(2)=QR(2)+1
928 _failcheck: ON -(QF AND QV-1) GOTO _failend:RETURN

940 _assert_err:GETARGS QA$,QE:GOSUB _quotes
942 ON ERROR GOTO _yes_err:QV=EVAL(QA$)
944 _no_err:ON ERROR GOTO 0:GOSUB _output:ARGS "No Error in "+QA$:QR(0)=QR(0)+1:goto _failcheck
946 _yes_err:QV=-(ERR=QE):QR(QV)=QR(QV)+1:GOSUB _output:ARGS QR$(QV)+"Error"+STR$(QE)+" in "+QA$
948 RESUME:ON ERROR GOTO 0:GOTO _failcheck

950 _outnewline:IF QU=0 THEN PRINT:QL=QL+1:RETURN
952 IF QU=1 THEN LPRINT:RETURN
954 QN$=\"\n":APPEND QF$,^QN$:RETURN

956 _outnlquote:GOSUB _outnewline
958 _outquoted:GOSUB _quotes:GOTO _output:ARGS QU$

960 _outpart: GETARGS QU$:ON QU GOTO _outprinter,_outfile
962 IF QL>20 THEN GOSUB _pause
964 PRINT QU$;:QL=QL+1:RETURN
966 _outprinter:LPRINT QU$;:RETURN

970 _quotes:FOR QI=1 TO LEN(QA$):QC$=QA$[QI]:IF QC$="`" THEN QA$[QI]=QQ$
972 IF QQ THEN PRINT QI;" ";QC$;" ";QA$
974 NEXT:QQ=0:RETURN

976 _nlscreen:QU$="":GOSUB _outstring
977 _outscreen:GETARGS QU$:GOTO _outstring
978 _nloutput:GOSUB _outnewline
980 _output:GETARGS QU$:ON QU GOTO _outprinter,_outfile
982 _outstring:IF QL>20 THEN GOSUB _pause
984 PRINT QU$:QL=QL+1:RETURN
986 _outprinter:LPRINT QU$:RETURN
988 _outfile:QN$=\"%%\n" % (QU$):APPEND QF$,^QN$:RETURN

992 _pause:IF QU OR QK='c' THEN RETURN
994 PRINT:PRINT "...Press a key...":QL=0:QK=GETKEY:PRINT:RETURN

996 _getkey:IF QU=0 AND QK<>'c' THEN QK=GETKEY
998 RETURN
