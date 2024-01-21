
880 _finish
882 rem SCREEN RESET
884 _passed: GOSUB _output:ARGS "Passed:"+STR$(QR(1))
886 _failed: GOSUB _output:ARGS "Failed:"+STR$(QR(0))
888 END

890 _dump
892 LOAD "/au/dump.bin",$B000
894 CALL $B000
896 END

900 _init:IF QU=0 THEN CLS
902 DIM QR(1),QR$(1)
904 GOSUB _output:ARGS "Running Tests":QR$(0)="Fail: ":QR$(1)="Pass: ":RETURN

910 _ptitle:GETARGS QT$:IF QL THEN GOSUB _pause
911 GOTO _xtitle
912 _stitle:GETARGS QT$
913 _xtitle:GOSUB _qtitle
914 _outskip:QU$="":GOTO _outret

916 _title:GETARGS QT$
917 _qtitle:GOSUB _output:ARGS "":GOSUB _output:ARGS "Testing "+QT$:RETURN

920 _assert:GETARGS QA$:GOSUB _quotes:ARGS QA$ RETURN QA$
922 ON ERROR GOTO 926:QV=-EVAL(QA$):ON ERROR GOTO 0
924 GOSUB _output:ARGS QR$(QV)+QA$:QR(QV)=QR(QV)+1:RETURN
926 GOSUB _output:ARGS ERR$+" error in "+QA$:RESUME:ON ERROR GOTO 0:RETURN

940 _assert_err:GETARGS QA$,QE:GOSUB _quotes:ARGS QA$ RETURN QA$
942 ON ERROR GOTO _yes_err:QV=EVAL(QA$)
944 _no_err:ON ERROR GOTO 0:GOSUB _output:ARGS "No Error in "+QA$:QR(0)=QR(0)+1:RETURN
946 _yes_err:QV=-(ERR=QE):QR(QV)=QR(QV)+1:GOSUB _output:ARGS QR$(QV)+"Error"+STR$(QE)+" in "+QA$
948 RESUME:ON ERROR GOTO 0:RETURN

960 _outquoted:GOSUB _quotes:ARGS QA$ RETURN QU$:GOTO _output:ARGS QU$

970 _quotes:QX$="":GETARGS QS$
972 FOR QI=1 TO LEN(QS$):QC$=MID$(QA$,QI,1):IF QC$="`" THEN QC$=CHR$(34)
976 QX$=QX$+QC$:NEXT:RETURN QX$

980 _output: GETARGS QU$:IF QU GOTO _lprint
982 _outret: IF QL>20 THEN GOSUB _pause
984 PRINT QU$:QL=QL+1:RETURN
986 _lprint:LPRINT QU$:RETURN

990 _pause:IF QU OR QK='c' THEN RETURN
992 rem QS=IN($E0):OUT $E0,1
994 PRINT:PRINT "...Press a key...":QL=0:QK=GETKEY:PRINT
996 RETURN:OUT $E0,QS:

