100 REM Length restricted string input
110 SET FNKEY 3 TO \"run /subr/input.bas\r"

200 CLS:LOCATE 10,9:PRINT "Date Entry"
205 D$=DATE$
210 GOSUB _indate:ARGS 10,10,D$ RETURN D1$
215 PRINT:PRINT D1$
220 GOSUB _date2ymd:ARGS D1$ RETURN Y,M,D
225 PRINT Y;M;D
230 GOSUB _ymd2date:ARGS Y,M,D RETURN D2$
235 PRINT D2$

59999 END
60000 REM String Input Routine
60001 REM GOSUB _input:ARGS col,row,min,max,val$,var$,prompt$ RETURN var$
60002 _input:GET ARGS Z1,Z2,Z3,Z4,Z5$,Z6$,Z7$:Z9=Z2*40+40+Z1:Z2$=CHR$(8)
60003 Z5=LEN(Z5$):Z6=LEN(Z6$):Z1$=STRING$(38):POKE SCREEN 1,Z1$:POKE SCREEN 1,Z7$
60004 LOCATE Z1,Z2:PRINT Z6$;:FOR Z0=0 TO 1:Z0=0:Z=GETKEY:Z$=CHR$(Z)
60005 IF Z=13 OR Z=9 OR Z=140 THEN ON -(Z6>=Z3) GOTO _zindone:PRINT $"07";:NEXT
60006 IF Z=8 AND Z6 THEN PRINT Z2$;:Z6=Z6-1:NEXT
60007 IF Z<32 OR Z>126 OR Z6>=Z4 OR (Z5 AND INSTR(Z5$,Z$)=0) THEN PRINT $"07";:NEXT
60008 PRINT Z$;:Z6=Z6+1:NEXT
60009 _zindone:POKE SCREEN 1,Z1$:RETURN PEEKSCREEN$(Z9,Z6)

60020 REM Date Input Routine (YYYYMMDD) - Requires _input
60021 REM GOSUB _indate:ARGS col,row,date$ RETURN date$
60022 _indate:GET ARGS ZC,ZR,ZE$:ZV$="0123456789":ZF$="    ":ZY$=LEFT$(ZE$,4):ZM$=MID$(ZE$,5,2):ZD$=MID$(ZE$,7,2)
60023 LOCATE ZC,ZR:PRINT LEFT$(ZY$+ZF$,4);"/";LEFT$(ZM$+ZF$,2);"/";LEFT$(ZD$+ZF$,2)
60024 _zinyr:GOSUB _input:ARGS ZC,ZR,4,4,ZV$,ZY$,"Enter year (YYYY)" RETURN ZY$:ZY=VAL(ZY$)
60025 _zinmo:GOSUB _input:ARGS ZC+5,ZR,2,2,ZV$,ZM$,"Enter month (MM)" RETURN ZM$:IF Z=140 THEN GOTO _zinyr
60026 ZM=VAL(ZM$):IF ZM<1 OR ZM>12 THEN ZM$="":PRINT $"07";:GOTO _zinmo
60027 _zindy:GOSUB _input:ARGS ZC+8,ZR,2,2,ZV$,ZD$,"Enter day (DD)" RETURN ZD$:ZD=VAL(ZD$):IF Z=140 THEN GOTO _zinmo
60028 IF ZD<1 OR ZD>30+(ZM+7*(ZM>7) AND 1)+(ZM=2)*(2+(ZY MOD 4=0)-(ZY MOD 100=0)+(ZY MOD 400=0)) THEN ZD$="":PRINT $"07";:GOTO _zindy
60029 RETURN ZY$+ZM$+ZD$  

60100 REM Convert Date String (YYYYMMDD) to Year, Month, Day
60101 REM GOSUB _date2ymd:ARGS date$ RETURN year,month,day
60102 _date2ymd:GET ARGS ZD$:RETURN VAL(LEFT$(ZD$,4)),VAL(MID$(ZD$,5,2)),VAL(MID$(ZD$,7,2))

60105 REM Convert Year, Month, Day to Date String (YYYYMMDD)
60106 REM GOSUB _ymd2date:ARGS year,month,day RETURN date$
60107 _ymd2date:GET ARGS ZY,ZM,ZD:RETURN RIGHT$("000%%"%(ZY),4)+RIGHT$("0%%"%(ZM),2)+RIGHT$("0%%"%(ZD),2)
