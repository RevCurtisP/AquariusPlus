100 REM Dublin Julian Date Routines
105 SET FNKEY 3 TO \"RUN /subr/dates.bas\r"
110 INPUT Y,M,D:GOSUB _ymd2djd:ARGS Y,M,D RETURN J
120 GOSUB _djd2ymd:ARGS J RETURN Y,M,D
130 PRINT J;Y;M;D

8999 END
9100 _ymd2djd:GET ARGS ZY,ZM,ZD
9102 ZA=(14-ZM)/12:ZX=ZY-ZA:ZN=ZM+12*ZA-3
9104 ZJ=ZD+INT((153*ZN+2)/5)+ZX*365+INT(ZX/4)-INT(ZX/100)+INT(ZX/400)
9106 RETURN INT(ZJ-693901)

9110 _djd2ymd:GET ARGS ZJ
9112 ZK=JZ-1:ZC=INT((4*ZK+3)/1461):ZE=ZK-INT((1461*ZC)/4):ZN=INT((5*ZE+2)/153)
9114 ZD=ZE-INT((153*ZN+2)/5)+1:ZM=ZN+3-INT(12*(ZN/10)):ZY=ZC+INT(ZN/10)
9116 RETURN ZY,ZM,ZD
