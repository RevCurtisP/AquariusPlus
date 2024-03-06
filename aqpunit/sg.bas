100 REM Program Description
110 QU=0:REM 0=Screen,1=Printer
130 GOSUB _init

200 GOSUB _title:ARGS "Screen Graphics 40-column"
202 GOSUB _output:ARGS LIST$(204)
204 SCREEN 1,,,,1


210 GOSUB _output:ARGS LIST$(212)
212 FILL SCREEN CHR '@'
214 I=1:J=2
216 GOSUB _assert:ARGS "I=1"
218 GOSUB _assert:ARGS "J=2"
220 GOSUB _assert_err:ARGS "I=`Hello`",13


