100 REM Miscelleneous Graphics Functions
110 QG$="pp"
130 GOSUB _init

200 GOSUB _title:ARGS "RGB Functions"
210 FOR R=0 TO 13:G=R+1:B=R+2:
212 S$=RIGHT$(HEX$(R),1)+RIGHT$(HEX$(G),1)+RIGHT$(HEX$(B),1)
214 C=R*256+G*16+B:C$=CHR$(G*16+B)+CHR$(R):H$=HEX$(C$)
216 X$=HEX$(R*16+R)+HEX$(G*16+G)+HEX$(B*16+B)
220 GOSUB _assert:ARGS "RGB(%%,%%,%%)=%%" % (R,G,B,C)
222 GOSUB _assert:ARGS \"RGB$(%%,%%,%%)=$\"%%\"" % (R,G,B,H$)
224 GOSUB _assert:ARGS \"RGB(\"%%\")=%%" % (S$,C)
226 GOSUB _assert:ARGS \"RGB$(\"%%\")=$\"%%\"" % (S$,H$)
228 GOSUB _assert:ARGS \"RGB$(\"%%\")=$\"%%\"" % (X$,H$)
230 RR=R*16+R:GG=G*16+G:BB=B*16+B
232 HH$=HEX$(RR)+HEX$(GG)+HEX$(BB)
234 R$="$`"+HEX$(RGB$(R,G,B))+"`"
240 GOSUB _assert:ARGS "RGBDEC$(%%)=`%%,%%,%%`" % (R$,RR,GG,BB)
242 GOSUB _assert:ARGS "RGBDEC$(%%,92)=`%%\%%\%%`" % (R$,RR,GG,BB)
244 GOSUB _assert:ARGS "RGBDEC$(%%,'|')=`%%|%%|%%`" % (R$,RR,GG,BB)
246 GOSUB _assert:ARGS "RGBDEC$(%%,` `)=`%% %% %%`" % (R$,RR,GG,BB)
250 GOSUB _assert:ARGS "RGBHEX$(%%)=`%%`" % (R$,HH$)
252 GOSUB _assert:ARGS "RGBHEX$(%%,92)=`\%%`" % (R$,HH$)
254 GOSUB _assert:ARGS "RGBHEX$(%%,'|')=`|%%`" % (R$,HH$)
256 GOSUB _assert:ARGS "RGBHEX$(%%,`#`)=`#%%`" % (R$,HH$)
290 NEXT
