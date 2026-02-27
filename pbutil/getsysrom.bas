100 REM Download latest plusBASIC System ROM
110 V$="v0.70":IF VER(1)<VER(V$) THEN GOTO _badv
115 S$="sysrom.bin"
120 ON ERROR GOTO _error
130 PRINT "Downloading %%..." % (S$)
150 LOAD "http://aquarius.plus/release/"+S$,@32,0
155 IF PEEK$(@35,$3FF8,6)<>"GfxROM" THEN GOTO _badrom
160 PRINT "Writing /%%..." % (S$)
170 SAVE "/sysrom.bin",@32,0,32768
175 APPEND "/sysrom.bin",@34,0,32768
180 PRINT "Press Shift-Ctrl-Esc to restart Aquarius+"
190 END

200 _error
210 M$="in line %%" % (ERRLINE)
220 IF ERRLINE=120 THEN M$="downloading sysrom.bin"
230 IF ERRLINE=130 THEN M$="writing sysrom.bin"
240 PRINT ERR$+" error "+M$
250 END
300 _badv
315 PRINT "This utility requires plusBASIC %% or higher" % (V$)
318 END
320 _badrom
325 PRINT "Incomplete download of "+S$
328 END
