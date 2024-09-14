100 REM Test SAVE SCREEN and LOAD SCREEN

110 QG$="sx"
120 GOSUB _init

130 SCREEN 2,,,,1:RESET PALETTE 0:CLS:SCREEN 3

180 IF QG THEN GOTO _tests
182 PRINT:PRINT $"07"+"WARNING: FLASHING SCREENS"
184 PRINT:PRINT "Press Q to quit, any other key to continue"
186 K=GETKEY:IF K='Q' or K='q' THEN END
188 PRINT:IF QU=0 THEN QL=QL+5

190 _tests

200 GOSUB _title:ARGS "LOAD/SAVE SCREEN/COLOR"

210 GOSUB _nloutput:ARGS "40 Column Screen"
212 FILL BYTE @32,0,32768,0
214 GOSUB _nloutput:ARGS LIST$(NEXT)
215 LOAD "assets/rect40.scrt",@32,0
216 GOSUB _output:ARGS LIST$(NEXT)
217 LOAD "assets/rect40.sclr",@32,1024
218 GOSUB _output:ARGS LIST$(NEXT)
219 LOAD "assets/grays.palt",@32,2048

220 GOSUB _nloutput:ARGS LIST$(NEXT):SCREEN 2
221 CLS:LOAD SCREEN "assets/rect40.scrt"
222 GOSUB _getkey:SCREEN 3:GOSUB _output:ARGS LIST$(NEXT):SCREEN 2
223 COPY SCREEN CHR TO @33,0
224 SCREEN 3:GOSUB _assert:ARGS "COMPARE(@32,0,@33,0,1024)"

230 GOSUB _nloutput:ARGS LIST$(NEXT):SCREEN 2
231 LOAD PALETTE 0,"assets/grays.palt"
232 SCREEN 3:GOSUB _output:ARGS LIST$(NEXT):SCREEN 2
233 LOAD SCREEN ATTR "assets/rect40.sclr"
234 GOSUB _getkey:SCREEN 3:GOSUB _output:ARGS LIST$(NEXT):SCREEN 2
235 COPY SCREEN ATTR TO @33,1024
236 SCREEN 3:GOSUB _assert:ARGS "COMPARE(@32,1024,@33,1024,1024)"

240 GOSUB _nloutput:ARGS LIST$(NEXT)
242 LOAD "assets/rect40.scrp",@32,0
243 GOSUB _output:ARGS LIST$(NEXT):SCREEN 2
244 SCREEN ,,,,0:CLS:LOAD SCREEN "assets/rect40.scrp"
245 GOSUB _getkey:SCREEN 3:GOSUB _output:ARGS LIST$(NEXT):SCREEN 2
246 COPY SCREEN CHR TO @33,0:V=IN($E0)
247 SCREEN 3:GOSUB _assert:ARGS "COMPARE(@32,0,@33,0,1024)"
248 GOSUB _assert:ARGS "(V AND 32)=(PEEK(@32,1024) AND 32)"

250 GOSUB _nloutput:ARGS LIST$(NEXT)
252 LOAD "assets/rect40.scrn",@32,0
253 GOSUB _output:ARGS LIST$(NEXT):SCREEN 2
254 CLS:LOAD SCREEN "assets/rect40.scrn"
255 GOSUB _getkey:SCREEN 3:GOSUB _output:ARGS LIST$(NEXT):SCREEN 2
256 COPY SCREEN TO @33,0
258 SCREEN 3:GOSUB _assert:ARGS "COMPARE(@32,0,@33,0,2048)"

260 GOSUB _nloutput:ARGS LIST$(NEXT):SCREEN 2
261 SAVE SCREEN "work/save40.scrn"
263 SCREEN 3:GOSUB _output:ARGS LIST$(NEXT)
264 FILL BYTE @32,0,16384,0:LOAD "work/save40.scrn",@32,0
265 SCREEN 3:GOSUB _output:ARGS LIST$(NEXT):SCREEN 2
266 SCREEN ,,,,0:CLS:LOAD SCREEN "work/save40.scrn"
267 GOSUB _getkey:SCREEN 3:GOSUB _output:ARGS LIST$(NEXT):SCREEN 2
268 COPY SCREEN TO @33,0:P$=GETPALETTE$(0):V=IN($E0)
270 SCREEN 3:GOSUB _assert:ARGS "COMPARE(@32,0,@33,0,2048)"
272 GOSUB _assert:ARGS "P$=PEEK$(@32,2048,32)"
274 GOSUB _assert:ARGS "(V AND 32)=(PEEK(@32,2080) AND 32)"
278 DEL "work/save40.scrn"

290 GOSUB _output:ARGS LIST$(NEXT)
291 SET FILE ERROR OFF:LOAD SCREEN "nofile40.scrn":SET FILE ERROR ON
292 GOSUB _assert:ARGS "ERR=50"

300 SWAP SCREEN:SCREEN ,,,,1:RESET PALETTE 0:CLS:SWAP SCREEN

310 GOSUB _nloutput:ARGS "80 Column Screen"
312 FILL BYTE @32,0,32768,0
314 GOSUB _nloutput:ARGS LIST$(NEXT)
315 LOAD "assets/rect80.scrt",@32,0
316 GOSUB _output:ARGS LIST$(NEXT)
317 LOAD "assets/rect80.sclr",@32,2048
318 GOSUB _output:ARGS LIST$(NEXT)
319 LOAD "assets/grays.palt",@32,4096

320 GOSUB _nloutput:ARGS LIST$(NEXT):SWAP SCREEN
321 LOAD SCREEN "assets/rect80.scrt"
322 GOSUB _getkey:SWAP SCREEN:GOSUB _output:ARGS LIST$(NEXT):SWAP SCREEN
323 COPY SCREEN CHR TO @33,0
324 SWAP SCREEN:GOSUB _assert:ARGS "COMPARE(@32,0,@33,0,2048)"

330 GOSUB _output:ARGS LIST$(NEXT):SWAP SCREEN
331 LOAD PALETTE 0,"assets/grays.palt"
332 SWAP SCREEN:GOSUB _output:ARGS LIST$(NEXT):SWAP SCREEN
333 LOAD SCREEN ATTR "assets/rect80.sclr":GOSUB _getkey
334 SWAP SCREEN:GOSUB _output:ARGS LIST$(NEXT):SWAP SCREEN
335 COPY SCREEN ATTR TO @33,2048
337 SWAP SCREEN:GOSUB _assert:ARGS "COMPARE(@32,2048,@33,2048,1024)"

340 GOSUB _nloutput:ARGS LIST$(NEXT)
341 LOAD "assets/rect80.scrp",@32,0
342 GOSUB _output:ARGS LIST$(NEXT):SWAP SCREEN
344 SCREEN ,,,,0:CLS:LOAD SCREEN "assets/rect80.scrp"
345 GOSUB _getkey:SWAP SCREEN:GOSUB _output:ARGS LIST$(NEXT):SWAP SCREEN
346 COPY SCREEN CHR TO @33,0:V=IN($E0)
347 SWAP SCREEN:GOSUB _assert:ARGS "COMPARE(@32,0,@33,0,2048)"
348 GOSUB _assert:ARGS "(V AND 32)=(PEEK(@32,2048) AND 32)"

350 GOSUB _nloutput:ARGS LIST$(NEXT)
351 LOAD "assets/rect80.scrn",@32,0
352 GOSUB _output:ARGS LIST$(NEXT):SWAP SCREEN
353 CLS:LOAD SCREEN "assets/rect80.scrn"
354 GOSUB _getkey:SWAP SCREEN:GOSUB _output:ARGS LIST$(NEXT):SWAP SCREEN
356 COPY SCREEN TO @33,0
358 SWAP SCREEN:GOSUB _assert:ARGS "COMPARE(@32,0,@33,0,4096)"

360 GOSUB _nloutput:ARGS LIST$(NEXT):SWAP SCREEN
361 SAVE SCREEN "work/save80.scrn"
362 SWAP SCREEN:GOSUB _output:ARGS LIST$(NEXT):SWAP SCREEN
363 FILL BYTE @32,0,16384,0:LOAD "work/save80.scrn",@32,0
364 SWAP SCREEN:GOSUB _output:ARGS LIST$(NEXT):SWAP SCREEN
365 RESET PALETTE 0:SCREEN ,,,,0:CLS:LOAD SCREEN "work/save80.scrn"
366 GOSUB _getkey:SWAP SCREEN:GOSUB _output:ARGS LIST$(NEXT):SWAP SCREEN
368 COPY SCREEN TO @33,0:P$=GETPALETTE$(0):V=IN($E0)
370 SWAP SCREEN:GOSUB _assert:ARGS "COMPARE(@32,0,@33,0,2048)"
372 GOSUB _assert:ARGS "P$=PEEK$(@32,4096,32)"
374 GOSUB _assert:ARGS "(V AND 32)=(PEEK(@32,4128) AND 32)"
378 DEL "work/save80.scrn"

390 GOSUB _output:ARGS LIST$(NEXT):SWAP SCREEN
391 SET FILE ERROR OFF:LOAD SCREEN "nofile80.scrn":SET FILE ERROR ON
392 SWAP SCREEN:GOSUB _assert:ARGS "ERR=50"

400 GOSUB _nloutput:ARGS "Invalid Screen Files"
401 FOR I=1 TO 8:READ S:F$="work/badsize%%.scrn" % (S)
402 SAVE F$,@32,0,S
403 GOSUB _outquoted:ARGS "LOAD SCREEN '%%'" % (F$)
404 SET FILE ERROR OFF:LOAD SCREEN F$:SET FILE ERROR ON
405 GOSUB _assert:ARGS "ERR=49"
406 DEL F$
407 NEXT
408 DATA 1,1023,1025,2047,2050,4095,4098,4130
