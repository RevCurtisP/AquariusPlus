100 REM Test Tiles Sprite BASIC Errors
110 GOSUB _init
123 SET FNKEY 3 TO \"RUN tx.baq\r"
130 QC=1:'Last Unit Test Program

252 GOSUB _pseterr:ARGS -1,5
254 GOSUB _pseterr:ARGS  4,5
257 goto _psetdone
258 _pseterr:GETARGS PN,QE:QA$="SET PALETTE %% TO R$" % (PN):ON ERROR GOTO _yes_err:SET PALETTE PN TO R$:GOTO _no_err
259 _psetdone:

350 GOSUB _tseterr:ARGS  -1,T$(1),5
352 GOSUB _tseterr:ARGS 512,T$(2),5
357 GOTO _tsetdone
358 _tseterr:GETARGS TN,TS$,QE:QA$="SET TILE %% TO $`%%`" % (TN,HEX$(TS$)):ON ERROR GOTO _yes_err:SET TILE TN TO TS$:GOTO _no_err
359 _tsetdone:



380 GOSUB _outskip
381 GOSUB _tfilaerr:ARGS -1,0,5
382 GOSUB _tfilaerr:ARGS 512,0,5
383 GOSUB _tfilaerr:ARGS 128,-1,5
384 GOSUB _tfilaerr:ARGS 128,4,5
385 GOSUB _outskip
386 GOSUB _tfillerr:ARGS -1, 0,63,31,128,0,5
387 GOSUB _tfillerr:ARGS  0,-1,63,31,128,0,5
388 GOSUB _tfillerr:ARGS  0, 0,64,31,128,0,5
389 GOSUB _tfillerr:ARGS  0, 0,63,32,128,0,5

396 GOTO _tfilldone
397 _tfilaerr:GETARGS TN,TP,QE:QA$="FILL TILEMAP TILE %% PALETTE %%" % (TN,TP):ON ERROR GOTO _yes_err:FILL TILEMAP TILE TN PALETTE TP:GOTO _no_err
398 _tfillerr:GETARGS X1,Y1,X2,Y2,TN,TP,QE:QA$="FILL TILEMAP (X1,Y1)-(X2,Y2) TILE %% PALETTE %%" % (TN,HEX$(TS$)):ON ERROR GOTO _yes_err:FILL TILEMAP (X1,Y1)-(X2,Y2) TILE TN PALETTE TP:GOTO _no_err
399 _tfilldone:

