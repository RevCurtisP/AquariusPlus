100 REM Keyboard Matrix Read Test
110 DIM C(128):C(0)=6
120 FOR I=1 TO 128:C(I)=96:NEXT
210 CLS
215 PRINT:PRINT:PRINT
220 PRINT "   Win  1 2 3 4 5 6 7 8 9 0 - ="
225 PRINT     
230 PRINT "   Tab   Q W E R T Y U I O P / BS"
235 PRINT     
240 PRINT " Alt Menu A S D F G H J K L ; :"
245 PRINT     
250 PRINT "   SHIFT   Z X C V B N M , . RTN"
255 PRINT
260 PRINT "   CTL SysRq Pgup Home Left Up   Ins"
265 PRINT
270 PRINT "   SPACE Brk PgDn End Down Right Del"
275 PRINT
300 B=255-IN(32767)
301 POKE 13483,C(B AND 1)                        :REM 2
302 POKE 13564,C(B AND 2)                        :REM W
303 POKE 13481,C(B AND 4)                        :REM 1
304 POKE 13562,C(B AND 8)                        :REM Q
305 POKE 13716,STRING$(5,C(B AND 16))            :REM SHIFT
306 POKE 13796,STRING$(3,C(B AND 32))            :REM CTL
307 POKE 13634,STRING$(3,C(B AND 64))            :REM ALT
308 POKE 13476,STRING$(3,C(B AND 128))           :REM GUI
310 B=255-IN(-16385)
311 POKE 13485,C(B AND 1)                        :REM 3
312 POKE 13566,C(B AND 2)                        :REM E
313 POKE 13645,C(B AND 4)                        :REM S
314 POKE 13724,C(B AND 8)                        :REM Z
315 C=C(B AND 16):FOR I=13876 TO 13880:POKE I,C:NEXT:REM SPACE
316 POKE 13643,C(B AND 32)                       :REM A
317 POKE 13638,STRING$(4,C(B AND 64))            :REM Menu
318 C=C(B AND 128):FOR I=13556 TO 13558:POKE I,C:NEXT:REM Tab
320 B=255-IN(-8193)
321 POKE 13489,C(B AND 1)                        :REM 5
322 POKE 13570,C(B AND 2)                        :REM T
323 POKE 13487,C(B AND 4)                        :REM 4
324 POKE 13568,C(B AND 8)                        :REM R
325 POKE 13647,C(B AND 16)                       :REM D
326 POKE 13726,C(B AND 32)                       :REM X
327 C=C(B AND 64):FOR I=13882 TO 13884:POKE I,C:NEXT:REM Break
328 C=C(B AND 128):FOR I=13800 TO 13804:POKE I,C:NEXT:REM SysReq
330 B=255-IN(-4097)                              
331 POKE 13491,C(B AND 1)                        :REM 6
332 POKE 13572,C(B AND 2)                        :REM Y
333 POKE 13651,C(B AND 4)                        :REM G
334 POKE 13730,C(B AND 8)                        :REM V
335 POKE 13728,C(B AND 16)                       :REM C
336 POKE 13649,C(B AND 32)                       :REM F
337 C=C(B AND 64):FOR I=13806 TO 13809:POKE I,C:NEXT:REM PgUp
338 C=C(B AND 128):FOR I=13886 TO 13889:POKE I,C:NEXT:REM PgDn
340 B=255-IN(-2049)                              
341 POKE 13495,C(B AND 1)                        :REM 8
342 POKE 13576,C(B AND 2)                        :REM I
343 POKE 13493,C(B AND 4)                        :REM 7
344 POKE 13574,C(B AND 8)                        :REM U
345 POKE 13653,C(B AND 16)                       :REM H
346 POKE 13732,C(B AND 32)                       :REM B
347 C=C(B AND 64):FOR I=13811 TO 13814:POKE I,C:NEXT:REM Home
348 C=C(B AND 128):FOR I=13891 TO 13893:POKE I,C:NEXT:REM End
350 B=255-IN(-1025)                              
351 POKE 13497,C(B AND 1)                        :REM 9
352 POKE 13578,C(B AND 2)                        :REM O
353 POKE 13657,C(B AND 4)                        :REM K
354 POKE 13736,C(B AND 8)                        :REM M
355 POKE 13734,C(B AND 16)                       :REM N
356 POKE 13655,C(B AND 32)                       :REM J
357 C=C(B AND 64):FOR I=13816 TO 13819:POKE I,C:NEXT:REM Left
358 C=C(B AND 128):FOR I=13895 TO 13898:POKE I,C:NEXT:REM Down
360 B=255-IN(-513)                               
361 POKE 13501,C(B AND 1)                        :REM -
362 POKE 13582,C(B AND 2)                        :REM /
363 POKE 13499,C(B AND 4)                        :REM 0
364 POKE 13580,C(B AND 8)                        :REM P
365 POKE 13659,C(B AND 16)                       :REM L
366 POKE 13738,C(B AND 32)                       :REM ,
367 C=C(B AND 64):FOR I=13821 TO 13822:POKE I,C:NEXT:REM Up
368 C=C(B AND 128):FOR I=13900 TO 13904:POKE I,C:NEXT:REM Right
370 B=255-IN(-257)                               
371 POKE 13503,C(B AND 1)                        :REM =
372 C=C(B AND 2):FOR I=13584 TO 13585:POKE I,C:NEXT :REM BS
373 POKE 13663,C(B AND 4)                        :REM :
374 C=C(B AND 8):FOR I=13742 TO 13744:POKE I,C:NEXT :REM RTN
375 POKE 13661,C(B AND 16)                       :REM ;
376 POKE 13740,C(B AND 32)                       :REM .
377 C=C(B AND 64):FOR I=13826 TO 13828:POKE I,C:NEXT:REM Ins
378 C=C(B AND 128):FOR I=13906 TO 13908:POKE I,C:NEXT:REM Delete
390 GOTO 300
