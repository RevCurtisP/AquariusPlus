# plusBASIC Release Checklist

- Implemented
  - LOAD/RUN tokenized BASIC programs
  - LOAD/RUN ASCII BASIC programs
  - SAVE "x1":SAVE "x2":SAVE "x3": DEL x1: DEL x2 x3:DIR x*
  - 
  - RUN "esp:settings
  - RUN ROM images  
  -   RUN /roms/minidiag.rom
  - RUN /aqds/aqds.aqx
  - CD /CPM:RUN gocpm.aqx
  - PLAY SAMPLE
    - CD /sounds:RUN bigben.baq
  - plusBASIC demos
    - RUN /demos/plusbasic/sprite.bas
    - RUN /demos/plusbasic/sprites.bas
    - RUN /demos/plusbasic/bigsprite.bas
    - RUN /demos/plusbasic/flipsprite.bas
    - RUN /demos/plusbasic/mouse.bas
    - RUN /demos/plusbasic/mdraw.bas
    - RUN /demos/plusbasic/mdraw80.bas
  - Unit tests
    - RUN cp.baq
    - RUN pp.baq
    - RUN sl.baq
    - RUN tf.baq
    - RUN tr.baq
    - RUN ts.baq
  - PLAY PT3
    - LOAD PT3 "/music/songs1/chuta.pt3"
    - PRINT PT3STATUS
    - PAUSE PT3
    - PRINT PT3STATUS
    - RESUME PT3
    - STOP PT3
    - PLAY PT3 "/music/songs1/dance.pt3"
    - PRINT PT3LOOP
    - LOOP PT3 "/music/songs1/dontstop.pt3"
    - PRINT PT3LOOP
