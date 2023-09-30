;====================================================================
; Graphics Bitmap Drawing Statements and Functions
;====================================================================


;-----------------------------------------------------------------------------
; Bloxel PRESET and PRESET with the EX AF,AF' factored out 
;-----------------------------------------------------------------------------
ST_PSET:
    call    SCAND             ;;Parse (X,Y)
    call    SCALXY            ;;Convert X,Y
    ld      a,1
    jp      z,RSETCC          ;;Semigraphics at screen location?
    ld      (hl),$A0          ;;No, store base semigraphic
    jp      RSETCC

ST_PRESET:
    call    SCAND             ;;Parse (X,Y)
    call    SCALXY            ;;Convert X,Y
    ld      a,0
    jp      z,RSETCC          ;;Semigraphics at screen location?
    ld      (hl),$A0          ;;No, store base semigraphic
    jp      RSETCC

