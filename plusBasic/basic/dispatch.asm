;=====================================================================================
; Statement/Function Dispatch, Keyword, and Error Message Tables and Related Routines
; Dispatch and Lookup Tables are all aligned to not cross boundaries
;======================================================================================

; ------------------------------------------------------------------------------
;  Statement, Function, and Hook Dispatch Tables
; ------------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; Hook 23 - GONE2 (Execute Statement with Token in A)
; ------------------------------------------------------------------------------
exec_next_statement:
    exx                         ; save BC,DE,HL
    sub     $80                 ; Convert from Token to Table Position
    add     a,a                 ; A * 2 to index WORD size vectors
    ld      l,a
    ld      h,high(STJUMPS)
_exec_statement:
    ld      a,(hl)
    ld      ixl,a
    inc     hl
    ld      a,(hl)
    ld      ixh,a
    exx                         ; Restore BC,DE,HL
    rst     CHRGET              ; Skip Token and Eat Spaces
    jp      (ix)                ; Go Do It

; ------------------------------------------------------------------------------
;  Hook 27 - Execute Function
; ------------------------------------------------------------------------------
execute_function:
    push    af                  ; Save A
    exx                         ; save BC,DE,HL
    add     a,a                 ; A * 2 to index WORD size vectors
    ld      l,a
    ld      h,high(FNJUMPS)
    ld      a,(hl)
    ld      ixl,a
    inc     hl
    ld      a,(hl)
    ld      ixh,a
    exx                         ; Restore BC,DE,HL
    pop     af                  ; Restore A
    jp      (ix)                ; Go Do It

extended_statement:
    sub     FILLTK              ; Work upwards from FILL
    jp      z,ST_FILL
    dec     a                   ; Skip COMPARE
    dec     a                   ; 
    jp      z,ST_PLAY
    jp      SNERR
   
extended_function:
    inc     hl                  ; Skip extended prefix
    ld      a,(hl)              ; Get Extended token
    sub     VERTK               ; Work forward from VER
    jp      z,FN_VER
    dec     a                   ; FILL
    dec     a                   ; VER
    jp      z,FN_COMPARE
    jp      SNERR

; ------------------------------------------------------------------------------
;  Issue Statement not implemented err
; ------------------------------------------------------------------------------

GSERR:
    ld    e,ERRGS
    jp    force_error    



    
