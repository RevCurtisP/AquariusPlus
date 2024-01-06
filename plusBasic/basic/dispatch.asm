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
    sub     RESETK              ; $90 RESET
    jp      z,ST_RESET
    dec     a                   ; $91
    dec     a                   ; $92 VER
    dec     a                   ; $93 FILL
    jp      z,ST_FILL
    dec     a                   ; $94 COMPARE
    dec     a                   ; $95 PLAY
    jp      z,ST_PLAY
    jp      SNERR
   
extended_function:
    inc     hl                  ; Skip extended prefix
    ld      a,(hl)              ; Get Extended token
    sub     KEYTK               ; $86 KEY
    jp      z,FN_KEY
    sub     VERTK-KEYTK         ; $92 VER
    jp      z,FN_VER
    dec     a                   ; $93 FILL
    dec     a                   ; $94 COMPARE
    jp      z,FN_COMPARE
    dec     a                   ; $95 PLAY
    dec     a                   ; $96 APPEND
    dec     a                   ; $97 TRIM
    jp      z,FN_TRIM
    jp      SNERR

; ------------------------------------------------------------------------------
;  Issue Statement not implemented err
; ------------------------------------------------------------------------------

GSERR:
    ld    e,ERRGS
    jp    force_error    



    
