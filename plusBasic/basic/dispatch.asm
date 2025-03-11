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
    dec     a                   ; $96 APPEND
    jp      z,ST_APPEND
    dec     a                   ; $97 TRIM
    dec     a                   ; $98 STASH
    jp      z,ST_STASH
    dec     a                   ; $99 TRO
    jp      z,ST_TRO
    dec     a                   ; $9A BREAK
    dec     a                   ; $9B LOOP
    jp      z,ST_LOOP
    sub     a,3                 ; $9E ERASE
    jp      z,ST_ERASE
    dec     a                   ; $9F SPLIT
    jp      z,ST_SPLIT
    sub     JOINTK-SPLITK       ; $A8 JOIN
    jp      z,ST_JOIN
    dec     a                   ; $A9 WAIT
    jp      z,ST_WAIT
    sub     DUMPTK-WAITK        ; $B5 DUMP
    jp      z,ST_DUMP
    jp      SNERR 
   
extended_function:
    inc     hl                  ; Skip extended prefix
    ld      a,(hl)              ; Get Extended token
    sub     ATTRTK              ; $80 ATTR
    jp      z,FN_ATTR
    dec     a                   ; $81 PALETTE
    dec     a                   ; $82 OFF
    jp      z,FN_OFF
    sub     KEYTK-OFFTK         ; $86 KEY
    jp      z,FN_KEY
    sub     ARGSTK-KEYTK        ; $8A ARGS
    jp      z,FN_ARGS
    dec     a                   ; $8B SAMPTK
    dec     a                   ; $8C PT3TK
    jp      z,FN_PT3
    sub     VERTK-PT3TK         ; $92 VER
    jp      z,FN_VER
    dec     a                   ; $93 FILL
    dec     a                   ; $94 COMPARE
    jp      z,FN_COMPARE
    dec     a                   ; $95 PLAY
    dec     a                   ; $96 APPEND
    dec     a                   ; $97 TRIM
    jp      z,FN_TRIM
    sub     a,5                 ; $9C STR
    jp      z,FN_STR
    dec     a                   ; $9D VAR
    jp      z,FN_VAR
    sub     a,3                 ; $A0 PAD
    jp      z,FN_PAD
    dec     a                   ; $A1 WORD
    jp      z,FN_WORD
    sub     a,BYTETK-WORDTK     ; $A5 BYTE
    jp      z,FN_BYTE          
    sub     a,BINTK-BYTETK      ; $AD MIN
    jp      z,FN_BIN
    
    dec     a                   ; $AD MIN
    jp      z,GSERR
    dec     a                   ; $AE MAX
    jp      z,GSERR
    dec     a                   ; $AF UPR
    jp      z,FN_UPR
    dec     a                   ; $B0 LWR
    jp      z,FN_LWR
    dec     a                   ; $B1 SPEED
    dec     a                   ; $B2 LOMG
    jp      z,FN_LONG
    dec     a                   ; $B3 FLOAT
    jp      z,FN_FLOAT
    jp      SNERR

; ------------------------------------------------------------------------------
;  Issue Statement not implemented err
; ------------------------------------------------------------------------------

GSERR:
    ld    e,ERRGS
    jp    force_error    



    
