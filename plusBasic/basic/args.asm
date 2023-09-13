;-----------------------------------------------------------------------------
; args.asm - Passing arguments into and results out of subroutines
; GOSUB line#/label : ARGS vallist [RETURN varlist]
; GETARGS varlist
; RETURN vallist
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; GETARGS statement
;-----------------------------------------------------------------------------
ST_GETARGS:
    rst     CHRGET                ; Skip ARGS Token
    jp      z,MOERR               ; "Missing operand" if end of state
    ex      de,hl                 ; DE = Text Pointer
    ld      hl,2
    add     hl,sp                 ; HL = Top FOR/GOSUB stack entry
    ld      a,(hl)                ; Get Token
    cp      GOSUTK                ; If not GOSUB
    jr      nz,AGERR              ;   "ARGS without GOSUB" error
    inc     hl                    ; Skip Token
    inc     hl                    ; Skip Line Number
    inc     hl                    ; Now HL = Pointer to Return Address
    push    hl                    ; Save it                                   Stack: ra_ptr
    ld      a,(hl)
    inc     hl
    ld      h,(hl)                
    ld      l,a                   ; HL = ArgVals Pointer
; Look for colon before ARGS
.find_colon:
    inc     hl
    ld      a,(hl)
    or      a
    jp      z,MOERR               
    cp      ':'
    jr      nz,.find_colon
; Skip Colon and Check for ARGS token
    rst     CHRGET                ; Get next character
    cp      ARGSTK                ; If not ARGS token
    jp      nz,MOERR
    rst     CHRGET                ; Skip the ARGS Token
    push    hl                    ; Save *ArgVals                             Stack: *ArgVals, ra_ptr
    ex      de,hl                 ; HL = *ArgVars
.get_arg:
;    ld      a,128
;    ld      (SUBFLG),a            ; Arrays not allowed
    call    PTRGET                ; DE = VarPtr
    ex      (sp),hl               ; HL = *ArgVals                             Stack: *ArgVars, ra_ptr
    call    LETDO                 ; Evaluate ArgVal into ArgVar
    ld      bc,TEMPST             ; Reset the Temporary String LIst
    ld      (TEMPPT),bc           ; to Avoid ST errors
    ld      a,(hl)                ; Get Char after ArgVal
    cp      ','                                   
    jr      nz,.not_comma          ; If it's a comma
    rst     CHRGET                ;   Skip it
.not_comma:
    ex      (sp),hl               ; HL = *ArgVars                             Stack: *ArgVals, ra_ptr
    ld      a,(hl)                ; Check character after ArgVar
    cp      ','
    jr      nz,.done              ; If comma
    rst     CHRGET                ;   Skip it
    jr      .get_arg              ;   and do next argument

.done
    ex      de,hl                 ; DE = *ArgVars
;    pop     bc                    ; BC = *ArgVals                             Stack: ra_ptr
; Check for junk after argvals    
    pop     hl                    ; HL = *ArgVals                             Stack: ra_ptr
    call    CHRGT2                ; Reget char after last Arg
    jr      z,.is_term            ; Terminator OK
    cp      RETTK                 ; Else Syntax Error if not RETURN
    jp      nz,SNERR
.is_term
    ld      b,h                   ; BC = End of ArgVals
    ld      c,l
    pop     hl                    ; HL = *ra_ptr
    ld      (hl),c
    inc     hl
    ld      (hl),b                ; End of ArgVals is new Return Address
    ex      de,hl                 ; HL = *ArgVars
    ret
    
AGERR:
    ld      e,ERRAG
    jp      ERROR
    

;-----------------------------------------------------------------------------
; RETURN arg, arg, ...
;-----------------------------------------------------------------------------
ST_RETURN:
    jp      z,RETURN              ; If no Args, do standard Return
    ld      (SAVTXT),hl           ; Save Pointer to RetVals
    ld      d,255                 ; MAKE SURE VARIABLE POINTER IN [D,E] NEVER GETS MATCHED
    call    FNDFOR                ; GO PAST ALL THE "FOR" ENTRIES
    ld      sp,hl                 ; UPDATE THE STACK
    cp      GOSUTK                ;
    ld      e,ERRRG               ; ERROR ERRRG IS "RETURN WITHOUT GOSUB"
    jp      nz,ERROR              ;
    pop     hl                    ; GET LINE # "GOSUB" WAS FROM
    ld      (CURLIN),hl           ; PUT IT INTO CURLIN
    inc     hl                    ;
    ld      a,h                   ;
    or      l                     ; Is line number $FFFF
    jr      nz,.line_ok           ; No, carry on
    ld      a,(USFLG)             ;
    or      a                     ; Is flag set?
    jp      nz,STPRDY             ; Yes, abort to direct mode
.line_ok:   
    ld      hl,NEWSTT             ; HL = NEWSTT, Stack = *RetVars
    ex      (sp),hl               ; HL = *RetVars; Stack = NEWSTT
    call    SYNCHR                ; Require RETURN after GOSUB Args
    byte    RETTK
    push    hl                    ; Stack = *RetVars, NEWSTT
    ld      hl,(SAVTXT)           ; HL = *RetVals; Stack = *RetVars, NEWSTT
    ex      (sp),hl               ; HL = **RetVars; Stack = *RetVals, NEWSTT
.get_arg:
    call    PTRGET                ; DE = *RetVar
    ex      (sp),hl               ; HL = *RetVals;  Stack: *RetVars, NEWSTT
    call    LETDO                 ; Evaluate RetVal into RetVar
    ld      a,(hl)                ; Get Char after RetVal
    cp      ','                                   
    jr      nz,.not_comma         ; If it's a comma
    rst     CHRGET                ;   Skip it
.not_comma:
    ex      (sp),hl               ; HL = *RetVars; Stack: *RetVals, NEWSTT
    ld      a,(hl)                ; Check character after RetVar
    cp      ','
    jr      nz,.done              ; If comma
    rst     CHRGET                ;   Skip it
    jr      .get_arg              ;   and do next argument

.done
    pop     de                    ; HL = *RetVars; Stack = NEWSTT
    ret