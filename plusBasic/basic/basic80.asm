;====================================================================
; Statements and Functions from MBASIC 80 for CP/M
;====================================================================

;----------------------------------------------------------------------------
; ON ERROR ... GOTO
; Copied from MX BASIC. Originally from CP/M MBASIC 80 - BINTRP.MAC
;----------------------------------------------------------------------------
on_error:
        cp      ERRTK             ; "ON...ERROR"?
        jr      nz,.noerr         ; NO. Do ON GOTO
        inc     hl                ; Check Following Byte
        ld      a,(hl)            ; Don't Skip Spaces
        cp      (ORTK)            ; Cheat: ERROR = ERR + OR
        jr      z,.onerr          ; If not ERROR
        dec     hl                ; Back up to ERR Token
        dec     hl                ; to process as Function
.noerr: jp      NTOERR            ; and do ON ... GOTO
.onerr: rst     CHRGET            ; GET NEXT THING
        rst     SYNCHR            ; MUST HAVE ...GOTO
        db      GOTOTK  
        call    scan_label        ; Check for a Label
        ld      a,d               ; IS LINE NUMBER ZERO?
        or      e                 ; SEE
        jr      z,reset_trap      ; IF ON ERROR GOTO 0, RESET TRAP
        push    hl                ; SAVE [H,L] ON STACK
        call    FNDLIN            ; SEE IF LINE EXISTS
        ld      d,b               ; GET POINTER TO LINE IN [D,E]
        ld      e,c               ; (LINK FIELD OF LINE)
        pop     hl                ; RESTORE [H,L]
        jp      nc,USERR          ; ERROR IF LINE NOT FOUND
reset_trap: 
        ld      (ONELIN),de       ; SAVE POINTER TO LINE OR ZERO IF 0.
        ret     c                 ; YOU WOULDN'T BELIEVE IT IF I TOLD YOU
        ld      a,(ONEFLG)        ; ARE WE IN AN "ON...ERROR" ROUTINE?
        or      a                 ; SET CONDITION CODES
        ret     z                 ; IF NOT, HAVE ALREADY DISABLED TRAPPING.
        ld      a,(ERRFLG)        ; GET ERROR CODE
        dec     a                 ; Convert to Offset
        add     a,a               ; and put 
        ld      e,a               ; INTO E.
        jp      force_error       ; FORCE THE ERROR TO HAPPEN


;----------------------------------------------------------------------------
; ERROR Hook Routine for Error Trapping
; Copied from MX BASIC. Originally from CP/M MBASIC 80 - BINTRP.MAC
;----------------------------------------------------------------------------
trap_error:
        ld      hl,(CURLIN)       ; GET CURRENT LINE NUMBER
        ld      (ERRLIN),hl       ; SAVE IT FOR ERL VARIABLE
        ld      a,e               ; Get Error Table Offset
        ld      c,e               ; ALSO SAVE IT FOR LATER RESTORE
        srl     a                 ; Divide by 2 and add 1 so
        inc     a                 ; [A]=ERROR NUMBER
        ld      (ERRFLG),a        ; Save it for ERR() Function
        ld      hl,(ERRLIN)       ; GET ERROR LINE #
        ld      a,h               ; TEST IF DIRECT LINE
        and     l                 ; SET CC'S
        inc     a                 ; SETS ZERO IF DIRECT LINE (65535)
        ld      hl,(ONELIN)       ; SEE IF WE ARE TRAPPING ERRORS.
        ld      a,h               ; BY CHECKING FOR LINE ZERO.
        or      l                 ; IS IT?
        ex      de,hl             ; PUT LINE TO GO TO IN [D,E]
        ld      hl,ONEFLG         ; POINT TO ERROR FLAG
        jr      z,.no_trap        ; SORRY, NO TRAPPING...
        and     (hl)              ; A IS NON-ZERO, SETZERO IF ONEFLG ZERO
        jr      nz,.no_trap       ; IF FLAG ALREADY SET, FORCE ERROR
        dec     (hl)              ; IF ALREADY IN ERROR ROUTINE, FORCE ERROR
        ex      de,hl             ; GET LINE POINTER IN [H,L]
        ld      sp,(SAVSTK)       ;   Restore the Stack Pointer
        jp      GONE4             ; GO DIRECTLY TO NEWSTT CODE
.no_trap: 
        xor     a                 ; A MUST BE ZERO FOR CONTROL
        ld      (hl),a            ; RESET 3
        ld      e,c               ; GET BACK ERROR CODE
        ld      a,(ERROR)         ; Get First Instruction of Error Routine
        cp      $F7               ; If it's RST HOOKDO (S3 BASIC)
        jp      z,$03DD           ;   Clear Stack and Force Error
        jp      force_error       ; FORCE THE ERROR TO HAPPEN

;----------------------------------------------------------------------------
; Error variables:
; ERR - Error number
; ERR$ - Error description
; ERRLINE - Error line number
;----------------------------------------------------------------------------
FN_ERR: rst     CHRGET            ; Skip ERR Token
;        jr      z,.push_it
;        inc     hl                ; Skip Following Token
;.push_it
;        push    hl                ; Save Text Pointer
        jr      z,.err_no
        cp      '$'               ; If Next Character is $
        jr      z,.err_desc       ;   Do ERR$
        cp      LINETK            ; If LINE
        jr      z,.err_line       ;   Do ERRLINE
;        jp      SNERR
.err_no:
        call    push_hl_labbck
        ld      a,(ERRFLG)        ; Get Error Number
        jp      SNGFLT            ; and Float it
.err_desc:
        call    push_hlinc_labbck
        ld      a,(ERRFLG)        ; Get Error Number
        call    get_errno_msg     ; Get Pointer to Error Message in HL
        jp      TIMSTR            ; Turn into Temp String and Return it
.err_line:
        call    push_hlinc_labbck
        ld      de,(ERRLIN)       ; Get Error Line Number
        jp      FLOAT_DE          ; Float it and Return

        

;----------------------------------------------------------------------------
; Resume after error 
; Clears all error sysvars except the ON ERROR GOTO line 
; Supported syntax: RESUME <lineno/label>
; Unsupported: RESUME / RESUME 0 - Retry statement that errored
;              RESUME NEXT - Resume exectuion at next statement
;----------------------------------------------------------------------------
ST_RESUME:
        push    af                ; Save Flags
;        jp      z,.moerr          ; Require Line Number or Label
        ld      a,(ONEFLG)        ; GET FLAG
        or	    a			            ; TRAP ROUTINE.
        jr	    z,.reerr			    ; GIVE RESUME WITHOUT ERROR ERROR	
        exx                       ; Save Registers
        ld      b,4               ; Clear ERRLIN,ERRFLG,ONEFLG
        call    clear_errvars     ; and Restore registers
        pop     af                ; Get back flags
        ret     z
        jp      GOTO              ; Scan Line Number and GOTO it

.moerr: ld      e,ERRMO
        jr      .error

.reerr: ld      e,ERRRE           ; Load RE Error Code
.error: jp      force_error       ; Do Error

;----------------------------------------------------------------------------
; Clear ERROR system variables
;----------------------------------------------------------------------------
clear_all_errvars:
        exx                       ; Save Registers
        ld      b,6               ; Clear ERRLIN,ERRFLG,ONEFLG,ONELIN
clear_errvars:
        ex      af,af'  
        xor     a
        ld      hl,ERRLIN 
.zloop: ld      (hl),a  
        inc      hl 
        djnz    .zloop  
        ex      af,af'  
        exx                       ; Restore Registers                  
        ret

;----------------------------------------------------------------------------
;;; ---
;;; STRING$ - Create string of repeating characters.
;;; STRING$ (*length*)
;;; STRING$ (*length*, *byte* )
;;; STRING$ (*length*, *string* )
FN_STRING: 
        rst     CHRGET
        SYNCHK  '$'               ;Require $
        SYNCHK  '('               ;MAKE SURE LEFT PAREN
        call    GETBYT            ;EVALUATE FIRST ARG (LENGTH)
        ld      a,(hl)            ;Check Next Character
        cp      ','               ;If No Comma
        jr      nz,SPACE          ;  Single Argument - Act Like SPACE$() Function
        rst     CHRGET            ;Else Skip Comma
        push    de                ;SAVE FIRST ARG (LENGTH)
        call    FRMEVL            ;GET FORMULA ARG 2
        SYNCHK  ')'               ;EXPECT RIGHT PAREN
        ex      (sp),hl           ;SAVE TEXT POINTER ON STACK, GET REP FACTOR
        push    hl                ;SAVE BACK REP FACTOR
        ld      a,(VALTYP)        ;GET TYPE OF ARG
        dec     a                 ;Make 1 into 0
        jr      z,STRSTR          ;WAS A STRING
        call    CONINT            ;GET ASCII VALUE OF CHAR
        jp      CALSPA            ;NOW CALL SPACE CODE
STRSTR: call    ASC2              ;GET VALUE OF CHAR IN [A]
CALSPA: pop     de                ;GET REP FACTOR IN [E]
        CALL  SPACE2              ;INTO SPACE CODE, PUT DUMMY ENTRY
SPACE:  SYNCHK  ')'               ;Require Right Paren after Single Argument
        push    hl                ;Save Text Pointer
        ld      a,' '             ;GET SPACE CHAR
        push    bc                ;Dummy Return Address for FINBCK to discard
SPACE2: push    af                ;SAVE CHAR
        ld      a,e               ;GET NUMBER OF CHARS IN [A]
        call    STRINI            ;GET A STRING THAT LONG
        ld      b,a               ;COUNT OF CHARS BACK IN [B]
        pop     af                ;GET BACK CHAR TO PUT IN STRING
        inc     b                 ;TEST FOR NULL STRING
        dec     b 
        jp      z,FINBCK          ;YES, ALL DONE
        ld      hl,(DSCTMP+2)     ;GET DESC. POINTER
SPLP:   ld      (hl),a            ;SAVE CHAR
        inc     hl                ;BUMP PTR
                                  ;DECR COUNT
        djnz    SPLP              ;KEEP STORING CHAR
        jp      FINBCK            ;PUT TEMP DESC WHEN DONE

;----------------------------------------------------------------------------
; Get Evaluated Formula Type
; Output: Zero Set if String, Clear if Number
; Clobbers: A
;----------------------------------------------------------------------------
GETYPE: ld      a,(VALTYP)        ;REPLACEMENT FOR "GETYPE" RST
        dec     a               
        ret
