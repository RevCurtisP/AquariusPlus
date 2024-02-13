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
; INSTR
;----------------------------------------------------------------------------
;; ? INSTR(2,"ABCABCABC","ABC")
FN_INSTR:  
    rst     CHRGET                ; EAT FIRST CHAR
    call    FRMPRN                ; EVALUATE FIRST ARG
    call    GETYPE                ; SET ZERO IF ARG A STRING.                    
    ld      a,1                   ; IF SO, ASSUME, SEARCH STARTS AT FIRST CHAR   
    push    af                    ; Stack = Offset, RtnAdr
    jp      z,.was_string         ; If first arg is not string
    pop     af                    ;   Stack = RtnAdr                      
    call    CONINT                ;   FORCE ARG1 (I%) TO BE INTEGER                
    or      a                     ;   DONT ALLOW ZERO OFFSET                                                   
    jp      z,FCERR               ;   KILL HIM.                                                                
    push    af                    ;   Stack = Offset, RtnAdr
    SYNCHK  ','                   ;   EAT THE COMMA                                -                            
    call    FRMEVL                ;   Evaluate needle                                                    
    call    CHKSTR                ;   BLOW UP IF NOT STRING                                                   
.was_string: 
    SYNCHK  ','                   ; EAT COMMA AFTER ARG                                                     
    push    hl                    ; Stack = TxtPtr, Offset, RtnAdr                                                   
    ld      hl,(FACLO)            ; HL = HayDsc
    ex      (sp),hl               ; HL = TxtPtr; Stack = HayDsc, Offset, RtnAdr
    call    FRMEVL                ; Evaluate Needle
    SYNCHK  ')'                   ; EAT RIGHT PAREN                                                         
    push    hl                    ; Stack = TxtPtr, HayDsc, Offset, RtnAdr
    call    FRESTR                ; HL = NdlDsc
    ex      de,hl                 ; DE = NdlDsc
    pop     bc                    ; BC = TxtPtr; Stack = HayDsc, Offset, RtnAdr
    pop     hl                    ; HL = HayDsc; Stack = Offset, RtnAdr
    pop     af                    ; A = Offset; Stack = RtnAdr
    push    bc                    ; Stack = TxtPtr, RtnAdr
    ld      bc,POPHRT             ; 
    push    bc                    ; Stack = POPHRT, TxtPtr, RtnAdr
    ld      bc,SNGFLT             ; 
    push    bc                    ; Stack =  SNGFLT, POPHRT. TxtPtr, RtnAdr
    push    af                    ; Stack = Offset, SNGFLT, POPHRT. TxtPtr, RtnAdr
    push    de                    ; Stack = NdlDsc, Offset, SNGFLT, POPHRT. TxtPtr, RtnAdr
    call    FRETM2                ; HL = HayDsk
    pop     de                    ; DE = NdlDsc; Stack = Offset, SNGFLT, POPHRT. TxtPtr, RtnAdr
    pop     af                    ; A = Offset; Stack = SNGFLT, POPHRT. TxtPtr, RtnAdr

;----------------------------------------------------------------------------
; Search for string in string
;  Input: A = Offset (base 1)
;        DE = Needle descriptor
;        HL = Haystack descriptor
; Output: A = Position of needle in haystack (0 = not found)
; Clobbered: BC, DE, HL
;----------------------------------------------------------------------------
ext_instr:
    ld      b,a                   ; B = OffCnt
    dec     a                     ; Back up Offset
    ld      c,a                   ; C = Offset
    cp      (hl)                  ; If Ofs > HayLen
    ld      a,0                   ;   Return 0
    ret     nc                                                              
    ld      a,(de)                ; A = NdlLen
    or      a                     ; Set flags
    ld      a,b                   ; A = Offset
    ret     z                     ; Return 0 if NdlLen = 0
    ld      a,(hl)                ; A = HayLen
    inc     hl                 
    inc     hl                    ; Bump to text addrss
    ld      b,(hl)             
    inc     hl                                                              
    ld      h,(hl)            
    ld      l,b                   ; HL = HayAdr
    ld      b,0                   ; BC = Offset
    add     hl,bc                 ; HL = HayPtr
    sub     c                     ; Adjust Haylen
    ld      b,a                   ; B = HayLen
    push    bc                    ; Stack = Offset, OffCnt, RtnAdr
    push    de                    ; Stack = NdlDsc, Offset, OffCnt, RtnAdr
    ex      (sp),hl               ; HL = NdlDsc; Stack = HayPtr, Offset, OffCnt, RtnAdr
    ld      c,(hl)                ; C = NdlLen
    inc     hl                    
    inc     hl                
    ld      e,(hl)            
    inc     hl                    ; DE = NdlAdr
    ld      d,(hl)            
    pop     hl                    ; HL = HayAdr; Stack = OfsOfc, RtnAdr
.cnt_loop
    push    hl                    ; Stack = HayAdr, OfsOfc, RtnAdr
    push    de                    ; Stack = NdlAdr, HayAdr, OfsOfc, RtnAdr
    push    bc                    ; Stack = HayLen, NdlLen, HayAdr, Offset, OffCnt, RtnAdr
.ofs_loop
    ld      a,(de)                ; A = NdlChr
    cp      (hl)                  ;  If NdlChr <> HayChr
    jp      nz,.notfound          ;    Return 0
    inc     de                    ; Bump NdlPtr
    dec     c                     ; Decrement NdlLen
    jp      z,.found              ; If not end of NdlStr
    inc     hl                    ;   Bump HayPtr
    dec     b                     ;   Decrement HayCnt
    jp      nz,.ofs_loop          ;   If HayCnt <> 0, check next character
.pop3ret0 
    pop     de                    ; Stack = NdlLen, HeyAdr, OfsOfc, RtnAdr
    pop     de                    ; Stack = HeyAdr, OfsOfc, RtnAdr
    pop     bc                    ; Stack = OfsOfc, RtnAdr
.pop1ret0 
    pop     de                    ; Stack = RtnAdr
    xor     a                     ; Return 0 with flags set
    ret                           ;
.found 
    pop     hl                    ; Stack = NdlLen, HayAdr, Offset, OffCnt, RtnAdr
    pop     de                    ; Stack = HayAdr, OfsOfc, RtnAdr
    pop     de                    ; Stack = OfsOfc, RtnAdr
    pop     bc                    ; B = Offset
    ld      a,b                   ; 
    sub     h                     ; 
    add     c                     ;
    inc     a                     ; Return offset of Needle in Haystack
    ret                           
.notfound                         
    pop     bc                    ; B = HayLen; Stack = NdlLen, HayAdr, OfsOfc, RtnAdr
    pop     de                    ; DE = NdlAdr; Stack = HayAdr, OfsOfc, RtnAdr
    pop     hl                    ; HL = HayAdr; Stack =  OfsOfc, RtnAdr
    inc     hl                    ; Bump HayAdr
    dec     b                     ; Decrement HayLen
    jp      nz,.cnt_loop          ; If not at end of NdlTxt, check next character
    jr      .pop1ret0             ; Else discard OfsOfc and return 0

;----------------------------------------------------------------------------
; Resume after error 
; Clears all error sysvars except the ON ERROR GOTO line 
; Supported syntax: RESUME <lineno/label>
; Unsupported: RESUME / RESUME 0 - Retry statement that errored
;              RESUME NEXT - Resume exectuion at next statement
;----------------------------------------------------------------------------
ST_RESUME:
        jp      m,.token          ; Check for token
        push    af                ; Save Flags
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

.token: rst     SYNCHR
        byte    XTOKEN
        cp      PT3TK
        jp      z,ST_RESUME_PT3
        jp      SNERR

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
; STRPTR
;----------------------------------------------------------------------------
FN_STR:
    rst     CHRGET                ; Skip STR
    rst     SYNCHR
    byte    XTOKEN
    rst       SYNCHR                ; Require PTR(
    byte    PTRTK 
    SYNCHK  '('
    xor     a
    ld      (SUBFLG),a            ; Evaluate Array Indexes
    call    PTRGET                ; DE = VarAdr
    jp      nz,FCERR              ; FC Error if Not There
    SYNCHK  ')'                   ; Require ')'
    call    CHKSTR                ; Make sure variable is a string
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ld      hl,LABBCK             ; HL = Return address for FLOAT_xx
    push    hl                    ; Stack = FltRtn, TxtPtr, RtnAdr
    ex      de,hl                 ; HL = VarAdr
    call    string_addr_len       ; DE = StrAdr, BC = StrLen
    jp      FLOAT_DE              ; Float StrAdr and return

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

;----------------------------------------------------------------------------
; TRON/TROFF
;----------------------------------------------------------------------------
ST_TRO:
        rst     CHRGET            ; Skip TRO
        cp      'N'
        jr      nz,.troff         ; If TRON
        rst     CHRGET            ;   Skip N
        ld      a,(EXT_FLAGS)
        or      TRON_FLAG         ;   Turn flag on
        ld      (EXT_FLAGS),a
        ret
.troff
        SYNCHK  'F'               
        SYNCHK  'F'               ; Require FF
        ld      a,(EXT_FLAGS)
        and     $FF-TRON_FLAG     ;   Turn flag on
        ld      (EXT_FLAGS),a
        ret

;----------------------------------------------------------------------------
; VARPTR
;----------------------------------------------------------------------------
FN_VAR:
    rst     CHRGET                ; Skip VAR
    rst     SYNCHR
    byte    XTOKEN
    rst     SYNCHR                ; Require PTR(
    byte    PTRTK 
    SYNCHK  '('
    cp      MULTK                 ; Check Next Character
    push    af                    ; Stack = VarPfx, RtnAdr
    jr      nz,.not_multk         ; If '*'
    rst     CHRGET                ;   Skip It
    ld      a,1                   ;   Evaluate Array Name
    jr      .get_ptr              ;  
.not_multk:
    xor     a
.get_ptr
    ld      (SUBFLG),a            ; Evaluate Array Indexes
    call    PTRGET                ; Get Pointer
    jp      nz,FCERR              ; FC Error if Not There
    SYNCHK  ')'                   ; Require ')'
    ld      (SUBFLG),a            ; Reset Sub Flag
    pop     af                    ; A = VarPfx; Stack = RtnAdr
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ld      hl,LABBCK             ; HL = Return address for FLOAT_xx
    push    hl                    ; Stack = FltRtn, TxtPtr, RtnAdr
    jp      nz,FLOAT_DE           ; If not *, eturn variable address
    dec     bc                    ; Else  
    dec     bc                    ;   Back up to beginning of array definition
    jp      FLOAT_BC              ;   and Float It
