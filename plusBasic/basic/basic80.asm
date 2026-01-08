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
        SYNCHKT GOTOTK
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
        call    set_error         ; Populate ERRLIN and ERRFLG
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
        jp      z,ERRINI          ;   Clear Stack and Force Error
        jp      force_error       ; FORCE THE ERROR TO HAPPEN

set_error:
        ld      hl,(CURLIN)       ; GET CURRENT LINE NUMBER
        ld      (ERRLIN),hl       ; SAVE IT FOR ERL VARIABLE
        ld      a,e               ; Get Error Table Offset
        ld      c,e               ; ALSO SAVE IT FOR LATER RESTORE
        srl     a                 ; Divide by 2 and add 1 so
        inc     a                 ; [A]=ERROR NUMBER
        ld      (ERRFLG),a        ; Save it for ERR() Function
        ret

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
        ld      a,(ERRFLG)        ; Get Error Number and float it
        jp      push_hl_labbck_sngflt
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
; Delete Array
; ERASE *arrayvar{, *arrayvar...}
;----------------------------------------------------------------------------
ST_ERASE:
    rst     CHRGET            ; Skip ERASE
_erase:
    call    get_star_array_pointer  ; BC = NumDim, DE = NxtAry
    push    hl
    ld      iy,bas_erase_array
    call    aux_call              ; Remove array from variable space
    pop     hl
    ld      a,(hl)                ;SEE IF MORE ERASURES NEEDED
    cp      ','                   ;ADDITIONAL VARIABLES DELIMITED BY COMMA
    ret     nz                    ;ALL DONE IF NOT
    rst     CHRGET
    jr      _erase

;----------------------------------------------------------------------------
; INSTR
;----------------------------------------------------------------------------
;; ? INSTR(2,"ABCABCABC","ABC")
FN_INSTR:
    rst     CHRGET                ; EAT FIRST CHAR
    call    FRMPRT                ; EVALUATE FIRST ARG, SET ZERO IF ARG A STRING.
    ld      a,1                   ; IF SO, ASSUME, SEARCH STARTS AT FIRST CHAR
    push    af                    ; Stack = Offset, RtnAdr
    jp      z,.was_string         ; If first arg is not string
    pop     af                    ;   Stack = RtnAdr
    call    CONINT                ;   FORCE ARG1 (I%) TO BE INTEGER
    or      a                     ;   DONT ALLOW ZERO OFFSET
    jp      z,FCERR               ;   KILL HIM.
    push    af                    ;   Stack = Offset, RtnAdr
    call    get_comma             ;   EAT THE COMMA                                -
    call    FRMSTR                ;   Evaluate needle
.was_string:
    call    get_comma             ; EAT COMMA AFTER ARG
    push    hl                    ; Stack = TxtPtr, Offset, RtnAdr
    ld      hl,(FACLO)            ; HL = HayDsc
    ex      (sp),hl               ; HL = TxtPtr; Stack = HayDsc, Offset, RtnAdr
    call    FRMEVL                ; Evaluate Needle
    SYNCHKC ')'                   ; EAT RIGHT PAREN
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
    ld      iy,string_search
    jp      aux_call

;----------------------------------------------------------------------------
; LEFT HAND SIDE MID$
; MID$(var$,len{,pos}) = string$
;----------------------------------------------------------------------------
ST_MID: SYNCHKC '('               ; MUST HAVE (                                      AF     BC     DE     HL   Stack
        call    PTRGET            ; GET A STRING VAR                               ------ ------  Dsc1  TxtPtr
        call    CHKSTR            ; MAKE SURE IT WAS A STRING
        push    hl                ; SAVE TEXT POINTER                                                          TxtPtr
        push    de                ; SAVE DESC. POINTER                                                         Dsc1,TxtPtr
        ex      de,hl             ; PUT DESC. POINTER IN [H,L]                                   TxtPtr  Dsc1
        inc     hl                ; MOVE TO ADDRESS FIELD                                                 ++
        inc     hl                ;                                                                       ++
        ld      e,(hl)            ; GET ADDRESS OF LHS IN [D,E]
        inc     hl                ; BUMP DESC. POINTER                                                    ++
        ld      d,(hl)            ; PICK UP HIGH BYTE OF ADDRESS                                 Addr1
        ld      hl,(TOPMEM)       ; SEE IF LHS STRING IS IN STRING SPACE                               (STREND)
        rst     COMPAR            ; BY COMPARING IT WITH TOPMEM
        jr      c,NCPMID          ; IF ALREADY IN STRING SPACE DONT COPY.
        pop     hl                ; GET BACK DESC. POINTER                                               Dsc1  TxtPtr
        push    hl                ; SAVE BACK ON STACK                                                         Dsc1,TxtPtr
        call    STRCPY            ; COPY THE STRING LITERAL INTO STRING SPACE                    DSCTMP   ~~
        pop     hl                ; GET BACK DESC. POINTER                                               Dsc1  TxtPtr
        push    hl                ; BACK ON STACK AGAIN                                                        Dsc1,TxtPtr
        ld      de,DSCTMP         ; Get New String Descriptor Address                             DscN
        call    VMOVE             ; MOVE NEW DESC. INTO OLD SLOT.                                  ~~     ~~
NCPMID: pop     hl                ; GET DESC. POINTER                                                    Dsc1  TxtPtr
        ex      (sp),hl           ; GET TEXT POINTER TO [H,L] DESC. TO STACK                            TxtPtr  Dsc1
        call    get_comma_byte    ; GET ARG#2 (OFFSET INTO STRING)                 Arg2
        or      a                 ; MAKE SURE NOT ZERO
        jp      z,FCERR           ; BLOW HIM UP IF ZERO
        push    af                ; SAVE ARG#2 ON STACK                                                        Arg2,Dsc1
        ld      a,(hl)            ; RESTORE CURRENT CHAR                         (TxtPtr)
        ld      e,255             ; IF TWO ARG GUY, TRUNCATE.                                     255
        cp      ')'               ; [E] SAYS USE ALL CHARS
        jp      z,.MID2           ; IF ONE ARGUMENT THIS IS CORRECT
        call    get_comma_byte    ; GET ARGUMENT  IN  [E]                          Arg3           Arg3
.MID2:  SYNCHKC ')'               ; MUST BE FOLLOWED BY )                           ~~                         Arg3,Arg2,Dsc1
        push    de                ; SAVE THIRD ARG ([E]) ON STACK
                                  ; MUST HAVE = SIGN
        call    FRMEQL            ; EVALUATE RHS OF THING.
        push    hl                ; SAVE TEXT POINTER.                                                         TxtPtr,Arg3,Arg2,Dsc1
        call    FRESTR            ; FREE UP TEMP RHS IF ANY.                        ~~             ~~    Dsc2
        ex      de,hl             ; PUT RHS DESC. POINTER IN [D,E]                                Dsc2    ~~
        pop     hl                ; TEXT POINTER TO [H,L]                                               TxtPtr Arg3,Arg2,Dsc1
        pop     bc                ; ARG #3 TO C.                                           Arg3                Arg2,Dsc1
        pop     af                ; ARG #2 TO A.                                    Arg2                       Dsc1
        ld      b,a               ; AND [B]                                               A2  A3
        ex      (sp),hl           ; GET LHS DESC. POINTER TO [H,L]                                       Dsc1  TxtPtr
                                  ; TEXT POINTER TO STACK
        push    hl                ;                                                                            Dsc1,TxtPtr
        ld      hl,POPHRT         ; GET ADDR TO RETURN TO                                               POPHRT
        ex      (sp),hl           ; SAVE ON STACK & GET BACK TXT PTR.                                    Dsc1  POPHRT,TxtPtr
        ld      a,c               ; GET ARG #3                                      Arg3
        or      a                 ; SET CC'S
        ret     z                 ; IF ZERO, DO NOTHING
        ld      a,(hl)            ; GET LENGTH OF LHS                              (Dsc1)
        sub     b                 ; SEE HOW MANY CHARS IN REMAINDER OF STRING
        jp      c,FCERR           ; CANT ASSIGN PAST LEN(LHS)!
        inc     a                 ; MAKE PROPER COUNT
        cp      c                 ; SEE IF # OF CHARS IS .GT. THIRD ARG
        jr      c,BIGLEN          ; IF SO, DONT TRUNCATE
        ld      a,c               ; TRUNCATE BY USING 3RD ARG.                      Arg3
BIGLEN: ld      c,b               ; GET OFFSET OF STRING IN [C]
        dec     c                 ; MAKE PROPER OFFSET                                    Arg2
        ld      b,0               ; SET UP [B,C] FOR LATER DAD B.
        push    de                ; SAVE [D,E]                                                                 Dsc2,POPHRT,TxtPtr
        inc     hl
        inc     hl                ; POINTER TO ADDRESS FIELD.
        ld      e,(hl)            ; GET LOW BYTE IN [E]
        inc     hl                ; BUMP POINTER
        ld      h,(hl)            ; GET HIGH BYTE IN [H]
        ld      l,e               ; NOW COPY LOW BYTE BACK TO [L]                                        Txt1
        add     hl,bc             ; ADD OFFSET                                                          + Arg2
        ld      b,a               ; SET COUNT OF LHS IN [B]
        pop     de                ; RESTORE [D,E]                                                 Dsc2        POPHRT,TxtPtr
        ex      de,hl             ; MOVE RHS. DESC. POINTER TO [H,L]
        ld      c,(hl)            ; GET LEN(RHS) IN [C]
        inc     hl                ; MOVE POINTER
        inc     hl                ; MOVE POINTER
        ld      a,(hl)            ; GET LOW BYTE OF ADDRESS IN [A]
        inc     hl                ; BUMP POINTER.
        ld      h,(hl)            ; GET HIGH BYTE OF ADDRESS IN [H]
        ld      l,a               ; COPY LOW BYTE TO [L]
        ex      de,hl             ; ADDRESS OF RHS NOW IN [D,E]
        ld      a,c               ; IS RHS NULL?
        or      a                 ; TEST
        ret     z                 ; THEN ALL DONE.
; NOW ALL SET UP FOR ASSIGNMENT.
; [H,L] = LHS POINTER
; [D,E] = RHS POINTER
; C = LEN(RHS)
; B = LEN(LHS)
.MIDLP: ld      a,(de)            ; GET BYTE FROM RHS.
        ld      (hl),a            ; STORE IN LHS
        inc     de                ; BUMP RHS POINTER
        inc     hl                ; BUMP LHS POINTER.
        dec     c                 ; BUMP DOWN COUNT OF RHS.
        ret     z                 ; IF ZERO, ALL DONE. IF LHS ENDED, ALSO DONE.
        djnz    .MIDLP            ; IF NOT DONE, MORE COPYING.
        ret                       ; BACK TO NEWSTT



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

.token: SYNCHKT XTOKEN
        cp      TRKTK
        jp      z,ST_RESUME_TRACK
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
    call    get_ptrtk
    SYNCHKC '('
    xor     a
    ld      (SUBFLG),a            ; Evaluate Array Indexes
    call    PTRGET                ; DE = VarAdr
    jp      nz,FCERR              ; FC Error if Not There
    SYNCHKC ')'                   ; Require ')'
    call    CHKSTR                ; Make sure variable is a string
    call    push_hl_labbck        ; Stack = LABBCK, TxtPtr, RtnAdr
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
        call    skip_dollar_paren ; Require $(
        call    GETBYT            ;EVALUATE FIRST ARG (LENGTH)
        ld      a,(hl)            ;Check Next Character
        cp      ','               ;If No Comma
        jr      nz,SPACE          ;  Single Argument - Act Like SPACE$() Function
        rst     CHRGET            ;Else Skip Comma
        push    de                ;SAVE FIRST ARG (LENGTH)
        call    FRMEVL            ;GET FORMULA ARG 2
        SYNCHKC ')'               ;EXPECT RIGHT PAREN
        ex      (sp),hl           ;SAVE TEXT POINTER ON STACK, GET REP FACTOR
        push    hl                ;SAVE BACK REP FACTOR
        ld      a,(VALTYP)        ;GET TYPE OF ARG
        dec     a                 ;Make 1 into 0
        jr      z,STRSTR          ;WAS A STRING
        call    CONINT            ;GET ASCII VALUE OF CHAR
        jp      CALSPA            ;NOW CALL SPACE CODE
STRSTR: call    ASC2              ;GET VALUE OF CHAR IN [A]
CALSPA: pop     de                ;GET REP FACTOR IN [E]
        CALL    SPACE2            ;INTO SPACE CODE, PUT DUMMY ENTRY
SPACE:  SYNCHKC ')'               ;Require Right Paren after Single Argument
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
ST_SWAP_VARS:
    rst     CHRGET            ; Skip VAR
    SYNCHKC 'S'
    call    PTRGET            ;[D,E]=POINTER AT VALUE #1
    push    de                ;SAVE THE POINTER AT VALUE #1
    push    hl                ;SAVE THE TEXT POINTER
    ld      hl,FACLO          ;TEMPORARY STORE LOCATION
    call    VMOVE             ;FACLO =VALUE #1
    ld      hl,ARYTAB         ;GET ARYTAB SO CHANGE CAN BE NOTED
    ex      (sp),hl           ;GET THE TEXT POINTER BACK AND SAVE CURRENT [ARYTAB]
    ld      a,(VALTYP)        ;Get Variable Type
    push    af                ;SAVE THE TYPE OF VALUE #1
    call    get_comma         ;MAKE SURE THE VARIABLES ARE DELIMITED BY A COMMA
    call    PTRGET            ;[D,E]=POINTER AT VALUE #2
    pop     bc                ;[B]=TYPE OF VALUE #1
    ld      a,(VALTYP)        ;[A]=TYPE OF VALUE #2
    cmp     b                 ;MAKE SURE THEY ARE THE SAME
    jp      nz,TMERR          ;IF NOT, "TYPE MISMATCH" ERROR
    ex      (sp),hl           ;[H,L]=OLD [ARYTAB] SAVE THE TEXT POINTER
    ex      de,hl             ;[D,E]=OLD [ARYTAB]
    push    hl                ;SAVE THE POINTER AT VALUE #2
    ld      hl,ARYTAB         ;GET NEW [ARYTAB]
    rst     COMPAR
    jp      nz,FCERR          ;IF ITS CHANGED, ERROR
    pop     de                ;[D,E]=POINTER AT VALUE #2
    pop     hl                ;[H,L]=TEXT POINTER
    ex      (sp),hl           ;SAVE THE TEXT POINTER ON THE STACK, [H,L]=POINTER AT VALUE #1
    push    de                ;SAVE THE POINTER AT VALUE #2
    call    VMOVE             ;TRANSFER VALUE #2 INTO VALUE #1'S OLD POSITION
    pop     hl                ;[H,L]=POINTER AT VALUE #2
    ld      de,FACLO          ;LOCATION OF VALUE #1
    call    VMOVE             ;TRANSFER FACLO =VALUE #1 INTO VALUE #2'S OLD POSITION
    pop     hl                ;GET THE TEXT POINTER BACK
    ret

;----------------------------------------------------------------------------
; TRON/TROFF
;----------------------------------------------------------------------------
;;; ToDo: Move code to AuxROM
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
    SYNCHKC 'F'
    SYNCHKC 'F'               ; Require FF
    ld      a,(EXT_FLAGS)
    and     $FF-TRON_FLAG     ;   Turn flag on
    ld      (EXT_FLAGS),a
    ret

;----------------------------------------------------------------------------
; VARPTR() and VARDEF()
;----------------------------------------------------------------------------
FN_VAR:
    inc     hl                    ; Skip VAR
    ld      a,(hl)                ; A = NxtChr
    cp      DEFTK                 ; If VARDEF
    jp      z,FN_VARDEF           ;   Go do it
    SYNCHKT XTOKEN                ; Else
    SYNCHKT PTRTK                 ;  Require PTR
    SYNCHKC '('
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
    SYNCHKC ')'                   ; Require ')'
    xor     a
    ld      (SUBFLG),a            ; Reset Sub Flag
    pop     af                    ; A = VarPfx; Stack = RtnAdr
    call    push_hl_labbck        ; Stack = LABBCK, TxtPtr, RtnAdr
    jp      nz,FLOAT_DE           ; If not *, Return variable address
    dec     bc                    ; Else
    dec     bc                    ;   Back up to beginning of array definition
    jp      FLOAT_BC              ;   and Float It

;----------------------------------------------------------------------------
; WAIT port, xor_mask, and_mask
;----------------------------------------------------------------------------
ST_WAIT:
        call    GETINT            ; get/evaluate port
        push    de                ; stored to be used in BC
        call    get_comma_byte    ; get/evaluate data
        push    af                ; SAVE THE MASK
        call    CHRGT2            ; SEE IF THE STATEMENT ENDED
        jr      z,NOTTHR          ; IF NO THIRD ARGUMENT SKIP THIS
        call    get_comma_byte    ; Get XOR mask into E
NOTTHR: pop     de                ; REGET THE "AND" MASK in D
        ld      e,a               ; Put the XOR mask in E
        pop     bc                ; Get back the Port #
LOPINP: in      a,(c)             ; THE INPUT INSTR
        xor     e                 ; XOR WITH MASK2
        and     d                 ; AND WITH MASK
        jr      z,LOPINP          ; LOOP UNTIL RESULT IS NON-ZERO
        ret                       ; NOTE: THIS LOOP CANNOT BE CONTROL-C'ED HOWEVER A RESTART AT 0 IS OK.
