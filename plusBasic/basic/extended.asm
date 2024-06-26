;====================================================================
; Statements and Functions from Aquarius Extended BASIC
;====================================================================


;-----------------------------------------------------------------------------
; CLEAR statement extension (Hook 11)
; CLEAR BITMAP [fgcolor, bgcolor]
;-----------------------------------------------------------------------------
clear_hook:
    jp      z,CLEARC              ; If no operands just CLEAR
    cp      BITTK                 
    jp      z,ST_CLEAR_BITMAP
    cp      MULTK                 ; 
    jp      z,ST_CLEAR_ARRAY
    jp      HOOK11+1              ; Continue with )CLEAR

ST_CLEAR_ARRAY:
    call    get_star_array        ; DE = AryAdr, BC = AryLen 
    call    clear_array
    ld      a,(hl)
    cp      ','
    ret     nz'
    rst     CHRGET
    jp      ST_CLEAR_ARRAY

; Input: A: Type, DE: Array Start, BC = Array Length
clear_array:
    call    GETYPE                ; A = AryTyp
    push    hl                    ; Stack = TxrPtr, RtnAdr
    push    de                    ; Stack = AryAdr, TxtPtr, RtnAdr
    push    bc                    ; Stack = AryLen, AryAdr, TxtPtr, RtnAdr
    push    af                    ; Stack = AryTyp, AryLen, AryAdr, TxtPtr, RtnAdr
    ex      de,hl                 ; HL = AryAdr
    call    sys_fill_zero         ; Fill array data with 0
    pop     af                    ; AF = AryTyp; Stack = AryLen, AryAdr, TxtPtr, RtnAdr
    call    z,GARBA2              ; If string, do garbage collection
    pop     bc                    ; BC = AryLen; Stack = AryLen, AryAdr, TxtPtr, RtnAdr
    pop     de                    ; DE = AryAdr; Stack = AryAdr, TxtPtr, RtnAdr
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; CLS statement 
; syntax: CLS [fgcolor, bgcolor]
;-----------------------------------------------------------------------------
ST_CLS:
    jp      z,do_cls              ; No parameters, use default
    call    get_color_args        ; Pares foreground and background colors
    jp      clear_home            ; Clear screen and homecursor

;-----------------------------------------------------------------------------
; DEF statement stub
;-----------------------------------------------------------------------------
ST_DEF:
    cp      FNTK
    jr      z,ST_DEF_FN
    cp      INTTK         
    jp      z,ST_DEF_INT            ; DEF INTLIST
    cp      TILETK
    jp      z,ST_DEF_TILELIST       ; DEF TILELIST
    cp      RGBTK                   
    jp      z,ST_DEF_RGB            ; DEF RGBLIST
    cp      USRTK
    jr      z,ST_DEF_USR
    cp      XTOKEN          
    jp      nz,SNERR                ; Extended Token Prefix
    inc     hl
    ld      a,(hl)                  ; Get Extended Token
    cp      SPRITK
    jp      z,ST_DEF_SPRITE         ; DEF SPRITE
    cp      ATTRTK
    jp      z,ST_DEF_ATTR           ; DEF ATTRLIST
    cp      BYTETK
    jp      z,ST_DEF_ATTR           ; DEF BYTELIST
    cp      PALETK
    jp      z,ST_DEF_PALETTE        ; DEF PALETTELIST
    jp      SNERR

;-----------------------------------------------------------------------------
; DEF USR
;-----------------------------------------------------------------------------
ST_DEF_USR:
    rst     CHRGET                  ; Skip USR
    rst     SYNCHR
    byte    EQUATK                  ; Require =
    call    GETINT                  ; Parse USR routine address
    ld      (USRADD),de
    ret

;-----------------------------------------------------------------------------
; DEF FN
;-----------------------------------------------------------------------------
ST_DEF_FN:
    call    get_fn_ptr                ; GET A POINTER TO THE FUNCTION NAME
    call    ERRDIR                ; DEF IS "ILLEGAL DIRECT"
    ld      bc,DATA               ; MEMORY, RESTORE THE TXTPTR AND GO TO "DATA" 
    push    bc                    ; SKIPPING THE REST OF THE FORMULA
    push    de              
    SYNCHK  '('                   ; SKIP OVER OPEN PAREN
    call    PTRGET                ; GET POINTER TO DUMMY VAR(CREATE VAR)
    push    hl              
    ex      de,hl            
    dec     hl              
    ld      d,(hl)          
    dec     hl              
    ld      e,(hl)          
    pop     hl              
    call    CHKNUM          
    SYNCHK  ')'                   ;{M80} MUST BE FOLLOWED BY )
    rst      SYNCHR
    db      EQUATK
    ld      b,h              
    ld      c,l              
    ex      (sp),hl          
    ld      (hl),c          
    inc     hl              
    ld      (hl),b          
    jp      STRADX           

FN_FN:
    call    get_fn_ptr            ; GET A POINTER TO THE FUNCTION NAME
    push    de                      
    call    PARCHK                ; RECURSIVELY EVALUATE THE FORMULA
    call    CHKNUM                ; MUST BE NUMBER
    ex      (sp),hl               ; SAVE THE TEXT POINTER THAT POINTS PAST THE 
                                      ; FUNCTION NAME IN THE CALL
    ld      e,(hl)                ; [H,L]=VALUE OF THE FUNCTION
    inc     hl                    
    ld      d,(hl)                
    inc     hl                    ; WHICH IS A TEXT POINTER AT THE FORMAL
    ld      a,d                   ; PARAMETER LIST IN THE DEFINITION
    or      e                     ; A ZERO TEXT POINTER MEANS THE FUNCTION 
                                      ; WAS NEVER DEFINED
    jp      z,UFERR               ; IF SO, GIVEN AN "UNDEFINED FUNCTION" ERROR
    ld      a,(hl)              
    inc     hl                  
    ld      h,(hl)              
    ld      l,a                 
    push    hl                    ; SAVE THE NEW VALUE FOR PRMSTK
    ld      hl,(VARNAM)           
    ex      (sp),hl             
    ld      (VARNAM),hl           
    ld      hl,(FNPARM)           
    push    hl                  
    ld      hl,(VARPNT)           
    push    hl                  
    ld      hl,VARPNT             
    push    de                  
    call    MOVMF               
    pop      hl                 
    call    FRMNUM                ; AND EVALUATE THE DEFINITION FORMULA
    dec     hl                    ; CAN HAVE RECURSION AT THIS POINT
    rst     CHRGET                ; SEE IF THE STATEMENT ENDED RIGHT
    jp      nz,SNERR              ; THIS IS A CHEAT, SINCE THE LINE 
                                  ; NUMBER OF THE ERROR WILL BE THE CALLERS
                                  ; LINE # INSTEAD OF THE DEFINITIONS LINE #
    pop     hl                  
    ld      (VARPNT),hl           
    pop     hl                  
    ld      (FNPARM),hl           
    pop     hl                  
    ld      (VARNAM),hl           
    pop     hl                    ; GET BACK THE TEXT POINTER
    ret                      



; SUBROUTINE TO GET A POINTER TO A FUNCTION NAME
; 
get_fn_ptr: 
    rst     SYNCHR  
    byte    FNTK                  ; MUST START WITH "FN"
    ld      a,128                 ; DONT ALLOW AN ARRAY
    ld      (SUBFLG),a            ; DON'T RECOGNIZE THE "(" AS THE START OF AN ARRAY REFEREENCE
    or      (hl)                  ; PUT FUNCTION BIT ON
    ld      c,a                   ; GET FIRST CHARACTER INTO [C]
    call    PTRGT2          
    jp      CHKNUM          
