;====================================================================
; Statements and Functions from Aquarius Extended BASIC
;====================================================================

;-----------------------------------------------------------------------------
; CLEAR statement extension (Hook 11)
; CLEAR *array
; CLEAR BITMAP [fgcolor, bgcolor]
; CLEAR CURSOR
;-----------------------------------------------------------------------------
; CLEAR CURSOR:PAUSE
clear_hook:
    jp      z,CLEARC              ; If no operands just CLEAR
    cp      BITTK                 
    jp      z,ST_CLEAR_BITMAP
    cp      MULTK                 ; 
    jr      z,ST_CLEAR_ARRAY
    cp      XTOKEN                ; If not extended token
    jp      nz,HOOK11+1           ;   Continue with CLEAR
    inc     hl                    ; Skip XTOKEN
_clear_cursor
    ld      iy,screen_clear_cursor
    rst     SYNCHR
    byte    CURTK
    SYNCHK  'S'                   
    rst     SYNCHR                ; Require CURSOR
    byte    ORTK
    jp      gfx_call_preserve_hl

aux_call_preserve_hl:
    push    hl
aux_call_popret:
    call    aux_call
    pop     hl

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

print_hook:
    cp      '@'       
    jr      nz,.not_at            ; If '@'
    rst     CHRGET                ;   Skip '@'
    SYNCHK  '('                   ;   Require open paren
    call    ST_LOCATE             ;    Do LOCATE
    SYNCHK  ')'                   ;   Rwquire close paren
    jr      .done
.not_at:
    call    CHRGT2
.done
    jp      HOOK6+1

;; ToDo: Add ELSE to IF THEN
; This routine is jumped to after a THEN if the expression is FALSE
then_hook:
    jp      REM
    ld      iy,scan_else
    call    aux_call              ; Scan else
    ret     z                     ; If EOL, return to NEWSTT
    jp      GONE3                 ; Else execute statement after ELSE

;-----------------------------------------------------------------------------
; ATN function
;-----------------------------------------------------------------------------
FN_ATN:
    rst     FSIGN                 ; SEE IF ARG IS NEGATIVE
    call    m,PSHNEG              ; IF ARG IS NEGATIVE, USE:
    call    m,NEG                 ;     ARCTAN(X)=-ARCTAN(-X)
    ld      a,(FAC)               ; SEE IF FAC .GT. 1
    cp      129               
    jp      c,ATN2          

    ld      bc,$8100              ; GET THE CONSTANT 1
    ld      d,c                
    ld      e,c                   ; COMPUTE RECIPROCAL TO USE THE IDENTITY:
    call    FDIV                  ;    ARCTAN(X)=PI/2-ARCTAN(1/X)
    ld      hl,FSUBS              ; PUT FSUBS ON THE STACK SO WE WILL RETURN       
    push    hl                    ;   TO IT AND SUBTRACT THE REULT FROM PI/2    
ATN2:   
    ld      hl,ATNCON             ; EVALUATE APPROXIMATION POLYNOMIAL
  
    call    POLYX              
    ld      hl,PI2                ; GET POINTER TO PI/2 IN CASE WE HAVE TO
    ret                           ;   SUBTRACT THE RESULT FROM PI/2
  
;CONSTANTS FOR ATN  
ATNCON: 
    db      9                     ;DEGREE
    db      $4A,$D7,$3B,$78       ; .002866226
    db      $02,$6E,$84,$7B       ; -.01616574
    db      $FE,$C1,$2F,$7C       ; .04290961
    db      $74,$31,$9A,$7D       ; -.07528964
    db      $84,$3D,$5A,$7D       ; .1065626
    db      $C8,$7F,$91,$7E       ; -.142089
    db      $E4,$BB,$4C,$7E       ; .1999355
    db      $6C,$AA,$AA,$7F       ; -.3333315
    db      $00,$00,$00,$81       ; 1.0

;-----------------------------------------------------------------------------
; CLS statement 
; syntax: CLS [fgcolor, bgcolor]
;-----------------------------------------------------------------------------
ST_CLS:
    jp      z,do_cls              ; No parameters, use default
    cp      COLTK
    jr      z,.cls_color
    call    get_screen_colors     ; Pares foreground and background colors
    jp      clear_home            ; Clear screen and homecursor
.cls_color:
    rst     CHRGET                ; Skip COL
    rst     SYNCHR                
    byte    ORTK                  ; Require COLOR    
    ld      iy,screen_clear_color
    jp      z,gfx_call_preserve_hl
    ld      iy,screen_clear_color_a
    call    get_screen_colors
    jp      gfx_call_preserve_hl

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
    ld      bc,USRADD               ; DefAdr = USR() 
    cp      INTTK
    jr      nz,.notint              ; If USRINT
    rst     CHRGET                  ; 
    ld      bc,BASINTADR            ;   DefAdr = User Interrupt
.notint
    push    bc                      ; Stack = DefAdr, RtnAdr
    rst     SYNCHR
    byte    EQUATK                  ; Require =
    call    GETINT                  ; DE = UsrAdr
    ex      (sp),hl                 ; HL = DefAdr; Stack = TxrPtr, RtnAdr
    ld      (hl),e
    inc     hl
    ld      (hl),d                  ; (DefAdr) = UsrAdr
    pop     hl                      ; HL = TxtPtr; Stack = RtnAdr
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
