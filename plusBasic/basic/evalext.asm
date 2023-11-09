;====================================================================
; Evaluation and Operator Extensions
;====================================================================

;-------------------------------------------------------------------------
; EVAL Extension - Hook 9
; Hexadecimal Integer Literal: $xxxx
; Hexadecimal String Literal: $"xxxx"
; Character Literal: 'x'
;-------------------------------------------------------------------------
eval_extension:

    xor     a                     ;
    ld      (VALTYP),a            ; ASSUME VALUE WILL BE NUMERIC
    rst     CHRGET                ;
    jp      z,MOERR               ; TEST FOR MISSING OPERAND - IF NONE GIVE ERROR
    jp      c,FIN                 ; IF NUMERIC, INTERPRET CONSTANT
    call    ISLETC                ; VARIABLE NAME?
    jp      nc,ISVAR              ; AN ALPHABETIC CHARACTER MEANS YES
    cp      '$'                       
    jr      z,.eval_hex     
    cp      $27                   ; Apostrophe                       
    jp      z,eval_ascii     
    cp      PLUSTK                ; IGNORE "+"
    jp      z,eval_extension      ;
    jp      QDOT     

.eval_hex:
    inc     hl                    ; Bump Text Pointer
    ld      a,(hl)                ; Get Next Character
    dec     hl                    ; and Move Back
    cp      '"'                   ; If Not a Quote
    jp      nz,eval_hex_int       ;   Convert HEX Number
    inc     hl                    ; Bump Again
eval_hex_str:     
    call    STRLTI                ; Get String Literal
    push    hl                    ; Save Text Pointer
    jp      hex_to_asc            ; Convert to ASCII

eval_hex_int:
    xor     a
    ld      (VALTYP),a        ; Returning Number
    ld      c,a               ; Parse up to 255 characters
_eval_hex:
    ld      d,a               
    ld      e,a               ; DE is the parsed Integer
.hex_loop:    
    dec     c
    jp      z,FLOAT_DE        ; Last Character - float it
    rst     CHRGET
    jp      z,FLOAT_DE        ; End of Line - float it
    jr      c,.dec_digit      ; Decimal Digit - process it
    cp      CDTK              ; If CD token
    jr      z,.hex_cdtoken    ;   Handle it
    and     $DF               ; Convert to Upper Case
    cp      'A'               ; If < 'A'
    jp      c,FLOAT_DE        ;   Not Hex Digit - float it
    cp      'G'               ; If > 'F'
    jp      nc,FLOAT_DE       ;   Not Hex Digit - float it
    sub     'A'-':'           ; Make 'A' come after '9'
.dec_digit:
    sub     '0'               ; Convert Hex Digit to Binary
    ld      b,4               ; Shift DE Left 4 bits
.sla_loop
    sla     e                
    rl      d
    jp      c,OVERR           ;   Overflow!
    djnz    .sla_loop
    or      e                 ; Put into low nybble
    ld      e,a
    jr      .hex_loop         ; Look for Next Hex Digit

.hex_cdtoken:
    ld      a,d               
    or      a                 ; If there's anything in the MSB
    jp      nz,OVERR          ;   Overflow!
    ld      d,e               ; Move LSB to MSB
    ld      e,$CD             ; Make LSB CD  
    jr      .hex_loop

hex_to_asc:
    ld      hl,(FACLO)      ; Get String Descriptor Address
    call    string_addr_len          ; Get Arg Length in C, Address in DE
    ld      a,c             ; A = String Length
    sra     a               ; Divide Length by 2
    jp      c,FCERR         ;   Error if Length was Odd
    jr      z,null_string   ;   If 0, Return Null String
    push    af              ; Save New String Length
    push    de              ; Save Argument String Address
    push    hl              ; Save Argument String Descriptor
    call    STRINI          ; Create Result String returning HL=Descriptor, DE=Text Address
    pop     de              ; Get Back Argument Descriptor
    call    FRESTR          ; and Free It
    ld      de,(DSCTMP+2)   ; Get Result String Text Address
    pop     hl              ; Get Argument String Address
    pop     af              ; Get New String Length
    ld      b,a             ; Loop Count = Result String Length
.asc_loop:
    call    get_hex         ; Get Hex Digit from Argument
    sla     a               ; Shift to High Nybble
    sla     a
    sla     a
    sla     a
    ld      c,a             ; and Save in C
    call    get_hex         ; Get Next Hex Digit from Argument
    or      c               ; Combine with High Nybble
    ld      (de),a          ; Store in Result String
    inc     de              ; Bump Result Pointer
    djnz    .asc_loop
    jp      PUTNEW          ; Return Result String

get_hex:
    ld      a,(hl)          ; Get Hex Digit 
    inc     hl              ; Bump Pointer
cvt_hex:
    cp      ':'             ; Test for Digit 
    jr      nc,.not_digit   ; If A <= '9'
    sub     '0'             ;   Convert Digit to Binary
    jr      c,.fcerr        ;   If it was less than '0', Error
    ret                     ;   Else Return 
.not_digit:
    and     $5F             ; Convert to Upper Case
    sub     'A'-10          ; Make 'A' = 10
    jr      c,.fcerr        ; Error if it was less than 'A'
    cp      16              ; If less than 16
    ret     c               ;   Return
.fcerr                      ; Else 
    jp      FCERR           ;   Error

null_string:
    ld      hl,REDDY-1      ; Point at ASCII 0 
    push    bc              ; Put Dummy Return Address on Stack
    jp      TIMSTR          ; Literalize and Return It

;-------------------------------------------------------------------------
; Evaluate numeric character constant in the form of 'C'
;-------------------------------------------------------------------------

eval_ascii:
    inc     hl              ; Skip single quote
    ld      c,(hl)          ; Get character
    inc     hl              ; Move on
    rst     SYNCHR          ; Require single quote
    byte    $27
    ld      a,c             ; A = character
    push    hl              ; Save text pointer
    call    SNGFLT          ; Float it
    pop     hl
    ret

;-------------------------------------------------------------------------
; RETAOP Extension - Hook 29
; String Substitution: "xx%xx%xx..." % (formula, formula, ...)
; Machine State:
;     A: Possible Operator
;    HL: Text Pointer
; TEMP3: Saved Text Pointer
;-------------------------------------------------------------------------
oper_extension:
    cp        '%'                 ; If string sub operator
    jr        z,oper_stringsub    ;   Go do it
    cp        MODTK
    jr        nz,.notmod
    ld        hl,optab_mod        ;   HL = OPTAB entry
    jp        EVALOP              ;   Do the operator
.notmod
    cp        XORTK
    jp        nz,HOOK29+1         ; 
    ld        hl,optab_xor        ;   HL = OPTAB entry
    jp        EVALOP              ;   Do the operator

optab_mod:
    byte      60
    word      oper_mod
optab_xor:
    byte      60
    word      oper_xor

; MOD operator: LeftArg - INT( LeftArg / RightArg ) * RightArg
oper_mod:
    pop     bc                    ; BCDE = LeftArg
    pop     de  
    push    de                    ; Stack = LeftArg
    push    bc
    ld      hl,(FACLO)            ; Stack = RightArg, LeftArg
    push    hl
    ld      hl,(FACHO)
    push    hl
    call    FDIV                  ; FACC = LeftArg/Right Arg
    call    INT                   ; FACC = INT(LeftArg/Right)
    pop     bc                    ; BCDE = RightArg; Stack = LeftArg
    pop     de
    call    FMULT                 ; FACC = RightArg * INT(LeftArg/Right)
    pop     bc                    ; BCDE = LeftArg
    pop     de  
    jp      FSUB                  ; Return LeftArg - RightArg * INT(LeftArg/Right)

; XOR operator - Copied from AND/OR
oper_xor:
    push    af                    ;[M80] SAVE THE PRECEDENCE or Operator...
    call    CHKNUM                ;[M65] MUST BE NUMBER
    call    FRCINT                ;COERCE RIGHT HAND ARGUMENT TO INTEGER
    pop     af                    ;GET BACK THE PRECEDENCE TO DISTINGUISH "AND" AND "OR"
    ex      de,hl             
    pop     bc                
    ex      (sp),hl           
    ex      de,hl             
    call    MOVFR             
    push    af                
    call    FRCINT            
    pop     af                
    pop     bc                
    ld      a,c               
    ld      hl,GIVINT             ;{M80} PLACE TO JUMP WHEN DONE
    xor     e                     
    ld      c,a                   
    ld      a,b                   
    xor     d                     
    jp      (hl)                  ;[M80] RETURN THE INTEGER [A,L]


oper_stringsub:
    call    GETYPE              ; If expression is not a string
    jp      nz,SNERR            ;   Syntax error
    rst     CHRGET              ; Skip %
    SYNCHK  '('                 ; Require (
    call    sbuff_init          ; Initialize string buffer
    push    hl                  ; Stack = TxtPtr
    call    FRESTR
    call    string_addr_len     ; HL = StrLen, DE = StrAdr, BC = StrLen
    ld      b,1                 ; B = ReqComma, C = StrLen
            
.loop:      
    inc     c                   ; Bump to Test at beginning of loop
    dec     c                   ; End of String
    jr      z,.done
    ld      a,(de)              ; A = StrChar
    inc     de                  ; Bump StrPtr
    cp      '%'                 
    jr      nz,.copychar        ; If substitution character        
    ex      af,af'
    dec     c                   ;   If character follows
    jr      z,.copychar            
    ld      a,(de)              ;     A = NextChar
    cp      '%'                 ;     If substitution character
    jr      z,.substitute       ;       Go substitute it
    ex      af,af'              
.copychar:                      ;
    call    sbuff_write_byte    ; Copy StrChar to Buffer
    dec     c
    jr      .loop
    
.done:
    pop     hl                  ; HL = TxtPtr 
    SYNCHK  ')'                 ; Require )
    push    hl                  ; Stack = TxtPtr
    call    sbuff_create_string ; Make temporary string from buffer
    call    FRESTR
    jp      PUTNEW              ; and Return it

.substitute:
    inc     de                  ; Skip second substitution character
    dec     c
    dec     b                   ; Update ReqComma
    pop     hl                  ; HL = TextPtr
    push    de                  ; Stack = StrPtr
    push    bc                  ; Stack = ReqComma+Counter, StrPtr
    jr      z,.nocomma          ; If not first arg
    SYNCHK  ','                 ;   Require comma
.nocomma:   
    call    FRMEVL              ; Evaluate argument
    push    hl                  ; Stack = TxtPtr, ReqComma+Counter, StrPtr
    call    GETYPE
    jr      z,.notnum           ; If numeric
    call    FOUT                ;   Convert to text
    call    STRLIT              ;   Create Temp String
.notnum                         ; 
    call    FRETMS
    call    free_addr_len       ; DE = ArgStr Addr, BC = ArgStr Length
.spaces
    ld      a,b
    or      c
    jr      z,.notspace
    ld      a,(de)
    cp      ' '
    jr      nz,.notspace
    inc     de
    dec     bc
    jr      .spaces
.notspace
    call    sbuff_write_bytes   ; Write it to StrBuff
    pop     hl                  ; HL = TxtPtr; Stack = ReqComma+Counter, StrPtr
    pop     bc                  ; B = ReqComma, C = Counter; Stack = StrPtr
    pop     de                  ; DE = StrPtr
    push    hl                  ; HL = TextPtr
    jp      c,OVERR             ; Error if Overflow
    jr      .loop               ; Do next StrChr