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
    cp      $5C                   ; Backslash
    jp      z,_escaped
    cp      LISTTK
    jp      z,eval_list
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
    call    string_addr_len ; Get Arg Length in C, Address in DE
    ld      a,c             ; A = String Length
    srl     a               ; Divide Length by 2
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
    cp      '.'             
    jr      nz,.not_dot     ; If dot
    add     '0'-'.'         ;   Convert to 0
.not_dot
    cp      ':'             ; Test for Digit 
    jr      nc,.not_digit   ; If A <= '9'
    sub     '0'             ;   Convert Digit to Binary
    jr      c,.fcerr        ;   If it was less than '0', Error
    ret                     ;   Else Return 
.not_digit
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
; Evaluate backslash escaped string
;-------------------------------------------------------------------------
;; Change to lookup tables should be smaller and faster
;; Proposed
;; \U CrsrUp, \D CrsrDn, \L CrsrLf, \R CrsrRt, \H Home, \E End, \G PgUp, \F PgDn
;; \I Insert, \J Delete, \M Menu, \K BckTab, \1 F1, ... \0 F10, \- F11, \= F12
;; \e $1B Escape, \l LineFeed
_escaped:
    inc     hl              ; Skip backslash
    ld      a,(hl)          ; 
    cp      '"'             ; If not followed by quotes
    jp      nz,SNERR        ;   Syntax error
    inc     hl              ; Skip quotes
    ex      de,hl           ; DE = TxtPtr
    call    get_strbuf_addr ; HL = StrBuf
    ex      de,hl           ; DE = StrBuf, HL = TxtPtr
.escape_loop
    ld      a,(hl)          ; A = NxtChr
    or      a               ; If EOL
    jr      z,.done         ;   Finish up
    inc     hl              ; Bump to NxtChr
    cp      '"'             ; If double quote
    jr      z,.done         ;   Finish up
    cp      $5C            
    jr      nz,.no_escape   ; If backslash
    ld      a,(hl)
    inc     hl              ;   Eat it
    cp      'a'             ;   If >= 'a'
    jr      c,.no_escape
    cp      'y'             ;   or <= 'x'
    jr      c,.sequence     ;     Interpret escape sequence
.no_escape
    ld      (de),a          ;   
    inc     de
    jr      .escape_loop
.sequence    
    ld      c,a             ; Save character
    ld      b,7             ; ^G
    sub     'a'             ;
    jr      z,.buff_it      ;   Ring the bell like Bob Dobbs
    inc     b               ; ^H
    dec     a               ; \b is for Backspace
    jr      z,.buff_it      ;
    ld      b,12            ; ^L
    sub     'f'-'b'         ; If \f
    jr      z,.buff_it      ;   Feed the form
    sub     'n'-'f'         ; If \n
    jr      z,.crlf         ;   It's a new line
    inc     b               ; ^M
    sub     'r'-'n'         ; If \r
    jr      z,.buff_it      ;   Make it clear screen
    ld      b,9             ; ^I
    dec     a               ; \s
    dec     a               ; If \t
    jr      z,.buff_it      ;   Just a step to the right
    ld      b,11            ; ^K
    dec     a               ; \u
    dec     a               ; If \v
    jr      z,.buff_it      ;   Make it clear
    dec     a               ; Wumbo
    dec     a               ; If \x
    jr      z,.hexit        ;   Do hex to ascii
    ld      b,c             ; No map - write character
.buff_it    
    ld      a,b             ; Write mapped character to buffer
    jr      .no_escape      ; and loop
.crlf
    ld      a,13            ; C/R
    ld      (de),a          ;  in the buffer
    inc     de
    ld      a,10            ; L/F 
    jr      .no_escape      ;  in and loop
.hexit
    call    get_hex
    sla     a               ; Shift to High Nybble
    sla     a
    sla     a
    sla     a
    ld      b,a
    call    get_hex
    or      b
    jr      .no_escape
.done:
    xor     a
    ld      (de),a          ; Terminate string
    push    hl              ; Save TxtPtr
    jp      return_strbuf

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
    byte      124
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

;;; ToDo: Give ERRMO or ERRTO if number of operands don't match.
;? "%%" % (1)
oper_stringsub:
    push    hl                  ; Stack = TxtPtr, RtnAdr
    call    get_strbuf_addr     ; HL = BufAdr
    ex      (sp),hl             ; HL = TxtPtr; Stack = BufPtr, RtnAdr
    call    GETYPE              ; If expression is not a string
    jp      nz,SNERR            ;   Syntax error
    rst     CHRGET              ; Skip %
    SYNCHK  '('                 ; Require (
    ex      (sp),hl             ; HL = BufPtr; Stack = TxtPtr, RtnAdr
    xor     a                   ; DatLen = 0
    push    af                  ; Stack = DatLen, TxtPtr, RtnAdr
    push    hl                  ; Stack = BufPtr, DatLen, TxtPtr, RtnAdr
    call    free_addr_len       ; DE = StrAdr, BC = StrLen
    ld      b,1                 ; B = ReqComma, C = StrLen
    pop     hl                  ; HL = BufPtr; Stack = DatLen, TxtPtr, RtnAdr
.loop:      
    inc     c                   ; Bump to Test at beginning of loop
    dec     c                   ; If not end of string
    jr      z,.done         
    ld      a,(de)              ;   A = StrChar
    inc     de                  ;   Bump StrPtr
    cp      '%'                     
    jr      nz,.copychar        ;   If substitution character        
    ex      af,af'                  
    dec     c                   ;     If character follows
    jr      z,.copychar              
    ld      a,(de)              ;       A = NextChar
    cp      '%'                 ;       If substitution character
    jr      z,.substitute       ;         Go substitute it
    ex      af,af'                  
.copychar:                      ;   
    ld      (hl),a              ;   Write A to StrBuf
    inc     hl                  ;   Bump StrPtr
    pop     af                  ;   A = DatLen; Stack = BufPtr, RtnAdr
    inc     a                   ;   Bump DatLen
    jp      z,LSERR             ;   Error if > 255
    push    af                  ;   Stack = DatLen, TxtPtr, RtnAdr
    dec     c
    jr      .loop               ;   Check next character
.done:
    pop     af                    ; A = DatLen; Stack = TxtPtr, RtnAdr
    call    strbuf_temp_str       ; HL = StrDsc
    ex      (sp),hl               ; HL = TxtPtr; Stack = StrDsc, RtnAdr
    SYNCHK  ')'                   ; Require )
    ex      (sp),hl               ; HL = StrDsc; Stack = TxtPtr, RtnAdr
    call    FRETMP                ; Free the temporary
    jp      PUTNEW                ; and Return it

.substitute:
    pop     af                  ; A = DatLen; Stack = TxtPtr, RtnAdr
    ex      (sp),hl             ; HL = TxtPtr; Stack = BufPtr, RtnAdr
    push    af                  ; Stack = DatLen, BufPtr, RtnAdr
    inc     de                  ; Skip second substitution character
    dec     c
    dec     b                   ; Update ReqComma
    jr      z,.nocomma          ; If not first arg
    SYNCHK  ','                 ;   Require comma
.nocomma:   
    push    de                  ; Stack = StrPtr, DatLen, BufPtr, RtnAdr
    push    bc                  ; Stack = ReqCnt, StrPtr, DatLen, BufPtr, RtnAdr
    call    FRMEVL              ; Evaluate argument
    pop     bc                  ; BC = ReqCnt, Stack = StrPtr, DatLen, BufPtr, RtnAdr
    pop     de                  ; DE = StrPtr; Stack = DatLen, BufPtr, RtnAdr
    pop     af                  ; A = DatLen; Stack = BufPtr, RtnAdr
    ex      (sp),hl             ; HL = BufPtr; Stack = TxtPtr, RtnAdr
    push    bc                  ; Stack = ReqCnt, TxtPtr, RtnAdr
    push    de                  ; Stack = StrPtr, ReqCnt, TxtPtr, RtnAdr
    push    af                  ; Stack = DatLen, StrPtr, ReqCnt, TxtPtr, RtnAdr
    push    hl                  ; Stack = BufPtr, DatLen, StrPtr, ReqCnt, TxtPtr, RtnAdr
    call    GETYPE
    jr      z,.notnum           ; If numeric
    call    FOUT                ;   Convert to text
    call    STRLIT              ;   Create Temp String
.notnum                         ; 
    call    FRETMS
    call    free_addr_len       ; DE = ArgAdr, BC = ArgLen
.spaces
    ld      a,b                 ; Remove leading space from arg string
    or      c
    jr      z,.notspace
    ld      a,(de)
    cp      ' '
    jr      nz,.notspace
    inc     de
    dec     bc
    jr      .spaces
.notspace
    pop     hl                  ; HL = BufPtr; Stack = DatLen, StrPtr, ReqCnt, TxtPtr, RtnAdr
    pop     af                  ; A = DatLen; Stack = StrPtr, ReqCnt, TxtPtr, RtnAdr
    ld      b,c                 ; B = ArgLen
    ld      c,a                 ; C = DatLen
.copy
    ld      a,(de)              ; Copy from arg to buffer
    ld      (hl),a
    inc     hl                  ; Bump BufPtr
    inc     de                  ; Bump ArgPtr
    inc     c                   ; Bump DatLen
    jp      z,LSERR             ; Error if > 255
    djnz    .copy               ; Do next arg character
; Return with BC = ReqCnt, DE = StrPtr, HL = BufPtr; Stack = DatLen, TxtPtr, RtnAdr    
    ld      a,c                 ; A = DatLen
    pop     de                  ; DE = StrPtr; Stack = ReqCnt, TxtPtr, RtnAdr
    pop     bc                  ; BC = ReqCnt; Stack = TxtPtr, RtnAdr
    push    af                  ; Stack = DatLen, TxtPtr, RtnAdr
    jr      .loop               ; Do next StrChr
    
    
;-----------------------------------------------------------------------------
; LIST$(line#) - Detozenize Line line#
; LIST(NEXT) - Detozenize following Line
;-----------------------------------------------------------------------------
eval_list:
    rst     CHRGET                ; Skip LIST
    SYNCHK  '$'                   ; Require $
    SYNCHK  '('                   ; Requite (
    cp      NEXTK
    jr      nz,.not_next          ; If NEXT 
    rst     CHRGET                ;   Skip NEXT
    push    hl                    ;   Stack = TxtPtr, RtnAdr
    call    REM                   ;   HL = End of BASIC line
    inc     hl                    ;   HL = LinLnk of next line
    ex      (sp),hl               ;   HL = TxtPtr; Stack = LinLnk, RtnAdr
    SYNCHK  ')'                   ;   Require )
    ex      (sp),hl               ;   HL = LinLnk; Stack = TxtPtr, RtnAdr
    push    hl                    ;   Stack = LinLnk, TxtPtr, RtnAdr
    ld      a,(hl)
    inc     hl
    or      (hl)                  ;   If end of program
    jp      z,USERR               ;     Undefined line error
    jr      .list_line            ; Else
.not_next
    call    GETINT                ;   DE = LinNum
    SYNCHK  ')'                   ;   Require )
    push    hl                    ;   Stack = TxtPtr, RtnAdr
    call    FNDLIN                ;   BC = LinLnk
    jp      nc,USERR              ;   No Carry = Line not found
    push    bc                    ;   Stack = LinLnk, TxtPtr, RtnAdr
.list_line
    call    get_strbuf_addr       ; HL = StrBuf
    ex      de,hl                 ; DE = StrBuf
    pop     hl                    ; HL = LinLnk; Stack = TxtPtr, RtnAdr
    inc     hl
    inc     hl                    ; Skip Line Link
    inc     hl
    inc     hl                    ; Skip Line Number
    call    unpack_line           ; Unpack the line
return_strbuf:
    call    get_strbuf_addr       ; HL = StrBuf
    push    hl                    ; Push Dummy Return Address
return_strlit:
    dec     hl                    ; Back up to before string
    ld      b,0                   ; NUL is the only teminator
    call    STRLT3            
    jp      TIMSTF                ; Return temporary string

