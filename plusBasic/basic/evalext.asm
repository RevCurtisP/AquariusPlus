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
    call    VALNUM                ; ASSUME VALUE WILL BE NUMERIC
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
    cp      '%'
    jp      z,eval_binary
    cp      ENDTK
    jp      z,eval_end
    cp      DIMTK
    jp      z,eval_dim
    cp      LISTK
    jp      z,eval_list
    cp      PLUSTK                ; IGNORE "+"
    jp      z,eval_extension      ;
    jp      QDOT

.eval_hex:
    inc     hl                    ; Bump Text Pointer
    ld      a,(hl)                ; Get Next Character
    dec     hl                    ; and Move Back
    cp      '"'                   ; If Not a Quote
    jp      nz,eval_hex_long      ;   Convert HEX Number
    inc     hl                    ; Bump Again
eval_hex_str:
    call    STRLTI                ; Get String Literal
    push    hl                    ; Save Text Pointer
    jp      hex_to_asc            ; Convert to ASCII

fin_extokens:
    ld      a,(hl)
    cp      MINUTK                ; Special token handling
    push    af
    jp      z,FIN1
    cp      PLUSTK
    jp      z,FIN1
    pop     af
fin_extension:
    ld      a,(hl)
    cp      '$'                   ; If not '$'
    jp      nz,FIN                ;   Evaluate float
eval_hex_long:
    call    VALNUM            ; Returning Number
    ld      b,a               ; Parse up to 255 characters
eval_hex:
    ld      c,a
    ld      d,a
    ld      e,a               ; DE is the parsed Integer
.hex_loop:
    dec     b
    jp      z,FLOAT_CDE       ; Last Character - float it
    rst     CHRGET
    jp      z,FLOAT_CDE       ; End of Line - float it
    jr      c,.dec_digit      ; Decimal Digit - process it
    cp      CDTK              ; If CD token
    jr      z,.hex_cdtoken    ;   Handle it
    and     $DF               ; Convert to Upper Case
    cp      'A'               ; If < 'A'
    jp      c,FLOAT_CDE       ;   Not Hex Digit - float it
    cp      'G'               ; If > 'F'
    jp      nc,FLOAT_CDE      ;   Not Hex Digit - float it
    sub     'A'-':'           ; Make 'A' come after '9'
.dec_digit:
    sub     '0'               ; Convert Hex Digit to Binary
    ex      af,af'            ; Save HexVal
    ld      a,4               ; Shift DE Left 4 bits
.sla_loop
    sla     e
    rl      d
    rl      c
    jp      c,OVERR           ;   Overflow!
    dec     a
    jr      nz,.sla_loop
    ex      af,af'            ; Restore HexVal
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
    call    faclo_addr_len        ; BC = ArgLen, DE = ArgAdr, HL = ArgDsc
    ld      a,c                   ; A = ArgLen
    srl     a                     ; NewLen = ArgLen / 2
    jp      c,SLERR               ; String length error if ArgLen was Odd
    jr      z,push_null_string    ; If NewLen = 0 Return Null String
    push    hl                    ; Stack = ArgDsc, RtnAdr
    push    bc                    ; Stack = ArgLen, ArgDsc, RtnAdr
    push    de                    ; Stack = ArgAdr, NewLen, ArgDsc, RtnAdr
    call    STRINI                ; DE = NewAdr
    pop     hl                    ; HL = ArgAdr; Stack = NewLen, ArgDsc, RtnAdr
    pop     bc                    ; A = ArgLen; Stack = ArgDsc, RtnAdr
    ex      de,hl                 ; DE = ArgAdr, HL = NewAdr
    ld      iy,aux_hex_to_asc
    call    aux_call
    jp      c,FCERR
    jp      PFRNEW                ; Pop ArgDsc, Free It, and Return NewStr

pop_null_string:
    pop     bc
    byte    $06                   ; LD B, over PUSH BC
push_null_string:
    push    bc                    ; Put Dummy Return Address on Stack
null_string:
    ld      hl,REDDY-1            ; Point at ASCII 0
    jp      TIMSTR                ; Literalize and Return It

;-------------------------------------------------------------------------
; Evaluate numeric character constant in the form of 'C'
;-------------------------------------------------------------------------
eval_ascii:
    inc     hl                    ; Skip single quote
    ld      c,(hl)                ; Get character
    inc     hl                    ; Move on
    SYNCHKT $27                   ; Require single quote
    ld      a,c                   ; A = character
    push    hl                    ; Save text pointer
    call    SNGFLT                ; Float it
    pop     hl
    ret

;-------------------------------------------------------------------------
; Evaluate binary numeric constant in the form of %xxxxxx
;-------------------------------------------------------------------------
eval_binary:
    ld      de,0                  ; LngVal = 0
    ld      c,0
.loop
    rst     CHRGET                ; A = Digit
    jp      nc,FLOAT_CDE          ; If not a digit, float number and return
    sub     '0'                   ; Convert digit to binary
    sra     a                     ; Carry Set if 1, Clear if 0
    jp      nz,FCERR              ; Illegal quantity error if Digit > 1
    rl      e
    rl      d
    rl      c                     ; Roll bit into LngVal
    jp      m,OVERR               ; Overflow error if more than 23 bits
    jr      .loop

;-------------------------------------------------------------------------
; Evaluate backslash escaped string
;-------------------------------------------------------------------------
;; Change to lookup tables should be smaller and faster
;; Proposed
;; \U CrsrUp, \D CrsrDn, \L CrsrLf, \R CrsrRt, \H Home, \E End, \G PgUp, \F PgDn
;; \I Insert, \J Delete, \M Menu, \K BckTab, \1 F1, ... \0 F10, \- F11, \= F12
;; \e $1B Escape, \l LineFeed
_escaped:
    inc     hl                    ; Skip backslash
    ld      a,(hl)                ;
    cp      '"'                   ; If not followed by quotes
    jp      nz,SNERR              ;   Syntax error
    inc     hl                    ; Skip quotes
    ex      de,hl                 ; DE = TxtPtr
    call    alloc_temp_buffer     ; HL = StrBuf
    ex      de,hl                 ; DE = StrBuf, HL = TxtPtr
    ld      iy,aux_escaped_string 
    call    aux_call              
    ld      de,free_temp_buffer
    push    de                    ; Stack = FunRtn, RtnAdr
    push    hl                    ; Stack = TxtPtr, FunRtn, RtnAdr
    call    tmpbuf_temp_string    ; HL = StrDsc
    jp      FRENEW                ; Free ArgDsc and return NewStr

;-------------------------------------------------------------------------
; RETAOP Extension - Hook 29
; String Substitution: "xx%xx%xx..." % (formula, formula, ...)
; Machine State:
;     A: Possible Operator
;    HL: Text Pointer
; TEMP3: Saved Text Pointer
;-------------------------------------------------------------------------
; ToDo: Add IMP(lowest precedence booleban operator)
oper_extension:
    cp        '%'                 ; If string sub operator
    jr        z,oper_stringsub    ;   Go do it
    cp        MODTK
    jr        nz,.notmod
    ld        hl,optab_mod        ;   HL = OPTAB entry
    jp        EVALOP              ;   Do the operator
.notmod
    cp        XORTK
    jr        nz,.notxor          ;
    ld        hl,optab_xor        ;   HL = OPTAB entry
    jp        EVALOP              ;   Do the operator
.notxor
    cp      PLUSTK            ;[M80] IS IT AN OPERATOR?
    ret     c                 ;
    jp        CHKLTK

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
    or      $01                     ; Clears Z flag for XOR
    jp      DANDOR
    
xor_check:
    jp      z,ISAND
    jp      m,NOTAND                ; Do OR
    xor     e
    ld      c,a
    ld      a,b
    xor     d
    jp      (hl)                  ;[M80] RETURN THE INTEGER [A,L]

;? "%%" % (1)
oper_stringsub:
    push    bc                  ; Save Old Precedence
    ld      bc,TSTOP
    push    bc                  ; Return to TSTOP
    ld      bc,free_temp_buffer ; After freeing temp buffer
    push    bc                  ; Stack = FunRtn, RtnAdr
    ld      de,(FACLO)          ; DE = ArgDsc
    push    de                  ; Stack = ArgDsc, FunRtn, RtnAdr
    push    hl                  ; Stack = TxtPtr, ArgDsc, FunRtn, RtnAdr
    call    alloc_temp_buffer   ; HL = BufAdr
    ex      (sp),hl             ; HL = TxtPtr; Stack = BufPtr, FunRtn, RtnAdr
    call    GETYPE              ; If expression is not a string
    jp      nz,TMERR            ;   Type mismatch error
    rst     CHRGET              ; Skip %
    SYNCHKC '('                 ; Require (
    ex      (sp),hl             ; HL = BufPtr; Stack = TxtPtr, ArgDsc, FunRtn, RtnAdr
    xor     a                   ; DatLen = 0
    push    af                  ; Stack = DatLen, TxtPtr, ArgDsc, FunRtn, RtnAdr
    push    hl                  ; Stack = BufPtr, DatLen, TxtPtr, ArgDsc, FunRtn, RtnAdr
    ex      de,hl               ; HL = ArgDsc
    call    string_addr_len     ; DE = ArgAdr, BC = ArgLen
    ld      b,1                 ; B = ReqComma, C = ArgLen
    pop     hl                  ; HL = BufPtr; Stack = DatLen, TxtPtr, ArgDsc, FunRtn, RtnAdr
.loop:
    inc     c                   ; Bump to Test at beginning of loop
    dec     c                   ; If not end of string
    jr      z,.done
    ld      a,(de)              ;   A = ArgChr
    inc     de                  ;   Bump ArgPtr
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
    inc     hl                  ;   Bump BufPtr
    pop     af                  ;   A = DatLen; Stack = TxtPtr, ArgDsc, FunRtn, RtnAdr
    inc     a                   ;   Bump DatLen
    jp      z,LSERR             ;   Error if > 255
    push    af                  ;   Stack = DatLen, TxtPtr, ArgDsc, FunRtn, RtnAdr
    dec     c
    jr      .loop               ;   Check next character
.done:
    pop     af                    ; A = DatLen; Stack = TxtPtr, ArgDsc, FunRtn, RtnAdr
    pop     hl                    ; HL = TxtPtr; Stack = ArgDsc, FunRtn, RtnAdr
    ex      (sp),hl               ; HL = ArgDsc; Stack = TxtPtr, FunRtn, RtnAdr
    push    af                    ; Stack = DatLen, TxtPtr, FunRtn, RtnAdr
    call    FRETM2                ; Free argument string
    pop     af                    ; AF = DatLen; Stack = TxtPtr, FunRtn, RtnAdr
    call    tmpbuf_temp_string    ; HL = TmpDsc
    ex      (sp),hl               ; HL = TxtPtr; Stack = ArgDsc, FunRtn, RtnAdr
    ld      a,(hl)                ; A = NxtChr
    cp      ','                   ; If it's a comma
    jp      z,no_more             ;   Too many operands
    SYNCHKC ')'                   ; Require )
    ex      (sp),hl               ; HL = ArgDsc; Stack = TxtPtr, FunRtn, RtnAdr
    ex      de,hl                 ; DE = ArgDsc
    jp      FRENEW                ; Free ArgDsc and return NewStr

.substitute:
    pop     af                  ; A = DatLen; Stack = TxtPtr, FunRtn, RtnAdr
    ex      (sp),hl             ; HL = TxtPtr; Stack = BufPtr, FunRtn, RtnAdr
    push    af                  ; Stack = DatLen, BufPtr, FunRtn, RtnAdr
    inc     de                  ; Skip second substitution character
    dec     c
    dec     b                   ; Update ReqComma
    call    nz,get_comma        ; If not first arg and no comma, Missing operand error
    push    de                  ; Stack = StrPtr, DatLen, BufPtr, FunRtn, RtnAdr
    push    bc                  ; Stack = ReqCnt, StrPtr, DatLen, BufPtr, FunRtn, RtnAdr
    call    FRMEVL              ; Evaluate argument
    pop     bc                  ; BC = ReqCnt, Stack = StrPtr, DatLen, BufPtr, FunRtn, RtnAdr
    pop     de                  ; DE = StrPtr; Stack = DatLen, BufPtr, FunRtn, RtnAdr
    pop     af                  ; A = DatLen; Stack = BufPtr, FunRtn, RtnAdr
    ex      (sp),hl             ; HL = BufPtr; Stack = TxtPtr, FunRtn, RtnAdr
    push    bc                  ; Stack = ReqCnt, TxtPtr, FunRtn, RtnAdr
    push    de                  ; Stack = StrPtr, ReqCnt, TxtPtr, FunRtn, RtnAdr
    push    af                  ; Stack = DatLen, StrPtr, ReqCnt, TxtPtr, FunRtn, RtnAdr
    push    hl                  ; Stack = BufPtr, DatLen, StrPtr, ReqCnt, TxtPtr, FunRtn, RtnAdr
    call    GETYPE
    jr      z,.notnum           ; If numeric
    call    FOUT                ;   Convert to text
    call    STRLIT              ;   Create Temp String
.notnum                         ;
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
    pop     hl                  ; HL = BufPtr; Stack = DatLen, StrPtr, ReqCnt, TxtPtr, FunRtn, RtnAdr
    pop     af                  ; A = DatLen; Stack = StrPtr, ReqCnt, TxtPtr, FunRtn, RtnAdr
    ld      b,c                 ; B = ArgLen
    ld      c,a                 ; C = DatLen
    ld      a,b                 ; A = ArgLen
    or      a                   
    jp      z,.empty            ; If ArgLen > 0
.copy
    ld      a,(de)              ; Copy from arg to buffer
    ld      (hl),a
    inc     hl                  ; Bump BufPtr
    inc     de                  ; Bump ArgPtr
    inc     c                   ; Bump DatLen
    jp      z,LSERR             ; Error if > 255
    djnz    .copy               ; Do next arg character
.empty
; Return with BC = ReqCnt, DE = StrPtr, HL = BufPtr; Stack = DatLen, TxtPtr, FunRtn, RtnAdr
    ld      a,c                 ; A = DatLen
    pop     de                  ; DE = StrPtr; Stack = ReqCnt, TxtPtr, FunRtn, RtnAdr
    pop     bc                  ; BC = ReqCnt; Stack = TxtPtr, FunRtn, RtnAdr
    push    af                  ; Stack = DatLen, TxtPtr, FunRtn, RtnAdr
    jp      .loop               ; Do next StrChr

; clear:s$="abcd":s$[2]="x":?s$
; clear:s$="abcd":s$[1,1]="x":?s$
; s$="abc":s$[3]="z"
; S$="abcde"
; S$[2]="x"
; ? S$
; S$[3 TO 4]="yz"
; ? S$
; 10 S$="abc":S$[2]="*":LIST
; Errors
; s$="abcd":s$[0]="x"
; s$="abcd":s$[5]="x"

; On Entry: DE = VarPtr, HL = TxtPtr
let_extension:
    call    PTRTYP                ; Get pointer to variable
    jp      nz,LETEQ              ; If not string variable  Continue LET
    ld      a,(hl)
    cp      '['                   ; If not followed by '['
    jp      nz,LETEQ              ;   Continue LET
    rst     CHRGET                ; Skip '['
    ld      iy,copy_literal_string
    call    aux_call              ; If literal string, copy to string space
    push    de                    ; Stack = VarPtr, RtnAdr
    call    get_substring_range   ; DE = StrAdr, A,BC = StrLen, H = FrmPos, L = ToPos; Stack = TxtPtr, RtnAdr
    cp      h                     ; If FrmPos > StrLen
    jp      c,BRERR               ;   Bad range error
    ex      (sp),hl               ; HL = TxtPtr; Stack = FrmTo, RtnAdr
    SYNCHKT EQUATK                ; Require =
    ex      (sp),hl               ; H = FrmPos, L = ToPos; Stack = TxtPtr, RtnAdr
    push    de                    ; Stack = StrAdr, TxtPtr, RtnAdr
    call    range_offset_len      ; DE = DstOfs, A = DstLen
    pop     hl                    ; HL = StrAdr; Stack = TxtPtr, RtnAdr
    jp      c,NULRT               ; If SubLen > StrLen, return ""
    add     hl,de                 ; HL = SubAdr
    ex      (sp),hl               ; HL = TxtPtr; Stack = DstAdr, RtnAdr
    push    af                    ; Stack = DstLen, DstAdr, RtnAdr
    call    FRMEVL                ; Evaluate Formula
    pop     af                    ; A = DstLen; Stack = DstAdr, RtnAdr
    ex      (sp),hl               ; HL = DstAdr; Stack = TxtPtr, RtnAdr
    push    hl                    ; Stack = DstAdr, TxtPtr, RtnAdr
    push    af                    ; Stack = DstLen, SubAdr, TxtPtr, RtnAdr
    call    free_addr_len         ; DE = ScrAdr, BC = SrcLen
    jp      z,pop3hl_ret          ; If "", just return
    pop     af                    ; A = DstLen; Stack = DstAdr, TxtPtr, RtnAdr
    cp      c                     
    jr      nc,.skip              ; If SrcLen > DstLen
    ld      c,a                   ;   SrcLen = DstLen
.skip
    pop     hl                    ; HL = DstAdr; Stack = TxtPtr, RtnAdr
    ex      de,hl                 ; HL = SrcAdr, DE = DstAdr
    ldir                          ; Copy into string variable
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret
    
pop3hl_ret:    
    pop     hl                    ; HL = SubLen; Stack = SubAdr, TxtPtr, RtnAdr
pop2hl_ret:
    pop     hl                    ; HL = SubAdr; Stack = TxtPtr, RtnAdr
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

; var$[p] evaluates to character p of var$
; var$[f TO t] evaluates to characters f through t of var$
isvar_extension:
    call    PTRGET                ; Parse variable name
    ld      a,(VALTYP)
    or      a                     ; If not string variable
    jp      z,RETVAR              ;   Return it
    ld      a,(hl)
    cp      '['                   ; If not followed by [
    jp      nz,RETVAR             ;   Return it
    rst     CHRGET                ; Skip [
; Get substring
    push    de                    ; Stack = VarPtr, RtnAdr
    call    get_substring_range   ; DE = StrAdr, A,BC = StrLen, H = FrmPos, L = ToPos; Stack = TxtPtr, RtnAdr
    jp      z,NULRT               ; If StrLen = 0, return ""
    push    de                    ; Stack = StrAdr, TxtPtr, RtnAdr
    call    range_offset_len      ; DE = SubOfs, A = SubLen
    pop     hl                    ; HL = StrAdr; Stack = TxtPtr, RtnAdr
    jp      c,NULRT               ; If SubLen > StrLen, return ""
    add     hl,de                 ; HL = SubAdr
    push    hl                    ; Stack = SubAdr, TxtPtr, RtnAdr
    call    STRINI
    pop     hl                    ; HL = SubAdr; Stack = TxtPtr, RtnAdr
    ld      c,a
    ld      b,0
    ldir
    jp      PUTNEW                ; Make temporary and put in FACLO

get_substring_range:
    call    get_byte_range        ; A,D = FrmPos, E = ToPos
    pop     ix                    ; IX = RtnAdr
    or      a
    jp      z,BRERR               ; If FrmPos = 0, Bad range error
    SYNCHKC ']'                   ; Require ]
    ex      (sp),hl               ; HL = VarPtr; Stack = TxtPtr
    push    de                    ; Stack = FrmTo, TxtPtr
    call    string_addr_len       ; DE = StrAdr, A,BC = StrLen
    pop     hl                    ; H = FrmPos, L = ToPos; Stack = TxtPtr
    jp      (ix)                  ; Return to caller

    
; Input: C = Max, H = From, L = To
; Output: A = Length, DE = Offset
; Sets carry if out of range
range_offset_len:
    ld      a,c                   ; A = Len
    cp      h                     ; If Len < From
    ret     c                     ;   Return Carry Set
    cp      l
    jr      nc,.skip              ; If Len < To
    ld      l,a                   ;   To = Len
.skip
    ex      de,hl                 ; D = From, E = To
    dec     d                     ; Offset = From - 1
    ld      a,e                   ; E = To
    sub     a,d                   ; A = Length
    ld      e,d
    ld      d,0                   ; DE = Offset
    ret

;-----------------------------------------------------------------------------
; DIMS() - Get Array Dimensions
; DIMS(*array) - number of dimensions
; DIMS(*array,dimension) - size of dimension
;-----------------------------------------------------------------------------
; DIM A(9):? DIM(*A),DIM(*A,1)
; DIM B$(3,2):? DIM(*B$),DIM(*B$,1);DIM(*B$,2)
eval_dim:
    rst     CHRGET                ; Skip DIM
    call    get_par_array_pointer ; BC = CntPtr
    ld      a,(bc)                ; A = DimCnt
    push    bc                    ; Stack = DimCnt, RtnAdr
    push    af                    ; Stack = CntPtr, DimCnt, RtnAdr
    ld      e,0                   ; DimNum = 0
    ld      a,(hl)
    cp      ','                   ; If Comma
    jr      nz,.no_dimnum
    call    skip_get_byte         ; E = DimNum
.no_dimnum
    SYNCHKC ')'                   ; No comma for now
    ld      a,e                   ; A = DimNum
    or      a                     
    jr      nz,.dimsize           ; If DimNum = 0
    pop     af                    ;   A = DimCnt; Stack = CntPtr, RtnAdr
    pop     bc                    ;   BC = CntPtr; Stack = RtnAdr
    jp      float_byte            ;   Float A
.dimsize
    pop     af                    ; A = DimCnt
    ex      (sp),hl               ; HL = CntPtr; Stack = TxtPtr, RtnAdr
    sub     e                     ; DimPos = DimCnt - DimNum
    jp      c,FCERR               ; If DimNum > DimCnt  Illegal quantity error
    ld      b,a                   ; B = DimNum
    inc     b
    dec     hl                    ; Back up for Skip
.dimloop
    inc     hl
    inc     hl                    ; HL = NxtDim
    djnz    .dimloop
    ld      e,(hl)
    inc     hl
    ld      d,(hl)                ; DE = DimSiz
    dec     de                    ; DE = LstIdx
float_de_pophrt:
    call    FLOAT_DE
    pop     hl
    ret

;-----------------------------------------------------------------------------
; ENDKEY - Return key that ended INPUT enhanced input
;-----------------------------------------------------------------------------
; ? ENDKEY
eval_end:
    rst     CHRGET                ; Skip End
    SYNCHKT XTOKEN
    SYNCHKT KEYTK                ; Require KEY
    ld      a,(IEND_KEY)
    jp      float_byte

;-----------------------------------------------------------------------------
; LIST$(line#) - Detokenize Line line#
; LIST(NEXT) - Detokenize following Line
;-----------------------------------------------------------------------------
;;; ToDo: Integrate push free_temp_buffer into return_tmpbuf, modify routines that jump to it
eval_list:
    call    skip_dollar_paren     ; Require $(
    ld      bc,free_temp_buffer
    push    bc                    ; Stack = FunRtn, RtnAdr
    cp      NEXTK
    jr      nz,.not_next          ; If NEXT
    rst     CHRGET                ;   Skip NEXT
    push    hl                    ;   Stack = TxtPtr, FunRtn, RtnAdr
    call    REM                   ;   HL = End of BASIC line
    inc     hl                    ;   HL = LinLnk of next line
    ex      (sp),hl               ;   HL = TxtPtr; Stack = LinLnk, FunRtn, RtnAdr
    SYNCHKC ')'                   ;   Require )
    ex      (sp),hl               ;   HL = LinLnk; Stack = TxtPtr, FunRtn, RtnAdr
    push    hl                    ;   Stack = LinLnk, TxtPtr, FunRtn, RtnAdr
    ld      a,(hl)
    inc     hl
    or      (hl)                  ;   If end of program
    jp      z,USERR               ;     Undefined line error
    jr      .list_line            ; Else
.not_next
    call    GETINT                ;   DE = LinNum
    SYNCHKC ')'                   ;   Require )
    push    hl                    ;   Stack = TxtPtr, FunRtn, RtnAdr
    call    FNDLIN                ;   BC = LinLnk
    jp      nc,USERR              ;   No Carry = Line not found
    push    bc                    ;   Stack = LinLnk, TxtPtr, FunRtn, RtnAdr
.list_line
    call    alloc_temp_buffer     ; HL = TmpBuf
    ex      de,hl                 ; DE = TmpBuf
    pop     hl                    ; HL = LinLnk; Stack = TxtPtr, FunRtn, RtnAdr
    inc     hl
    inc     hl                    ; Skip Line Link
    inc     hl
    inc     hl                    ; Skip Line Number
    call    unpack_line           ; Unpack the line
return_tmpbuf:
    call    get_temp_buffer       ; HL = TmpBuf
    push    hl                    ; Stack = Dummy, TxtPtr, FunRtn, RtnAdr
return_strlit:
    dec     hl                    ; Back up to before string
    ld      b,0                   ; NUL is the only teminator
    call    STRLT3
    jp      TIMSTF                ; Return temporary string

