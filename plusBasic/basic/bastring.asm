;-----------------------------------------------------------------------------
; bastring.asm - plusBASIC string handling statements and functions
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; PAD$ function
;-----------------------------------------------------------------------------
;; ToDo: Debug PAD$() overwriting string variable text
; ? "<";PAD$("xyz",9);">"
; ? "<";PAD$("xyz",-10);">"
; ? "<";PAD$("xyz",1);">"
; ? "<";PAD$("xyz",-2);">"
; ? "<";PAD$("xyz",0);">" 
; ? "<";PAD$("xyz",9,"!");">"
; ? "<";PAD$("xyz",-10,"@");">"
; ? "<";PAD$("xyz",5,0);">"
; ? "<";PAD$("xyz",-6,0);">"
; F$="123":G$=PAD$("ABCD",3):PRINT F$,G$
FN_PAD:
    inc     hl                    ; Skip PAD
    call    _pad_arg              ; DE = ArgDsc
    push    de                    ; Stack = ArgDsc, RtnAdr
    call    get_comma_int         ; A, E = PadLen
    push    de                    ; Stack = PadLen, ArgDsc, RtnAdr
    push    af                    ; Stack = LenFlg, PadLen, ArgDsc, RtnAdr
    ld      c,' '                 ; PadChr = Space
    call    get_char_optional     ; C = PadChr
    SYNCHK  ')'                   ; Require end paren
    pop     af                    ; F = LenFlg; Stack = PadLen, ArgDsc, RtnAdr
    pop     de                    ; DE = PadLen; Stack = ArgDsc, RtnAdr
    ex      (sp),hl               ; HL = ArgDsc; Stack = TxtPtr, RtnAdr
    jr      z,.nullstring         ; If PadLen = 0, return null string
    push    hl                    ; Stack = ArgDsc, TxtPtr, RtnAdr
    push    de                    ; Stack = PadLen, ArgDsc, TxtPtr, RtnAdr
    push    bc                    ; Stack = PadChr, PadLen, ArgDsc, TxtPtr, RtnAdr
    push    de                    ; Stack = PadLen, PadChr. PadLen, ArgDsc, TxtPtr, RtnAdr
    ex      de,hl                 ; DE = ArgDsc
    call    FRETMS
    pop     de                    ; DE = Padlen, Stack = PadChr, PadLen, ArgDsc, TxtPtr, RtnAdr
    call    abs_de_byte           ; A = ABS(PadLen)
    call    STRINI                ; DE = ResAdr
    pop     bc                    ; C = PadChr; Stack = PadLen, ArgDsc, RtnAdr
    ld      a,c                   ; A = PadChr
    pop     bc                    ; BC = PadLen; Stack = ArgDsc, TxtPtr, RtnAdr
    pop     hl                    ; HL = ArgDsc; Stack = TxtPtr, RtnAdr
    ld      iy,string_pad
    call    aux_call              
    jp      PUTNEW
.nullstring
    ex      de,hl                 ; DE = ArgDsc
    call    FRETMP                ; Free argument string
    jp      NULRT                 ; Return null string
    
;-----------------------------------------------------------------------------
; TRIM functions stub
;-----------------------------------------------------------------------------
; ? TRIML$("   123   ");"<"
; ? TRIMR$("   123   ");"<"
; ? TRIM$("   123   ");"<"
; ? TRIM$(\" \t1 2 3   \r\n")
; ? TRIML$("@@@123@@@","@")
; ? TRIMR$("@@@123@@@","@")
; ? TRIM$("@@@123@@@","@")
; ? TRIML$("*#*#123#*#","#*")
; ? TRIMR$("#*#123*#*","#*")
; ? TRIM$("#*#123#*#","*#")
; ? TRIML$("      ");"<"
; ? TRIMR$("      ");"<"
; ? TRIM$("      ");"<"
; ? TRIML$("@@@@@@","@");"<"
; ? TRIMR$("@@@@@@","@");"<"
; ? TRIM$("@@@@@@","@");"<"
FN_TRIM:
    rst     CHRGET            ; Skip TRIM
    jp      m,.token
    ld      iy,string_trim
    cp      '$'
    jr      z,trim_stringd
    ld      iy,string_trim_left
    cp      'L'
    jr      z,trim_string
    ld      iy,string_trim_right
    SYNCHK  'R'
    jr      trim_stringd
.token
    cp      XTOKEN            ; If XTOKEN
    jr      z,.trimext        ;   Parse extended token
    cp      DIRTK             ; If DIR
    jp      z,FN_TRIMDIR      ;   Do TRIMDIR
    jr      .snerr
.trimext:
    rst     CHRGET            ; Skip XTOKEN
    cp      EXTTK             ; If EXT
    jp      z,FN_TRIMEXT      ;   Do TRIMEXT$
.snerr
    jp      SNERR

trim_stringd:
    dec     hl                    ; Back up to before $
trim_string:
    push    iy                    ; Stack = TrmRtn, RetAdr
    call    _trim_arg             ; DE = ArgDsc
    ld      bc,null_desc          ; BC = TrmDsc
    cp      ','
    jr      nz,.notcomma          ; If comma
    push    de                    ;   Stack = ArgDsc, TrmRtn, RetAdr
    rst     CHRGET                ;   Skip comma
    call    FRMEVL                ;   Evaluate argument
    call    CHKSTR                ;   Error if not string
    ld      bc,(FACLO)            ;   BC = TrmDsc
    pop     de                    ;   DE = ArgDsc; Stack = TrmRtn, RetAdr
.notcomma
    pop     iy                    ; IY = TrmRtn; Stack = RetAdr
    SYNCHK  ')'                   ; Require )
    push    hl                    ; Stack = TxtPtr, RetAdr
    push    de                    ; Stack = ArgDsc, TxtPtr, ret
    ld      d,b
    ld      e,c                   ; DE = TrmDsc
    call    FRETMP                ; HL = TrmDsc
    call    string_addr_len       ; DE = TrmAdr, A = TrmLen
    pop     hl                    ; HL = ArgDsc; Stack = TxtPtr, RetAdr
    push    af                    ; Stack = TrmLen, TxtPtr, RetAdr
    push    de                    ; Stack = TrmAdr, TrmLen, TxtPtr, RetAdr
    call    free_hl_addr_len      ; DE = ArgAdr, BC = ArgLen, HL = ArgDsc
    ex      de,hl                 ; HL = ArgAdr
    pop     de                    ; DE = TrmAdr; Stack = TrmLen, TxtPtr, RetAdr
    pop     af                    ; A = TrmLen; Stack = TxtPtr, RetAdr
    ld      b,a                   ; B = TrmLen
    call    aux_call              ; DE = TrmAdr, C = TrmLen
    or      a                     ; Set flags
    jr      nz,.ret_str
    ld      hl,REDDY-1
.ret_str
    jp      STRNEX                ; Create temporary and return it


; Evaluate $(str_ezpr
; Output: DE = ArgDsc, A = NxtChr
_trim_arg:
    rst     CHRGET                ; Skip L/R
_pad_arg:
    SYNCHK  '$'                   ;
    call    FRMPRN                ; Evaluate argument after (
    call    CHKSTR
;    call    GETYPE                ; If numeric
;    call    nz,facc_to_string     ;   Convert to string
    ld      de,(FACLO)            ; DE = ArgDsc
    ld      a,(hl)                ; A = Next character
    ret
