; Variable Dumper

; To assemble:
; zmac -o dump.cim -o dump.lst dump.asm

; Print string length and address instead of text
STRADDR = 1

OUTCHR      equ $18     ; 3 Output character
COMPAR      equ $20     ; 4 Compare HL with DE
SNGFLT      equ $0B36   ; Store variable 8 bit (out: B = value)
POPHRT      equ $141A   ; Pop HL and return
MOVFM       equ $1520   ; MOVE NUMBER FROM MEMORY [(HL)] TO FAC
POPART      equ $15C8   ; Pop AF and return
FOUT        equ $1680   ; Convert float in FAC to text
OUTDO       equ $198A   ; Output a Character
CRDO        equ $19EA   ; Print CR+LF

str_print   equ $210C   ; Print null-terminated string
str_printi  equ $210F   ; Print inline null terminated string

CNTOFL      equ $3808   ; Line Counter
DIMFLG      equ $38AA   ; Using this to hold the number of dimensions
VALTYP      equ $38AB   ; THE TYPE INDICATOR 0=NUMERIC 1=STRING
VARTAB      equ $38D6   ; POINTER TO START OF SIMPLE VARIABLE SPACE. UPDATED WHENEVER
ARYTAB      equ $38D8   ; POINTER TO BEGINNING OF ARRAY TABLE. INCREMENTED BY 6 WHENEVER
STREND      equ $38DA   ; END OF STORAGE IN USE. INCREASED WHENEVER A NEW ARRAY
VARNAM:     equ $38DE   ; Variable Name
FBUFFR      equ $38E8   ; BUFFER FOR FOUT

dim_base    equ $38F6   ; (RESHO) Dimension list base address
;           equ $38F8   ; (RESLO) Dimension index numbers

strbuf_addr equ $C1A3   ; Get string buffer address
FLOAT_DE    equ $C1A9   ; Convert BC to unsigned float in FACC


    .org    $B000
    
    ld      a,23                  ; Set line count to 23
    ld      (CNTOFL),a            ;
    
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ld      hl,(VARTAB)           ; Start of variables
    
    call    variables
    call    arrays
    pop     hl
    ret
    
variables:
    call    CRDO                  ; NewLine
    ld      de,(ARYTAB)           
    rst     COMPAR                ; If DatPtr >= ARYTAB
    ret     nc                    ;   Get out
    call    _out_varname
    jp      m,.string             ; If float
    call    _out_eq_float         ;   Print it
    jp      variables               ; Else
.string
    call    _out_eq_string
    jr      variables             ;   Do next variable

arrays:
    ld      de,(STREND)           
    rst     COMPAR                ; If HL >= DE
    ret     nc                    ;   Get out
    call    _out_varname          ; Print variable name
    rla                           ; Carry = StrFlg
    ld      a,0
    rla                           ; A = StrFlg
    ld      (VALTYP),a            ; VALTYP = StrFlg
    inc     hl                    ; Skip array data size
    inc     hl
    call    _out_equals
    ld      a,(hl)                ; A = Number of dimensions
    inc     hl
    ld      (DIMFLG),a            ; DIMFLG = NumDim
    ld      (dim_base),hl         ; Start of dimension list
    call    _walk_array
    jr      arrays

; Assuming one dimension for now
_walk_array
    ld      a,'['                 ; Open bracket
    rst     OUTCHR
    ld      c,(hl)
    inc     hl
    ld      b,(hl)                ; BC = IdxCnt
    inc     hl
.dimloop
    push    bc                    ; Stack = IdxCnt, RtnAdr
    ld      a,(VALTYP)
    dec     a
    jp      z,.string             ; If float
    call    _out_float            ;   Print it
    jr      .next                 ; Else
.string
    call    _out_string           ;   Print string
.next
    pop     bc                    ; BC = IdxCnt; Stack = RtnAdr
    dec     bc                    ; Countdown
    ld      a,b
    or      c                     
    jr      z,.done               ; If not zero
    ld      a,','                 ;   Print comma
    rst     OUTCHR
    jr      .dimloop              ;   and do next element
.done
    ld      a,']'                 ; Close bracket
    rst     OUTCHR                
    jp      CRDO                  ; Newline

_out_varname:
    ld      d,(hl)                ; HL = VarAdr
    inc     hl
    ld      e,(hl)                ; DE = VarNam  
    inc     hl                    ; HL = VarDat
_out_varname_de:
    ld      a,d                   ; A = 1st character
    rst     OUTCHR                ; Print it
    ld      a,e                   ; A = 2nd character
    or      a                     ; Set flags
    push    af                    ; Stack = StrFlg, RtnAdr
    and     $7F                   ; Strip string flag
    call    nz,OUTCHR
    pop     af                    ; AF = StrFlg; Stack = RtnAdr
    push    af                    ; Stack = StrFlg, RtnAdr
    call    m,_out_dollar         ; If string, print $
_pop_af_ret:
    pop     af                    ; AF = StrFlg; Stack = RtnAdr
    ret
    
_out_equals:
    call    _out_space
    ld      a,'='
    rst     OUTCHR
_out_space:
    ld      a,' '
    rst     OUTCHR
    ret

; Print float pointed to by StrDsc at (HL)
_out_eq_float:
    call    _out_equals
_out_float:
    call    MOVFM                 ; Copy float from [HL] to FACC
_out_facc:
    push    hl
    call    FOUT                  ; Convert FACC to string in FBUFFR
    ld      hl, FBUFFR+1
    call    str_print
    pop     hl
    ret

; Print string pointed to by StrDsc at (HL)
_out_eq_string:
    call    _out_equals
_out_string:
    ld      b,(hl)                ; B = StrLen
    inc     hl
    inc     hl                    ; Unused byte
    ld      e,(hl)                ; 
    inc     hl
    ld      d,(hl)                ; DE = StrAdr
    inc     hl
_print_str:    
ifdef STRADDR
    push    hl
    push    de
    call    str_printi
    byte    "(Len:",0
    ld      a,b
    call    SNGFLT
    call    _out_facc
    call    str_printi
    byte    ", Address: $",0
    ld      a,1
    ld      (VALTYP),a            ; Reset VALTYPE to string
    pop     de
    pop     hl
    ld      a,d
    call    out_hex
    ld      a,e
    call    out_hex
    ld      a,')'
    jp      OUTDO
else
    call    _out_dollar
    inc     b                     ; Bump Count for decrement
    jr      .next
.loop
    ld      a,(de)
    inc     de
    call    out_hex
.next
    djnz    .loop
endif
_out_dollar:
    ld      a,'$'
    jp      OUTDO


out_hex:
    push    af                    
    rra
    rra
    rra
    rra
    call    .hex
    pop     af
.hex:
    and     $0F
    cp      10
    jr      c, .chr
    add     7
.chr:
    add     '0'
    jp      OUTDO
