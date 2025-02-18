;====================================================================
; Debugging routines
;====================================================================

_BUFADDR    equ   $3838
_CHANNEL    equ   $383A
_LINLEN     equ   $383B
_STRADDR    equ   $383C
_STRLEN     equ   $383E

;-----------------------------------------------------------------------------
; Dump BASIC variables to file vardump.txt
; Input: DE: String Buffer Address
;        HL: Filename String Desc Address
; Clobbers: AF,AF',BC,DE,HL,IX
;-----------------------------------------------------------------------------
dump_vars:
    ld      (_BUFADDR),de
    call    dos_open_write
    ret     m
    ld      (_CHANNEL),a
    call    _dump
    ld      a,(_CHANNEL)
    call    dos_close
    ret

_dump:
    ld      de,_dtitle
    ld      bc,_dtitlen
    call    _write_string
    call    _sysvars
    call    _variables
;    call    _arrays
    ret

; Dump Simple Variables
_variables:
    ld      de,svar
    ld      bc,svarlen
    call    _write_string
    ld      hl,(VARTAB)           ; Start of variables
.loop
    ld      de,(ARYTAB)
    rst     COMPAR                ; If DatPtr >= ARYTAB
    ret     nc                    ;   Get out
    push    hl                    ; Stack = VarPtr, RtnAdr
    call    _out_varname          ; Buffer VarNam
    pop     de                    ; DE = VarPtr; Stack = RtnAdr
    call    _out_addr
    jp      m,.string             ; If float
    call    _out_float_hl         ;   Buffer VarVal
    call    _write_line
    jr      .loop                 ;   Do next variable
.string
    call    _out_strlen           ;   Buffer StrLen
    call    _out_stradr           ;   Buffer StrAdr
    call    _write_line           ;
    call    _write_strtxt         ;   Write ASCII String Text
    call    _write_strhex         ;   Write HEX String Text
    jr      .loop                 ;   Do next variable

_arrays:
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
;    call    _out_equals
    ld      a,(hl)                ; A = Number of dimensions
    inc     hl
    ld      (DIMFLG),a            ; DIMFLG = NumDim
;    ld      (dim_base),hl         ; Start of dimension list
    call    _walk_array
    jr      _arrays

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
    ;call    _out_float            ;   Print it
    jr      .next                 ; Else
.string
;    call    _out_string           ;   Print string
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


; Input: HL: VarPtr
; Output: F: StrFlg (Neg if String)
;        BC: BufPtr
;        HL: Updated VarPtr
; Sets: VALTYP
_out_varname:
    ld      bc,(_BUFADDR)        ; BC = BufAdr
    call    _out_space2
    ld      d,(hl)                ; HL = VarAdr
    inc     hl
    ld      e,(hl)                ; DE = VarNam
    inc     hl                    ; HL = VarDat
    ld      a,d                   ; A = 1st character
    call    _out_buff
    ld      a,e                   ; A = 2nd character
    and     $80
    push    af                    ; Stack = StrFlg, RtnAdr
    rla
    rla                           ; Bit 87 --> Bit 0
    ld      (VALTYP),a
    ld      a,e
    and     $7F                   ; Strip string flag
    call    nz,_out_buff
    pop     af                    ; AF = StrFlg; Stack = RtnAdr
    push    af                    ; Stack = StrFlg, RtnAdr
    call    m,_out_dollar         ; If string, print $
    call    _out_colon
    pop     af                    ; AF = StrFlg; Stack = RtnAdr
    ret

; Dump System Variables
_sysvars:
    ld      de,yvar
    ld      bc,yvarlen
    call    _write_string
    ld      hl,_sysvar_list
.loop
    ld      a,(hl)                ; Output Type
    inc     hl
    or      a
    ret     z
    ld      (VALTYP),a            ; VARTYP = Output Type
    ld      bc,(_BUFADDR)         ; BC = Buffer Address
    call    _out_space2
    ld      d,(hl)                ; DE = Address
    inc hl
    ld      e,(hl)
    inc     hl
    call    _out_addr
    push    de                    ; Stack = VarAdr, RtnAdr
    ld      e,6
.nameloop
    ld      a,(hl)
    inc     hl
    ld      (bc),a
    inc     bc
    dec     e
    jr      nz,.nameloop
    pop     de                    ; DE = VarAdr; Stack = RtnAdr
    push    hl                    ; Stack = LstPtr, RtnAdr
    ex      de,hl                 ; HL = VarAdr
    ld      e,(hl)
    inc     hl
    ld      d,(hl)                ; DE = VarVal
    ld      a,(VALTYP)
    cp      'B'                   ; If OutTyp = F
    jr      nz,.f                 ;   Output floated sysvar value
    ld      a,e
    call    _out_byte
    jr      .write
.f
    cp      'F'                   ; If OutTyp = F
    jr      nz,.w                 ;   Output floated sysvar value
    call    _out_word
    jr      .write
.w
    call    _out_space
    call    _out_addr
.write
    call    _write_line
    pop     hl                    ; HL = LstPtrl Stack = RtnAdr
    jr      .loop

_sysvar_list
    byte    'B',$38,$00,'TTYPOS'
    byte    'W',$38,$01,'CURRAM'
    byte    'W',$38,$04,'USRADD'
    byte    'B',$38,$48,'LINLEN'
    byte    'F',$38,$4D,'CURLIN'
    byte    'W',$38,$DC,'DATPTR'
    byte    'F',$38,$C9,'DATLIN'
    byte    'W',$38,$AF,'TEMPPT'
    byte    'e',$38,$B3,'TEMPST'
    byte    'e',$38,$B3,'TEMPST'
    byte    'e',$38,$B7,'      '
    byte    'e',$38,$BB,'      '
    byte    'W',$38,$BD,'DSCTMP'
    byte    'W',$38,$4F,'TXTTAB'
    byte    'W',$38,$D6,'VARTAB'
    byte    'W',$38,$D8,'ARYTAB'
    byte    'W',$38,$DA,'STREND'
    byte    'W',$38,$F9,'SAVSTK'
    byte    'W',$38,$4B,'TOPMEM'
    byte    'W',$38,$C1,'FRETOP'
    byte    'W',$38,$AD,'MEMSIZ'
    byte    0


; Updates: BC: BufPtr, HL: VarPtr; Clobbers: AF, DE
_out_strlen:
    ld      a,(hl)
    inc     hl
    ld      (_STRLEN),a
    call    _out_byte
    inc     hl                    ;   Skip unused byte
    jp      _out_space

_write_strhex:
    ld      a,32
    ld      ix,_out_hex
    ld      de,_hextxt
    ld      bc,_hexlen
    jr      _write_strcon

_write_strtxt:
    ld      a,64
    ld      ix,_out_asc
    ld      de,_asctxt
    ld      bc,_asclen
_write_strcon:
    ex      af,af'
    ld      a,(_STRLEN)
    or      a                     ; If StrLen = 0
    ret     z                     ;   Returm
    push    hl                    ; Stack = VarPtr, RtnAdr
    push    af                    ; Stack = StrLen, VarPtr, RtnAdr
    ex      af,af'
    ld      (_LINLEN),a
    call    _write_string         ; Write "ASCII:"
    ld      hl,(_STRADDR)
    jr      .doline
.newline
    push    de                    ; Stack = StrLen, VarPtr, RtnAdr
    call    _write_line
.doline
    pop     de                    ; D = StrLen; Stack = VarPtr, RtnAdr
    ld      bc,(_BUFADDR)
    call    _out_space6           ; Indent 6 spaces
    ld      a,(_LINLEN)
    ld      e,a                   ; E = LinLen
.loop
    dec     e
    jr      z,.newline
    ld      a,(hl)
    inc     hl
    call    (jump_ix)
    dec     d
    jr      nz,.loop
    call    _write_line
    pop     hl                    ; HL = VarPtr; Stack = RtnAdr
    ret

; Updates: BC: BufPtr, HL: VarPtr; Clobbers: AF, DE
_out_byte:
    push    bc                    ; Stack = BufPtr, RtnAdr
    push    hl                    ; Stack = VarPtr, BufPtr, RtnAdr
    call    SNGFLT                ; FACC = Floated A
    jr      __out_facc            ; Output FACC

_out_word:
    push    bc                    ; Stack = BufPtr, RtnAdr
    push    hl                    ; Stack = VarPtr, BufPtr, RtnAdr
    ld      a,d
    ld      d,e
    call    FLOATD                ; FACC = Floated AD
    jr      __out_facc            ; Output FACC

; Updates: BC: BufPtr, HL: VarPtr; Clobbers: AF, DE
_out_float_hl:
    push    bc                    ; Stack = BufPtr, RtnAdr
    call    MOVFM                 ; Copy float from [HL] to FACC
    push    hl                    ; Stack = VarPtr, BufPtr, RtnAdr
__out_facc:
    call    FOUT                  ; Convert FACC to string in FBUFFR
    pop     hl                    ; HL = VarPtr; Stack = BufPtr, RtnAdr
    pop     bc                    ; BC = BufPtr; Stack = RtnAdr
    push    hl                    ; Stack = VsrPtr, RtnAdr
    ld      hl, FBUFFR+1
.loop
    ld      a,(hl)
    inc     hl
    or      a
    jr      z,.done
    ld      (bc),a
    inc     bc
    jr      .loop
.done
    pop     hl                    ; HL = VarPtr; Stack = RtnAdr
    ret

_out_dollar:
    ld      a,'$'
    jp      _out_buff

_out_stradr:
    ld      a,(hl)
    ld      (_STRADDR),a
    ld      e,a
    inc     hl
    ld      a,(hl)
    ld      (_STRADDR+1),a
    ld      d,a
    inc     hl
; Input: DE: Address; Updates: BC: BufPtr
_out_addr:
    push    af                    ; Stack = StrFlg, RtnAdr
    ld      a,'$'
    ld      (bc),a
    inc     bc
_out_de:
    ld      a,d
    call    _out_hex
    ld      a,e
    call    _out_hex
    call    _out_space
    pop     af                    ; F = StrFlg; Stack = RtnAdr
    ret

_out_asc:
    cp      '!'
    jr      c,.dot
    cp      127
    jr      c,_out_buff
.dot
    ld      a,'.'
    jr      _out_buff

_out_colon:
    ld      a,':'
    ld      (bc),a
    inc     bc
    jr      _out_space

_out_space6:
    call    _out_space2
_out_space4:
    call    _out_space2
_out_space2:
    call    _out_space
_out_space:
    ld      a,' '
_out_buff:
    ld      (bc),a
    inc     bc
    ret

_out_crlf:
    ld      a,13
    ld      (bc),a
    inc     bc
    ld      a,10
    jr      _out_buff

_out_hex:
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
    jr      _out_buff


; Input: BC = BufPtr
; Clobbers: A, BC, DE
_write_line:
    call    _out_crlf
_write_buff:
    push    hl                    ; Stack = VarPtr, RtnAdr
    ld      h,b                   ; HL = BufPtr
    ld      l,c
    ld      bc,(_BUFADDR)        ; BC = BufAdr
    ld      d,b
    ld      e,c                   ; DE = BufAdr
    or      a
    sbc     hl,bc                 ; HL = OutLen
    ld      b,h
    ld      c,l                   ; BC = OutLen
    call    _write_string
    pop     hl                    ; HL =  VarPtr; Stack = RtnAdr
    ret



_write_string:
    ld      a,(_CHANNEL)
    jp      esp_writec_bytes

_dtitle:
    byte    "# Aquarius+ Variable Dump",13,10
_dtitlen     equ $ - _dtitle

yvar:
    byte    "System Variables:",13,10
yvarlen     equ $ - yvar

svar:
    byte    13,10,"Simple Variables:",13,10
svarlen     equ $ - svar

_asctxt:
    byte    "    ASCII:",13,10
_asclen     equ $ - _asctxt

_hextxt:
    byte    "    HEX:",13,10
_hexlen     equ $ - _hextxt
