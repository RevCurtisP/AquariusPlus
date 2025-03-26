; Core routines for BASIC statements and functions

; ------------------------------------------------------------------------------
; ------------------------------------------------------------------------------
bas_bin_str:


; ------------------------------------------------------------------------------
; ERASE statement core code
; ------------------------------------------------------------------------------
; On entry:
bas_erase_array:
        ld      h,b               ;[B,C]=START OF ARRAY TO ERASE
        ld      l,c 
        dec     bc                ;BACK UP TO THE FRONT
        dec     bc                
        dec     bc  
        dec     bc  
        add     hl,de             ;[H,L]=THE END OF THIS ARRAY ENTRY
        ex      de,hl             ;[D,E]=END OF THIS ARRAY
        ld      hl,(STREND)       ;[H,L]=LAST LOCATION TO MOVE UP
ERSLOP: rst     COMPAR            ;SEE IF THE LAST LOCATION IS GOING TO BE MOVED
        ld      a,(de)            ;DO THE MOVE
        ld      (bc),a  
        inc     de                ;UPDATE THE POINTERS
        inc     bc  
        jr      nz,ERSLOP         ;MOVE THE REST
        dec     bc  
        ld      h,b               ;SETUP THE NEW STORAGE END POINTER
        ld      l,c 
        ld      (STREND),hl 
        ret

; Called from clear_hook in extended.asm
; On entry, HL points to KEY token
bas_clear_keys:
    inc     hl                ; Skip KEY
    SYNCHK  'S'               ; Require S
    xor     a
    ld      (RESPTR+1),a      ; Reset autotype
    ld      (CHARC),a         ; Clear INKEY buffer
    jp      key_clear_fifo    ; Clear keyboard buffer and return

; Called from ST_DATE
bas_date:
    ld      ix,esp_get_datetime   ; Read Date and Time into buffer
    call    bas_read_to_buff      ; Set buffer address and call routine
    inc     c                     ; 0 = DATETIME$, 1=DATE$
    ret     z
    ld      de,8
    add     hl,de                 ; Start of Time substring
    xor     a
    ld      (hl),a                ; Terminate Date substring
    sbc     hl,de                 ; Set HL back to Buffer address
    ret

; Called from ST_TIME
bas_time:
    ld      ix,esp_get_datetime   ; Read Date and Time into buffer
    call    bas_read_to_buff      ; Set buffer address and call routine
    ld      bc,8
    add     hl,bc                 ; Start of Timr String
    ret

; ------------------------------------------------------------------------------
; Decrement variable
; Input: DE: VarPtr
;        HL: TxtPtr
; ------------------------------------------------------------------------------
bas_dec:
    ld      ix,FSUBS              ; Operation = FACC = Arg - FACC
    jr      _incdec               ; Go do it

; ------------------------------------------------------------------------------
; Increment variable
; Input: DE: VarPtr
;        HL: TxtPtr
; ------------------------------------------------------------------------------
bas_inc:
    ld      ix,FADDS              ; Operation = FACC = Arg + FACC
_incdec:
    call    CHKNUM                ; Type mismatch error if not numeric
    push    hl                    ; Stack = TxtPtr, RtnAdr
    push    de                    ; Stack = VarPtr, TxtPtr, RtnAdr
    push    de                    ; Stack = VarPtr, VarPtr, TxtPtr, RtnAdr
    ld      hl,FONE               ; (HL) = 1.0
    call    MOVFM                 ; FACC = 1/0
    pop     hl                    ; HL = VarPtr; Stack = VarPtr, TxtPtr, RtnAdr
    call    (jump_ix)             ; Do operation
    pop     hl                    ; HL = VarPtr; Stack = TxtPtr, RtnAdr
    call    MOVMF                 ; VarVal = FACC
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

; Called from FN_KEY
; Input: Arg Type
bas_key:
    jr      z,.string             ; If numeric
    ld      hl,float_signed_byte
    ld      a,-1
    rst     FSIGN
    call    p,CONINT              ;   A = Matrix code
    call    key_pressed           ;   A = -1 if pressed
    jp      c,FCERR               ;   Error if invalid keycode
    ret                           ;   Return result
.string
    call    free_addr_len         ; DE = Address, BC - StrLen
    jp      z,FCERR               ; Error if NULL string
    ld      hl,SNGFLT
    ld      b,c
    ld      c,1
.loop
    ld      a,(de)
    push    bc
    push    de
    call    key_pressed           ;   NZ if pressed
    pop     de
    pop     bc
    ld      a,c
    ret     nz
    inc     c
    inc     de
    djnz    .loop
    xor     a
    ret


; Called from FN_MOUSE
; Input: H: SfxChr
; Output: A: Result, Sign set if negative
;         BC: Rsult if Carry set
bas_mouse:
    push    hl                    ; Stack = SfxChr, RtnAdr
    call    espn_get_mouse        ; BC = xpos, D = buttons, E = ypos, L = wheel
    jr      nz,.not_found
    call    .update_wdelt
    pop     af                    ; AF = SfxChr; Stack = RtnAdr
    cp      'X'
    jr      z,.xpos
    cp      'Y'
    jr      z,.ypos
    cp      'B'
    jr      z,.buttons
    cp      'W'
    jr      z,.wheel
.snerr:
    jp      SNERR
.buttons
    ld      a,d
    byte    $06                   ; LD B, over next instruction
.ypos:
    ld      a,e
    ret
.not_found:
    pop     af                    ; Stack = RtnAdr
    ld      a,-1                  ; Return Not found
    or      a                     ; Signed result
    ret
.xpos:
    scf
    ret
.wheel
    call    .read_wdelt           ; A = WheelDelt
    push    af                    ; Stack = WheelDelt, RtnAdr
    xor     a
    call    .write_wdelt
    pop     af
.signed_byte
    or      a                     ; Signed result
    ret

.update_wdelt
    push    bc
    push    de
    call    .read_wdelt
    add     l                     ; Accumulate mouse wheel delta
    call    .write_wdelt
    pop     de
    pop     bc
    ret

.read_wdelt
    ld      de,MOUSEWDLT
    call    basbuf_read_byte
    ld      a,c                   ; C = WheelDelt
    ret  

.write_wdelt
    dec     de                    ; DE = MOUSEWDLT
    ld      c,a
    jp      basbuf_write_byte

      
; Called from FN_OFF
; On entry: C = Column, E = Row
bas_offset:
    ld      a,(LINLEN)
    dec     a                   ; 
    cp      c                   ; If LinLen - 1 < Column
    jp      c,FCERR             ;   Illegal Quantity error
    ld      a,24
    cp      e                   ; If 24 < Row
    jp      c,FCERR             ;   Illegal Quantity error
    ld      d,c                 ; D = Column
    ex      de,hl               ; H = Column, L = Row
    call    cursor_offset       ; HL = Offset
    ex      de,hl               ; DE = Offset
    ret

; Called from ST_SET_BIT
; On entry: AF = VarTyp, BC = VarPtr, DE = BitNo
bas_set_bit:
    ld      ix,bool_setbit_long   ; Set the bit
    jp      z,TMERR               ; No Strings (for now)
    xor     a                     
    or      d                     ; If BitNo > 255            
    jp      nz,FCERR              ;   Illegal Quantity Error
    ld      a,e                   ; A = BitNo
    ld      h,b                   ; HL = VarPtr
    ld      l,c                 
    push    hl                    ; Stack = VarPtr, RtnAdr
    push    af                    ; Stack = BitNo, VarPtr, RtnAdr
    call    MOVFM                 ; FACC = (VarPtr)
    call    FRC_LONG              ; CDE = Long
    call    jump_ix
    jp      c,FCERR
    ret

; ------------------------------------------------------------------------------
; Scan literal string and put Descriptor in FACC
; ------------------------------------------------------------------------------
aux_str_literal:
    cp      '"'                   ; Require Opening Quote
    jp      nz,TMERR
    inc     hl                    ; HL = StrAdr
    ld      (FACLO+2),hl          ; Store in StrDsc
    ld      bc,0                  ; BC = StrLen
.loop
    ld      a,(hl)                
    inc     hl
    or      a
    jr      z,.done               ; If not end of statement
    cp      '"'                   ; or Closing Quote
    jr      z,.done
    inc     bc
    jr      .loop
.done    
    ld      a,b
    or      a                     ; If StrLen > 255
    jp      nz,FCERR              ;   Error
    or      c                     ; If StrLen = 0
    jp      z,FCERR               ;   Error
    ld      (FACLO),bc            ; Srore in StrDsc
    ret

aux_eserr:
    ld      e,ERRES
    byte    $01                   ; LD BC over LD E
aux_slerr:
    ld      e,ERRSL
    jp      ERROR

; *** Not called from anywhere, don't remember why it's here ***
aux_set_array:
    rst     CHRGET                ; Skip =
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ld      hl,(TEMP3)            ; HL = DimsPtr
    dec     hl
    dec     hl
    ld      b,(hl)
    inc     hl
    ld      c,(hl)                ; BC = AryLen;
    inc     hl
    ld      (ARRAYLEN),bc         ; BC = AryLen
    ld      a,(hl)                ; A = NumDims
    or      a                     ; If no dimensions
    jp      z,UDERR               ;   Undimensioned array error
    inc     hl
    add     a,a                   ; A = NumDims * 2
    ld      b,0
    ld      c,a                   ; BC = NumDims * 2
    add     hl,bc                 ; HL = AryAdr    
    ld      (ARRAYPTR),hl         ; ARRAYPTR = AryPtr
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    call    CHRGT2                ; Reget current character
    ret     z                     ; If terminator, return
    ld      a,$FF
    ld      (ARRAYREQ),a          ; Strings must be quoted
    call    _array_reader         ; Read literals into array
    ret     c                     ; Return if end of statement
    call    CHRGT2                ; If terminator
    ret     z
    cp      ','
    ld      e,ERRTO
    jp      z,ERROR               ; If comma, Too many operands error
    jp      SNERR                 ; Else syntax error

; Populate array from comma separated text
; (ARRAYPTR) = Pointer into array data
; (ARRAYLEN) = Bytes left in array data
; HL = TxtPtr (next literal to read)
_array_read:
    call    GETYPE                ; A = 0: String, 1: Numeric
    ld      (ARRAYTYP),a          ; Save arraytype
_array_reader:
    ld      a,(ARRAYTYP)
    or      a
    jr      z,.string             ; If numeric array
    call    fin_extension         ;   Read number into FACC
    push    hl                    ;   Stack = TxtPtr, RtnAdr
    ld      hl,FACLO              ;   HL = FACC
    jr      .next                 ; Else
.string
    call    CHRGT2                ;   A = First character
    cp      ','
    jr      z,.comma
    ld      d,a                   ;   If it's a quote, terminators will be quote
    ld      e,a                   ;   
    cp      '"'
    jr      z,.parse              ;   If not quoted
    ld      a,(ARRAYREQ)
    or      a                     ;     If quotes required
    jp      nz,TMERR              ;       Type mismatch error
.comma
    ld      d,':'                 ;     Terminators are colon and comms
    ld      b,','                 ;    
    dec     hl                    ;     Back up TxtPtr
.parse
    call    STRLT2                ;   Parse string literal
    push    hl                    ;   Stack = TxtPtr, RtnAdr
    call    FREFAC                ;   HL = StrDsc 
    ex      de,hl                 ;   DE = StrDsc
    call    copy_literal_string
    ex      de,hl                 ;   HL = StrDsc
.next
    ld      de,(ARRAYPTR)         ; HL = AryPtr
    ld      bc,4
    ldir                          ; Copy to Array
    ld      (ARRAYPTR),de         ; ARRAYPTR = AryPtr
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ld      bc,(ARRAYLEN)         ; BC = AryLen
    dec     bc                    ; Count down
    dec     bc
    dec     bc
    dec     bc
    ld      (ARRAYLEN),bc         ; BC = AryLen
    ld      a,b
    or      c                     ; If end of array
    ret     z                     ;   Return
    call    CHRGT2                ; If terminator
    scf                           ;   Return Carry Set
    ret     z
.noteol
    SYNCHK  ','                   ; Else require comma
    jr      _array_reader           ;   and get next item

; Called from ST_JOIN
bas_join:
    call    get_strbuf_addr       ; HL = StrBuf
    ld      (BUFADR),hl           ; BufAdr = StrBuf
    ld      (BUFPTR),hl           ; BufPtr = StrBuf
    ld      bc,0
    ld      (BUFLEN),bc           ; BufLen = 0
.loop
    ld      hl,(ARRAYPTR)         ; HL = AryPtr
    call    string_addr_len       ; DE = StrAdr, BC = StrLen
    ex      de,hl                 ; HL = StrAdr
    ld      de,(BUFPTR)           ; DE = BufPtr
    ld      a,(BUFLEN)            ; A = Buflen
    jr      z,.empty              ; If StrLen > 0
    add     c                     ;   A = BufLen + StrLen + 1
    inc     a                     ;   If A > 255
    jp      c,LSERR               ;     String to long error
    ldir                          ;   DE = NewPtr
    dec     a
.empty
    inc     a
    ld      (BUFLEN),a            ;   BufLen = A 
    ld      a,(SPLITDEL)          ; A = DelChr
    ld      (de),a                ; (NewPtr) = DelChr
    inc     de                    ; Bump NewPtr
    ld      (BUFPTR),de           ; DE = NewPtr
    call    _skipone              ; If not last element
    jr      nz,.loop              ;   Loop
    ld      hl,(BUFPTR)
    dec     hl                    ; Back up to last delimiter
    ld      (hl),0                ; Terminate string
    ld      a,(BUFLEN)            ; A = StrLen
    dec     a                     ; Cut last delimiter
    call    STRINI                ; HL = DscTmp, DE = DstAdr
    push    hl                    ; Stack = DscTmp, TxtAdr, RtnAdr
    ld      hl,(BUFADR)           ; HL = StrBuf
    ld      bc,(BUFLEN)           ; BC = BufLen
    ldir                          ; Copy Buffer to String Space
    pop     hl                    ; HL = DscTmp; Stack = TxtAdr, RtnAdr
    ld      de,(SPLITDSC)         ; DE = VarPtr
aux_movvfm:
    ld      bc,4                  ; Copy StrDsc to Variable
    ldir
    ret

; Called from ST_SPLIT
bas_split:
    ld      hl,(SPLITDSC)         ; HL = SrcDsc
    call    free_hl_addr_len      ; DE = SrcAdr, BC = SrcLen, HL = SrcDsc
    jp      z,aux_split_fill      ; If null string, pop TxtPtr and return
    push    bc                    ; Stack = SrcLen, TxtPtr, RtnAdr
    call    get_strbuf_addr       ; HL = StrBuf
    pop     bc                    ; BC = SrcLen; Stack = TxtPtr, RtnAdr
    push    hl                    ; Stack = StrBuf, TxtPtr, RtnAdr
    ex      de,hl                 ; DE = StrBuf, HL = SrcAdr
    ldir                          ; Copy Source String to String Buffer
    xor     a
    ld      (de),a                ; Null terminate string
    ld      (SPLITSEG),a          ; SPLITSEG = SegCnt
    pop     hl                    ; HL = BufPtr; Stack = TxtPtr, RtnAdr
; Populate array from comma separated text
; Input: HL: BufPtr
; (SPLITDSC) = String descriptor of string to split
; (ARRAYADR) = Address of first array element
; (ARRAYLEN) = Length of array data
; (SPLITDEL) = Delimiter to split on 
; (SPLITSEG) = String segment count
aux_split_array:
    call    _skipone              ; Skip first array element
.loop
    push    hl                    ; Stack = SegPtr, TxtPtr, RtnAdr
    ld      a,(SPLITDEL)          ; A = DelChr
    ld      b,a                   ; B = DelChr
    ld      c,0
.parse
    ld      a,(hl)
    or      a
    jr      z,.store              ; If not NUL
    cp      b                     
    jr      z,.store              ; Or delimiter
    inc     hl
    inc     c                     ;   Check next character
    jr      .parse
.store
    push    hl                    ; Stack = BufPtr, SegPtr, TxtPtr, RtnAdr
    xor     a
    ld      d,a
    ld      e,a                   ; StrAdr = 0
    ld      b,a                   ; StrLen = 0
    or      c                     ; A = SegLen
    jr      z,.null               ; If SegLen <> 0
    push    bc                    ;   Stack = SegLen, BufPtr, SegPtr, TxtPtr, RtnAdr
    call    GETSPA                ;   DE = StrAdr
    push    de                    ;   Stack = StrAdr, SegLen, BufPtr, TxtPtr, RtnAdr
    call    FRETMS                ;   Free temporary but not string space
    pop     de                    ;   DE = StrAdr; Stack = SegLen, BufPtr, SegPtr, TxtPtr, RtnAdr
    pop     bc                    ;   BC = SegLen; Stack = BufPtr, SegPtr, TxtPtr, RtnAdr
.null
    ld      hl,(ARRAYPTR)         ; HL = AryPtr
    call    aux_bcde2hl           ; Write string descriptor to array
    ld      (ARRAYPTR),hl         ; ARRAYPTR = AryPtr
    ld      a,b
    or      c
    pop     hl                    ; HL = BufPtr; Stack = SegPtr, TxtPtr, RtnAdr
    ex      (sp),hl               ; HL = SegPtr, Stack = BufPtr, TxtPtr, RtnAdr
    jr      z,.nocopy             ; If SegLen <> 0
    ldir                          ;   Copy Segment to String Text
.nocopy
    ld      hl,SPLITSEG           ; Bump SegCnt
    inc     (hl)
    pop     hl                    ; HL = BufPtr; Stack = TxtPtr, RtnAdr
    ld      a,(hl)                ; A = SegDel
    inc     hl                    ; Skip segment delimiter
    or      a                      
    jr      z,.done               ; If not null (end of SrcStr)  
    ld      bc,(ARRAYLEN)         ;   BC = AryLen
    call    _countdown            ;   If not last element
    jr      nz,.loop              ;     Get next segment
.done
    ld      a,(SPLITSEG)          ; A = SegCnt
    call    SNGFLT                ; Float Segment Count
    call    FOUT                  ; Convert to ASCII
    ld      de,FBUFFR+1           ; DE = FltAdr
    call    str_length            ; A, BC = FltLen
    push    bc                    ; Stack = FltLen, TxtPtr, RtnAdr
    call    GETSPA                ; DE = StrAdr
    push    de                    ; Stack = StrAdr, FltLen, TxtPtr, RtnAdr 
    call    FRETMS                ; Free temporary but not string space
    pop     de                    ; DE = StrAdr; Stack = FltLen, TxtPtr, RtnAdr
    pop     bc                    ; BC = FltLen; Stack = TxtPtr, RtnAdr
    dec     bc                    ; Adjust for skip leading space
    push    de                    ; Stack = StrAdr, TxtPtr, RtnAdr
    push    bc                    ; Stack = FltLen, StrAdr, TxtPtr, RtnAdr
    ld      hl,FBUFFR+2           ; Skip leading space
    ldir                          ; Copy FBUFFR to string space
    pop     bc                    ; BC = FltLen; Stack = StrAdr, TxtPtr, RtnAdr
    pop     de                    ; DE = StrAdr; Stack = TxtPtr, RtnAdr
    ld      hl,(ARRAYADR)         ; HL = ARRAYADR
    call    aux_bcde2hl           ; Write string descriptor to VAR$(0)
aux_split_fill:
    ld      bc,(ARRAYLEN)
    ld      hl,(ARRAYPTR)
    ld      a,b                   
    or      c                     ; If BC = 0
    ret     z                     ;   Return
    jp      sys_fill_zero         ; Fill to end with 0 bytes

_skipone
    ld    de,(ARRAYPTR)
    ld    bc,(ARRAYLEN)
bas_skipfirst
    inc   de
    inc   de
    inc   de
    inc   de
    jr    _ARRAYPTR
_clearfirst
    xor     a
    ld      b,4
.skiploop
    ld      (de),a
    inc     de
    djnz    .skiploop
_ARRAYPTR
    ld      (ARRAYPTR),de         ; ARRAYPTR = AryPtr
_countdown:
    dec     bc
    dec     bc
    dec     bc
    dec     bc                    ; AryLen = AryLen - 4
    ld      (ARRAYLEN),bc         ; ARRAYLEN = AryLen
_array_len
    ld      a,b
    or      c                     ;   If not last element
    ret

;-----------------------------------------------------------------------------
; Convert Hexademimal String to Binary String
; Input: BC: Hex string length
;        DE: Hex string buffer
;        HL: Binary string buffer
; Output: BC: Binary strin length
; Clobbered: A,DE
;-----------------------------------------------------------------------------
aux_hex_to_asc:
    srl     a                     ; BinLen = AscLen / 2
    ret     c                     ; Return Carry if AscLen was Odd
    push    hl                    ; Stack = BinAdr, RtnAdr
    push    af                    ; Stack = BinLen, BinAdr, RtnAdr
    ex      de,hl                 ; HL = AscAdr, DE=BinAdr
    ld      b,a                   ; B = BinLen
.loop
    call    aux_hex_to_byte
    jr      c,_ret_nz
    ld      (de),a
    inc     de
    djnz    .loop
    pop     af
    ld      b,0
    ld      c,a                   ; BC = BinLen
    pop     hl                    ; HL = BufAdr
    ret

_ret_nz:
    or      $FF
    ret

; Clobbers: C
aux_hex_to_byte
    call    aux_get_hex           ; Get Hex Digit from Argument
    ret     c
    sla     a                     ; Shift to High Nybble
    sla     a
    sla     a
    sla     a
    ld      c,a                   ; and Save in C
    call    aux_get_hex           ; Get Next Hex Digit from Argument
    ret     c
    or      c                     ; Combine with High Nybble
    ret

aux_get_hex:
    ld      a,(hl)          ; Get Hex Digit
    inc     hl              ; Bump Pointer
aux_cvt_hex:
    cp      '.'
    jr      nz,.not_dot     ; If dot
    add     '0'-'.'         ;   Convert to 0
.not_dot
    cp      ':'             ; Test for Digit
    jr      nc,.not_digit   ; If A <= '9'
    sub     '0'             ;   Convert Digit to Binary
    ret                     ;   Else Return
.not_digit
    and     $5F             ; Convert to Upper Case
    sub     'A'-10          ; Make 'A' = 10
    ret     c               ; Error if it was less than 'A'
    cp      16              ; If less than 16
    ret                     ;   Return


;; Function enhancement remove
;; ToDo: Move this functionality to JOIN
; dim a(9):S$=STR$(*A)
FN_STRS:
    rst     CHRGET                ; Skip STR$
    SYNCHK  '('                   ; Require (
    cp      MULTK                 ;
    jr      z,.numeric_array;     ; If not *
    call    FRMEVL                ;   Evaluate argument
    SYNCHK  ')'                   ;   Require ')'
    jp      STR                   ;   Execute S3BASIC STR$ function
.numeric_array
    call    get_star_array        ; DE = AryDat, BC = AryLen
    call    CHKNUM                ; Type mismatch error if not numeric
    ld      (ARRAYPTR),de         ; ARRAYPTR = AryPtr
    ld      ix,.dofloat           ; Default to floating point
    ld      de,4                  ; SegLen = 4
    cp      ','
    jr      nz,.nocomma           ; If comma
    rst     CHRGET                ;
    rst     SYNCHR
    byte    XTOKEN                ;   Extended Tokens only
    ld      e,2                   ;   SegLen = 2
    ld      ix,.doword
    cp      WORDTK                ;   If not WORD
    jr      z,.skipchr
    dec     e                     ;   SegLen = 4
    ld      ix,.dobyte
    cp      BYTETK                ;   If not BYTE
    jp      nz,SNERR              ;     Syntax error
.skipchr
    rst    CHRGET                 ;   Skip BYTE/WORD
.nocomma
    SYNCHK  ')'                   ; Require ')'
    push    hl                    ; Stack = TxtPtr, RtnAdr
    sra     b
    rr      c                     ; BC = BC / 2
    sra     b
    jp      nz,LSERR              ; IF BC/4 is 255, String too long error
    rr      c                     ; BC = BC / 4
    ld      (ARRAYLEN),bc         ; ARRAYLEN = ArySiz
    call    mult_a_de             ; HL = StrLen
    ld      a,h
    or      a                     ; If StrLen > 255
    jp      nz,LSERR              ;   String too long error
    ld      a,l                   ; A = StrLen
    call    STRINI                ; DSCTMP = StrDsc, DE = StrAdr, A = StrLen
    ld      (BUFPTR),de           ; BUFPTR = StrPtr
    call    FREFAC                ; Free up temp
.loop
    call    jump_ix               ; Convert array element and write to string
    jr      nz,.loop              ; If not done, loop
    jp      PUTNEW                ; Else return the string
; Subroutines
.dobyte
    call    .array_to_facc
    call    CONINT                ; E = Byte
    ld      hl,(BUFPTR)           ; HL = StrPtr
    ld      (hl),e
    inc     hl
    jr      .next
.doword
    call    .array_to_facc
    call    FRCINT                ; DE = Word
    ld      hl,(BUFPTR)           ; HL = StrPtr
    ld      (hl),e
    inc     hl
    ld      (hl),d
    inc     hl                    ; Copy word to string
.next
    ld      (BUFPTR),hl           ; HL = StrPtr
    ld      hl,ARRAYLEN
    dec     (hl)                  ; ArySiz = ArySiz - 1
    ret
.dofloat:
    ld      hl,(ARRAYPTR)         ; HL = AryPtr
    ld      de,(BUFPTR)           ; DE = StrPtr
    ld      bc,4                  ; Copy 4 byte Float from Array to String
    ldir                          ; HL = AryPtr, DE = StrPtr
    ld      (ARRAYPTR),hl         ; ARRAYPTR = AryPtr
    ld      (BUFPTR),de           ; BUFPTR = StrPtr
    ret
; Copy array element to FACC
.array_to_facc
    ld      hl,(ARRAYPTR)         ; HL = AryPtr
    ld      de,FACLO              ; DstAdr = FACC
    ld      bc,4
    ldir                          ; Do copy
    ld      (ARRAYPTR),hl         ; ARRAYPTR = AryPtr
    ret
