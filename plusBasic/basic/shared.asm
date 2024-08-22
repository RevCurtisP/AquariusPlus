;====================================================================
; Shared BASIC Statement and Function Subroutines
;====================================================================

;-----------------------------------------------------------------------------
; Parse to 23-bit integer in CDE
;-----------------------------------------------------------------------------
GET_LONG:
    call    FRMNUM                ; Parse a number

;-----------------------------------------------------------------------------
; Convert FACC to 23-bit integer in CDE
;-----------------------------------------------------------------------------
FRC_LONG:
    ld      a,(FAC)                ; Get Exponent
    cp      153                    ; If < 2^24
    jp      c,QINT                 ;   Convert it
    jp      FCERR                  ; Else Illegal Quantity

;-----------------------------------------------------------------------------
; Parse to 23-bit integer in CDE
;-----------------------------------------------------------------------------
GET_STRING:
    call    FRMEVL                ; Parse a number
    jp      CHKSTR

;-----------------------------------------------------------------------------
; Convert C to an unsigned Floating Point number in FACC
;-----------------------------------------------------------------------------
FLOAT_C:
    ld      a,c
    jp      SNGFLT

;-----------------------------------------------------------------------------
; Convert BC into an unsigned Floating Point number in FACC
;-----------------------------------------------------------------------------
FLOAT_BC:
    ld      d,b                   ;  Copy into DE
    ld      e,c                   ;  
;-----------------------------------------------------------------------------
; Convert DE into an unsigned Floating Point number in FACC
;-----------------------------------------------------------------------------
FLOAT_DE:
    ld      c,0                   ; Set HO to 0
;-----------------------------------------------------------------------------
; Convert CDE into an unsigned Floating Point number in FACC
; Clobbers: A,BC,DE
;-----------------------------------------------------------------------------
FLOAT_CDE:
    push    hl
    xor     a
    ld      (VALTYP),a            ; Force Return Type to numeric
    ld      a,c
    ld      b,$98                 ; Exponent = 2^24
    call    FLOATR                ; Float It
    pop     hl
    ret
 
 
pop_float_minus_one:
    pop     af
float_minus_one:
    ld      a,-1
;-----------------------------------------------------------------------------
; Convert A into a signed Floating Point number in FACC
; Clobbers: A,BC,DE
;-----------------------------------------------------------------------------
float_signed_byte:
    push    hl
    ld      b,0
    ld      c,a
    cp      128                   ; If A < 128
    ld      a,0                   ;   Return Positive Integer
    jr      c,.positive           ; Else
    dec     a                     ;   Return Negative Integer
.positive
    call    GIVINT
    pop     hl
    ret

;-----------------------------------------------------------------------------
; Convert DE into a signed Floating Point number in FACC
; Clobbers: A,BC,DE
;-----------------------------------------------------------------------------
float_signed_int:
    push    hl
    ld      a,d                   ; A = MSB
    ld      d,e                   ; E = LSB
    call    FLOATD
    pop     hl
    ret

get_star_array: 
    call    CHRGT2                ; Reget character
    jp      z,MOERR               ; If terminator, MO Error
    cp      MULTK                 ; If no *
    jp      nz,TMERR              ;   Type mismatch error
skip_star_array:    
    rst     CHRGET                ; Skip *   
;-----------------------------------------------------------------------------
; Parse Array Variable without subscript
; Output: A = Array Type: 0 = String, $FF = Numeric
;        DE = First Byte of Data
;        BC = Length of Data
;-----------------------------------------------------------------------------
get_array: 
    ld      a,1                   ; SEARCH ARRAYS ONLY
    ld      (SUBFLG),a            
    call    PTRGET                ; GET PTR TO ARRAY
    jp      nz,UDERR              ; NOT THERE - ERROR
    ld      (SUBFLG),a            ; CLEAR THIS
    push    hl                    ; SAVE TXTPTR
    ld      h,b                   ; HL = PTR TO ARRAY
    ld      l,c                   
    ex      de,hl                 ; HL = LENGTH
    add     hl,de                 ; HL = LAST BYTE OF ARRAY
    push    hl                    ; SAVE
    ld      a,(bc)                ; GET NO. OF DIMS
    add     a,a                   ; DOUBLE SINCE 2 BYTE ENTRIES
    ld      l,a                   
    ld      h,0                   
    inc     bc                    ; SKIP NO. OF DIMStion
    add     hl,bc                 
    ex      de,hl                 ; DE = PTR TO FIRST BYTE OF DATA
    xor     a                     ; Clear carry
    pop     hl                    ; HL = PTR TO LAST BYTE OF DATA
    sbc     hl,de                 ; HL = Data Length
    ld      b,h
    ld      c,l                   ; BE = Data Length
    pop     hl                    ; HL = TxtPtr
    jp      GETYPE                ; Reread next character and set flags

;Undimensioned Array Error
UDERR:
    ld      e,ERRUD
    jp      ERROR

;-----------------------------------------------------------------------------
; Require Close Paren on Function
;-----------------------------------------------------------------------------
close_paren:
    ld      a,(hl)
    cp      ','
    jp      z,TOERR
    SYNCHK  ')'
    ret

;-----------------------------------------------------------------------------
; Force FACC to an integer
;-----------------------------------------------------------------------------
force_integer:
    call    CHKNUM
    jp      FRCINT

;-----------------------------------------------------------------------------
; If next character is a comma, Too many operands error
;-----------------------------------------------------------------------------
no_more:
    ld      a,(hl)
    cp      ','
    ret     nz
TOERR:
    ld      e,ERRTO
    jp      ERROR

;-----------------------------------------------------------------------------
; Parse foreground and background color arguments
; Output: A: Screen memory byte value
;-----------------------------------------------------------------------------
get_color_args:
    call    get_byte16            ; get foreground color
    push    af                    ; save it
    call    get_comma_byte16      ; get background color
    pop     af                    ; get back foreground color
    or      a                     ; clear carry
    rla       
    rla       
    rla       
    rla                           ; shift to high nybble
    or      e                     ; combine background color
    ret

;-----------------------------------------------------------------------------
; Require extended token
; Input: C: Extended Token
; Output: A: Next Character
;-----------------------------------------------------------------------------
get_pt3_tk:


get_ptrtk:
    ld      c,PTRTK
get_extoken:
    rst     CHRGET                ; Skip VAR
    rst     SYNCHR
    byte    XTOKEN
    cp      c                     ; If NxtChr = Token
    jp      z,CHRGTR              ;   Skip it and return
    jp      SNERR                 ; Else Syntax error


skip_get_comma:
    rst     CHRGET
;-----------------------------------------------------------------------------
; Require comma
;-----------------------------------------------------------------------------
get_comma:
    ld      a,(hl)
    cp      ','
    jp      nz,MOERR
    jp      CHRGTR

;-----------------------------------------------------------------------------
; Require comma, then parse byte
;-----------------------------------------------------------------------------
get_comma_byte:
    call    get_comma
    jp      GETBYT

;-----------------------------------------------------------------------------
; Require comma, then parse integer
;-----------------------------------------------------------------------------
get_comma_int:
    ld      a,(hl)
    cp      ','
    jp      nz,MOERR
;-----------------------------------------------------------------------------
; Skip next character then get integer
;-----------------------------------------------------------------------------
skip_get_int:
    rst     CHRGET
    jp      GETINT

;-----------------------------------------------------------------------------
; Skip character, require (, return expression type
;-----------------------------------------------------------------------------
skip_frmprn_getype:
  rst     CHRGET
  call    FRMPRN
  jp      GETYPE

;-----------------------------------------------------------------------------
; Parse C, X, or Y
; Output: E: 0 = C, 1 = X, 255 = Y
;-----------------------------------------------------------------------------
parse_cxy:
    ld      a,(hl)                ; A = NxtChr
    ld      e,0                   ; E = 0
    cp      'C'
    jr      nz,.not_c             ; If C 
    jp      CHRGTR                ;   Skip C and Return E
.not_c    
    cp      'X'                    
    jr      nz,.not_x             ; If X 
    inc     e                     ;   E = 1
    jp      CHRGTR                ;   Skip X and Return E
.not_x    
    SYNCHK  'Y'                   ; Else require Y
    dec     e                     ;   E = 255
    ret                           ;   Return E

;-----------------------------------------------------------------------------
; Parse @Page
; Output: A, E = Page number`
;         Carry Set if page specified
; Clobbered: BC,D
;-----------------------------------------------------------------------------
get_page_arg:
    cp      '@'                   
    jr      nz,_notat             ; If page prefix
    rst     CHRGET                ;   Skip '@'
    byte    $11                   ;   LD DE, over SYNCHK
req_page_arg:
    SYNCHK  '@'
    call    GETBYT                ;   Parse byte into E
    ld      a,e
setcarry_ret:
    scf                           ;   Return Carry Set
    ret
_notat
    or      a                     ; Else return Carry Clear
    ret     

;-----------------------------------------------------------------------------
; Parse @Page, Address
; Output: A = Page number`
;         DE = Address
;         Carry Set if page specified
; Clobbered: BC
;-----------------------------------------------------------------------------
get_comma_page_addr:
    ld      a,(hl)
    cp      ','
    jp      nz,MOERR
    rst     CHRGET
get_page_addr:
    call    get_page_arg          ; Get (optional) Page
    push    af                    ; Stack = Page+Flag
    jr      nc,_no_page           ; If Page specified
    SYNCHK  ','                   ;   Require Comma
    call    get_int16k            ;   DE = Address
    pop     af                    ; AF = Page+Flag
    ret
_no_page:
    call    GETINT                ; DE = Address
    pop     af                    ; AF = Page+Flag
    ret

;-----------------------------------------------------------------------------
; Parse @Page, Address, Length
; Output: A = Page number`
;         BC = Length
;         DE = Address
;         Carry Set if page specified
;-----------------------------------------------------------------------------
get_page_addr_len:
    call    get_page_addr         ; Get Page and Address
    push    af                    ; Stack = Page+Flag
    push    de                    ; Stack = Address, Page+Flag
    SYNCHK  ','                   ; Require Comma
    call    GETINT                ; Get Length
    ld      b,d
    ld      c,e                   ; BC = Length
    pop     de                    ; DE = Address, Stack = Page+Flag
    pop     af                    ; AF = Page+Flag
    ret
    
;-----------------------------------------------------------------------------
; Parse Address, Length
; Output: BC = Length
;         DE = Address
;         Carry Set if page specified
; Clobbers: A
;-----------------------------------------------------------------------------
get_addr_len:
    call    GETINT                ; DE = Address
    push    de                    ; Stack = Address, Page+Flag
    SYNCHK  ','                   ; Require Comma
    call    GETINT                ; Get Length
    ld      b,d
    ld      c,e                   ; BC = Length
    pop     de                    ; DE = Address, Stack = Page+Flag
    ret


;-----------------------------------------------------------------------------
; Parse Byte Operand followed by optional TO and second operand
; Output: A,D = From Operand
;         E = To Operand
;         Carry Set if no TO operand
;-----------------------------------------------------------------------------
get_byte_range:
    call    GETBYT                ; A = FrmOpd
    ld      d,a                   ; D = FrmOpd
    ld      e,a                   ; E = FrmOpd
    ld      a,(hl)                ; A = NxtChr
    cp      TOTK                  ; If not TO
    ld      a,d                   ;   A = FrmOpd
    jr      nz,setcarry_ret       ;   Return Carry Set
    push    de                    ; Stack = FrmOpd, RtnAdr
    rst     CHRGET                ; Skip TO
    call    GETBYT                ; A = ToOpd
    pop     de                    ; D = FrmOpd; Stack = RtnAdr
    cp      d                     ; If ToOpd < FrmOpd
    jr      c,BRERR               ;   Bad range error
    ld      e,a                   ; E = ToOpd
    ld      a,d                   ; A = FrmOpd
    or      a                     ; Clear Carry
    ret
    
; Substring out of range error
BRERR:
    ld      e,ERRBR
    jp      ERROR
    
;-----------------------------------------------------------------------------
; Parse Optional Byte Operand
; Output: A = Operand (terminator or comma if none)
;         Carry Set if no argument (terminator or skipped comma)
;-----------------------------------------------------------------------------
get_byte_optional:
    push  bc                      ; Save BC
    push  de                      ; Save DE
    call  CHRGT2                  ; If terminator
    jr    z,.scf_ret              ;   Set carry and return
    cp    ','                     ; If comma
    jr    z,.skip_scf_ref         ;   Eat it, set carry and return
    call  GETBYT                  ; Parse the operand
    ld    a,(hl)                  ; Get character after operand
    cp    ','                     ; If it's a comma
    call  z,CHRGTR                ;   Skip it
    ld    a,e                     ; A = Operand
    or    a                       ; Clear carry and set flags
    byte  $01                     ; LD BC over rst and scf
.skip_scf_ref
    rst   CHRGET                  ; Skip comma
.scf_ret
    scf                           ; Set Carry
    pop   de                      ; Restore DE
    pop   bc                      ; Restore BC
    ret


;-----------------------------------------------------------------------------
; Parse Byte between 0 - 1
; Output: A,E = Nybble
; Clobbers: A,BC
;-----------------------------------------------------------------------------
get_byte2:
    call    GETBYT                ; get foreground color in e
    cp      2                     ; if > 15
    jp      nc,FCERR              ;   FC Error
    ret

;-----------------------------------------------------------------------------
; Parse Byte between 0 - 3
; Output: A,E = Nybble
; Clobbers: A,BC
;-----------------------------------------------------------------------------
get_byte4:
    call    GETBYT                ; get foreground color in e
    cp      4                     ; if > 15
    jp      nc,FCERR              ;   FC Error
    ret

get_comma_byte16:
    call    get_comma             ; MO error if no comma
;-----------------------------------------------------------------------------
; Parse Byte 0 - 15
; Output: A,E = Nybble
; Clobbers: A,BC
;-----------------------------------------------------------------------------
get_byte16:
    call    GETBYT                ; get foreground color in e
    cp      16                    ; if > 15
    jp      nc,FCERR              ;   FC Error
    or      a                     ; set flags
    ret

;-----------------------------------------------------------------------------
; Parse Byte 0 - 31
; Output: A,E = Nybble
; Clobbers: BC
;-----------------------------------------------------------------------------
get_byte32:
    call    GETBYT                ; get foreground color in e
    cp      32                    ; if > 15
    jp      nc,FCERR              ;   FC Error
    or      a
    ret

;-----------------------------------------------------------------------------
; Parse Byte 0 - 15
; Output: A,E = Nybble
; Clobbers: BC
;-----------------------------------------------------------------------------
get_byte64:
    call    GETBYT                ; get foreground color in e
    cp      63                    ; if > 15
    jp      nc,FCERR              ;   FC Error
    ret

;-----------------------------------------------------------------------------
; Parse Byte 0 - 199
; Output: A,E = Byte
; Clobbers: A,BC
;-----------------------------------------------------------------------------
get_byte200:
    call    GETBYT                ; get foreground color in e
    cp      200                   ; if > 1200
    jp      nc,FCERR              ;   FC Error
    ret

;-----------------------------------------------------------------------------
; Parse Optional Character after comma as byte or string
;  Input: C = Default character
; Output: A,C = Character ASCII value
; Clobbers: BC,DE
;-----------------------------------------------------------------------------
get_char_optional:
    ld      a,(hl)                ; Get current character
    cp      ','                   ; If not comma
    ld      a,c                   ;   
    ret     nz                    ;   Return default character
skip_get_char:
    rst     CHRGET                ; Skip character before expression
;-----------------------------------------------------------------------------
; Parse Character as byte or string
; Output: A,C = Character ASCII value
; Clobbers: B,DE
;-----------------------------------------------------------------------------
get_char:
    call    FRMEVL                ; Evaluate Character
    call    GETYPE
    jr      z,.string             ; If numeric
    call    CONINT                ;   Convert to byte
    ret                           ; Else
.string
    push    hl                    ; Stack = TxtPtr, RtnAdr
    call    free_addr_len         ; BC = StrLen, DE = StrAdr
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    xor     a
    or      c
    jp      z,FCERR               ; Error if LEN = 0
    ld      a,(de)                ;   Get first character
    ld      c,a
    ret

;-----------------------------------------------------------------------------
; Parse 9 bit Integer
; Output: DE = Integer
; Clobbers: A,BC
;-----------------------------------------------------------------------------
get_int512:
    call    GETINT                ; Get integer
    ld      a,d
    cp      2                     ; If not 0-511
    ret     c
    jp      FCERR

;-----------------------------------------------------------------------------
; Parse 12 bit Integer
; Output: DE = Integer
; Clobbers: A,BC
;-----------------------------------------------------------------------------
get_int4096:
    call    GETINT                ; Get integer
    ld      a,d
    cp      16                     ; If not 0-4095
    ret     c
    jp      FCERR

;-----------------------------------------------------------------------------
; Parse Integer between 0 and 16383
; Output: DE = Integer
; Clobbers: A,BC
;-----------------------------------------------------------------------------
get_int16k:
    call    GETINT                ; Get integer
    ld      a,d
    cp      64                    ; If not 0-4095
    ret     c
    jp      FCERR
    
    
;-----------------------------------------------------------------------------
; Parse Integer and reget next character
; Output: A = Next character
;        DE = Integer
; Flags set as CHRGET
; Clobbered: BC
;-----------------------------------------------------------------------------
get_int_reget:
    call    GETINT
    jp      CHRGT2

; Returns Z if BITMAPB, NZ if BITMAPC
parse_bitmap:
    rst     SYNCHR
    byte    BITTK
    rst     SYNCHR                ; Require BITMAP
    byte    MAPTK
    cp      'B'
    jr      nz,.notb              ; If BITMAPB
    rst     CHRGET                ;   Skip B
    xor     a                     ;   and return Z
    ret
.notb    
    SYNCHK  'C'                   ; Else Require C
    or      $FF                   ;   and return NZ
    ret

; Parse COLOR color
parse_color:
    rst     SYNCHR                
    byte    COLTK
    rst     SYNCHR                ; Require COLOR
    byte    ORTK
    jp      get_byte16

; Parse COLOR fgcolor,bgcolor
parse_colors:
    rst     SYNCHR                
    byte    COLTK
    rst     SYNCHR                ; Require COLOR
    byte    ORTK
    byte    $01                   ; LD BC over SYNCHK
get_comma_colors:
    SYNCHK  ','
;-----------------------------------------------------------------------------
; Parse Foreground and Background Colors
; Output: A = Combined coloe
; Clobbers: BC, DE
;-----------------------------------------------------------------------------
; ToDo: eliminate get_color_args and call this instead
get_screen_colors:
    call    get_byte16            ; A = FColor
    sla     a                     ;
    sla     a                     ;
    sla     a                     ;
    sla     a                     ;
    push    af                    ; Stack = FColor, Char, Cols, Rows, RtnAdr
    call    get_comma_byte16      ; Require comma
    pop     bc                    ; D = FColor; Stack = Char, Cols, Rows, RtnAdr
    or      b                     ; A = Colors    
    ret

;-----------------------------------------------------------------------------
; Parse ON or OFF
; Output: A = $FF for ON, 0 for OFF
; Clobbers: BC, DE
;-----------------------------------------------------------------------------
get_on_off:
    rst     CHRGET                ; Get argument
check_on_off:
    cp      ONTK                  ;  
    jr      nz,.not_on            ; If ON
    rst     CHRGET                ;   Skip it
    or      $FF                   ;   Return $FF with flags set
    ret 
.not_on
    rst     SYNCHR
    byte    XTOKEN                
    rst     SYNCHR                ; Else
    byte    OFFTK                 ;   Require OFF
    xor     a                     ;   Return 0 with flags set
    ret

skip_get_stringvar:
    rst     CHRGET
;-----------------------------------------------------------------------------
; Parse String Variable Name
; Syntax: VAR$
; Output: DE = Pointer to variable
; Clobbers: A,BC
;-----------------------------------------------------------------------------
get_stringvar:
    call    PTRGET            ; DE = Pointer to variable
    call    GETYPE            ; If not a string
    jp      nz,TMERR          ;   Type Mismatch error
    jp      CHRGT2            ; Reget Character and Return

; ------------------------------------------------------------------------------
; Increment XTEMP0
; Clobbered: A
; ------------------------------------------------------------------------------
inc_xtemp0:
    ld      a,(XTEMP0)
    inc     a                     ; SptlCnt += 1
    ld      (XTEMP0),a            ; XTEMP0 = SptlCnt
    ret

; ------------------------------------------------------------------------------
; Increment text pointer, push it then LABBCK
; ------------------------------------------------------------------------------
push_hlinc_labbck:
    inc     hl                ; Skip current character
; ------------------------------------------------------------------------------
; Push text pointer then LABBCK
; ------------------------------------------------------------------------------
push_hl_labbck:
    ex      (sp),hl           ; HL = RtnAdr; Stack = TxtPtr
    push    hl                ; Stack = RtnAdr, TxtPtr
    ld      hl,LABBCK         ; HL = LABBCK
    ex      (sp),hl           ; HL = RtnAdr; Stack = LABBCK, TxtPtr 
    jp      (hl)              ; Fast Return

;-----------------------------------------------------------------------------
; Scan Rectangular Coordinates
; Input: HL: Text Pointer
; Output B: Start Column
;        C: End Column  
;        D: Start Row
;        E: End Row
;-----------------------------------------------------------------------------
scan_rect:
    call    SCAND                 ; C = BgnCol, E = BgnRow
    push    de                    ; Stack = BgnRow, RtnAdr
    push    bc                    ; Stack = BgnCol, BgnRow, RtnAdr
    rst     SYNCHR                ; Require -
    byte    MINUTK                 
    call    SCAND                 ; C = EndCol, E = EndRow
    ex      (sp),hl               ; L = BgnCol; Stack = TxtPtr, BgnRow, RtnAdr
    ld      b,l                   ; B = BgnCol, C = EndCol
    pop     hl                    ; HL = TxtPtr; Stack = BgnRow, RtnAdr
    ex      (sp),hl               ; L = BgnRow; Stack = TxtPtr, RtnAdr
    ld      d,l                   ; D = BgnRow, C = EndRow
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; Create temporary string from string buffer
;  Input: A = Buffer data length
; Output: BC = StrLen
;         DE = StrAdr
;         HL = StrDsc
; Clobbers: A
;-----------------------------------------------------------------------------
strbuf_temp_str:
    or      a                     ; If length = 0 
    jp      z,pop_ret_nullstr     ;   Return null string           
    call    STRINI                ; Create temporary string
    ld      c,a                   ;   A = StrLen, HL = StrDsc, DE = StrAdr
    ld      b,0                   ; BC = StrLen
    push    hl                    ; Stack = StrDsc, RtnAdr
    push    de                    ; Stack = StrAdr, StrDsc, RtnAdr
    push    bc                    ; Stack = StrLen, StrAdr, StrDsc, RtnAdr
    push    bc                    ; Stack = StrLen, StrLen, StrAdr, StrDsc, RtnAdr
copy_strbuf:
    call    get_strbuf_addr       ; HL = StrBuf
    pop     bc                    ; BC = StrLen; Stack = StrLen, StrAdr, StrDsc, RtnAdr
    ldir                          ; Copy BC bytes from StrBuf to StrAdr
    pop     bc                    ; BC = StrLen; Stack = StrAdr, StrDsc
    pop     de                    ; DE = StrAdr; Stack = StrDsc
    pop     hl                    ; HL = StrDsc
    ret

write_bcde2hl:
    ld      (hl),c
    inc     hl
    ld      (hl),b                ; Write StrLen to array entry
    inc     hl
    ld      (hl),e
    inc     hl
    ld      (hl),d                ; Write StrAdr to array entry
    inc     hl
    ret
