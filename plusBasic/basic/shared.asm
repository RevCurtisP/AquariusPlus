;====================================================================
; Shared BASIC Statement and Function Subroutines
;====================================================================

FLOAT_BC:
    ld      d,b                   ;  Copy into DE
    ld      e,c                   ;  
FLOAT_DE:
    push    hl
    xor     a                     ; Set HO to 0
    ld      (VALTYP),a            ; Force Return Type to numeric
    ld      b,$98                 ; Exponent = 2^24
    call    FLOATR                ; Float It
    pop     hl
    ret

;-----------------------------------------------------------------------------
; Parse @Page
; Output: A, E = Page number`
;         Carry Set if page specified
; Clobbered: BC,D
;-----------------------------------------------------------------------------
get_page_arg:
    cp      '@'                   
    jr      nz,.notat             ; If page prefix
    rst     CHRGET                ;   Skip '@'
    call    GETBYT                ;   Parse byte into E
    ld      a,e
    scf                           ;   Return Carry Set
    ret
.notat
    or      a                     ; Else return Carry Clear
    ret     

;-----------------------------------------------------------------------------
; Parse @Page, Address
; Output: A = Page number`
;         DE = Address
;         Carry Set if page specified
; Clobbered: BC
;-----------------------------------------------------------------------------
get_page_addr:
    call    get_page_arg          ; Get (optional) Page
    push    af                    ; Stack = Page+Flag
    jr      nc,.no_page           ; If Page specified
    SYNCHK  ','                   ;   Require Comma
.no_page:
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
; Parse Byte between 0 - 3
; Output: A,E = Nybble
; Clobbers: A,BC
;-----------------------------------------------------------------------------
getbyte4:
    call    GETBYT                ; get foreground color in e
    cp      4                     ; if > 15
    jp      nc,FCERR              ;   FC Error
    ret

;-----------------------------------------------------------------------------
; Parse Byte 0 - 15
; Output: A,E = Nybble
; Clobbers: A,BC
;-----------------------------------------------------------------------------
get_byte16:
    call    GETBYT                ; get foreground color in e
    cp      16                    ; if > 15
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
; Parse 9 bit Integer
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
; Parse String Variable Name
; Syntax: VAR$ =
; Output: DE = Pointer to variable
; Clobbers: A,BC
;-----------------------------------------------------------------------------
get_stringvar:
    call    PTRGET            ; DE = Pointer to variable
    call    GETYPE            ; If not a string
    jp      nz,TMERR          ;   Type Mismatch error
    jp      CHRGT2            ; Reget Character and Return

move_cursor:
    push    af

    ; Restore character behind cursor
    push    hl
    exx
    ld      hl, (CURRAM)        ; CHRPOS - address of cursor within matrix
    ld      a, (CURCHR)         ; BUFO - storage of the character behind the cursor
    ld      (hl), a             ; Put original character on screen
    pop     hl

    ; Calculate new cursor location
    ld      a, l
    add     a, a
    add     a, a
    add     a, l
    ex      de, hl
    ld      e, d
    ld      d, $00
    ld      h, d
    ld      l, a
    ld      a, e
    dec     a
    add     hl, hl
    add     hl, hl
    add     hl, hl              ; HL is now 40 * rows
    add     hl, de              ; Added the columns
    ld      de, SCREEN          ; Screen character-matrix (= 12288 dec)
    add     hl, de              ; Putting it all together
    jp      TTYFIS              ; Save cursor position and return

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
