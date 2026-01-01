;=====================================================================================
; Core routines for BASIC statements and functions
;=====================================================================================

; ------------------------------------------------------------------------------
; ------------------------------------------------------------------------------
bas_bin_str:

; ------------------------------------------------------------------------------
; CHECK VER core code
; On entry: BC = ArgLen, DE = ArgAdr, HL = ArgDsc
; ------------------------------------------------------------------------------
aux_check_ver:
        ld      (FILNAF),bc
        ld      (FILNAF+2),de
        ex      de,hl             ; HL = ArgAdr
        call    version_to_long   ; C = Major, D = Minor, E = Suffix
        push    de                ; Stack = ArgLo, RtnAdr
        push    bc                ; Stack = ArgHi, Arglow, RtnAdr
        ld      hl,FBUFFR
        call    bas_get_version   ; HL = VerAdr, BC - VerKen
        call    version_to_long   ; C = Major, D = Minor, E = Suffix
        ld      a,c               ; A = BasHi
        pop     bc                ; C = ArgHi; Stack = ArgLo, RtnAdr
        cp      c
        pop     hl                ; HL = ArgLo; Stack = RtnAdr
        jr      c,.badver         ; If ArgHi < BasHi
        ret     nz       
        ex      de,hl             ; HL = ArgLo, DE = VerLo
        rst     COMPAR            ; and ArgLo <  VerLo
        ret     nc
.badver
        call    get_strbuf_addr   ; HL = BufAdr, RtnAdr
        push    hl                ; Stack = BufAdr
        ex      de,hl             ; DE = BufAdr
        ld      hl,.head
        ld      bc,.headln-1
        ldir                      ; Write head to String Buffer
        ld      bc,(FILNAF)       ; HL = ArgAdr
        ld      hl,(FILNAF+2)     ; BC = ArgLen
        ldir                      ; Write version string to String Buffer
        ld      hl,.tail
        ld      bc,.tailln
        ldir                      ; Write tail to String Buffer
        pop     hl                ; HL = BufAdr; Stack = RtnAdr
        scf
        ret
        
.head   byte    13,10,7,"This program requires",13,10,"plusBASIC ",0
.headln equ     $ - .head
.tail   byte    " or higher.",0
.tailln equ     $ - .tail

; ------------------------------------------------------------------------------
; FN_VER core code
; On entry: BC = StrFlg, FACC, VALTYP = Arg
; In exit: AF = StrFlg, HL = VerAdr or CDE = VerLng
; ------------------------------------------------------------------------------
aux_ver:
    push    af                    ; Stack = StrFlg, RtnAdr
    call    GETYPE                ; A = ArgTyp
    jr      nz,.numarg            ; If string arg
    call    free_addr_len         ;   BC = StrLen, DE = StrAdr, HL = StrDsc
    ex      de,hl                 ;   HL = StrAdr
    jr      .return_ver           ; Else
.numarg
    call    CONINT                ;   A = Arg
    ld      iy,get_system_version ;   If Arg = 0
    or      a                     ;     Getting system version
    jr      z,.getver             ;   Else If Arg = 1
    ld      iy,get_plusbas_version;     Getting plusBASIC version
    cp      1                     ;   Else
    jp      nz,FCERR              ;     Illegal quantity error
.getver
    ld      hl,FBUFFR
    ld      bc,14
    call    jump_iy               ;   HL = VerAdr
.return_ver
    pop     af                    ; AF = StrFlg, Stack = RtnAdr
    ret     z                     ; If string, return A=0, HL = VerAdr
    call    version_to_long       ; Else return A=-1, CDE = VerHsh
    or      $FF                   ;  
    ret

;-----------------------------------------------------------------------------
; Return BASIC Version
; Input: HL: String Buffer address
; Output: BC: Version String Length
; Clobbers: A,DE
;-----------------------------------------------------------------------------
bas_get_version:
    ex      de,hl                 ; DE = BufAdr
    ld      hl,plus_version       ; HL = VerAdr
    call    str_copy              ; Copy VerStr to StrBuf
    ex      de,hl                 ; HL = BufAdr
    ret

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
    inc     hl                    ; Skip KEY
    SYNCHKC 'S'                   ; Require S
    xor     a
    ld      (RESPTR+1),a          ; Reset autotype
    ld      (CHARC),a             ; Clear INKEY buffer
    jp      key_clear_fifo        ; Clear keyboard buffer and return

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
    ld      hl,FLOAT
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
    call    read_mousewdlt        ; A = WheelDelt
    push    af                    ; Stack = WheelDelt, RtnAdr
    call    clear_mousedlt
    pop     af
.signed_byte
    or      a                     ; Signed result
    ret

.update_wdelt
    push    bc
    push    de
    call    read_mousewdlt
    add     l                     ; Accumulate mouse wheel delta
    call    write_mousewdlt
    pop     de
    pop     bc
    ret


      
; Called from FN_OFF
; On entry: C = Column, E = Row
bas_offset:
    ld      a,(LINLEN)
    dec     a                     ; 
    cp      c                     ; If LinLen - 1 < Column
    jp      c,FCERR               ;   Illegal Quantity error
    ld      a,24
    cp      e                     ; If 24 < Row
    jp      c,FCERR               ;   Illegal Quantity error
    ld      d,c                   ; D = Column
    ex      de,hl                 ; H = Column, L = Row
    call    cursor_offset         ; HL = Offset
    ex      de,hl                 ; DE = Offset
    ret

; Called from ST_SET_BIT, ST_RESET_BIT
; On entry: HL = BitNo, CDE = Long
bas_bit_numvar:
    xor     a                     
    or      h                     ; If BitNo > 255            
    jp      nz,FCERR              ;   Illegal Quantity Error
    ld      a,(TEMP8)             ; A = Mode
    or      a                     ; Set Z if SET BIT
    ld      a,l                   ; A = BitNo
    ld      ix,bool_setbit_long
    jr      z,call_ix_fcerr
    ld      ix,bool_resetbit_long
call_ix_fcerr:
    call    jump_ix
    ret     nc
    jp      FCERR
    

; Called from ST_SET_BIT, ST_RESET_BIT
; On entry: HL = VarPtr, DE = BitNo
bas_bit_string:
    push    de                    ; Stack = BitNo, RtnAdr
    call    string_addr_len       ; DE = StrAdr; BC = StrLen
    pop     hl                    ; HL = BitNo
    ld      a,(TEMP8)             ; A = Mode
    or      a                     ; Set Z if SET BIT
    ld      ix,bool_setbit_string
    jr      z,call_ix_fcerr
    ld      ix,bool_resetbit_string
    jr      call_ix_fcerr

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
; (SPLITSEG) = String segment count
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
;  Input: HL: BufPtr
; (SPLITDSC): String descriptor of string to split
; (ARRAYADR): Address of first array element
; (ARRAYLEN): Length of array data
; (ARRAYPTR): Pointer to current array element
; (SPLITDEL): Delimiter to split on 
; (SPLITSEG): String segment count
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

_skipone:
    ld    de,(ARRAYPTR)
    ld    bc,(ARRAYLEN)
bas_skipfirst
    inc   de
    inc   de
    inc   de
    inc   de
    jr    _arrayptr
_clearfirst
    xor     a
    ld      b,4
.skiploop
    ld      (de),a
    inc     de
    djnz    .skiploop
_arrayptr
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


; HEX$(long) core code
bas_hex_long:
    ld      hl,FBUFFR+1           ; HL = TmpBuf
    ld      a,c
    or      a                     ; If high byte <> 0
    jr      z,.middle_byte
    call    aux_byte_to_hex       ;   Convert to hex string
    ld      a,d
    jr      .force_middle
.middle_byte
    ld      a,d
    or      a                     ; If middle byte <> 0
    jr      z,.lower_byte
.force_middle
    call    aux_byte_to_hex       ;   Convert to hex string
.lower_byte
    ld      a,e
    call    aux_byte_to_hex       ; Convert low byte to hex string
    ld      (hl),0                ; Null-terminate string
    ld      hl,FBUFFR+1
    ret

; HEX$(string$) core code
bas_hex_string:
    push    de                    ; Stack = ArgDsc, DmyRtn, TxtPtr
    push    hl                    ; Stack = ArgAdr, ArgDsc, DmyRtn, TxtPtr
    push    af                    ; Stack = ArgLen, ArgAdr, ArgDsc, DmyRtn, TxtPtr
    add     a,a                   ; NewLen = ArgLen * 2
    jp      c,LSERR               ; LS Error if greater than 255
    call    STRINI                ; DE = NewAdr
    pop     af                    ; A = ArgLen; Stack = ArgAdr, ArgDsc, DmyRtn, TxtPtr
    pop     hl                    ; HL = ArgAdr; Stack = ArgDsc, DmyRtn, TxtPtr
    ex      de,hl                 ; DE = ArgAdr, HL = NewAdr
    ld      c,a                   ; A = StrLen
    call    aux_asc_to_hex
    pop     de                    ; DE = ArgDsc; Stack = DmyRtn, TxtPtr
    ret

; Called from read_file
; On entry: A = RecLen, H = Channel 
; Returns: HL = TmpDsc
bas_read_string:
    push    hl                    ; Stack = Channel, RtnAdr
    call    STRINI                ; A = StrLen, HL = StrDsc, DE = StrAdr
    ld      c,a
    ld      b,0                   ; BC = StrLen
    pop     af                    ; A = Channel
    push    hl                    ; Stack = StrDsc, RtnAdr
    call    esp_readc_bytes       
    pop     hl                    ; HL = StrDsc; Stack = RtnAdr
    ld      (hl),c                ; Write bytes read to StrDsc
    ret
    
; Called from write_file
; On entry: DE = StrDsc, H = FilChn
bas_write_string:
    push    hl                    ; Stack = FilDsc, RtnAdr
    ex      de,hl
    call    string_addr_len       ; DE = StrAdr, BC = StrLen
    pop     af                    ; A = FilDsc; Stack = RtnAdr
    jp      esp_writec_bytes      ; Write and return

; Called from SAVE SCREEN
; On entry: A = SaveArgs
;          HL = TxtPtr
; Returns: A = SaveArgs + SaveOpts
; Clobbers: B
bas_save_screen_opts:
    ex      af,af'                ; A' = SaveArgs
    ld      b,0                   ; C = NoOpts
    call    CHRGT2                ; A = CurChr
    jr      z,.done               ; If not terminator
    SYNCHKC ','                   ;   Require comma
    SYNCHKT XTOKEN                ;   Require extended tokeb
    cp      PALETK         
    jr      nz,.not_palette       ;   If PALETTE
    set     7,b                   ;     Set WritePalette bit
    rst     CHRGET                ;     A = NxtChr
    jr      z,.done               ;     If terminator, finish up
    SYNCHKC ','                   ;     Require comma
    SYNCHKT XTOKEN                ;     Require extended tokeb
.not_palette
    SYNCHKT BORDTK                ;   Require BORDERMAP
    SYNCHKT MAPTK
    set     6,b                   ;   Set WriteBorderRemap bit
.done
    ex      af,af'                ; A = SaveArgs
    or      b                     ; A = SaveArgs + SaveOpts
    ret
