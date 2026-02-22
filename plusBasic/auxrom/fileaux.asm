;=====================================================================================
; Core routines for BASIC File I/O statements and functions
;=====================================================================================

; Called from ST_OPEN
; On entry: DE = StrDsc, HL = TxtPtr
bas_open:
    push    de                    ; Stack = StrDsc, RtnAdr
    SYNCHKT FORTK                 ; Require FOR
; parse mode and set IX to respective dos_open routine
    ld      ix,dos_open_read
    cp      INPUTK
    jr      z,.getdone
    cp      OUTTK                 
    jr      z,.output
    cp      XTOKEN      
    jp      nz,SNERR
    inc     hl
    ld      a,(hl)
    ld      ix,dos_open_append    
    cp      APNDTK                
    jr      z,.getdone 
    SYNCHKT RANTK                 
    SYNCHKT XTOKEN                             
    SYNCHKT DOMTK                 
    ld      ix,dos_open_random
    jr      .done                 
.output
    ld      ix,dos_open_write     
    inc     hl                    
    SYNCHKT PUTTK                 
    byte    $F6                   ; OR over CHRGET 
.getdone
    rst     CHRGET                ; Skip Input
.done
    SYNCHKC 'A'                   ; Require AS
    SYNCHKC 'S'
; Open the file
    ex      (sp),hl               ; HL = StrDsc; Stack = TxtPtr, RtnAdr
    call    jump_ix               ; Open the file
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret     m
    inc     a                     ; A = FilChn
    ret

; On entry: A = FilDsc, F = AscFlg (Z = ASCII), BC = AryLen, HL = AryPtr
bas_save_string_array:
    push    bc                    ; Stack = AryLen, RtnAdr
.strloop
    push    hl                    ; Stack = AryPtr, AryLen, RtnAdr
    push    af                    ; Stack = DscFlg, AryPtr, AryLen, RtnAdr
    call    string_addr_len       ; DE = StrAdr, BC = StrLen
    pop     af                    ; F = DscAsc; Stack = AryPtr, AryLen, RtnAdr
    push    af                    ; Stack = DscFlg, AryPtr, AryLen, TxtPtr, RtnAdr
    jr      z,.skip_len           ; If Not ASCII mode
    call    esp_write_byte        ;   Write string length
.skip_len
    pop     af                    ; F = AscFlag; Stack = AryPtr, AryLen, RtnAdr
    push    af                    ; Stack = AscFlag, AryPtr, AryLen, RtnAdr
    call    esp_write_bytes       ; Write string data
    pop     af                    ; F = AscFlag; Stack = AryPtr, AryLen, RtnAdr
    push    af                    ; Stack = AscFlag, AryPtr, AryLen, RtnAdr
    jr      nz,.skip_crlf         ; If ASCII mode
    jr      c,.skip_cr
    ld      c,13                  
    call    esp_write_byte        ; Write CR
.skip_cr
    ld      c,10                  
    pop     af                    ; F = AscFlag; Stack = AryPtr, AryLen, RtnAdr
    push    af                    ; Stack = AscFlag, AryPtr, AryLen, RtnAdr
    call    esp_write_byte        ; Write LF
.skip_crlf
    pop     af                    ; F = AscFlag; Stack = AryPtr, AryLen, RtnAdr
    pop     hl                    ; HL = AryPtr; Stack = AryLen, RtnAdr
    pop     de                    ; DE = AryLen; Stack = RtnAdr
    ld      b,4
.nextloop
    inc     hl
    dec     de
    djnz    .nextloop
    ex      af,af'                ; F' = DscFlg
    ld      a,d
    or      e
    ret     z
    ex      af,af'                ; F = DscFlg
    push    de                    ; Stack = AryLen, RtnAdr
    jr      .strloop


bas_lookup_prog:
    push    hl                    ; Stack = StrDsc, RtnAdr
    call    string_addr_len       ; DE = StrAdr, BC = StrLen
    call    file_get_ext          ; DE = ExtAdr, A = ExtLen
    pop     hl                    ; HL = StrDsc; Stack = RtnAdr
    ret     nz                    ; Return if extension specified
; Add wildcard
    call    string_addr_len       ; DE = StrAdr, BC = StrLen
    push    bc                    ; Stack = StrLen, RtnAdr
    push    bc                    ; Stack = StrLen, StrLen, RtnAdr
    call    get_strbuf_addr       ; HL = StrBuf
    pop     bc                    ; BC = StrLen; Stack = StrLen, RtnAdr
    push    hl                    ; Stack = StrBuf, StrLen, RtnAdr
    ex      de,hl                 ; DE = StrBuf, HL = StrAdr
    ldir                          ; DE = StrEnd
    pop     hl                    ; HL = StrBuf; Stack = StrLen, RtnAdr
    pop     bc                    ; BC = StrLen; Stack = RtnAdr
    ld      a,'.'
    call    .add_char
    ld      a,'*'                 ; Add .* to filename
    call    .add_char
    ex      de,hl                 ; DE = StrBuf
    call    dos_open_dir
    ret     m
    call    get_strbuf_addr       ; HL = BufAdr
.read_loop:
    push    af                    ; Stack = FilDsc, RtnAdr
    call    dos_read_dir          ; A = Result, B = NamLen, C = EntLen, DE = NamAdr, HL = BufAdr
    jp      c,LSERR               ; Error if overflow
    ret     m

.ret_filename
    pop     af                    ; A = FilDsc
    call    dos_close             ; Close file
    ld      a,b                   ; A = NamLen, DE = NamAdr
    jp      STRAD2                ; Build string descriptor and return

.error
    cp      ERR_EOF               ; If not EOF
    ret     m                     ;   Error out
    pop     af                    ; A = FilDsc
    call    dos_close             ; Close file
    pop     hl                    ; HL = FilDsc; Stack = RtnAdr
    ret

.add_char:
    inc     c                     ; Bump StrLen for *
    jp      z,LSERR               ; Error if > 255
    ld      (de),a                ; Append character to filename
    inc     de                    ; Bump BufPtr
    ret

