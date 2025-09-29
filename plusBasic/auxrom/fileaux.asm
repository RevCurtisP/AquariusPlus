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
    push    af                    ; Stack = FilChn, TxtPtr, RtnAdr
; parse and po pulate variable
    call    PTRGET                ; DE = VarPtr
    call    CHKNUM                ; Error if not numeric
    pop     af                    ; A = FilChn
    push    hl                    ; Stack = TxtPtr, RtnAdr
    push    de                    ; Stack = VarPtr, TxtPtr, RtnAdr
    call    SNGFLT                ; FACC = FilChn
    pop     hl                    ; HL = VarPtr; Stack = TxtPtr, RtnAdr
    call    MOVMF                 ; Copy FilChn to Ver
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    xor     a                     ; Return no errors
    ret

; On entry: F = AscFlg (Z = ASCII), BC = AryLen, HL = AryPtr
bas_save_string_array:
    push    bc                    ; Stack = AryLen, RtnAdr
.strloop
    push    hl                    ; Stack = AryPtr, AryLen, RtnAdr
    push    af                    ; Stack = AscFlg, AryPtr, AryLen, RtnAdr
    call    string_addr_len       ; DE = StrAdr, BC = StrLen
    pop     af                    ; F = AscFlag; Stack = AryPtr, AryLen, RtnAdr
    push    af                    ; Stack = AscFlg, AryPtr, AryLen, TxtPtr, RtnAdr
    jr      z,.skip_len           ; If Not ASCII mode
    call    esp_write_byte        ;   Write string length
.skip_len
    call    esp_write_bytes       ; Write string data
    pop     af                    ; F = AscFlag; Stack = AryPtr, AryLen, RtnAdr
    push    af                    ; Stack = AscFlag, AryPtr, AryLen, RtnAdr
    jr      nz,.skip_crlf         ; If ASCII mode
    jr      c,.skip_cr
    ld      c,13                  
    call    esp_write_byte        ; Write CR
.skip_cr
    ld      c,10                  
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
    ex      af,af'                ; F' = AscFlg
    ld      a,d
    or      e
    ret     z
    ex      af,af'                ; F = AscFlg
    push    de                    ; Stack = AryLen, RtnAdr
    jr      .strloop
