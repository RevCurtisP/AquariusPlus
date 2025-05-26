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
