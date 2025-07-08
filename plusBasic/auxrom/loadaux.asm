;====================================================================
; BASIC LOAD Statement Auxillary ROM routines
;====================================================================

;-----------------------------------------------------------------------------
; Load BC bytes of CAQ array file to address DE
; Input: A: File descriptor
;       BC: Array data length
;       DE: Array 
; Clobbered registers: A, DE, HL
;-----------------------------------------------------------------------------
; CLEAR:DIM A(9):LOAD "/t/array.caq",*A
; SET FILE ERROR OFF:CLEAR:DIM A(9):LOAD "/t/array.caq",*A:PRINT ERR
; SET FILE ERROR OFF:CLEAR:DIM A(9):LOAD "ax",*A:PRINT ERR
aux_load_caq_array:
    call   _open_read
    ret     m
    push    iy
    push    af                    ; Stack = FilDsc, RtnAdr
    push    bc                    ; Stack = AryLen, FilDsc, RtnAdr
    push    de                    ; Stack = AryAdr, AryLen, FilDsc, RtnAdr
    call    _check_sync_bytes     ; If sync bytes don't match
    jp      m,_load_array_error   ;   Return error
    call    _check_filnam         ; If CAQ filename <> "######"
    jp      m,_load_array_error   ;   Return error
    ; Load data into array
    pop     de                    ; DE = AryAdr; Stack = AryLen, FilDsc, RtnAdr
    pop     bc                    ; BC = AryLen; Stack = FilDsc, RtnAdr
    call    esp_read_bytes
    xor     a                     ; Return no error
    jp      _load_array_done

;-----------------------------------------------------------------------------
; Load Binary String File into String Array
; Input: A: File descriptor
;       BC: Array data length
;       DE: Array 
; Clobbered registers: A, DE, HL
;-----------------------------------------------------------------------------
; CLEAR:DIM A$(9):LOAD "/t/array.strl",*A$
; SET FILE ERROR OFF:CLEAR:DIM A$(9):LOAD "/t/array.strl",*A$:PRINT ERR
; SET FILE ERROR OFF:CLEAR:DIM A$(9):LOAD "ax",*A$:PRINT ERR
aux_load_str_array:
    call    _open_read
    ret     m
    push    iy
    push    af                    ; Stack = FilDsc, RtnAdr
    push    bc                    ; Stack = AryLen, FilDsc, RtnAdr
    push    de                    ; Stack = AryAdr, AryLen, FilDsc, RtnAdr
.loop
    call    esp_readc_byte        ; B = StrLen
    jp      m,_load_array_error
    ex      af,af'                ; A' = FilDsc
    ld      a,b
    push    af                    ; Stack = StrLen, AryPtr, AryLen, FilDsc, RtnAdr
    call    GETSPA                ; DE = StrAdr
    call    FRETMS                ; Free temporary but not string space
    pop     af                    ; A = StrLen; Stack = AryPtr, AryLen, FilDsc, RtnAdr
    ld      c,a
    ld      b,0                   ; BC = StrLen
    push    de                    ; Stack = StrAdr, AryPtr, AryLen, FilDsc, RtnAdr
    ex      af,af'                ; A = FilDsc
    call    esp_readc_bytes       ; BC = StrLen
    ex      af,af'                ; A' = FilDsc
    pop     de                    ; DE = StrAdr; Stack = AryPtr, AryLen, FilDsc, RtnAdr
    pop     hl                    ; HL = AryPtr; Stack = AryLen, FilDsc, RtnAdr
    call    aux_bcde2hl           ; Write string descriptor
    pop     bc                    ; BC = AryLen; Stack = FilDsc, RtnAdr
    dec     bc
    dec     bc
    dec     bc
    dec     bc
    ld      a,b
    or      c
    jp      z,_load_array_done
    push    bc                    ; Stack = AryLen, FilDsc, RtnAdr
    push    hl                    ; Stack = AryPtr, AryLen, FilDsc, RtnAdr
    ex      af,af'                ; A = FilDsc
    jr      .loop

;-----------------------------------------------------------------------------
; Load ASCII Text File into String Array
; Input: A: File descriptor
;       BC: Array data length
;       DE: Array 
; Clobbered registers: A, DE, HL
;-----------------------------------------------------------------------------
; CLEAR:DIM A$(5): LOAD "/lipsum.txt",*A$,ASC
; SET FILE ERROR OFF:CLEAR:DIM A$(5):LOAD "/lipsum.txt",*A$,ASC:PRINT ERR
; SET FILE ERROR OFF:CLEAR:DIM A$(5):LOAD "/ax",*A$,ASC:PRINT ERR
aux_load_asc_array:
    call    _open_read
    ret     m
    push    iy
    ld      iy,espx_read_line
; Read lines into array starting at second entry
; First entry contains number of lines read
_asc_array:
    push    af                    ; Stack = FilDsc, RtnAdr
    push    de                    ; Stack = AryAdr, FilDsc, RtnAdr
    inc     de
    inc     de
    inc     de
    inc     de                    ; AryPtr = Second element of array
    srl     b
    rr      c
    srl     b
    rr      c                     
    dec     bc                    ; ArySiz = AryLen / 4 -1
    push    bc                    ; Stack = ArySiz, AryAdr, FilDsc, RtnAdr
    push    af                    ; Stack = FilDsc, ArySiz, AryAdr, FilDsc, RtnAdr
    push    bc                    ; Stack = AryCnt, FilDsc, ArySiz, AryAdr, FilDsc, RtnAdr
    push    de                    ; Stack = AryPtr, AryCnt, FilDsc, ArySiz, AryAdr, FilDsc, RtnAdr
.loop:
    call    get_strbuf_addr       ; HL = StrBuf, BC = BufLen
    call    jump_iy               ; Read line from file, BC = StrLen
    jp      m,.error
    ld      de,0                  ; StrAdr = 0 for empty lines
    ld      b,0                   ; Clear length MSB
    ld      a,c                   ; A = LinLen
    or      a
    jr      z,.empty
    push    hl                    ; Stack = BufAdr, AryPtr, AryCnt, FilDsc, ArySiz, AryAdr, FilDsc, RtnAdr
    push    bc                    ; Stack = LinLen, BufAdr, AryPtr, AryCnt, FilDsc, ArySiz, AryAdr, FilDsc, RtnAdr
    call    GETSPA                ; DE = StrAdr
    call    FRETMS                ; Free temporary but not string space
    pop     bc                    ; BC = LinLen; Stack = BufAdr, AryPtr, AryCnt, FilDsc, ArySiz, AryAdr, FilDsc, RtnAdr
    pop     hl                    ; HL = BufAdr; Stack = AryPtr, AryCnt, FilDsc, ArySiz, AryAdr, RtnAdr
    push    de                    ; Stack = StrAdr, AryPtr, AryCnt, FilDsc, ArySiz, AryAdr, RtnAdr
    push    bc                    ; Stack = LinLen, StrAdr, AryPtr, AryCnt, FilDsc, ArySiz, AryAdr, FilDsc, RtnAdr
    ldir                          ; Copy StrBuf to TmpStr
    pop     bc                    ; BC = LinLen; Stack = StrAdr, AryPtr, AryCnt, FilDsc, ArySiz, AryAdr, FilDsc, RtnAdr
    pop     de                    ; DE = StrAdr; Stack = AryPtr, AryCnt, FilDsc, ArySiz, AryAdr, FilDsc, RtnAdr
.empty
    pop     hl                    ; HL = AryPtr; Stack = AryCnt, FilDsc, ArySiz, AryAdr, FilDsc, RtnAdr
    call    aux_bcde2hl           ; Write string descriptor
    pop     bc                    ; BC = AryCnt; Stack = FilDsc, ArySiz, AryAdr, FilDsc, RtnAdr
    dec     bc                    ; Decrement AryCnt
    ld      a,b
    or      c
    jr      z,.done
    pop     af                    ; A = FilDsc, Stack = ArySiz, AryAdr, FilDsc, RtnAdr
    push    af                    ; Stack = FilDsc, ArySiz, AryAdr, FilDsc, RtnAdr
    push    bc                    ; Stack = AryCnt, FilDsc, ArySiz, AryAdr, FilDsc, RtnAdr
    push    hl                    ; Stack = AryPtr, AryCnt, FilDsc, ArySiz, AryAdr, FilDsc, RtnAdr
    jr      .loop
.error
    cp      ERR_EOF               
    jp      nz,_load_asc_ary_error               
    pop     hl                    ; Stack = AryCnt, FilDsc, ArySiz, AryAdr, FilDsc, RtnAdr
    pop     bc                    ; BC = AryCnt; Stack = FilDsc, ArySiz, AryAdr, FilDsc, RtnAdr
.done
    pop     af                    ; AF = FilDsc; Stack = ArySiz, AryAdr, FilDsc, RtnAdr
    pop     hl                    ; HL = ArySiz; Stack = AryAdr, FilDsc, RtnAdr
    sbc     hl,bc                 ; HL = LinCnt (ArySiz - AryCnt)
    ex      de,hl                 ; DE = LinCnt
    xor     a
    ld      b,$98
    call    FLOATR                ; Float LinCnt
    call    FOUT                  ; Convert to string
    ld      de,FBUFFR+1           ; DE = FltAdr
    call    str_length            ; A, BC = FltLen
    push    bc                    ; Stack = FltLen, AryAdr, FilDsc, RtnAdr
    push    de                    ; Stack = FltAdr, FltLen, AryAdr, FilDsc, RtnAdr
    push    bc                    ; Stack = FltLen, FltAdr, FltLen, AryAdr, FilDsc, RtnAdr
    call    GETSPA                ; Allocate string space
    call    FRETMS                ; DE = StrAdr
    pop     bc                    ; BC = FltLen; Stack = FltAdr, FltLen, AryAdr, FilDsc, RtnAdr
    pop     hl                    ; HL = FltAdr; Stack = FltLen, AryAdr, FilDsc, RtnAdr
    push    de                    ; Stack = StrAdr, FltLen, AryAdr, FilDsc, RtnAdr
    ldir                          ; Copy from FBUFFR to string space
    pop     de                    ; DE = StrAdr; Stack = FltLen, AryAdr, FilDsc, RtnAdr
    pop     bc                    ; BC = FltLen; Stack = AryAdr, FilDsc, RtnAdr
    pop     hl                    ; HL = AryAdr; Stack = FilDsc, RtnAdr
    call    aux_bcde2hl           ; Write string descriptor
    xor     a                     ; Return no error
    jr      _load_array_done
; Stack = AryPtr, AryCnt, FilDsc, ArySiz, AryAdr, FilDsc, RtnAdr
_load_asc_ary_error:
    pop     de                    ; Stack = AryCnt, FilDsc, ArySiz, AryAdr, FilDsc, RtnAdr
    pop     de                    ; Stack = FilDsc, ArySiz, AryAdr, FilDsc, RtnAdr
    pop     de                    ; Stack = ArySiz, AryAdr, FilDsc, RtnAdr
_load_array_error:
    pop     de                    ; Stack = AryLen, FilDsc, RtnAdr
    pop     de                    ; Stack = FilDsc, RtnAdr
_load_array_done:
    ex      af,af'                ; AF' = Result
    pop     af                    ; A = FilDsc; Stack = RtnAdr
    pop     iy
    call    jump_iy       
    ex      af,af'                ; AF = Result
    ret

;-----------------------------------------------------------------------------
; Load Directory into String Array
; Input: A: File descriptor
;       BC: Array data length
;       DE: Array 
; Clobbered registers: A, DE, HL
;-----------------------------------------------------------------------------
; 10 CLEAR 4096:DIM A$(99):LOAD DIR *A$
; 20 FOR I=1 TO VAL(A$(0)):PRINT A$(I):NEXT
;
; 10 CLEAR 4096:DIM A$(99):LOAD DIR *A$,ASC
; 20 FOR I=1 TO VAL(A$(0)):PRINT A$(I):NEXT
;
; 10 CLEAR 4096:DIM A$(99):LOAD DIR *A$,BIN:FOR I=1 TO VAL(A$(0))
; 20 PRINT HEX$(LEFT$(A$(I),9));" ";MID$(A$(I),10):NEXT
;
; CLEAR 4096:DIM A$(99):LOAD DIR "/t",*A$
; SET FILE ERROR OFF:CLEAR 4096:DIM A$(99):LOAD DIR "*",*A$:PRINT ERR
; SET FILE ERROR OFF:CLEAR 4096:DIM A$(99):LOAD DIR "xa",*A$:PRINT ERR
; 10 CLEAR 4096:DIM A$(99):LOAD DIR *A$,ASC
; CLEAR 4096:DIM A$(99):LOAD DIR "/t",*A$,ASC
; SET FILE ERROR OFF:CLEAR 4096:DIM A$(99):LOAD DIR "*",*A$,ASC:PRINT ERR
; SET FILE ERROR OFF:CLEAR 4096:DIM A$(99):LOAD DIR "xa",*A$,ASC:PRINT ERR
aux_load_dir_bin:
    call    _open_dir
    ret     m
    push    iy
    ld      iy,dos_read_dir
    jp      _asc_array

; CLEAR 4096:DIM A$(99):LOAD DIR "",*A$,ASC
aux_load_dir_ascii:
;_load_dir:
    call    _open_dir
    ret     m
    push    iy
    ld      iy,file_read_dir_asc
    jp      _asc_array

; CLEAR 4096:DIM A$(99):LOAD DIR "",*A$,ASC
aux_load_dir:
    call    _open_dir
    ret     m
    push    iy
    ld      iy,file_read_dir
    jp      _asc_array



; Check array filename sequence 
; Input: HL: Filename
_check_filnam:
    push    af
    ld      bc,6                 
    ld      de,FILNAM
    call    esp_readc_bytes
    ld      b, 6
    ld      de,FILNAM
.filnam:
    ld      a, (de)
    cp      '#'
    jr      nz,_badfile
    inc     de
    djnz    .filnam
    jr      _goodfile

aux_open_dir:
    call    dos_open_dir
    ret     m
    push    af                    ; Stack = DirSpc, TxtPtr, RtnAdr
    inc     a
    or      $80
    jr      _set_fdesc

_open_dir:
    ld      iy,dos_close_dir
    call    dos_open_dir          ; A = FilSpc
    jr      _open_cont
_open_read:
    ld      iy,dos_close
    call    dos_open_read         ; A = FilSpc
_open_cont:
    ret     m
    push    af                    ; Stack = FilSpc, AuxCall, TxtPtr, RtnAdr
    inc     a
_set_fdesc:
    ld      (BAS_FDESC),a         ; Save FilDsc
    pop     af                    ; A = FilSpc; Stack = AuxCall, TxtPtr, RtnAdr
    ret

;-----------------------------------------------------------------------------
; Check for sync sequence (12x$FF, 1x$00)
;-----------------------------------------------------------------------------
_check_sync_bytes:
    push    af                    ; Stack = FilDsc, RtnAdr
    ld      bc,13
    ld      de,FBUFFR
    call    esp_readc_bytes       ; Read 13 bytes into FBUFFR
    ld      a,c
    cp      13
    jr      nz,_badfile
    ld      b,12                  ; Check for 12x$FF
    ld      de,FBUFFR
.ffloop:
    ld      a, (de)
    cp      $FF
    jr      nz,_badfile
    inc     de
    djnz    .ffloop
    ld      a,(de)                ; Check for 0 byte
    or      a
    jr      nz,_badfile
_goodfile:
    pop     af                    ; A = FilDsc; Stack = RtnAdr
    or      a                     ; Set flags
    ret
_badfile:
    pop     af                    ; Stack = RtnAdr
    ld      a,$FF-ERRBDF
    or      a
    ret

aux_bcde2hl:
    ld      (hl),c
    inc     hl
    ld      (hl),b                ; Write StrLen to array entry
    inc     hl
    ld      (hl),e
    inc     hl
    ld      (hl),d                ; Write StrAdr to array entry
    inc     hl
    ret

