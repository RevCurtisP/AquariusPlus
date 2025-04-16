; Core routines for BASIC File I/O statements and functions
    
aqplus_run:
    call    aqplus_open
    ret     nz
    jr      aqplus_run_chunk
    
; On entry: HL = StrDsc
aqplus_open:
    call    _open_read            ; A = FilDsc
    ret     m
; Load File Header
    ld      bc,6
    ld      de,FBUFFR
    call    esp_read_bytes        ; Read aqx header
    ret     m                     ; Return if Error
    ld      a,c                   ; If < 6 bytes read
    cp      6                     ;   Set Carry
    ld      a,l                   ; A = FilDsc
    jr      nz,.not_aqplus
; Check File Header
    ld      de,sig_aqplus
    ld      hl,FBUFFR
    ld      b,6
    call    string_cmp_upper
    ret     z
.not_aqplus
    call    dos_close
_init_fdesc:
    xor     a
    ld      (BAS_FDESC),a
    inc     a                     ; Set NZ
    ret
    
sig_aqplus:
    byte    "AQPLUS"
sig_exec:
    byte    "EXEC"

_open_read:
    call    _init_fdesc
    call    dos_open_read
    ret     m
    inc     a
    ld      (BAS_FDESC),a
    dec     a
    ret


aqplus_run_chunk:
    call    aqplus_read_cheader   ; BC = ReadLen, DE = BufAdr
    ret     m
    ld      hl,sig_exec
    ld      b,4
    call    string_cmp_upper      ; DE = &DatLen
    jr      nz,_badfile
aqplus_run_exec:
    ex      de,hl                 ; HL = &DatLen
    ld      de,FILNAM
    ld      bc,4
    ldir                          ; (FILNAM) = ProgLen
    ld      bc,16                 ; Skip RscName (Carry cleared)
    add     hl,bc                 ; (FILNAM+4) = LoadAdr
    ld      bc,5                  ; (FILNAM+6) = ExecAdr
    ldir                          ; (FILNAM+8) = LoadPg
; Load Program
    ld      ix,esp_read_bytes
    ld      bc,$4000
    ex      af,af'                ; A' = FilDsc
    ld      a,(FILNAM+8)          ; A = Page
    or      a
    jr      z,.nopage
    ld      ix,esp_read_paged
    ld      bc,$C000
.nopage
    ld      hl,(FILNAM+4)         ; HL = LoadAdr
    ld      bc,$4000
    sbc     hl,bc                 ; If LoadAdr < $4000
    jr      c,_badfile            ;   Bad file error
    ld      bc,(FILNAM)           ; BC = ProgLen
    ld      de,(FILNAM+4)         ; DE = LoadAdr
    ld      h,a                   ; H = Page
    ex      af,af'                ; A = FilDsc
    call    jump_ix
    ret     m
    ld      a,l
    call    dos_close
    jp      run_exec              ; Run the program

aqplus_read_cheader:
    call    get_strbuf_addr       ; HL = StrBuf
    push    hl                    ; Stack = StrBuf, RtnAdr
    ex      de,hl                 ; DE = StrBuf
    ld      bc,64
    call    esp_read_bytes
    pop     de                    ; DE = StrBuf; Stack = RtnAdr
    ret     m                     ; Return if Error
    ld      a,c
    cp      64
    ld      a,l
    ret     z
_badfile:
    ld      a,$FF-ERRBDF
    or      a
    ret
