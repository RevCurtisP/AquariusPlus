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
    ld      bc,16
    add     hl,bc                 ; Skip RscName (Carry cleared)
    ld      bc,4                  ; (FILNAM+4) = LoadAdr
    ldir                          ; (FILNAM+6) = ExecAdr
; Load Program
    ld      hl,(FILNAM+4)         ; HL = LoadAdr
    ld      bc,$4000
    sbc     hl,bc                 ; If LoadAdr < $4000
    jr      c,_badfile            ;   Bad file error
    ld      bc,(FILNAM)           ; BC = ProgLen
    ld      de,(FILNAM+4)         ; DE = LoadAdr
    call    esp_read_bytes
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



;    ld      de,FBUFFR
;    ld      bc,10
;    call    esp_read_bytes        ; (FBUFFR) = RsrcLen
;    ret     m
;    ex      af,af'                ; A' = FilDsc
;    ld      a,c
;    cp      10                    ; If < 10 bytes read
;    ret     c                     ;   Return Carry Set
;    ld      hl,(_resource_len)    ; HL = RsrcLen
;    ld      de,6
;    sbc     hl,de
;    ld      b,h                   ; BC = ProgLen
;    ld      c,l
;    ld      de,(_load_addr)
;    call    esp_read_bytes        ; DE = EndAdr
;    ld      a,l                   ; L = FilDsc
;    call    dos_close             ; Close file
;    ld      de,(_exec_addr)
;    jp      run_exec
;
;
;; Validate Load and Execute Adress
;    ld      hl,(_resource_len)    ; HL = RsrcLen
;    ld      de,6
;    sbc     hl,de
;    ld      b,h                   ; BC = ProgLen
;    ld      c,l
;    ld      hl,$4000              ; HL = MinAdr
;    ld      de,(_load_addr)
;    rst     COMPAR                ; If LoadAdr < MinAdr
;    ret     c                     ;   Return Carry set
;    add     hl,bc                 ; HL = PrgEnd
;    rst     COMPAR
;    ccf                           ; If ExecAdr >= PrgEnd
;    ret     c                     ;   Return Carry Set
;    ex      de,hl                 ; DE = PrgEnd
;    ld      hl,-512
;    add     hl,sp                 ; TopAdr = StkPtr - 512
;    rst     COMPAR                ; If TopAdr < PrgEnd
;    ret     c                     ;   Bad file error
;; Load the executable
;    push    de                    ; Stack = PrgEnd
;    ex      af,af'                ; AF = FilDsc
;    ld      de,(_load_addr)       ; DE = LoadAddr
;    call    esp_read_bytes        ; DE = EndAdr
;    ld      a,l                   ; L = FilDsc
;    call    dos_close             ; Close file
;    pop     hl                    ; HL = PrgEnd
;; Check execution address
;    rst     COMPAR                ; If EndAdr < PrgEnd
;    jp      c,.badfile            ;   Bad file error
;    ld      de,(FBUFFR+14)        ; DE = ExeAdr
;    rst     COMPAR                ; If ExeAdr >= PrgEnd
;    jp      c,.badfile            ;   Bad file error
;    ld      hl,(FBUFFR+10)        ; HL = BgnAdr
;    rst     COMPAR                ; If ExeAdr < BgnAdr
;    jp      c,.badfile            ;   Bad file error
;; Start the program    
;    ex      de,hl                 ; HL = ExeAdr
;    call    jump_hl
;    pop     hl
;    ret
;.badfile


