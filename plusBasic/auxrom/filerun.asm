;====================================================================
; File RUN auxillary routines
;====================================================================

; To check file signature
; For (16k) cartridge binaries
; the bytes at 0x2003...05...07...09...0B...0D...0F are consistent.
; 2A, 9C, B0, 6C, 64, A8, 70
; Of course we would need to load 8k bytes to check a 16k cart

;-----------------------------------------------------------------------------
; Check for .ROM and .CORE extensions (case insensitive)
; Input: A: First character of raw filename
;       HL: FileName string descriptor
; Output: Original or new filename string descriptor
; Flags: Carry Set if filename is neither .ROM or .CORE
;-----------------------------------------------------------------------------
run_check_extension:
    call    string_addr_len       ; DE = NamAdr, A,BC = NamLen
    ld      a,c                   ; A = NamLen
    cp      a,5                   ; If NamLen < 5
    ret     c                     ;   Return Carry Set
    sub     a,4
    ld      c,a                   ; BC = NamLen - 4
    push    hl                    ; Stack = NamDsc, RtnAdr
    ex      de,hl                 ; HL = NamAdr
    add     hl,bc                 ; HL = NamAdr + NamLen - 4
    ex      de,hl                 ; DE = ExtAdr
    ld      hl,.romext            ; HL = ".ROM"
    ld      b,4                   ; Comparing 4 bytes
    push    de                    ; Stack = ExtAdr, NamDsc, RtnAdr
    call    string_cmp_upper      ; Set Z if ".ROM"
    pop     de                    ; DE = ExtAdr; Stack = NamDsc, RtnAdr
    pop     hl                    ; HL = NamDsc; Stack = RtnAdr
    ld      a,1                   ; If .ROM
    ret     z                     ;   Return A = 1
    push    hl                    ; Stack = NamDsc, RtnAdr
    dec     de                    ; Bump ExtAdr for 5 character extension
    ld      hl,.coreext           ; HL = ".CORE"
    ld      b,5                   ; Comparing 5 bytes
    call    string_cmp_upper      ; Set Z if ".CORE"
    pop     hl                    ; HL = NamDsc; Stack = RtnAdr
    ld      a,2                   ; If .CORE
    ret     z                     ;   Return A = 2
    scf                           ; Else return carry set
    ret                           

.coreext db ".CORE",0
.romext: db ".ROM",0

;-----------------------------------------------------------------------------
; Search for runnable file if wildcards specified in direct mode
; Input: A: Filespec start character
;       HL: Filespec string descriptor
; Output: A: $7F if no wildcards in file spec
;            -3 if both wilcard and path in filespec
;            -1 if no matching file found
;            0 if matching file found
;        HL: New or original filespec string descriptor
; Clobbered: BC, DE
;-----------------------------------------------------------------------------
; ToDo: Isolate path and prepend to matching file name
; Probably allocate a temp buffer, then copy path and matching name into it
run_search_files:
    cp      '"'                   ; If filename began with quote
    ret     z                     ;   Return
    call    in_direct             ; If not in Direct Mode
    ret     c                     ;   Return
    call    check_wildcard        ; If no wildcards in filename
    ret     nz                    ;   Return no wildcards in filespec
    call    check_path            ; If path found
    ret     m                     ;   Return Invalid parameter
    push    hl                    ; Stack = StrDsc, RtnAdr
    call    dos_open_dir          ; A = FilDsc
    jp      m,POPHRT              ; If error, pop StrDsc and return
.check_files
    push    af                    ; Stack = FilDsc, StrDsc, RtnAdr
.read_loop:
    pop     af                    ; A = FilDsc; Stack = StrDsc, RtnAdr
    call    get_strbuf_addr       ; HL = TmpBuf
    push    af                    ; Stack = FilDsc, StrDsc, RtnAdr
    call    file_read_dir         ; A = Result, BC = NamLen, D = Attrs, HL = NamAdr
    jp      m,.error              ; Return if Dos Error
    jr      c,.read_loop          ; Skip if name too long
    ld      a,d                   ; A = Attrs
    and     1                     ; If Directory
    jr      nz,.read_loop         ;   Skip
    push    bc                    ; Stack = NamLen, FilDsc, StrDsc, RtnAdr
    push    hl                    ; Stack = NamAdr, NamLen, FilDsc, StrDsc, RtnAdr
    call    .check_ext            ; Zero set if extension in listr
    pop     de                    ; DE = NamAdr; Stack = NamLen, FilDsc, StrDsc, RtnAdr
    pop     bc                    ; BC = NamLen; Stack = FilDsc, StrDsc, RtnAdr
    jr      nz,.read_loop         ; If FilExt not in list, check next file
    pop     af                    ; A = FilDsc; Stack = StrDsc, RtnAdr
    call    dos_close             ; Close file
    pop     hl                    ; Stack = RtnAdr
    ld      a,c                   ; A = NamLen
    call    STRAD2                ; HL = StrDsc (DSCTMP)
    xor     a                     ; Clear Carry
    inc     a                     ; Return 1
    ret

.check_ext
    ex      de,hl                 ; DE = NamAdr
    call    file_get_ext          ; A, BC = ExtLen, DE = ExtAdr
    ld      hl,run_ext_list       ; HL = ListAdr
.extloop
    ld      a,(hl)                ; A = EntLen
    inc     hl                    ; Skip EntLen
    or      a                     ; If End of List
    ret     m                     ;   Return Zero Clear
    cp      c                     ; If EntLen = ExtLen
    call    z,.compare            ; and FilExt = LstEnt
    ret     z
    add     a,l
    ld      l,a                   ; Next List Entry
    jr      .extloop              ; and loop

.compare
    push    bc                    ; Stack = ExtLen, RtnAdr
    push    af                    ; Stack = EntLen, ExtLen, RtnAdr
    push    de                    ; Stack = ExtAdr, EntLen, ExtLen, RtnAdr    
    push    hl                    ; Stack = LstPtr, ExtAdr, EntLen, ExtLen, RtnAdr    
    ld      b,c                   ; B = ExtLen
    call    string_cmp_upper      ; Return Zero Set if match`
    pop     hl                    ; HL = LstPtr; Stack = ExtAdr, EntLen, ExtLen, RtnAdr
    pop     de                    ; DE = ExtAdr; Stack = EntLen, ExtLen, RtnAdr
    pop     bc                    ; BC = EntLen; Stack = ExtLen, RtnAdr
    ld      a,b                   ; A = EntLen
    pop     bc                    ; BC = ExtLen; Stack = RtnAdr
    ret

.error
    pop     af                    ; A = FilDsc; Stack = StrDsc, RtnAdr
    call    dos_close             ; Close directory
    pop     hl                    ; HL = StrDsc; Stack = RtnAdr
    xor     a
    dec     a                     ; Return File Not Found
    ret                           ; No need to report error
    
.add_char:
    inc     c                     ; Bump StrLen for added character
    jp      z,LSERR               ; Error if > 255
    ld      (de),a                ; Append character to filename
    inc     de                    ; Bump BufPtr
    or      a
    ret     nz                    ; If terminator
    dec     c                     ;   Debump StrLen
    ret

; Input: HL: String descriptor
; Output: A: 0 if found, else $7F (flags set accordingly)
; Clobbered: A, BC, DE
check_wildcard:
    call    string_addr_len       ; DE = StrAdr; BC = StrLeb
    ld      b,c                   ; C = StrLen
.loop    
    ld      a,(de)
    sub     '?'                   ; A = 0 if ?
    ret     z
    sub     '*'-'?'               ; A = 0 if *
    ret     z
    inc     de
    djnz    .loop
    or      $7F                   ; Clear Zero, Carry, Sign Flag
    ret

check_path:
; Input: HL: String descriptor
; Output: A: 0 if no path, else $FF (flags set accordingly)
; Clobbered: A, BC, DE
    call    string_addr_len       ; DE = StrAdr; BC = StrLeb
    ld      b,c                   ; C = StrLen
.loop    
    ld      a,(de)
    cp      '/'
    jr      z,.slash
    inc     de
    djnz    .loop
    xor     a
    ret
.slash
    ld      a,-3                  ; Return Invalid Parameter
    or      a                     ; Set flags
    ret

