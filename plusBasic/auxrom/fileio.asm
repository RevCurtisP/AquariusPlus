;====================================================================
; File I/O Machine Language Routines
;====================================================================

;-----------------------------------------------------------------------------
; file_copy - Copy file
; Input: DE: Destination filename string descriptor
;        HL: Source filename string descriptor
; Output:  A: Result
; Clobbered: AF' BC, DE
;-----------------------------------------------------------------------------
file_copy:
    call    dos_open_read         ; A = SrcFil
    ret     m                     
    push    af                    ; Stack = SrcFil, RtnAdr
    ex      de,hl                 ; HL = DstSpc
    call    dos_open_write        ; A = DstFil
    pop     de                    ; D = SrcFil; Stack = RtnAdr
    ret     m
    ld      e,a                   ; E = DstFil
    in      a,(IO_BANK1)           
    push    af                    ; Stack = OldPg, RtnAdr
    ld      a,TMP_BUFFR
    out     (IO_BANK1),a          ; Map TMP_BUFFR into Bank 1
    call    _do_copy              ;
    ex      af,af'                ; AF' = Result
    pop     af                    ; A = OldPg; Stack = RtnAdr
    out     (IO_BANK1),a          ; Map original page into Bank 1
    ex      af,af'                ; AF = Result
    push    af                    ; Stack = Result, RtnAdr
    ld      a,d                   ; A = SrcFil
    call    dos_close             ; Close Source FIle
    ld      a,e                   ; A = DstFil
    call    dos_close             ; Close Destination FIle
    pop     af                    ; AF = Result; Stack = RtnAdr
    ret
    
_do_copy:
    push    de                    ; Stack = SrcDst, RtnAdr
    ld      a,d                   ; A = SrcFil
    ld      bc,$4000              ; BC = LoadLen
    ld      de,BANK1_BASE         ; DE = LoadAdr
    call    esp_readc_bytes       ; A = Result, BC = Bytes Read
    pop     de                    ; DE = SrcDst
    ret     m                     ; Return if error
    ld      a,b
    or      c                     ; If zero bytes read
    ret     z                     ;   Return
    ld      a,e                   ; A = DstFil
    push    de                    ; Stack = SrcDst, RtnAdr
    ld      de,BANK1_BASE         ; DE = LoadAdr
    call    esp_writec_bytes      ; A = Result
    pop     de                    ; DE = SrcDst; Stack = RtnAdr
    ret     m                     ; Return if error
    jr      _do_copy
    

;-----------------------------------------------------------------------------
; Return file extension
; Input: BC: filespec length
;        DE: filespec address
; Output: A,BC: extension length
;         DE: extension address
; Clobbered: HL
;-----------------------------------------------------------------------------
file_get_ext:
    ld      l,'.'                 ; Delimiter
_chop_filespec
    ex      de,hl                 ; HL = SpcAdr; E = Delim
    add     hl,bc                 ; HL = SpcEnd + 1
    ld      b,c                   ; B = Counter
    ld      c,0                   ; C = ExtLen
.loop
    dec     hl                    ; Move left
    ld      a,(hl)                ; Get character
    cp      e
    jr      z,.found              ; If not Delim
    inc     c                     ;   Bump length
    djnz    .loop                 ;   Count down
    ex      de,hl                 ;   Restore hl
    ld      c,b                   ;   BC = 0
    jr      .return               ; Else
.found
    ex      de,hl                 ; DE = DotAdr
    inc     de                    ; DE = ExtAdr
    ld      b,0                   ; BC = ExtLen
.return
    ld      a,c
    or      a
    ret

;-----------------------------------------------------------------------------
; Return filespec without directory
; Input: BC: filespec length
;        DE: filespec address
; Output: A,BC: filename length
;         DE: filename address
; Clobbered: HL
;-----------------------------------------------------------------------------
file_trim_dir:
    push    de                    ; Stack = StrAdr, RtnAdr
    push    bc                    ; Stack = StrLen, StrAdr, RtnAdr
    push    de                    ; Stack = StrAdr, StrLen, StrAdr, RtnAdr
    ld      l,'/'                 ; Delimiter
    call    _chop_filespec        ; DE = NamAdr, A,BC = NamLen
    pop     hl                    ; HL = StrAdr; Stack = StrLen, StrAdr, RtnAdr
    rst     COMPAR                ;
    jr      z,.no_dir             ; If NamAdr <> StrAdr
    ld      a,c
    or      a
    jr      pop2hlr_aux 

.no_dir
    pop     bc                    ; BC = StrLen; ; Stack = StrAdr, RtnAdr
    pop     de                    ; HL = StrAdr; ; Stack = RtnAdr
    ld      a,c                   ; A = StrLen
    or      a
    ret
    
;-----------------------------------------------------------------------------
; Return directory portion of filespec
; Input: BC: filespec length
;        DE: filespec address
; Output: A,BC: directory length
;         DE: directory address
; Clobbered: HL
;-----------------------------------------------------------------------------
file_get_dir:
    push    de                    ; Stack = StrAdr, RtnAdr
    push    bc                    ; Stack = StrLen, StrAdr, RtnAdr
    push    de                    ; Stack = StrAdr, StrLen, StrAdr, RtnAdr
    ld      l,'/'                 ; Delimiter
    call    _chop_filespec        ; DE = NamAdr, A,BC = NamLen
    pop     hl                    ; HL = StrAdr; Stack = StrLen, StrAdr, RtnAdr
    rst     COMPAR                ;
    jr      z,.no_dir             ; If NamAdr <> StrAdr
    pop     hl                    ; HL = OldLen; Stack = StrAdr, RtnAdr
    dec     bc
    jr      _no_ext
.no_dir
    xor     a                     ; Else
    ld      b,a                   ;   A,BC = 0
    ld      c,a
pop2hlr_aux:
    pop     hl                    ; Stack = StrAdr, RtnAdr
    pop     hl                    ; Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; Return filespec without extension
; Input: BC: filespec length
;        DE: filespec address
; Output: A: extension length
;         BC: trimmed length
;         DE: filespec address
; Set Flags: NZ if file had extension, Z if it did not
; Clobbered: HL
;-----------------------------------------------------------------------------
file_trim_ext:
    ld      l,'.'                 ; Delimiter
    push    de                    ; Stack = StrAdr, RtnAdr
    push    bc                    ; Stack = StrLen, StrAdr, RtnAdr
    call    _chop_filespec        ; DE = ExtAdr, A,BC = ExtLen
    pop     hl                    ; HL = OldLen; Stack = StrAdr, RtnAdr
    jr      z,_no_ext             ; If extension found
    inc     bc                    ;   BC = ExtLen + 1
_no_ext:
    sbc     hl,bc                 ; HL = ChpLen
    ld      b,h
    ld      c,l                   ; BC = ChpLen
    ld      a,c                   ; A = ChpLen
    or      a                     ; Set flags
    pop     de                    ; DE = StrAdr; Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; Load binary file into main memory
; Input: BC: maximum length
;        DE: destination address
;        HL: string descriptor address
; Output: A: result code
;        BC: number of bytes actually read
;        DE: next destination address
; Flags Set: S if I/O error
; Clobbered: HL
;-----------------------------------------------------------------------------
file_load_binary:
    call    dos_open_read
    ret     m
    call    esp_read_bytes
    ret     m
    push    af
    call    dos_close
    pop     af
    or      a
    ret

;-----------------------------------------------------------------------------
; Load bitmap image
; Input: HL: String descriptor address
; Output: A: result code
; Flags Set: S if I/O error, C if invalid file contents
; Clobbered: CD, DE, EF
;-----------------------------------------------------------------------------
file_load_bitmap:
    call    file_load_tmpbuffr    
    ret     m                     ; Return if Error 
    jp      bitmap_read_tmpbfr

;-----------------------------------------------------------------------------
; Save bitmap image
; Input: HL: String descriptor address
; Output: A: result code
; Flags Set: S if I/O error, C if invalid file contents
; Clobbered: CD, DE, EF
;-----------------------------------------------------------------------------
file_save_bitmap:
    push    hl                    ; Stack = StrDsc, RtnAdr
    call    bitmap_write_tmpbfr   ; BC = SavLen
    pop     hl                    ; HL = StrDsc; Stack = RtnAdr
    jp      file_save_tmpbuffr    ; Save bitmap data to file

;-----------------------------------------------------------------------------
; Load file into character RAM buffer
; Input: HL: File name string descriptor address
; Output: A: Result
; Clobbered registers: BC,DE,HL
;-----------------------------------------------------------------------------
file_load_defchrs:
    ld      de,DEFCHRSET
    jr      _load_chrset
file_load_altchrs:
    ld      de,ALTCHRSET
_load_chrset
    ld      a,BAS_BUFFR
    ld      bc,CHRSETLEN
;-----------------------------------------------------------------------------
; Load binary file into paged memory`
; Input: A: Page
;        BC: maximum length
;        DE: destination address
;        HL: string descriptor address
; Output: A: result code
;        BC: number of bytes actually read
;        DE: next destination address
;         H: destination page
; Flags Set: Z if llegal page
;            C if page overflow
;            S if I/O error
; Clobbered: AF',L
;-----------------------------------------------------------------------------
file_load_paged:
    push    af                    ; Stack = Page, RtnAdr
    call    dos_open_read
    pop     hl                    ; H = Page, Stack = RtnAdr
    ret     m
    call    esp_read_paged        ;
    push    af                    ; Stack = Result, RtnAdr
    ld      a,l
    call    dos_close
    pop     af                    ; AF = Result; Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; Load ROM file into page 35
;        HL: string descriptor address
;-----------------------------------------------------------------------------
file_load_rom:
    ld      a,RAM_BAS_3
    ld      de, $C000
    ld      bc, $4000
    call    file_load_paged
    ret     m

    ; Check length
    ld      a, b
    cp      $20         ; 8KB ROM?
    jr      z,.copy
    xor     a
    ret
.copy
    ld      a,RAM_BAS_3
    call    page_map_bank1
    ld      hl, BANK1_BASE
    ld      de, BANK1_BASE+$2000
    ld      bc, $2000
    ldir
    xor     a
    jp      page_restore_bank1

;-----------------------------------------------------------------------------
; Load screen image
; Input: HL: String descriptor address
; Output: A: result code
; Flags Set: S if I/O error, C if invalid file contents
; Clobbered: CD, DE, EF
;-----------------------------------------------------------------------------
; ToDo: Allow loading to a screen buffer
file_load_screen:
    call    file_load_tmpbuffr    
    ret     m                     ; Return if Error 
    jp      screen_read_tmpbfr

;-----------------------------------------------------------------------------
; Load file into TMP_BUFFR
; Input: HL: String descriptor address
; Output: A: result code
;        BC: number of bytes read
; Flags Set: S if I/O error, C if invalid file contents
; Clobbered: CD, DE, EF
;-----------------------------------------------------------------------------
file_load_tmpbuffr:
    ld      a,TMP_BUFFR            
    ld      bc,$4000
    ld      de,0
    jp      file_load_paged       ; Load SCRN file into TMP_BUFFR

;-----------------------------------------------------------------------------
; Load PT3 file
; Input: HL: String descriptor address
; Output: A: result code
; Flags Set: S if I/O error
; Clobbered: BC,DE,HL,IX,IY
;-----------------------------------------------------------------------------
; LOAD PT3 "/music/songs1/drops.pt3"
file_load_pt3:
    ld      a,PT3_BUFFR
    ld      de,pt3song
    ld      bc,$4000-pt3song
    call    file_load_paged
    push    af
    call    pt3_reset
    pop     af
    ret

;-----------------------------------------------------------------------------
; Load Pallete
; Input: A: Palette number
;       HL: String descriptor address
; Output: A: result code
; Flags Set: S if I/O error
;            C if file too large
; Clobbered: BC, DE, EF
; Populates: String Buffer
;-----------------------------------------------------------------------------
file_load_palette:
    ld      e,a                   ; E = PalNum
    push    de                    ; Stack = PalNum, RtnAdr
    call    file_load_strbuf      ; A = Result, BC = DatLen, HL = StrBuf
    pop     de                    ; E = PalNum; Stack = RtnAdr
    ret     m                     ; Return if I/O Error
    ld      a,32                  ; If result > 32
    cp      c
    jp      c,discard_ret         ;   Return overflow
    ex      de,hl                 ; DE = StrBuf, L = PalNum
    xor     a
    jp      palette_set           ; Write out palette and return

;-----------------------------------------------------------------------------
; Read file into string buffer
; Input: HL: String descriptor address
; Output: A: result code
;        BC: result length
;        DE: terminator address
;        HL: string buffer address
; Flags Set: S if I/O error
; Clobbered: HL
;-----------------------------------------------------------------------------
file_load_strbuf:
    ex      de,hl                 ; DE = FilStd
    call    get_strbuf_addr       ; HL = StrBuf, BC = BufMax
    ex      de,hl                 ; DE = StrBuf, HL = FilStd
    push    de                    ; Stack = StrBuf
    call    file_load_binary      ; Load file into string buffer
    pop     hl                    ; HL = StrBuf
    ret     m
    xor     a
    ld      (de),a
    ret

;-----------------------------------------------------------------------------
; Save binary file into main memory
; Input: BC: maximum length
;        DE: destination address
;        HL: string descriptor address
; Output: A: result code
;        BC: number of bytes actually read
;        DE: next destination address
; Flags Set: S if I/O error
; Clobbered: HL
;-----------------------------------------------------------------------------
file_save_binary:
    call    dos_open_write
    ret     m
    call    esp_write_bytes
    push    af
    call    esp_close_all
    pop     af
    or      a
    ret

;-----------------------------------------------------------------------------
; Save character RAM to file
; Input: HL: File name string descriptor address
; Output: A: Result
; Clobbered registers: BC,DE,HL
;-----------------------------------------------------------------------------
file_save_chrset:
    ld      a,CHAR_RAM
    ld      bc,2048
    ld      de,0

;-----------------------------------------------------------------------------
; Save binary data from paged memory to file
; Input: A: Page
;       BC: length
;       DE: source address
;       HL: string descriptor address
; Output: A: result code
;        BC: number of bytes actually read
;        DE: next source address
; Flags Set: S if I/O error
; Clobbered: HL
;-----------------------------------------------------------------------------
file_save_paged:
    push    af
    call    dos_open_write
    jp      m,discard_ret
    pop     af
    call    esp_write_paged
    push    af
    call    esp_close_all
    pop     af
    ret

;-----------------------------------------------------------------------------
; Save Pallete
; Input: A: Palette number
;       HL: String descriptor address
; Output: A: result code
; Flags Set: S if I/O error
;            C if file too large
; Clobbered: BC, DE, EF
; Populates: String Buffer
;-----------------------------------------------------------------------------
file_save_palette:
    push    hl                    ; Stack = FilStd, RtnAdr
    call    get_strbuf_addr       ; HL = StrBuf
    ex      de,hl                 ; DE = StrBuf
    ld      bc,32                 ; Read 16 palette entries
    call    palette_get           ; Read palette into string buffer
    pop     hl                    ; HL = FilStd; Stack = RtnAdr
    ld      a,32                  ;
;-----------------------------------------------------------------------------
; Write string buffer to file
; Input: A: Number of bytes to write
;        HL: String descriptor address
; Output: A: result code
; Flags Set: S if I/O error
; Clobbered: HL
;-----------------------------------------------------------------------------
file_save_strbuf:
    ex      de,hl                 ; DE = FilStd
    call    get_strbuf_addr       ; HL = StrBuf
    ex      de,hl                 ; DE = StrBuf, HL = FilStd
    push    de                    ; Stack = StrBuf, RtnAdr
    ld      b,0
    ld      c,a
    call    file_save_binary      ; Save string buffer to file
    pop     hl                    ; HL = StrBuf; Stack = RtnAdr
    ret     m
    xor     a
    ld      (de),a
    ret

;-----------------------------------------------------------------------------
; Save screen image
; Input: HL: String descriptor address
; Output: A: result code
; Flags Set: S if I/O error
; Clobbered: CD, DE, EF
;-----------------------------------------------------------------------------
; ToDo: Allow saving from a screen buffer
file_save_screen:
    push    hl                    ; Stack = StrDsc, RtnAdr
    call    screen_write_tmpbfr   ; BC = SavLen
    pop     hl                    ; HL = StrDsc; Stack = RtnAdr
file_save_tmpbuffr:
    ld      de,0                  ; DE = SavAdr
    ld      a,TMP_BUFFR           ; A = SavePg
    jp      file_save_paged 

