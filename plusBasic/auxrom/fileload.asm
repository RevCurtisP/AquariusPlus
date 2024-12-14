;====================================================================
; File Load Machine Language Routines
;====================================================================

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
    call    esp_readc_bytes
; L: file descriptor, preserves AF
file_close:
    push    af
    ld      a,l
    call    dos_close
    pop     af
_or_a_ret:
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
    ld      iy,bitmap_read_tmpbfr
_load_read_tmpbuffr:
    call    file_load_tmpbuffr
    ret     m                     ; Return if Error
    jp      (iy)

;-----------------------------------------------------------------------------
; Load bitmap image
; Input: HL: String descriptor address
; Output: A: result code
; Flags Set: S if I/O error, C if invalid file contents
; Clobbered: CD, DE, EF
;-----------------------------------------------------------------------------
file_load_color:
    ld      iy,color_read_tmpbfr
    jr      _load_read_tmpbuffr

;-----------------------------------------------------------------------------
; Load bitmap image
; Input: HL: String descriptor address
; Output: A: result code
; Flags Set: S if I/O error, C if invalid file contents
; Clobbered: CD, DE, EF
;-----------------------------------------------------------------------------
file_load_colormap:
    ld      iy,colormap_read_tmpbfr
    jr      _load_read_tmpbuffr

;-----------------------------------------------------------------------------
; Load file into character RAM buffer
; Input: HL: File name string descriptor address
; Output: A: Result
; Clobbered registers: BC,DE,HL
;
; File Length ASCII Range Offset
;  $300  768   32 to 127    256
;  $400 1024  128 to 255   1024
;  $800 2048    0 to 255      0
;-----------------------------------------------------------------------------
file_load_chrset:
    call    file_load_tmpbuffr    ; A = 0 if no error
    ret     m                     ; Return if arror
    or      c
    jr      nz,ret_carry_set
    ld      a,b
    cp      $08                   ; If FileLen = 2048
    ld      de,ALTCHRSET          ;   Copy over entire character set
    jr      z,copy_chrset         ;
    cp      $04
    ld      de,ALTCHRSET+1024     ; Else if FileLen = 1024
    jr      z,copy_chrset         ;
    cp      $03                   ; Else If FileLen = 769
    ld      de,ALTCHRSET+256      ;   Copy starting at space character
    jr      z,copy_chrset         ; Else
ret_carry_set:
    scf                           ;   Return Bad file error
    ret
copy_chrset:
    ld      hl,0                  ; Copy from start
    ld      a,TMP_BUFFR           ; of TMP_BUFFR
    ex      af,af'
    ld      a,BAS_BUFFR           ; to BASIC buffers
    jp      page_copy_bytes_sys   ; Copy and return

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
    jp      file_load_buffer
    
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
; Flags Set: Z if illegal page
;            C if page overflow
;            S if I/O error
; Clobbered: AF',L
;-----------------------------------------------------------------------------
xfile_load_paged:
    ld      iy,xesp_read_paged
    jr      _load_paged
file_load_paged:
    ld      iy,esp_read_paged
_load_paged:
    push    af                    ; Stack = Page, RtnAdr
    call    dos_open_read
    pop     hl                    ; H = Page, Stack = RtnAdr
    ret     m
.readbytes
    call    jump_iy               ; AF = Result, L = FilDsc
    jr      z,.done
    ex      af,af'                ; AF' = Result
    ld      a,b
    and     c
    inc     a
    ld      a,l
    jr      z,.readbytes
    ex      af,af'                ; A = Result
.done
    push    af                    ; Stack = Result, RtnAdr
    ld      a,l
_close_pop_af:
    call    dos_close
    pop     af                    ; AF = Result; Stack = RtnAdr
    ret

file_load_boot:
    ld      a,BOOT_BUFR
    ld      hl,_bootdesc
file_load_page
    ld      de, $C000
    ld      bc, $4000
    jp      xfile_load_paged

_bootbin
    byte    "esp:boot.bin"
_bootdesc 
    word    $ - _bootbin, _bootbin

;-----------------------------------------------------------------------------
; Load ROM file into page 35
;        HL: string descriptor address
;-----------------------------------------------------------------------------
file_load_rom:
    ld      a,TMP_BUFFR
    ld      de, $C000
    ld      bc, $4000
    call    xfile_load_paged
    ret     m

    ; Check length
    ld      a, b
    cp      $20         ; 8KB ROM?
    jr      z,.copy
    xor     a
    ret
    
.copy
    ld      a,TMP_BUFFR
    call    page_map_bank1
    ld      hl, BANK1_BASE
    ld      de, BANK1_BASE+$2000
    ld      bc, $2000
    ldir
    jp      page_restore_bank1

;-----------------------------------------------------------------------------
; Load screen image
; Input: HL: String descriptor address
; Output: A: result code
; Flags Set: S if I/O error, C if invalid file contents
; Clobbered: BC ,DE, HL
;-----------------------------------------------------------------------------
; ToDo: Allow loading to a screen buffer
file_load_screen:
    ld      iy,screen_read_tmpbfr
    jp      _load_read_tmpbuffr

;-----------------------------------------------------------------------------
; Load file into TMP_BUFFR
; Input: HL: String descriptor address
; Output: A: result code
;        BC: number of bytes actually read
;        DE: next destination address
;         H: destination page
; Flags Set: S if I/O error, C if buffer crosses page bpundary
; Clobbered: AF', L
;-----------------------------------------------------------------------------
file_load_tmpbuffr:
    ld      a,TMP_BUFFR
    ld      bc,$4000
    ld      de,0
;-----------------------------------------------------------------------------
; Load file into buffer
; - Load specified file into a buffer in paged memory
;   - Buffer must be contained eith a memory page
; Input: A = Page
;       BC = Buffer Length
;       DE - Guffer Srart Address
;         HL: String descriptor address
; Output: A: result code
;        BC: number of bytes actually read
;        DE: next destination address
;         H: destination page
; Flags Set: S if I/O error, C if buffer crosses page bpundary
; Clobbered: AF', L
; Clobbered: AF', L
;-----------------------------------------------------------------------------
;; YoDo: Return carry set if BC + DE > 116384
file_load_buffer:
    push    af                    ; Stack = Page, RtnAdr
    call    dos_open_read         ; A = FilDsc
    pop     hl                    ; H = Page, Stack = RtnAdr
    ret     m
    ex      af,af'                ; A' = FilDsc
    in      a,(IO_BANK1)          ; A = OldPg
    push    af                    ; Stack = OldPg, RtnAdr
    ld      a,h                   ; A = Page
    out     (IO_BANK1),a
    ld      a,d
    or      $40                   ; Coerce address to Bank 1  
    ld      d,a
    ex      af,af'                ; A = FilDsc
    call    esp_readc_bytes       ; A = Result, L = FilDsc
    ex      af,af'                ; A' = Result
    ld      a,l                   ; A = FilDsc
    call    dos_close             
    pop     af                    ; A = OldPg; Stack = RtnAdr
    out     (IO_BANK1),a
    ex      af,af'                ; A = Result
    ret


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
    call    file_load_buffer
    push    af
    call    pt3_reset
    pop     af
    ret

;-----------------------------------------------------------------------------
; Load Palette
; Input: A: Palette number
;        B: File type: 0 = binary, else ASCII
;       HL: String descriptor address
; Output: A: result code
; Flags Set: S if I/O error
;            C if invalid file
; Clobbered: BC, DE, EF
; Populates: String Buffer
;-----------------------------------------------------------------------------
file_load_palette:
    ld      e,a                   ; E = PalNum
    ld      a,b
    or      a
    jr      nz,_load_palette_asc
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

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Resource: Palette
; File extension: .palt
; Legacy extension: .pal
; Binary 32 bytes
; - 16 pairs of $GB $0R (little-endian $0RGB
; ASCII
; - Up to 16 lines of hexadecimal RGB values in the format #RRGGBB
; - Lines that do not start with # are ignored
; - Text after #RRGGBB is ignored
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

_load_palette_asc:
    call    dos_open_read         ; A = FilDsc
    ret     m                     ; Return if I/O Error
    push    de                    ; Stack = PalNum, RtnAdr
    push    af                    ; Stack = FilDsc, PalNum, RtnAdr
    call    get_strbuf_addr       ; HL = StrBuf
    ld      d,h
    ld      e,l                   ; DE = BinPtr
    ld      bc,32
    add     hl,bc                 ; LinBuf = StrBuf+32
    pop     af                    ; A = FilDsc; Stack = PalNum, RtnAdr
.loop
    push    hl                    ; Stack = LinBuf, PalNum, RtnAdr
    push    de                    ; Stack = BinPtr, LinBuf, PalNum, RtnAdr
    ld      bc,255-32
    call    espx_read_line        ; A = Result, C = LinLen, D = FilDsc
    ld      b,d                   ; B = FilDsc
    jp      m,.finish
    pop     de                    ; DE = BinPtr; Stack = LinBuf, PalNum, RtnAdr
    ld      a,(hl)                
    inc     hl
    cp      '#'                   
    jr      nz,.next              ; If first character is #
    ld      a,c                   
    cp      7                     ;   If LinLen < 7
    jr      c,.badline            ;     Exit with carry set
    push    bc                    ;   Stack = FilDsc, LinBuf, PalNum, RtnAdr
    call    asc_to_rgb            ;   Read RRGGBB to (DE)
    pop     bc                    ;   B = FilDsc; Stack = LinBuf, PalNum, RtnAdr
    jr      c,.badline
    ld      a,b                   ;   A = FilDsc
.next
    pop     hl                    ; HL = LinBuf; Stack = PalNum, RtnAdr
    jr      .loop
.finish
    pop     de                    ; DE = BinPtr, Stack = LinBuf, PalNum, RtnAdr
    pop     hl                    ; Stack = PalNum, RtnAdr
    pop     hl                    ; L = PalNum; Stack = RtnAdr
    push    af                    ; Stack = Result, RtnAdr
    ld      a,b                   ; A = FilDsc
    call    dos_close             
    pop     af                    ; AF = Result; Stack = RtnAdr
    jp      p,.write              ; If Error
    cp      ERR_EOF               ;   and not EOF
    jp      nz,_or_a_ret          ;   Return it
.write
    push    hl                    ; Stack = PalNum, RtnAdr
    call    get_strbuf_addr       ; HL = StrBuf
    ex      de,hl                 ; DE = StrBuf; HL = BinPtr
    sbc     hl,de                 
    ld      b,h
    ld      c,l                   ; BC = BinLen
    pop     hl                    ; L = PalNum; Stack = RtnAdr
    xor     a                     ; Palette entry 0
    jp      palette_set           ; Write palette and return
.badline
    pop     hl                    ; HL = LinBuf; Stack = PalNum, RtnAdr
    pop     hl                    ; HL = PalNum; Stack = RtnAdr
.close
    push    af
    ld      a,b                   ; A = FilDsc
    jp      _close_pop_af
    
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
; Load tilemap into Video RAM
;  Input: HL: String descriptor address
; Output: A: result code
; Flags Set: S if I/O error
;            C if illegal file
; Clobbered: BC, DE, HL
;-----------------------------------------------------------------------------
file_load_tilemap:
    ld      iy,tilemap_read_tmpbfr
    jp      _load_read_tmpbuffr

;-----------------------------------------------------------------------------
; Load tileset into Video RAM
;  Input: DE: Starting tile#
;         HL: String descriptor address
; Output: A: result code
; Flags Set: S if I/O error
;            C if tile# out of range
; Clobbered: AF', BC, DE, HL
;-----------------------------------------------------------------------------
file_load_tileset:
    push    de                    ; Stack = TileNo, RtnAdr
    call    file_load_tmpbuffr
    pop     de                    ; DE = TileNo; Stack = RtnAdr
    ret     m                     ; Return if Error
    jp      tileset_read_tmpbfr

; Input: DE: Tile#; OutputL Tile Address; Clobbers: BC, DE
tile_address:
    ld      hl,511
    rst     COMPAR                ; If TileNo > 511
    ret     c                     ;   Return Carry Set
    ex      de,hl                 ; HL = Ti
    ld      b,5
    jp      shift_hl_left         ; TilAdr = TileNo * 32

