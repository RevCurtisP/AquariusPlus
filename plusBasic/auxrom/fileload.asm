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
_load_read_gfx:
    call    file_load_tmpbuffr
    ret     m                     ; Return if Error
    jp      gfx_call

;-----------------------------------------------------------------------------
; Load bitmap image
; Input: HL: String descriptor address
; Output: A: result code
; Flags Set: S if I/O error, C if invalid file contents
; Clobbered: CD, DE, EF
;-----------------------------------------------------------------------------
file_load_color:
    ld      iy,color_read_tmpbfr
    jr      _load_read_gfx

;-----------------------------------------------------------------------------
; Load bitmap image
; Input: HL: String descriptor address
; Output: A: result code
; Flags Set: S if I/O error, C if invalid file contents
; Clobbered: CD, DE, EF
;-----------------------------------------------------------------------------
file_load_colormap:
    ld      iy,colormap_read_tmpbfr
    jr      _load_read_gfx

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
; Load PT3 player into paged memory
;-----------------------------------------------------------------------------
file_load_pt3play:
    ld      a,TRK_BUFFR           ; Page
    ld      l,0
    call    xpage_fill_all_byte   ; Zero out buffer
    ld      hl,.ptdesc            ;   Load from ESP
.load_ptplay:
    ld      a,TRK_BUFFR           ; Page
    ld      bc,$4000              ; Load up to 16k
    ld      de,PT3_BASE           ; Player Start address
    jr      xfile_load_paged
.ptplay:
    byte    "esp:ptplay.bin"
.ptdesc:
    word    $-.ptplay,.ptplay
   
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
    jp      _load_read_gfx

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
    ld      a,TRK_BUFFR
    ld      de,pt3song
    ld      bc,$4000-pt3song
    call    file_load_buffer
    push    af
    call    track_reset
    pop     af
    ret


;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Resource: Palette
; File extension: .palt
; Legacy extension: .pal
; Binary 32 bytes
; - 16 pairs of $GB $0R (little-endian $0RGB
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;-----------------------------------------------------------------------------
; Load Palette
; Input: A: Palette number
;        B: File type: 0 = binary, 1 = ASCII, 2 = RGB, 3 = Hex string
;       HL: String descriptor address
; Output: A: result code
; Flags Set: S if I/O error
;            C if invalid file
; Clobbered: BC, DE, EF
; Populates: String Buffer
;-----------------------------------------------------------------------------
file_load_palette:
    ld      e,a                   ; E = PalNum
    push    de                    ; Stack = PalNum, RtnAdr
    call    file_load_strbuf      ; A = Result, BC = PalLen, HL = StrBuf
    pop     de                    ; E = PalNum; Stack = RtnAdr
    ret     m                     ; Return if I/O Error
    ld      a,c
    cp      64                    ; If FileLen = 64
    jr      z,_pallete_asc        ;   Treat as hexadecimal ASCII
    cp      32                    ; If FilLen <> 32
    jp      nz,ret_carry_set      ;   Bad file error
_ex_de_palette_set:
    ex      de,hl                 ; DE = StrBuf, L = PalNum
    xor     a
_palette_set:
    ld      iy,palette_set        ; Write out palette and return
    jp      gfx_call

_pallete_asc:
    push    de                    ; Stack = PalNum, RtnAdr
    push    hl                    ; Stack = StrBuf, PalNum, RtnAdr
    ld      de,64
    add     hl,de                 ; HL = PalBuf
    pop     de                    ; DE = StrBuf, Stack = PalNum, RtnAdr
    call    aux_hex_to_asc        ; BC = PalLen, HL = PalBuf
    pop     de                    ; E = PalNum; Stack = RtnAdr
    ret     c                     ; If bad hex string, return bad file
    jr      _ex_de_palette_set


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
    jp      _load_read_gfx

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
    ld      iy,tileset_read_tmpbfr
    jp      gfx_call

; Input: DE: Tile#; OutputL Tile Address; Clobbers: BC, DE
tile_address:
    ld      hl,511
    rst     COMPAR                ; If TileNo > 511
    ret     c                     ;   Return Carry Set
    ex      de,hl                 ; HL = Ti
    ld      b,5
    jp      shift_hl_left         ; TilAdr = TileNo * 32

