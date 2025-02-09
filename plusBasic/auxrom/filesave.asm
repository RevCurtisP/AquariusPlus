;====================================================================
; File Save Machine Language Routines
;====================================================================

;-----------------------------------------------------------------------------
; Append RAM chunk to file
; Input: BC: maximum length
;        DE: destination address
;        HL: string descriptor address
; Output: A: result code
;        BC: number of bytes actually read
;        DE: next destination address
; Flags Set: S if I/O error
; Clobbered: HL
;-----------------------------------------------------------------------------
file_append_binary:
    call    dos_open_append
    jr      _write_binary

;-----------------------------------------------------------------------------
; Write RAM chunk to file
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
_write_binary:
    ret     m
    call    esp_write_bytes
    push    af
    call    esp_close_all
    pop     af
    or      a
    ret

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
file_save_colormap:
    scf
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


_save_palette_asc:
    push    hl                    ; Stack = FilStd, RtnAdr
    call    _get_palette
    call    get_strbuf_addr       ; HL = StrBuf
    ld      d,h
    ld      e,l                   ; DE = StrBuf
    ld      bc,32
    add     hl,bc                 ; AscAdr = StrBuf+32
    push    hl                    ; Stack = AscAdr, FilStd,, RtnAdr
    ld      c,16
    call    rgb_to_asc
    pop     de                    ; DE = AscAdr; Stack = FilStd, RtnAdr
    pop     hl                    ; HL = FilStd; Stack = RtnAdr
    jr      _save_string

_get_palette:
    call    get_strbuf_addr       ; HL = StrBuf
    ex      de,hl                 ; DE = StrBuf
    ld      bc,32                 ; Read 16 palette entries
    ld      iy,palette_get           ; Read palette into string buffer
    jp      gfx_call

;-----------------------------------------------------------------------------
; Save Pallete
; Input: A: Palette number
;        C: File Type - 0: Binary, !0: ASCII
;       HL: String descriptor address
; Output: A: result code
; Flags Set: S if I/O error
;            C if file too large
; Clobbered: BC, DE, EF
; Populates: String Buffer
;-----------------------------------------------------------------------------
file_save_palette:
    ld      c,a
    ld      a,b
    or      a
    ld      a,c
    jr      nz,_save_palette_asc
    push    hl                    ; Stack = FilStd, RtnAdr
    call    _get_palette          ; 
    pop     hl                    ; HL = FilStd; Stack = RtnAdr
    ld      a,32                  
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
; Input: A: StrLen, DE: StrAdr, HL: FilStd
_save_string:
    ld      b,0
    ld      c,a
; Input: BC: StrLen, DE: StrAdr, HL: FilStd
file_save_string:
    push    de                    ; Stack = StrBuf, RtnAdr
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
    ld      iy,screen_write_tmpbfr
    call    gfx_call              ; BC = SavLen
    pop     hl                    ; HL = StrDsc; Stack = RtnAdr
file_save_tmpbuffr:
    ld      de,0                  ; DE = SavAdr
    ld      a,TMP_BUFFR           ; A = SavePg
    jp      file_save_paged 

;-----------------------------------------------------------------------------
; Save tilemap from Video RAM
;  Input: HL: String descriptor address
; Output: A: result code
;        BC: result length
;        DE: terminator address
; Flags Set: S if I/O error
;            C if tile# out of range
; Clobbered: BC, DE,HL
;-----------------------------------------------------------------------------
file_save_tilemap:
    ld      bc,4096               ; BC = MaxLen
    ld      de,0                  ; DE = LoadAdr
    jr      _save_video_ram       ; Load and return

;-----------------------------------------------------------------------------
; Save tileset from Video RAM
;  Input: BC: Number of tiles
;         DE: Starting tile#
;         HL: String descriptor address
; Output: A: result code
;        BC: result length
;        DE: terminator address
; Flags Set: S if I/O error
;            C if tile# out of range
; Clobbered: HL
;-----------------------------------------------------------------------------
file_save_tileset:
    push    hl                    ; Stack = StrDsc, RtnAdr
    push    bc                    ; Stack = TilCnt, StrDsc, RtnAdr
    ld      iy,tile_address
    call    gfx_call              ; HL = TilAdr
    pop     de                    ; DE = TilCnt; Stack = StrDsc, RtnAdr
    jp      c,POPHRT              ; If TileNo > 511 Return Carry Set
    push    hl                    ; Stack = TilAdr, StrDsc, RtnAdr
    ex      de,hl                 ; HL = TilCnt, DE = TilAdr
    ld      b,5
    call    shift_hl_left         ; TilLen = TileCnt * 32
    ld      b,h
    ld      c,l                   ; BC = TilLen
    ex      de,hl                 ; HL = TilAdr
    add     hl,bc                 ; HL = EndAdr
    ex      de,hl                 ; DE = EndAdr
    ld      hl,$4000
    rst     COMPAR
    pop     de                    ; DE = TilAdr; Stack = StrDsc, RtnAdr
    pop     hl                    ; HL = StrDsc; Stack = RtnAdr
    ret     c                     ; If EndAdr > $4000, return Carry Set
_save_video_ram
    ld      a,VIDEO_RAM           ; A = Page
    jp      file_save_paged

