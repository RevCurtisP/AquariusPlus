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
    ret     m
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
    ld      l,a                   ; L = FilDsc
    call    esp_write_bytes
    jp      _close

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
    ld      l,a                   ; L = FilDsc
    pop     af                    ; A = Page
    call    esp__write_paged
_close:
    push    af
    ld      a,l
    call    dos_close
    pop     af
    ret


;-----------------------------------------------------------------------------
; RGB pallet format is 16 lines of comma separated decimal red, green, and 
; blue, each of which is followed by a CR/LF
;-----------------------------------------------------------------------------
; Input: A: PalNum, HL: FilStd
_save_palette_rgb:
    ld      iy,rgb_to_dec
    ld      b,','
    jr      _save_palette

;-----------------------------------------------------------------------------
; ASC Pallet format is 16 lines of hexadecimal RRGGBB,
; each of which is followed by a CR/LF
;-----------------------------------------------------------------------------
; Input: A: PalNum, HL: FilStd
_save_palette_asc:
    ld      iy,rgb_to_asc
    ld      b,0
_save_palette:
    push    hl                    ; Stack = FilStd, RtnAdr
    push    bc                    ; Stack = PfxDlm, FilStd, RtnAdr
    call    _get_palette
    call    get_strbuf_addr       ; HL = StrBuf
    ld      d,h
    ld      e,l                   ; DE = StrBuf
    ld      bc,32
    add     hl,bc                 ; AscAdr = StrBuf+32
    pop     bc                    ; B = PfxDlm; Stack = FilStd, RtnAdr
    push    hl                    ; Stack = AscAdr, FilStd, RtnAdr
    ld      c,16
    call    gfx_call              ; Call RGB converter
    pop     de                    ; DE = AscAdr; Stack = FilStd, RtnAdr
    pop     hl                    ; HL = FilStd; Stack = RtnAdr
    jr      _save_string

; Input: A: PalNum, HL: FilStd
_save_palette_hex:
    push    hl                    ; Stack = FilStd, RtnAdr
    call    _get_palette
    call    get_strbuf_addr       ; HL = StrBuf
    push    hl                    ; Stack = PalBuf, FilStd, RtnAdr
    ld      bc,32
    add     hl,bc                 ; HL = HexBuf
    pop     de                    ; DE = PalBuf; Stack = FilStd. RtnAdr
    call    aux_asc_to_hex        ; BC = HexLen, HL = HexBuf
    ex      de,hl                 ; DE = HexBuf
    pop     hl                    ; HL = FilStd; Stack = RtnAdr
    jr      file_save_string

; Input: A: PalNum, DE: BufAdr
_get_palette:
    push    iy
    call    get_strbuf_addr       ; HL = StrBuf
    ex      de,hl                 ; DE = StrBuf
    ld      bc,32                 ; Read 16 palette entries
    ld      iy,palette_get        ; Read palette into string buffer
aux_gfxcall_pop_iy:
    call    gfx_call
aux_pop_iy_ret:
    pop     iy
    ret

;-----------------------------------------------------------------------------
; Save Pallete
; Input: A: Palette number
;        B: File type: 0 = Binary, 1 = ASCII, 2 = RGB, 3 = Hex string
;; ToDo  C: Line prefix (ASCII), Delimiter (RGB)
;; ToDo DE: Work buffer address (256 bytes)
;       HL: String descriptor address
; Output: A: result code
; Flags Set: S if I/O error
;            C if file too large
; Clobbered: BC, DE, EF
; Populates: String Buffer
;-----------------------------------------------------------------------------

DEBUG: File not saved when SAVE PALETTE 1,"/t/palette.txt",ASC,"#"

file_save_palette:
    dec     b                     ; If FilTyp = 1
    jr      z,_save_palette_asc   ;   Save ASCII palette
    dec     b                     ; If FilTyp = 2
    jr      z,_save_palette_rgb   ;   Save RGB palette
    dec     b                     ; If FilTyp = 3
    jr      z,_save_palette_hex   ;   Save Hex String palette
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

