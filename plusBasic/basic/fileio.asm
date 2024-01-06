;====================================================================
; File I/O Machine Language Routines
;====================================================================

;-----------------------------------------------------------------------------
; Return pointer to file extension
; Input: BC: filespec length
;        DE: filespec address
; Output: A,BC: extension length
;         DE: extension address
; Clobbered: BC,DE
;-----------------------------------------------------------------------------
file_get_ext:
      ld      l,'.'               ; Delimiter
      ex      de,hl               ; HL = SpcAdr; E = Delim
      add     hl,bc               ; HL = SpcEnd + 1
      ld      b,c                 ; B = Counter
      ld      c,0                 ; C = ExtLen
.loop
      dec     hl                  ; Move left
      ld      a,(hl)              ; Get character
      cp      e             
      jr      z,.found            ; If not Delim
      inc     c                   ;   Bump length
      djnz    .loop               ;   Count down
      ex      de,hl               ;   Restore hl
      ld      c,b                 ;   BC = 0
      jr      .return             ; Else
.found
      ex      de,hl               ; DE = DotAdr
      inc     de                  ; DE = ExtAdr
      ld      b,0                 ; BC = ExtLen
.return
      ld      a,c
      or      a
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
; Load file into character RAM buffer
; Input: HL: File name string descriptor address
; Output: A: Result
; Clobbered registers: BC,DE,HL
;-----------------------------------------------------------------------------
file_load_chrset:
    ld      a,BAS_BUFFR
    ld      bc,CHRSETLEN
    ld      de,CHRSETBUF
    call    file_load_paged
    ret     z                     ; Illegal page
    ret     c                     ; Page overflow
    ret     m                     ; I/O error
    jp      custom_chrset    

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
; Flags Set: S if I/O error
; Clobbered: CD, DE, EF
;-----------------------------------------------------------------------------
file_load_screen:
    call    dos_open_read
    ret     m
; Read first 2k into Screen RAM
    ld      bc,2048
    ld      de,SCREEN
    call    esp_read_bytes
    ret     m
; Read second 2k into scratch RAM
    ld      a,RAM_BAS_3
    ld      bc,2048
    ld      de,$3000
    call    esp_read_paged

    
    
    push    af
    call    esp_close_all
    pop     af
    or      a
    ret

;-----------------------------------------------------------------------------
; Load PT3 file
; Input: HL: String descriptor address
; Output: A: result code
; Flags Set: S if I/O error
; Clobbered: BC,DE,HL,IX,IY
;-----------------------------------------------------------------------------
file_load_pt3:
    ld      a,PT3_BUFFR
    ld      de,$0400
    ld      bc,$4000-$0400
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
    xor     a                     ; A = Entry#
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
; Save binary data from paged memory to file
; Input: A: Page
;       BC: length
;       DE: destination address
;       HL: string descriptor address
; Output: A: result code
;        BC: number of bytes actually read
;        DE: next destination address 
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

file_save_screen:
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
