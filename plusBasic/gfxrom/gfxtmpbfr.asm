;====================================================================
; Graphics Bitmap Read/Write TMP_BUFFR Routines
;====================================================================

;-----------------------------------------------------------------------------
; Copy Bitmap data in TMP_BUFFR to Video RAM and palette
; Input: BC = Length of data to copy
; Sets flags: Carry if invalid data length
; Clobbers: A, AF', BC, DE, HL
; 
;  Size  Hex  Format
;  8000 $1F40 1bpp pixels only
;  9000 $2328 1bpp pixel + color
;  9032 $2348 1bpp pixel + color + palette
;  9192 $23E8 1bpp pixel + palette + 160 byte gap + color
; 16000 $3E80 4bpp pixels only
; 16032 $3EA0 4bpp pixels + palette
;-----------------------------------------------------------------------------
bitmap_read_tmpbfr:
    ld      h,b
    ld      l,c                   ; HL = DatLen
    ld      de,$1F40              ; 
    rst     COMPAR                ; If DatLen = 8000 
    jr      z,.bitmap1bpp         ;   Copy 1bpp pixels only
    ld      de,$2328              ; 
    rst     COMPAR                ; Else If DatLen = 9000 
    jr      z,.color1bpp          ;   Copy 1bpp pixels + color
    ld      e,$48                 ; 
    rst     COMPAR                ; Else If DatLen = 9032 
    jr      z,.palette1bpp        ;   Copy 1bpp pixels + color + palette
    ld      e,$E8                 ; 
    rst     COMPAR                ; Else If DatLen = 9192 
    jr      z,.bitgap1bpp         ;   Legacy 1bpp pixels
    ld      de,$3E80
    rst     COMPAR                ; Else If DatLen = 16000
    jr      z,.bitmap4bpp         ;   Copy 4bpp pixels
    ld      e,$A0
    rst     COMPAR                ; Else If DatLen <> 16032
    scf                           ;   Return Carry Set
    ret     nz
.palette4bpp    
    ld      de,BANK1_BASE+16000
    call    .palette
.bitmap4bpp
    ld      bc,16000
    jr      .bitmap
.color
    ld      bc,1000
    ld      de,8192
    jr      .zero
.bitgap1bpp
    ld      bc,8000               
    call    .bitmap
    ld      de,BANK1_BASE+8000
    call    .palette
    ld      bc,1000
    ld      de,8192
    ld      hl,8192
    jr      .copy
.palette1bpp
    ld      de,BANK1_BASE+9000
    call    .palette 
.color1bpp
    ld      hl,8000
    ld      bc,1000
    ld      de,8192
    call    .copy
.bitmap1bpp
    ld      bc,8000
.bitmap
    ld      de,0
.zero
    ld      hl,0
.copy
    jp      _copy_tmpbfr_vidram
; Enter with DE pointing to bitmap data
.palette
    call    page_map_tmpbfr_af
    xor     a
    ld      bc,32
    ld      l,1
    call    palette_set
    jp      page_restore_bank1_af
 
;-----------------------------------------------------------------------------
; Copy Bitmap date in TMP_BUFFR to Video RAM and palette
; Input: A = 0 = 1bpp, 1 = 4bpp
; Output: BC = Length of copied data
; Sets flags: Carry if invalid data length
; Clobbers: A, AF', BC, DE, HL
;-----------------------------------------------------------------------------
bitmap_write_tmpbfr:
    or      a
    jr      nz,.do4bpp            ; If A = 0
    ld      bc,8000
    call    .bitmap               ;   Write pixel data to 0-7999          
    ld      hl,8192
    ld      de,8000
    ld      bc,1000
    call    .copy                 ;   Write color data to 8000-8999             
    ld      de,BANK1_BASE+9000
    call    .palette              ;   Write palette to 9000-9031
    ld      bc,9032               ;   Return DatLen = 9032
    ret                           ; Else
.do4bpp
    ld      bc,16000              
    call    .bitmap               ;   Write pixel data to 0-15999
    ld      de,BANK1_BASE+16000
    call    .palette              ;   Write palette to 16000-16031
    ld      bc,16032              ;   Return DatLen = 9032
    ret
.bitmap
    ld      hl,0
    ld      de,0
.copy
    ld      a,VIDEO_RAM           ; Copying from Video RAM
    ex      af,af'
    ld      a,TMP_BUFFR           ; to buffer
    jp      page_fast_copy
; Enter with DE pointing to bitmap data
.palette
    call    page_map_tmpbfr_af
    ld      a,1
    ld      bc,32
    call    palette_get
    jp      page_restore_bank1_af

;-----------------------------------------------------------------------------
; Copy 1bpp ColorMAP data in TMP_BUFFR to Video RAM
; Input: BC = Length of data to copy
; Sets flags: Carry if invalid data length
; Clobbers: A, AF', BC, DE, HL
;-----------------------------------------------------------------------------
colormap_read_tmpbfr:
    ld      h,b
    ld      l,c
    ld      de,1000
    rst     COMPAR
    scf                           ; If FilLen <> 1000
    ret     nz                    ;   Return Carry Set
    ld      de,8192
    jr      _copy_tmpbase_vidram
; Input: BC: BytCnt
; Output: DE: NewDstAdr, HL: NewSrcAdr
_copy_tmpbase_vidbase:
    ld      de,0
_copy_tmpbase_vidram:
    ld      hl,0
; Input: BC: BytCnt, DE: DstAdr, HL: SrcAdr
; Output: DE: NewDstAdr, HL: NewSrcAdr
_copy_tmpbfr_vidram:
    ld      a,TMP_BUFFR           ; Copying from buffer
    ex      af,af'
    ld      a,VIDEO_RAM           ; to Video RAM
    jp      page_fast_copy

;-----------------------------------------------------------------------------
; Copy 1bpp ColorMap data in Video RAM to TMP_BUFFR
; Output: BC: ColorMap length
; Clobbers: A, AF', DE, HL
;-----------------------------------------------------------------------------
colormap_write_tmpbfr:
    ld      bc,1000               ; Copying 1000 bytes 
    push    bc                    ; Stack = DatLen, RtnAdr
    ld      hl,8192               ; from ColorMap
    ld      de,0                  ; to Start of buffer
copy_vidram_tmpbfr
    ld      a,VIDEO_RAM           ; Copying from Video RAM
    ex      af,af'
    ld      a,TMP_BUFFR           ; to buffer
    call    page_fast_copy_sys
    pop     bc                    ; BC = DatLen, Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; Copy text color map from temp buffer into Screen RAM
; Input: BC: Source file Length
; Sets flags: Carry if data length does not match screen mode
; Clobbers: AF,AF',BC,DE,HL
; 40 column mode only: 1024 bytes
; 80 column mode only: 2048 bytes
;-----------------------------------------------------------------------------
color_write_tmpbfr:
    ld      iy,_write_tmpbfr
    jr      _color_tmpbfr
color_read_tmpbfr:
    ld      iy,_read_tmpbfr
_color_tmpbfr:
    call    page_map_tmpbfr
    ld      a,c
    or      a                     ; If FileSize not multiple of 256
    jp      nz,_badfile           ;   Return Carry Set
    in      a,(IO_VCTRL)
    and     VCRTL_80COL_EN        ; Z if 40-column, NZ if 80-column
    ld      a,b
    jr      nz,.col80             ; If 40-column
    cp      $04                   ;   If FileSize <> 1024
    jp      nz,_badfile           ;     Return Carry Set
    ld      de,COLOR              ;   DstAdr = ColorRAM
    jp      (iy)
.col80
    cp      $08
    jp      nz,_badfile
    ld      hl,BANK1_BASE         ; SrcAdr = TmpBuf
_color80:
    ld      de,SCREEN             ; DstAdr = Screen
    in      a,(IO_VCTRL)
    push    af
    or      VCTRL_TEXT_PAGE
    out     (IO_VCTRL),a
    ldir
    pop     af
    out     (IO_VCTRL),a
    jp      _done

;-----------------------------------------------------------------------------
; Copy TMP_BUFFR to Screen RAM , Color RAM, Palette, and IO_VCTRL
; Input: BC = Length of data to copy
; Sets flags: Carry if data length does not match screen mode
; Clobbers: A, AF', BC, DE, HLh
;          ----- 40 Column Mode ------
; Length   SCREEN COLOR PALETTE BORDER    SCREEN COLOR PALETTE BORDER
;  1024        0

;  40 column: 1024 byte Screen RAM + 1024 byte Color RAM {+ 32 byte palette} {+ 1 byte border flag}
;  80 column: 2048 byte Screen RAM + 2048 byte Color RAM {+ 32 byte palette} {+ 1 byte border flag}
;-----------------------------------------------------------------------------
screen_read_tmpbfr:
    call    page_map_tmpbfr
    ld      de,SCREEN             ; DstAdr = Screen
    ld      hl,BANK1_BASE
    add     hl,bc
    dec     hl                    ; HL = EndByte
    in      a,(IO_VCTRL)
    and     VCRTL_80COL_EN        ; A = $40 if 80-column
    ld      a,b
    jr      nz,.col80             ; If 40-column
    cp      $04
    jr      nz,.not1024           ;   If Size = 1024 - 1279
    ld      a,c
    cp      1                     ;   If 1025
    call    z,.remap              ;     Copy remap bit
    jr      .screen               ;   Copy Screen RAM
.not1024
    cp      $08                   ;   If not 2048 - 2033
    jp      nz,_badfile           ;     Return error
    ld      a,c
    cp      33
    call    z,.remap
    cp      32
    call    z,.palette
.check2049
    cp      1
    call    z,.remap
.screen
    or      a                     ;   If Size = 1024
    jp      z,_read_tmpbfr_scrn   ;     Copy to Screen RAM and return
    jp      _badfile              ;   Else return error
.col80
    cp      $08
    jr      nz,.not2048           ; If 2048 - 2303
    ld      a,c
    jr      .check2049            ;   Copy to Screen RAM anf return
.not2048
    cp      $10                   ; If not 4096 - 4361
    jp      nz,_badfile           ;   Return error
    ld      a,c
    cp      33
    call    z,.remap
    cp      32
    call    z,.palette
    cp      1
    call    z,.remap
    or      a
    jp      nz,_badfile
    in      a,(IO_VCTRL)
    push    af
    or      VCTRL_TEXT_PAGE
    out     (IO_VCTRL),a          ; Switch to Color RAM
    ld      hl,BANK1_BASE+2048
    ld      de,SCREEN
    ld      bc,2048
    ldir                          ; Copy Color RAM data
    pop     af
    out     (IO_VCTRL),a          ;   Switch to Screen RAM
    ld      bc,2048
    jp      _read_tmpbfr_scrn

.palette:
    push    bc
    push    hl
    xor     a
    ld      d,h
    ld      e,a
    ld      l,a                   ; HL = PalAdr
    ld      bc,32
    ld      iy,palette_set
    call    gfxrom_call
    pop     hl
    pop     bc
    xor     a
    ld      c,a
    ld      l,a
    ret

.remap
    ld      a,(hl)                ; A = VCTRL
    and     VCTRL_REMAP_BC        ;
    ld      e,a                   ; B = vctrl_remap_bc
    in      a,(IO_VCTRL)
    and     $FF-VCTRL_REMAP_BC
    or      e                     ; Set VCTRL_REMAP_BC
    out     (IO_VCTRL),a          ; and write IO_VCTRL
    dec     c                     ; BC = PrvLen
    dec     l                     ; HL = PrvEnd
    ld      a,c
    ret

;-----------------------------------------------------------------------------
; Copy Screen, Palette, and IO_VCTRL to TMP_BUFFR
; Input: A: WriteOptBits 
;   (7 = Palette, 6 = Border, 2 = Swap, 0-1 = Screen#)
; Output: BC: Length of copied data
; Clobbers: AF, AF', DE, HL
;-----------------------------------------------------------------------------
screen_write_tmpbfr:
    ld      d,a                   ; D = WrtOpt
    and     7
    ld      e,a                   ; E = Swap+Scrn
    jr      z,_scrn_write_tmpbfr  ; If not writing active screen
    call    screen_mode           ;   A = ScrnMode
    cp      e                     ;   
    jr      z,_scrn_write_tmpbfr  ;   If not writing current screen
    call    _calc_buff_ofs         ;     HL = ScrnBuffOfs
    call    _calc_screen_len       ;     BC = ScrnLen
    ld      e,a                   ;     E = VarBufOfs
    push    de                    ;     Stack = WrtOpt+VarBufOfs, RtnAdr
    ld      de,0                  ;     DE = TmpBufOfs
    ld      a,SCR_BUFFR           ;      Copying from Screen Buffer
    ex      af,af'                
    ld      a,TMP_BUFFR           ;     to TempBuffer 
    call    page_copy_bytes_sys   ;     Copy Screen Data to TempBuffer
    pop     bc                    ;     WrtOpt+VarBufOfs
    ld      a,d
    and     $3F
    ld      b,a
    ld      c,e                   ;     BC = WriteLen
    ret

; Input: D = WrtOpt, E = Swap+Scrn
_scrn_write_tmpbfr:
    call    page_map_tmpbfr       ; Bank 1 = TempBuffer. AF, Af', IX clobbered
    in      a,(IO_VCTRL)
    push    af                    ; Stack = IO_VCTRL, RtnAdr
    push    de                    ; Stack = WrtOpt, IO_VCTRL, RtnAdr
    and     VCRTL_80COL_EN
    jr      z,.column40           ; If 80 column mode
    in      a,(IO_VCTRL)
    and     $FF-VCTRL_TEXT_PAGE
    out     (IO_VCTRL),a          ;   Switch to Screen RAM
    ld      ix,.copy              ; 
    bit     5,d                   ;   If not Color Only 
    jr      nz,.color80
    call    jump_ix               ;     Copy to buffer
    pop     af                    ;     A = WrtOpt; Stack = IO_VCTRL, RtnAdr
    push    af                    ;     Stack = WrtOpt, IO_VCTRL, RtnAdr
    bit     4,d                   ;     If Text Only
    jr      nz,.trailer           ;       Finish up
    ld      ix,.copy_next
.color80:
    in      a,(IO_VCTRL)
    or      VCTRL_TEXT_PAGE
    out     (IO_VCTRL),a          ;     Switch to Color RAM
    call    jump_ix               ;     and copy to buffer
    jr      .trailer              ; Else
.column40
    bit     4,d
    jr      nz,.text40
    bit     5,d
    jr      nz,.color40
    call    .copy
.trailer
    pop     af                    ; A = WrtOpt; Stack = IO_VCTRL, RtnAdr
    rla                           ; Carry = WritePalette, Bit7 = WriteRemap
    ld      h,a                   ; H = WrtOpt
    jr      nc,.dont_palette      ; If WritePalette
    xor     a                     ;   Start at palette index 0
    ld      bc,32                 ;   Total 32 bytes
    ld      iy,palette_get
    call    gfxrom_call           ;   Write palette to buffer. DE = NxtAdr
.dont_palette
    pop     af                    ; A = IO_VCTRL; Stack = RtnAdr
    out     (IO_VCTRL),a          ; Restore VCTRL
    sla     h                     ; Carry = WrtRmp
    jr      nc,.dont_remap        ; If WriteRemap
    and     VCTRL_REMAP_BC        ;   Check Remap Border flag
    ld      a,0                   ;   Border Control byte = 0 (False)
    jr      z,.remap_off          ;   If Remap Border flag set
    dec     a                     ;   Border Control byte = $FF (True)
.remap_off
    ld      (de),a                ;   Write Border control byte to buffer
    inc     de
.dont_remap:
    ex      de,hl                 ; HL = EndAdr
    ld      bc,-BANK1_BASE
    add     hl,bc                 ; HL = DatLen
    ld      b,h
    ld      c,l                   ; BC = DatLen
    jp      page_restore_bank1
.copy
    ld      de,BANK1_BASE
.copy_next
    ld      hl,SCREEN
    ld      bc,2048
    ldir
    ret

.text40
    ld      hl,SCREEN
    jr      .copy40
.color40
    ld      hl,SCREEN+1024
.copy40
    ld      de,BANK1_BASE
    ld      bc,1024
    ldir
    jr      .trailer
  
; Input: E: Swap+Scrn; Output: H: VarOfs, HL: BufAdr; Clobbers: A
_calc_buff_ofs:
    ld      a,e
    and     3
    dec     a                     ; $00, $01, or $02
    rla                           ; $00, $02, or $04
    rla                           ; $00, $04, ot $08
    rla                           ; $00, $08, ot $10
    ld      h,a
    ld      l,0                   ; HL = $0000, $0800, or $10000
    ld      a,e
    and     4                     ; If not Swap
    ret     z                     ;   Return
    ld      a,$20
    add     h                     ; $20, $28, or $30
    ld      h,a                   ; HL = $2000, $2800, or $3000
    ret                           

; Input: E: Swap+Scrn; Output: DE: ScrnLen; Clobbers: A
_calc_screen_len:
    ld      bc,$0800               ; ScrnLen = 2048 (40 column)
    ld      a,e
    and     3                     ; A = Screen#
    cp      3                     ; If Screen# <> 3
    ret     nz                    ;   Return 2048
    ld      b,$10                 ; Else
    ret                           ;   Return 4096

;-----------------------------------------------------------------------------
; Copy TMP_BUFFR to Screen RAM
; Input: BC = Length of data to copy
; Sets flags: Carry if data length does not match screen mode
; Clobbers: A, AF', BC, DE, HL
;-----------------------------------------------------------------------------
text_read_tmpbfr:
    call    page_map_tmpbfr
    in      a,(IO_VCTRL)
    and     VCRTL_80COL_EN        ; A = $40 if 80-column
    add     $40                   ; A = $40 if 40-column, $80 if 80 column
    cp      b
    jp      nz,_badfile
    ld      a,c
    and     a
    jp      nz,_badfile
    jp      _read_tmpbfr_scrn


_write_tmpbfr:
    ex      de,hl
    ld      de,BANK1_BASE
    jr      _copy
_write_tmpbfr_scrn:
    ld      hl,SCREEN             ; DstAdr = Screen
    ld      de,BANK1_BASE         ; SrcAdr = TmpBuf
    jr      _copy
_read_tmpbfr_scrn:
    ld      de,SCREEN             ; DstAdr = Screen
_read_tmpbfr:
    ld      hl,BANK1_BASE         ; SrcAdr = TmpBuf
_copy:
    ldir                          ;   Copy it
_done:
    xor     a                     ; Clear Carry
    byte    $06                   ; LD B, over SCF
_badfile:
    scf
    jp      page_restore_bank1
