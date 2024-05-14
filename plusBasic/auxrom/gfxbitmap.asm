;====================================================================
; Bitmap Graphics Routines
;====================================================================


;-----------------------------------------------------------------------------
; Initialize bitmap sysvars
;-----------------------------------------------------------------------------
_bmp_defaults
    byte    $06,0,0,0,$06,0,0,0,$70,0,0,0,$07,0,0,0
_bmp_deflen = $ - _bmp_defaults   

bitmap_init_vars:
    ld      hl,_bmp_defaults
    ld      de,BANK1_BASE+GFXVBASE
    ld      bc,_bmp_deflen
    in      a,(IO_BANK1)
    ex      af,af'                ; A' = OldPg
    ld      a,BAS_BUFFR
    out     (IO_BANK1),a
    ldir
    ex      af,af'                ; A = OldPg, af' = BMP_DRAWCOLOR
    out     (IO_BANK1),a          ; Map Original Page
    ret

;-----------------------------------------------------------------------------
; Set bitmap mode system variable from video control register
; Input: A: Use buffer: $FF = Yes, 0 = No
; Clobbered: A, B, C
;-----------------------------------------------------------------------------
bitmap_set_mode_nobuff:
    xor     a
bitmap_set_mode:
    and     GFXBUFLAG
    ld      b,a                   ; B = BufFlg
    in      a,(IO_VCTRL)          
    ld      c,a
    rra                           ; A = 2: 1bpp, 3: 4bpp
    and     GFXM_MASK             ; Isolate bits
    cp      2                     ; If Text
    jr      nc,.write             ;   
    ld      a,c                   ;   A = VCTRL
    and     a,VCRTL_80COL_EN      ;   If 40 column
    jr      z,.write              ;     A = 0
    ld      a,1                   ;   Elss A =1
.write 
    or      b                     ; Include BufFlg
    ld      c,a                   ; C = New flags
    ld      a,(EXT_FLAGS)         
    and     GFXSETMSK             ; Clear old flags
    or      c                     ; Set new flags
    ld      (EXT_FLAGS),a
    ret                     

;-----------------------------------------------------------------------------
; Read bitmap system variables
; Output: B: Colors
;         C: Y-coordinate
;        DE: X-coordinate
; Clobbered: A, HL
;-----------------------------------------------------------------------------
bitmap_read_sysvars:
    call    _get_varbase          ; HL = VarBase
    in      a,(IO_BANK1)
    ex      af,af'                ; AF' = OldPg
    ld      a,BAS_BUFFR
    out     (IO_BANK1),a          ; Map Basic Buffers
    ld      b,(hl)                ; A = BMP_DRAWCOLOR
    inc     hl
    ld      c,(hl)
    inc     hl
    ld      e,(hl)
    inc     hl
    ld      d,(hl)                ; DE = BMP_LASTX
    ex      af,af'                ; A = OldPg, af' = BMP_DRAWCOLOR
    out     (IO_BANK1),a          ; Map Original Page
    ret

;-----------------------------------------------------------------------------
; Read Bitmap Draw Color
; Output: A,B: Colors
;-----------------------------------------------------------------------------
bitmap_read_color:
    call    _get_varbase          ; HL = VarBase
    in      a,(IO_BANK1)
    ex      af,af'                ; A' = OldPg
    ld      a,BAS_BUFFR
    out     (IO_BANK1),a
    ld      b,(hl)                ; A = Byte
    ex      af,af'                ; A = OldPg, A' = Byte
    out     (IO_BANK1),a          ; Restore Page
    ld      a,b                   ; A = Byte
    ret

; In: A: GfxMode; Out: HL: VarBase
_get_varbase:
    ld      a,(EXT_FLAGS)
    and     GFXM_MASK             ; A = BmpMode
    ld      h,high(BANK1_BASE+GFXVBASE)
    ld      l,a
    sla     l
    sla     l                     ; HL = SysVar Base
    ret

;-----------------------------------------------------------------------------
; Write to bitmap draw color system variable
; Input: B: Color(s)
; Clobbered: A, HL
;-----------------------------------------------------------------------------
bitmap_write_color:
    call    _get_varbase          ; HL = VarBase
    in      a,(IO_BANK1)
    ex      af,af'                ; A' = OldPg
    ld      a,BAS_BUFFR
    out     (IO_BANK1),a
    ld      (hl),b
    ex      af,af'                ; A = OldPg, A' = Byte
    out     (IO_BANK1),a          ; Restore Page
    ret

;-----------------------------------------------------------------------------
; Fill Bitmap with Byte
; Input: A: Mode (0: 40col, 1: 80col, 2: 1bpp, 3: 4bpp)
;        B: Byte
; Clobbered: AF,AF',BC,DE,HL
;-----------------------------------------------------------------------------
bitmap_fill_byte:
    ld      a,(EXT_FLAGS)
    and     GFXM_MASK             ; A = GfxMode
    ld      d,b                   ; D = FillByte
    ld      hl,$3000
    ld      bc,1000
    jp      z,sys_fill_mem_d
    ld      bc,2000
    dec     a
    jp      z,sys_fill_mem_d
    ld      hl,BANK1_BASE+BMP_BASE
    dec     a
    ld      bc,8000
    jr      z,_fill_video_ram
    ld      bc,16000
_fill_video_ram:
    in      a,(IO_BANK1)          ; A = OrigPg
    ex      af,af'                ; A' = OrigPg
    ld      a,VIDEO_RAM           ; A = NewPg
    out     (IO_BANK1),a          ; Map video RAM
    ld      a,d                   ; A = FillByte
    call    sys_fill_mem          ; Do the fill
    ex      af,af'                ; A = OrigPg
    out     (IO_BANK1),a          ; Restore original page
    ret

;-----------------------------------------------------------------------------
; Clear Bitmap
; Input: B: Screen colors (0 = use default)
; Clobbered: AF,AF',BC,DE,HL
;-----------------------------------------------------------------------------
bitmap_clear_screen:
    ld      a,(EXT_FLAGS)
    and     GFXM_MASK
    push    af                    ; Stack = BmpMode, RtnAdr
    push    bc                    ; Stack = Color, BmpMode, RtnAdr
    and     GFXM_1BPP             ; If Text Screen
    ld      b,$A0                 ;   Fill with $A0
    jr      z,.fill               ; Else
    ld      b,0                   ;   Fill with $00
.fill
    call    bitmap_fill_byte      ; Do the fill
    pop     bc                    ; B = Color, Stack = BmpMode, RtnAdr
    pop     af                    ; A = BmpMode; Stack = RtnAdr
    cp      GFXM_4BPP             ; If 4bpp bitmap
    ret     z                     ;   Return
    ld      a,b                   ; A = Color
    or      a                     ; If color = 0
    call    z,bitmap_read_color   ;   Get default colors
;-----------------------------------------------------------------------------
; Fill Bitmap Color RAM
; Input: A: Mode (0: 40col, 1: 80col, 2: 1bpp, 3: 4bpp)
;        B: Byte
; Clobbered: AF,AF',BC,DE,HL
;-----------------------------------------------------------------------------
bitmap_fill_color:
    ld      a,(EXT_FLAGS)
    and     GFXM_MASK
    ld      d,b
    ld      hl,$3400
    ld      bc,1000               ; BC = Count
    or      a
    jp      z,sys_fill_mem_d
    dec     a
    jr      z,.fill80
    ld      hl,BANK1_BASE+BMP_BASE+8192
    ld      bc,1000
    dec     a
    jr      z,_fill_video_ram
    scf                           ; If 4bpp
    ret     c                     ;   Return error
.fill80
    ld      hl,$3000
    ld      bc,2000
    in      a,(IO_VCTRL)
    or      VCTRL_TEXT_PAGE       ; Select color page
    out     (IO_VCTRL),a
    ex      af,af'
    call    sys_fill_mem_d
    ex      af,af'
    and     $FF-VCTRL_TEXT_PAGE
    out     (IO_VCTRL),a
    ret


;-----------------------------------------------------------------------------
; Draw line on 1bpp bitmap screen
; Input: B: Y1
;        C: Y2
;       DE: X1
;       HL: X2
; Output:
; Clobbered: A
;-----------------------------------------------------------------------------
bitmap_line:

;-----------------------------------------------------------------------------
; Move one pixel and set/reset point
;  Input: A: 0 = Move, 1 = Set, 255 = Reset
;         C: Y-coordinate
;        DE: X-coordinate
;         H: 255 = Up, 1 = Down
;         L: 255 = Left, 1 = Right
; Output: C: New Y-coordinate
;        DE: New X-coordinate
;      Sets: Carry if new position out of bounds
; Clobbered: A, B, HL
;-----------------------------------------------------------------------------
bitmap_move:
    ex      af,af'                ; AF' = Action
    xor     a                     ; A = 0
    sla     h                     ; Set Carry if Up, NotZero if Down
    jr      nc,.notup             ; If Up
    dec     a                     ;   A = -1
    add     a,c                   ;   NewY = Y - 1
    ret     c                     ;   If NewY < 0, return Carry Set
    ld      c,a                   ;   Else C = NewY
    jr      .notdown
.notup
    jr      nz,.notdown           ; ElseIf Down
    inc     a                     ;   A = 1
    add     a,c                   ;   NewY = Y + 1
    cp      200
    ccf                           ; ``If NewY >= 100
    ret     c                     ;     Return Carry Set
    ld      c,a                   ;   Else C = NewY
.notdown:
    ld      h,a                   ; H = 0
    sla     l                     ; Set Carry if Up, NotZero if Down
    ld      l,a                   ; HL = 0
    jr      nc,.notleft           ; If Up
    dec     hl                    ;   HL = -1
    add     hl,de                 ;   NewX = X - 1
    ex      de,hl                 ;   DE = NewX
    ret     c                     ;   If NewX < 0, return Carry Set
    jr      .notright             ;
.notleft
    jr      nz,.notright          ; ElseIf Down
    inc     hl                    ;   HL = 1
    add     hl,de                 ;   NewX = X + 1
    ex      de,hl                 ;   DE = NewX
    ld      hl,-320               ;
    add     hl,de                 ;   HL = HL - 320
    ccf                           ;   If NewY >= 320
    ret     c                     ;     Return Carry Set
.notright
    push    bc                    ; Stack = NewY, RtnAdr
    push    de                    ; Stack = NewX, NewY, RtnAdr
    call    bitmap_setpixel       ; Set the pixel
    pop     de                    ; DE = NewX; Stack = NewY, RtnAdr
    pop     bc                    ; BC = NewY; Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; Set foreground and background colors of bitmap cell
;  Input: A: Mode (0: 40col, 1: 80col, 2: 1bpp, 3: 4bpp)
;         B: Cell colors
;         C: Y-coordinate
;        DE: X-coordinate
;    Output: A: New byte containing pixel
;      Sets: Carry if coordinates out of range
; Clobbered: A, BC, DE, HL
;-----------------------------------------------------------------------------
bitmap_setcell:
    cp      3                     ; If Mode >= 3
    ccf                           ;   Set carry
    ret     c                     ;   and return
    cp      2                     ; If Mode < 2
    ret     c                     ;   Return carry (for now)

;-----------------------------------------------------------------------------
; Return pixel/bloxel at position
;  Input: A: Mode
;         C: Y-coordinate
;        DE: X-coordinate
;    Output: A = Pixel value
;      Sets: Csrry if coordinates out of range
; Clobbered: BC, DE, HL
;-----------------------------------------------------------------------------
bitmap_getpixel:
    ld      a,(EXT_FLAGS)
    and     GFXM_MASK             ; Mask bits and set flags
    ld      l,a                   ; L = GfxMode
    ld      ix,_getpixel4         ;
    jr      z,_dopixel          ;   Do Bloxel 40
    ld      ix,_getpixel8         ; Bloxel80
    dec     a                     ; If GfxMode = 1
    jr      z,_dopixel          ;   Do Bloxel 80
    ld      ix,_getpixelm         ;
    dec     a                     ; If GfxMode = 2
    jr      z,_dopixel          ;   Do Bitmap 1bpp
    ld      ix,_getpixelc         ; Else fo Bitmap 4bpp
    jr      _dopixel

;-----------------------------------------------------------------------------
; Erase pixel on 1 bpp bitmap screen
;  Input: C: Y-coordinate
;        DE: X-coordinate
;    Output: AF': New byte containing pixel
;      Sets: Csrry if coordinates out of range
; Clobbered: A, BC, DE, HL
;-----------------------------------------------------------------------------
bitmap_resetpixel:
    ld      a,(EXT_FLAGS)
    and     GFXM_MASK             ; Mask bits and set flags
    ld      l,a                   ; L = GfxMode
    ld      ix,_resetpixel4
    jr      z,_dopixel            ;   Do Bloxel 40
    ld      ix,_resetpixel8       ; Bloxel80
    dec     a                     ; If GfxMode = 1
    jr      z,_dopixel            ;   Do Bloxel 80
    ld      ix,_resetpixelm
    dec     a                     ; If GfxMode = 2
    jr      z,_dopixel            ;   Do Bitmap 1bpp
    ld      ix,_resetpixelc       ; Else fo Bitmap 4bpp
    jr      _dopixel

;-----------------------------------------------------------------------------
; Draw pixel
;  Input: B: Draw color(s3) (0 = Default, $FF = None)
;         C: Y-coordinate
;        DE: X-coordinate
;      Sets: Carry if coordinates out of range
; Clobbered: A, HL
;-----------------------------------------------------------------------------
bitmap_setpixel:
    ld      a,(EXT_FLAGS)
    and     GFXM_MASK             ; Mask bits and set flags
    ld      l,a                   ; L = GfxMode
    ld      ix,_setpixel4         ;
    jr      z,.dopixel            ;   Do Bloxel 40
    ld      ix,_setpixel8         ; Bloxel80
    dec     a                     ; If GfxMode = 1
    jr      z,.dopixel            ;   Do Bloxel 80
    ld      ix,_setpixelm         ;
    dec     a                     ; If GfxMode = 2
    jr      z,.dopixel            ;   Do Bitmap 1bpp
    ld      ix,_setpixelc         ; Else fo Bitmap 4bpp
.dopixel
    call    _dopixel
    ld      a,(EXT_FLAGS)
    and     GFXM_MASK             ; 
    cp      3                     ; If 4bpp
    call    nz,_set_cellcolor
    ret

_dopixel:
    ld      a,l                   ; A = GfxMode
    call    _check_coords
    ret     c
    in      a,(IO_BANK1)
    push    af                    ; Stack = OldPg, RtnAdr
    push    bc                    ; Stack = ColorY, OldPg, RtnAdr
    push    de                    ; Stack = X, ColorY, OldPg, RtnAdr
    ld      a,VIDEO_RAM
    out     (IO_BANK1),a
    call    jump_ix
    ex      af,af'                ; A' = NewByte
    pop     de                    ; DE = X; Stack = ColorY, OldPg, RtnAdr
    pop     bc                    ; BC = ColorY; Stack = OldPg, RtnAdr
    call    _get_varbase          ; HL = VarAdr
    ld      a,BAS_BUFFR           ; Write sysvars
    out     (IO_BANK1),a
    inc     hl
    ld      (hl),c
    inc     hl
    ld      (hl),e
    inc     hl
    ld      (hl),d
    pop     af                    ; A = OldPg; Stack = RtnAdr
    out     (IO_BANK1),a
    ex      af,af'                ; A = NewByte
    ret

_set_cellcolor:
    ld      a,b                   ; A = Color
    or      a                     ; If Color = 0
    ret     z                     ;   Return
_set_cellcolorm:
    in      a,(IO_BANK1)
    ex      af,af'
    ld      a,VIDEO_RAM
    out     (IO_BANK1),a
    ld      a,b
    call    _calc_1bpp_cell       ; HL = Cell address
    ld      (hl),a
    ex      af,af'
    out     (IO_BANK1),a
    ret

_getpixel8:
_getpixel4:
_getpixelm:
    call    _calc_1bpp_addr           ; DE = BytAdr; BC = PxlOfs
    ld      hl,_ormask1bpp
    add     hl,bc                 ; HL = MskAdr
    ld      b,(hl)                ; B = BitMsk
    ld      a,(de)                ; Get byte
    and     b                     ; If bit not set
    ret     z                     ;   Return 0
    ld      a,1                   ; Else return 1
    ret

_setpixel4:
_setpixel8:
_resetpixelm:
    call    _calc_1bpp_addr           ; DE = BytAdr; BC = PxlOfs
    ld      hl,_andmask1bpp
    add     hl,bc                 ; HL = MskAdr
    ld      b,(hl)                ; B = BitMsk
    ld      a,(de)
    and     b
    ld      (de),a
    ret

_getpixelc:
    call    _calc_4bpp_addr;         ; AF = NybOfs, DE = BytAdr; BC = PxlOfs
    ld      hl,_getmask
    add     hl,bc                 ; HL = MskAdr
    ld      b,(hl)                ; B = BitMsk
    ld      a,(de)
    and     b
    rl      b                     ; Set carry if mask = $F0
    ret     nc                    ; Color in right nybble, so return
    ccf                           ; Clear the carry flag
    rr      a
    rr      a
    rr      a                     ; Move left nybble to right
    rr      a                     ; and return it
    ret


; C = Y-coordinate, DE: X-coordinate
; bloxel
_resetpixel4:
_resetpixel8:
; 1bpp
_setpixelm:
    call    _calc_1bpp_addr           ; DE = BytAdr; BC = PxlOfs
    ld      hl,_ormask1bpp
    add     hl,bc                 ; HL = MskAdr
    ld      b,(hl)                ; B = BitMsk
    ld      a,(de)
    or      b
    ld      (de),a
    ret
; 4bpp


_resetpixelc:
    xor     a                     ; DrwClr = 0 (Background)
    jr      _pixelc
_setpixelc:
    ld      a,b                   ; A = DrwClr
    and     $0F                   ; Force color to 0 - 15
    call    z,bitmap_read_color
_pixelc
    push    af                    ; Stack = Color, RtnAdr
    call    _calc_4bpp_addr       ; AF = NybOfs, DE = BytAdr; BC = PxlOfs
    pop     hl                    ; H = Color; Stack = RtnAdr
    ld      a,h                   ; A = Color (preserve flags)
    jr      nz,.noshift           ; If Odd X-coordinate
    rla                           ; Shift color to high nybble
    rla
    rla
    rla
.noshift
    ld      hl,_setmask
    add     hl,bc                 ; HL = MskAdr
    ld      b,(hl)                ; B = BitMsk
    ld      c,a
    ld      a,(de)
    and     b
    or      c
    ld      (de),a
    ret


;-----------------------------------------------------------------------------
; Calculate 1bpp bitmap color cell address
;   Input: C: Y-coordinate
;         DE: X-coordinate
; Output: HL: Cell address
; Clobbered: BC, DE
;-----------------------------------------------------------------------------
_calc_1bpp_cell:
    sra     d
    rr      e
    srl     e
    srl     e
    ld      d,0                   ; DE = ColOfs
    srl     c
    srl     c
    srl     c                     ; C = Y/8
    call    _mult40c              ; HL = RowOfs
    add     hl,de                 ; HL = BytOfs
    ld      de,BANK1_BASE+BMP_COLORRAM
    add     hl,de                 ; HL = BytAdr
    ret

;-----------------------------------------------------------------------------
; Calculate 4bpp bitmap coordinate address
;   Input: C: Y-coordinate
;          E: X-coordinate
; Output: BC: Pixel Offset
;         DE: Coordinate Address
; Clobbered: HL
;-----------------------------------------------------------------------------
_calc_4bpp_addr:
    ld      a,e
    and     1                     ; A = Nybble
    ex      af,af'                ; A' = PxlOfs
    srl     e                     ; E = BytOfs
    ld      d,high(BANK1_BASE)    ; DE = VidRAM + BytOfs
    call    _mult40c              ; HL = RowAdr
    add     hl,hl                 ; HL = Y * 80
    jr      __calc_1bpp_addr

;-----------------------------------------------------------------------------
; Calculate 1bpp bitmap coordinate address
;   Input: C: Y-coordinate
;         DE: X-coordinate
; Output: BC: Pixel Offset
;         DE: Coordinate Address
; Clobbered: HL
;-----------------------------------------------------------------------------
_calc_1bpp_addr:
    ld      a,e
    and     7                     ; A = X mod 8
    ex      af,af'                ; A' = PxlOfs
    srl     d                     ; E = X / 8
    rr      e                     ;
    srl     e
    srl     e                     ; E = BytOfs
    ld      d,high(BANK1_BASE)    ; DE = VidRAM + BytOfs
    call    _mult40c              ; HL = RowAdr
__calc_1bpp_addr:
    add     hl,de                 ; HL = BytAdr, B = 0
    ex      de,hl                 ; DE = BytAdr
    ex      af,af'
    ld      c,a                   ; BC = PxlOfs
    ret

; Must preserve AF'
; A: GfxMode, DE: X, C: Y
; Clobbers A, HL
_check_coords:
    rra                           ; Carry = TextMode
    rra
    jr      c,.bitmap             ; If bloxel
    rla                           ;   Carry = 40/80
    ld      a,72                  ;   MaxY=72
    jr      c,.x160               ;   If 80, MaxX = 160 and check
    ld      hl,-80                ;   Else MaxX = 80 and check
    jr      .check_coords
.bitmap
    rla                           ;   Carry = 1bpp/4bpp
    ld      a,199                 ;   MaxY = 199
    ld      hl,-320               ;
    jr      nc,.check_coords      ;   If 1bpp MaxX=320
.x160
    ld      hl,-160               ;   Else MaxX = 160
; DE = X, C = Y, HL = MaxX, A = MaxY
.check_coords:
    add     hl,de                 ; If HL >= MaxX
    ret     c                     ;   Return carry set
    cp      c                     ; If Y >= MaxY
    ret                           ;   Return carry set

; HL = C * 40
; Clobbered: B
_mult40c:
    ld      b,0
    ld      h,b
    ld      l,c                   ; HL = X
    add     hl,hl                 ; HL = C * 2
    add     hl,hl                 ; HL = C * 4
    add     hl,bc                 ; HL = C * 5
    add     hl,hl                 ; HL = C * 10
    add     hl,hl                 ; HL = C * 20
    add     hl,hl                 ; HL = C * 40
    ret

;-----------------------------------------------------------------------------
; Graphics Lookup tables
;-----------------------------------------------------------------------------
_ormask1bpp:
    byte    $80,$40,$20,$10,$08,$04,$02,$01
_andmask1bpp:
    byte    $7F,$BF,$DF,$EF,$F7,$FB,$FD,$FE
_setmask:
    byte    $0F,$F0
_getmask:
    byte    $F0,$0F

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
;  9192 $23E8 1bpp pixel + 192 byte gap + color
; 16000 $3E80 4bpp pixels only
; 16032 $3EA0 4bpp pixels + palette
;-----------------------------------------------------------------------------
bitmap_read_tmpbfr:
    ld      h,b
    ld      l,c                   ; HL = DatLen
    ld      de,$03E8
    rst     COMPAR                ; If DatLen < 1000 
    ret     c                     ;   Return carry set
    ld      de,$14F0              ; 
    rst     COMPAR                ; If DatLen < 8000 
    jr      c,.color              ;   Copy 1bpp color only
    ld      de,$2328              ; 
    rst     COMPAR                ; Else If DatLen < 9000 
    jr      c,.bitmap1bpp         ;   Copy 1bpp pixels only
    ld      e,$48                 ; 
    rst     COMPAR                ; Else If DatLen < 9000 
    jr      c,.color1bpp          ;   Copy 1bpp pixels + color
    ld      e,$E8                 ; 
    rst     COMPAR                ; Else If DatLen < 9192
    jr      c,.palette1bpp        ;   Copy 1bpp pixels + color + palette
    ld      de,$3E80
    rst     COMPAR                ; Else If DatLen < 16000
    jr      c,.bitgap1bpp         ;   Copy 1bpp pixels + color + palette
    ld      e,$A0
    rst     COMPAR                ; Else If DatLen < 16032
    jr      c,.bitmap4bpp         ;   Copy 4bpp pixels
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
    ld      bc,9192               
    call    .bitmap
    ld      de,BANK1_BASE+8000
    jr      .palette
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
    ld      a,TMP_BUFFR           ; Copying from buffer
    ex      af,af'
    ld      a,VIDEO_RAM           ; to Video RAM
    jp      page_fast_copy
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
; Read Bitmap Screen Section into Buffer
; Input: BMPMODE: Mode: 0 = 1bpp, 1 = 4bpp
;        STARTCOL: Start Column
;        STARTROW: Start Row
;        HL: End Row
;        ENDCOL: End Row
;        BUFADR: Buffer Address
;        BUFLEN: Buffer Length
;  Flags: Carry Set if invalid coordinates
;         Not Zero if buffer overflow
; Clobbered: A, AF', BC, DE, HL, IX, IY
;--------------`---------------------------------------------------------------
bitmap_get:
    call    get_set_init         ; If coordinates out of range
    ret     c                     ;   Return Carry Set
    ret
    
    jr      nz,.get_4bpp          ; If A <> 0
    
.get_4bpp

get_set_init:
    ld      hl,(BUFADR)           
    ld      (BUFPTR),hl           ; BufPtr = BufAdr
    ld      bc,(BUFLEN)
    add     hl,bc                 ; HL = BufEnd
    ld      (BUFLEN),hl           ; BUFLEN = BufEnd
    ld      a,(BMPMODE)             
    and     a,1                   ; A = 0 (1bpp), 1 (4bpp)
    or      2                     ; A = GfxMode
    ld      (BMPMODE),a           ; BMPMODE = GfxMode
    ld      de,(STARTCOL)
    ld      bc,(STARTROW)
    call    _check_coords         ; If start coordinates out of range
    ret     c                     ;   Return Carry Set
    ld      a,(BMPMODE)           ; A = GfxMode
    ld      de,(ENDCOL)
    ld      bc,(ENDROW)
    call    _check_coords         ; If end coordinates out of range
    ret                           ;  Return Carry Set


_bitmap_code_size = $ - bitmap_resetpixel
