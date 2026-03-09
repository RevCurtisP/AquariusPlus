;====================================================================
; Graphics Pixel and Bloxel Drawing Kernel Routines
;====================================================================
;; Routines being moved from auxrom to gfxrom

;-----------------------------------------------------------------------------
; Return pixel/bloxel at position
;  Input: A: Mode
;         C: Y-coordinate
;        DE: X-coordinate
;    Output: A = Pixel value
;      Sets: Csrry if coordinates out of range
; Clobbered: BC, DE, HL
;-----------------------------------------------------------------------------
bitmap__getpixel:
    ld      a,(GFX_FLAGS)
    and     GFXM_MASK             ; Mask bits and set flags
    ld      l,a                   ; L = GfxMode
    ld      h,0                   ; Don't update LastX, Last Y
    ld      ix,_getpixel40        ; If 40 column
    jr      z,_dopixel            ;   Do Bloxel 40
    ld      ix,_getpixel80        ; 
    dec     a                     ; If 80 column
    jr      z,_dopixel            ;   Do Bloxel 80
    ld      ix,_getpixelm         ;
    dec     a                     ; If GfxMode = 2
    jr      z,_dopixel            ;   Do Bitmap 1bpp
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
bitmap__resetpixel:
    call    _resetpixel_addr
    jr      _dopixel

_resetpixel_addr:
    ld      a,(GFX_FLAGS)
    and     GFXM_MASK             ; Mask bits and set flags
    ld      h,$FF                 ; Update LastX, Last Y
    ld      l,a                   ; L = GfxMode
    ld      ix,_resetpixel40
    ret     z                     ;   Do Bloxel 40
    ld      ix,_resetpixel80
    dec     a                     ; If GfxMode = 1
    ret     z                     ;   Do Bloxel 80
    ld      ix,_resetpixelm
    dec     a                     ; If GfxMode = 2
    ret     z                     ;   Do Bitmap 1bpp
    ld      ix,_resetpixelc       ; Else fo Bitmap 4bpp
    ret

;-----------------------------------------------------------------------------
; Draw pixel
;  Input: A: Draw color - 0 = Default (4bpp), None (1bpp/Bloxel)
;        BC: Y-coordinate
;        DE: X-coordinate
;      Sets: Carry if coordinates out of range
; Clobbered: A, HL
;-----------------------------------------------------------------------------
bitmap__setpixel:
    ld      (PSETCOLOR),a         ; Save DrwClr
    call    _setpixel_addr        ; L = GfxMode, IX = SetPxlAdr
; On Entry C = X-Coord, DE = Y-Coord, H = UpdateXY, L = GfxMode 
_dopixel:
    push    hl                    ; Stack = UpdateXY, RtnAdr
    ld      a,l                   ; A = GfxMode
    call    _check_coords
    pop     hl                    ; HL = UpdateXY; Stack = RtnAdr
    ret     c
; On Entry BC = X-Coord, DE = Y-Coord, H = UpdateXY, L = GfxMode 
_drawpixel:
    in      a,(IO_BANK1)
    push    af                    ; Stack = OldPg, RtnAdr
    push    bc                    ; Stack = ColorY, OldPg, RtnAdr
    push    de                    ; Stack = X, ColorY, OldPg, RtnAdr
    push    hl                    ; Stack = UpdateXY, X, ColorY, OldPg, RtnAdr
    ld      a,VIDEO_RAM
    out     (IO_BANK1),a
    call    jump_ix
    ex      af,af'                ; A' = NewByte
    pop     hl                    ; H = UpdateXY; Stack = X, ColorY, OldPg, RtnAdr
    pop     de                    ; DE = X; Stack = ColorY, OldPg, RtnAdr
    pop     bc                    ; BC = ColorY; Stack = OldPg, RtnAdr
    ld      a,h
    or      a
    jr      z,.done
    call    get_varbase           ; HL = VarAdr
    ld      a,BAS_BUFFR           ; Write sysvars
    out     (IO_BANK1),a
    inc     hl
    ld      (hl),c
    inc     hl
    ld      (hl),e
    inc     hl
    ld      (hl),d
.done
    pop     af                    ; A = OldPg; Stack = RtnAdr
    out     (IO_BANK1),a
    ex      af,af'                ; A = NewByte
    ret

bitmap__togglepixel:
    call    _togglepixel_addr
    jr      _dopixel

_togglepixel_addr:
    ld      a,(GFX_FLAGS)
    and     GFXM_MASK             ; Mask bits and set flags
    ld      h,$FF                 ; Update LastX, Last Y
    ld      l,a                   ; L = GfxMode
    ld      ix,_togglepixel40     ;
    ret     z                     ;   Do Bloxel 40
    ld      ix,_togglepixel80         ; Bloxel80
    dec     a                     ; If GfxMode = 1
    ret     z                     ;   Do Bloxel 80
    ld      ix,_togglepixelm      ;
    dec     a                     ; If GfxMode = 2
    ret     z                     ;   Do Bitmap 1bpp
    ld      ix,_togglepixelc      ;
    ret



; 40 column bloxel subroutines
;--------------------------------------------------------------------
_getpixel40:
    call    _bloxel40
_getbloxel
    jr      nz,.notbloxel
    and     (hl)
    ret     z
    xor     a
    inc     a                     ; A = 1 with flags set
    ret
.notbloxel
    xor     a
    ret     
    
_resetpixel40:
    call    _bloxel40
_resetbloxel:
    jr      z,.reset
    ld      (hl),$A0              ;   Set to blank
.reset
    ld      a,(de)                ; A = BitMsk
    xor     $FF                   ; Invert Bitmap
    and     (hl)                  ; Clear Pixel
    ld      (hl),a
    ret

_setpixel40:
    call    _bloxel40
    jr      z,.set
    ld      (hl),$A0              ;   Set to blank
.set
    or      (hl)                  ; Set bit 
    ld      (hl),a                ; Write to screen
    call    _psetcolor
    ret     m
    set     2,h
_bloxel_color    
    ld      a,(hl)
    and     $0F
    or      b
    ld      (hl),a
    ret

; 80 column bloxel subroutines
;--------------------------------------------------------------------
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
_getpixel80:
    call    _bloxel80
    jr      _getbloxel

_resetpixel80:
    call    _bloxel80
    jr      _resetbloxel

_setpixel80:
    call    _bloxel80
    jr      z,.set
    ld      (hl),$A0              ;   Set to blank
.set
    or      (hl)                  ; Set bit 
    ld      (hl),a                ; Write to screen
    call    _psetcolor
    ret     m
    in      a,(IO_VCTRL)
    push    af                    ; Stack = IO_VCTRL, RtnAdr
    or      VCTRL_TEXT_PAGE
    out     (IO_VCTRL),a
    call    _bloxel_color
    pop     af                    ; A = IO_VCTRL; Stack = RtnAdr
    out     (IO_VCTRL),a
    ret

_togglepixel80:
    call    _bloxel80
    jr      _togglepixel
_togglepixel40:
    call    _bloxel40
_togglepixel:
    jr      z,.set
    ld      (hl),$A0              ;   Set to blank
.set
    xor     (hl)                  ; Set bit 
    ld      (hl),a                ; Write to screen
    ret

; Common bloxel subroutines
_psetcolor:
    ld      a,(PSETCOLOR)
    or      a
    ret     m
    and     $0F
    rla
    rla
    rla
    rla
    ld      b,a
    ret

_bloxel80:
    ld      a,80
    byte    $21                   ; LD HL, over LD A,
;-----------------------------------------------------------------------------
; Calculate 40 column bitmap coordinate address
;   Input: BC: Y-coordinate
;          DE: X-coordinate
; Output: A: BitMsk
;        DE: Coordinate Address
;        HL: Screen Address
; Clobbered: BC, HL
;-----------------------------------------------------------------------------
_bloxel40:
    ld      a,40                  ; A = LinLen
    push    de                    ; Stack = X-Coord, RtnAdr
    ld      e,a                   ; DE = LineLen
    ld      a,c                   ; A = Y-Coord
    ld      hl,SCREEN             
    add     hl,de                 ; HL = PxlAdr
.lineloop
    cp      3        
    jr      c,.xcoord             ; If A >= 3
    sub     a,3                   ;   A = A - 3
    add     hl,de                 ;  PxlAdr += LinLen
    jr      .lineloop
.xcoord
    rlca                          ; A = PxlOfs
    pop     de                    ; DE = X-Coord
    sra     e                     ; Column = X-Coord / 2
    jr      nc,.even              ; If Odd
    inc     a                     ;   Bump PxlOfs
.even
    add     hl,de                 ; PxlAdr += Column
    ld      de,BITTAB
.pxlloop
    or      a
    jr      z,.pxlmask
    inc     de
    dec     a
    jr      .pxlloop
.pxlmask
    ld      a,(hl)                ; A = ScrnChr
    or      $A0                   ; Convert to GfxChr
    xor     (hl)                  ; Clear bits 5 and 7
    ld      a,(de)                ; A = BitMsk
    ret

; 1bpp pixel subroutines
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

_resetpixelm:
    call    _calc_1bpp_addr           ; DE = BytAdr; BC = PxlOfs
    ld      hl,_andmask1bpp
    add     hl,bc                 ; HL = MskAdr
    ld      b,(hl)                ; B = BitMsk
    ld      a,(de)
    and     b
    ld      (de),a
    ret

_togglepixelm:
    call    _pixelm
    xor     b
    ld      (de),a
    ret

_setpixelm:
    ld      (STARTCOL),de
    ld      (STARTROW),bc
    call    _pixelm
    or      b
    ld      (de),a
    call    _psetcolor
    ret     m
    ld      de,(STARTCOL)
    ld      bc,(STARTROW)
    call    _calc_1bpp_cell       ; HL = Cell address
    ld      b,a
    ld      a,(hl)
    and     $0F
    or      b
    ld      (hl),a
    ret

_pixelm:
    call    _calc_1bpp_addr       ; DE = BytAdr; BC = PxlOfs
    ld      hl,_ormask1bpp
    add     hl,bc                 ; HL = MskAdr
    ld      b,(hl)                ; B = BitMsk
    ld      a,(de)
    ret

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
_calc_byte_addr:
    add     hl,de                 ; HL = BytAdr, B = 0
    ex      de,hl                 ; DE = BytAdr
    ex      af,af'
    ld      c,a                   ; BC = PxlOfs
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

; 4bpp pixel subroutines
_togglepixelc:
    ld      a,$FF
    jr      _pixelc
_resetpixelc:
    xor     a                     ; DrwClr = 0 (Background)
    jr      _pixelc
_setpixelc:
    ld      a,(PSETCOLOR)
    or      a
    call    m,bitmap_get_color
    and     $0F                   ; Force to 0 - 15
_pixelc:
    push    af                    ; Stack = Color, RtnAdr
    call    _calc_4bpp_addr       ; AF = NybOfs, DE = BytAdr; BC = PxlOfs
    pop     hl                    ; H = Color; Stack = RtnAdr
    ld      a,h                   ; A = Color (preserve flags)
    rla                           
    jr      nc,.notxor            ; If $FF
    push    bc
    call    .getmask              ;   B = Mask, C = $FF
    ld      a,b
    xor     $FF
    ld      b,a                   ;   B = Mask ^ $FF
    ld      a,(de)                ;   A = Byte
    xor     $FF                   ;   Flip Bits
    and     b                     ;   Mask it
    pop     bc
    jr      .noshift              ; Else
.notxor
    ld      a,h                   ;   A = Color
    jr      nz,.noshift           ;   If Odd X-coordinate
    rla                           ;   Shift color to high nybble
    rla
    rla
    rla
.noshift
    call    .getmask              ; B = Mask, C = Color
    ld      a,(de)
    and     b
    or      c
    ld      (de),a
    ret
.getmask
    ld      hl,_setmask
    add     hl,bc                 ;   HL = MskAdr
    ld      b,(hl)                ;   B = BitMsk
    ld      c,a
    ret

_setpixel_addr:
    ld      a,(GFX_FLAGS)
    and     GFXM_MASK             ; Mask bits and set flags
    ld      h,$FF                 ; Update LastX, Last Y
    ld      l,a                   ; L = GfxMode
    ld      ix,_setpixel40        ;
    ret     z                     ;   Do Bloxel 40
    ld      ix,_setpixel80         ; Bloxel80
    dec     a                      ; If GfxMode = 1
    ret     z                     ;   Do Bloxel 80
    ld      ix,_setpixelm         ;
    dec     a                     ; If GfxMode = 2
    ret     z                     ;   Do Bitmap 1bpp
    ld      ix,_setpixelc         ; Else fo Bitmap 4bpp
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
    jp      _calc_byte_addr

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
