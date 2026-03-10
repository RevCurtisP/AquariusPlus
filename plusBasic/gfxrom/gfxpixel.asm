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
;        BC: X-coordinate
;        DE: Y-coordinate
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
    push    bc                    ; Stack = X, OldPg, RtnAdr
    push    de                    ; Stack = Y, X, OldPg, RtnAdr
    push    hl                    ; Stack = UpdateXY, X, ColorY, OldPg, RtnAdr
    ld      a,VIDEO_RAM
    out     (IO_BANK1),a
    call    jump_ix
    ex      af,af'                ; A' = NewByte
    pop     hl                    ; H = UpdateXY; Stack = Y, X, OldPg, RtnAdr
    pop     de                    ; DE = Y; Stack = X, OldPg, RtnAdr
    pop     bc                    ; BC = X; Stack = OldPg, RtnAdr
    ld      a,h
    or      a
    jr      z,.done
    call    get_varbase           ; HL = VarAdr
    ld      a,BAS_BUFFR           ; Write sysvars
    out     (IO_BANK1),a
    inc     hl
    ld      (hl),e                ; Write Y
    inc     hl
    ld      (hl),c                ; Write X
    inc     hl
    ld      (hl),b
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

;new bloxel40
; BC = X-Coord, DE = Y-Coord
    ld      h,d                   ; HL = Y-Coord
    ld      l,e
    add     hl,hl
    add     hl,de                 ; HL = Index into Table (Y-Coord * 3)
    ld      de,_offsets40
    add     hl,de                 ; HL = Table Addr
    ld      e,(hl)
    inc     hl
    ld      d,(hl)                ; DE = LineAdr
    inc     hl
    ld      a,(hl)                ; A  = BITTAB offset
    ex      de,hl                 ; HL = LineAdr
    
    add     hl,bc


_bloxel80:
    ld      a,80
    byte    $21                   ; LD HL, over LD A,
;-----------------------------------------------------------------------------
; Calculate 40 column bitmap coordinate address
;   Input: BC: X-Coord (was Y-coordinate)
;          DE: Y-Coord (was X-coordinate)
; Output: A: Current Bloxel Char (0 if not a valid character)
;        DE: BITTAB Offset
;        HL: Screen Address
; Clobbered: BC, HL
;-----------------------------------------------------------------------------
_bloxel40:
    ld      a,40                  ; A = LinLen
    push    bc                    ; Stack = X-Coord, RtnAdr
    ld      b,0
    ld      c,a                   ; BC = LineLen
    ld      a,e                   ; A = Y-Coord
    ld      hl,SCREEN             
    add     hl,bc                 ; HL = PxlAdr
.lineloop
    cp      3        
    jr      c,.xcoord             ; If A >= 3
    sub     a,3                   ;   A = A - 3
    add     hl,bc                 ;  PxlAdr += LinLen
    jr      .lineloop
.xcoord
    rlca                          ; A = PxlOfs
    pop     bc                    ; BC = X-Coord
    srl     c                     ; Column = X-Coord / 2
    jr      nc,.even              ; If Odd
    inc     a                     ;   Bump PxlOfs
.even
    add     hl,bc                 ; PxlAdr += Column
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
    push    bc                    ; Stack = X, RtnAdr
    push    de                    ; Stack = Y, X, RtnAdr
    call    _pixelm               ; Draw the pixel
    or      b
    ld      (de),a
    call    _psetcolor
    pop     de                    ; DE = Y; Stack = X, RtnAdr
    pop     bc                    ; BC = X; Stack = RtnAdr
    ret     m
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
;  Input: BC: X-coordinate
;         DE: Y-coordinate
; Output: BC: Pixel Offset
;         DE: Coordinate Address
; Clobbered: HL
;-----------------------------------------------------------------------------
_calc_1bpp_addr:
    ld      a,c
    and     7                     ; A = X mod 8
    ex      af,af'                ; A' = PxlOfs
    srl     b                     ; E = X / 8
    rr      c                     ;
    srl     c
    srl     c                     ; E = BytOfs
    ld      b,high(BANK1_BASE)    ; DE = VidRAM + BytOfs
    call    _mult40e              ; HL = RowAdr
_calc_byte_addr:
    add     hl,bc                 ; HL = BytAdr
    ex      de,hl                 ; DE = BytAdr
    ex      af,af'
    ld      b,0
    ld      c,a                   ; BC = PxlOfs
    ret

;-----------------------------------------------------------------------------
; Calculate 1bpp bitmap color cell address
;  Input: BC: X-coordinate
;         DE: Y-coordinate
; Output: HL: Cell address
; Clobbered: BC, DE
;-----------------------------------------------------------------------------
_calc_1bpp_cell:
    sra     b
    rr      c
    srl     c
    srl     c
    ld      b,0                   ; DE = ColOfs
    srl     e
    srl     e
    srl     e                     ; C = Y/8
    call    _mult40e              ; HL = RowOfs
    add     hl,bc                 ; HL = BytOfs
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
;   Input: C: X-coordinate
;          E: Y-coordinate
; Output: BC: Pixel Offset
;         DE: Coordinate Address
; Clobbered: HL
;-----------------------------------------------------------------------------
_calc_4bpp_addr:
    ld      a,c
    and     1                     ; A = Nybble
    ex      af,af'                ; A' = PxlOfs
    srl     c                     ; E = BytOfs
    ld      b,high(BANK1_BASE)    ; BC = VidRAM + BytOfs
    call    _mult40e              ; HL = RowAdr
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

 ; HL = E * 40
; Clobbered: D
_mult40e:
    ld      d,0
    ld      h,d
    ld      l,e                   ; HL = X
    add     hl,hl                 ; HL = C * 2
    add     hl,hl                 ; HL = C * 4
    add     hl,de                 ; HL = C * 5
    add     hl,hl                 ; HL = C * 10
    add     hl,hl                 ; HL = C * 20
    add     hl,hl                 ; HL = C * 40
    ret


; Must preserve AF'
; A: GfxMode, BC: X, DE: Y
; Clobbers A, HL
_check_coords:
    rra                           ; Carry = TextMode
    rra
    jr      c,.bitmap             ; If bloxel
    rla                           ;   Carry = 40/80
    ld      hl,-72                ;   MaxY=72
    push    hl                    ;   Stack = MaxY, RtnAdr
    jr      c,.x160               ;   If 80, MaxX = 160 and check
    ld      hl,-80                ;   Else MaxX = 80 and check
    jr      .check_coords
.bitmap
    rla                           ;   Carry = 1bpp/4bpp
    ld      hl,-200               ;   MaxY = 199
    push    hl                    ;   Stack = MaxY, RtnAdr
    ld      hl,-320               ;
    jr      nc,.check_coords      ;   If 1bpp MaxX=320
.x160
    ld      hl,-160               ;   Else MaxX = 160
; BC = X, DE = Y, HL = MaxX; Stack = MaxY, RtnAdr
.check_coords:
    add     hl,bc                 ; Set Carry if X >= MaxX
    pop     hl                    ; HL = MaxY; Stack = RtnAdr
    ret     c                     ; Return carry set if X > MaxX
    add     hl,de                 ; Set Carry if Y >= MaxY
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

; Line Address LSB, MSB, ormask offset
; Bloxel Address = Line Address + Column / 2
; Bit Mask Offset = Line Offset + (Column & 1)
_offsets40:
    byte  $28,$30,$00, $50,$30,$02, $78,$30,$04, $A0,$30,$00, $C8,$30,$02, $F0,$30,$04
    byte  $18,$31,$00, $40,$31,$02, $68,$31,$04, $90,$31,$00, $B8,$31,$02, $E0,$31,$04
    byte  $08,$32,$00, $30,$32,$02, $58,$32,$04, $80,$32,$00, $A8,$32,$02, $D0,$32,$04
    byte  $F8,$32,$00, $20,$33,$02, $48,$33,$04, $70,$33,$00, $98,$33,$02, $C0,$33,$04
    byte  $E8,$33,$00, $10,$34,$02, $38,$34,$04, $60,$34,$00, $88,$34,$02, $B0,$34,$04
    byte  $D8,$34,$00, $00,$35,$02, $28,$35,$04, $50,$35,$00, $78,$35,$02, $A0,$35,$04
    byte  $C8,$35,$00, $F0,$35,$02, $18,$36,$04, $40,$36,$00, $68,$36,$02, $90,$36,$04
    byte  $B8,$36,$00, $E0,$36,$02, $08,$37,$04, $30,$37,$00, $58,$37,$02, $80,$37,$04
    byte  $A8,$37,$00, $D0,$37,$02, $F8,$37,$04, $20,$38,$00, $48,$38,$02, $70,$38,$04
    byte  $98,$38,$00, $C0,$38,$02, $E8,$38,$04, $10,$39,$00, $38,$39,$02, $60,$39,$04
    byte  $88,$39,$00, $B0,$39,$02, $D8,$39,$04, $00,$3A,$00, $28,$3A,$02, $50,$3A,$04
    byte  $78,$3A,$00, $A0,$3A,$02, $C8,$3A,$04, $F0,$3A,$00, $18,$3B,$02, $40,$3B,$04

_offsets80:
    byte    $50,$40,$00, $A0,$40,$02, $F0,$40,$04, $40,$41,$00, $90,$41,$02, $E0,$41,$04
    byte    $30,$42,$00, $80,$42,$02, $D0,$42,$04, $20,$43,$00, $70,$43,$02, $C0,$43,$04
    byte    $10,$44,$00, $60,$44,$02, $B0,$44,$04, $00,$45,$00, $50,$45,$02, $A0,$45,$04
    byte    $F0,$45,$00, $40,$46,$02, $90,$46,$04, $E0,$46,$00, $30,$47,$02, $80,$47,$04
    byte    $D0,$47,$00, $20,$48,$02, $70,$48,$04, $C0,$48,$00, $10,$49,$02, $60,$49,$04
    byte    $B0,$49,$00, $00,$4A,$02, $50,$4A,$04, $A0,$4A,$00, $F0,$4A,$02, $40,$4B,$04
    byte    $90,$4B,$00, $E0,$4B,$02, $30,$4C,$04, $80,$4C,$00, $D0,$4C,$02, $20,$4D,$04
    byte    $70,$4D,$00, $C0,$4D,$02, $10,$4E,$04, $60,$4E,$00, $B0,$4E,$02, $00,$4F,$04
    byte    $50,$4F,$00, $A0,$4F,$02, $F0,$4F,$04, $40,$50,$00, $90,$50,$02, $E0,$50,$04
    byte    $30,$51,$00, $80,$51,$02, $D0,$51,$04, $20,$52,$00, $70,$52,$02, $C0,$52,$04
    byte    $10,$53,$00, $60,$53,$02, $B0,$53,$04, $00,$54,$00, $50,$54,$02, $A0,$54,$04
    byte    $F0,$54,$00, $40,$55,$02, $90,$55,$04, $E0,$55,$00, $30,$56,$02, $80,$56,$04
