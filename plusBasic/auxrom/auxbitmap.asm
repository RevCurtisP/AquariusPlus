;====================================================================
; Bitmap Graphics Routines
;====================================================================

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
    and     a,VCRTL_80COL_EN      ;   If 80 column
    ld      a,1                   ;     A = 1
    jr      nz,.write     
    ld      a,c                   ;   A = VCTRL
    and     VCTRL_TEXT_PAGE 
    jr      z,.write              ;   If Screen 2
    ld      a,GFXM_TXPG           ;     A = 16
.write 
    or      b                     ; Include BufFlg
    ld      c,a                   ; C = New flags
    ld      a,(GFX_FLAGS)         
    and     GFXSETMSK             ; Clear old flags
    or      c                     ; Set new flags
    ld      (GFX_FLAGS),a
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
    ld      h,high(BANK1_BASE+GFXVBASE)
    ld      a,(GFX_FLAGS)
    and     GFXM_TXPG
    ld      l,a                   ; If Screen 1
    ret     nz                    ;   Return 16
    ld      a,(GFX_FLAGS)
    and     GFXM_MASK             ; A = BmpMode
    ld      l,a
    sla     l
    sla     l                     ; HL = SysVar Base (0-12)
    ret

;-----------------------------------------------------------------------------
; Draw rectangle of bloxels or pixels screen
; Input: A: Color/Option
;       BC: Start X-Coord
;       DE: Start Y-Coord
;      BC': End X-Coord
;      DE': End Y-Coord
; Clobbered: All
; Derived from code written by Mack Wharton
;-----------------------------------------------------------------------------
bitmap_frect:
    ld      (PSETCOLOR),a
    call    _check_rect
    push    de                    ; Stack = y0, RtnAdr
    exx                           ; BC = x1, DE = y1, BC' = x0, DE' = y0
    pop     hl                    ; HL = y0; Stack = RtnAdr
    ex      de,hl                 ; DE = y0, HL = y1
    rst     COMPAR                ; Set carry if HL < DE
    jr      nc,.skip              ; If y1 < y0
    ex      de,hl                 ; Swap y1 and y0
.skip
    push    hl                    ; Stack = y1, RtnAdr
    push    de                    ; Stack = y0, RtnAdr
    exx                           ; BC = x0, DE = Old_y0, BC' = x1, DE' = y0
    pop     de                    ; BC = x0, DE = y0, BC' = x1, DE' = y0
.loop
    call    _save_rect            ; BC = x1, DE = y1, BC' = x0, DE' = y0; Stack = x1, y0, x0, y0, y1, RtnAdr
    exx                           ; BC = x0, DE = y0, BC' = x1, DE' = y0
    call    _line
    call    _restore_rect         ; BC = x0, DE = y0, BC' = x1, DE' = y0; Stack = y1, RtnAdr
    pop     hl                    ; HL = y1; Stack = RtnAdr
    rst     COMPAR                ; If y0 = y1
    ret     z                     ;   Return
    push    hl                    ; Stack = y1, RtnAdr
    inc     de                    ; y0 += 1
    push    de                    ; Stack = y0, y1, RtnAdr
    exx
    pop     de
    exx                           ; BC = x0, DE = y0, BC' = x1, DE' = y0
    jr      .loop

;-----------------------------------------------------------------------------
; Draw rectangle of bloxels or pixels screen
; Input: A: Color/Option
;       BC: Start X-Coord
;       DE: Start Y-Coord
;      BC': End X-Coord
;      DE': End Y-Coord
; Clobbered: All
; Derived from code written by Mack Wharton
;-----------------------------------------------------------------------------
bitmap_rect:
    ld      (PSETCOLOR),a
    call    _check_rect
    ld      h,d
    ld      l,e                   ; HL = y0
    call    _save_rect            ; BC = x1, DE = y1, BC' = x0, DE' = y0; Stack = x1, y1, x0, y0, RtnAdr
    ex      de,hl                 ; DE = y0
    exx                           ; BC = x0, DE = y0, BC' = x1, DE' = y0
    call    _line                 ; Draw Top Line
    call    _restore_rect         ; BC = x0, DE = y0, BC' = x1, DE' = y1
    call    _save_rect            ; BC = x1, DE = y1, BC' = x0, DE' = y0; Stack = x1, y1, x0, y0, RtnAdr
    push    de                    ; Stack = y1, x1, y1, x0, y0
    exx                           ; BC = x0, DE = y0, BC' = x1, DE' = y1
    pop     de                    ; BC = x0, DE = y1, BC' = x1, DE' = y1
    call    _line                 ; Draw Bottom Line
    call    _restore_rect         ; BC = x0, DE = y0, BC' = x1, DE' = y1
    ld      h,b
    ld      l,c                   ; HL = x0
    call    _save_rect            ; BC = x1, DE = y1, BC' = x0, DE' = y0; Stack = x1, y1, x0, y0, RtnAdr
    ld      b,h
    ld      c,l                   ; BC = x0
    exx                           ; BC = x0, DE = y0, BC' = x0, DE' = y1
    call    _line                 ; Draw Top Line
    call    _restore_rect         ; BC = x0, DE = y0, BC' = x1, DE' = y1
    call    _save_rect            ; BC = x1, DE = y1, BC' = x0, DE' = y0; Stack = x1, y1, x0, y0, RtnAdr
    push    bc                    ; Stack = x1, x1, y1, x0, y0
    exx                           ; BC = x0, DE = y0, BC' = x1, DE' = y1
    pop     bc                    ; BC = x1, DE = y1, BC' = x1, DE' = y1
    call    _line                 ; Draw Bottom Line
    call    _restore_rect         ; BC = x0, DE = y0, BC' = x1, DE' = y1
    ret

_save_rect:
    pop     ix                    ; IX = RtnAdr; Stack = SavCrd
    push    de                    ; Stack = Y
    push    bc                    ; Stack = X, Y
    push    hl
    exx
    pop     hl
    push    de                    ; Stack = Y', X, Y
    push    bc                    ; Stack = X', Y', X, Y
    jp      (ix)                  ; Return
_restore_rect
    pop     ix                    ; IX = RtnAdr; Stack = SavCrd
    pop     bc                    ; BC = X'; Stack = Y', X, Y
    pop     de                    ; DE = Y'; Stack = X, Y
    exx                           ; BC' = X', DE' = Y'
    pop     bc                    ; BC = X, Stack = Y
    pop     de                    ; DE = Y
    jp      (ix)                  ; Return



;-----------------------------------------------------------------------------
; Draw line of bloxels or pixels screen
; Input: A: Color/Option
;       BC: Start X-Coord
;       DE: Start Y-Coord
;      BC': End X-Coord
;      DE': End Y-Coord
; Clobbered: All
; Derived from code written by Mack Wharton
;-----------------------------------------------------------------------------
bitmap_line:
    ld      (PSETCOLOR),a
    call    _check_rect
    ret     c
_line:
    ld      (BMP_X0),bc
    ld      (BMP_Y0),de
    exx
    ld      (BMP_X1),bc
    ld      (BMP_Y1),de
    call    _line_write_xy
    call    _line_addr            ; IY = SetPxlAdr
.setup
    call    (jump_iy)
    ld      bc,$8000
    xor     a                     ; Clear carry
;dx = abs(x1 - x0)
    ld      hl,(BMP_X1)
    ld      de,(BMP_X0)
    sbc     hl,de                 ; HL = x1 - x0
    call    c,neg_hl              ; If negative, make it positive
    push    hl                    ; Stack = dx, RtnAdr
    add     hl,bc                 ; Offset to $8000 for compares
    ld      (BMP_DX),hl
;dy = -abs(y1 - y0)
    or      a
    ld      hl,(BMP_Y1)
    ld      de,(BMP_Y0)
    sbc     hl,de
    call    nc,neg_hl
    push    hl                    ; Stack = dy, dx, RtnAdr
    add     hl,bc                 ; Offset to $8000 for compares
    ld      (BMP_DY),hl
;error = dx + dy
    pop     de                    ; DE = dy; Stack = dx, RtnAdr
    pop     hl                    ; HL = dx; Stack = RtnAdr
    add     hl,de
    ld      (BMP_ERROR),hl
;sx = x0 < x1 ? 1 : -1
    ld      hl,(BMP_X0)
    ld      de,(BMP_X1)
    rst     COMPAR                ; Set Carry if HL < DE
    ld      bc,$0001
    jr      c,.save_sx
    dec     bc
    dec     bc
.save_sx
    ld      (BMP_SX),bc
;sy = y0 < y1 ? 1 : -1
    ld      de,(BMP_Y1)
    ld      hl,(BMP_Y0)
    rst     COMPAR                ; Set Carry if HL < DE
    ld      bc,$0001
    jr      c,.save_sy
    dec     bc
    dec     bc
.save_sy
    ld      (BMP_SY),bc
;while true
.loop
; plot(x0, y0)
;    ld      a,(PSETCOLOR)
    ld      bc,(BMP_Y0)
    ld      de,(BMP_X0)
    ld      h,0
    call    _drawpixel
; e2 = 2 * error
    ld      hl,(BMP_ERROR)
    add     hl,hl                 ; HL = 2 * error
    ld      bc,$8000
    add     hl,bc
    ld      (BMP_E2),hl
; if e2 >= dy
    ld      de,(BMP_DY)
    rst     COMPAR                ; Carry Clear if HL >= DE
    jr      c,.skip1
;  if x0 == x1 break
    ld      hl,(BMP_X0)
    ld      de,(BMP_X1)
    rst     COMPAR
    ret     z
;  error = error + dy
    ld      hl,(BMP_ERROR)
    ld      de,(BMP_DY)
    add     hl,de
    ld      (BMP_ERROR),hl
;  x0 = x0 + sx
    ld      hl,(BMP_X0)
    ld      de,(BMP_SX)
    add     hl,de
    ld      (BMP_X0),hl
; end if
.skip1
; if dx >= e2  {e2 <= dx}
    ld      hl,(BMP_DX)
    ld      de,(BMP_E2)
    rst     COMPAR                ; Carry Clear if HL >= DE
    jr      c,.skip2
;  if y0 == y1 break
    ld      hl,(BMP_Y0)
    ld      de,(BMP_Y1)
    rst     COMPAR
    ret     z
;  error = error + dx
    ld      hl,(BMP_ERROR)
    ld      de,(BMP_DX)
    add     hl,de
    ld      (BMP_ERROR),hl
;  y0 = y0 + sy
    ld      hl,(BMP_Y0)
    ld      de,(BMP_SY)
    add     hl,de
    ld      (BMP_Y0),hl
; end if
.skip2
;end while
    jr      .loop

_line_addr:
    ld      a,(PSETCOLOR)
    ld      iy,_resetpixel_addr
    cp      PRESETK
    ret     z
    ld      iy,_togglepixel_addr
    cp      XORTK
    ret     z
    ld      iy,_setpixel_addr
    ret

;Input: BC = X1, DE = Y1
_line_write_xy:
    in      a,(IO_BANK1)
    push    af          
    call    _get_varbase          ; HL = VarAdr
    ld      a,BAS_BUFFR           ; Write sysvars
    out     (IO_BANK1),a
    inc     hl
    ld      (hl),e
    inc     hl
    ld      (hl),c
    inc     hl
    ld      (hl),b
    pop     af
    out     (IO_BANK1),a
    ret

abs_hl:
    ld      a,h
    and     $80
    ret p
neg_hl:
    ld      a,h
    cpl
    ld      h,a
    ld      a,l
    cpl   
    ld      l,a
    inc     hl
    ret
   
 
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
;; ToDo: Rewrite and move to GfxROM
;bitmap_move:
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
;    call    bitmap_setpixel       ; Set the pixel
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
    call    _get_varbase          ; HL = VarAdr
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

_setpixel_addr:
    ld      a,(GFX_FLAGS)
    and     GFXM_MASK             ; Mask bits and set flags
    ld      h,$FF                 ; Update LastX, Last Y
    ld      l,a                   ; L = GfxMode
    ld      ix,_setpixel40        ;
    ret     z                     ;   Do Bloxel 40
    ld      ix,_setpixel80         ; Bloxel80
    dec     a                     ; If GfxMode = 1
    ret     z                     ;   Do Bloxel 80
    ld      ix,_setpixelm         ;
    dec     a                     ; If GfxMode = 2
    ret     z                     ;   Do Bitmap 1bpp
    ld      ix,_setpixelc         ; Else fo Bitmap 4bpp
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

_togglepixelc:
    ld      a,$FF
    jr      _pixelc
_resetpixelc:
    xor     a                     ; DrwClr = 0 (Background)
    jr      _pixelc
_setpixelc:
    ld      a,(PSETCOLOR)
    or      a
    call    m,bitmap_read_color
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
    jr      _calc_byte_addr

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

; Must preserve AF'
; A: GfxMode, BC: X, DE: Y
; Clobbers A, HL
_check_coords2:
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
    add     hl,bc                 ; If HL >= MaxX
    ret     c                     ;   Return carry set
    cp      e                     ; If Y >= MaxY
    ret                           ;   Return carry set

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
    jp      nz,ret_carry_set
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
    
bitmap_put:
    ret
    
    
;------------------------------------------------------------------------------
; Draw Character on 1bpp Bitmap Screen
; Input: BC: Column (0-39)
;        DE: Line (0-199)
;        HL: Character Data Address
; Sets: Carry if coordinate out of range
; Clobbered: A, B, DE, HL
;------------------------------------------------------------------------------
bitmap_putchar:
    call    _char_bounds          ; If ColRow out of bounds
    ret     c                     ; Return Carry Set
    push    hl                    ; Stack = ChrAdr, RtnAdr
    call    _char_offset          ; HL = Offset
    pop     hl                    ; DE = ChrAdr; Stack RtnAdr
_put_char:    
    ld      a,VIDEO_RAM           ; A = Bank
    ld      b,8                   ; B = LinCnt
    ld      c,40                  ; C = LinLen
    jp      page_skip_write

; In: C=Column, E=Line
; Out: Carry set if out of bounds
_char_bounds:
    ld      a,39
    cp      c                     ; If EndCol > 39
    ret     c                     ;   Return Carry Set
    ld      a,19
    cp      e                     ; If EndRow > 191
    ret                           ;   Return Carry Set
    
; Input: BC = Column, DE = Line
; Output: HL = VRAM Offset
; Clobbered: DE
_char_offset:
    ex      de,hl                 ; HL = Row
    add     hl,hl                 ; HL = Row * 2
    add     hl,hl                 ; HL = Row * 4
    add     hl,hl                 ; HL = Row * 8
    ld      d,h
    ld      e,l                   ; DE = Row * 8
    add     hl,hl                 ; HL = Row * 16
    add     hl,hl                 ; HL = Row * 32
    add     hl,de                 ; HL = Row * 40
    add     hl,bc                 ; HL = Row * 40 + Col
_swap_de_hl:
    ex      de,hl                 ; DE = Row * 40 + Col
    ret

get_set_init:
    ld      hl,(BUFADR)           
    ld      (BUFPTR),hl           ; BufPtr = BufAdr
    ld      bc,(BUFLEN)
    add     hl,bc                 ; HL = BufEnd
    ld      (BUFLEN),hl           ; BUFLEN = BufEnd
    ld      a,(BMPMODE)             
    and     a,1                   ; A = 0 (1bpp), 1 (4bpp)
    or      2                     ; A = GfxMode
    ld      a,(GFX_FLAGS)
    and     GFXM_MASK             ; Mask bits and set flags
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

_check_rect:    
    ld      a,(GFX_FLAGS)
    and     GFXM_MASK             ; Mask bits and set flags
    ld      (BMPMODE),a           ; BMPMODE = GfxMode
    call    _check_coords2        ; Set Carry if out of range
    ret     c                     ; Return Carry Set if (X0,Y0) out of range
    exx
    ld      a,(BMPMODE)           ; A = GfxMode
    call    _check_coords2        ; Set Carry if out of range
    exx
    ret                           ; Return Carry Set if (X0,Y0) out of range
