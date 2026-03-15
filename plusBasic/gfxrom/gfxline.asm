
; SCREEN 0,2:CALL @63,$8DF ARGS 60,50,20:PAUSE


test_hline:
    push    hl
    exx
    pop     bc
    exx
;-----------------------------------------------------------------------------
; Draw horizontal line of bloxels or pixels
; Input: A: Color/Option
;       BC: Start X-Coord
;       DE: Y-Coord
;      BC': End X-Coord
; Clobbered: All
; Derived from code written by Mack Wharton
;-----------------------------------------------------------------------------
bitmap_hline:
    ld      (PSETCOLOR),a
    push    de                    ; Stack = Y1, RtnAdr
    exx
    pop     de                    ; Y2 = Y1; Stack = RtnAdr
    exx
    call    bitmap_check_rect     ; If coords out of bounds
    ret     c                     ;   Return Carry Set
    exx
    push    bc                    ; Stack = X2, RtnAdr
    exx     
    pop     hl                    ; HL = X2; Stack = RtnAdr
    sbc     hl,bc                 ; HL = Width
    ret     c
    push    hl                    ; Stack = Width, RtnAdr

    ld      a,(BMPMODE)
    or      a
    jr      z,_hline40
    dec     a
    jr      z,_hline80
    dec     a
    jr      z,_hline1bpp
    jr      _hline4bpp
    
_hline40:
    ret

_hline80:
    ret


_hline1bpp:
    ld      a,VIDEO_RAM
    out     (IO_BANK1),a
    call    pixel_1bpp_addr     ; A = BitMsk, DE = BytAdr
    ex      de,hl               ; HL = BytAdr
    ld      d,a                 ; D = BitMsk
    pop     bc                  ; BC = Width; Stack = RtnAdr
    in      a,(IO_BANK1)
    push    af
.orbytes
    ld      e,(hl)              ; A = ScrByt
.orbits:
    ld      a,b
    or      c
    jr      z,.done
    dec     bc
    ld      a,e                 ; A = ScrByt
    or      d                   ; A = NewByt
    ld      e,a
    rr      d                   ; Shift Bitmask
;    jr      nc,.bitloop         ; Do next bit
    rr      d
    ld      (hl),e              ; Store Byte
    inc     hl                  ; Next Byte
 ;   jr      .byteloop
.done
    pop     af
    out     (IO_BANK1),a
    ret

    
.and_pxl

_hline4bpp:
    ret


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
bitmap__line:
    ld      (PSETCOLOR),a
    call    bitmap_check_rect
    ret     c
_line:
    ld      (BMP_X0),bc
    ld      (BMP_Y0),de
    exx
    ld      (BMP_X1),bc
    ld      (BMP_Y1),de
    call    _line_write_xy
    call    _line_addr            ; IY = SetPxlAdr
    call    _line_setup
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

_line_setup:
.setup
    call    (jump_iy)
    ld      bc,$8000
    xor     a                     ; Clear carry
;dx = abs(x1 - x0)
    ld      hl,(BMP_X1)
    ld      de,(BMP_X0)
    sbc     hl,de                 ; HL = x1 - x0
    call    c,_neg_hl             ; If negative, make it positive
    push    hl                    ; Stack = dx, RtnAdr
    add     hl,bc                 ; Offset to $8000 for compares
    ld      (BMP_DX),hl
;dy = -abs(y1 - y0)
    or      a
    ld      hl,(BMP_Y1)
    ld      de,(BMP_Y0)
    sbc     hl,de
    call    nc,_neg_hl
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
    ret

_line_addr:
;    ld      a,(PSETCOLOR)
;    ld      iy,_resetpixel_addr
;    cp      PRESETK
;    ret     z
;    ld      iy,_togglepixel_addr
;    cp      XORTK
;    ret     z
;    ld      iy,_setpixel_addr
;    ret

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
bitmap__rect:
    ld      (PSETCOLOR),a
    call    bitmap_check_rect
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
    pop     af                    ; A = OldPg; Stack = RtnAdr
    out     (IO_BANK1),a
    ex      af,af'                ; A = NewByte
    ret

;Input: BC = X1, DE = Y1
_line_write_xy:
    in      a,(IO_BANK1)
    push    af          
    call    get_varbase           ; HL = VarAdr
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

_abs_hl:
    ld      a,h
    and     $80
    ret p
_neg_hl:
    ld      a,h
    cpl
    ld      h,a
    ld      a,l
    cpl   
    ld      l,a
    inc     hl
    ret
