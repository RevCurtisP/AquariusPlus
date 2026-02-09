;====================================================================
; Graphics Bitmap Drawing Statements and Functions
;====================================================================


;-----------------------------------------------------------------------------
; CLEAR BITMAP [foreground, background]
;-----------------------------------------------------------------------------
; SCREEN 1:CLEAR BITMAP
; SCREEN 1:CLEAR BITMAP 7,0
; SCREEN 3:CLEAR BITMAP 0,7
; SCREEN 0,2:CLEAR BITMAP:PAUSE
; SCREEN 0,2:CLEAR BITMAP 7,1:PAUSE
; SCREEN 0,3:CLEAR BITMAP:PAUSE
ST_CLEAR_BITMAP:
    rst     CHRGET                ; Skip BIT
    SYNCHKT MAPTK
    ld      b,0
    call    nz,_color_args
    ld      iy,bitmap_clear_screen
    jp      gfx_call_preserve_hl

;-----------------------------------------------------------------------------
; FILL BITMAP [BYTE byte] [COLOR foreground,background]
;-----------------------------------------------------------------------------
; To Do: FILL COLORMAP (x,y)-(x,y),fg,bg
; SCREEN 1,0:FILL BITMAP BYTES $EE:PAUSE
; SCREEN 1,0:FILL BITMAP BYTES $AA COLOR 7,0:PAUSE
; SCREEN 1,0:FILL BITMAP COLOR 2,8:PAUSE
; SCREEN 3,0:FILL BITMAP BYTES $AA COLOR 6,1:PAUSE
; SCREEN 0,2:FILL BITMAP BYTES $AB COLOR 6,1:PAUSE
; SCREEN 0,3:FILL BITMAP BYTES $AE:PAUSE
; SCREEN 0,3:FILL BITMAP BYTES $AA COLOR 6,1:PAUSE
ST_FILL_BITMAP:
    rst     CHRGET                ; Skip BIT
    SYNCHKT MAPTK
    cp      XTOKEN
    jr      nz,.fill_color        ; If Extended Token
    rst     CHRGET                ;   Skip it
    SYNCHKT BYTETK                ;   Require BYTES
    SYNCHKC 'S'
    call    GETBYT                ;   A = FillByte
    ld      b,a                   ;   B = FillByte
    push    hl                    ;   Stack = TxtPtr, RtnAdr
    ld      iy,bitmap_fill_byte
    call    gfx_call              ;   Do the fill
    pop     hl                    ;   HL = TxtPtr; Stack = RtnAdr
    call    CHRGT2                ;   Reget next character
    ret     z                     ;   Return if end of statement
.fill_color
    call    parse_colors          ; Require COLOR, A = Color Byte
_fill_color:
    ld      b,a
    call    get_bitmap_mode
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ld      iy,bitmap_fill_color
    call    gfx_call              ; Do the fill
    jp      c,TOERR
    pop     hl                    ; HL = TxtPtr; Stack = NxtChr, RtnAdr
    ret

;-----------------------------------------------------------------------------
; COLOR fgcolor, bgcolor
; COLOR color
; Set default oolor(s)
;-----------------------------------------------------------------------------
; SCREEN 1,0:COLOR 7,0:PRINT HEX$(COLOR)
; SCREEN 3,0:COLOR 0,7:PRINT HEX$(COLOR)
; SCREEN 0,2:COLOR 6,1:PRINT HEX$(COLOR)
; SCREEN 0,3:COLOR 5:PRINT HEX$(COLOR)
ST_COLOR:
    SYNCHKT ORTK                  ; Require OR
    call    _color_args
    ld      iy,bitmap_write_color
    call    gfx_call_preserve_hl
    jp      no_more               ; Error if any more operands

; Output: B: Color(s)
_color_args:
    ld      a,(GFX_FLAGS)
    and     GFXM_MASK             ; A = BmpMode
    cp      3
    jr      z,.color4bpp          ; If not 4bpp
    call    get_screen_colors     ;   A = fgcolor + bgcolor
    jr      .done                 ; Else
.color4bpp
    call    get_byte16            ;   A = color
.done
    ld      b,a
    ld      a,(hl)
    cp      ','                   ; If NxtChr is comma
    ret     nz
    jp      TOERR                 ;   Too many arguments error

;-----------------------------------------------------------------------------
; COLOR
; Return default oolor(s)
;-----------------------------------------------------------------------------
; SCREEN 0,2:PRINT COLOR
FN_COLOR:
    rst     CHRGET                ; Skip COL
    SYNCHKT ORTK                  ; Require OR
    ld      iy,bitmap_read_color
gfx_call_sngflt:
    call    push_hl_labbck        ; Stack = LABBCK, TxtPtr, RtnAdr
    call    gfx_call              ; A = Color
    jp      SNGFLT


;----------------------------------------------------------------------------
; LINE Statement
; LINE (x0,y0)-(x1,y1){,color|XOR}
;----------------------------------------------------------------------------
ST_LINE:
    cp      INPUTK
    jp      z,ST_LINE_INPUT
    call    SCAND                 ; BC = x0, DE = y0
    push    de                    ; Stack = y0, RtnAdr
    push    bc                    ; Stack = x0, y0, RtnAdr
    SYNCHKT MINUTK                ; Require -
    call    SCAND                 ; BC = x0, DE = y0
    push    de                    ; Stack = y1, x0, y0, RtnAdr
    push    bc                    ; Stack = x1, y1, x0, y0, RtnAdr
    call    _pset_opt             ; A = Color, $FF, or XORTK
    ld      (PSETCOLOR),a
    pop     bc                    ; BC = x1; Stack = y1, x0, y0, RtnAdr
    ld      (BMP_X1),bc
    pop     de                    ; DE = y1; Stack = x0, y0, RtnAdr
    ld      (BMP_Y1),de
    pop     bc                    ; BC = x0; Stack = y0, RtnAdr
    ld      (BMP_X0),bc
    pop     de                    ; DE = y0; Stack = RtnAdr
    ld      (BMP_Y0),de

    ld      iy,bitmap_line
gfx_call_fcerr:
    push    hl
    jp      gfx_call_fc_popret



;-----------------------------------------------------------------------------
; POSX, POSY
; Last X,Y position
;-----------------------------------------------------------------------------
; SCREEN 0,2:PSET (10,23):PRINT POSX;POSY
FN_POS:
    inc     hl
    ld      a,(hl)
    cp      '('
    jp      z,ABORT_FN
    ex      af,af'
    rst     CHRGET                ; Skip SfxChr
    call    push_hl_labbck        ; Stack = LABBCK, TxtPtr, RtnAdr
    ex      af,af'
    push    af                    ; Stack = SfxChr, LABBCK, TxtPtr, RtnAdr
    ld      iy,bitmap_read_sysvars
    call    gfx_call              ; B = BmpClr, C = BmpY, DE = BmpX
    pop     af                    ; A = SfxChr; Stack = LABBCK, TxtPtr, RtnAdr
    cp      'X'                   ; If POSX
    jp      z,FLOAT_DE            ;   Return X
    cp      'Y'
    jp      z,FLOAT_C
    jp      SNERR

;-----------------------------------------------------------------------------
; POINT(x,y)
;-----------------------------------------------------------------------------
; SCREEN 0,2:PSET(1,1):PRINT POINT(1,1):PAUSE
; SCREEN 0,2:PRESET(1,1):PRINT POINT(1,1):PAUSE

; SCREEN 0,2:PSET(299,55):PRINT POINT(299,55):PAUSE
; SCREEN 0,2:PRESET(299,55):PRINT POINT(299,55):PAUSE

; SCREEN 0,3:PSET(1,1),7:PRINT POINT(1,1):PAUSE
; SCREEN 0,3:PSET(1,1),2:PRINT POINT(1,1):PAUSE
; SCREEN 0,3:PRESET(1,1):PRINT POINT(1,1):PAUSE

; SCREEN 0,3:PSET(159,66),9:PRINT POINT(159,66):PAUSE
; SCREEN 0,3:PRESET(159,66):PRINT POINT(159,66):PAUSE


; SCREEN 0,2:? POINT (299,55)
FN_POINT:
    rst     CHRGET                ; Skip POINT
_point:
    ld      de,bitmap_getpixel
    call    _do_pixel_no_color
    jp      push_hl_labbck_sngflt

;-----------------------------------------------------------------------------
; Bloxel PRESET and PRESET with the EX AF,AF' factored out
;-----------------------------------------------------------------------------
; SCREEN 0,2:CLEAR BITMAP 7,0:PSET (2,2):PAUSE
; SCREEN 0,2:PSET (4,4),2,7:PAUSE
; SCREEN 0,2:PSET (160,0):PAUSE
; SCREEN 0,2:PSET (160,0),0,3:PAUSE
; SCREEN 0,3:CLEAR BITMAP:COLOR 1:PSET (5,5):PAUSE
; SCREEN 0,3:PSET (159,0),2:PAUSE
; SCREEN 0,3:PSET (159,0),0:PAUSE
ST_PSET:
    ld      de,bitmap_setpixel
    or      $FF
    jr      _do_pixel

; SCREEN 0,2:PRESET (160,0)
ST_PRESET:
    ld      de,bitmap_resetpixel
_do_pixel_no_color:
    xor     a
_do_pixel:
    push    de                    ; Stack = SubAdr, RtnAdr
    push    af
    call    paren_coords          ; C = Y, DE = X
    pop     af
    call    nz,_pset_opt          ; A = Color
iy_aux_call_fcerr
    pop     iy                    ; IY = SubAdr; Stack = RtnAdr
    cp      XORTK
    jr      nz,aux_call_fcerr
    ld      iy,bitmap_togglepixel
aux_call_fcerr:
    push    hl                    ; Stack = TxtPtr, RtnAdr
    call    aux_call
    jp      c,FCERR               ; Error if illegal coordinate
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

; Return optional color arg in A
_pset_opt:
    ld      a,(hl)
    cp      ','                   ; If no comma
    ld      a,$FF                 ;   Return -1 (none)
    ret     nz
    rst     CHRGET                ; Skip Comma
    cp      XORTK
    jr      z,skip_char
    cp      PRESETK
    jr      z,skip_char
    push    bc                    ; Stack = ModeY, RtnAdr
    push    de                    ; Stack = X, ModeY, RtnAdr
    call    get_byte16
    pop     de                    ; DE = X; Stack = Y, RtnAdr
    pop     bc                    ; B = Mode, C = Y; Stack = RtnAdr
    ret
skip_char:
    push    af
    rst     CHRGET
    pop     af
    ret


;-----------------------------------------------------------------------------
; Return current bitmap mode
; Output: A: Mode (0: 40col, 1: 80col, 2: 1bpp, 3: 4bpp)
;  Flags: Carry set if text screen, clear if bitmap screen
; Clobbered: C
;-----------------------------------------------------------------------------
get_bitmap_mode:
    in      a,(IO_VCTRL)
    ld      c,a
    rra                           ; A = 2: 1bpp, 3: 4bpp
    and     GFXM_MASK             ; Isolate bits
    cp      2                     ; If 1bpp or 2 bpp
    ret     nc                    ;   Return
    ld      a,c
    and     a,VCRTL_80COL_EN      ; A = 0: 40col, 1: 80col
    scf
    ret     z                     ; If 40 col return 0
    ld      a,1                   ; Else Return 1
    ret

;-----------------------------------------------------------------------------
; Move Bitmap Cursor
; MOVE{B|C) {ABS} (x,y)
;-----------------------------------------------------------------------------
ST_MOVE: