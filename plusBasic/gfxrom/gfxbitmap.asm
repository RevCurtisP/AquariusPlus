;====================================================================
; Graphics Bitmap Drawing Statements and Functions
;====================================================================
;; Routines being moved from auxrom to gfxrom

;-----------------------------------------------------------------------------
; Initialize bitmap sysvars
;-----------------------------------------------------------------------------
; Ofs Desription
;  0  Screen 1 Bloxel Mode
;  4  Screen 3 Bloxel Mode
;  8  1bpp Pixel Mode
; 12  4bpp Pixel Mode
; 16  Screen 2 Bloxel Mode
; +0  Default Colors
; +1  Last Y-Coord
; +2  Last X-Coord

_bmp_defaults:
    byte    $06,0,0,0,$06,0,0,0,$70,0,0,0,$07,0,0,0,$06,0,0,0
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
