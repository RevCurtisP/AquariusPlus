;====================================================================
; Graphics Bitmap Drawing Statements and Functions
;====================================================================
;; Routines being moved from auxrom to gfxrom

bitmap_init_screen:
    ld      a,2
    ld      (GFX_FLAGS),a
    xor     a
    ld      b,a                   ; Use default colors
    call    bitmap_clear_screen
    xor     a
    ld      (GFX_FLAGS),a
    ret

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
    call    get_varbase          ; HL = VarBase
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
; Set bitmap mode system variable from video control register
; Input: A: Use buffer: $FF = Yes, 0 = No
; Clobbered: A, B, C
;-----------------------------------------------------------------------------
bitmap__set_mode_nobuff:
    xor     a
bitmap__set_mode:
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
; Clear Bitmap
; Input: B: Screen colors ($00 = use default)
; Clobbered: AF,AF',BC,DE,HL
;-----------------------------------------------------------------------------
bitmap_clear_screen:
    call    get_varbase           ; HL = VarAdr
    in      a,(IO_BANK1)
    ex      af,af'
    ld      a,BAS_BUFFR           ; Write sysvars
    out     (IO_BANK1),a
    inc     hl
    ld      (hl),0
    inc     hl
    ld      (hl),0
    inc     hl
    ld      (hl),0
    ex      af,af'
    out     (IO_BANK1),a
    ld      a,(GFX_FLAGS)
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
    call    z,bitmap_get_color    ;   Get default colors
;-----------------------------------------------------------------------------
; Fill Bitmap Color RAM
; Input: B: Color Byte
; Clobbered: AF,AF',BC,DE,HL
;-----------------------------------------------------------------------------
bitmap_fill_color:
    ld      a,(GFX_FLAGS)
    and     GFXM_MASK
    ld      d,b
    ld      hl,$3400+40
    ld      bc,1000-40            ; BC = Count
    or      a
    jp      z,sys_fill_mem_d
    dec     a
    jr      z,.fill80
    ld      hl,BANK1_BASE+BMP_BASE+8192
    ld      bc,1000
    dec     a
    jr      z,_fill_video_ram
    scf
    ret
.fill80
    ld      hl,$3000+80
    ld      bc,2000-80
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
; Fill Bitmap with Byte
; Input: A: Mode (0: 40col, 1: 80col, 2: 1bpp, 3: 4bpp)
;        B: Byte
; Clobbered: AF,AF',BC,DE,HL
;-----------------------------------------------------------------------------
bitmap_fill_byte:
    ld      a,(GFX_FLAGS)
    and     GFXM_MASK             ; A = GfxMode
    ld      d,b                   ; D = FillByte
    ld      hl,$3000+40
    ld      bc,1000-40
    jp      z,sys_fill_mem_d
    ld      hl,$3000+80
    ld      bc,2000-80
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
; Read Bitmap Draw Color
; Output: A,B: Colors
;-----------------------------------------------------------------------------
bitmap_get_color:
    call    get_varbase           ; HL = VarBase
    in      a,(IO_BANK1)
    ex      af,af'                ; A' = OldPg
    ld      a,BAS_BUFFR
    out     (IO_BANK1),a
    ld      b,(hl)                ; A = Byte
    ex      af,af'                ; A = OldPg, A' = Byte
    out     (IO_BANK1),a          ; Restore Page
    ld      a,b                   ; A = Byte
    ret

;-----------------------------------------------------------------------------
; Write to bitmap draw color system variable
; Input: B: Color(s)
; Clobbered: A, HL
;-----------------------------------------------------------------------------
bitmap_set_color:
    call    get_varbase          ; HL = VarBase
    in      a,(IO_BANK1)
    ex      af,af'                ; A' = OldPg
    ld      a,BAS_BUFFR
    out     (IO_BANK1),a
    ld      (hl),b
    ex      af,af'                ; A = OldPg, A' = Byte
    out     (IO_BANK1),a          ; Restore Page
    ret

; In: A: GfxMode; Out: HL: VarBase
get_varbase:
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

; Input: BC = X1, DE = Y1, BC' = X2, DE' = Y2
; Output: BC' = Width, DE' = Height, HL = Width
; Returns Carry Set if out of bounds
; Must not alter A
bitmap_rect_size:
    call    bitmap_check_rect     ; If coords out of bounds
    ret     c                     ;   Return Carry Set
    exx                           ; BC = X2, DE = Y2
    push    bc                    ; Stack = X2, RtnAdr
    push    de                    ; Stack = Y2, X2, RtnAdr
    exx                           ; BC = X1, DE = Y1
    pop     hl                    ; HL = Y2; Stack = X2, RtnAdr
    sbc     hl,de                 ; If Y2 - Y1 < 0
    jp      c,POPHRT              ;   Discard X2 and Return
    inc     hl                    ; HL = Height
    ex      (sp),hl               ; HL = X2; Stack = Height, RtnAdr
    sbc     hl,bc                 ; If X2 - X1 < 0
    jp      c,POPHRT              ;   Discard Width and Return
    inc     hl                    ; HL = Width
    push    hl                    ; Stack = Width, Height, RtnAdr
    exx                           ; BC' = X1, DE' = Y1
    pop     bc                    ; BC = Width; Stack = Height, RtnAdr
    pop     de                    ; DE = Height; Stack = RtnAdr
    exx                           ; BC = X1, DE = Y1, BC' = Width, DE' = Height
    ret


;-----------------------------------------------------------------------------
; Validate Bitmap Rectangle Coordinates
; Input: A = GfxMode
; BC = X1, DE = Y1, BC' = X2, DE' = Y2
; Returns Carry set if out of bounds
; Clobbers: A, HL
bitmap__check_rect:    
    exx
    call    bitmap__check_coords
    exx
    ret     c                     ; Return Carry Set if (X0,Y0) out of range
;-----------------------------------------------------------------------------
; Validate Bitmap Rectangle Coordinates
; Input: A = GfxMode
;       BC = X
;       DE = Y
; Flags: Carry Set if out of bounds
; Clobbered: HL
;-----------------------------------------------------------------------------
bitmap__check_coords:
    cp    2
    jr    nc,.bitmap              ; If GfxMode < 2
    call  .bloxelx                ;   HL = -BmpWid
    add   hl,bc                   ;   If X >= BmpWid
    ret   c                       ;     Return Carry Set
    ld    hl,-72                  ;   If Y >= BmpHgt
    add   hl,de                   ;     Return Carry Sets
    ret                           ; Else
.bitmap    
    call  .pixelx                 ;   HL = -BmpWid
    add   hl,bc                   ;   If X >= BmpWid
    ret   c                       ;     Return Carry Set
    ld    hl,-200                 ;   If Y >= BmpHgt
    add   hl,de                   ;     Return Carry Sets
    ret
.bloxelx:
    ld    hl,-80                  ; If GfxMode = 0
    or    a                       ;   Return BmpWid = -80
    ret   z                       ; Else
    ld    hl,-160                 ;   Return BmpWid = -160
    ret
.pixelx:
    ld    hl,-320                 ; If GfxMode = 2
    cp    2                       ;   Return BmpWid = -320
    ret   z                       ; Else
    ld    hl,-160                 ;   Return BmpWid = -160
    ret

; Input: BC = X1, DE = Y1, BC' = X2, DE' = Y2
; Returns Carry set if out of bounds
; Clobbers: A, HL
bitmap_check_rect:    
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

