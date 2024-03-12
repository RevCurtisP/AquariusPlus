;====================================================================
; Bitmap Graphics Routines
;====================================================================



bitmapc_read_sysvars:
    ld      hl,BANK1_BASE+GFXVBASE+BMPC_DRAWCOLOR
    jr      _read_sysvars
;-----------------------------------------------------------------------------
; Read bitmap system variables
; Output: A: Colors
;        BC: Y-coordinate
;        DE: X-coordinate
; Clobbered: HL
;-----------------------------------------------------------------------------
bitmap_read_sysvars:
    ld      hl,BANK1_BASE+GFXVBASE+BMP_DRAWCOLOR
_read_sysvars:
    in      a,(IO_BANK1)
    ex      af,af'                ; AF' = OldPg
    ld      a,BAS_BUFFR
    out     (IO_BANK1),a          ; Map Basic Buffers
    ld      a,(hl)                ; A = BMP_DRAWCOLOR
    inc     hl
    ld      c,(hl)                
    inc     hl
    ld      b,(hl)                ; BC = BMP_LASTY
    inc     hl
    ld      e,(hl)                
    inc     hl
    ld      d,(hl)                ; DE = BMP_LASTX
    ex      af,af'                ; A = OldPg, af' = BMP_DRAWCOLOR
    out     (IO_BANK1),a          ; Map Original Page
    ex      af,af'                ; A = BMP_DRAWCOLOR
    ret

; Input: IX: Address
; Output: A: Var byte
_read_bytevar:
    in      a,(IO_BANK1)
    ex      af,af'                ; A' = OldPg
    ld      a,BAS_BUFFR
    out     (IO_BANK1),a          
    ld      a,(IX)                ; A = Byte
    ex      af,af'                ; A = OldPg, A' = Byte
    out     (IO_BANK1),a          ; Restore Page
    ex      af,af'                ; A = Byte
    ret

;-----------------------------------------------------------------------------
; Write to bitmap draw color system variable
; Input: A: Colors
; Clobbered: A, DE
;-----------------------------------------------------------------------------
bitmapc_write_color:
    ld      de,GFXVBASE+BMPC_DRAWCOLOR
    jr      _write_color
bitmap_write_color:
    ld      de,GFXVBASE+BMP_DRAWCOLOR
_write_color:
    ld      c,a
    jp      basbuf_write_byte
    
;-----------------------------------------------------------------------------
; Write to bitmap X-coordinate system variable
; Input: DE: Colors
; Clobbered: A, BC, DE
;-----------------------------------------------------------------------------
bitmapc_write_xpos:
    push    de
    ld      de,GFXVBASE+BMPC_LASTX
    jr      _write_xpos
bitmap_write_xpos:
    push    de
    ld      de,GFXVBASE+BMP_LASTX
_write_xpos:
    pop     bc
    jp      basbuf_write_word
    
;-----------------------------------------------------------------------------
; Write to bitmap X-coordinate system variable
; Input: BC: Colors
; Clobbered: A, DE
;-----------------------------------------------------------------------------
bitmapc_write_ypos:
    ld      de,GFXVBASE+BMPC_LASTY
    jr      _write_ypos
bitmap_write_ypos:
    ld      de,GFXVBASE+BMP_LASTY
_write_ypos:
    jp      basbuf_write_word

;-----------------------------------------------------------------------------
; Clear Bitmap
; Input: B: 0=1bpp, 1=4bpp 
; Clobbered: AF,AF',BC,DE,HL
;-----------------------------------------------------------------------------
bitmap_clear:
    xor     a                     ; A = FillByte
    jr      bitmapc_fill_byte
bitmapc_clear:
    xor     a
;-----------------------------------------------------------------------------
; Fill Bitmap with Byte
; Input: A: Byte
; Clobbered: AF,AF',BC,DE,HL
;-----------------------------------------------------------------------------
bitmapc_fill_byte:
    ld      bc,16000
    jr      _fill_bitmap
bitmap_fill_byte:
    ld      bc,8000               ;   BC = Count
_fill_bitmap:
    ld      hl,BANK1_BASE+BMP_BASE
_fill_video_ram:
    ld      e,a                   ; L = FillByte
    in      a,(IO_BANK1)          ; A = OrigPg
    ex      af,af'                ; A' = OrigPg
    ld      a,VIDEO_RAM           ; A = NewPg
    out     (IO_BANK1),a          ; Map video RAM
    ld      a,e                   ; A = FillByte
    call    sys_fill_mem          ; Do the fill
    ex      af,af'                ; A = OrigPg
    out     (IO_BANK1),a          ; Restore original page
    ret
bitmap_fill_color:
    ld      bc,2000               ; BC = Count
    ld      hl,BANK1_BASE+BMP_COLORRAM
    jr      _fill_video_ram

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
; Return pixel from 4bpp bitmap screen
;  Input: C: Y-coordinate
;        DE: X-coordinate
;    Output: A = 1 if set, 0 if reset
;      Sets: Csrry if coordinates out of range
; Clobbered: BC, DE, HL
;-----------------------------------------------------------------------------
bitmapc_getpixel:
    ld      ix,_getpixelc
    call    _bitmapc
    ex      af,af'
    or      a
    ret

;-----------------------------------------------------------------------------
; Return pixel from 1bpp bitmap screen
;  Input: C: Y-coordinate
;        DE: X-coordinate
;    Output: AF': Byte containing isolated pixel
;      Sets: C if coordinates out of range
;            NZ if set. Z if reset
; Clobbered: BC, DE, HL
;-----------------------------------------------------------------------------
bitmap_getpixel:
    ld      ix,_getpixel
    call    _dopixel
    ex      af,af'
    ret     z                     ; A = 1: C clear, Z set
    xor     a
    inc     a                     ; A = 1: C clear, Z clear
    ret

;-----------------------------------------------------------------------------
; Erase pixel on 4bpp bitmap screen
;  Input: C: Y-coordinate
;        DE: X-coordinate
;    Output: AF': New byte containing pixel
;      Sets: Csrry if coordinates out of range
; Clobbered: A, BC, DE, HL
;-----------------------------------------------------------------------------
bitmapc_resetpixel:   
    ld      ix,_resetpixelc
    jr      _bitmapc

;-----------------------------------------------------------------------------
; Erase pixel on 1 bpp bitmap screen
;  Input: C: Y-coordinate
;        DE: X-coordinate
;    Output: AF': New byte containing pixel
;      Sets: Csrry if coordinates out of range
; Clobbered: A, BC, DE, HL
;-----------------------------------------------------------------------------
bitmap_resetpixel:   
    ld      ix,_resetpixel
    jr      _dopixel

;-----------------------------------------------------------------------------
; Draw pixel on 4bpp bitmap screen
;  Input: A: Color
;         C: Y-coordinate
;        DE: X-coordinate
;    Output: AF': New byte containing pixel
;      Sets: Carry if coordinates out of range
; Clobbered: A, BC, DE, HL
;-----------------------------------------------------------------------------
bitmapc_setpixel:   
    or      a
    ld      ix,BANK1_BASE+GFXVBASE+BMPC_DRAWCOLOR
    call    m,_read_bytevar
.useit
    ex      af,af'
    ld      ix,_setpixelc
_bitmapc:
    call    _check_coords_c
    ld      hl,BANK1_BASE+GFXVBASE+BMPC_LASTY
    jr      _dopixelc

;-----------------------------------------------------------------------------
; Draw pixel on 1bpp bitmap screen
;  Input: C: Y-coordinate
;        DE: X-coordinate
;    Output: AF': New byte containing pixel
;      Sets: Csrry if coordinates out of range
; Clobbered: A, BC, DE, HL
;-----------------------------------------------------------------------------
bitmap_setpixel:   
    ld      ix,_setpixel
_dopixel:
    call    _check_coords_b
    ld      hl,BANK1_BASE+GFXVBASE+BMP_LASTY
_dopixelc:
    ret     c
    in      a,(IO_BANK1)
    push    af                    ; Stack = OldPg, RtnAdr
    push    bc                    ; Stack = Y, OldPg, RtnAdr
    push    de                    ; Stack = X, Y, OldPg, RtnAdr
    push    hl                    ; Stack = VarAdr, X, Y, OldPg, RtnAdr
    ld      a,VIDEO_RAM
    out     (IO_BANK1),a
    ex      af,af'                ; Restore color for bitmapc_setpixel
    call    jump_ix
    ex      af,af'                
    pop     hl                    ; HL = VarAdr; Stack = X, Y, OldPg, RtnAdr
    pop     de                    ; DE = X; Stack = Y, OldPg, RtnAdr
    pop     bc                    ; BC = Y; Stack = OldPg, RtnAdr
    ld      a,BAS_BUFFR           ;   Write sysvars
    out     (IO_BANK1),a
    ld      (hl),c
    inc     hl
    ld      (hl),b
    inc     hl
    ld      (hl),e
    inc     hl
    ld      (hl),d
    pop     af                    ; A = OldPg; Stack = RtnAdr
    out     (IO_BANK1),a
    ret

_getpixel:
    call    bitmap_addr           ; DE = BytAdr; BC = PxlOfs
    ld      hl,_ormask1bpp
    add     hl,bc                 ; HL = MskAdr
    ld      b,(hl)                ; B = BitMsk
    ld      a,(de)
    and     b
    ret

_resetpixel:
    call    bitmap_addr           ; DE = BytAdr; BC = PxlOfs
    ld      hl,_andmask1bpp
    add     hl,bc                 ; HL = MskAdr
    ld      b,(hl)                ; B = BitMsk
    ld      a,(de)
    and     b
    ld      (de),a
    ret

; C: Y-coordinate, DE: X-coordinate
_setpixel:
    call    bitmap_addr           ; DE = BytAdr; BC = PxlOfs
    ld      hl,_ormask1bpp
    add     hl,bc                 ; HL = MskAdr
    ld      b,(hl)                ; B = BitMsk
    ld      a,(de)
    or      b
    ld      (de),a
    ret

_getpixelc:
    call    bitmapc_addr;         ; AF = NybOfs, DE = BytAdr; BC = PxlOfs
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

_resetpixelc:
    call    bitmapc_addr;         ; AF = NybOfs, DE = BytAdr; BC = PxlOfs
    ld      hl,_setmask
    add     hl,bc                 ; HL = MskAdr
    ld      b,(hl)                ; B = BitMsk
    ld      a,(de)
    and     b
    ld      (de),a
    ret

_setpixelc:
    and     $0F                   ; Force color to 0 - 15
    push    af                    ; Stack = Color, RtnAdr
    call    bitmapc_addr;         ; AF = NybOfs, DE = BytAdr; BC = PxlOfs
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
; Calculate 1bpp bitmap coordinate address
;   Input: C: Y-coordinate
;          E: X-coordinate
; Output: BC: Pixel Offset
;         DE: Coordinate Address
; Clobbered: HL
;-----------------------------------------------------------------------------
bitmapc_addr:   
    ld      a,e                   
    and     1                     ; A = Nybble
    ex      af,af'                ; A' = PxlOfs
    srl     e                     ; E = BytOfs
    ld      d,high(BANK1_BASE)    ; DE = VidRAM + BytOfs
    call    _mult40c              ; HL = RowAdr
    add     hl,hl                 ; HL = Y * 80
    jr      _bitmap_addr

;-----------------------------------------------------------------------------
; Calculate 1bpp bitmap coordinate address
;   Input: C: Y-coordinate
;         DE: X-coordinate
; Output: BC: Pixel Offset
;         DE: Coordinate Address
; Clobbered: HL
;-----------------------------------------------------------------------------
bitmap_addr:   
    ld      a,e                   
    and     7                     ; A = X mod 8
    ex      af,af'                ; A' = PxlOfs
    srl     d                     ; E = X / 8
    rr      e                     ; 
    srl     e                     
    srl     e                     ; E = BytOfs
    ld      d,high(BANK1_BASE)    ; DE = VidRAM + BytOfs
    call    _mult40c              ; HL = RowAdr
_bitmap_addr:
    add     hl,de                 ; HL = BytAdr
    ex      de,hl                 ; DE = BytAdr
    ex      af,af'                
    ld      b,0                   ; BC = PxlOfs
    ld      c,a       
    ret 

; Must preserve AF
; DE = X, C = Y    
_check_coords_c:
    ld      hl,-160               ; 4bpp 
    jr      _check_coords_bc
_check_coords_b:
    ld      hl,-320               ; 1bpp
_check_coords_bc:
    ld      a,199
; DE = X, C = Y, HL = MaxX, A = MaxY
_check_coords:
    add     hl,de                 ; If HL >= MaxX
    ret     c                     ;   Return carry set
    cp      c                     ; If Y >= 200
    ret                           ;   Return carry set

; HL = C * 40
; Clobbered: A,B
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
; Lookup tables
;-----------------------------------------------------------------------------
_ormask1bpp:
    byte    $80,$40,$20,$10,$08,$04,$02,$01
_andmask1bpp:
    byte    $7F,$BF,$DF,$EF,$F7,$FB,$FD,$FE
_setmask:
    byte    $0F,$F0
_getmask:
    byte    $F0,$0F

_bitmap_code_size = $ - bitmap_resetpixel
