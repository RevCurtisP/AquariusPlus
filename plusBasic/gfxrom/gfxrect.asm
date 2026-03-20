;====================================================================
; Graphics Pixel and Bloxel Rectangle Drawing Kernel Routines
;====================================================================

; SCREEN 0,2:CLEAR BITMAP 7,0:LINE (20,50)-(120,150),,FILL:PAUSE
; SCREEN 0,2:CLEAR BITMAP 7,0:CALL @63,$04B ARGS 0,60,30,0,160,130:PAUSE
; SCREEN 0,2:CALL @63,$04B ARGS $CB00,50,20,0,150,120:PAUSE
; SCREEN 0,2:CALL @63,$04B ARGS $9D00,60,30,0,160,130:PAUSE
; SCREEN 0,3:CLEAR BITMAP:CALL @63,$04B ARGS 3,50,20,0,150,120:PAUSE
; CALL @63,$04B ARGS 0,20,30,0,50,45


;-----------------------------------------------------------------------------
; Draw rectangle of bloxels or pixels screen
; Input: BC: Start X-Coord
;        DE: Start Y-Coord
;         H: Option: 0, PSET (colors)PRESET, XOR
;         L: Colors
;        BC': End X-Coord
;        DE': End Y-Coord
; Clobbered: All
;-----------------------------------------------------------------------------
bitmap__frect:
    push    hl                    ; Stack = ModClr, RtnAdr
    call    bitmap_rect_size      ; BC' = Width, DE' = Height
    pop     hl                    ; H = Option, L = Colors; Stack = RtnAdr
    ret     c
    ld      a,(BMPMODE)
    or      a
    jr      z,_frect40
    dec     a
    jr      z,_frect80
    dec     a
    jr      z,_frect1bpp
    jp      _frect4bpp

; Next X: DE += 1, Next Byte if DE & 2
; Next Y: DE += 2, Next Row if E > $F5
 
_frect40:
    call    bloxel_40col_addr   ; HL = ScrAdr, DE = MskAdr, A = BitMask
.vloop:
    exx                         ; DE = VertCount
    push    bc                  ; Stack = Width, RtnAdr
    dec     de
    ld      a,d
    or      e                   ; VertCount -= 1
    exx                         ; DE' = VertCount
    pop     bc                  ; BC = Width; Stack = RtnAdr
    ret     z
    push    hl                  ; Stack = BytAdr, RtnAdr
    push    de                  ; Stack = MskAdr, BytAdr, RtnAdr
    call    _xloop
    pop     de
    pop     hl
    ld      a,e
    add     a,2
    cp      low(gfx_bloxel_mask)+6
    ld      e,a
    jr      c,.vloop
    sub     a,6
    ld      e,a
    push    de
    ld      de,40
    add     hl,de               ; HL = Next BytAdr
    pop     de                  ; D = BitMsk Stack = RtnAdr
    jr      .vloop              ; Do next line

_xloop:
    ld      a,(hl)                ; A = ScrnChr
    or      $A0                   ; Convert to GfxChr
    xor     (hl)                  ; Clear bits 5 and 7
    ld      a,(hl)                ; Reload ScrnChr
    jr      z,.ploop              ; If not GfxChr
    ld      a,$A0                 ;   Set to blank
.ploop
    ex      af,af'
    ld      a,b
    or      c
    ret     z
    ex      af,af'
    dec     bc
    ex      de,hl                 ; HL = MskAdr, DE = ScrAdr
    or      (hl)                  ; Set Pixel in Mask
    ex      de,hl                 ; DE = MskAdr, HL = ScrAdr
    inc     e                     ; Next Pixel
    bit     0,e                   ; If not next byte
    jr      nz,.ploop             ;   Do next pixel
    ld      (hl),a                ; Write GfxChr
    inc     hl
    dec     e
    dec     e                     ; Back up to left pixel
    jr      _xloop


_frect80:
    ret

; PSETTK = $9C %10011100
;PRESETK = $9D %10011101
;  XORTK = $CB %11001011
_frect1bpp:
    ld      a,h                   ; A = Mode
    ld      h,$FF                 ; H = Flip BitMsk
    ld      ix,andbit1bpp
    cp      PRESETK
    jr      z,.frect
    inc     h                     ; H = 0
    ld      ix,xorbit1bpp
    cp      XORTK
    jr      z,.frect
    ld      ix,orbit1bpp
.frect
    ld      a,l                 ; A = Color
    ex      af,af'              ; A' = Color
    in      a,(IO_BANK1)
    push    af                  ; Stack = OldPg, RtnAdr
    ld      a,VIDEO_RAM
    out     (IO_BANK1),a
    push    hl                  ; Stack = MskClr, OldPg, RtnAdr`
    call    pixel_1bpp_addr     ; A = BitMsk, DE = BytAdr, HL = CellAdr
    push    bc
    exx
    pop     hl                  ; HL' = CellAdr
    exx
    pop     hl                  ; H = BitFlp; Stack = OldPg, RtnAdr
    xor     h                   ; Flip bits if PRESET
    ex      de,hl               ; HL = BytAdr
    ld      d,a                 ; D = BitMsk
    call    .vloop
    pop     af                  ; A = OldPg; Stack = RtnAdr
    out     (IO_BANK1),a
    ret

.vloop
    exx                         ; DE = VertCount
    push    bc                  ; Stack = Width, RtnAdr
    dec     de
    ld      a,d
    or      e
    exx                         ; DE' = VertCount
    pop     bc                  ; BC = Width; Stack = RtnAdr
    ret     z
    push    de                  ; Stack = BitMsk, RtnAdr
    push    hl                  ; Stack = BytAdr, BitMsk, RtnAdr
    call    .hloop
    pop     hl                  ; HL = BytAdr; Stack = BitMsk, RtnAdr
    ld      de,40
    add     hl,de               ; HL = Next BytAdr
    pop     de                  ; D = BitMsk Stack = RtnAdr
    jr      .vloop              ; Do next line

.hloop
    ld      e,(hl)              ; E = ScrByt
.bloop
    ld      a,b
    or      c
    ret     z
    dec     bc
    ld      a,e                 ; A = ScrByt
    call    jump_ix
    jr      nc,.bloop           ; Do next bit
    rr      d
    ld      (hl),e              ; Store Byte
    inc     hl                  ; Next Byte
    jr      .hloop

orbit1bpp:
    or      d                   ; A = NewByt
    ld      e,a
    rr      d                   ; Shift Bitmask
    ret

xorbit1bpp:
    xor     d                   ; A = NewByt
    ld      e,a
    rr      d                   ; Shift Bitmask
    ret

andbit1bpp:
    and     d                   ; A = NewByt
    ld      e,a
    rr      d                   ; Shift Bitmask
    scf
    ret


_frect4bpp:
    in      a,(IO_BANK1)
    push    af                  ; Stack = OldPg, RtnAdr
    ld      a,VIDEO_RAM
    out     (IO_BANK1),a
    ld      a,l                 ; A = Color
    push    af                  ; Stack = Color, OldPg, RtnAdr
    call    pixel_4bpp_addr     ; BC = PxlOfs, DE = BytAdr
    pop     af                  ; A = Color; Stack = OldPg, RtnAdr
    call    pixel_4bpp_mask     ; B = BitMsk, C = Color
    or      a
    rla
    rla
    rla
    rla
    or      c                   ; A = Colors
    ex      af,af'              ; A' = Colors
    ex      de,hl               ; HL = BytAdr
    ld      d,b                 ; D = BitMsk
    call    .vloop
.done
    pop     af                  ; A = OldPg; Stack = RtnAdr
    out     (IO_BANK1),a
    ret

.vloop
    exx                         ; DE = VertCount
    push    bc                  ; Stack = Width, RtnAdr
    dec     de
    ld      a,d
    or      e
    exx                         ; DE' = VertCount
    pop     bc                  ; BC = Width; Stack = RtnAdr
    ret     z
    push    de                  ; Stack = BitMsk, RtnAdr
    push    hl                  ; Stack = BytAdr, BitMsk, RtnAdr
    call    .hloop
    pop     hl                  ; HL = BytAdr; Stack = BitMsk, RtnAdr
    ld      de,80
    add     hl,de               ; HL = Next BytAdr
    pop     de                  ; D = BitMsk Stack = RtnAdr
    jr      .vloop              ; Do next line

.hloop
    ld      e,(hl)              ; E = ScrByt
.bloop:
    ld      a,b
    or      c
    ret     z
    dec     bc
    ex      af,af'              ; A = Colors
    push    af                  ; Stack = Colors, RtnAdr
    and     d                   ; A = ClrNyb
    ex      af,af'              ; A' = ClrNyb
    ld      a,d                 ; A = BitMsk
    xor     $FF                 ; A = AndMsk
    push    af                  ; Stack = NewMsk, Colors, RtnAdr
    and     e                   ; Clear Nybble
    ld      e,a
    ex      af,af'              ; A = ClrNyb
    or      e                   ; OR into byte
    ld      e,a                 ; E = New Byte
    pop     af                  ; A = NewMsk; Stack = Colors, RtnAdr
    ex      af,af'              ; A' = NewMsk
    pop     af                  ; A = Colors; Stack = RtnAdr
    ex      af,af'              ; A = NewMsk, A' = Colors
    ld      d,a                 ; D = NewMsk
    jp      p,.bloop            ; Loop if right nybble
    ld      (hl),e              ; Store Byte
    inc     hl                  ; Next Byte
    jr      .hloop
