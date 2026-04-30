;====================================================================
; Graphics Pixel and Bloxel Rectangle Drawing Kernel Routines
;====================================================================

; SCREEN 0,2:CLEAR BITMAP 7,0:LINE (20,50)-(120,150),,FILL:PAUSE
; SCREEN 0,2:CLEAR BITMAP 7,0:CALL @63,$04B ARGS 0,60,30,0,160,130:PAUSE
; SCREEN 0,2:CALL @63,$04B ARGS $CB00,50,20,0,150,120:PAUSE
; SCREEN 0,2:CALL @63,$04B ARGS $9D00,60,30,0,160,130:PAUSE
; SCREEN 0,3:CLEAR BITMAP:CALL @63,$04B ARGS 3,50,20,0,150,120:PAUSE
; CALL @63,$04B ARGS 0,20,30,0,50,45
; CALL @63,$04B ARGS $9D00,20,30,0,50,45

;-----------------------------------------------------------------------------
; Draw filled rectangle of bloxels or pixels
; Input: BC: Start X-Coord
;        DE: Start Y-Coord
;         H: Option: 0, PRESET, XOR
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
    jp      z,_frect1bpp
    jp      _frect4bpp

_frect80:
    call    _bloxelop           ; IX = BxlAdr, HL = TblOfs
    push    hl                  ; Stack = TblMSB, RtnAdr
    call    bloxel_80col_addr   ; HL = ScrAdr, DE = MskAdr
    ld      bc,80
    jr      _frectb
_frect40:
    call    _bloxelop           ; IX = BxlAdr, HL = TblOfs
    push    hl                  ; Stack = TblMSB, RtnAdr
    call    bloxel_40col_addr   ; HL = ScrAdr, DE = MskAdr
    ld      bc,40
_frectb:
    pop     af                  ; A = TblMSB; Stack = RtnAdr
    ld      d,a                 ; DE = MskAdr
    push    bc                  ; Stack = ScrWid, RtnAdr
    exx
    inc     de                  ; Bump Height for decrement
    pop     hl                  ; HL = ScrWid; Stack = RtnAdr
    exx                         ; HL' = ScrWid
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
    pop     de                  ; DE = MskAdr; Stack = BytAdr, RtnAdr
    pop     hl                  ; HL = BytAdr; Stack = RtnAdr
    ld      a,e
    add     a,2
    cp      $F6
    ld      e,a
    jr      c,.vloop
    sub     a,6
    ld      e,a
    push    de                  ; Stack = MskAdr, RtnAdr
    exx
    push    hl                  ; Stack = ScrWid, MskAdr, RtnAdr
    exx
    pop     de                  ; DE = ScrWid; Stack = MskAdr, RtnAdr
    add     hl,de               ; HL = Next BytAdr
    pop     de                  ; D = BitMsk Stack = RtnAdr
    jr      .vloop              ; Do next line

_xloop:
    ld      a,(hl)                ; A = ScrnChr
    or      $A0                   ; Convert to GfxChr
    xor     (hl)                  ; Clear bits 5 and 7
    ld      a,(hl)                ; Reload ScrnChr
    jr      z,_ploop              ; If not GfxChr
    ld      a,$A0                 ;   Set to blank
_ploop
    ex      af,af'                ; A' = BitMsk
    ld      a,b
    or      c                     ; If HrzCnt = 0
    ret     z                     ;   Return
    ex      af,af'                ; A = BitMsk
    dec     bc                    ; HrzCnt -= 1
    ex      de,hl                 ; HL = MskAdr, DE = ScrAdr
    call    jump_ix
    ex      de,hl                 ; DE = MskAdr, HL = ScrAdr
    inc     e                     ; Next Pixel
    bit     0,e                   ; If not next byte
    jr      nz,_ploop             ;   Do next pixel
    ld      (hl),a                ; Write GfxChr
    inc     hl
    dec     e
    dec     e                     ; Back up to left pixel
    jr      _xloop

_bloxelop:
    ld      a,h
    ld      ix,_presetb
    cp      PRESETK
    ld      h,high(bloxel_preset_mask)
    ret     z
    ld      h,high(bloxel_pset_mask)
    ld      ix,_xorb
    cp      XORTK
    ret     z
    ld      ix,_psetb
    sub     PSETK
    inc     a                    ; Return NZ if PSET
    ret

_psetb:
    or      (hl)
    ret

; On Entry, A = GfxChr, HL = MskAdr, DE = ScrAdr
_presetb:
    and     (hl)
    ret

_xorb:
    xor     (hl)
    ret

; PSETTK = $9C %10011100
;PRESETK = $9D %10011101
;  XORTK = $CB %11001011
_frect1bpp:
    ld      a,h                   ; A = Mode
    ld      h,$FF                 ; H = Flip BitMsk
    ld      ix,.andbit1bpp
    cp      PRESETK
    jr      z,.frect
    inc     h                     ; H = 0
    ld      ix,.xorbit1bpp
    cp      XORTK
    jr      z,.frect
    ld      ix,.orbit1bpp
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
    jp      (ix)
.bnext
    rr      d
    ld      (hl),e              ; Store Byte
    inc     hl                  ; Next Byte
    jr      .hloop

.orbit1bpp:
    or      d                   ; A = NewByt
    ld      e,a
    rr      d                   ; Shift Bitmask
    jr      nc,.bloop           ; Do next bit
    jr      .bnext

.xorbit1bpp:
    xor     d                   ; A = NewByt
    ld      e,a
    rr      d                   ; Shift Bitmask
    jr      nc,.bloop           ; Do next bit
    jr      .bnext

.andbit1bpp:
    and     d                   ; A = NewByt
    ld      e,a
    scf
    rr      d                   ; Shift Bitmask
    jr      c,.bloop            ; Do next bit
    jr      .bnext

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
