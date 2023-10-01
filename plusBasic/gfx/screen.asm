;=============================================================================
; Text Screen Assembly Routines
;=============================================================================

;-----------------------------------------------------------------------------
; Fill Text Screen Section with Character+Color
; Input: B: Start Column
;        C: End Column
;        D: Start Row
;        E: End Row
;        H: Character
;        L: Color
; Clobbered: A, BC, DE
;-----------------------------------------------------------------------------
screen_fill:
    call    screen_convert_rect   ; A = RowCnt, C = ColCnt, DE = RowAdr
    ret     c
    ex      de,hl                 ; D = Char, E = Color, HL = RowAdr
    ex      af,af'                ; A' = RowCnt
    ld      a,c                   ; A = ColCnt
    ex      af,af'                ; A = RowCnt, A' = ColCnt
.loop
    push    hl                    ; Save RowAdr
    ex      af,af'                ; A = ColCnt, A' = RowCnt
    ld      b,a                   ; B = ColCnt
.row
    ld      (hl),d                ; Write Char
    set     2,h
    ld      (hl),e                ; Write Color
    res     2,h
    inc     hl
    djnz    .row                  ; Next Column
    pop     hl                    ; HL = RowAdr
    ld      bc,40
    add     hl,bc                 ; HL = Next RowAdr
    ex      af,af'                ; A = RowCnt, A' = ColCnt
    dec     a
    jr      nz,.loop
    ret

;-----------------------------------------------------------------------------
; Read Text Screen Section into Buffer
; Input: B: Start Column
;        C: End Column
;        D: Start Row
;        E: End Row
;        HL: Buffer Address
; Clobbered: A, BC, DE
;-----------------------------------------------------------------------------
screen_get:
    call    screen_convert_rect   ; A = RowCnt, C = ColCnt, DE = RowAdr
    ret     c
    ld      (hl),c                ; Buffer[0] = Columns
    inc     hl
    ld      (hl),a                ; Buffer[1] = Rows
    inc     hl
    ex      af,af'
.rows:
    push    de                    ; Stack = RowAdr, RtnAdr
    ld      b,c                   ; B = Column Countdown
.columns
    ld      a,(de)                ; A = Character
    ld      (hl),a                ; Put in buffer
    inc     hl
    set     2,d
    ld      a,(de)                ; A = Colors
    ld      (hl),a                ; Put in buffer
    inc     hl
    res     2,d
    inc     de
    djnz    .columns
    pop     de                    ; DE = RowAdr; Stack = RtnAdr
    ex      af,af'
    dec     a                     ; If no more rows
    ret     z                     ; Return
    ex      af,af'
    ld      a,40
    add     e
    ld      e,a
    ld      a,0
    adc     d
    ld      d,a                   ; DE = Next RowAdr
    jr      .rows

;-----------------------------------------------------------------------------
; Write Text Screen Section from Buffer
; Input: C: Column
;        E: Row
;       HL: Buffer Address
; Clobbered: A, BC, DE
;-----------------------------------------------------------------------------
screen_put:
    call    _screen_bounds
    ret     c
    call    screen_pos_addr       ; DE = RowAdr
    ld      c,(hl)                ; C = ColCnt
    inc     hl
    ld      a,(hl)                ; A = RowCnt
    inc     hl
    ex      af,af'
.rows:
    push    de                    ; Stack = RowAdr, RtnAdr
    ld      b,c                   ; B = Column Countdown
.columns
    ld      a,(hl)                ; A = Character
    ld      (de),a                ; Write it
    inc     hl
    ld      a,(hl)                ; A = Colors
    set     2,d
    ld      (de),a                ; Write it
    inc     hl
    res     2,d
    inc     de
    djnz    .columns
    pop     de                    ; DE = RowAdr; Stack = RtnAdr
    ex      af,af'
    dec     a                     ; If no more rows
    ret     z                     ; Return
    ex      af,af'
    ld      a,40
    add     e
    ld      e,a
    ld      a,0
    adc     d
    ld      d,a                   ; DE = Next RowAdr
    jr      .rows

;-----------------------------------------------------------------------------
; Convert Screen Coordinates to Size and Start Address
; Input: B: Start Column
;        C: End Column
;        D: Start Row
;        E: End Row
; Output: A = Row Count
;         C = Column Count
;        DE = Start Address
;-----------------------------------------------------------------------------
screen_convert_rect:
    call    _screen_bounds        ; Check EndCol and EndRow
    ret     c
    ld      ix,screen_pos_addr
    call    gfx_convert_rect      ; A = RowCnt, C = ColCnt, DE = RowAdr
    ret

;-----------------------------------------------------------------------------
; Calculate screen position address
; Input: C: Column
;        E: Row
; Output: DE = Cell Address
; Clobbered: A
;-----------------------------------------------------------------------------
screen_pos_addr:
    ld      a,e
    add     a,a
    add     a,a
    add     a,e
    ld      e,a
    ld      d,0                   ; DE = Row * 5
    ex      de,hl                 ; HL = Row * 5
    add     hl,hl
    add     hl,hl
    add     hl,hl                 ; HL = Row * 40
    push    bc
    ld      b,0                   ; BC = Column
    add     hl,bc                 ; HL = Row * 40 + Column
    ld      bc,SCREEN             ; Screen character-matrix (= 12288 dec)
    add     hl,bc                 ; HL = Cell Address
    pop     bc
    ex      de,hl                 ; DE = Cell Address
    ret

; In: C=Column, E=Row
; Out: Carry set if out of bounds
_screen_bounds:
    ld      a,40
    cp      b                     ; If EndCol > 39
    ret     c                     ;   Return Carry Set
    ld      a,25
    cp      e                     ; If EndRow > 24
    ret                           ;   Return Carry Set

;-----------------------------------------------------------------------------
; Copy Screen Buffer to Text Screen
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
screen_restore:
    ld      a,BAS_BUFFR
    ld      bc,2048
    ld      de,SCREEN             ; Copying from Text and Color RAM
    ld      hl,SCREENBUFF         ; Copying to Text Screen Buffer
    jp      page_fast_read_bytes  ; Do it

;-----------------------------------------------------------------------------
; Copy Text Screen to Screen Buffer
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
screen_save:
    ld      a,BAS_BUFFR
    ld      bc,2048
    ld      de,SCREENBUFF         ; Copying to Text Screen Buffer
    ld      hl,SCREEN             ; Copying from Text and Color RAM
    jp      page_fast_write_bytes ; Do it

;-----------------------------------------------------------------------------
; Swap Text Screen with Screen Buffer
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
screen_swap:
    ld      a,BAS_BUFFR
    ld      bc,2048
    ld      de,SCREENBUFF         ; Swapping Text Screen Buffer
    ld      hl,SCREEN             ; with Text and Color RAM
    jp      page_mem_swap_bytes   ; Do it

;-----------------------------------------------------------------------------
; Set Screen Mode
; Input: A: Mode (see SCREEN statement)
; Output: A: Byte written to Video Control Regiter
;        BC: Mode
;        HL: Address in lookup table
;-----------------------------------------------------------------------------
screen_set_mode:
    ld      hl,_screen_modes
    ld      b,0
    ld      c,a
    add     hl,bc
    ld      a,(hl)
    out     (IO_VCTRL),a
    ret

_screen_modes:                    ; mode  b p s gm t
    byte    $01                   ;   0   0 0 0 00 1 Text
    byte    $02                   ;   1   0 0 0 01 0 64 x 32 Color Tilemap
    byte    $04                   ;   2   0 0 0 10 0 320 x 200 Bitmap mode ON
    byte    $06                   ;   3   0 0 0 11 0 Multicolor Bitmap Mode

    byte    $09                   ;   4   0 0 1 00 1 Text    + Sprites
    byte    $0A                   ;   5   0 0 1 01 0 Tilemap + Sprites
    byte    $0C                   ;   6   0 0 1 10 0 Bitmap  + Sprites
    byte    $0E                   ;   7   0 0 1 11 0 Multi   + Sprites

    byte    $01                   ;   8   0 0 0 00 1 Text
    byte    $03                   ;   9   0 0 0 01 1 Tilemap + BackText
    byte    $05                   ;  10   0 0 0 10 1 Bitmap  + BackText
    byte    $07                   ;  11   0 0 0 11 1 Multi   + BackText

    byte    $09                   ;  12   0 0 1 00 1 Text    + Sprites
    byte    $0B                   ;  13   0 0 1 01 1 Tilemap + BackText = Sprites
    byte    $0D                   ;  14   0 0 1 10 1 Bitmap  + BackText = Sprites
    byte    $0F                   ;  15   0 0 1 11 1 Multi   + BackText = Sprites

    byte    $01                   ;  16   0 0 0 00 1 Text
    byte    $13                   ;  17   0 1 0 01 1 Tilemap + FrontText
    byte    $15                   ;  18   0 1 0 10 1 Bitmap  + FrontText
    byte    $17                   ;  19   0 1 0 11 1 Multi   + FrontText

    byte    $09                   ;  20   0 0 1 00 1 Text    + Sprites
    byte    $1B                   ;  21   0 1 1 01 1 Tilemap + FrontText = Sprites
    byte    $1D                   ;  22   0 1 1 10 1 Bitmap  + FrontText = Sprites
    byte    $1F                   ;  23   0 1 1 11 1 Multi   + FrontText = Sprites

    byte    $21                   ;  24   1 0 0 00 1 Remap + Text
    byte    $22                   ;  25   1 0 0 01 0 Remap + 64 x 32 Color Tilemap
    byte    $24                   ;  26   1 0 0 10 0 Remap + 320 x 200 Bitmap mode ON
    byte    $26                   ;  27   1 0 0 11 0 Remap + Multicolor Bitmap Mode

    byte    $29                   ;  28   1 0 1 00 1 Remap + Text    + Sprites
    byte    $2A                   ;  29   1 0 1 01 0 Remap + Tilemap + Sprites
    byte    $2C                   ;  30   1 0 1 10 0 Remap + Bitmap  + Sprites
    byte    $2E                   ;  31   1 0 1 11 0 Remap + Multi   + Sprites

    byte    $21                   ;  32   1 0 0 00 1 Remap + Text
    byte    $23                   ;  33   1 0 0 01 1 Remap + Tilemap + BackText
    byte    $25                   ;  34   1 0 0 10 1 Remap + Bitmap  + BackText
    byte    $27                   ;  35   1 0 0 11 1 Remap + Multi   + BackText

    byte    $29                   ;  36   1 0 1 00 1 Remap + Text    + Sprites
    byte    $2B                   ;  37   1 0 1 01 1 Remap + Tilemap + BackText = Sprites
    byte    $2D                   ;  38   1 0 1 10 1 Remap + Bitmap  + BackText = Sprites
    byte    $2F                   ;  39   1 0 1 11 1 Remap + Multi   + BackText = Sprites

    byte    $21                   ;  40   1 0 0 00 1 Remap + Text
    byte    $33                   ;  41   1 1 0 01 1 Remap + Tilemap + FrontText
    byte    $35                   ;  42   1 1 0 10 1 Remap + Bitmap  + FrontText
    byte    $37                   ;  43   1 1 0 11 1 Remap + Multi   + FrontText

    byte    $29                   ;  44   1 0 1 00 1 Remap + Text    + Sprites
    byte    $3B                   ;  45   1 1 1 01 1 Remap + Tilemap + FrontText = Sprites
    byte    $3D                   ;  46   1 1 1 10 1 Remap + Bitmap  + FrontText = Sprites
    byte    $3F                   ;  47   1 1 1 11 1 Remap + Multi   + FrontText = Sprites
