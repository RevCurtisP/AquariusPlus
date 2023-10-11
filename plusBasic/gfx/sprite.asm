;=============================================================================
; Spritle and Sprite Assembly Routines
;=============================================================================

; Sprite attributes
SPR_ENABLE    equ   $80
SPR_PRIORITY  equ   $40
SPR_HEIGHT    equ   $08
SPR_VFLIP     equ   $04
SPR_HFLIP     equ   $02

;-----------------------------------------------------------------------------
; Sprite Registers         76543210  76543210
; IO_VSPRSEL                        ~~NNNNNN  spriteNum
; IO_VSPRX_H  IO_VSPRX_L  ~~~~~~~X  XXXXXXXX  Xposition
; IO_VSPRY	                        YYYYYYYY  Yposition
; IO_VSPRATTR IO_VSPRIDX  ERPPTVHI  IIIIIIII  
;                         Enable pRiority Palette heighT Vflip Hflip tileIndex
;-----------------------------------------------------------------------------
; SpriteDef Structure
; Byte 0: Spritle Count
;      1: Total pixel width
;      2: Total pixel height
;   3...: Spritles List   + 0 : Spritle Number
;                         + 1: X-offset
;                         + 2: Y-offset
;
; TileDef Structure
; Byte 0: Tile Count            
;   1...: Tile List       + 0 : Tile LSB
;                         + 1 : Tile MSB
;
; AtrDef Structure
; Byte 0: Tile Count
;   1...: Attr List     Bit 7 : Enabled (sprite only)
;                           6 : Priority
;                         4-5 : Palette
;                           3 : Height (sprite only)
;                           2 : H-flip
;                           1 : V-flip
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; Set Sprite Attributes
; Input:  C: Attribute Count
;        DE: Attribute List Address
;        HL: SpriteDef Address
; Output: Not Zero if tile count <> spritle count
; Clobbered: A,BC,DE
;-----------------------------------------------------------------------------
sprite_set_attrs:
    ld      ix,spritle_set_attr
    jr      _sprite_attrs

sprite_set_palettes:
    ld      ix,spritle_set_palette

_sprite_attrs:
    push    hl                    ; Stack = SprAdr, RtnAdr
    ld      a,c                   ; A = AtrCnt
    cp      (hl)                  ; If AtrCnt <> SprCnt
    jr      nz,.error             ;   Error
    ld      b,(hl)                ; B = spritle/tile count
    ex      de,hl                 ; DE = SprAdr, HL = AtrAdr
.loop 
    inc     de                    ; Skip SprCnt/PrvSprt#
    inc     de                    ; Skip X,Y/Width/Height
    inc     de
    ld      a,(de)                ; A = Sprtl#
    ld      c,(hl)                ; C = Attrbt
    call    _sprite_attr
    inc     hl                    ; AtrAdr += 1
    djnz    .loop
.error
    pop     hl                    ; HL = SprAdr; Stack = RtnAdr
    ret
    

_sprite_attr:
    jp      (ix)

;-----------------------------------------------------------------------------
; Set spritle attributes 
; Input: A: sprite #  0-63
;        C: attributes: Bit 6: Priority
;                       Bit 3: Double-Height
;                       Bit 2: Vertical Flip
;                       Bit 1: Horizontal Flip
;-----------------------------------------------------------------------------
spritle_set_attr:
    out   (IO_VSPRSEL),a          ; Select sprite
    ex    af,af'
    ld    a,c
    and   $4E                     ; Only attribute bits
    ld    c,a
    in    a,(IO_VSPRATTR)         ; Get current attributes
    and   $B1                     ; Keep enabled, palette and tile index msb
    or    c                       ; Set attribute bits
    out   (IO_VSPRATTR),a         ; and write back out
    ex    af,af'
    ret
    
;-----------------------------------------------------------------------------
; Set spritle palette
; Input: A: sprite #  0-63
;        C: color palette  0-3
;-----------------------------------------------------------------------------
spritle_set_palette:
    out   (IO_VSPRSEL),a          ; Select sprite
    ex    af,af'
    ld    a,c                     ; Get palette #
    exx
    and   $03                     ; Mask it
    rla                           ; Shift into position
    rla
    rla
    rla
    ld    c,a                     ; Put back in C
    in    a,(IO_VSPRATTR)         ; Get current attributes
    and   $C7                     ; Keep attributes and tile index msb
    or    c                       ; Set attribute bits
    out   (IO_VSPRATTR),a         ; and write back out
    exx
    ex    af,af'
    ret


;-----------------------------------------------------------------------------
; Set Sprite Properties
; Input: BC: PrpLen 
;        DE: PrpAdr
;        HL: SprAdr
;        Not Zero tile count <> spritle count
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
sprite_set_props:
    ld      a,c                   ; Get tile count
    srl     a
    cp      (hl)                  ; If not equal to spritle count
    jr      nz,.ret               ;   Return NZ (error)
    ld      b,(hl)                ; B = spritle/tile count
.loop 
    inc     hl                    ; Skip X-offset
    inc     hl                    ; Skip Y-offset
    inc     hl                    ; Next spritle entry
    ld      a,(hl)                ; Get spritle#
    out     (IO_VSPRSEL),a        ; Select sprite
    ld      a,(de)                ; Get Tile Index
    out     (IO_VSPRIDX),a        ; and Write it
    inc     de
    ld      a,(de)                ; Get Attributes
    out     (IO_VSPRATTR),a       ; and write them
    inc     de                    ; Next Property
    djnz    .loop                 ; Do next one
    xor     a                     ; Return Z (success)
.ret
    ret



;-----------------------------------------------------------------------------
; Set sprite tile indexes
; Input: BC: TilLen 
;        DE: TilAdr
;        HL: SprAdr
;        Not Zero tile count <> spritle count
; Clobbered: A,BC,DE
;-----------------------------------------------------------------------------
sprite_set_tiles:
    push    hl
    ld      a,c                   ; Get tile count
    srl     a
    cp      (hl)                  ; If not equal to spritle count
    jr      nz,.ret               ;   Return NZ (error)
    ld      b,(hl)                ; B = spritle/tile count
    ex      de,hl                 ; DE = SprAdr, HL = TilAdr
.loop 
    inc     de                    ; Skip X-offset
    inc     de                    ; Skip Y-offset
    inc     de                    ; Next spritle entry
    ld      a,(de)                ; Get spritle#
    push    de                    ; Stack = SprAdr
    ld      e,(hl)
    inc     hl 
    ld      d,(hl)                ; DE = TilIdx
    inc     hl
    call    spritle_set_tile
    pop     de                    ; DE = SprAdr
    djnz    .loop                 ; Do next one
    xor     a                     ; Return Z (success)
.ret
    pop     hl
    ret

;-----------------------------------------------------------------------------
; Set spritle tile index
; Input: A: spritle# (0-63)
;        DE: tile index (0-511)
;-----------------------------------------------------------------------------
spritle_set_tile:
    out     (IO_VSPRSEL),a        ; Select sprite
    ex      af,af'
    ld      a,e                   ; Write index LSB             
    out     (IO_VSPRIDX),a
    ld      a,d                   ; Get index MSB
    exx     
    and     $01                   ; Mask off unused bits
    ld      d,a                   ; Back into D
    in      a,(IO_VSPRATTR)       ; Get current attributes
    and     $FE                   ; Keep attribute and palette bits
    or      d                     ; Set attribute bits
    out     (IO_VSPRATTR),a       ; and write back out
    exx 
    ex      af,af'
    ret


;-----------------------------------------------------------------------------
; Get sprite deta
; Input:  BC: Buffer Length
;         DE: Buffer Address
;         HL: Sprite Address
; Data Format: 5 byte entry for each spritle 
;          X-position - +0
;          Y-position - +2
;          Attrs+Tile - +3
;;-----------------------------------------------------------------------------
sprite_get_attrs:
    ld      a,(hl)                ; SptlCnt
    ld      b,a
    add     a                     ; x 2     
    add     a                     ; x 4
    add     b                     ; x 5
    cp      c                     ; If Buffer Size <> Spritle Count x 5
    ret     nz                    ;   Return Error
    ld      b,(hl)                ; B = SptCnt
.loop    
    inc     hl                    ; Skip SptCnt/Spritle#
    inc     hl                    ; Skip Width/X-offset
    inc     hl                    ; Skip Height/Y-offxet
    ld      a,(hl)                ; A = Spritle#
    push    bc                    ; Stack = SptCnt, RtnAdr
    push    hl                    ; Stack = SprAdr, SprCnt, RtnAdr
    push    de                    ; Stack = BufAdr, SprAdr, SprCnt, RtnAdr
    call    spritle_get_attrs ; BC = X-position; DE = Y-position, HL = Tile+Attrs
    ex      (sp),hl               ; HL = BufAdr; Stack = Tile+Attrs, SprAdr, SprCnt, RtnAdr
    ld      (hl),c
    inc     hl
    ld      (hl),b                ; X-position into Buffer
    inc     hl
    ld      (hl),e                ; Y-position into Buffer                
    inc     hl
    pop     de                    ; DE = Tile+attrs; Stack = SprAdr, SprCnt, RtnAdr
    ld      (hl),e                
    inc     hl
    ld      (hl),d                ; Tile+Addrs into Buffer                
    inc     hl
    ex      de,hl                 ; DE = BufAdr
    pop     hl                    ; HL = SprAdr; Stack = SprCnt, RtnAdr
    pop     bc                    ; BC = SprCnt; Stack = RtnAdr
    djnz    .loop
    xor     a                     ; Set Zero Flag - No Errors
    ret

;-----------------------------------------------------------------------------
; Get spritle detaila
; Input:  A: Spritle#
; Output: BC: X-position
;         DE: Y-position
;         HL: Attrs+Tile#
;-----------------------------------------------------------------------------
spritle_get_attrs:
    out   (IO_VSPRSEL),a          ; Select sprite
    ex    af,af'
    in    a,(IO_VSPRX_L)          ; BC = X-position
    ld    c,a
    in    a,(IO_VSPRX_H)  
    ld    b,a
    in    a,(IO_VSPRY)            ; DE = Y-position
    ld    e,a
    ld    d,0
    in    a,(IO_VSPRIDX)          ; HL = Attrs+Tile#
    ld    l,a
    in    a,(IO_VSPRATTR)         ; HL = Attrs+Tile#
    ld    h,a
    ex    af,af'
    ret

;-----------------------------------------------------------------------------
; Clear all properties of a spritle
; Input: A: sprite #  0-63
; Clobbers: BC
;-----------------------------------------------------------------------------
spritle_clear:
    out     (IO_VSPRSEL),a        ; Select sprite
    ex      af,af'
    xor     a
    ld      c,$E5
    ld      b,5
.loop
    out     (c),a
    inc     c
    djnz    .loop
    ex      af,af'
    ret

;-----------------------------------------------------------------------------
; Clear all properties of all spritles
; Clobbers: A,BC
;-----------------------------------------------------------------------------
spritle_clear_all:
    ld      a,63
spritle_clear_a2z:
    call    spritle_clear
    dec     a
    ret     m
    jr      spritle_clear_a2z
    ret

;-----------------------------------------------------------------------------
; Enable/disable sprite
; Input: A: 0 = Disable, else Enable
;        HL: spritedef address
; Clobbered: A,BC,HL
;-----------------------------------------------------------------------------
sprite_toggle:
    or      a
    jr      z,.zero
    ld      a,$80
.zero
    ld      c,a
    ld      b,(hl)                ; Get sprite list length
.loop 
    inc     hl                    ; Skip X-offset
    inc     hl                    ; Skip Y-offset
    inc     hl                    ; Next spritle entry
    ld      a,(hl)                ; Get spritle#
    call    spritle_toggle        ; Toggle it
    djnz    .loop
    xor     a                     ; No Errors
    ret

;-----------------------------------------------------------------------------
; Enable/Disable spritle
; Input: A: sprite #  0-63
;        C: 128 = Enable, $0 = Disable
;-----------------------------------------------------------------------------
spritle_toggle:
    out     (IO_VSPRSEL),a        ; Select sprite
    ex      af,af'
    ld      a,$80                 ; Only Enable Bit
    and     c
    ld      c,a 
    in      a,(IO_VSPRATTR)       ; Get current attributes
    and     $7F                   ; Keep all other attributes
    or      c                     ;   Set enable bit
    out     (IO_VSPRATTR),a       ; and write back out
    ex      af,af'
    ret

;-----------------------------------------------------------------------------
; Enable/Disable all spritles
; Input: C: 128 = Enable, $0 = Disable
; Clobbers: A
;-----------------------------------------------------------------------------
spritle_toggle_all:
    ld      a,63
spritle_toggle_a2z:
    call    spritle_toggle
    dec     a
    ret     m 
    jr      spritle_toggle_a2z

;-----------------------------------------------------------------------------
; Set sprite position
; Input: BC: X-position
;        DE  Y-position 
;        HL: spritedef address
; Clobbered: A, HL
;-----------------------------------------------------------------------------
sprite_set_pos:
    ld      a,(hl)                ; A = spritle count
    push    hl                    ; Stack = SprAdr, RtnAdr
    inc     hl                    ; Skip width and height
    inc     hl
.loop  
    ex      af,af'                
    inc     hl                    ; 
    ld      a,(hl)                ; Get spritle#
    out     (IO_VSPRSEL),a        ; Select it
    inc     hl                    ; 
    ld      a,(hl)                ; Get X-offset LSB
    add     c                     ; Add to X-position
    out     (IO_VSPRX_L),a        ; and write it
    ld      a,0                   ; Add carry to MSB
    adc     b
    out     (IO_VSPRX_H),a        ; and write it
    inc     hl                    ; 
    ld      a,(hl)                ; Get Y-offset LSB
    add     e                     ; Add to Y-position
    out     (IO_VSPRY),a          ; and write it
    ex      af,af'
    dec     a
    jr      nz,.loop              ; 144 cycles per loop = 
    pop     hl                    ; HL = SprAdr; Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; Set spritle position
; Input: A: sprite #  0-63
;       BC: X-position
;       DE  Y-position 
;-----------------------------------------------------------------------------
spritle_set_pos:
    out   (IO_VSPRSEL),a          ; Select sprite
    ex    af,af'
    ld    a,c
    out   (IO_VSPRX_L),a          ; Set X-position
    ld    a,b
    out   (IO_VSPRX_H),a  
    ld    a,e                       
    out   (IO_VSPRY),a            ; Set Y-position
    ex    af,af'
    ret

;-----------------------------------------------------------------------------
; Get spritle position
; Input: A: sprite #  0-63
; Output: BC: X-position
;         DE  Y-position 
;-----------------------------------------------------------------------------
spritle_get_pos:
    out   (IO_VSPRSEL),a          ; Select sprite
    ex    af,af'
    in    a,(IO_VSPRX_L)          ; Get X-position
    ld    c,a
    in    a,(IO_VSPRX_H)  
    ld    b,a
    in    a,(IO_VSPRY)            ; Get Y-position
    ld    e,a
    ld    d,0
    ex    af,af'
    ret

