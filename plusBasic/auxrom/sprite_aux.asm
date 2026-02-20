;=============================================================================
; Sprite and Spritle Assembly Routines
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
; Detect collision between two sprites
; Input: DE = SpriteDef 1 Address
;        HL = SpriteDef 2 Address
; Returns: Carry Set if ???
; Clobbered: AF, BC, DE, HL
;-----------------------------------------------------------------------------
sprite_collision:
    ld    a,(de)
    ld    c,a                     ; C = SprLen1
    ld    b,(hl)                  ; B = SprLen2
.loop1
    push  bc                      ; Stack = SptCnt1+SprLen2, RtnAdr
    inc   de
    inc   de                      
    inc   de                      ; DE = SptPtr1
    push  hl                      ; Stack = SptDef2, SptCnt1+SprLen2, RtnAdr
.loop2
    push  bc                      ; Stack = SptCnt2, SptDef2, SptCnt1+SprLen2, RtnAdr
    push  de                      ; Stack = SptPtr1, SptCnt2, SptDef2, SptCnt1+SprLen2, RtnAdr
    inc   hl
    inc   hl
    inc   hl                      ; HL = SptPtr2
    push  hl                      ; Stack = SptPtr2, SptPtr1, SptCnt2, SptDef2, SptCnt1+SprLen2, RtnAdr
    ld    a,(de)
    ld    b,a                     ; B = Spt1
    ld    c,(hl)                  ; C = Spt2
    call  spritle_collision       ; Set Carry if Collision
    pop   hl                      ; HL = SptPtr2; Stack = SptPtr1, SptCnt2, SptDef2, SptCnt1+SprLen2, RtnAdr
    pop   de                      ; DE = SptPtr1; Stack = SptCnt2, SptDef2, SptCnt1+SprLen2, RtnAdr
    pop   bc                      ; BC = SptCnt2; Stack = SptDef2, SptCnt1+SprLen2, RtnAdr
    jr    c,.next2                ; If No collision
    djnz  .loop2                  ;   Check next spritle in Sprite 2
.next2
    pop   hl                      ; HL = SptDef2; Stack = SptPtr1, SptCnt1+SprLen2, RtnAdr
    pop   bc                      ; B = SprLen2, C = SprCnt1; Stack = RtnAdr
    ret   c                       ; Return Carry Set if Collision
    dec   c
    jr    nz,.loop1               ; Do next spritle in Sprite 1
    ret
    
;-----------------------------------------------------------------------------
; Detect collision between two spritles
; Input: B = Spritle1
;        C = Spritle2
; Returns: Carry Set if Collision Detected
; Clobbered: AF, BC, DE, HL
;-----------------------------------------------------------------------------
spritle_collision:
    push    bc                    ; Stack = Spritle1, RtnAdr
    ld      a,c                   ; A = Spritle2
    call    spritle_get_pos       ; BC = Xpos2, DE = Ypos2, HL = Height2
    jp      nc,POPHRT             ; If disabled, return Carry Clear
    exx                           ; BC' = Xpos2, DE' = Ypos2
    pop     af                    ; A = Spritle1; Stack = RtnAdr
    call    spritle_get_pos       ; BC = Xpos1, DE = Ypos1, HL = Height1
    ret     nc                    ; If disabled, return Carry Clear
    call    _check_y              ; BC = Xpos2, DE = Ypos2, BC' = Xpos1, DE' = Ypos1
    ret     nc                    ; Return Carry Clear if Ypos2 >= Ypos1 + 8 
    call    _check_y              ; BC = Xpos2, DE = Ypos2, BC' = Xpos1, DE' = Ypos1
    ret     nc                    ; Return Carry Clear if Xpos1 >= Xpos2 + 8 
    ld      hl,8                  ; SptWid = 8
    call    _check_x              ; BC = Xpos2, DE = Ypos2, BC' = Xpos1, DE' = Ypos1
    ret     nc                    ; Return Carry Clear if Xpos2 >= Xpos1 + 8 
    call    _check_x              ; BC = Xpos1, DE = Ypos1, BC' = Xpos2, DE' = Ypos2, 
    ret                           ; Return Carry Clear if Ypos1 >= Ypos2 + 8 

; Input: BC = Xpos1, DE = Ypos1, HL = SprHgt1, BC' = Xpos2, DE' = Ypos2, HL' = SprHgt2
_check_x:
    push    hl                    ; Stack = Height1, RtnAdr
    add     hl,bc                 ; HL = Xpos1 + 8
    ex      (sp),hl               ; HL = Height1; Stack = Xpos1 + 8, RtnAdr
    exx                           ; BC = Xpos2, DE = Ypos2, BC' = Xpos1, DE' = Ypos1, HL' = Height1
    ex      (sp),hl               ; HL = Xpos1 + 8; Stack = Height2, RtnAdr
    push    de                    ; Stack = Ypos1, Height2, RtnAdr
    ex      de,hl                 ; DE = Xpos1 + 8
    ld      h,b
    ld      l,c                   ; DE = Xpos2
    rst     COMPAR                ; Clear Carry if Xpos2 >= Xpos1 + 8 
    pop     de                    ; DE = Ypos2; Stack = Height2, RtnAdr
    pop     hl                    ; HL = Height2; Stack = RtnAdr
    ret

_check_y:
    push    hl                    ; Stack = Height1, RtnAdr
    add     hl,de                 ; HL = Ypos1 + Height1
    ex      (sp),hl               ; HL = Height1; Stack = Ypos1 + Height1, RtnAdr
    exx                           ; BC = Xpos2, DE = Ypos2, BC' = Xpos1, DE' = Ypos1
    ex      (sp),hl               ; HL = Ypos1 + Height; Stack = Height2, RtnAdr
    ex      de,hl                 ; HL = Ypos2, DE = Ypos1 + 8
    rst     COMPAR                ; Clear Carry if Ypos2 >= Ypos1 + 8 
    ex      de,hl                 ; DE = Ypos2
    pop     hl                    ; HL = Height2
    ret

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; sprite_define - Define Sprite
; Builds spritedef from spritle list
; Spritedef must be three bytes longer thst spritle list
; Input: BC: Spritle list length
;        DE: Spritle list address
;        HL: Spritdef address
; Output: C: Spritle count
;         D: Sprite width
;         E: Sprite Height
; Clobbered: A, BC
; Flags: Carry set if invalid spritle list
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sprite_define:
    ld      a,c
    or      a
    jr      z,_retcs
    push    bc                    ; Stack = LstLen, RtnAdr
    push    hl                    ; Stack = DefAdr, LstLen, RtnAdr
    push    bc                    ; Stack = LstLen, DefAdr, LstLen, RtnAdr
    ex      de,hl                 ; HL = LstAdr, DE = DefAdr
    inc     de
    inc     de
    inc     de                    ; Skip past spritedef header
    push    de                    ; Stack = LstPtr
    ldir                          ; Copy spritle list to spritedef
    pop     hl                    ; HL = LstPtr`; Stack = LstLen, DefAdr, LstLen, RtnAdr
    pop     bc                    ; BC = LstLen; Stack = DefAdr, LstLen, RtnAdr
    ld      de,0                  ; SprWid = 0, SprHgt = 0
    ld      b,c                   ; B = DefLen
    inc     b                     ; Bump for decrement
    ld      c,e                   ; SptCnt = 0
.loop
    call    .get                  ; A = SptNum
    jr      z,.done
    cp      63                    ; If SptNum > 63
    jr      nc,.error             ;   Return Carry set
    call    .get                  ; A = Xofs
    jr      z,.error
    add     8                     ; If Xend >255
    jr      c,.error              ;   Return Carry set
    cp      d
    jr      c,.yofs               ; Xend > SprWid
    ld      d,a                   ; SprWid = Xend
.yofs
    call    .get                  ; Y = Xofs
    jr      z,.error
    add     8                     ; If Yend >255
    jr      c,.error              ;   Return Carry set
    cp      e
    jr      c,.next               ; If Yend > SprHgt
    ld      e,a                   ; SprHgt = Yend
.next
    inc     c                     ; SptCnt += 1
    jr      .loop
.done
    pop     hl                    ; HL = DefAdr; Stack = LstLen, RtnAdr
    ld      (hl),c                ; Write SptCnt
    inc     hl
    ld      (hl),d                ; Write SprWid
    inc     hl
    ld      (hl),e                ; Write SprHgt
    dec     hl
    dec     hl                    ; HL = DefAdr
    pop     bc                    ; BC = LstLen, RtnAdr
    inc     bc
    inc     bc
    inc     bc                    ; BC = DefLen
    ret
.get
    ld      a,(hl)
.skip
    inc     hl                    ; Bump DefPtr
    dec     b                     ; Count down
    ret
.error
    pop     hl                    ;   Stack = DefAdr, RtnAdr
_popretcs:
    pop     hl                    ;   Stack = RtnAdr
_retcs:
    scf                           ;   Return Carry Set
    ret

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; sprite_defrect - Define Rectsngular Sprite
; Spritedef length = (SpCols + SpRows) * 3 + 3
; Input:  A: Start Spritle
;         C: Sprite Columns
;         E: Sprite Rows
;        HL: Spritedef address
; Output: A: Spritle count
;        BC: Spritedef length
;         D: Sprite height
;         E: Sprite width
; Clobbered: B, IY
; Flags: Carry set if overflow
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sprite_defrect:
    ld      d,a                   ; D = SptNum
    ld      b,e                   ; B = SpRows
    ld      a,b                   ; A = SpRows
    or      a                     ; If SpRows = 0
    jr      z,_retcs              ;   Eeturn Carry set
    ld      a,c                   ; A = SpCols
    or      a                     ; If SpCols = 0
    jr      z,_retcs              ;   Eeturn Carry set
.mult
    dec     b                     ;
    jr      z,.mdone
    add     a,c                   ; A = SpRows * SpCols
    jr      .mult
.mdone
    push    af                    ; Stack = SptCnt, RtnAdr
    add     d                     ; A = LstSpt
    cp      64                    ; If LstSpr > 63
    jr      nc,_popretcs          ;   Return Carry set
    push    hl                    ; Stack = DefAdr, SptCnt, RtnAdr
    inc     hl
    inc     hl
    inc     hl                    ; Skip header
    push    hl
    pop     iy                    ; IY = DefPtr
    ld      h,d                   ; H = SptNum
    ld      l,c                   ; L = SpCols
    ld      b,e                   ; B = SpRows
    ld      de,0                  ; Yofs = 0, Xofs = 0
; A = SptNum, IXH = SptNum, IXL = SpCols, IY = DefPtr
.rows
    ld      c,l                   ; C = SpCols
    ld      e,0                   ; Xofs = 0
.cols
    ld      a,h                   ; A = SptNum
    inc     h                     ; Bump SptNum
    call    .writedef             ; SptNum -> SprDef
    ld      a,e                   ; A = Xofs
    call    .writedef             ; Xofs -> SprDef
    add     a,8
    ld      e,a                   ; Xofs += 8
    ld      a,d                   ; A = Yofs
    call    .writedef             ; Yofs -> SprDef
    dec     c
    jr      nz,.cols              ; Do next column
    add     8
    ld      d,a                   ; Yofs += 8
    djnz    .rows
    pop     hl                    ; HL = DefAdr; Stack = SptCnt, RtnAdr
    pop     af                    ; A = SptCnt; Stack = RtnAdr
    ld      b,a                   ; B = SptCnt
    add     a,a
    add     a,b
    add     3                     ; DefLen = SptCnt * 3 + 3
    ld      c,a                   ; C = DefLen
    ld      a,b                   ; A = SptCnt
    ld      b,0                   ; BC = DefLen
    ld      (hl),a                ; SptCnt -> SprDef
    inc     hl
    ld      (hl),e                ; Width -> SprDef
    inc     hl
    ld      (hl),d                ; Height -> SprDef
    dec     hl
    dec     hl                    ; HL = DefAdr
    ret

.writedef
    ld      (iy+0),a
    inc     iy                    ; Bump DefPtr
    ret

;-----------------------------------------------------------------------------
; Get sprite position
;  Input: HL: SpriteDef Address
; Output: BC: X-position
;         DE: Y-position
; Clobbered: A
;-----------------------------------------------------------------------------
sprite_get_pos:
    inc     hl                    ; Skip SptCnt
    inc     hl                    ; Skip SprWid
    inc     hl                    ; Skip SprHgt
    ld      a,(hl)                ; A = SptNum
    call    spritle_get_pos       ; BC = SptXpos. DE = SptYpos
    inc     hl                    ; Skip SptNum
    push    hl                    ; Stack = SprPtr, RtnAdr
    push    bc                    ; Stack = SptXpos, SprPtr, RtnAdr
    xor     a                     ; Clear carry
    ld      b,a
    ld      c,(hl)                ; BC = SptXofs
    pop     hl                    ; HL = SptXpos; Stack = SprPtr, RtnAdr
    sbc     hl,bc                 ; HL = SprXpos
    ld      b,h
    ld      c,l                   ; BC = SprXpos
    pop     hl                    ; HL = SprPtr; Stack = RtnAdr
    inc     hl                    ; Skip SptXofs
    xor     a                     ; Clear carry
    ld      l,(hl)
    ld      h,a                   ; HL = SptYofs        
    ex      de,hl                 ; HL = SptYpos, DE = SptYofs
    sbc     hl,de                 ; HL = SprYpos
    ex      de,hl                 ; DE = SprYofs
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
; Input: A: spritle#  0-63
; Output: BC: X-position
;         DE: Y-position
;         HL: Spritle Height
; Flags: Carry Set if enabled
;-----------------------------------------------------------------------------
spritle_get_pos:
    out   (IO_VSPRSEL),a          ; Select sprite
    push  af                      ; Stack = Spritle#, RtnAdr
    in    a,(IO_VSPRX_L)          ; Get X-position
    ld    c,a
    in    a,(IO_VSPRX_H)
    ld    b,a
    in    a,(IO_VSPRY)            ; Get Y-position
    ld    e,a
    ld    d,0
    ld    hl,8
    in    a,(IO_VSPRATTR)
    bit   3,a
    jr    z,.single_height
    add   hl,hl
.single_height
    rla                           ; Carry = Enabled Bit
    ex    (sp),hl                 ; H = SptNum; Stack = SptHgt, RtnAdr
    ld    a,h                     ; A = SptNum
    pop   hl                      ; HL = SptHgt; Stack = RtnAdr
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
    ld      a,(hl)                ; SptCnt
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
    call    spritle_string_attrs  ; DE = New BufAdr
    pop     hl                    ; HL = SprAdr; Stack = SprCnt, RtnAdr
    pop     bc                    ; BC = SprCnt; Stack = RtnAdr
    djnz    .loop
    xor     a                     ; Set Zero Flag - No Errors
    ret

; Input: A:SptNum, DE: BufAdr
spritle_string_attrs:
    pop     ix                    ; IX = RtnAdr
    push    de                    ; Stack = BufAdr
    call    spritle_get_attrs     ; BC = X-position; DE = Y-position, HL = Tile+Attrs
    ex      (sp),hl               ; HL = BufAdr; Stack = Tile+Attrs
    ld      (hl),c
    inc     hl
    ld      (hl),b                ; X-position into Buffer
    inc     hl
    ld      (hl),e                ; Y-position into Buffer
    inc     hl
    pop     de                    ; DE = Tile+attrs
    ld      (hl),e
    inc     hl
    ld      (hl),d                ; Tile+Addrs into Buffer
    inc     hl
    ex      de,hl                 ; DE = BufAdr
    jp      (ix)

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

; Input C: DefLen, HL: DefAdr; Output: B: SptCnt; Clobbers: A
_check_sprite_length:
    ld      b,(hl)                ; B = SptCnt
    ld      a,b                   ; A = SptCnt
    add     a,a                   ; A = SptCnt * 2
    add     b                     ; A = SptCnt * 3
    add     3                     ; Add 3 for header
    cp      c                     ; If equal to StrLen
    ret     z                     ;   Return Carry clear
    scf                           ; Else
    ret                           ;   Return Carry set
  
;-----------------------------------------------------------------------------
; Reset spritles in spritedef to system boot vakues
; Input: C: SprLen, DE: SprAdr
; Clobbers: A,C
;-----------------------------------------------------------------------------
sprite_reset:
    ex      de,hl                 ; HL = SprDef
    call    _check_sprite_length  ; B = SptCnt
    ret     c                     ; If bad length, return Carry set
    inc     hl                    ; Skip SptCnt
.loop
    inc     hl                    ; Skip X-Offset
    inc     hl                    ; Skip Y-Offset
    ld      a,(hl)                ; A = SptNum
    call    spritle_reset         ; Reset it
    djnz    .loop                 ; Do the next one
    or      a                     ; Return Carry clear
    ret

;-----------------------------------------------------------------------------
; Reset spritle to system boot vakues
; Clobbers: A,C
;-----------------------------------------------------------------------------
spritle_reset_all:
    xor     a
.loop
    call  spritle_reset
    inc     a
    cp      64
    jp      c,.loop
    ret

;-----------------------------------------------------------------------------
; Reset spritle to system boot vakues
; Input: A: sprite #  0-63
; Clobbers: C
;-----------------------------------------------------------------------------
spritle_reset:
    ld      c,IO_VSPRSEL
    out     (c),a                 ; Select Spritle A
    ex      af,af'
    xor     a                     ; A = 0
    out     (IO_VSPRX_L),a        ; Reset X-position
    out     (IO_VSPRX_H),a
    out     (IO_VSPRY),a          ; Reset Y-Position
    out     (IO_VSPRIDX),a        ; Reset tile index LSB
    out     (IO_VSPRATTR),a       ; Rext tile MSB and properties
    ex      af,af'
    ret

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

;-----------------------------------------------------------------------------
; Set Sprite Attributes
; Input:  C: Palette List Length
;        DE: Palette List Address
;        HL: SpriteDef Address
; Output: Not Zero if tile count <> spritle count
; Clobbered: A,BC,DE
;-----------------------------------------------------------------------------
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
    call    JUMPIX
    inc     hl                    ; AtrAdr += 1
    djnz    .loop
.error
    pop     hl                    ; HL = SprAdr; Stack = RtnAdr
    ret

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
    and   ~$30                    ; Keep attributes and tile index msb
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
; Set Spritle Properties
; Input: A: SptNum 
;        DE: Props
;-----------------------------------------------------------------------------
spritle_set_props:
    out     (IO_VSPRSEL),a        ; Select sprite
    ex      af,af'
    ld      a,e                   ; Get Tile Index LSB
    out     (IO_VSPRIDX),a        ; and Write it
    ld      a,d                   ; Get Attributes
    out     (IO_VSPRATTR),a       ; and write them
    ex      af,af'
    ret

;-----------------------------------------------------------------------------
; Set Spritle Properties
;   Input: A: SptNum 
; Output: DE: Props
;-----------------------------------------------------------------------------
spritle_get_props:
    out     (IO_VSPRSEL),a        ; Select sprite
    ex      af,af'
    in      a,(IO_VSPRIDX)
    ld      e,a                   ; E = Tile Index LSB
    in      a,(IO_VSPRATTR)
    ld      e,a                   ; D = Attributes
    ex      af,af'
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
spritle_clear_all:
    ld      c,0
spritle_toggle_all:
    ld      a,63
spritle_toggle_a2z:
    call    spritle_toggle
    dec     a
    ret     m 
    jr      spritle_toggle_a2z


