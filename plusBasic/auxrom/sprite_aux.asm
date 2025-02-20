;=============================================================================
; Sprite and Spritle Assembly Routines
;=============================================================================

;; ToDo: Move routines from sprite.asm (EXTROM) to here (AUXROM), changing BASIC commands and functions to match

mspritex:

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
    add     a,a                   ; A = SpRows * SpCols
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
; A = SptNum, IXH = SptMun, IXL = SpCols, IY = DefPtr
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

;-----------------------------------------------------------------------------
; Rese6 spritle to system boot vakues
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
; Rese6 spritle to system boot vakues
; Input: A: sprite #  0-63
; Clobbers: C
;-----------------------------------------------------------------------------
spritle_reset:
    ld      c,IO_VSPRSEL
    out     (c),a                 ; Select Spritle A
    ex      af,af'
    out     (IO_VSPRX_L),a        ; Reset X-position
    out     (IO_VSPRX_H),a
    out     (IO_VSPRY),a          ; Reset Y-Position
    out     (IO_VSPRIDX),a        ; Reset tile index LSB
    out     (IO_VSPRATTR),a       ; Rext tile MSB and properties
    ex      af,af'
    ret


    msize_sprite_aux = $ - mspritex