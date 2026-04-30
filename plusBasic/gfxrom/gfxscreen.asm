;=============================================================================
; Text Screen Graphics Assembly Routines
;=============================================================================

;-----------------------------------------------------------------------------
; Fill Color RAM Rectangle with Colors
; Input: A: Screen# (0 = Current)
;       BC: Start Column
;       DE: Start Row
;      BC': End Column
;      DE': End Row
;        H: Reserved (Set to 0)
;        L: Combined Colors
; Flags: Sign set if illegal Screen#
;        Carry set if coordinates out of range
; Clobbered: All
;-----------------------------------------------------------------------------
color__fill:
    call    _setup_fill           ; IXH = AndMsk, IXL = Colors
    ret     c                     ;   Return Carry if Screen# <> 0
do_color_fill:
    ld      iy,$3400              ; BasAdr = Color RAM
    cp      40                    ; If ScrWid = 40
    jr      z,fill_rect           ;   Do the fill and Return
    ex      af,af'                ; A' = ScrWid
    in      a,(IO_VCTRL)          ; A = VCTRL
    push    af                    ; Stack = VCTRL, RtnAdr
    or      VCTRL_TEXT_PAGE
    out     (IO_VCTRL),a          ; Switch to Color RAM Page
    ld      iy,$3000              ; BasAdr = Screen RAM
    ex      af,af'                ; A = ScrWid
    call    fill_rect             ; Do the fill
    pop     a                     ; A = Original VCTRL, Stack = RtnAdr
    out     (IO_VCTRL),a          ; Restore VCTRL
    ret

;----------------------------------------------------------------------------
; Fill Screen RAM Rectangle with Character
; Input: A: Screen# (0 = Current)
;       BC: Start Column
;       DE: Start Row
;      BC': End Column
;      DE': End Row
;        L: Character
; Flags: Sign set if illegal Screen#
;        Carry set if coordinates out of range
; Clobbered: All
;-----------------------------------------------------------------------------
screen__fill:
    call    _setup_fill           ; A = ScrWid, IXH = AndMsk, IXL = Colors
    ret     c                     ;   Return Carry if Screen# <> 0
    ld      iy,$3000              ; ScrOfs = Screen RAM
; This is the master Screen RAM, Color RAM, and 1bpp Colormap fill routine
; The calling routine must set up any bank switching ahead of time
; Input: A: Screen Width
;        IXL: Fill Byte, IXH: AND Mask, IY: Base Address
;        BC, DE, BC', DE': Rectangle coordinates
fill_rect:
    call    screen__convert_rect  ; B = RctWid, C = RctHgt, DE = ScrWid, HL = ScrOfs
    ret     c                     ; Return Carry Set if invalid coordinates
    push    de                    ; Stack = ScrWid, RtnAdr
    push    iy                    ; Stack = BasAdr, ScrWid, RtnAdr
    pop     de                    ; DE = BasAdr; Stack = ScrWid, RtnAdr
    add     hl,de                 ; HL = ScrAdr
    ex      de,hl                 ; DE = ScrAdr
    pop     hl                    ; HL = SrcWid; Stack = RtnAdr
    push    ix                    ; Stack = Mask+Byte, RtnAdr
.vloop
    ex      (sp),hl               ; HL = Mask+Byte; Stack = ScrWid, RtnAdr
    push    bc                    ; Stack = RowWid+RowCnt, ScrWid, RtnAdr
    push    de                    ; Stack = RowAdr, RctWid+RowCnt, ScrWid, RtnAdr
.hloop
    ld      a,(de)
    and     h
    or      l
    ld      (de),a                ; Update Cell
    inc     de
    djnz    .hloop
    pop     de                    ; DE = ScrAdr; Stack = RctWid+RowCnt, ScrWid, RtnAdr
    pop     bc                    ; B = ColCnt, C = RowCnt; Stack = ScrWid, RtnAdr
    ex      (sp),hl               ; HL = ScrWid; Stack = Mask+Byte, RtnAdr
    ex      de,hl                 ; DE = ScrWid, HL = ScrAdr
    add     hl,de                 ; HL = NewAdr
    ex      de,hl                 ; DE = NewAdr, HL = ScrWid
    dec     c
    jr      nz,.vloop
.popret
    pop     hl                    ; Stack = RtnAdr
    ret

_setup_fill:
    ld      h,0                   ; AndMsk = 0
    push    hl
    pop     ix                    ; IXH = AndMsk, IXL = Colors
_screen_num:
    or      a                     ; If ScrnNo = 0
    ld      a,(LINLEN)            ;   A = ScrWid
    ret     z                     ;   Return Positive
    or      $FF                   ; Else
    ret                           ;   Returns Negative
    
;-----------------------------------------------------------------------------
; Draw Rectangle on Text Screen
; Input: A: Screen# (0 = Current)
;       BC: Start Column
;       DE: Start Row
;      BC': End Column
;      DE': End Row
;       HL: DrawChars Address
;      HL': DrawColors Address
; Flags: Sign set if illegal Screen#
;        Carry set if coordinates out of range
; Clobbered: All
;--------------`---------------------------------------------------------------
screen__rect:
    call    _screen_num           ; A = ScrWid
    ret     m
    exx                           ; HL = DrwClrs
    push    hl                    ; Stack = DrwClrs, RtnAdr
    exx
    push    hl                    ; Stack = DrwChrs, DrwClrs, RtnAdr
    call    .convert_rect         ; B = ColCnt-2, C = RowCnt-2, DE = ScrWid, HL = RowAdr
    pop     ix                    ; DE = ChrAdr; Stack = DrwClrs, RtnAdr
    jp      c,POPHRT              ; Discard DrwClrs and Return if error
    push    bc                    ; Stack = Counts, DrwClrs, RtnAdr
    push    hl                    ; Stack = RowAdr, Counts, DrwClrs, RtnAdr
    call    .do_rect              ; Draw Rectangle Characters
    pop     hl                    ; HL = RowAdr, Stack = Counts, DrwClrs, RtnAdr
    pop     bc                    ; BC = Counts; Stack = DrwClrs, RtnAdr
    pop     ix                    ; IX = DrwClrs; Stack = RtnAdr
    ld      a,e                   ; A = ScrWid
    cp      40
    jr      z,.do40               ; If 80 columns
    in      a,(IO_VCTRL)
    push    af
    or      VCTRL_TEXT_PAGE
    out     (IO_VCTRL),a          ;   Switch to Color RAM
    call    .do_rect
    pop     af
    out     (IO_VCTRL),a          ;   Restore VCTRL
    ret
.do40
    ld      a,h
    add     a,$04
    ld      h,a                   ; ScrAdr += $4000
.do_rect
    call    .doline               ; Draw top line
    inc     ix
    inc     ix
    inc     ix
.dolines
    call    .doline               ; Draw middle lines
    dec     c
    jr      nz,.dolines
    inc     ix
    inc     ix
    inc     ix                    ; Then draw bottom line
.doline:
    push    bc                    ; Stack = RowCnt+ColCnt, RtnAdr
    push    hl                    ; Stack = RowAdr, RowCnt+ColCnt, RtnAdr
    ld      a,(ix+0)              ; A = UpperLeft
    ld      (hl),a                ; Write to screen
    inc     hl
    ld      a,(ix+1)              ; A = UpperMiddle
.lineloop
    ld      (hl),a
    inc     hl
    djnz    .lineloop
    ld      a,(ix+2)              ; A = UpperRight
    ld      (hl),a                ; Write to screen
    pop     hl                    ; HL = RowAdr; Stack = RowCnt+ColCnt, RtnAdr
    add     hl,de                 ; HL = Next RowAdr
    pop     bc                    ; BC = RowCnt+ColCnt, Stack = RtnAdr
    ret


.convert_rect
    call    screen__convert_rect  ; B = ColCnt, C = RowCnt, DE = ScrWid, HL = ScrAdr
    ret     c
    push    de
    ld      de,SCREEN
    add     hl,de
    ld      a,b                   ; A = ColCnt
    cp      3                     ; If ColCnt < 3
    ret     c                     ;   Return Carry Set
    dec     a
    dec     a
    ld      b,a                   ; B = ColCnt-2
    ld      a,c                   ; A = RowCnt
    cp      3                     ; Set Carry if RowCnt < 3
    dec     a
    dec     a
    ld      c,a                   ; C = RowCnt-2
    pop     de                    ; IX = ScrWid
    ret

;-----------------------------------------------------------------------------
; Convert Screen Coordinates to Size and Start Address
; Input: A: Screen Width
;       BC: Start X
;       DE: Start Y
;      BC': End X
;      DE': End Y
; Output: B = Rect Width
;         C = Rect Height
;        DE = Screen Width
;        HL = Start Offset
; Clobbers: All
;  Flags: Carry set if out of bounds
;-----------------------------------------------------------------------------
screen__convert_rect:
    ld      l,a
    ld      h,0                   ; HL = ScrWid
    push    hl                    ; Stack = ScrWid, RtnAdr
    inc     de                    ; Bump StartY
    exx
    inc     de                    ; Bump EndY
    ld      hl,25                 ; ScrHgt = 25
    exx       
    call    gfx_rect_size         ; BC' = RctWid, DE' = RctHgt
    pop     hl                    ; HL = ScrWid, Stack = RtnAdr
    ret     c                     ;   Return Carry if Coords invalid
    push    hl                    ; Stack = ScrWid, RtnAdr
    ld      a,l                   ; A = ScrWid
    call    mult_a_de             ; HL = RowAdr
    add     hl,bc                 ; Hl = ScrOfs
    exx                           ; BC = RctWid, DE = RctHgt, HL' = ScrOfs
    ld      b,c                   ; B = RctWid
    ld      c,e                   ; C = RctHgt
    push    bc                    ; Stack = WidHgt, ScrWid,RtnAdr
    exx                           ; HL = ScrOfs
    pop     bc                    ; BC = WidHgt; Stack = ScrWid, RtnAdr
    pop     de                    ; DE = ScrWid; Stack = RtnAdr
    ret

screen__colors:
    ld      a,(SCREENCTL)         ; 
    rla                           ; Carry = SCRCOLOR
    ld      a,DFLTATTRS
    ret     nc
    ld      a,(SCOLOR)
    ret

; Set RECT_
; Set RECT_X1, RECT_Y1, RECT_X2, RECT_Y2 to full screen
screen__size:
    ld      bc,0
    ld      (RECT_X1),bc          ; Start Column = 0
    ld      (RECT_Y1),bc          ; Start Row = 0
    ld      bc,(LINLEN)
    ld      b,0
    dec     bc
    ld      (RECT_X2),bc          ; End Column = LinLen -1
    ld      bc,23
    ld      (RECT_Y2),bc          ; End Row = 23
    ret
