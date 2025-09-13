;-----------------------------------------------------------------------------
; Initialize screen buffers
; Called from _coldboot
; Clobbers: AF,BC,DE,HL
;-----------------------------------------------------------------------------
init_screen_buffers:
    call    _map_screen_buff

    ld      a,' '
    ld      hl,BANK1_BASE+SCRN40BUF
    ld      bc,1024
    call    sys_fill_mem
    ld      hl,BANK1_BASE+SCRN41BUF
    ld      bc,1024
    call    sys_fill_mem
    ld      hl,BANK1_BASE+SCRN80BUF
    ld      bc,2048
    call    sys_fill_mem
    ld      hl,BANK1_BASE+SCRN40SWP
    ld      bc,1024
    call    sys_fill_mem
    ld      hl,BANK1_BASE+SCRN41SWP
    ld      bc,1024
    call    sys_fill_mem
    ld      hl,BANK1_BASE+SCRN80SWP
    ld      bc,2048
    call    sys_fill_mem

    ld      a,6
    ld      hl,BANK1_BASE+SCRN40BUF+1024
    ld      bc,1024
    call    sys_fill_mem
    ld      hl,BANK1_BASE+SCRN41BUF+1024
    ld      bc,1024
    call    sys_fill_mem
    ld      hl,BANK1_BASE+SCRN80BUF+2048
    ld      bc,2048
    call    sys_fill_mem
    ld      hl,BANK1_BASE+SCRN40SWP+1024
    ld      bc,1024
    call    sys_fill_mem
    ld      hl,BANK1_BASE+SCRN41SWP+1024
    ld      bc,1024
    call    sys_fill_mem
    ld      hl,BANK1_BASE+SCRN80SWP+2048
    ld      bc,2048
    call    sys_fill_mem

    jp      page_restore_bank1

;-----------------------------------------------------------------------------
; Initialize screen variable buffers
; Called from _coldboot
; Copies current screen control sysvars into buffers
; Output: HL: Address after end of last buffer
; Clobbers: A,B,DE
;-----------------------------------------------------------------------------
init_screen_vars:
    call    _map_screen_vars
    ld      hl,BANK1_BASE         ; Start at BUFSCRN40
    call    .init_buffs
    ld      l,SWPSCRN40
    call    .init_buffs
    call    .init_palettes
    jp      page_restore_bank1
.init_buffs
    ld      de,SCREEN + 41        ; CURRAM
    call    .init_buff            ; xxxSCRN40
    call    .init_buff            ; xxxSCRN41
    ld      e,81
; DE = CURRAM, HL = Buffer Address
.init_buff:
    xor     a                     ; TTYPOS = 0
    ld      (hl),a
    inc     hl

    ld      a,' '                 ; CURCHR = ' '
    ld      (hl),a
    inc     hl

    ld      (hl),e                ; CURRAM = DE
    inc     hl
    ld      (hl),d
    inc     hl

    xor     a                     ; BASYSCTL = 0
    ld      (hl),a
    inc     hl

    ld      (hl),a                ; IO_VCTRL = 0
    inc     hl

    ld      a,DFLTATTRS           ; Default Screen Colors
    ld      (hl),a                ; SCOLORS = Default
    inc     hl

    ld      (hl),a                ; SCREENCTL = 0
    inc     hl                    
    ret

; Loop 5 times then fall into .init_palette to do the 6th
.init_palettes
    ld    de,BANK1_BASE+BUFPALT40
    ld    a,5
.loop
    call  .init_palette
    dec   a
    jr    nz,.loop
.init_palette
    ld    hl,default_palette
    ld    bc,32
    ldir
    ret

;-----------------------------------------------------------------------------
; Reset Screen to Text only and default palette
; Clobbers: A,BC,D,HL
;-----------------------------------------------------------------------------
screen_reset:
    ld      a,VCTRL_TEXT_EN
    out     (IO_VCTRL),a
    call    bitmap_set_mode_nobuff
    xor     a
    jp      palette_reset

;-----------------------------------------------------------------------------
; Expand Convert IO_VCTRL to screen status
; Output: A,L: Bit 7-6: Text Mode 5: Border; 4: Priority; 3: Sprites; 2-1: Graphics
;           H: (IO_VCTRL)
;-----------------------------------------------------------------------------
screen_status:
    ld      bc,0
    ld      de,0
    in      a,(IO_VCTRL)
    ld      h,a                   ; H = (IO_VCTRL)
    ld      l,0
    bit     0,h                   ; Test VCTRL_TEXT_EN
    jr      z,.skip               ; If text enabled
    ld      l,192
    bit     6,h                   ;   Test VCRTL_80COL_EN
    jr      nz,.skip              ;   If not in 80 column mode
    ld      l,128
    bit     7,h                   ;     Test VCTRL_TEXT_PAGE
    jr      nz,.skip              ;     If in screen page 1
    ld      l,64
.skip
    ld      a,h                   ; A = IO_VCTRL
    and     VCTRL_TEXT_MASK       ; Strip text screen bits
    or      l                     ; Or in text mode
    ld      l,a                   ; L = Screen status
    ret

screen_swap_vars:
    push    de                    ; Stack = SwapBufAdr
    push    hl                    ; Stack = VarBufBase, SwapBufAdr, RtnAdr
    ex      de,hl                 ; HL = SwapBufAdr
    call    screen_stash_vars     ; Stash to SwapVarBuf
    pop     de                    ; DE = VarBufBase; Stack = SwapBufAdr, RtnAdr
    call    screen_swap_palette
    ex      de,hl                 ; HL = VarBufBase
    call    _svar_buff_addr       ; HL = VarBufAdr, A = CurSCRCHRSET
    push    hl                    ; Stack = VarBufAdr, SwapBufAdr, RtnAdr
    push    af                    ; Stack = CurSCRCHRSET, VarBufAdr, SwapBufAdr, RtnAdr
    call    screen_restore_vars   ; A = OrgSCRCHRSET
    pop     bc                    ; B = CurBASCHGSER; Stack = VarBufAdr, SwapBufAdr, RtnAdr
    cp      b                     ; If OrgSCRCHRSET <> CurBASCHGSER
    call    nz,_select_chrset     ;   Switch to buffered character set
    pop     de                    ; DE = VarBufAdr; Stack = SwapBufAdr, RtnAdr
    pop     hl                    ; HL = SwapBufAdr; Stack = RtnAdr
    push    de                    ; Stack = VarBufAdr, RtnAdr
    ld      bc,SVBUFLEN
    ldir                          ; SwapVarBuf to VarBuf
    pop     de                    ; DE = VarBufAdr
    ret

;-----------------------------------------------------------------------------
; Copy Screen Buffer to Text Screen
; Called from ST_RESTORE_SCREEN, *ST_SCREEN
; Input: L: BUFSCRN40 or SWPSCRN40
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
screen_restore:
    call    _map_screen_vars      ; Stack = SavePg, RtnAdr
    call    _restore_vars         ; DE = BufAdr, BC = BufLen
    push    af                    ; Stack = 80Cols, SavePg, RtnAdr
    ex      de,hl
    ldir
    pop     af                    ; AF = 80cols; Stack = SavePg, RtnAdr
    jp      z,page_restore_bank1  ; If 80-columns
    ld      de,SCREEN
    ld      bc,2048
    jr      _copy_color

_restore_vars:
    call    _svar_buff_addr       ; HL = BufAdr, A = CurSCRCHRSET
    push    hl                    ; Stack = ScrBuf, RtnAdr
    push    af                    ; Stack = CurSCRCHRSET, SavePg, RtnAdr
    call    screen_restore_vars   ; A = OrgSCRCHRSET
    pop     bc                    ; B = CurBASCHGSER
    cp      b                     ; If OrgSCRCHRSET <> CurBASCHGSER
    call    nz,_select_chrset     ;   Switch to buffered character set
    pop     de                    ; DE = ScrBuf
    call    screen_restore_palette
    jp     _scrn_buff_addr        ; DE = BufAdr, BC = BufLen

;-----------------------------------------------------------------------------
; Copy Text Screen to Screen Buffer
; Called from ST_STASH_SCREEN, *ST_SCREEN
; Input: L: Cursor buffer offset: BUFSCRN40 or SWPSCRN40
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
screen_stash:
    call    _map_screen_vars      ; Stack = SavePg, RtnAdr
    call    _svar_buff_addr       ; HL = BufAdr
    push    hl                    ; Stack = VarBufAdr, SavePg, RtnAdr
    call    screen_stash_vars     ;
    pop     de                    ; E = VarBufOfs; Stack = SavePg, RtnAdr
    call    screen_stash_palette  ; DE preserved
    call    _scrn_buff_addr       ; DE = BufAdr, BC = ScrLen, AF = 80cols
    push    af                    ; Stack = 80Cols, SavePg, RtnAdr
    ldir
    pop     af                    ; AF = 80cols; Stack = SavePg, RtnAdr
    jp      z,page_restore_bank1  ; If 80-columns
    ld      hl,SCREEN
    ld      bc,2048
_copy_color:
    in      a,(IO_VCTRL)
    or      VCTRL_TEXT_PAGE
    out     (IO_VCTRL),a          ;   Select Color Page
    ldir                          ;   Do the copy
_seltext_resret:
    and     $FF-VCTRL_TEXT_PAGE
    out     (IO_VCTRL),a          ;   Select Text Page
    jp      page_restore_bank1    ;   Restore page and return

;-----------------------------------------------------------------------------
; Swap screen variables
; Called from ST_STASH_SCREEN, *ST_SCREEN
; Input: L: Cursor buffer offset: BUFSCRN40 or SWPSCRN40
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
screen_swap:
    call    _map_screen_vars      ; Stack = SavePg, RtnAdr
    ld      de,BANK1_BASE+SWPSVARS
    call    screen_swap_vars      ; DE = VarBufAdr
    call    _scrn_buff_addr       ; DE = ScrBufAdr, BC = BufLen
    push    af                    ; Stack = 80Cols, SavePg, RtnAdr
    call    sys_swap_mem          ; Swap screen with buffer
    pop     af                    ; AF = 80cols; Stack = SavePg, RtnAdr
    jp      z,page_restore_bank1  ; If 80-columns
    ld      hl,SCREEN
    ld      bc,2048
    in      a,(IO_VCTRL)
    or      VCTRL_TEXT_PAGE
    out     (IO_VCTRL),a          ;   Select Color Page
    call    sys_swap_mem          ;   Swap color with buffer
    in      a,(IO_VCTRL)
    jr      _seltext_resret

;-----------------------------------------------------------------------------
; Switch Text Screen
; Input: A: Screen # - 1,2 = 40 column, 3 = 80 column
; Output: A: (IO_VCTRL)
; Clobbers: AF',BC,DE,HL
;-----------------------------------------------------------------------------
screen_switch:
    ld      de,.text_mode_table
    call    table_lookup
    ld      b,a                   ; B = VCTRL bits
    in      a,(IO_VCTRL)          ; A = Current VCTRL
    and     VCTRL_TEXT_BITS       ; Isolate text screen bits
    cp      b                     ; If text modes are the same
    ret     z                     ;   Never mind
    push    bc                    ; Stack = VCTRLbits, RtnAdr
    ld      hl,BUFSCRN40
    push    hl                    ; Stack = BufOfs, NewVCTRL
    call    screen_stash          ; Stash current screen to switch buffer
    pop     hl                    ; HL = BufOfs; Stack = NewVCTRL, RtnAdr
    pop     bc                    ; B = VCTRLbits; Stack = RtnAdr
    in      a,(IO_VCTRL)          ; A = Current VCTRL
    and     VCTRL_TEXT_MASK       ; Clear screen control bits
    or      b
    out     (IO_VCTRL),a          ; Switch to new text mode
    call    screen_restore        ; Restore new screen and return
    jp      set_linlen

.text_mode_table:
    byte    VCTRL_TEXT_OFF                  ; 0 = Text Off
    byte    VCTRL_TEXT_EN                   ; 1 = 40 Column Primary
    byte    VCTRL_TEXT_EN+VCTRL_TEXT_PAGE   ; 2 = 40 Column Secondary
    byte    VCTRL_TEXT_EN+VCRTL_80COL_EN    ; 3+ = 80 Column


;-----------------------------------------------------------------------------
; Copy screen variables into buffer
; Input: HL = Buffer Address
; Output: A = SCREENCTL
;      HL: Address of next Buffer
; Clobbers: DE
;-----------------------------------------------------------------------------
screen_restore_vars:
    ld      a,(hl)                ; Offset $0
    inc     hl
    ld      (TTYPOS),a

    ld      a,(hl)                ; Offset $1
    inc     hl
    ld      (CURCHR),a

    ld      e,(hl)                ; Offset $2
    inc     hl
    ld      d,(hl)
    inc     hl
    ld      (CURRAM),de

    inc     hl                    ; Offset 4

    ld      d,(hl)                ; Offset $5
    inc     hl
    in      a,(IO_VCTRL)          ;
    and     $FF-VCTRL_REMAP_BC
    or      d
    out     (IO_VCTRL),a

    ld      a,(hl)                ; Offset $6
    inc     hl
    ld      (SCOLOR),a

    ld      a,(hl)                ; Offset $7
    inc     hl                    ; Next buffer
    ld      (SCREENCTL),a
    and     SCRCHRSET             ; A = ChrSet
    ret

;-----------------------------------------------------------------------------
; Copy screen variables into buffer
; Input: HL = Buffer Address
; Clobbers: A, DE, HL
;-----------------------------------------------------------------------------
screen_stash_vars:
    ld      a,(TTYPOS)
    ld      (hl),a                ; Offset $0
    inc     hl
    ld      a,(CURCHR)
    ld      (hl),a                ; Offset $1
    inc     hl
    ld      de,(CURRAM)
    ld      (hl),e                ; Offset $2
    inc     hl
    ld      (hl),d
    inc     hl
    xor     a
    ld      (hl),a                ; Offset $4
    inc     hl
    in      a,(IO_VCTRL)
    and     VCTRL_REMAP_BC
    ld      (hl),a                ; Offset $5
    inc     hl
    ld      a,(SCOLOR)
    ld      (hl),a                ; Offset $6
    inc     hl                    
    ld      a,(SCREENCTL)
    ld      (hl),a                ; Offset $7
    inc     hl                    ; Next buffer
    ret

; Input: E = VarBufOfs
; Output: DE = ScrBufAdr, BC = ScrLen, AF = 80column
_scrn_buff_addr:
    call    _bank_screen_buff
    ld      hl,SCREEN
    ld      a,high(BANK1_BASE)
    add     e
    ld      d,a
    ld      e,l
    ld      bc,2048
    and     $10                   ; A = $10 if 80-columns
    ret

;-----------------------------------------------------------------------------
; Get screen buffer address
;  Input: L: BUFSCRN40 or SWPSCRN40
; Output: A: SCRCHRSET
;         DE: Buffer offset
;         HL: Buffer Address
;  Flags: Zero set if default chrset
;-----------------------------------------------------------------------------
_svar_buff_addr:
    ld      h,high(BANK1_BASE)
    call    _svar_buff_ofs
    add     hl,de
    ld      a,(SCREENCTL)
    and     SCRCHRSET
    ret

;-----------------------------------------------------------------------------
; Get screen buffer offset
; Force Text Page if 80-column mode
; Output: DE: Screen variable buffer offset
;-----------------------------------------------------------------------------
_svar_buff_ofs:
    ld      d,0
    in      a,(IO_VCTRL)
    rla                           ; Carry = TEXT_PAGE
    rla                           ; Carry = 80COL_EN
    jr      nc,.not80             ; If 80 column mode
    ld      e,BUFSCRN80           ;   E = 80-column offset
    rra                           ;   Carry = TEXT_PAGE
    ret     nc                    ;   If color page
    ccf                           ;     Carry = 0 (Screen Page)
    rra                           ;     A = New IO_VCTRL
    out     (IO_VCTRL),a
    ret                           ; Else
.not80
    rra                           ; Carry = TEXT_PAGE
    ld      e,BUFSCRN41
    ret     c
    ld      e,BUFSCRN40
    ret

;-----------------------------------------------------------------------------
; Copy buffer into palette
; Input: DE = Variable Buffer Address
; Clobbers: A, HL
;-----------------------------------------------------------------------------
screen_restore_palette:
    call    _palt_buff_addr       ; HL = BufAdr
.loop
    ld      a,c
    out     (IO_VPALSEL),a        ; Select palette index
    inc     c
    ld      a,(hl)
    out     (IO_VPALDATA),a
    inc     hl
    djnz    .loop
    ret

;-----------------------------------------------------------------------------
; Copy current palette into buffer
; Input: DE = Variable Buffer Address
; Clobbers: A, HL
;-----------------------------------------------------------------------------
screen_stash_palette:
    call    _palt_buff_addr       ; HL = BufAdr
.loop
    ld      a,c
    out     (IO_VPALSEL),a        ; Select palette index
    inc     c
    in      a,(IO_VPALDATA)
    ld      (hl),a
    inc     hl
    djnz    .loop
    ret

;-----------------------------------------------------------------------------
; Copy current palette into buffer
; Input: DE = Variable Buffer Offset
; Clobbers: A, HL
;-----------------------------------------------------------------------------
screen_swap_palette:
    call    _palt_buff_addr       ; HL = BufAdr
.loop
    ld      a,c
    out     (IO_VPALSEL),a        ; Select palette index
    inc     c
    in      a,(IO_VPALDATA)
    ex      af,af'
    ld      a,(hl)
    out     (IO_VPALDATA),a
    ex      af,af'
    ld      (hl),a
    inc     hl
    djnz    .loop
    ret

;-----------------------------------------------------------------------------
; Copy text color map from temp buffer into Screen RAM
; Input: BC: Source file Length
; Sets flags: Carry if data length does not match screen mode
; Clobbers: AF,AF',BC,DE,HL
; 40 column mode only: 1024 bytes
; 80 column mode only: 2048 bytes
;-----------------------------------------------------------------------------
color_write_tmpbfr:
    ld      iy,_read_tmpbfr
color_read_tmpbfr:
    ld      iy,_read_tmpbfr
_color_tmpbfr:
    call    page_map_tmpbfr
    ld      a,c
    or      a                     ; If FileSize not multiple of 256
    jr      nz,_badfile           ;   Return Carry Set
    in      a,(IO_VCTRL)
    and     VCRTL_80COL_EN        ; Z if 40-column, NZ if 80-column
    ld      a,b
    jr      nz,.col80             ; If 40-column
    cp      $04                   ;   If FileSize <> 1024
    jr      nz,_badfile           ;     Return Carry Set
    ld      de,COLOR              ;   DstAdr = ColorRAM
    jr      _read_tmpbfr
.col80
    cp      $08
    jr      nz,_badfile
    ld      hl,BANK1_BASE         ; SrcAdr = TmpBuf
_color80:
    ld      de,SCREEN             ; DstAdr = Screen
    in      a,(IO_VCTRL)
    push    af
    or      VCTRL_TEXT_PAGE
    out     (IO_VCTRL),a
    ldir
    pop     af
    out     (IO_VCTRL),a
    jr      _done

_write_tmpbfr:
    ex      de,hl
    ld      de,BANK1_base
    jr      _copy
_write_tmpbfr_scrn:
    ld      hl,SCREEN             ; DstAdr = Screen
    ld      de,BANK1_BASE         ; SrcAdr = TmpBuf
    jr      _copy
_read_tmpbfr_scrn:
    ld      de,SCREEN             ; DstAdr = Screen
_read_tmpbfr:
    ld      hl,BANK1_BASE         ; SrcAdr = TmpBuf
_copy:
    ldir                          ;   Copy it
_done:
    xor     a                     ; Clear Carry
    byte    $06                   ; LD B, over SCF
_badfile:
    scf
    jp      page_restore_bank1


;-----------------------------------------------------------------------------
; Copy TMP_BUFFR to Screen RAM , Color RAM, Palette, and IO_VCTRL
; Output: BC = Length of data to copy
; Sets flags: Carry if data length does not match screen mode
; Clobbers: A, AF', BC, DE, HLh
;          ----- 40 Column Mode ------
; Length   SCREEN COLOR PALETTE BORDER    SCREEN COLOR PALETTE BORDER
;  1024        0

;  40 column: 1024 byte Screen RAM + 1024 byte Color RAM {+ 32 byte palette} {+ 1 byte border flag}
;  80 column: 2048 byte Screen RAM + 2048 byte Color RAM {+ 32 byte palette} {+ 1 byte border flag}
;-----------------------------------------------------------------------------
screen_read_tmpbfr:
    call    page_map_tmpbfr
    ld      de,SCREEN             ; DstAdr = Screen
    ld      hl,BANK1_BASE
    add     hl,bc
    dec     hl                    ; HL = EndByte
    in      a,(IO_VCTRL)
    and     VCRTL_80COL_EN        ; A = $40 if 80-column
    ld      a,b
    jr      nz,.col80             ; If 40-column
    cp      $04
    jr      nz,.not1024           ;   If Size = 1024 - 1279
    ld      a,c
    cp      1                     ;   If 1025
    call    z,.remap              ;     Copy remap bit
    jr      .screen               ;   Copy Screen RAM
.not1024
    cp      $08                   ;   If not 2048 - 2033
    jr      nz,_badfile           ;     Return error
    ld      a,c
    cp      33
    call    z,.remap
    cp      32
    call    z,.palette
.check2049
    cp      1
    call    z,.remap
.screen
    or      a                     ;   If Size = 1024
    jr      z,_read_tmpbfr_scrn   ;     Copy to Screen RAM and return
    jr      _badfile              ;   Else return error
.col80
    cp      $08
    jr      nz,.not2048           ; If 2048 - 2303
    ld      a,c
    jr      .check2049            ;   Copy to Screen RAM anf return
.not2048
    cp      $10                   ; If not 4096 - 4361
    jr      nz,_badfile           ;   Return error
    ld      a,c
    cp      33
    call    z,.remap
    cp      32
    call    z,.palette
    cp      1
    call    z,.remap
    or      a
    jr      nz,_badfile
    in      a,(IO_VCTRL)
    push    af
    or      VCTRL_TEXT_PAGE
    out     (IO_VCTRL),a          ; Switch to Color RAM
    ld      hl,BANK1_BASE+2048
    ld      de,SCREEN
    ld      bc,2048
    ldir                          ; Copy Color RAM data
    pop     af
    out     (IO_VCTRL),a          ;   Switch to Screen RAM
    ld      bc,2048
    jr      _read_tmpbfr_scrn

.palette:
    push    bc
    push    hl
    xor     a
    ld      d,h
    ld      e,a
    ld      l,a                   ; HL = PalAdr
    ld      bc,32
    call    palette_set
    pop     hl
    pop     bc
    xor     a
    ld      c,a
    ld      l,a
    ret

.remap
    ld      a,(hl)                ; A = VCTRL
    and     VCTRL_REMAP_BC        ;
    ld      e,a                   ; B = vctrl_remap_bc
    in      a,(IO_VCTRL)
    and     $FF-VCTRL_REMAP_BC
    or      e                     ; Set VCTRL_REMAP_BC
    out     (IO_VCTRL),a          ; and write IO_VCTRL
    dec     c                     ; BC = PrvLen
    dec     l                     ; HL = PrvEnd
    ld      a,c
    ret

;-----------------------------------------------------------------------------
; Copy Screen, Palette, and IO_VCTRL to TMP_BUFFR
; Input: A = Write Options (Bit 7 - Palette, Bit 6 - Border)
; Output: BC = Length of copied data
; Clobbers: AF, AF', DE, HL
;-----------------------------------------------------------------------------
screen_write_tmpbfr:
    ld      d,a                   ; D = WrtOpt
    call    page_map_tmpbfr       ; Bank 1 = TempBuffer. AF, Af', IX clobbered
    in      a,(IO_VCTRL)
    push    af                    ; Stack = IO_VCTRL, RtnAdr
    push    de                    ; Stack = WrtOpt, IO_VCTRL, RtnAdr
    and     VCRTL_80COL_EN
    jr      z,.col40              ; If 80 column mode
    in      a,(IO_VCTRL)
    and     $FF-VCTRL_TEXT_PAGE
    out     (IO_VCTRL),a          ;   Switch to Screen RAM
    call    .copy                 ;   and copy to buffer
    or      VCTRL_TEXT_PAGE
    out     (IO_VCTRL),a          ;   Switch to Color RAM
    call    .copy_next            ;   and copy to buffer
    jr      .trailer              ; Else
.col40
    call    .copy
.trailer
    pop     af                    ; A = WrtOpt; Stack = IO_VCTRL, RtnAdr
    rla                           ; Carry = WritePalette, Bit7 = WriteRemap
    ld      h,a                   ; H = WrtOpt
    jr      nc,.dont_palette      ; If WritePalette
    xor     a                     ;   Start at palette index 0
    ld      bc,32                 ;   Total 32 bytes
    call    palette_get           ;   Write palette to buffer. DE = NxtAdr
.dont_palette
    pop     af                    ; A = IO_VCTRL; Stack = RtnAdr
    out     (IO_VCTRL),a          ; Restore VCTRL
    sla     h                     ; Carry = WrtRmp
    jr      nc,.dont_remap        ; If WriteRemap
    and     VCTRL_REMAP_BC        ;   Check Remap Border flag
    ld      a,0                   ;   Border Control byte = 0 (False)
    jr      z,.remap_off          ;   If Remap Border flag set
    dec     a                     ;   Border Control byte = $FF (True)
.remap_off
    ld      (de),a                ;   Write Border control byte to buffer
    inc     de
.dont_remap:
    ex      de,hl                 ; HL = EndAdr
    ld      bc,-BANK1_BASE
    add     hl,bc                 ; HL = DatLen
    ld      b,h
    ld      c,l                   ; BC = DatLen
    jp      page_restore_bank1
.copy
    ld      de,BANK1_BASE
.copy_next
    ld      hl,SCREEN
    ld      bc,2048
    ldir
    ret

; Input: DE = Variable buffer address
; Output: HL = Palette buffer address, B = 32, C = 0
_palt_buff_addr:
    ld      h,0
    ld      a,e
    cp      $20
    jr      c,.skip
    sub     8
.skip
    add     a,a
    add     a,a
    add     a,BUFPALT40
    ld      l,a                   ; HL = VarBufOfs
    ld      h,high(BANK1_BASE)    ; HL = PalBufAdr
    ld      bc,32*256
    ret

; 40-col #1: E=$00
; 40-col #2: E=$08
; 80-col:    E=$10

; Map SCR_BUFFR into Bank 1, Text/Color page into screen memory
; Clobbered: A
_bank_screen_buff:
    ld      a,SCR_BUFFR
    out     (IO_BANK1),a
    ret

_bank_color_page:
    in      a,(IO_VCTRL)
    and     $FF-VCTRL_TEXT_PAGE
    or      VCTRL_TEXT_PAGE
    out     (IO_VCTRL),a
    ret

; Clobbered: A, BC
_map_screen_buff:
    ld      a,SCR_BUFFR
    byte    $01                   ; LD BC over LD A
_map_screen_vars:
    ld      a,BAS_BUFFR
    jp      page_map_bank1

; Select character set
; Input: A = SCREENCTL
; Clobbered: AF',BC,DE,HL,IX
_select_chrset:
    and     SCRCHRSET
    rra
    rra
    rra                           ; Move
    jp      select_chrset
