;=====================================================================================
; plusBASIC Graphics Module
;=====================================================================================


;-----------------------------------------------------------------------------
; Initialize graphics variables
;-----------------------------------------------------------------------------
gfx_init:
    call    init_screen_buffers
    call    init_screen_vars
    call    bitmap_init_vars
    ret
    
;-----------------------------------------------------------------------------
; Get character definition from character RAM
; Input: A = ASCII Code
;       BC = Definition length
;       DE = Buffer address
;        L: Source 0: Char RAM
; Output:
; Flags: Carry Set if overflow
;-----------------------------------------------------------------------------
gfx_get_char_def:
    call    _get_chardef_address    ; A = MemPg, DE = ChrAdr, HL = BufAdr
    ex      de,hl                   ; HL = ChrAdr, DE = BufAdr
    jp      page_read_bytes         ; Write to Character RAM and return

;-----------------------------------------------------------------------------
; Get character definition in character RAM to default definition
; Input: A = ASCII Code
;       BC = Definition Length
;       DE = Buffer address
; Flags: Carry Set if overflow
; Clobbered: A, BC, DE, HL
;-----------------------------------------------------------------------------
gfx_reset_char_def:
    push    de                      ; Stack = BufAdr, RtnAdr
    push    bc                      ; Stack = DefLen, BufAdr, RtnAdr
    push    af                      ; Stack = ChrASC, DefLen, BufAdr, RtnAdr
    ld      l,0                     ; L = ChrSet (Default)
    call    gfx_get_char_def        ; A = MemPg, DE = ChrAdr, HL = BufAdr
    call    page_read_bytes         ; Read ChrDef to Buffer
    pop     af                      ; A = ChrASC; Stack = DefLen, BufAdr, RtnAdr
    pop     bc                      ; BC = DefLen; Stack = BufAdr, RtnAdr
    pop     de                      ; DE = BufAdr; Stack = RtnAdr
    ret     c                       ; Return Carry Set if overflow
;-----------------------------------------------------------------------------
; Redefine character in character RAM
; Input: A: ASCII code
;       BC: Definition length
;       DE: Definition address
; Flags: Carry Set if overflow
; Clobbered: A, BC, DE, HL
;-----------------------------------------------------------------------------
gfx_redefine_char:
    ld      l,255                   ; ChrSet = CharRAM
    call    _get_chardef_address    ; A = MemPg, DE = ChrAdr, HL = BufAdr
    jp      page_write_bytes        ; Write to Character RAM and return

; Input: A: ASCIcode, DE: Buffer Address, L: reserved
; Output: A: Memory Page DE: address Offset, HL: Buffer Address
_get_chardef_address:
    push    de                    ; Stack = BufAdr, RtnAdr
    ex      af,af'                ; A' = ASCcode
    inc     l
    dec     l                     ; If L = 0
    ld      a,BAS_BUFFR           ;   MemPg = BasBuf
    ld      h,$30                 ;   BasAdr = $3000
    jr      z,.based
    dec     l                     ; Else If L = 1
    ld      h,$38                 ;   BasAdr = $3800
    jr      z,.based              ; Else
    ld      a,CHAR_RAM            ;   MemPg = CharRAM
    ld      hl,0                  ;   BasAdr = $0000
.based
    push    af                    ; Stack = MemPg, BufAdr, RtnAdr
    ex      af,af'                ; A = ASCcode
    ld      d,0
    or      a                     ; Clear carry
    rl      a
    rl      d                     ; DA = ChrASC * 2
    rl      a
    rl      d                     ; DA = ChrASC + 4
    rl      a
    rl      d                     ; DA = ChrASC * 8
    ld      e,a                   ; DE = ChrASC * 8
    add     hl,de                 ; HL = ChrAdr
    ex      de,hl                 ; DE = ChrAdr
    pop     af                    ; A = MemPg; Stack = BufAdr, RtnAdr
    pop     hl                    ; HL = BufAdr; Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; Set bitmap drawing mode
;  Input: A: Mode (0: bloxel, 2: 1bpp, 3: 4bpp)
; Output: A: New GFX_FLAGS
;-----------------------------------------------------------------------------
gfx_set_mode:
    and     GFXM_MASK             ; Isolate mode bits
    push    bc
    ld      b,a                   ; B = Mode
    ld      a,(GFX_FLAGS)         ; A = Extended Flags
    and     $FF-GFXM_MASK         ; Clear old mode
    or      b                     ; Set new modebit
    pop     bc
    ret

;-----------------------------------------------------------------------------
; Get bitmap drawing mode
; Output: A: Mode (0: bloxel, 2: 1bpp, 3: 4bpp)
; Sets: Z if bloxel, NZ if bitmap
;-----------------------------------------------------------------------------
gfx_get_mode:
    ld      a,(GFX_FLAGS)         ; A = Extended Flags
    and     GFXM_MASK             ; Isolate mode bits
    ret

;-----------------------------------------------------------------------------
; Convert rectangular coordinates to address, columns and row count
; Input: B: Start Column
;        C: End Column  
;        D: Start Row
;        E: End Row
;       IX: Cell Address calculation routine
; Output: A: Row Count
;        BC: Column Count
;        DE: Start Address
;-----------------------------------------------------------------------------
gfx_convert_rect:
    ld      a,c                   ; A = EndCol
    sub     b                     ; A = EndCol - BgnCol
    ret     c                     ; If EndCol < BgnCol Return error
    inc     a                     ; ColCnt = EndCol - BgnCol + 1
    ld      c,b                   ; C = BgnCol
    ld      b,a                   ; B = ColCnt
    ld      a,e                   ; A = EndRow
    sub     d                     ; A= EndRow - BgnRow
    ret     c                     ; If EndRow < BgnRow Return error
    inc     a                     ; A = RowCnt 
    push    af                    ; Stack = RowCnt, RtnAdr
    ld      e,d                   ; E = BgnRow
    call    jump_ix               ; A = 0, DE = BgnAdr
    ret     c                     ; Return if error
    ld      c,b                   
    ld      b,0                   ; BC = ColCnt
    pop     af                    ; A = RowCnt; Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; Calculate total number of cells in a rectangle
; Input: B: Start Column
;        C: End Column  
;        D: Start Row
;        E: End Row
; Output: HL: Total number of cells
;     Flags: Carry set if more than 255 cells 
;-----------------------------------------------------------------------------
gfx_rect_size:
    push    de                    ; Stack = RowSE, RtnAdr
    ld      a,e
    sub     d
    inc     a
    ld      e,a
    ld      d,0                   ; E = EndRow - BgnRow + 1
    ld      a,c
    sub     b
    inc     a                     ; A = EndCol - BgnCol + 1
    call    mult_a_de             ; HL = Rows * Cols
    pop     de                    ; DE = RowSE; Stack = RtnAdr
    ret
