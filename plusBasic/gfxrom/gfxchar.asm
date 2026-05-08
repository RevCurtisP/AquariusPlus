;=====================================================================================
; Character Set and Definition Kernel Routines
;=====================================================================================

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
