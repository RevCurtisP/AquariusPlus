;=============================================================================
; Paged Memory Management routines
;=============================================================================

;-----------------------------------------------------------------------------
; Copy bytes from one Page to another Page
; Input: A: Destination Page
;       A': Source Page
;       BC: Byte Count
;       DE: Destination address (0-16383)
;       HL: Source Address (0-16383)
;     Zero: Set if either page is invalid
; Clobbers: A,BC,DE,HL,IX
; No rollover or error checking 
;-----------------------------------------------------------------------------
page_fast_copy:
    call    page_check_read_write
    ret     z
    ex      af,af'
    ld      ixl,a                 ; IXL = SrcPg
    ex      af,af'
    cp      ixl                   ; If SrcPg = DstPg
    ret     z                     ; Return Error
    call    page_swap_two         ; Bank3 = DstPg, Bank2 = SrcPg
    call    page_coerce_address   ; Coerce DstAdr to Bank3
    ld      a,h                   ; Coerce SrcAdr to Bank2
    and     $3F                   ;  
    or      $40
    ld      h,a
    or      a                     ; Clear Carry, Zero flags
    ldir                          ; Do the copy
    call    page_restore_two
    xor     a
    inc     a                     ; Clear carry, zero, sign flags
    ret

;-----------------------------------------------------------------------------
; Copy entire Page to another Page
; Input: A: Destination Page
;       A': Source Page
; Zero Flag: Clear if valid pages, Set if not
; Clobbers: A,BC,DE,AF',HL',IX
;-----------------------------------------------------------------------------
page_full_copy:
    call    page_check_read_write
    ret     z
    push    hl                    ; Stack = HL, RetAddr
    call    page_swap_two         ; Bank3 = A, Bank4 = A'
    ld      hl,$4000              ; Copying from page in bank 1
    ld      de,$C000              ; to page in bank 3
    ld      bc,$4000              ; Entire bank/page
    xor     a                     ; Clear Carry
    ldir
    call    page_restore_two
    pop     hl                    ; Stack = RetAddr
    cp      $FF                   ; Clear zero flag
    ret

;-----------------------------------------------------------------------------
; Input: A: Source Page
;       BC: Byte Count
;       DE: Destination address
;       HL: Source Address (0-16383)
; No rollover or error checking 
; Clobbers: A,BC,DE,HL
;-----------------------------------------------------------------------------
page_fast_read_bytes:
    ex      de,hl                 ; DE = SrcAdr, HL = DstAdr 
    call    page_coerce_address   ; Coerce DstAdr
    ex      de,hl                 ; DE = DstAdr, HL = SrcAdr
    jr      _page_copy            ; Copy it

;-----------------------------------------------------------------------------
; Input: A: Destination Page
;       BC: Byte Count
;       DE: Destination address (0-16383)
;       HL: Source Address
; No rollover or error checking
; Clobbers: A,BC,DE,HL
;-----------------------------------------------------------------------------
page_fast_write_bytes:
    call    page_coerce_address   ; Coerce DstAdr
_page_copy:
    out     (IO_BANK3),a          ; Map DestPg
    or      a                     ; Clear Carry, Zero flags
    ldir                          ; Do the Copy
    jp      page_restore_plus     ; Restore ROM Page and return

;-----------------------------------------------------------------------------
; Input: A: Page
;       BC: Byte Count
;       DE: Page Start Address (0-16383)
;       HL: RAM Start Address
; No rollover or error checking
; Clobbers: A,BC,DE,HL
;-----------------------------------------------------------------------------
page_mem_swap_bytes:
    call    page_coerce_address   ; Coerce DstAdr
    out     (IO_BANK3),a          ; Map DestPg
.loop    
    ld      a,b
    or      c                     ; If BC = 0
    jp      z,page_restore_plus   ;   Restore ROM Page and return
    ld      a,(hl)                ; Get RAM byte
    ex      af,af'
    ld      a,(de)                ; Copy paged byte
    ld      (hl),a                ; to RAM address
    ex      af,af'
    ld      (de),a                ; Put RAM byte in paged address
    inc     de                    ; Next paged addres
    inc     hl                    ; Next RAM address
    dec     bc                    ; Count down
    jr      .loop                 ; and do next byte

;-----------------------------------------------------------------------------
; Input: A: Destination Page
;       A': Source Page
;       BC: Byte Count
;       DE: Destination address (0-16383)
;       HL: Source Address (0-16383)
; Output: Zero Set if either page is not valid
;         Carry Set if Overflow
; Clobbers: A,BC,DE,HL,AF',HL',IX
;-----------------------------------------------------------------------------
page_copy_bytes:
    call    page_check_read_write
    ret     z
    ex      af,af'
    call    page_swap_two         ; Bank4 = Source, Bank3 = Dest
    ex      de,hl                 ; DE = Source, HL = Dest
    call    page_coerce_address   ; Coerce DE
    call    _coerce_hl
    dec     de
    dec     hl
.loop
    ld      a,b
    or      c
    jr      z,.done
    dec     bc
    call    page_inc_addr
    jr      c,.over
    call    _inc_hl
    jr      c,.over
    ld      a,(de)
    ld      (hl),a
    jr      .loop
.done
    xor     a                     ; Clear Carry Flag
    inc     a                     ; Clear Zero Flag
.over
    call    page_restore_two      
    ret

_inc_hl:
    inc     hl                    ; Increment 
    ld      a,$C0
    cp      h
    ret     nz

    in      a,(IO_BANK1)
    call    page_check_next
    ret     c
    inc     a
    out     (IO_BANK1),a

_coerce_hl:
    ld      a,h                   ; Coerce HL
    and     $3F
    or      $40
    ld      h,a
    ret

;-----------------------------------------------------------------------------
; Fill Paged Memory with Byte
; Input: A: Page
;       BC: Byte Count
;       DE: Start Address
;        L: Byte
; Output: Zero: Cleared if fill succesful, Set if invalid page
;         Carry: Cleared if succesful, Set if overflow
; Clobbers: A, BC, DE
;-----------------------------------------------------------------------------
;ToDo: finish rewrite page_fill_word
page_fill_byte:
    call    page_set4write_coerce ; DE = Coerced Start Address
    ret     z                     ; If invalid page, return error
.loop
    ld      a,b
    or      c
    jp      z,_success
    dec     bc
    ld      a,l
    ld      (de),a
    call    page_inc_addr
    jp      c,page_restore_plus   

    jr      .loop

;-----------------------------------------------------------------------------
; Fill Paged Memory with Word
; Input: A: Page
;       BC: Word Count
;       DE: Start Address
;       HL: Word
; Output: Zero: Cleared if fill succesful, Set if invalid page
;         Carry: Cleared if succesful, Set if overflow
; Clobbers: A, BC, DE
;-----------------------------------------------------------------------------
;ToDo: finish rewrite page_fill_word
page_fill_word:
    call    page_set4write_coerce ; DE = Coerced Start Address
    ret     z                     ; If invalid page, return error
.loop
    ld      a,b
    or      c
    jp      z,_success
    dec     bc
    ld      a,l
    ld      (de),a
    call    page_inc_addr
    jp      c,page_restore_plus
    ld      a,h
    ld      (de),a
    call    page_inc_addr
    jp      c,page_restore_plus
    jr      .loop

;-----------------------------------------------------------------------------
; Swap pages into Banks 1 and 3
; Input: A: Page to Swap into Bank 3
;       A': Page to Swap into Bank 1
; Leaves previous pages in banks stack
; Clobbers: IX
;-----------------------------------------------------------------------------
page_swap_two:
    pop     ix                    ; IX = RtnAdr
    push    hl                    ; Stack = OldHL
    push    af                    ; Stack = NewPg3, OldHL
    in      a,(IO_BANK3)
    ld      h,a                   ; H = OrgPg3
    in      a,(IO_BANK1)
    ld      l,a                   ; L = OrgPg1
    pop     af                    ; A = NewPg3
    ex      (sp),hl               ; HL = OldHL, Stack = OrgPgs
    out     (IO_BANK3),a          ; Set Bank 3 to A
    ex      af,af'
    out     (IO_BANK1),a          ; Set Bank 1 to A'
    ex      af,af'
    jp      (ix)                  ; Return

;-----------------------------------------------------------------------------
; Restore original pages and stack and enable interrupts
; Clobbers: AF',IX
;-----------------------------------------------------------------------------
page_restore_two:
    pop     ix                    ; IX = RtnAdr
    ex      af,af'
    ex      (sp),hl               ; HL = OrgPgs, Stack = OldHL
    ld      a,h                   ; A = OrgPg3
    out     (IO_BANK3),a          ; Restore Bank 3
    ld      a,l                   ; A = OrgPg1
    out     (IO_BANK1),a          ; Restore Bank 1
    ex      af,af'
    pop     hl                    ; HL = OldHL
    jp      (ix)                  ; Return

;-----------------------------------------------------------------------------
; Read Byte from Page
; Input: A: Page
;       DE: Address 0-16383 (Higher addresses mapped to input range) 
; Output: A: Original page in bank 3
;         C: Byte read
;        DE: Address coerced to $C000-$FFFF
;      Zero: Cleared if succesful, set if invalid page
;-----------------------------------------------------------------------------
page_read_byte:
    call    page_set4read_coerce
    jr      z,page_restore_plus  ; Return if illegal page
    ld      a,(de)
    ld      c,a
    jr      page_restore_plus

;-----------------------------------------------------------------------------
; Write Byte to Page
; Input: A: Page
;        C: Byte to write
;       DE: Address 0-16383 (Higher addresses mapped to input range) 
; Output: A: Original page in bank 3
;         C: Byte written
;        DE: Address coerced to $C000-$FFFF
;      Zero: Cleared if succesful, set if invalid page
;-----------------------------------------------------------------------------
page_write_byte:
    call    page_set4write_coerce
    jr      z,page_restore_plus  ; Return if illegal page
    ld      a,c
    ld      (de),a
    jr      page_restore_plus

;-----------------------------------------------------------------------------
; Read Word from Page - wraps to next page if address is 16383
; Input: A: Page
;       DE: Address 0-16383 (Higher addresses mapped to input range) 
; Output: BC: Word read
;         DE: Address coerced to $C000-$FFFF
;       Zero: Cleared if succesful, Set if invalid page
;      Carry: Cleared if succesful, Set if overflow
;-----------------------------------------------------------------------------
page_read_word:
    call    page_set4read_coerce
    jr      z,page_restore_plus  ; Return if illegal page
    ld      a,(de)
    ld      c,a
    inc     de
    ld      a,d
    or      e 
    jr      nz,.not_end
    call    page_next_address
    jp      c,page_restore_plus    ; Return if overflow
.not_end
    ld      a,(de)
    ld      b,a
    dec     de                    ; DE = Original address
    jr      page_restore_plus

;-----------------------------------------------------------------------------
; Write Word to Page - wraps to next page if address is 16383
; Input: A: Page
;       BC: Word to write
;       DE: Address 0-16383 (Higher addresses mapped to input range) 
; Output: BC: Word written
;         DE: Address coerced to $C000-$FFFF
;       Zero: Cleared if succesful, Set if invalid page
;      Carry: Cleared if succesful, Set if overflow
; Clobbered: A
;-----------------------------------------------------------------------------
page_write_word:
    call    page_set4write_coerce
    jr      z,page_restore_plus  ; Return if illegal page
    ld      a,c
    ld      (de),a
    inc     de
    ld      a,d
    or      e 
    jr      nz,.not_end
    call    page_next_address
    jp      c,page_restore_plus   ; Return if overflow
.not_end
    ld      a,b
    ld      (de),a
    dec     de                    ; Restore DE and fall into page_restore

    
;-----------------------------------------------------------------------------
; Map Bank 3 to Page 2
;-----------------------------------------------------------------------------
page_map_aux:
    push    af
    ld      a,ROM_AUX_PG
    jr      _map_page_bank3

;-----------------------------------------------------------------------------
; Map Bank 3 to Page 36
;-----------------------------------------------------------------------------
page_map_basbuf:
    push    af
    ld      a,BAS_BUFFR
    jr      _map_page_bank3

;-----------------------------------------------------------------------------
; Restore Bank 3 to Page 1
;-----------------------------------------------------------------------------
page_restore_plus:
    push    af
    ld      a,ROM_EXT_PG
_map_page_bank3:
    out     (IO_BANK3),a
    pop     af
    ret

;-----------------------------------------------------------------------------
; Write Bytes to Page - wraps to next page if address is 16383
; Input: A: Page
;       BC: Byte Count
;       DE: Destination address 0-16383 
;       HL: Source Address
; Output: DE: Updated destination address (coerced)
; Flags Set: Z if llegal page
;            C if page overflow
; Clobbers: A, AF', BC, HL
;-----------------------------------------------------------------------------
page_write_bytes:
    call    page_set4write_coerce
    jr      z,page_restore_plus  ; Return if illegal page
    dec     de
.loop
    ld      a,b 
    or      c
    jr      z,_success
    call    page_inc_addr
    jr      c,page_restore_plus
    ld      a,(hl)
    ld      (de),a
    inc     hl
    dec     bc
    jr      .loop
_success:
    xor     a                     ; Clear Carry Flag
    inc     a                     ; Clear Zero Flag
    jr      page_restore_plus     ; Restore BANK3 page and return

;-----------------------------------------------------------------------------
; Read Bytes from Page - wraps to next page if address is 16383
; Input: A: Page
;       BC: Byte Count
;       DE: Destination Address
;       HL: Source Address  0-16383 
; Output: Zero: Cleared if succesful, Set if invalid page
;         Carry: Cleared if succesful, Set if overflow
; Clobbers: A, BC, DE, HL
;-----------------------------------------------------------------------------
page_read_bytes:
    ex      de,hl                 ; DE = Source Addr, HL = Dest Addr
    call    page_set4write_coerce
    jr      z,page_restore_plus   ; Return if illegal page
    dec     de
.loop
    ld      a,b 
    or      c
    jr      z,.done
    call    page_inc_addr
    jr      c,page_restore_plus
    ld      a,(de)
    ld      (hl),a
    inc     hl
    dec     bc
    jr      .loop
.done
    xor     a                     ; Clear Carry Flag
    inc     a                     ; Clear Zero Flag
    jr      page_restore_plus     ; Restore BANK3 page and return

;-----------------------------------------------------------------------------
; Map Page into Bank 3 and coerce address to bank 3
; Input: A: Bank to map into bank 3
;       DE: Address to coerce
; Zero Flag: Set if trying to page into bank that isn't ram
;-----------------------------------------------------------------------------
page_set4read_coerce:
    call    page_set_for_read
    jr      page_coerce_address

;-----------------------------------------------------------------------------
; Map Page into valid Bank 3 and coerce address to bank 3
; Input: A: Bank to map into bank 3
;       DE: Address to coerce
; Output: DE: Coerced address
; Flags Set: Z if page not RAM
; Clobbers: AF'
;-----------------------------------------------------------------------------
page_set4write_coerce:
    call    page_set_for_write
    jr      page_coerce_address

;-----------------------------------------------------------------------------
; Increment Page Write Address
; Sets carry if trying to move out of video, character, or end of main RAM
;-----------------------------------------------------------------------------
page_inc_addr:
    inc     de                    ; Increment Address
    ld      a,d
    or      e                     ; If Rolled Over
    ret     nz                    ;   Drop into page_next_address
    
;-----------------------------------------------------------------------------
; Map next Page into Bank 3 and coerce address to bank 3
; Input: DE: Address
; Output: A: New page
;        DE: Coerced address
; Sets carry if trying to move out of video, character, or end of main RAM
;-----------------------------------------------------------------------------
page_next_address:
    call    page_next             ; Move to next page and coerce address
    
;-----------------------------------------------------------------------------
; Coerce address into bank 3
; Input: DE: Address to coerce
; Output: DE: Coerced address
; Zero Flag: Set if trying to page into bank that isn't RAM
; Clobberes: AF'
;-----------------------------------------------------------------------------
page_coerce_address:
    ex      af,af'                ; Save page and flags
    ld      a,d                   ; Address MSB    
    or      $C0                   ; Make 49152 - 65535
    ld      d,a                   ; Put back
    ex      af,af'                ; Restore page and flags
    ret

;-----------------------------------------------------------------------------
; Map Page into Bank 3 
; Input: A: Bank to map into bank 3
; Zero Flag: Set if trying to page into bank that isn't ram
;-----------------------------------------------------------------------------
page_set_for_write:
    call    page_check_write
    ret     z
    jr      _set_page
page_set_for_read:
    call    page_check_read
    ret     z
_set_page
    out     (IO_BANK3),a          ; Map page into bank 3
    ret

;-----------------------------------------------------------------------------
; page_check_read_write
; Verify page in A' is valid for Read
;    and page in A is valid for Write
; Zero Flag: Clear if valid page, Set if not
;-----------------------------------------------------------------------------
page_check_read_write:
    ex      af,af'
    call    page_check_read       ; If Source Page not valid for read
    ret     z                     ;   Return Error    
    ex      af,af'

;-----------------------------------------------------------------------------
; page_check_read
; page_check_write
; Verify page in A is valid for Read/Write
; Zero Flag: Clear if valid page, Set if not
;-----------------------------------------------------------------------------
page_check_write:
    cp      20                    ; If in video RAM
    jr      z,_page_ok            ;   do it
    cp      21                    ; If in character RAM
    jr      z,_page_ok            ;   do it
    cp      32                    ; If below main RAM
    jr      c,set_zero_flag       ;   error out
page_check_read:
    cp      64                    ; If above main RAM
    jr      nc,set_zero_flag      ;   error out
_page_ok
    cp      $FF                   ; Clear zero flag
    ccf                           ; Clear carry flag
    ret


;-----------------------------------------------------------------------------
; Map next Page into Bank 3 
; Sets carry if trying to move out of video, character, or end of main RAM
;-----------------------------------------------------------------------------
page_next:
    in      a,(IO_BANK3)
    call    page_check_next
    ret     c
    inc     a
    out     (IO_BANK3),a
    ret

page_check_next:
    cp      VIDEO_RAM             ; If in video RAM
    jr      z,set_carry_flag      ;   error out
    cp      CHAR_RAM              ; If in character RAM
    jr      z,set_carry_flag      ;   error out
    cp      63                    ; If in last RAM page
    jr      z,set_carry_flag      ;   error out
    cp      0                     ; Clear carry and zero flags
    ret

set_carry_flag:
    cp      0                     ; Clear carry and zero flags
    scf                           ; Now set carry flag
    ret

set_zero_flag:
    push    af                    ; Stack = AF 
    ex      (sp),hl               ; HL = copy of AF, Stack = HL
    set     6,l                   ; Set bit 7 (zero flag)
    res     0,l                   ; Clear bit 1 (carry flag)
    ex      (sp),hl               ; Restore HL, Stack = Modified AF
    pop     af                    ; AF now has zero bit set
    ret
    
