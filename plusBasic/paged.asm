;=============================================================================
; Paged Memory Management routines
;=============================================================================

;-----------------------------------------------------------------------------
; Compare paged memory to main memory
; Input: A: Page
;        BC: Compare Length
;        DE: Memory Address
;        HL: Page Address
; Output: A: $FF if match, else 0
;        DE: $FF if match, else 0
;-----------------------------------------------------------------------------
page_mem_compare:
    ex      af,af'
    ld      a,$BF
    cp      d                   ; If MemAdr > $BFFF                   
    ret     c                   ;   Return error
    ex      af,af'
    ex      de,hl               ; DE = PgAdr, HL = MemAdr
    call    page__set4read_coerce
    ret     z
    dec     de
.loop
    ld      a,b
    or      c                       
    jr      z,_cmp_done                  
    dec     bc
    call    page_inc_de_addr
    jr      c,_cmp_error
    ld      a,(de)
    cp      (hl)
    inc     hl
    jr      z,.loop
    ld      a,$FF                   
_cmp_done:
    cpl                           ; Make $FF matched, 0 not
    ld      d,a
    ld      e,a
    dec     a                     ; Clear Zero and Carry
    ld      a,e
_cmp_error:
    jp      page_restore_bank3
    ret

      
;-----------------------------------------------------------------------------
; Compare paged memory to paged memory
; Input: A: Destination Page
;       A': Source Page
;       BC: Byte Count
;       DE: Destination Page Address
;       HL: Source Page Address
; Output: A: $FF if match, else 0
;        DE: $FF if match, else 0
; Flags: Zero if either page is not valid
;        Carry if Overflow
; Clobbers: BC,HL,AF',HL',IX

;-----------------------------------------------------------------------------
page_page_compare:
    call    page_check_read_write
    jp      z,_cpp_error
    call    page_swap_two         ; Bank1 = Source, Bank3 = Dest
    call    page_coerce_de_addr   
    call    page_coerce_hl_addr
    dec     de
    dec     hl
.loop
    ld      a,b
    or      c                       
    jr      z,_cpp_done
    dec     bc
    call    page_inc_de_addr
    jr      c,_cpp_error
    call    page_inc_hl_addr
    jr      c,_cpp_error
    ld      a,(de)
    cp      (hl)
    jr      z,.loop
    ld      a,$FF                   
_cpp_done:
    cpl                           ; Make $FF matched, 0 not
    ld      d,a
    ld      e,a
    dec     a                     ; Clear Zero and Carry
    ld      a,e
_cpp_error:
    call    page_restore_two      
    ret

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
    call    page_swap_two         ; Bank3 = DstPg, Bank2 = SrcPg
    call    page_coerce_de_addr   ; Coerce DstAdr to Bank3
    call    page_coerce_hl_addr   ; Coerce SrcAdr to Bank1
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
; Clobbers: AF,AF',BC,DE,HL
;-----------------------------------------------------------------------------
page_fast_read_bytes:
    ex      de,hl                 ; DE = SrcAdr, HL = DstAdr 
    call    page_coerce_de_addr   ; Coerce DstAdr
    ex      de,hl                 ; DE = DstAdr, HL = SrcAdr
    jr      _fast_copy            ; Copy it

;-----------------------------------------------------------------------------
; Input: A: Destination Page
;       BC: Byte Count
;       DE: Destination address
;       HL: Source Address
; No rollover or error checking
; Clobbers: AF,AF',BC,DE,HL
;-----------------------------------------------------------------------------
page_fast_write_bytes:
    call    page_coerce_de_addr   ; Coerce DstAdr
_fast_copy:
    ex      af,af'
    in      a,(IO_BANK3)
    push    af
    ex      af,af'
    out     (IO_BANK3),a          ; Map DestPg
    or      a                     ; Clear Carry, Zero flags
    ldir                          ; Do the Copy
_fast_done:
    pop     af
    out     (IO_BANK3),a          ; Restore Original Page
    ret

;-----------------------------------------------------------------------------
; Input: A: Page
;       BC: Byte Count
;       DE: Page Start Address (0-16383)
;       HL: RAM Start Address
; No rollover or error checking
; Clobbers: AF,AF',BC,DE,HL
;-----------------------------------------------------------------------------
page_mem_swap_bytes:
    call    page_coerce_de_addr   ; Coerce DstAdr
    ex      af,af'
    in      a,(IO_BANK3)
    push    af
    ex      af,af'
    out     (IO_BANK3),a          ; Map DestPg
.loop    
    ld      a,b
    or      c                     ; If BC = 0
    jr      z,_fast_done          ;   Restore ROM Page and return
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
    call    page_swap_two         ; Bank1 = Source, Bank3 = Dest
    call    page_coerce_de_addr   
    call    page_coerce_hl_addr
    dec     de
    dec     hl
.loop
    ld      a,b
    or      c
    jr      z,.done
    dec     bc
    call    page_inc_de_addr
    jr      c,.over
    call    page_inc_hl_addr
    jr      c,.over
    ld      a,(hl)
    ld      (de),a
    jr      .loop
.done
    xor     a                     ; Clear Carry Flag
    inc     a                     ; Clear Zero Flag
.over
    call    page_restore_two      
    ret

;-----------------------------------------------------------------------------
; Fill Entire Page with Byte
; Input: A: Page
;        L: Byte
; Output: Zero: Cleared if fill succesful, Set if invalid page
; Clobbers: A, BC, DE
;-----------------------------------------------------------------------------
page_fill_all_byte:
    ld      de,0
    ld      bc,$4000
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
page_fill_byte:
    call    page__set4write_coerce ; DE = Coerced Start Address
    ret     z                     ; If invalid page, return error
.loop
    ld      a,b
    or      c
    jp      z,_success
    dec     bc
    ld      a,l
    ld      (de),a
    call    page_inc_de_addr
    jp      c,page_restore_bank3
    jr      .loop

;-----------------------------------------------------------------------------
; Fill Entire Page with Word
; Input: A: Page
;        L: Byte
; Output: Zero: Cleared if fill succesful, Set if invalid page
; Clobbers: A, BC, DE
;-----------------------------------------------------------------------------
page_fill_all_word:
    ld      de,0
    ld      bc,$2000
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
page_fill_word:
    call    page__set4write_coerce
    ret     z                     ; If invalid page, return error
.loop
    ld      a,b
    or      c
    jr      z,_success
    dec     bc
    ld      a,l
    ld      (de),a
    call    page_inc_de_addr
    jr      c,.done
    ld      a,h
    ld      (de),a
    call    page_inc_de_addr
.done
    jr      nc,.loop
    jp      page_restore_bank3
_success:
    xor     a                     ; Clear Carry Flag
    inc     a                     ; Clear Zero Flag
    jp      page_restore_bank3    ; Restore BANK3 page and return

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
; Restore original pages
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
    call    page__set4read_coerce
    ret     z                     ; Return if illegal page
    ld      a,(de)
    ld      c,a
    jp      page_restore_bank3

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
    call    page__set4write_coerce
    ret     z                     ; Return if illegal page
    ld      a,c
    ld      (de),a
    jp      page_restore_bank3



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
    call    page__set4read_coerce
    ret     z
    ld      a,(de)
    ld      c,a
    inc     de
    ld      a,d
    or      e 
    jr      nz,.not_end
    call    page_next_de_address
    jr      c,.done               ; Return if overflow
.not_end
    ld      a,(de)
    ld      b,a
    dec     de                    ; DE = Original address
.done
    jp      page_restore_bank3

page__write_word:
    call    page__set_for_write
    ret     z
    call    page_coerce_de_addr
    ld      a,c
    ld      (de),a
    inc     de
    ld      a,d
    or      e 
    jr      nz,.not_end
    call    page_next_de_address
    jr      c,.done               ; Return if overflow
.not_end
    ld      a,b
    ld      (de),a
    dec     de                    ; Restore DE and fall into page_restore
.done
    jp      page_restore_bank3


;-----------------------------------------------------------------------------
; Write Word to Page - wraps to next page if address is 16383
; Input: A: Page
;       BC: Word to write
;`       DE: Address 0-16383 (Higher addresses mapped to input range) 
; Output: BC: Word written
;         DE: Address coerced to $C000-$FFFF
;       Zero: Cleared if succesful, Set if invalid page
;      Carry: Cleared if succesful, Set if overflow
; Clobbered: A
;-----------------------------------------------------------------------------
page_write_word:
    call    page__set4write_coerce
    ret     z                     ; Return if illegal page
    ld      a,c
    ld      (de),a
    inc     de
    ld      a,d
    or      e 
    jr      nz,.not_end
    call    page_next_de_address
    jr      c,.done               ; Return if overflow
.not_end
    ld      a,b
    ld      (de),a
    dec     de                    ; Restore DE and fall into page_restore
.done    
    jp      page_restore_bank3


;-----------------------------------------------------------------------------
; Map Bank 3 to Aux ROM
;-----------------------------------------------------------------------------
page_set_aux:
    push    af
    ld      a,ROM_AUX_RO
    jr      _map_page_bank3
;-----------------------------------------------------------------------------
; Map Bank 3 to Ext ROM
;-----------------------------------------------------------------------------
page_set_plus:
    push    af
    ld      a,ROM_EXT_RO
_map_page_bank3:
    out     (IO_BANK3),a
    pop     af
    ret



;-----------------------------------------------------------------------------
; Map Video RAM into Bank 1
; Input: A = Page
; Returns with original page on stack
; Clobbers AF',IX
;-----------------------------------------------------------------------------
page_map_vidram:
    ld      a,VIDEO_RAM
;-----------------------------------------------------------------------------
; Map Page into Bank 1
; Input: A = Page
; Returns with original page on stack
; Clobbers AF',IX
;-----------------------------------------------------------------------------
page_map_bank1:
    pop     ix                    ; IX = RtnAdr
    ex      af,af'                ; A' = NewPg
    in      a,(IO_BANK1)          ; A = CurPg
    push    af                    ; Stack = CurPg
    ex      af,af'                ; A = NewPg
    out     (IO_BANK1),a          ; Map into Bank 3
    jp      (ix)

;-----------------------------------------------------------------------------
; Map Page into Bank 3
; Input: A = Page
; Returns with original page on stack
; Clobbers AF',IX
;-----------------------------------------------------------------------------
page_map_bank3:
    pop     ix                    ; IX = RtnAdr
    ex      af,af'                ; A' = NewPg
    in      a,(IO_BANK3)          ; A = CurPg
    push    af                    ; Stack = CurPg
    ex      af,af'                ; A = NewPg
    out     (IO_BANK3),a          ; Map into Bank 3
    jp      (ix)

;-----------------------------------------------------------------------------
; Map Bank 3 to Page 2
; Clobbers AF',IX
;-----------------------------------------------------------------------------
page_map_auxrom:
    pop     ix                    ; IX = RtnAdr
    ex      af,af'
    in      a,(IO_BANK3)          ; A = CurPg
    push    af                    ; Stack = CurPg
    ld      a,ROM_AUX_RO
    out     (IO_BANK3),a
    ex      af,af'
    jp      (ix)

;-----------------------------------------------------------------------------
; Map Bank 3 to Page 2
; Clobbers AF',IX
;-----------------------------------------------------------------------------
page_map_extrom:
    pop     ix                    ; IX = RtnAdr
    ex      af,af'
    in      a,(IO_BANK3)          ; A = CurPg
    push    af                    ; Stack = CurPg
    ld      a,ROM_EXT_RO
    out     (IO_BANK3),a
    ex      af,af'
    jp      (ix)

;-----------------------------------------------------------------------------
; Restore Bank 1 to Previous Page and Return to Caller
; JP to this, do not CALL
; Clobbers AF'
;-----------------------------------------------------------------------------
page_restore_bank1:
    ex      af,af'
    pop     af                    ; A = OldPg
_map_1_ex:
    out     (IO_BANK1),a
    ex      af,af'
    ret

;-----------------------------------------------------------------------------
; Restore Bank 3 to Previous Page and Return to Caller
; Clobbers AF'
;-----------------------------------------------------------------------------
page_restore_bank3:
    ex      af,af'
    pop     af                    ; A = OldPg
_map_3_ex:
    out     (IO_BANK3),a
    ex      af,af'
    ret

;-----------------------------------------------------------------------------
; Map Bank 3 to Page 36
;-----------------------------------------------------------------------------
page_set_basbuf:
    push    af
    ld      a,BAS_BUFFR
    jr      _map_page_bank3

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
    call    page__set4write_coerce
    ret     z
    dec     de
.loop
    ld      a,b 
    or      c
    jr      z,.success
    call    page_inc_de_addr
    jr      c,.done
    ld      a,(hl)
    ld      (de),a
    inc     hl
    dec     bc
    jr      .loop
.success:
    xor     a                     ; Clear Carry Flag
    inc     a                     ; Clear Zero Flag
.done
    jp      page_restore_bank3    ; Restore BANK3 page and return


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
page_read_bytes_ex:
    call    page__set_for_read
    ret     z
    call    page_coerce_de_addr
    dec     de
.loop
    ld      a,b 
    or      c
    jr      z,.success
    call    page_inc_de_addr
    jr      c,.done
    ld      a,(de)
    ld      (hl),a
    inc     hl
    dec     bc
    jr      .loop
.success
    xor     a                     ; Clear Carry Flag
    inc     a                     ; Clear Zero Flag
.done
    jp      page_restore_bank3    ; Restore BANK3 page and return

;-----------------------------------------------------------------------------
; Map Page into Bank 3 and coerce address to bank 3
; Input: A: Bank to map into bank 3
;       DE: Address to coerce
; Zero Flag: Set if trying to page into bank that isn't ram
;-----------------------------------------------------------------------------
page_set4read_coerce:
    call    page_set_for_read
    jr      page_coerce_de_addr

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
    jr      page_coerce_de_addr

;-----------------------------------------------------------------------------
; Increment Bank 3 Write Address in DE
; Sets carry if trying to move out of video, character, or end of main RAM
; Clobbers: A
;-----------------------------------------------------------------------------
page_inc_de_addr:
    inc     de                    ; Increment Address
    ld      a,d
    or      e                     ; If Rolled Over
    ret     nz                    ;   Drop into page_next_de_address
    
;-----------------------------------------------------------------------------
; Map next Page into Bank 3 and coerce address to bank 3
; Input: DE: Address
; Output: A: New page
;        DE: Coerced address
; Sets carry if trying to move out of video, character, or end of main RAM
;-----------------------------------------------------------------------------
page_next_de_address:
    call    page_next_bank3             ; Move to next page and coerce address
    
;-----------------------------------------------------------------------------
; Coerce address into bank 3
; Input: DE: Address to coerce
; Output: DE: Coerced address
; Zero Flag: Set if trying to page into bank that isn't RAM
; Clobberes: AF'
;-----------------------------------------------------------------------------
page_coerce_de_addr:
    ex      af,af'                ; Save page and flags
    ld      a,d                   ; Address MSB    
    or      $C0                   ; Make 49152 - 65535
    ld      d,a                   ; Put back
    ex      af,af'                ; Restore page and flags
    ret

;-----------------------------------------------------------------------------
; Increment Page 1 Write Address in HL
; Sets carry if trying to move past page 63 (end of paged memory).
; Clobbers: A
;-----------------------------------------------------------------------------
page_inc_hl_addr:
    inc     hl                    ; Increment 
    ld      a,$80
    cp      h
    ret     nz

;-----------------------------------------------------------------------------
; Map next Page into Bank 3 and coerce address to bank 3
; Input: HL: Address
; Output: A: New page
;        HL: Coerced address
; Sets carry if trying to move out of video, character, or end of main RAM
;-----------------------------------------------------------------------------
page_next_hl_address:
    call    page_next_bank1             ; Move to next page and coerce address
    
;-----------------------------------------------------------------------------
; Coerce address into bank 1
; Input: HL: Address to coerce
; Output: HL: Coerced address
; Clobbers: AF'
;-----------------------------------------------------------------------------
page_coerce_hl_addr:
    ex      af,af'                ; Save page and flags
    ld      a,h                   ; Address MSB    
    and     $3F
    or      $40                   ; Make 49152 - 65535
    ld      h,a                   ; Put back
    ex      af,af'                ; Restore page and flags
    ret

page__set4read_coerce
    call    page_coerce_de_addr
    jr      page__set_for_read
page__set4write_coerce
    call    page_coerce_de_addr
page__set_for_write:
    call    page_check_write
    ret     z
    jr      __set_page
page__set_for_read:
    call    page_check_read
    ret     z
__set_page
    pop     ix
    ex      af,af'
    in      a,(IO_BANK3)
    push    af
    ex      af,af'
    out     (IO_BANK3),a          ; Map page into bank 3
    jp      (ix)


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
; Map next Page into Bank 1
; Sets carry if trying to move out of video, character, or end of main RAM
;-----------------------------------------------------------------------------
page_next_bank1:
    in      a,(IO_BANK1)
    call    page_check_next4read
    ret     c
    inc     a
    out     (IO_BANK1),a
    ret

;-----------------------------------------------------------------------------
; Map next Page into Bank 3 
; Sets carry if trying to move out of video, character, or end of main RAM
;-----------------------------------------------------------------------------
page_next_bank3:
    in      a,(IO_BANK3)
    call    page_check_next4write
    ret     c
    inc     a
    out     (IO_BANK3),a
    ret

;-----------------------------------------------------------------------------
; Check if next page is valid for read or write
; Sets carry if trying to move out of video or character RAM (write)
; or end of main RAM (read and write)
;-----------------------------------------------------------------------------
page_check_next4write:
    cp      VIDEO_RAM             ; If in video RAM
    jr      z,set_carry_flag      ;   error out
    cp      CHAR_RAM              ; If in character RAM
    jr      z,set_carry_flag      ;   error out
page_check_next4read:
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
    
