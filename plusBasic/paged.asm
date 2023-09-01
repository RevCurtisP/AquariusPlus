;=============================================================================
; Paged Memory Management routines
;=============================================================================

;-----------------------------------------------------------------------------
; Copy entire Page to another Page
; Input: B: Source Page
;        C: Destination Page
; Clobbers: A, BC, DE
;-----------------------------------------------------------------------------
page_copy:
    ld      a,b                   ; If Source Page not valid for read
    call    page_check_read       ;   Return Error
    ret     c                         
    ld      a,c                   ; If Destination Page not valid for write
    call    page_check_write      ;   Return Error
    ret     c                         
    push    hl                    ; Save HL
    ld      hl,0
    add     hl,sp                 ; Get Stack Pointer
    ld      (PLUSTCK),hl          ; Save it
    ld      sp,PLUSTCK            ; Use temporary stack
    in      a,(IO_BANK3)          ; Save current page# in bank 2
    push    af                    ; 
    in      a,(IO_BANK2)          ; Save current page# in bank 2
    push    af                    ; 
    ld      a,b                   ; Map source page into bank 2         
    out     (IO_BANK2),a          ;
    ld      a,c                   ; Map destination page into bank 3        
    out     (IO_BANK3),a          ; 
    ld      hl,$8000              ; Copying from page in bank 2
    ld      de,$C000              ; to page in bank 3
    ld      bc,$4000              ; Entire bank/page
    ldir
    pop     af                    ; Restore bank 2 page
    out     (IO_BANK2),a          ;
    pop     af                    ; Restore bank 3 page
    out     (IO_BANK3),a          ;
    ld      sp,(PLUSTCK)          ; Back to original stack
    pop     hl                    ; Restore HL
    ret

;-----------------------------------------------------------------------------
; page_check_read
; page_check_write
; Verify page in A is valid for read/Write
; Carry Clear if valid, Set if not valid
;-----------------------------------------------------------------------------
page_check_write:
    cp      20                    ; If in video RAM
    ret     z                     ;   Return No Carry
    cp      21                    ; If in character RAM
    ret     z                     ;   Return No Carry
    cp      32                    ; If below main RAM
    ret     c                     ;   Return Carry
page_check_read:
    cp      64                    ; See if above main RAM
    ccf                           ; Invert Carry Flag
    ret

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
; Output: A: Original page in bank 3
;        BC: Word read
;        DE: Address coerced to $C000-$FFFF
;      Zero: Cleared if succesful, Set if invalid page
;     Carry: Cleared if succesful, Set if overflow
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
;       BC: Word to read
;       DE: Address 0-16383 (Higher addresses mapped to input range) 
; Output: A: Original page in bank 3
;        BC: Word written
;        DE: Address coerced to $C000-$FFFF
;      Zero: Cleared if succesful, Set if invalid page
;     Carry: Cleared if succesful, Set if overflow
;-----------------------------------------------------------------------------
page_write_word:
    call    page_set4write_coerce
;    call    debug
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
; Restore Bank 3 to Page 1
;-----------------------------------------------------------------------------
page_restore_plus:
    ex      af,af'
    ld      a,plus_page
    out     (IO_BANK3),a
    ex      af,af'
    ret

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
; Zero Flag: Set if trying to page into bank that isn't ram
;-----------------------------------------------------------------------------
page_set4write_coerce:
    call    page_set_for_write
    jr      page_coerce_address

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
; Coerce address in to bank 3
; Input: DE: Address to coerce
; Output: DE: Coerced address
; Zero Flag: Set if trying to page into bank that isn't ram
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
    cp      20                    ; If in video RAM
    jr      z,page_set_for_read   ;   do it
    cp      21                    ; If in character RAM
    jr      z,page_set_for_read   ;   do it
    cp      32                    ; If below main RAM
    jr      c,set_zero_flag       ;   error out
page_set_for_read:
    cp      64                    ; If above main RAM
    jr      nc,set_zero_flag      ;   error out
    out     (IO_BANK3),a          ; Map page into bank 3
    cp      0                     ; Clear zero flag and carry flag
    ret

    
;-----------------------------------------------------------------------------
; Map next Page into Bank 3 
; Output: A: New page in bank 3 (current page if error)
; Sets carry if trying to move out of video, character, or end of main RAM
;-----------------------------------------------------------------------------
page_next:
    in      a,(IO_BANK3)
    cp      20                    ; If in video RAM
    jr      z,set_carry_flag      ;   error out
    cp      21                    ; If in character RAM
    jr      z,set_carry_flag      ;   error out
    cp      63                    ; If after last RAM page
    jr      nc,set_carry_flag     ;   error out
    inc     a
    out     (IO_BANK3),a
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
    
    
    
buff_to_temp_string:
    call    STRLIT
    ret

