;; Miscellaneous utility routines

;-----------------------------------------------------------------------------
; Discard top entry on stack and return
;-----------------------------------------------------------------------------
discard_ret:
    inc     sp
    inc     sp
    ret
    
;-----------------------------------------------------------------------------
; Call Statement Tokenizer Loop
;-----------------------------------------------------------------------------
tokenize:
    call    KLOOP
    jp      page_restore_plus

;-----------------------------------------------------------------------------
; Convert byte to two digit number
;  Input: A: byte
;        HL: Address to write digits to
; Clobbers: A,B,HL
;-----------------------------------------------------------------------------
byte_to_hex:
    ld      b, a
    rra
    rra
    rra
    rra
    call    .hex
    ld      a, b
.hex:
    and     $0F
    cp      10
    jr      c, .chr
    add     7
.chr:
    add     '0'
    ld      (hl), a
    inc     hl
    ret

;-----------------------------------------------------------------------------
; Multiply A * DE
; Input: A: Multiplier
;       DE: Multiplicand
; Output: HL: Product
;       A, B: 0
;-----------------------------------------------------------------------------
mult_a_de:
    ld      hl,0
    ld      b,8
.loop
    add     hl,hl
    add     a,a
    jr      nc,.next
    add     hl,de
.next
    djnz    .loop
    ret

;-----------------------------------------------------------------------------
; Convert character to uppercase
;  Input: A: Character
; Output: A: Uppercased character
;-----------------------------------------------------------------------------
uppercase_char:
    cp      'a'                   ;;
    ret     c                     ;;If >= 'a'
    cp      '{'                   ;;
    ret     nc                    ;;and less than <'{'
    and     $5F                   ;;Clear Bit 5
    ret                           ;;

;-----------------------------------------------------------------------------
; Print C style (null terminated) string
; Faster than STROUT
;  Input: HL: Address of string
; Output: HL: Address of byte after terminator
; Clobbered: A
;-----------------------------------------------------------------------------
print_c_string:
    ld      a,(hl)                ;;Get Byte
    inc     hl                    ;;Point to Next Byte
    or      a                     ;;If Zero
    ret     z                     ;;  Return
    rst     OUTCHR                ;;Output Byte
    jr      print_c_string        ;;and Do it Again

;-----------------------------------------------------------------------------
; Print inline null terminated string
; Output: HL: Address of byte after string terminator terminator
;         PC: Address of byte after string terminator terminator
; Clobbered: A
;-----------------------------------------------------------------------------
print_string_immd:
    pop     hl                    ;; Get String Address off Stack
    call    print_c_string        ;; Print the String
    jp      (hl)                  ;; Fast Return

;-----------------------------------------------------------------------------
; Free temporary string, then get string address and length
;  Input: HL: String descriptor of string
; Output: BC: String length
;         DE: String text address
; Clobbered: A
;-----------------------------------------------------------------------------
free_addr_len:
    call    FRESTR                ;; Free Temporary String
;-----------------------------------------------------------------------------
; Get string address and length from string descriptor
;  Input: HL: String descriptor of string
; Output: BC: String length
;         DE: String text address
;-----------------------------------------------------------------------------
string_addr_len:
    push    hl                    ;; Save Descriptor Address
    ld      c,(hl)                ;; Get Length LSB
    inc     hl                    ;;
    ld      b,0                   ;; Get Length MSB (0 for BASIC strings)
    inc     hl                    ;;
    ld      e,(hl)                ;; Get Text Address LSB
    inc hl                        ;;
    ld      d,(hl)                ;; Get Text Address LSB
    pop     hl                    ;;
    ret                           ;;

;-----------------------------------------------------------------------------
; Compare uppercased string to another string
;  Input:  B: Number of characters to compare
;         DE: String to uppercase (returned as original string)
;         HL: String to compare to
; Output: Zero set if strings match, otherwise zero cleared
; Clobbered: A, B
;-----------------------------------------------------------------------------
string_cmp_upper:
    ld      a,(de)                ;; Get char from First String
    call    uppercase_char        ;; Convert to Upper Case
    inc     de                    ;;
    cp      (hl)                  ;; Compare to char in Second String
    inc     hl                    ;;
    ret     nz                    ;; Return NZ if not equal
    djnz    string_cmp_upper      ;;
    xor     a                     ;;
    ret                           ;; Return 0 = Equal

;-----------------------------------------------------------------------------
; Copy null terminated string
;  Input: DE: Destination address
;         HL: Source string address
; Output: BC: Length of copied string
; Clobbered: A
;-----------------------------------------------------------------------------
string_copy:
    push  hl
    push  de
    ld    bc,0
.loop
    ld    a,(hl)
    ld    (de),a
    or    a
    jr    z,.done
    inc   bc
    inc   de
    inc   hl
    jr    .loop
.done
    pop   de
    pop   hl
    ret

;-----------------------------------------------------------------------------
; Shift HL Left
;   Input: B = Shift Count
;  Output: B = 0
; Changes: HL
;-----------------------------------------------------------------------------
shift_hl_left:
    sla     l
    rl      h
    djnz    shift_hl_left
    ret

;-----------------------------------------------------------------------------
; Push HL and Return Null String
;-----------------------------------------------------------------------------
pop_ret_nullstr:
    push    hl
    jp      NULRT                 ; Pop HL abd return Null String


;-----------------------------------------------------------------------------
; Compare blocks of memory
; Input: BC: Compare Length
;        DE: First Address
;        HL: Second Address
; Output: A: $FF if match, else 0
;        DE: $FF if match, else 0
;-----------------------------------------------------------------------------
sys_mem_compare:
    ld    a,b
    or    c                       
    jr    z,.done                  
    dec   bc
    ld    a,(de)
    cp    (hl)
    inc   de
    inc   hl
    jr    z,sys_mem_compare
    ld    a,$FF                   
.done    
    cpl                           ; Make $FF matched, 0 not
    ld    d,a
    ld    e,a
    ret

;-----------------------------------------------------------------------------
; Convert null-terminated Version string to BCD integer
; Input: HL: Version String Address - ['V']major.minor[letter]
; Output: C,D,E = Major,Minor,Letter
;         HL = Address after Version string
; Clobbered: A, B
;-----------------------------------------------------------------------------
sys_num_ver:    
    ld      a,(hl)
    call    uppercase_char        ; Skip 'V'
    cp      'V'                 
    jr      nz,.notv
    inc     hl
.notv
    call    asc_to_bcd_byte       ; Get Major number
    ld      c,e                   ;   and put in C
    ld      de,0                  ; Init minor to 0
    cp      '.'                   ; If no dot
    ret     nz                    ;   Return
    inc     hl                    ; Skip .
    call    asc_to_bcd_byte       ; Get Minor number
    ld      d,e                   ;   and put in D
    ld      e,0                   ; Init letter to none
    call    uppercase_char        ; Capitalize 
    cp      'A'                   ; If not a letter
    ret     c                     ;    Return
    cp      'Z'+1
    ret     nc                    
    ld      e,a                   ; Else put in E
    ret
    
;-----------------------------------------------------------------------------
; Convert ASCII to BCD
;  Input: HL: String address
; Output: A: Character after last digit
;        DE: BCD Result
;        HL: Address after last digit 
; Clobbers: A, B
;----------------------------------------------------------------------------
asc_to_bcd_byte:
    ld      de,0                  ; Init Result
    dec     hl                    ; Back up for chrget
.loop
    rst     CHRGET                ; Get next character
    ret     nc                    ; Return if not a digit
    sub     a,'0'                 ; Convert digit to BCD
    ld      b,4
.rollde
    rl      e                     ; Shift DE left 4 bits
    rl      d
    djnz    .rollde
    or      e
    ld      e,a                   ; Move digit into bottom nybble                  
    jr      .loop

;-----------------------------------------------------------------------------
; Look up byte in table
; Input: A: Offset
;       DE: Table base address
; Output: A: Value at offset
;        DE: Address of offset
;-----------------------------------------------------------------------------
table_lookup:
    add     a,e
    ld      e,a
    jr      nc,.no_carry
    inc     d
.no_carry
    ld      a,(de)
    ret
    
;----------------------------------------------------------------------------
; Set Timer
; Input C,DE = Timer count
; Clobbers: A,B
;----------------------------------------------------------------------------
timer_write:
    ld      a,(BASYSCTL)
    and     $7F
    ld      (BASYSCTL),a
    ld      b,IRQ_TIMER
    call    enable_vblank_irq
    ld      a,c
    and     $7F                   ; Clear high bit
    jr      _twrite
    
;----------------------------------------------------------------------------
; Decrement timer one tick
; Output: C,DE = Timer count
; Flags: N if stopped 
; Clobbers: A
;----------------------------------------------------------------------------
timer_tick:
    call    timer_read            ; C,A = HiByt, D = MdByt, E = LoByt
    ret     m                     ; If stopped, Return

    ld      a,e
    sub     1                     
    ld      e,a                   ; LoByt -= 1

    ld      a,d                   
    sbc     0
    ld      d,a                   ; MdByt -= Carry

    ld      a,c                   ; A = HiByt
    sbc     0                     ; HiByt -= Carry
    ld      c,a
_twrite:
    ld      (TIMERCNT),de         ; 
    ld      (TIMERCNT+2),a        ; Put it back
    ret     p                     ; If >= 0, return it
_stop_timer:
    dec     a
    ld      (TIMERCNT+2),a
    ld      a,(IRQACTIVE)
    and     ~IRQ_TIMER
    ld      (IRQACTIVE),a
    jr      _tstopped             ;   and return 0

;----------------------------------------------------------------------------
; Read Timer
; Output: C,DE = Timer count
; Flags: N if stopped 
; Clobbers: A
;----------------------------------------------------------------------------
timer_read:
    ld      de,(TIMERCNT)         ; E = LoByt, D = MdByt
    ld      a,(TIMERCNT+2)        ; A = HiByt
    or      a                     ; If count < 0
    ld      c,a                   ; C = HiByt
    ret     p
_tstopped:
    ld      a,0
    ld      c,a
    ld      d,a
    ld      e,a
    ret

