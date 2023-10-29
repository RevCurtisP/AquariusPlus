;; Miscellaneous utility routines

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
; ToDo: Change calls to this to MAKUPR in S3
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
; Convert null-terminated Version string to BCD integer
; Input: HL: Version String Address - ['V']major.minor[letter]
; Output: C,D,E = Major,Minor,Letter
;         HL = Address after Version string
; Clobbered: A, B
;-----------------------------------------------------------------------------
sys_num_ver:    
    ld      a,(hl)
    call    MAKUPR                ; Skip 'V'
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
    call    MAKUPR                ; Capitalize 
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
    
