;; Miscellaneous utility routines

;-----------------------------------------------------------------------------
; Discard top entry on stack and return
;-----------------------------------------------------------------------------
discard3ret:
    inc     sp
    inc     sp
discard2ret:
    inc     sp
    inc     sp
discard_ret:
    inc     sp
    inc     sp
    ret

;-----------------------------------------------------------------------------
; Search variable table, create variable if not found
;     Input: BC: Variable name as stored in variable table 
;    Output: DE: Address of variable contents in variable table
; Clobbered: AF
;-----------------------------------------------------------------------------
find_var:
    push    hl
    jp      VARFND

;-----------------------------------------------------------------------------
; Get Line Buffer Address
; Output: HL: Line Buffer Address
;-----------------------------------------------------------------------------
get_linbuf_addr:
    ld      hl,(TOPMEM)
    ret

get_linbuf_de:
    ld      de,(TOPMEM)
    inc     de
    ret

get_linbuf_hl:
    ld      hl,(TOPMEM)
inc_hl:
    inc     hl
    ret


get_strbuf_addr_no_bc:
    push    bc
    Call    get_strbuf_addr
    pop     bc
    ret

;-----------------------------------------------------------------------------
; Get String Buffer Address
; Output: HL: String Buffer Address
;         BC: Buffer Length minus 1
;-----------------------------------------------------------------------------
get_strbuf_addr:
    ld      bc,256
    ld      hl,(TOPMEM)
    add     hl,bc                 ; HL = StrBuf
    dec     bc
    ret
    
;-----------------------------------------------------------------------------
; Call Statement Tokenizer Loop
;-----------------------------------------------------------------------------
tokenize:
    call    KLOOP
    jp      page_set_plus

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

upr_get:
    call    get_key
    jr      uppercase_char

lwr_get:
    call    get_key
    jr      lowercase_char

upr_key:
    call    in_key
    jr      uppercase_char

lwr_key:
    call    in_key
    jr      lowercase_char
    
uprlwr_char:
    jr      z,uppercase_char
;-----------------------------------------------------------------------------
; Convert character to lowercase
;  Input: A: Character
; Output: A: Lowercased character
;-----------------------------------------------------------------------------
lowercase_char:
    cp      'A'                   ;;
    ret     c                     ;;If >= 'a'
    cp      '['                   ;;
    ret     nc                    ;;and less than <'{'
    or      $20                   ;;Clear Bit 5
    ret                           ;;

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
    ex      (sp),hl
    call    print_c_string
    ex      (sp),hl
    ret

;-----------------------------------------------------------------------------
; Free temporary string, then get string address and length
;  Input: HL: String descriptor of string
; Output: BC: String length
;         DE: String text address
; Clobbered: A
;-----------------------------------------------------------------------------
free_hl_addr_len:
    call    FRETM2
    jr      string_addr_len

fretms_addr_len:
    call    FRETMS
faclo_addr_len:
    ld      hl,(FACLO)
    jr      string_addr_len

;-----------------------------------------------------------------------------
; Free temporary string, then get string address and length
; Output: BC: String length
;         DE: String text address
; Clobbered: A
;-----------------------------------------------------------------------------
free_addr_len:
    call    FRESTR                ;; Free Temporary String
;-----------------------------------------------------------------------------
; Get string address and length from string descriptor
;  Input: HL: String descriptor of string
; Output: BC: String length (flags set accordingly)
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
    ld      a,b
    or      c                     ;; Set flags for length
    ret                           

;-----------------------------------------------------------------------------
; Compare string to another string
;  Input:  B: Number of characters to compare
;         DE: String to uppercase (returned as original string)
;         HL: String to compare to
; Output: Zero set if strings match, otherwise zero cleared
; Clobbered: A, B
;-----------------------------------------------------------------------------
string_cmp:
    ld      a,(de)                ;; Get char from First String
    inc     de                    ;;
    cp      (hl)                  ;; Compare to char in Second String
    inc     hl                    ;;
    ret     nz                    ;; Return NZ if not equal
    djnz    string_cmp            ;;
    xor     a                     ;;
    ret                           ;; Return 0 = Equal
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
; Search for string in memory
;  Input:  B: String length
;         DE: String address
;         HL: Start address
; Output: Zero set if strings match, otherwise zero cleared
; Clobbered: A, B
;-----------------------------------------------------------------------------
;-----------------------------------------------------------------------------
;   Input: A: Page
;          C: String length
;         DE: String address
;         HL: Start address
; Output: HL: Address in page ($FFFF = not found)
;-----------------------------------------------------------------------------
string_find:
    ld      b,c                   ; B = StrLen
.find
    push    bc                    ; Stack = StrLen, RtnAdr
    push    de                    ; Stack = StrAdr, StrLen, RtnAdr
    push    hl                    ; Stack = MemAdr, StrAdr, StrLen, RtnAdr
.compare
    ld      a,(de)                
    cp      (hl)                  ; If (StrPtr) = (MemPtr)
    jr      nz,.nope
    inc     de                    ;   Bump StrPtr
    inc     hl                    ;   and MemPtr
    djnz    .compare              ;   Loop until end of string
.nope
    pop     hl                    ; HL = MemAdr, Stack = StrAdr, StrLen, RtnAdr
    pop     de                    ; DE = StrAdr, Stack = StrLen, RtnAdr
    pop     bc                    ; B = StrLen, Stack = RtnAdr
    ret     z                     ; Return if match 
    inc     hl                    ; Bump MemAdr
    ld      a,h
    and     a,l                   ; If HL = $FFFF
    inc     a                     ;   A = 0
    jp      nz,.find              ; Else try again
    ret

;-----------------------------------------------------------------------------
; Copy null terminated string
;  Input: DE: Destination address
;         HL: Source string address
; Output: BC: Length of copied string
; Clobbered: A
;-----------------------------------------------------------------------------
str_copy:
    push  hl
    push  de
    call  str_cpy0
    pop   de
    pop   hl
    ret

str_cpy0:
    ld    bc,0
str_cpy:
    ld    a,(hl)
    ld    (de),a
    or    a
    ret   z
    inc   bc
    inc   de
    inc   hl
    jr    str_cpy


;-----------------------------------------------------------------------------
; Get length of null terminated string
;  Input: DE: String address
; Output: A, BC: String length
;  Flags: Carry Set if longer than 255 characters
;----------------------------------------------------------------------------
str_length:
    push  de                      ; Stack = StrAdr, RtnAdr
    ld    bc,0
.loop
    ld    a,(de)
    or    a
    jr    z,.done
    inc   de
    inc   c                       ; Bump counter
    jr    nz,.loop
    scf
.done
    pop   de                      ; DE = StrAdr; Stack = RtnAdr
    ld    a,c                     ; A = Length
    ret                           

;-----------------------------------------------------------------------------
; Creare string descriptor in DSCTMP for null terminated string
;  Input: DE: String address
; Output: A, BC: String length
;         HL: DSCTMP address
;  Flags: Carry Set if longer than 255 characters
;----------------------------------------------------------------------------
str_tempdesc:
    ld      hl,DSCTMP
;-----------------------------------------------------------------------------
; Creare string descriptor for null terminated string
;  Input: DE: String address
;         HL: String descriptor address
; Output: A, BC: String length
;  Flags: Carry Set if longer than 255 characters
;----------------------------------------------------------------------------
str_stringdesc:
    call    str_length            ; A = StrLen
    jp      STRADI                ; Set string descriptor
    
;-----------------------------------------------------------------------------
; Multiply A by 32, discarding carry
;-----------------------------------------------------------------------------
mult_a_32:
    and     $03                   ; Remove extraneous b
    rla                           ; Shift palette # to bits 5 and 6
    rla
    rla
    rla
    rla
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
; Shift HL Right
;   Input: B = Shift Count
;  Output: B = 0
; Changes: HL
;-----------------------------------------------------------------------------
shift_hl_right:
    sra     h
    rr      l
    djnz    shift_hl_right
    ret


;-----------------------------------------------------------------------------
; Push HL and Return Null String
;-----------------------------------------------------------------------------
;pop_ret_nullstr:
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

;----------------------------------------------------------------------------
; Enable user interrupts
; Clobbers: A,BC
;----------------------------------------------------------------------------
userint_enable:
    ld      b,IRQ_USER
    jr      _enable_irq

;----------------------------------------------------------------------------
; Diswable user interrupts
; Clobbers: A,BC
;----------------------------------------------------------------------------
userint_disable:
    ld      b,IRQ_USER
    jr      _clear_irq

;----------------------------------------------------------------------------
; Start background PT3 player
; Input C,DE = Timer count
; Clobbers: A,B
;----------------------------------------------------------------------------
pt3_start:
    ld      iy,track_reset
    call    aux_call
pt3_enable:
    ld      b,IRQ_TRACKER
_enable_irq:
    jp      enable_vblank_irq

;----------------------------------------------------------------------------
; End background PT3 player interrupts
; Clobbers: A,BC
;----------------------------------------------------------------------------
pt3_disable:
    ld      b,IRQ_TRACKER
_clear_irq:
    jp      clear_vblank_irq

      
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
; Flags: NZ if stopped 
; Clobbers: A
;----------------------------------------------------------------------------
timer_read:
    ld      de,(TIMERCNT)         ; E = LoByt, D = MdByt
    ld      a,(TIMERCNT+2)        ; A = HiByt
    or      a                     ; If Timer >= 0
    ld      c,a                   ;  
    ret     p                     ; Return CDE = Timer
_tstopped:
    ld      a,0                   
    ld      c,a
    ld      d,a
    ld      e,a                   ; Return S, NZ, CDE = 0 
    ret

