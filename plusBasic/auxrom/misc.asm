;====================================================================
; Miscellaneous Assembly Calls used by BASIC
;====================================================================

;-----------------------------------------------------------------------------
; Return bit status in 23 bit integer
; Input: A: Bit# (0 - 23)
;      CDE: 24 bit Long
; Output: A: -1 if set, 0 if not set
; Flags: Carry set if A > 23
; Clobbered: BC, DE
;-----------------------------------------------------------------------------
bool_checkbit_long:
    ld      b,a                   ; B = BitNo
    ld      a,23
    cp      b                     ; If BitNo > 23
    ret     c                     ;   Return Carry Set
    inc     b                     ; Bump BitNo for Loop
    xor     a                     ; Result = 0
.loop
    srl     c
    rr      d
    rr      e                     ; Carry = Next Bit
    djnz    .loop                 ; Countdown and loop
    ccf                           ; Carry = ! Bit
    adc     $FF                   ; Convert bit to 0/$FF
    or      a                     ; Clear carry
    ret

;-----------------------------------------------------------------------------
; Return bit status in string
; Input: BC: string length (0 to 255)
;        DE: string address
;        HL: bit# (0 - 2039)
; Output: A: -1 if set, 0 if not set
; Flags: Carry set if HL > 2039
; Clobbered: AF',BC,DE,HL
;-----------------------------------------------------------------------------
bool_checkbit_string:
    push    de                    ; Stack = StrAdr, RtnAdr
    ld      de,2040
    rst     COMPAR
    ccf
    pop     de                    ; DE = StrAdr; Stack = RtnAdr
    ret     c                     ; If BitNo > 2039, return Carry Set
    ld      a,l
    and     $07                   ; A = ChrBit
    push    af                    ; Stack = ChrBit, RtnAdr
    ld      b,3
    call    shift_hl_right        ; H = ChrPos
    ld      a,l
    cp      c                     ; If ChrPos+1 > StrLen
    ccf
    jp      c,POPHRT              ;   Return Carry Set
    ld      h,0
    add     hl,de                 ; HL = ChrAdr
    pop     bc                    ; B = ChrBit; Stack = RtnAdr
    inc     b                     ; Bump BitNo for Loop
    ld      a,(hl)                ; A = StrChr
.loop
    rra
    djnz    .loop
    ccf                           ; Carry = ! Bit
    ld      a,0
    adc     $FF                   ; Convert bit to 0/$FF
    or      a                     ; Clear carry
    ret

;-----------------------------------------------------------------------------
; Pause program execution
; Input: A: Diaable Ctrl-C 
;       DE: Number of jiffies
; Clobbered: A, BC, DE, L
;-----------------------------------------------------------------------------
pause_jiffies: 
    ld        l,a                 ; L = NoCtrl
    ld        bc,$00FF
.loop
    ld        a,d                 ; If DE = 0
    or        e                   ;   Return  
    ret       z                   
    ld        a,l                 ; A = NoCtrl
    or        a
    jr        nz,.wait            ; Ctrl-C enabled
    in        a,(c)               ;   Poll keyboard
    cp        $CF                 ;   If Ctrl-C
    ret       z                   ;     Return
.wait
    in        a,(IO_VLINE)        
    inc       a                   ; Wait for video line 255
    jr        nz,.wait      
.wait0
    in        a,(IO_VLINE)        
    dec       a                   ; Wait for video line 1
    jr        nz,.wait0      
    dec       de                  ; Count down
    jr        .loop               ; and loop

;-----------------------------------------------------------------------------
; Read game controller
; Input: A: Controller ID (0: Both, 1: Left, 2: Right)
; Output: A, E: Controller state with bits inverted
; Clobbered: BC
;-----------------------------------------------------------------------------
read_gamepad_e:
    ld      a,e
read_gamepad:
    or      a
    jr      nz, .joy01
    ld      a, $03
.joy01:
    ld      e, a
    ld      bc, $00F7
    ld      a, $FF
    bit     0, e
    jr      z, .joy03
    ld      a, $0e
    out     (c), a
    dec     c
    ld      b, $FF
.joy02:
    in      a,(c)
    djnz    .joy02
    cp      $FF
    jr      nz, .joy05
.joy03:
    bit     1,e
    jr      z, .joy05
    ld      bc, $00F7
    ld      a, $0F
    out     (c), a
    dec     c
    ld      b, $FF
.joy04:
    in      a, (c)
    djnz    .joy04
.joy05:
    xor     $FF                    ; Invert bits and set flags
    ld      e,a
    ret

;-----------------------------------------------------------------------------
; Read game controller D-Pad
; Input: A: Controller ID (0: Both, 1: Left, 2: Right)
; Output: A: D-Pad direction (1 - 16), 0 if none
; Clobbered: BC, DE
;-----------------------------------------------------------------------------
read_game_dpad_e:
    ld     a,e
read_game_dpad:
    call    read_gamepad          ; A = ConVal
decode_game_dpad_e:
    ld      a,e
decode_game_dpad:
    or      a
    ret     z                     ; Return 0 if nothing pressed
    rla                           ; If Bit 7 set (Keys 2, 3, 5, or 6)
    jr      c,_retzero            ;   Return 0
    rra                           ; Put bits back
    and     $1F                   ; Strip button bits
    ld      d,high(dpad_table)
    ld      e,a                   ; DE = TblIdx
    ld      a,(de)                ; A = PadDir
_retflags:
    or      a                     ; Set Flags
    ret
_retzero:
    xor     a
    ret

;-----------------------------------------------------------------------------
; Read game controller D-Pad
; Input: A: Controller ID (0: Both, 1: Left, 2: Right)
; Output: A: Button (1-6), 0 for none
; Clobbered: BC, DE
;-----------------------------------------------------------------------------
read_game_button_e:
    ld     a,e
read_game_button:
    call    read_gamepad          ; A = ConVal
decode_game_btn_e:
    ld      a,e
decode_game_button:
    or      a
    ret     z                     ; Return 0 if nothing pressed
    push    hl                    ; Save HL
    rla                           ; Bit7 = K1, Bit6 = K4, Carry = Other
    jr      c,.lookup             ; If Buttons 1 or 4
    ld      b,1
    rla                           ;   Bit7 = K4, Carry = K1
    jr      c,.ret_b              ;   
    ld      b,4
    rla                           ;   Bit7 = K4, Carry = K1
    jr      c,.ret_b              ;   
.lookup
    rra                           ; Put bits back
    ld      b,6
    ld      hl,button_table
.loop
    cp      a,(hl)                
    jr      z,.ret_b              
    inc     hl
    djnz    .loop
.ret_b:
    pop     hl                    ; Restore HL
    ld      a,b
    or      a
    ret




    

;-----------------------------------------------------------------------------
; Read keys into buffer
; Input: HL: Buffer address
; Output: A,C: String length
; Clobbered: A
;-----------------------------------------------------------------------------
read_keys:
    ld      c,0
.loop
    call    INCHRH
    ld      (hl),a
    inc     hl
    or      a
    ret     z
    inc     c
    jr      nz,read_keys
    xor     a
    ld      (hl),a
    ld      a,c
    ret

;-----------------------------------------------------------------------------
; Scan for ELSE
;  Input: HL = TxtPtr (pointing at character after THEN)
; Output: Terminating 0 byte or Character after ELSE
;  Flags: As set by CHRGET
;-----------------------------------------------------------------------------
scan_else:
    ld      a,(hl)                ; A = Current character
    or      a                     ; If null terminator
    ret     z                     ;   Return Z set
    cp      a,ELSETK              ; If ELSE
    jp      z,CHRGTR              ;   Return next character with flags set
    cp      '"'                   ; If quote
    jr      z,.skip_string        ;   Skip to end of literal string
    cp      92                    ; If backslash
    jr      z,.skip_escaped       ;   Skip to end of escaped string
.next
    inc     hl
    jr      scan_else

.skip_string
    inc     (hl)
    ld      a,(hl)                ; A = Next Character
    or      a                     ; If null terminator
    ret     z                     ;   Return Z set
    cp      '"'                   ; If quote
    jr      z,.next               ;   Skip it and continue
   
.skip_escaped
    inc     (hl)
    ld      a,(hl)                ; A = Next Character
    or      a                     ; If null terminator
    ret     z                     ;   Return Z set
    cp      92                    
    jr      z,.not_escape         ; If backslash
    inc     (hl)
    ld      a,(hl)                ;   Get following character
    or      a                     ;   If null terminator
    ret     z                     ;     Return Z set
    jr      .skip_escaped         ;   Else skip and check next 
.not_escape
    cp      '"'                   ; Else If quote
    jr      z,.next               ;   Skip and continue
    jr      .skip_escaped         ; Else skip and check next
