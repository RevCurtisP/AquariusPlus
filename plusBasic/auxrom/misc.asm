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
    ld      b,5
    call    shift_hl_left         ; H = ChrPos
    ld      a,h
    cp      c                     ; If ChrPos > StrLen
    ret     c                     ;   Return Carry Set
    ld      l,a
    ld      h,0
    add     hl,de                 ; HL = ChrAdr
    pop     bc                    ; B = ChrBit
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
; Input: A: Controler ID (0: Both, 1: Left, 2: Right)
;           0: Botj
; Output: A: Controller state with bits inverted
; Clobbered: B, E
;-----------------------------------------------------------------------------
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
    cpl
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

