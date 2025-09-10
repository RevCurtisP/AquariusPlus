;=====================================================================================
; Sound and Music Routines
;=====================================================================================

;-----------------------------------------------------------------------------
; Input: A: $FF for ON, 0 for OFF
;-----------------------------------------------------------------------------
set_soundfast:
    ld      a,(EXT_FLAGS)         ; Read Extended Flags
    jr      z,.off                ; If ON
    and     $FF-SOUNDSLOW         ;   Clear Slow Sound bit
    jr      .done                 ; Else
.off    
    or      a,SOUNDSLOW           ;   Set Slow Sound bit
.done
    ld      (EXT_FLAGS),a         ; Write Extended Flags
    ret

;-----------------------------------------------------------------------------
; Output: A: $FF for ON, 0 for OFF
;-----------------------------------------------------------------------------
get_soundfast:
    ld      a,(EXT_FLAGS)         ; Read Extended Flags
    and     SOUNDSLOW             ;   Isolate Slow Sound bit
    jr      z,.done               ; If Set
    ld      a,$FF                 ;   Return 0
.done    
    xor     $FF                   ; Else Return $FF
    ret


;-----------------------------------------------------------------------------
; Play Raw Sample in Paged Memory
; Input: A: Page
;       HL: Start address
;
; Sample Format:
;  0-1   Sample Length in Bytes
;   2    0
;   3    Sample Rate index (see delay table)
;-----------------------------------------------------------------------------
play_sample:
    ld      b,a                   ; B = SampPg    
    in      a,(IO_BANK1)          ; A = OrigPg
    push    af                    ; Stack = OrigPg, RtnAdr
    ld      a,b                   ; A = SampPg
    out     (IO_BANK1),a          ; Map SampPg into Bank 1
    ld      a,h                   ; Coerce start address in Bank 1
    and     $3F
    or      $40                   ;
    ld      h,a                   ;

    ld      e,(hl)                ; Length LSB
    inc     hl
    ld      d,(hl)                ; Length MSB
    inc     hl
    inc     hl
    ld      c,(hl)                ; Rate index
    inc     hl
    push    hl                    ; Stack = Length, OrigPg, RtnAdr
    ld      hl,_delay_table
    ld      b,0                   ; BC = Index
    add     hl,bc                 ; HL = Address in Table
    ld      c,(hl)                ; C = Delay value
    pop     hl                    ; HL = Length; Stack = OrigPg, RtnAdr
    di                            ; Disable interrupts
    in      a,(IO_SYSCTRL)
    and     $7F                   ; Strip Reset bit
    push    af                    ; Stack = SysCtrl, OrigPg, RtnAdr
    and     ~SYSCTRL_TURBO
    out     (IO_SYSCTRL),a
;-----------------------------------------------------------------------------
;  A: Page
;  C: Delay
; DE: Length in bytes; Clobbers: BC,DE,HL
; HL: Start address
;-----------------------------------------------------------------------------
.loop
    ld      a,$6F                 ;  7	 7
    in      a,($FF)               ; 11  18
    sub     $CF                   ;  7	25
    jp      z,.done               ; 10  35
    ld      a,d                   ;  4  39    ; CPU Cycles = 105 + 13 * Delay
    or      e                     ;  4	43    ; Rate = 3579545 / (105 + 13 * Delay)
    jr      z,.done               ;  7  50
    dec     de                    ;  6	56
    ld      a,(hl)                ;  7	63
    out     (IO_PCMDAC),a         ; 11  74
    ld      b,c                   ;  4  78
.delay
    djnz    .delay                ; 13 * B
                                  ; -5  73
    inc     hl                    ;  6	79
    bit     7,h                   ;  8  87    ; If H is $80
    jr      z,.nocross            ;  7  94    ;   L will be $00

    in      a,(IO_BANK1)          ; 11 105
    srl     h                     ;  8 113    ;   HL = $4000
    inc     a                     ;  4 117    ;   Map next page into Bank 1
    out     (IO_BANK1),a          ; 11 128
    jr      .loop                 ; 12 140    ; Else

.nocross                          ;  5  99    ; need 29
    push    ix                    ; 15 114
    pop     ix                    ; 14 128
    jr      .loop                 ; 12 140    ; NEW = 140

.done
    ld      a,128                 ; Avoid clicks
    out     (IO_PCMDAC),a
    pop     af                    ; A = SysCtrl; Stack = OrigPg, RtnAdr
    out     (IO_SYSCTRL),a
    pop     af                    ; A = OrigPg; Stack = RtnAdr
    out     (IO_BANK1),a          ; Restore original page
    ld      a,(IRQACTIVE)
    or      a
    ret     z                     ; If any IRQ routines are active
    ei                            ;   Enable interrupts
    ret

;-----------------------------------------------------------------------------
; Sample Rate to Delay lookup table
;-----------------------------------------------------------------------------
_delay_table:                     
    byte    58                    ; 0  4000    4003.97
    byte    35                    ; 1  6000    6016.04
    byte    24                    ; 2  8000    7919.35
    byte    14                    ; 3 11000   11116.6
    byte    12                    ; 4 12000   12093.1
    byte     9                    ; 5 14000   13928.2
    byte     6                    ; 6 16000   16419.9
    byte     3                    ; 7 20000   19997.5
