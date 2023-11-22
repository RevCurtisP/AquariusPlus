;=====================================================================================
; Sound and Music Routines
;=====================================================================================



;-----------------------------------------------------------------------------
; Play Sample in Main Memory
; Input: C: Delay
;       DE: Length in bytes
;       HL: Start address
;-----------------------------------------------------------------------------
play_raw:
.loop
    ld      a,d                   ; 4    4       ; CPU Cycles = 56 + 13 * Delay
    or      e                     ; 4	   8       ; Rate = 3579545 / (56 + 13 * Delay)
    jr      z,.done               ; 7   15       
    dec     de                    ; 6	  21      
    ld      a,(hl)                ; 7	  28
    inc     hl                    ; 6	  34
    out     (IO_PCMDAC),a         ; 11  45
    ld      b,c                   ; 4   49
.delay
    djnz    .delay                ; 13 * B 
    jr      .loop                 ; 12  61 - 5 
    
.done
    out     (IO_PCMDAC),a
    ret
    
    
;-----------------------------------------------------------------------------
; Play Sample in Paged Memory
; Input: A: Page
;        C: Delay
;       DE: Length in bytes
;       HL: Start address
; Clobbers: BC,DE,HL
;-----------------------------------------------------------------------------
play_paged:
    ld      b,a                   ; B = SampPg
    in      a,(IO_BANK1)          ; A = CrntPg
    push    af                    ; Stack = CrntPg, RtnAdr
    ld      a,b                   ; A = SampPg
    out     (IO_BANK1),a          ; Map SampPg into Bank 1
    ld      a,h                   ; Coerce start address in Bank 1
    and     $3F
    or      $40                   ; 
    ld      h,a                   ; 
.loop
    ld      a,d                   ;  4   4    ; CPU Cycles = 105 + 13 * Delay
    or      e                     ;  4	 8    ; Rate = 3579545 / (105 + 13 * Delay)
    jr      z,.done               ;  7  15       
    dec     de                    ;  6	21      
    ld      a,(hl)                ;  7	28
    out     (IO_PCMDAC),a         ; 11  39
    ld      b,c                   ;  4  43
.delay
    djnz    .delay                ; 13 * B 
                                  ; -5  38
    inc     hl                    ;  6	44
    bit     7,h                   ;  8  52    ; If H is $80
    jr      z,.nocross            ;  7  59    ;   L will be $00 

    in      a,(IO_BANK1)          ; 11  70
    srl     h                     ;  8	78    ;   HL = $4000
    inc     a                     ;  4	82    ;   Map next page into Bank 1
    out     (IO_BANK1),a          ; 11  93
    jr      .loop                 ; 12 105    ; Else

.nocross                          ;  5  64    ; need 29
    push    ix                    ; 15  79
    pop     ix                    ; 14  93
    jr      .loop                 ; 12 105    
    
.done
    out     (IO_PCMDAC),a
    pop     af                    ; A = OrigPg; Stack = RtnAdr
    out     (IO_BANK1),a          ; Restore original page
    ret
    
