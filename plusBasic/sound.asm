;=====================================================================================
; Sound and Music Routines
;=====================================================================================



;-----------------------------------------------------------------------------
; Play Sample in Paged Memory
; Input: A: Page
;        C: Delay
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