;====================================================================
; Graphics Bitmap GET and PUT
;====================================================================

;-----------------------------------------------------------------------------
; Read Bitmap Screen Section into Buffer
; Input: BMPMODE: Mode: 0 = 1bpp, 1 = 4bpp
;        STARTCOL: Start Column
;        STARTROW: Start Row
;        HL: End Row
;        ENDCOL: End Row
;        BUFADR: Buffer Address
;        BUFLEN: Buffer Length
;  Flags: Carry Set if invalid coordinates
;         Not Zero if buffer overflow
; Clobbered: A, AF', BC, DE, HL, IX, IY
;--------------`---------------------------------------------------------------
bitmap_get:
    call    get_set_init         ; If coordinates out of range
    ret     c                     ;   Return Carry Set
    ret
    
    jr      nz,.get_4bpp          ; If A <> 0

.get_4bpp
    
bitmap_put:
    ret
    
