;-----------------------------------------------------------------------------
; string.asm - string manipulation routines
;-----------------------------------------------------------------------------

;----------------------------------------------------------------------------
; Search for string in string
;  Input: A = Offset (base 1)
;        DE = Needle descriptor
;        HL = Haystack descriptor
; Output: A = Position of needle in haystack (0 = not found)
; Clobbered: BC, DE, HL
;----------------------------------------------------------------------------
string_search:
    ld      b,a                   ; B = OffCnt
    dec     a                     ; Back up Offset
    ld      c,a                   ; C = Offset
    cp      (hl)                  ; If Ofs > HayLen
    ld      a,0                   ;   Return 0
    ret     nc
    ld      a,(de)                ; A = NdlLen
    or      a                     ; Set flags
    ld      a,b                   ; A = Offset
    ret     z                     ; Return 0 if NdlLen = 0
    ld      a,(hl)                ; A = HayLen
    inc     hl
    inc     hl                    ; Bump to text addrss
    ld      b,(hl)
    inc     hl
    ld      h,(hl)
    ld      l,b                   ; HL = HayAdr
    ld      b,0                   ; BC = Offset
    add     hl,bc                 ; HL = HayPtr
    sub     c                     ; Adjust Haylen
    ld      b,a                   ; B = HayLen
    push    bc                    ; Stack = Offset, OffCnt, RtnAdr
    push    de                    ; Stack = NdlDsc, Offset, OffCnt, RtnAdr
    ex      (sp),hl               ; HL = NdlDsc; Stack = HayPtr, Offset, OffCnt, RtnAdr
    ld      c,(hl)                ; C = NdlLen
    inc     hl
    inc     hl
    ld      e,(hl)
    inc     hl                    ; DE = NdlAdr
    ld      d,(hl)
    pop     hl                    ; HL = HayAdr; Stack = OfsOfc, RtnAdr
.cnt_loop
    push    hl                    ; Stack = HayAdr, OfsOfc, RtnAdr
    push    de                    ; Stack = NdlAdr, HayAdr, OfsOfc, RtnAdr
    push    bc                    ; Stack = HayLen, NdlLen, HayAdr, Offset, OffCnt, RtnAdr
.ofs_loop
    ld      a,(de)                ; A = NdlChr
    cp      (hl)                  ;  If NdlChr <> HayChr
    jp      nz,.notfound          ;    Return 0
    inc     de                    ; Bump NdlPtr
    dec     c                     ; Decrement NdlLen
    jp      z,.found              ; If not end of NdlStr
    inc     hl                    ;   Bump HayPtr
    dec     b                     ;   Decrement HayCnt
    jp      nz,.ofs_loop          ;   If HayCnt <> 0, check next character
.pop3ret0
    pop     de                    ; Stack = NdlLen, HeyAdr, OfsOfc, RtnAdr
    pop     de                    ; Stack = HeyAdr, OfsOfc, RtnAdr
    pop     bc                    ; Stack = OfsOfc, RtnAdr
.pop1ret0
    pop     de                    ; Stack = RtnAdr
    xor     a                     ; Return 0 with flags set
    ret                           ;
.found
    pop     hl                    ; Stack = NdlLen, HayAdr, Offset, OffCnt, RtnAdr
    pop     de                    ; Stack = HayAdr, OfsOfc, RtnAdr
    pop     de                    ; Stack = OfsOfc, RtnAdr
    pop     bc                    ; B = Offset
    ld      a,b                   ;
    sub     h                     ;
    add     c                     ;
    inc     a                     ; Return offset of Needle in Haystack
    ret
.notfound
    pop     bc                    ; B = HayLen; Stack = NdlLen, HayAdr, OfsOfc, RtnAdr
    pop     de                    ; DE = NdlAdr; Stack = HayAdr, OfsOfc, RtnAdr
    pop     hl                    ; HL = HayAdr; Stack =  OfsOfc, RtnAdr
    inc     hl                    ; Bump HayAdr
    dec     b                     ; Decrement HayLen
    jp      nz,.cnt_loop          ; If not at end of NdlTxt, check next character
    jr      .pop1ret0             ; Else discard OfsOfc and return 0

;----------------------------------------------------------------------------
; Search for string in array
;  Input: A = String Length
;        BC = Array Count
;        DE = Array Address
;        HL = String Address
; Output: DE = Address of array entry
;     BC, HL = Index into array
;----------------------------------------------------------------------------
string_search_array:
    push    bc                    ; Stack = ArySiz, RetAdr
.loop
    ex      af,af'
    ld      a,b
    or      c
    dec     bc                    ; Decrement AryCnt
    jp      z,discard_ret         ; Return if -1
    ex      af,af'
    push    bc                    ; Stack = AryCnt, ArySiz, RetAdr
    push    af                    ; Stack = StrLen, AryCnt, ArySiz, RetAdr
    push    hl                    ; Stack = StrAdr, StrLen, AryCnt, ArySiz, RetAdr
    push    hl                    ; Stack = StrAdr, StrAdr, StrLen, AryCnt, ArySiz, RetAdr
    ld      b,a
    ex      de,hl                 ; HL = AryPtr
    ld      a,(hl)                ; A = ElmLen
    inc     hl
    inc     hl
    ld      e,(hl)
    inc     hl
    ld      d,(hl)                ; DE = ElmAdr
    inc     hl
    ex      (sp),hl               ; HL = StrAdr; Stack = AryPtr, StrAdr, StrLen, AryCnt, ArySiz, RetAdr
    cp      a,b
    jr      nz,.next              ; If StrLen = ElmLen
.compare
    ld      a,(de)
    inc     de
    cp      (hl)
    inc     hl
    jr      nz,.next
    djnz    .compare
    pop     de                    ;   Stack = StrAdr, StrLen, AryCnt, ArySiz, RetAdr
    pop     de                    ;   DE = StrAdr; Stack = StrLen, AryCnt, ArySiz, RetAdr
    pop     af                    ;   A = StrLen; Stack = AryCnt, ArySiz, RetAdr
    pop     bc                    ;   BC = AryCnt; Stack = ArySiz, RetAdr
    pop     hl                    ;   HL = ArySiz; Stack = RetAdr
    or      a                     ;   Clear flags
    sbc     hl,bc                 ;   HL = AryIdx
    dec     hl                    ;   Zero adjust AryIdx
    ld      b,h
    ld      c,l                   ;   Return BC = AryIdx
    ret
.next
    pop     de                    ; DE = AryPtr; Stack = StrAdr, StrLen, AryCnt, ArySiz, RetAdr
    pop     hl                    ; HL = StrAdr; Stack = StrLen, AryCnt, ArySiz, RetAdr
    pop     af                    ; A = StrLen; Stack = AryCnt, ArySiz, RetAdr
    pop     bc                    ; BC = AryCnt; Stack = ArySiz, RetAdr
    jp      .loop

;-----------------------------------------------------------------------------
; Trim whitespace characters from both ends of string
; Input: A: trim character, 0 = whitespace
;       BC: string length
;       DE: string address
; Output: C: trimmed length
;        DE: trimmed address
; Clobbered: HL
;-----------------------------------------------------------------------------
string_trim:
    push    af
    call    string_trim_right
    pop     af
;-----------------------------------------------------------------------------
; Trim whitespace characters from left end of string
; Input: B: trim chars length
;        C: string length
;       DE: trim chars address
;       HL: string address
; Output: C: trimmed length
;        HL: trimmed address
;-----------------------------------------------------------------------------
string_trim_left:
    ld      a,c
    or      a                     ; If null string
    ret     z                     ;   Return
    ld      a,b
    or      a
    jr      z,.whitespace          ; If trim chars
.trim
    push    de                    ; Stack = TrmLst, RtnAdr
    push    bc                    ; Stack = TrmLen, TrmLst, RtnAdr
.loop
    ld      a,(de)
    cp      (hl)
    jr      z,.next
    inc     de
    djnz    .loop
    pop     bc                    ; BC = TrmLen; Stack = TrmLst, RtnAdr
    pop     de                    ; DE = TrmLst; Stack = RtnAdr
    ret
.next
    inc     hl
    pop     bc                    ; BC = TrmLen; Stack = TrmLst, RtnAdr
    pop     de                    ; DE = TrmLst; Stack = RtnAdr
    dec     c
    ret     z
    jp      .trim
.whitespace
    ld      a,(hl)
    cp      ' '+1
    ret     nc
    inc     hl
    dec     c
    jr      nz,.whitespace
    ret

;-----------------------------------------------------------------------------
; Trim whitespace characters from right end of string
; Input: B: trim chars length
;        C: string length
;       DE: trim chars address
;       HL: string address
; Output: C: trimmed length
;        HL: trimmed address
;-----------------------------------------------------------------------------
string_trim_right:
    ld      a,c
    or      a                     ; If null string
    ret     z                     ;   Return
    push    hl                    ; Stack = StrAdr, RetAdr
    push    hl                    ; Stack = StrAdr, StrAdr, RetAdr
    ld      hl,POPHRT
    ex      (sp),hl               ; HL = StrAdr; Stack = POPHRT, StrAdr, RetAdr
    push    bc                    ; Stack = StrTrmLen, POPHRT, StrAdr, RetAdr
    ld      b,0
    add     hl,bc                 ; HL = StrPtr (byte after end)
    pop     bc                    ; Stack = POPHRT, StrAdr, RetAdr
    ld      a,b
    or      a
    jr      z,.whitespace         ; If trim chars
.trim
    push    de                    ; Stack = TrmLst, POPHRT, StrAdr, RtnAdr
    push    bc                    ; Stack = TrmLen, TrmLst, POPHRT, StrAdr, RtnAdr
    dec     hl                    ; Back up StrPtr
.loop
    ld      a,(de)
    cp      (hl)
    jr      z,.next
    inc     de
    djnz    .loop
    pop     bc                    ; BC = TrmLen; Stack = TrmLst, POPHRT, StrAdr, RtnAdr
    pop     de                    ; DE = TrmLst; Stack = POPHRT, StrAdr, RtnAdr
    ret
.next
    pop     bc                    ; BC = TrmLen; Stack = TrmLst, POPHRT, StrAdr, RtnAdr
    pop     de                    ; DE = TrmLst; Stack = POPHRT, StrAdr, RtnAdr
    dec     c
    ret     z
    jp      .trim
.whitespace
    dec     hl                      ; Back up to previous character
    ld      a,(hl)
    cp      ' '+1
    ret     nc
    dec     c
    jr      nz,.whitespace
    ret

;-----------------------------------------------------------------------------
; Trim whitespace characters from right end of string
; Input: A: pad character
;       BC: pad length (negative for pad right)
;       DE: buffer address
;       HL: string descriptor
; Output: BC: result length
;-----------------------------------------------------------------------------
string_pad:
    ex      af,af'                ; A' = PadChr
    ld      a,b
    or      c                     ; If PadLen = 0
    ret     z                     ;   Return DE = BufAdr, BC = 0
    ld      a,b
    rla
    jp      c,.padleft            ; If BC >0
    push    bc                    ;   Stack = PadLen, RtnAdr
    push    de                    ;   Stack = BufAdr, PadLen, RtnAdr
    call    .padcount             ;   A = PadCnt, BC = StrLen, DE = StrAdr, HL = PadCnt
    ex      (sp),hl               ;   HL = BufAdr; Stack = PadCnt, PadLen, RtnAdr
    push    hl                    ;   Stack = BufAdr, PadCnt, PadLen, RtnAdr
    ex      de,hl                 ;   HL = StrAdr, DE = BufAdr
    ldir                          ;   DE = BufPtr
    pop     hl                    ;   HL = BufAdr; Stack = PadCnt, PadLen, RtnAdr
    pop     bc                    ;   BC = PadCnt; Stack = PadLen, RtnAdr
    ex      af,af'                ;   A = PadChr
    ex      de,hl                 ;   HL = BufPtr
    call    .pad_it               ;   HL = BufPtr
    pop     bc                    ;   BC = PadLen; Stack = RtnAdr
    ret
.padleft
    push    hl                    ;   Stack = StrDsc, RtnAdr
    or      a                     ;   Clear carry
    ld      hl,0
    sbc     hl,bc                 ;   Padlen = -PadLen
    ld      b,h
    ld      c,l                   ;   BC = PadLen
    pop     hl                    ;   HL = StrDsc; Stack = RtnAdr
    push    bc                    ;   Stack = PadLen, RtnAdr
    push    de                    ;   Stack = BufAdr, PadLen, RtnAdr
    call    .padcount             ;   A = PadCnt, BC = StrLen, DE = StrAdr, HL = PadCnt
    jr      nc,.no_trunc          ;   If StrLen > PadLen
    add     hl,bc                 ;     HL = PadLen
    push    hl                    ;     Stack = PadLen, BufAdr, PadLen, RtnAdr
    ex      de,hl                 ;     HL = StrAdr, DE = PadLen
    add     hl,bc
    sbc     hl,de                 ;     HL = StrPtr
    pop     bc                    ;     BC = PadLen; Stack = BufAdr, PadLen, RtnAdr
    pop     de                    ;     DE = BufAdr; Stack = PadLen, RtnAdr
    jr      .copy                 ;   Else
.no_trunc
    ex      de,hl                 ;     HL = StrAdr, DE = PadCnt
    ex      (sp),hl               ;     HL = BufPtr; Stack = StrAdr, PadLen, RtnAdr
    push    bc                    ;     Stack = StrLen, StrAdr, PadLen, RtnAdr
    ld      b,d
    ld      c,e                   ;     BC = PadLen
    ex      af,af'                ;     A = PadChr
    call    .pad_it               ;     HL = BufPtr
    ex      de,hl                 ;     DE = BufPtr
    pop     bc                    ;     BC = StrLen; Stack = StrAdr, PadLen, RtnAdr
    pop     hl                    ;     HL = StrAdr; Stack = PadLen, RtnAdr
.copy
    ldir
    pop     bc                    ;   BC = PadLen; Stack = RtnAdr
    ret


; In: BC: PadLen, HL: StrDsc
; Out: BC = StrLen, DE = StrAdr, HL = PadCnt
.padcount:
    push    bc                    ; Stack = PadLen, RtnAdr
    call    string_addr_len       ; DE = StrAdr, BC = StrLen
    pop     hl                    ; HL = PadLen; Srack = PadLen, RtnAdr
    or      a                     ; Clear carry
    sbc     hl,bc                 ; HL = PadLen - StrLen
    ret
; In: HL = BufPtr, BC = PadLen; Out HL = BufPtr
.pad_it
    ld      e,a                   ; E = PadChr
    ld      a,b
    or      a                     ; If BC < 0
    ret     m                     ;   Return
.loop
    ld      a,b
    or      c                     ; If BC = 0
    ret     z                     ;   Return
    ld      (hl),e                ; Write PadChr to Buffer
    inc     hl
    dec     bc                    ; Decrement BC
    jr      .loop
