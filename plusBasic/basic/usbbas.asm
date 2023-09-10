;====================================================================
; Statements and Functions from USB BASIC
;====================================================================


;-----------------------------------------------------------------------------
; ST_CALL
;
; syntax: CALL address
; address is signed integer, 0 to 32767   = $0000-$7FFF
;                            -32768 to -1 = $8000-$FFFF
;
; on entry to user code, HL = text after address
; on exit from user code, HL should point to end of statement
;-----------------------------------------------------------------------------
ST_CALL:
    call    FRMNUM           ; Get number from BASIC text
    call    FRCINT           ; Convert to 16 bit integer
    push    de
    ret                      ; Jump to user code, HL = BASIC text pointer

;-----------------------------------------------------------------------------
; HEX$() function
; eg. A$=HEX$(B)
;-----------------------------------------------------------------------------
FN_HEX:
    rst     CHRGET            ; Skip Token and Eat Spaces
    call    PARCHK
    call    GETYPE          ; Get Type of Argument
    jr      z,HEX_STRING    ; If String, Convert It and Return
    push    hl              ; Stack = TxtPtr
    push    bc              ; Stack = DummyRtn, TxtPtr
    call    FRCINT          ; Evaluate formula @HL, result in DE
    ld      hl, FPSTR       ; HL = temp string
    ld      a, d
    or      a               ; > zero ?
    jr      z, .lower_byte
    ld      a, d
    call    byte_to_hex     ; Yes, convert byte in D to hex string
.lower_byte:
    ld      a, e
    call    byte_to_hex     ; Convert byte in E to hex string
    ld      (hl), 0         ; Null-terminate string
    ld      hl, FPSTR
.create_string:
    jp      TIMSTR                ; Create BASIC string

HEX_STRING:
    push    hl              ; Stack = TxtPtr
    push    bc              ; Stack = DummyRtn, TxtPtr
    call    free_addr_len   ; BC = StrLen, DE = StrAdr, HL = StrDsc
    ex      de,hl           ; HL = StrAdr
    ld      a,c             ; A = ArgLen
    or      a               ; If Length is 0
    jr      z,_null_string  ;   Return Null String
    push    hl              ; Stack=Arg Text Address
    push    af              ; Stack=Arg Length, Arg Text Address
    add     a,a             ; New String will be Twice as long
    jp      c,LSERR         ; LS Error if greater than 255
    call    STRINI          ; Create Result String returning HL=Descriptor, DE=Text Address
    pop     af              ; A=Arg Length, Stack=Arg Text Address
    pop     hl              ; HL=Arg Text Address
    ex      de,hl           ; DE=Arg Text Address, HL=Result Text Address
    ld      b,a             ; Loop through Arg String Text 
.hexloop:
    ld      a,(de)          ; Get Arg String Character
    inc     de              ; and Bump Pointer
    call    _hexbyte        ; Convert to Hex in Result String
    djnz    .hexloop        ; Loop until B=0
    jp      FINBCK          ; Return Result String

_hexbyte:
    ld      c,a
    rra
    rra
    rra
    rra
    call    .hex
    ld      a,c
.hex:
    and     $0f
    cp      10
    jr      c,.chr
    add     7
.chr:
    add     '0'
    ld      (hl),a
    inc     hl
    ret

_null_string:
    ld      hl,REDDY-1    ; Point at null terminator
    jp      TIMSTR        ; and return null string


;-----------------------------------------------------------------------------
; IN() function
; syntax: var = IN(port)
;-----------------------------------------------------------------------------
FN_IN:
    rst     CHRGET                ; Skip Token and Eat Spaces
    cp      KEYTK                 ; If followed by KEY
    jp      z,FN_INKEY            ; It's INKEY
    call    PARCHK
    push    hl
    ld      bc,LABBCK
    push    bc
    call    FRCINT           ; Evaluate formula pointed by HL, result in DE
    ld      b, d
    ld      c, e             ; BC = port

    ; Read from port
    in      a, (c)           ; A = in(port)
    jp      SNGFLT           ; Return with 8 bit input value in variable var

;-----------------------------------------------------------------------------
; LOCATE statement
; Syntax: LOCATE col, row
;-----------------------------------------------------------------------------
ST_LOCATE:
    call    GETBYT              ; Read number from command line (column). Stored in A and E
    push    af                  ; Column store on stack for later use
    dec     a
    cp      38                  ; Compare with 38 decimal (max cols on screen)
    jp      nc, FCERR           ; If higher then 38 goto FC error

    ; Expect comma
    SYNCHK  ','

    call    GETBYT              ; Read number from command line (row). Stored in A and E
    cp      $18                 ; Compare with 24 decimal (max rows on screen)
    jp      nc,FCERR            ; If higher then 24 goto FC error

    inc     e
    pop     af                  ; Restore column from store
    ld      d, a                ; Column in register D, row in register E
    ex      de, hl              ; Switch DE with HL
    call    move_cursor         ; Cursor to screen location HL (H=col, L=row)
    ex      de, hl
    ret

;-----------------------------------------------------------------------------
; OUT statement
; syntax: OUT port, data
;-----------------------------------------------------------------------------
ST_OUT:
    call    FRMNUM              ; Get/evaluate port
    call    FRCINT              ; Convert number to 16 bit integer (result in DE)
    push    de                  ; Stored to be used in BC

    ; Expect comma
    SYNCHK  ","

    call    GETBYT              ; Get/evaluate data
    pop     bc                  ; BC = port
    out     (c), a              ; Out data to port
    ret
