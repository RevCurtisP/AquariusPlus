;=====================================================================================
; Enhanced BASIC Statements and Functions
;======================================================================================

ST_POKE
    cp      '!'                   ; If POKE!
    jr      z,.pokeword           ;   Poke a word
    call    parse_page_arg        ; Parse page
    push    af                    ; Save it
    call    GETINT                ; Parse Address
    push    de                    ; Save It
    SYNCHK  ','                   ; Require comma
    call    GETBYT                ; Parse Byte
    ld      c,a                   ; and put into C
    pop     de                    ; Get address
    pop     af                    ; Get page
    jp      c,page_write_byte     ; If page specified, write to it
    ld      a,c                   ; Write byte
    ld      (de),a                ; to address
    ret
.pokeword    
    rst     CHRGET                ; Eat the !
    call    parse_page_arg        ; Parse page
    push    af                    ; Save it
    call    GETINT                ; Parse Address
    push    de                    ; Save It
    SYNCHK  ','                   ; Require comma
    call    GETINT                ; Parse Word
    ld      b,d                       
    ld      c,e                   ;   
    pop     de                    ; Get address
    pop     af                    ; Get page
    jp      c,page_write_word     ; If page specified, write to it
    ld      a,c                   ; Write byte
    ld      (de),a                ; to address
    inc     de
    ld      a,b                   ; Write byte
    ld      (de),a                ; to address
    ret
    ret
    
FN_PEEK:
    rst     CHRGET                ; Skip token
    cp      '!'                   ; If POKE!
    jr      z,.peekword           ;   Poke a word
    SYNCHK  '('                   ; Require open paren
    call    parse_page_arg        ; Parse page
    push    af                    ; Save it
    call    GETINT                ; Parse Integer
    SYNCHK  ')'                   ; Require close paren
    pop     af                    ; Get page
    push    hl                    ; Save text pointer
    ld      bc,LABBCK             ; Return address for SNGFLT
    push    bc
    jp      c,.get_page_byte      ; If not specified
    ld      a,(de)                ;   Get Byte
.float_it:
    jp      SNGFLT                ; and float it

.get_page_byte:
    call    page_read_byte        ; Read byte into C
    ld      a,c
    jr      .float_it

.peekword:
    rst     CHRGET                ; Skip token
    SYNCHK  '('                   ; Require open paren
    call    parse_page_arg        ; Parse page
    push    af                    ; Save it
    call    GETINT                ; Parse Integer
    SYNCHK  ')'                   ; Require close paren
    pop     af                    ; Get page
    push    hl                    ; Save text pointer
    ld      bc,LABBCK             ; Return address for SNGFLT
    push    bc
    jr      c,.read_page_word     ; If not specified
    ld      a,(de)                ;   Get LSB
    ld      c,a
    inc     de
    ld      a,(de)
    jr      .float_it
.read_page_word
    call    page_read_word
    ld      a,b 
    jp      GIVINT

; Check for and parse @page,
; Output: A, E = Page (0 if not specified)
parse_page_arg:
    cp      '@'                   
    jr      nz,.notat             ; If page prefix
    rst     CHRGET                ;   Skip '@'
    call    GETBYT                ;   Parse byte into E
    SYNCHK  ','                   ;   Require comma
    ld      a,e
    scf
    ret
.notat
    or      a                     ; Set Flags
    ret     
