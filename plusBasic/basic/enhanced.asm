;=====================================================================================
; Enhanced BASIC Statements and Functions
;======================================================================================

ST_POKE:
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
    jr      c,.write_paged_byte   ; If page specified, write to it
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
    pop     af                    ; Get page number
    jr      c,.write_paged_word   ; If page specified, write to it
    ld      a,c                   ; Get LSB
    ld      (de),a                ; Write to address
    inc     de
    ld      a,b                   ; Get MSV
    ld      (de),a                ; Write to address
    ret

.write_paged_byte:
    call    page_write_byte     ; If page specified, write to it
    jp      z,FCERR             ; FC error if illegal page
    ret

.write_paged_word  
    call    page_write_word       ; If page specified, write to it
    jp      z,FCERR               ; FC error if illegal page
    jp      c,OVERR               ; Return overflow error if end of RAM 
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
    jp      c,.get_page_byte      ; If page not specified
    ld      a,(de)                ;   Get Byte
.float_it:
    jp      SNGFLT                ; and float it

.get_page_byte:
    call    page_read_byte        ; Read byte into C
    jp      z,FCERR               ; FC error if illegal page
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
    ld      b,c
    jp      FLOAT_DE

.read_page_word
    call    page_read_word
    jp      z,FCERR               ; FC error if illegal page
    jp      c,OVERR               ; Return overflow error if end of RAM 
    jp      FLOAT_BC              ; Float word and return

; Check for and parse @page,
; Output: A, E = Page number`
;  Carry: Set if page specified
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
