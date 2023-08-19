;=====================================================================================
; Enhanced BASIC Statements and Functions
;======================================================================================

ST_POKE
    call    parse_page_arg        ; Parse page
    push    af                    ; Save it
    call    GETINT                ; Parse Address
    push    de                    ; Save It
    SYNCHK  ','                   ; Require comma
    call    GETBYT                ; Parse Byte
    ld      c,a                   ; and put into C
    pop     de                    ; Get address
    pop     af                    ; Get page
    jp      nz,page_write_byte    ; If specified, write to page
    ld      a,c                   ; Write byte
    ld      (de),a                ; to address
    ret
    
FN_PEEK:
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
    jp      nz,.get_page_byte     ; If not specified
    ld      a,(de)                ;   Get Byte
.float_it:
    jp      SNGFLT                ; and float it

.get_page_byte:
    call    page_read_byte        ; Read byte into C
    ld      a,c
    jr      .float_it

; Check for and parse @page,
; Output: A, E = Page (0 if not specified)
parse_page_arg:
    ld      e,0                   ; Default to Page 0
    cp      '@'                   
    jr      nz,.notat             ; If page prefix
    rst     CHRGET                ;   Skip '@'
    call    GETBYT                ;   Parse byte into E
    SYNCHK  ','                   ;   Require comma
.notat
    
    ld      a,e                   ; A = page
    or      a                     ; Set Flags
    ret     
