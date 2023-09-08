;=====================================================================================
; Enhanced BASIC Statements and Functions
;======================================================================================

;-----------------------------------------------------------------------------
; Enhanced COPY
; syntax: COPY @page TO @page
; ToDo: COPY [@page,] addr, len TO [@page],addr
;-----------------------------------------------------------------------------

ST_COPY:   
    jp      z,COPY          ; No Parameters? Do Standard COPY
    call    get_page_arg    ; Check for Page Arg
    push    af              ; Stack = SrcPgFlg
    jr      nc,.no_fpg      ; If specified
    ld      a,(hl)
    cp      TOTK            ; and followed by TO
    jr      z,.page2page    ;   Do Page to Page copy
    SYNCHK  ','             
.no_fpg
    call    GETINT          ; DE = SrcAddr
    pop     af              ; AF = SrcPgFlg
    push    de              ; Stack = SrcAddr
    push    af              ; Stack = SrcPgFlg, SrcAddr
    SYNCHK  ','             ; Require Comma
    call    GETINT          ; DE = Len
    push    de              ; Stack = Len, SrcPgFlg, SrcAddr
    rst     SYNCHR
    byte    TOTK
    call    get_page_addr   ; A = DstPgFlg, DE = DstAddr
    ex      af,af'          ; AF' = DstPgFlg
    pop     bc              ; BC = Len; Stack = SrcPgFlg, SrcAddr
    pop     af              ; AF = SrcPgFlg, Stack = SrcAddr
    ex      (sp),hl         ; HL = SrcAddr, Stack = TxtPtr
    jr      nc,.no_spg      ; If Source Page
    ex      af,af'          ;   AF = DstPgFlg, AF' = SrcPgFlg
    jr      nc,.spg_no_dpg  ;   If Dest Page
    call    page_copy_bytes ;     Do Page to Page
    jr      .pg_done        ;   Else
.spg_no_dpg
    ex      af,af'          ;     AF = SrcPgFlg
    call    page_read_bytes ;     Do Page to Main
    jr      .pg_done        ; Else
.no_spg:
    ex      af,af'          ;   AF = DstPgFlg, AF' = SrcPgFlg
    jr      nc,.no_pages    ;   If No Dest Page
    call    page_write_bytes ;     Do Main to Page
.pg_done    
    jp      z,FCERR
    jp      c,OVERR
    pop     hl               
    ret                     ;   Else
.no_pages:
    rst      COMPAR         ;     If SrcAddr >= DstAddr
    jr       c,.copy_down   ;    
    ldir                    ;       Do the Copy
    pop      hl             
    ret                     ;     Else
.copy_down
    push    de              ;       Stack = DstAddr, TxtPtr
    ex      (sp),hl         ;       HL = DstAddr, Stack = SrcAddr, TxtPtr
    add      hl,bc                  
    dec      hl                     
    ld       d,h                    
    ld       e,l            ;       DE = DstAddr + Len - 1
    pop      hl             ;       HL = SrcAddr, Stack = TxtPtr
    add      hl,bc                  
    dec      hl             ;       HL = SrcAddr + Len - 1
    lddr                    ;       Do the Copy
    pop      hl
    ret

.page2page:
    rst     CHRGET          ; Skip TO
    call    get_page_arg    ; Check for Page Arg
    jp      nc,SNERR        ; Error if not specified
    ex      af,af'
    pop     af              ; AF' = Source page
    ex      af,af'
    call    page_copy       ; Copy the page
    jp      z,FCERR         ; Error if invalid page
    ret

;-----------------------------------------------------------------------------
; Enhanced POKE
; syntax: POKE address, byte
;         POKE! address, word
;         POKE @page, address, byte
;         POKE! @page, address, word
;-----------------------------------------------------------------------------

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
    call    check_paged_address
    call    page_write_byte     ; If page specified, write to it
    jp      z,IQERR             ; FC error if illegal page
    ret

.write_paged_word  
    call    check_paged_address   ; Verify pages addres is between 0 and 16383
    call    page_write_word       ; If page specified, write to it
    jp      z,IQERR               ; FC error if illegal page
    jp      c,OVERR               ; Return overflow error if end of RAM 
    ret
    
;-----------------------------------------------------------------------------
; Generate Illegal Quantity error if address in DE is not between 0 and 16383
;-----------------------------------------------------------------------------
check_paged_address:
    push    a                     ; Save Page
    ld      a,$C0
    and     d                     ; If address is not 0 - 16383
    jp      nz,IQERR              ;   Illegal Quantity error
    pop     a
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
    call    check_paged_address
    call    page_read_byte        ; Read byte into C
    jp      z,IQERR               ; FC error if illegal page
    ld      a,c
    jr      .float_it

.peekword:
    rst     CHRGET                ; Skip token
    SYNCHK  '('                   ; Require open paren
    call    parse_page_arg        ; Parse page
    push    af                    ; Save itj
    call    GETINT                ; Parse Integer
    SYNCHK  ')'                   ; Require close paren
    pop     af                    ; Get page
    push    hl                    ; Save text pointer
    ld      bc,LABBCK             ; Return address for FLOAT_BC
    push    bc
    jr      c,.read_page_word     ; If not specified
    ld      a,(de)                ;   Get LSB
    ld      c,a
    inc     de
    ld      a,(de)
    ld      b,a
    jp      FLOAT_BC

.read_page_word
    call    check_paged_address
    call    page_read_word
    jp      z,IQERR               ; FC error if illegal page
    jp      c,OVERR               ; Return overflow error if end of RAM 
    jp      FLOAT_BC


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
