;=====================================================================================
; Enhanced BASIC Statements and Functions
;======================================================================================

;-----------------------------------------------------------------------------
; ASC$(hexstring$)
; Convert Hexadecimal String to Binary String
;-----------------------------------------------------------------------------

FN_ASC:
    inc     hl                    ; Check character directly after ASC Token
    ld      a,(hl)                ; (don't skip spaces)
    cp      '$'                   ; If it's not a dollar sign
    jr      nz,ABORT_FN           ;   Return to do normal ASC
    rst     CHRGET                ; Eat $ and Skip Spaces
    call    PARCHK                ; Parse Argument in Parentheses
    push    hl                    ; Save Text Pointer
    call    CHKSTR                ; TM Error if Not a String
    jp      hex_to_asc            ; Convert to ASCII
    
ABORT_FN:   
    dec     hl                    ; Back up to Function Token
    ld      a,(hl)                ; Re-Read Token
    sub     ONEFUN                ; Convert to Offset
    jp      HOOK27+1              ; Continue with Standard Function Code
  
;-----------------------------------------------------------------------------
; Enhanced COPY
; syntax: COPY @page TO @page
;         COPY [@page,] source, length TO [@page,] destination
;         COPY! @page,] source, length TO @page, destination
;-----------------------------------------------------------------------------
ST_COPY:
    jp      z,COPY          ; No Parameters? Do Standard COPY
    cp      '!'             ; If COPY!
    jr      z,.fast_page    ;   Do fast page to page
    call    get_page_arg    ; Check for Page Arg
    push    af              ; Stack = SrcPgFlg
    jr      nc,.no_fpg      ; If specified
    ld      a,(hl)
    cp      TOTK            ; and followed by TO
    jr      z,.page2page    ;   Do Page to Page copy
    SYNCHK  ','
.no_fpg
    call    GETINT          ; DE = SrcAdr
    pop     af              ; AF = SrcPgFlg
    push    de              ; Stack = SrcAdr
    push    af              ; Stack = SrcPgFlg, SrcAdr
    SYNCHK  ','             ; Require Comma
    call    GETINT          ; DE = Len
    push    de              ; Stack = Len, SrcPgFlg, SrcAdr
    rst     SYNCHR
    byte    TOTK
    call    get_page_addr   ; A = DstPgFlg, DE = DstAdr
    ex      af,af'          ; AF' = DstPgFlg
    pop     bc              ; BC = Len; Stack = SrcPgFlg, SrcAdr
    pop     af              ; AF = SrcPgFlg, Stack = SrcAdr
    ex      (sp),hl         ; HL = SrcAdr, Stack = TxtPtr
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
    rst      COMPAR         ;     If SrcAdr >= DstAdr
    jr       c,.copy_down   ;
    ldir                    ;       Do the Copy
    pop      hl
    ret                     ;     Else
.copy_down
    push    de              ;       Stack = DstAdr, TxtPtr
    ex      (sp),hl         ;       HL = DstAdr, Stack = SrcAdr, TxtPtr
    add      hl,bc
    dec      hl
    ld       d,h
    ld       e,l            ;       DE = DstAdr + Len - 1
    pop      hl             ;       HL = SrcAdr, Stack = TxtPtr
    add      hl,bc
    dec      hl             ;       HL = SrcAdr + Len - 1
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
    call    page_full_copy  ; Copy the page
    jp      z,FCERR         ; Error if invalid page
    ret

;COPY! @40,0,4096 TO @20,0
.fast_page:
    rst     CHRGET          ; Skip !
    SYNCHK  '@'             ; Require page prefix
    call    GETBYT          ; A = SrcPg
    push    af              ; Stack = SrcPg
    SYNCHK  ','             ; Require Comma
    call    GETINT          ; DE = SrcAdr
    pop     af              ; AF = SrcPgFlg
    push    de              ; Stack = SrcAdr
    push    af              ; Stack = SrcPg, SrcAdr
    SYNCHK  ','             ; Require Comma
    call    GETINT          ; DE = Len
    push    de              ; Stack = Len, SrcPg, SrcAdr
    rst     SYNCHR
    byte    TOTK
    SYNCHK  '@'             ; Require page prefix
    call    GETBYT          ; A = DstPg
    push    af              ; Stack = DstPg, Len, SrcPg, SrcAdr
    SYNCHK  ','             ; Require Comma
    call    GETINT          ; DE = DstAdr
    pop     af              ; AF = DstPg; Len, SrcPg, SrcAdr
    ex      af,af'          ; AF' = DstPgFlg    
    pop     bc              ; BC = Len; Stack = SrcPg, SrcAdr
    pop     af              ; AF = SrcPg; Stack = SrcAdr
    ex      (sp),hl         ; HL = SrcAdr, Stack = TxtPtr
    call    page_fast_copy  ; Do fast page to page copy
    jr      .pg_done        ; Finish up

;-----------------------------------------------------------------------------
; Enhanced POKE
; syntax: POKE address, byte
;         POKE address, string$
;         POKE! address, word
;         POKE @page, address, byte
;         POKE @page, address, string$
;         POKE! @page, address, word
;-----------------------------------------------------------------------------
ST_POKE:
    cp      '!'                   ; If POKE!
    jr      z,.pokeword           ;   Poke a word
    call    parse_page_arg        ; Parse page
    push    af                    ; Stack = Page, RtnAdr
    call    GETINT                ; Parse Address
    push    de                    ; Stack = Addr, Page, RtnAdr
    SYNCHK  ','                   ; Require comma
    call    FRMEVL                ; Evaluate argumenr
    call    GETYPE                ; If String
    jr      z,.pokestring         ;   Poke It
    call    CONINT                ; Convert to Byte
    ld      c,a                   ; C = Byte
    pop     de                    ; DE = Addr; Stack = Page, RtnAdr
    pop     af                    ; AF = Page, Stack = RtnAdr
    jr      c,.write_paged_byte   ; If page specified, write to it
    ld      a,c                   ; Write byte
    ld      (de),a                ; to address
    ret

.pokestring
    pop     de                    ; DE = DstAdr; Stack = Page, RtnAdr
    pop     af                    ; AF = Page; Stack = RtnAdr
    push    hl                    ; Stack = TxtPtr, RtnAdr
    push    de                    ; Stack = DstAdr, TxtPtr, RtnAdr
    ex      af,af'
    call    FRESTR                ; HL = StrDsc
    call    string_addr_len       ; BC = StrLen, DE = StrAdr, HL = StrDsc
    ex      de,hl                 ; HL = StrAdr
    pop     de                    ; DE = DstAdr; Stack = TxtPtr, RtnAdr
    ex      af,af'
    jr      c,.write_paged_bytes  ; If page specified, write to it
    ldir                          ; Else copy bytes
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
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

.write_paged_bytes:
    call    check_paged_address
    call    page_write_bytes    ; If page specified, write to it
    jp      z,IQERR             ; FC error if illegal page
    pop     hl
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

;-----------------------------------------------------------------------------
; Enhanced PEEK
; syntax: PEEK(address)
;         PEEK!(address)
;         PEEK(@page, address)
;         PEEK!(@page, address)
;-----------------------------------------------------------------------------
;ToDo: Add PEEK$
FN_PEEK:
    rst     CHRGET                ; Skip token
    cp      '!'                   ; If POKE!
    jr      z,.peekword           ;   Poke a word
    cp      '$'                   ; If followed by dollar sign
    jr      z,.peekstring         ;   Do PEEK$()
    SYNCHK  '('                   ; Require open paren
    call    parse_page_arg        ; Parse page
    push    af                    ; Save it
    call    GETINT                ; Parse Address
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
    push    af                    ; Save it
    call    GETINT                ; Parse Address
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

.peekstring:
    rst     CHRGET                ; Skip token
    SYNCHK  '('                   ; Require open paren
    call    parse_page_arg        ; Parse page
    push    af                    ; Stack = PgArg, RtnAdr
    call    GETINT                ; Parse Address
    push    de                    ; Stack = PkAdr, PgArg, RtnAdr
    SYNCHK  ','                   ; Require comma
    call    GETBYT                ; Parse Length
    SYNCHK  ')'                   ; Require close paren
    pop     bc                    ; BC = PkAdr; Stack = PgArg, RtnAdr
    pop     af                    ; AF = PgArg; Stack = RtnAdr
    push    hl                    ; Stack = TxtPtr, RtnAdr
    push    af                    ; Stack = PgArg, TxtPtr, RtnAr
    ld      a,e                   ; A = Length
    or      a                     ; If Length is 0
    jp      z,null_string         ;   Return Empty String
    push    bc                    ; Stack = PkAdr, PgArg, TxtPtr, RtnAdr
    push    de                    ; Stack = Length, PkAdr, PgArg, TxtPtr, RtnAdr
    call    STRINI                ; Make String with Length [A], HL=StrDsc, DE=StrTxt
    pop     bc                    ; BC = Length; Stack = Address, PgArg, TxtPtr, RtnAdr
    pop     hl                    ; HL = PkAdr; Stack = PgArg, TxtPtr, RtnAdr
    pop     af                    ; AF = PgArg; Stack = TxtPtr, RtnAdr
    jr      nc,.do_ldir           ; If paged read
    call    page_read_bytes       ;   Copy from Paged Memory to String
    jr      .do_putnew            ; Else
.do_ldir
    ldir                          ;   Copy from Main Memory to String
.do_putnew
    jp      PUTNEW                ; Return the Temporary String

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
    or      a                     ; Clear Carry Flag
    ret
