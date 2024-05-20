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
;         COPY [@page,] source, length TO [@page,] destination [FAST]
;-----------------------------------------------------------------------------
ST_COPY:
    jp      z,COPY                ; No Parameters? Do Standard COPY
    cp      FILETK                
    jp      z,ST_COPY_FILE
    call    get_page_arg          ; Check for Page Arg
    push    af                    ; Stack = SrcPgFlg
    jr      nc,.no_fpg            ; If specified
    ld      a,(hl)
    cp      TOTK                  ; and followed by TO
    jr      z,.page2page          ;   Do Page to Page copy
    SYNCHK  ','
.no_fpg
    call    GETINT                ; DE = SrcAdr
    pop     af                    ; AF = SrcPgFlg
    push    de                    ; Stack = SrcAdr
    push    af                    ; Stack = SrcPgFlg, SrcAdr
    SYNCHK  ','                   ; Require Comma
    call    GETINT                ; DE = Len
    push    de                    ; Stack = Len, SrcPgFlg, SrcAdr
    rst     SYNCHR
    byte    TOTK
    call    get_page_addr         ; A = DstPgFlg, DE = DstAdr

    push    af                    ; Save AF
    ld      a,(hl)                ; Get character after DstAdr
    cp      XTOKEN                ; If Extended Token Prefix
    jr      nz,.not_xtoken
    inc     hl                    ;   Skip it
    ld      ixl,FASTK             ;   Assume FAST
    rst     SYNCHR                ;
    byte    FASTK                 ;   SNERR if not FAST
.not_xtoken
    pop     af                    ; Restore AF

    ex      af,af'                ; AF' = DstPgFlg
    pop     bc                    ; BC = Len; Stack = SrcPgFlg, SrcAdr
    pop     af                    ; AF = SrcPgFlg, Stack = SrcAdr
    ex      (sp),hl               ; HL = SrcAdr, Stack = TxtPtr
    jr      nc,.no_spg            ; If Source Page
    ex      af,af'                ;   AF = DstPgFlg, AF' = SrcPgFlg
    jr      nc,.spg_no_dpg        ;   If Dest Page
    push    af                    ;
    ld      a,ixl                 ;
    ld      ix,page_copy_bytes    ;
    cp      FASTK                 ;
    jr      nz,.do_copy
    ld      ix,page_fast_copy
    jr      .do_copy              ;   Else
.spg_no_dpg
    ex      af,af'                ;     AF = SrcPgFlg
    push    af                    ;     Stack = DstPgFlg, TxtPtr
    ld      a,ixl                 ;
    ld      ix,page_read_bytes    ;
    cp      FASTK                 ;
    jr      nz,.do_copy
    ld      ix,page_fast_read_bytes
    jr      .do_copy              ;   Else
.no_spg:
    ex      af,af'                ;   AF = DstPgFlg, AF' = SrcPgFlg
    jr      nc,.no_pages          ;   If No Dest Page
    push    af                    ;     Stack = DstPgFlg, TxtPtr
    ld      a,ixl                 ;
    ld      ix,page_write_bytes   ;
    cp      FASTK                 ;
    jr      nz,.do_copy
    ld      ix,page_fast_write_bytes
.do_copy
    pop     af                    ;     AF = DstPgFlg; Stack = TxtPtr
    call    jump_ix               ;     Do Page to Page
    jp      z,FCERR
    jp      c,OVERR
    pop     hl
    ret                           ;   Else
.no_pages:
    rst      COMPAR               ;     If SrcAdr >= DstAdr
    jr       c,.copy_down         ;
    ldir                          ;       Do the Copy
    pop      hl
    ret                           ;     Else
.copy_down
    push    de                    ;       Stack = DstAdr, TxtPtr
    ex      (sp),hl               ;       HL = DstAdr, Stack = SrcAdr, TxtPtr
    add      hl,bc
    dec      hl
    ld       d,h
    ld       e,l                  ;       DE = DstAdr + Len - 1
    pop      hl                   ;       HL = SrcAdr, Stack = TxtPtr
    add      hl,bc
    dec      hl                   ;       HL = SrcAdr + Len - 1
    lddr                          ;       Do the Copy
    pop      hl
    ret


.page2page:
    rst     CHRGET          ; Skip TO
    call    req_page_arg    ; Check for Page Arg
    ex      af,af'
    pop     af              ; AF' = Source page
    ex      af,af'
    call    page_full_copy  ; Copy the page
    jp      z,FCERR         ; Error if invalid page
    ret

;-----------------------------------------------------------------------------
; Enhanced POKE
; syntax: POKE address, byte
;         POKE address, string$
;         POKE @page, address, byte
;         POKE @page, address, string$
;-----------------------------------------------------------------------------
ST_POKE:
    cp      SCRNTK
    jr      z,.poke_screen
    cp      COLTK
    jr      z,.poke_color
.poke
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
    jr      .poke_done

.pokestring
    pop     de                    ; DE = DstAdr; Stack = Page, RtnAdr
    pop     af                    ; AF = Page; Stack = RtnAdr
    push    hl                    ; Stack = TxtPtr, RtnAdr
    push    de                    ; Stack = DstAdr, TxtPtr, RtnAdr
    ex      af,af'
    call    FRESTR                ; HL = StrDsc
    call    string_addr_len       ; BC = StrLen, DE = StrAdr, HL = StrDsc
    jr      z,.nullstring
    ex      de,hl                 ; HL = StrAdr
    pop     de                    ; DE = DstAdr; Stack = TxtPtr, RtnAdr
    ex      af,af'
    jr      c,.write_paged_bytes  ; If page specified, write to it
    ldir                          ; Else copy bytes
    byte    $26                   ; LD H over first pop
.nullstring
    pop     hl                    ; Stack = DstAdr, RtnAdr
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    jr      .poke_done

.write_paged_byte:
    call    check_paged_address
    call    page_write_byte     ; If page specified, write to it
    jp      z,IQERR             ; FC error if illegal page
    jr      .poke_done

.write_paged_bytes:
    call    check_paged_address
    call    page_write_bytes    ; If page specified, write to it
    jp      z,IQERR             ; FC error if illegal page
    pop     hl
.poke_done
    ld      a,(hl)
    cp      ';'
    ret     nz
    rst     CHRGET                  ; Skip ;
    jr      .poke                   ; Get next address

.poke_screen
    rst     CHRGET                ; Skip SCREEN or ;
    call    .screen_args          ; Stack = AdrOfs, TxtPtr, RtnAdr
    jr      z,.screenstring       ; If Poking byte
    call    CONINT                ;   A = Byte
    pop     de                    ;   DE = AdrOfs; Stack = TxtPtr, RtnAdr
    call    screen_write_byte     ;   Write byte
    jr      .screen_done          ; Else
.screenstring
    call    FRESTR                ;   Free Temporary
    pop     de                    ;   DE = AdrOfs; Stack = TxtPtr, RtnAdr
    call    screen_write_string   ;   Write wtring
.screen_done
    jp      c,FCERR               ; Error if Offset too large
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr 
    ld      a,(hl)
    cp      ';'
    ret     nz
    jr      .poke_screen            ; Get next address
    ret                           

.poke_color
    rst     CHRGET                ; Skip COL
    rst     SYNCHR
    byte    ORTK                  ; Require OR
.next_color
    call    .screen_args          ; Stack = AdrOfs, TxtPtr, RtnAdr
    jr      z,.colorstring        ; If Poking byte
    call    CONINT                ;   A = Byte
    pop     de                    ;   DE = AdrOfs; Stack = TxtPtr, RtnAdr
    call    color_write_byte      ;   Write byte
    jr      .color_done           ; Else
.colorstring
    call    FRESTR                ;   Free Temporary
    pop     de                    ;   DE = AdrOfs; Stack = TxtPtr, RtnAdr
    call    color_write_string    ;   Write wtring
.color_done
    jp      c,FCERR               ; Error if Offset too large
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr 
    ld      a,(hl)
    cp      ';'
    ret     nz
    rst     CHRGET                ; Skip ;
    jr      .next_color 

.screen_args
    call    GETINT                ; Get Address Offset
    push    de                    ; Stack = AdrOfs, RtnAdr
    SYNCHK  ','                   ; Require comma
    call    FRMEVL                ; Evaluate argumenr
    pop     de                    ; DE = AdrOfs; Stack = RtnAdr
    ex      (sp),hl               ; HL = RtnAdr; Stack = TxtPtr
    push    de                    ; Stack = AdrOfs, TxtPtr
    push    hl                    ; Stack = RtnAdr, AdrOfs, TxtPtr
    jp      GETYPE                ; Get Operand Type 

;-----------------------------------------------------------------------------
; DOKE
; syntax: DOKE address, word
;         DOKE @page, address, word
;-----------------------------------------------------------------------------
ST_DOKE:
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
    ld      a,b                   ; Get MSB
    ld      (de),a                ; Write to address
    jr      .doke_done

.write_paged_word
    call    check_paged_address   ; Verify pages addres is between 0 and 16383
    call    page_write_word       ; If page specified, write to it
    jp      z,IQERR               ; FC error if illegal page
    jp      c,OVERR               ; Return overflow error if end of RAM
.doke_done
    ld      a,(hl)
    cp      ';'
    ret     nz
    rst     CHRGET                ; Skip semicolon
    jr      ST_DOKE               ; Get next address

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
;         PEEK(@page, address)
;         PEEK$(address, length)
;         PEEK$(@page, address, length)
;-----------------------------------------------------------------------------
FN_PEEK:
    rst     CHRGET                ; Skip token
    cp      SCRNTK
    jr      z,.peek_screen
    cp      COLTK
    jp      z,.peek_color
    cp      '$'                   ; If followed by dollar sign
    jr      z,.peekstring         ;   Do PEEK$()
    call    paren_page_arg        ; Parse page
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

.peekstring:
    rst     CHRGET                ; Skip token
    call    paren_page_arg        ; Parse page
    push    af                    ; Stack = PgArg, RtnAdr
    call    GETINT                ; Parse Address
    push    de                    ; Stack = PkAdr, PgArg, RtnAdr
    SYNCHK  ','                   ; Require comma
    call    GETBYT                ; Parse Length
    SYNCHK  ')'                   ; Require close paren
    pop     bc                    ; BC = PkAdr; Stack = PgArg, RtnAdr
    pop     af                    ; AF = PgArg; Stack = RtnAdr
    push    hl                    ; Stack = TxtPtr, RtnAdr
    push    af                    ; Stack = PgArg, TxtPtr, RtnAdr
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


.peek_color
    rst     CHRGET                ; Skip COL
    rst     SYNCHR                ; Require OR
    byte    ORTK                    
    xor     a                     ; Z = Color RAM
    push    af                    ; Stack = PkRAM, TxtPtr
    jr      .do_screen_color   
.peek_screen
    rst     CHRGET                ; Skip SCREEN
    or      $FF                   ; NZ = Screen RAM
    push    af
.do_screen_color
    ld      a,(hl)
    cp      '$'                   ; Check for PEEK$
    jr      z,.screenstring       ; If not PEEKSTRING$
    call    GETINT                ; DE = Address
    pop     af                    ; AF = PkRAM, Stack = RtnAdr
    push    hl                    ; Stack = TxtPtr, RtnAdr
    jr      z,.color
    call    screen_read_byte
    jr      .done_screen_color    
.color
    call    oolor_read_byte
.done_screen_color
    jp      c,FCERR
    ld      bc,LABBCK             ; 
    push    bc                    ; Stack = LABBCK, TxtPtr, RtnAdr
    jp      SNGFLT

.screenstring
    rst     CHRGET                ; Skip $
    SYNCHK  '('                   ; Require '('
    call    get_addr_len          ; DE = ScrOfs, BC = PkLen
    SYNCHK  ')'                   ; Require ')'
    pop     af                    ; AF = PkRAM; Stack = RtnAdr
    ex      af,af'                ; AF' = PkRAM
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ld      a,c                   ; A = PkLen
    or      a                     ; If 0
    jp      z,null_string         ;   Return Empty String
    push    bc                    ; Stack = PkLen, TxtPtr, RtnAdr
    push    de                    ; Stack = ScrOfs, PkLen, TxtPtr, RtnAdr
    call    STRINI                ; Make String with Length A, HL=StrDsc, DE=StrTxt
    pop     de                    ; DE = ScrOfs; Stack = PkLen, TxtPtr, RtnAdr
    pop     bc                    ; BC = PkLen; Stack = TxtPtr, RtnAdr
    ex      af,af'                ; AF = PkRAM
    jr      z,.colorstring
    call    screen_read_string    ; Read string
    jr      .done_screen_string
.colorstring
    call    color_read_string
.done_screen_string
    jp      c,FCERR
    jp      PUTNEW                ; and return it

;-----------------------------------------------------------------------------
; DEEK
; syntax: DEEK(address)
;         DEEK(@page, address)
;-----------------------------------------------------------------------------
FN_DEEK:
    rst     CHRGET                ; Skip DEEK
    call    paren_page_arg        ; Parse page
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

.read_page_word
    call    check_paged_address
    call    page_read_word
    jp      z,IQERR               ; FC error if illegal page
    jp      c,OVERR               ; Return overflow error if end of RAM
    jp      FLOAT_BC


paren_page_arg:
    SYNCHK  '('
; Check for and parse @page,
; Output: A, E = Page number`
;  Carry: Set if page specified
parse_page_arg:
    cp      '@'
    jr      nz,.notat             ; If page prefix
    rst     CHRGET                ;   Skip '@'
    call    GETBYT                ;   Parse byte into E
    call    get_comma
;   SYNCHK  ','                   ;   Require comma
    ld      a,e
    scf
    ret
.notat
    or      a                     ; Clear Carry Flag
    ret

;-----------------------------------------------------------------------------
; Enhanced STOP statement stub
;-----------------------------------------------------------------------------
ST_RESTORE:
    cp      SCRNTK                
    jp      z,ST_RESTORE_SCREEN
    call    CHRGT3                ; Set digit and terminator flags
    jp      RESTOR

;-----------------------------------------------------------------------------
; Enhanced STOP statement stub
;-----------------------------------------------------------------------------
ST_STOP:
    jp      z,STOP
    rst     SYNCHR
    byte    XTOKEN
    cp      PT3TK
    jp      z,ST_STOP_PT3
    jp      SNERR
