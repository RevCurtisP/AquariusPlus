;=====================================================================================
; Enhanced BASIC Statements and Functions
;======================================================================================

;-----------------------------------------------------------------------------
; ASC(string$)
; Enhanced:
; ASC(string$, offset)
; ASC$(hexstring$) - Convert Hexadecimal String to Binary String
;-----------------------------------------------------------------------------
FN_ASC:
    inc     hl                    ; Check character directly after ASC Token
    ld      a,(hl)                ; (don't skip spaces)
    cp      '$'                   
    jr      nz,asc_chr            ; If ASC$
    rst     CHRGET                ;   Eat $ and Skip Spaces
    call    PARSTR                ;   Parse String Argument in Parentheses
    push    hl                    ;   Save Text Pointer
    jp      hex_to_asc            ;   Convert to ASCII and return

asc_chr:
    ld      iy,SNGFLT
sng_chr:
    push    iy                    ; Stack = FltJmp, RtnAdr
    SYNCHKC '('
    call    FRMSTR
    pop     bc                    ; BC = FltJmp; Stack = RtnAdr
    ld      de,(FACLO)
    push    de                    ; Stack = StrDsc, RtnAdr
    push    bc                    ; Srack = FltJmp, StrDsc, RtnAdr
    ld      de,1                  ; ChrPos = 1
    ld      a,(hl)
    cp      ','
    jr      nz,_asc_char
    rst     CHRGET                ; Skip Comma
    call    GETBYT                ; DE = ChrPos
    or      a
    jp      z,FCERR
_asc_char:
    SYNCHKC ')'
    pop     iy                    ; IY = FltJmp; Stack = StrDsc, RtnAdr
    ex      (sp),hl               ; HL = StrDsc; Stack = TxtPtr, RtnAdr                
    ld      bc,LABBCK
    push    bc                    ; Stack = LABCK, TxtPtr, RtnAdr
    push    de                    ; Stack = ChrPos, LABBCK, TxtPtr, RtnAdr
    call    free_hl_addr_len      ; DE = StrAdr, BC = StrLen
    pop     hl                    ; HL = ChrPos; Stack = LABBCK, TxtPtr. RtnAdr
    ld      a,c
    cp      l                     ; If ChrPos > StrLen
    jp      c,FCERR               ;   Illegal quantity
    dec     hl                    ; Adjust ChrPos for add
    add     hl,de                 ; HL = ChrAdr
    xor     a
    ld      (VALTYP),a
    ld      a,(hl)                ; A = ChrVal
    jp      (iy)                  ; FloatIt

;-----------------------------------------------------------------------------
; Enhanced COPY
; COPY @page TO @page
; COPY [@page,] source, length TO [@page,] destination [FAST]
; COPY FILE filespec TO filespec
;-----------------------------------------------------------------------------
ST_COPY:
    jp      z,COPY                ; No Parameters? Do Standard COPY
    cp      FILETK
    jp      z,ST_COPY_FILE
    cp      SCRNTK
    jp      z,ST_COPY_SCREEN
    cp      MULTK
    jr      nz,.not_sary          ; If *
    call    skip_star_array       ;   DE = SrcAdr, BC = SrcLen
    push    de                    ;   Stack = SrcAdr
    or      a                     ;   PgFlag = NoPage
    push    af                    ;   Stack = SrcPgFlg, SrcAdr
    push    bc                    ;   Stack = Len, SrcPgFlg, SrcAdr
    jr      .do_to_arg            ;   Evaluate TO clause and copy
.not_sary
    cp      '!'
    jr      nz,.not_ext           ; If !
    call    get_ext_addr          ;   AF = PgFlg, DE = Addr
    jr      .pg_addr
.not_ext
    call    get_page_arg          ; Check for Page Arg
    push    af                    ; Stack = SrcPgFlg
    jr      nc,.no_fpg            ; If specified
    ld      a,(hl)
    cp      TOTK                  ; and followed by TO
    jp      z,.page2page          ;   Do Page to Page copy
    SYNCHKC ','
.no_fpg
    call    GETINT                ; DE = SrcAdr
    pop     af                    ; AF = SrcPgFlg
.pg_addr
    push    de                    ; Stack = SrcAdr
    push    af                    ; Stack = SrcPgFlg, SrcAdr
    ld      a,(hl)
    cp      TOTK
    jp      z,.copy_page_addr_to
    SYNCHKC ','                   ; Require Comma
    call    GETINT                ; DE = Len
    push    de                    ; Stack = Len, SrcPgFlg, SrcAdr
.do_to_arg
    SYNCHKT TOTK
    cp      MULTK
    jr      nz,.not_dary          ; If *
    call    skip_star_array       ;   DE = DstAdr, BC = AryLen
    inc     bc                    ;   Bump Arylen for compare
    ex      (sp),hl               ;   HL = Len, Stack = TxtPtr, SrcPgFlg, SrcAdr
    call    compare_hl_bc         ;   If Len > AryLen (HL >= BC)
    jp      nc,BRERR              ;     Bad range error
    ex      (sp),hl               ;   HL = TxtPtr, Stack = Len, SrcPgFlg, SrcAdr
    or      a                     ;   DstPgFlg = NoPage
    jr      .dary_done
.not_dary
    call    get_page_addr         ; A = DstPgFlg, DE = DstAdr
.dary_done
    push    af                    ; Stack = DstPgFlg, Len, SrcPgFlg, SrcAdr
    ld      a,(hl)                ; Get character after DstAdr
    cp      XTOKEN                ; If Extended Token Prefix
    jr      nz,.not_xtoken
    inc     hl                    ;   Skip it
    ld      ixl,FASTK             ;   Assume FAST
    SYNCHKT FASTK                 ;   SNERR if not FAST
.not_xtoken
    pop     af                    ; AR = DstPgFlg; Stack = Len, SrcPgFlg, SrcAdr
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

.copy_page_addr_to
    rst     CHRGET                ; Skip TO
    cp      MULTK                 ; If *
    jr      z,.copy_to_array      ;   Parse array and copy to it
    ld      ix,copy_to_screen
    SYNCHKT SCRNTK                ;   Only TO SCREEN allowed right now
.do_page_addr_to
    pop     af                    ; AF = SrcPgFlg; Stack = SrcAdr, RtnAdr
    pop     de                    ; DE = SrcAdr; Stack = SrcAdr, RtnAdr
    jp      nc,MOERR              ; If NoPage, Missing operand error
    jp      (ix)

.copy_to_array
    call    skip_star_array       ; DE = DstAdr, BC = Len
    push    bc                    ; Stack = Len, Stack = SrcPgFlg, SrcAdr
    or      a                     ; DstPgFlg = NoPage
    jp      .dary_done            ; Check for FAST and ro copy

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
; Enhanced FRE
;FRE ( -1 ) Returns the number of bytes in memory not being used by BASlC as unsigned int.
;FRE ( 0 ) Returns the number of bytes in memory not being used by BASlC as signed int.
;FRE ( 1 ) Returns the total size of string space (as set by the first argument of CLEAR).
;FRE ( 2 ) Returns the top of BASIC memory (as set by the second argument of CLEAR).
;FRE ( "" ) Garbage collect and return free string space
;-----------------------------------------------------------------------------
FN_FRE:
    rst     CHRGET
    call    PARCHK
    call    push_hl_labbck
    call    GETYPE          ; If FRE(string)
    jp      z,FRE_STR       ;   Garbage Collect and Return Free String Space
    rst     FSIGN           ; If argument < 0, A = -1
    call    p,CONINT        ; Else A = argument
    ld      hl,(STREND)     ;
    ex      de,hl           ; DE = End of Variables/Arrays
    ld      hl,0            ;
    add     hl,sp           ; HL = Stack Pointer
    inc     a               ; If FRE(01)
    jp      z,FLOAT_DIFF    ;   Return Free Space as unsigned int
    dec     a               ; If FRE(0)
    jp      z,GIVFLT        ;   Return Free Space as signed int
    ld      hl,(MEMSIZ)     ;
    ld      de,(STRSPC)     ;
    dec     a               ; If FRE(1)
    jp      z,FLOAT_DIFF    ;   Return Total String Space
    ex      de,hl           ;
    dec     a               ; If FRE(2)
    jr      z,_float_de     ;   Return Top of BASIC Memory
    jp      FCERR           ; Else FC Error
FRE_STR:
    call    FREFAC          ; Free up string argumenr
    call    GARBA2          ; Do garbage collection
    ld      de,(STRSPC)     ; DE = Bottom of String Space
    ld      hl,(FRETOP)     ; HL = Start of allocated strings
FLOAT_DIFF:                 ; Return HL minus DE has a positive floating point number
    ld      a,l             ; E = L - E
    sub     e               ;
    ld      e,a             ;
    ld      a,h             ;
    sbc     a,d             ; D = H - E - carry
    ld      d,a
_float_de:
    jp      FLOAT_DE        ; Float It

;-----------------------------------------------------------------------------
; Enhanced INPUT
; syntax: INPUT (col,row),minlen,maxlen,INT var
;-----------------------------------------------------------------------------
; 10 INPUT (20,15),1,3,INT I
ST_INPUT:
    push    af
    call    set_cursor_on
    pop     af
    cp      '('                   ; If not INPUT (...
    jp      nz,INPUT              ;   Do regular INPUT
    call    SCAND                 ; C = Col, E = Row
    ld      d,c                   ; D = Col
    push    de                    ; Stack = ColRow, RtnAdr
;; ToDo: make routine get_comma_bytes in shared.asm
;; Call from here and _set_tile_ext
    call    get_comma_byte        ; A = MinLen
    push    af                    ; Stack = MinLen, ColRow, RtnAdr
    call    get_comma_byte        ; E = MaxLen
    pop     af                    ; A = MinLen; Stack = ColRow, RtnAdr
    ld      d,a                   ; D = MinLen
    push    de                    ; Stack = MinMax, ColRow, RtnAdr
    call    get_comma             ; Missing operand error if no comma
    SYNCHKT INTTK                 ; Require INT (for now)
    call    PTRGET                ; DE = VarPtr
    pop     bc                    ; BC = MinMax; Stack = ColRow, RtnAdr
    ex      (sp),hl               ; HL = ColRow; Stack = TxtPtr, RtnAdr
    ex      de,hl                 ; DE = ColRow; HL = VarPtr
    push    hl                    ; Stack = VarPtr, TxtPtr, RtnAdr
    call    GETYPE                ; A = VarTyp
    jp      z,TMERR               ; If string, Type mismatch error (for now)
    call    con_input_int
    jp      c,FCERR               ; If bad parameters, Illegal quantity error
    jp      m,pop2hl_ret          ; If aborted, don't change variable
    call    FIN                   ; FACC = Entry
    pop     hl                    ; HL = VarPtr
    call    MOVMF                 ; Copy FACC to Variable
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; Enhanced INT function
; INT(string$)
; INT(string$, offset)
;-----------------------------------------------------------------------------
FN_INT:
    call    FRMPRS                ; Skip INT, Eequire '(', and evaluate argument 
    ld      iy,float_signed_int   ; If string, set conversion routine
    jp      z,word_str            ;   and jump into WORD()
    SYNCHKC ')'                   ; Else require ')'
    call    push_hl_labbck        ;   Stack = LABBCK, TxtPtr, RtnAdr
    jp      INT                   ; Perform standard INT()

;-----------------------------------------------------------------------------
; Enhanced LEN function
; LEN(*array)
;-----------------------------------------------------------------------------
FN_LEN:
    ld      d,h
    ld      e,l                   ; Save pointer to function token
    rst     CHRGET                ; Skip LEN
    SYNCHKC '('                   ; Require (
    cp      MULTK                 ; If no *
    jp      nz,EX_ABORT_FN        ;   Do normal LEN
    call    skip_star_array       ; BC = DatLen
    call    CHKNUM
    SYNCHKC ')'
    call    push_hl_labbck        ;   Stack = LABBCK, TxtPtr, RtnAdr
    jp      FLOAT_BC

;-----------------------------------------------------------------------------
; Enhanced POKE
; syntax: POKE address, byte
;         POKE address, string$
;         POKE @page, address, byte
;         POKE @page, address, string$
;         POKE extaddr, byte
;         POKE extaddr, string$
;-----------------------------------------------------------------------------
;; ToDo: POKE SCREEN (col,row),byte
;;       POKE SCREEN (col,row),string$
ST_POKE:
    cp      SCRNTK
    jr      z,.pokescreen
    cp      COLTK
    jr      z,.pokecolor
.poke
    call    get_page_addr         ; AF = PgFlg, DE = Addr
    push    af                    ; Stack = Page, RtnAdr
    push    de                    ; Stack = Addr, Page, RtnAdr
    SYNCHKC ','                   ; Require comma
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
    call    free_addr_len         ; BC = StrLen, DE = StrAdr, HL = StrDsc
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
    jp      z,FCERR             ; FC error if illegal page
    jr      .poke_done

.write_paged_bytes:
    call    check_paged_address
    call    page_write_bytes    ; If page specified, write to it
    jp      z,FCERR             ; FC error if illegal page
    pop     hl
.poke_done
    ld      a,(hl)
    cp      ';'
    ret     nz
    rst     CHRGET                  ; Skip ;
    jr      .poke                   ; Get next address

.pokescreen
    rst     CHRGET                ; Skip SCREEN or ;
    call    .screen_args          ; Stack = AdrOfs, TxtPtr, RtnAdr
    jr      z,.screenstring       ; If Poking byte
    call    CONINT                ;   A = Byte
    pop     de                    ;   DE = AdrOfs; Stack = TxtPtr, RtnAdr
    ld      iy,screen_write_byte  ;   Write byte
    jr      .gfx_call_next_screen ; Else
.screenstring
    call    free_addr_len         ;   DE = StrAdr, BC = StrLen
    ex      de,hl                 ;   HL = StrAdr
    pop     de                    ;   DE = AdrOfs; Stack = TxtPtr, RtnAdr
    ld      iy,screen_write_bytes
.gfx_call_next_screen
    call    gfx_call              ;   Write string
    jp      c,FCERR               ; Error if Offset too large
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ld      a,(hl)
    cp      ';'
    ret     nz
    jr      .pokescreen           ; Get next address
    ret

.pokecolor
    rst     CHRGET                ; Skip COL
    SYNCHKT ORTK                  ; Require OR
.next_color
    call    .screen_args          ; Stack = AdrOfs, TxtPtr, RtnAdr
    jr      z,.colorstring        ; If Poking byte
    call    CONINT                ;   A = Byte
    pop     de                    ;   DE = AdrOfs; Stack = TxtPtr, RtnAdr
    ld      iy,color_write_byte   ;   Write byte
    jr      .gfx_call_next_color  ; Else
.colorstring
    call    free_addr_len         ;   DE = StrAdr, BC = StrLen
    ex      de,hl                 ;   HL = StrAdr
    pop     de                    ;   DE = AdrOfs; Stack = TxtPtr, RtnAdr
    ld      iy,color_write_bytes ;   Write wtring
.gfx_call_next_color
    call    gfx_call
    jp      c,FCERR               ; Error if Offset too large
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ld      a,(hl)
    cp      ';'
    ret     nz
    rst     CHRGET                ; Skip ;
    jr      .next_color

.screen_args
    call    GET_POS_INT           ; DE = Address Offset
    push    de                    ; Stack = AdrOfs, RtnAdr
    call    get_comma             ; Require comma
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
    call    get_page_addr         ; AF = PgFlg, DE = Addr
    push    af                    ; Save it
    push    de                    ; Save It
    SYNCHKC ','                   ; Require comma
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
    jp      z,FCERR               ; FC error if illegal page
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
    jp      nz,FCERR              ;   Illegal Quantity error
    pop     a
    ret

;-----------------------------------------------------------------------------
; Enhanced PEEK
; syntax: PEEK(address)
;         PEEK(@page, address)
;         PEEK(!extaddr)
;         PEEK$(address, length)
;         PEEK$(@page, address, length)
;         PEEK$(!extaddr, extaddr)
;-----------------------------------------------------------------------------
FN_PEEK:
    rst     CHRGET                ; Skip token
    cp      SCRNTK
    jr      z,.peekscreen
    cp      COLTK
    jp      z,.peekcolor
    cp      '$'                   ; If followed by dollar sign
    jr      z,.peekstring         ;   Do PEEK$()
    call    get_par_page_addr     ; AF = PgFlg, DE = Addr
    push    af                    ; Save it
    SYNCHKC ')'                   ; Require close paren
    pop     af                    ; Get page
    push    hl                    ; Save text pointer
    ld      bc,LABBCK             ; Return address for SNGFLT
    push    bc
    jp      c,.get_page_byte      ; If page not specified
    ld      a,(de)                ;   Get Byte
.float_it
    jp      SNGFLT                ; and float it

.get_page_byte
    call    check_paged_address
    call    page_read_byte        ; Read byte into C
    jp      z,FCERR               ; FC error if illegal page
    ld      a,c
    jr      .float_it

.peekstring
    rst     CHRGET                ; Skip token
    call    get_par_page_addr_len ; AF = PgFlg, BC = PkLen, DE = PkAdr
    push    af                    ; Stack = PgArg, RtnAdr
    SYNCHKC ')'                   ; Require close paren
    pop     af                    ; AF = PgArg; Stack = RtnAdr
    push    hl                    ; Stack = TxtPtr, RtnAdr
    call    .init_peek_string     ; DE = StrPtr
    jr      nc,.do_ldir           ; If paged read
    call    page_read_bytes       ;   Copy from Paged Memory to String
    jr      .do_putnew            ; Else
.do_ldir
    ldir                          ;   Copy from Main Memory to String
.do_putnew
    jp      PUTNEW                ; Return the Temporary String

.init_peek_string
    push    af                    ; Stack = Flags, RtnAdr, TxtPtr, RtnAdr
    ld      a,c                   ; A = PkLen
    or      a                     ; If Length is 0
    jp      z,pop_null_string     ;   Pop RtnAdr and Return Empty String
    push    de                    ; Stack = PkAdr, Flags, RtnAdr, TxtPtr, RtnAdr
    push    bc                    ; Stack = PkLen, PkAdr, Flags, RtnAdr, TxtPtr, RtnAdr
    call    STRINI                ; DE = StrPtr
    push    de                    ; Stack = StrPtr, PkLen, PkAdr, Flags, RtnAdr, TxtPtr, RtnAdr
    call    FREFAC                ; Free the temp
    pop     de                    ; DE = StrPtr; Stack = PkLen, PkAdr, Flags, RtnAdr, TxtPtr, RtnAdr
    pop     bc                    ; BC = PkLen; Stack = PkAdr, Flags, RtnAdr, TxtPtr, RtnAdr
    pop     hl                    ; HL = PkAdr; Flags, RtnAdr, TxtPtr, RtnAdr
    pop     af                    ; AF = Flags; Stack = RtnAdr, TxtPtr, RtnAdr
    ret

; POKE SCREEN 1,64:PRINT PEEKSCREEN(1)
; POKE COLOR 1,$07:PRINT PEEKCOLOR(1)
; POKE SCREEN 999,'#':PRINT PEEKSCREEN(999)
; POKE COLOR 999,$70:PRINT PEEKCOLOR(999)
; POKE SCREEN 2,"aaa":PRINT PEEKSCREEN$(2,3)
; POKE COLOR 2,"bbb":PRINT PEEKCOLOR$(2,3)
; POKE SCREEN 999,"":POKE COLOR 999,""
.peekcolor
    rst     CHRGET                ; Skip COL
    SYNCHKT ORTK                  ; Require OR
    xor     a                     ; SCFlag = Color RAM
    push    af                    ; Stack = SCFlag, TxtPtr
    jr      .do_screen_color
.peekscreen
    rst     CHRGET                ; Skip SCREEN
    or      $FF                   ; SCFlag = Screen RAM
    push    af                    ; Stack = SCFlag, TxtPtr
.do_screen_color
    ld      a,(hl)
    cp      '$'                   ; Check for PEEK$
    jr      z,.peek_sc_string     ; If not PEEKSTRING$
    call    PARCHK
    call    FRCINT                ;   DE = Address
    pop     af                    ;   AF = SCFlag, Stack = RtnAdr
    push    hl                    ;   Stack = TxtPtr, RtnAdr
    jr      z,.color
    ld      iy,screen_read_byte
    jr      .gfx_call
.color
    ld      iy,oolor_read_byte
.gfx_call
    call    gfx_call
    jp      c,FCERR
    ld      bc,LABBCK             ;
    push    bc                    ; Stack = LABBCK, TxtPtr, RtnAdr
    jp      SNGFLT

.peek_sc_string
    rst     CHRGET                ; Skip $
    SYNCHKC '('                   ; Require '('
    call    get_addr_len          ; DE = ScrOfs, BC = PkLen
    SYNCHKC ')'                   ; Require ')'
    pop     af                    ; AF = SCFlag; Stack = RtnAdr
    push    hl                    ; Stack = TxtPtr, RtnAdr
    call    .init_peek_string     ; DE = StrPtr, HL = ScrOfs
    ex      de,hl                 ; DE = ScrOfs, HL = StrPtr
    jr      z,.colorstring
    ld      iy,screen_read_bytes ; Read string
    jr      .gfx_call_putnew
.colorstring
    ld      iy,color_read_bytes
.gfx_call_putnew
    call    gfx_call
    jp      c,FCERR
    jp      PUTNEW                ; and return it

;-----------------------------------------------------------------------------
; DEEK
; syntax: DEEK(address)
;         DEEK(@page, address)
;-----------------------------------------------------------------------------
FN_DEEK:
    rst     CHRGET                ; Skip DEEK
    call    get_par_page_addr     ; AF = PgFlg, DE = Addr
    push    af                    ; Save it
    SYNCHKC ')'                   ; Require close paren
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
    jp      z,FCERR               ; FC error if illegal page
    jp      c,OVERR               ; Return overflow error if end of RAM
    jp      FLOAT_BC

_null_string:
    ld      hl,REDDY-1    ; Point at null terminator
    jp      TIMSTR        ; and return null string

paren_page_arg:
    SYNCHKC '('
; Check for and parse @page,
; Output: A, E = Page number`
;  Carry: Set if page specified
parse_page_arg:
    cp      '@'
    jr      nz,.notat             ; If page prefix
    rst     CHRGET                ;   Skip '@'
    call    GETBYT                ;   Parse byte into E
    call    get_comma
    ld      a,e
    scf
    ret
.notat
    or      a                     ; Clear Carry Flag
    ret


; WRITE KEYS "123":READ KEYS K$:PRINT K$
read_xtoken:
    inc     hl                    ; Skip XTOKEN
    SYNCHKT KEYTK                 ; Require KEY
ST_READ_KEYS:
    SYNCHKC 'S'                   ; Require S
    call    get_stringvar
    push    hl                    ; Stack = TxtPtr, RtnAdr
    push    de                    ; Stack = VarPtr, TxtPtr, RtnAdr
    call    get_strbuf_addr       ; HL = BufAdr, BC = BufLen
    call    aux_call_inline
    word    read_keys             ; C = StrLen
    call    strbuf_temp_str_c     ; Copy Buffer to Temporary. Return HL = StrDsc
    push    hl                    ; Stack = StrDsc, VarPtr, TxtPtr, RtnAdr
    jp      INBUFC                ; Copy Temporary to Variable and return

;-----------------------------------------------------------------------------
; Enhanced RESTORE statement stub
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
    SYNCHKT XTOKEN
    cp      TRKTK
    jp      z,ST_STOP_TRACK
    jp      SNERR

