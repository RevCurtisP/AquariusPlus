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
    call    PARCHK                ;   Parse Argument in Parentheses
    push    hl                    ;   Save Text Pointer
    call    CHKSTR                ;   TM Error if Not a String
    jp      hex_to_asc            ;   Convert to ASCII and return

asc_chr:
    ld      iy,SNGFLT
sng_chr:
    push    iy                    ; Stack = FltJmp, RtnAdr
    SYNCHK  '('
    call    GET_STRING
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
    SYNCHK  ')'
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
    SYNCHK  ','
.no_fpg
    call    GETINT                ; DE = SrcAdr
    pop     af                    ; AF = SrcPgFlg
.pg_addr
    push    de                    ; Stack = SrcAdr
    push    af                    ; Stack = SrcPgFlg, SrcAdr
    ld      a,(hl)
    cp      TOTK
    jr      z,.copy_page_addr_to
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

.copy_page_addr_to
    rst     CHRGET                ; Skip TO
    ld      ix,copy_to_screen
    rst     SYNCHR
    byte    SCRNTK                ;   Only TO SCREEN allowed right now
.do_page_addr_to
    pop     af                    ; AF = SrcPgFlg; Stack = SrcAdr, RtnAdr
    pop     de                    ; DE = SrcAdr; Stack = SrcAdr, RtnAdr
    jp      nc,MOERR              ; If no page, Missing operand error
    jp      (ix)

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
    cp      '('                   ; If not INPUT (...
    jp      nz,INPUT              ;   Do regular INPUT
    call    SCAND                 ; C = Col, E = Row
    ld      d,c                   ; D = Col
    push    de                    ; Stack = ColRow, RtnAdr
    call    get_comma_byte        ; A = MinLen
    push    af                    ; Stack = MinLen, ColRow, RtnAdr
    call    get_comma_byte        ; E = MaxLen
    pop     af                    ; A = MinLen; Stack = ColRow, RtnAdr
    ld      d,a                   ; D = MinLen
    push    de                    ; Stack = MinMax, ColRow, RtnAdr
    call    get_comma             ; Missing operand error if no comma
    rst     SYNCHR
    byte    INTTK                 ; Require INT (for now)
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
; Enhanced POKE
; syntax: POKE address, byte
;         POKE address, string$
;         POKE @page, address, byte
;         POKE @page, address, string$
;         POKE extaddr, byte
;         POKE extaddr, string$
;-----------------------------------------------------------------------------
ST_POKE:
    cp      SCRNTK
    jr      z,.pokescreen
    cp      COLTK
    jr      z,.pokecolor
.poke
    call    get_page_addr         ; AF = PgFlg, DE = Addr
    push    af                    ; Stack = Page, RtnAdr
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

.pokescreen
    rst     CHRGET                ; Skip SCREEN or ;
    call    .screen_args          ; Stack = AdrOfs, TxtPtr, RtnAdr
    jr      z,.screenstring       ; If Poking byte
    call    CONINT                ;   A = Byte
    pop     de                    ;   DE = AdrOfs; Stack = TxtPtr, RtnAdr
    ld      iy,screen_write_byte  ;   Write byte
    jr      .aux_call_next_screen ; Else
.screenstring
    call    free_addr_len         ;   DE = StrAdr, BC = StrLen
    ex      de,hl                 ;   HL = StrAdr
    pop     de                    ;   DE = AdrOfs; Stack = TxtPtr, RtnAdr
    ld      iy,screen_write_string
.aux_call_next_screen
    call    aux_call              ;   Write string
    jp      c,FCERR               ; Error if Offset too large
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ld      a,(hl)
    cp      ';'
    ret     nz
    jr      .pokescreen           ; Get next address
    ret

.pokecolor
    rst     CHRGET                ; Skip COL
    rst     SYNCHR
    byte    ORTK                  ; Require OR
.next_color
    call    .screen_args          ; Stack = AdrOfs, TxtPtr, RtnAdr
    jr      z,.colorstring        ; If Poking byte
    call    CONINT                ;   A = Byte
    pop     de                    ;   DE = AdrOfs; Stack = TxtPtr, RtnAdr
    ld      iy,color_write_byte   ;   Write byte
    jr      .aux_call_next_color  ; Else
.colorstring
    call    free_addr_len         ;   DE = StrAdr, BC = StrLen
    ex      de,hl                 ;   HL = StrAdr
    pop     de                    ;   DE = AdrOfs; Stack = TxtPtr, RtnAdr
    ld      iy,color_write_string ;   Write wtring
.aux_call_next_color
    call    aux_call
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
    SYNCHK  ')'                   ; Require close paren
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
    jp      z,IQERR               ; FC error if illegal page
    ld      a,c
    jr      .float_it

.peekstring
    rst     CHRGET                ; Skip token
    call    get_par_page_addr_len ; AF = PgFlg, BC = PkLen, DE = PkAdr
    push    af                    ; Stack = PgArg, RtnAdr
    SYNCHK  ')'                   ; Require close paren
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
    rst     SYNCHR                ; Require OR
    byte    ORTK
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
    jr      .aux_call
.color
    ld      iy,oolor_read_byte
.aux_call
    call    aux_call
    jp      c,FCERR
    ld      bc,LABBCK             ;
    push    bc                    ; Stack = LABBCK, TxtPtr, RtnAdr
    jp      SNGFLT

.peek_sc_string
    rst     CHRGET                ; Skip $
    SYNCHK  '('                   ; Require '('
    call    get_addr_len          ; DE = ScrOfs, BC = PkLen
    SYNCHK  ')'                   ; Require ')'
    pop     af                    ; AF = SCFlag; Stack = RtnAdr
    push    hl                    ; Stack = TxtPtr, RtnAdr
    call    .init_peek_string     ; DE = StrPtr, HL = ScrOfs
    ex      de,hl                 ; DE = ScrOfs, HL = StrPtr
    jr      z,.colorstring
    ld      iy,screen_read_string ; Read string
    jr      .aux_call_putnew
.colorstring
    ld      iy,color_read_string
.aux_call_putnew
    call    aux_call
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

_null_string:
    ld      hl,REDDY-1    ; Point at null terminator
    jp      TIMSTR        ; and return null string

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
; SGNINT(string$)
; SGNINT(string$, offset)
;-----------------------------------------------------------------------------
;; ToDo: Finish and add to dispatch table
FN_SGN:
    inc     hl                    ; Check character directly after SGN Token
    ld      a,(hl)                ; (don't skip spaces)
    cp      INTTK                  
    jr      nz,ABORT_FN
    rst     CHRGET                ; Skip INT
    call    frmprn_getype
    jr      nz,sgnint
    ld      iy,float_signed_int
    jp      word_str
    
ABORT_FN:
    dec     hl                    ; Back up to Function Token
    ld      a,(hl)                ; Re-Read Token
    sub     ONEFUN                ; Convert to Offset
    jp      HOOK27+1              ; Continue with Standard Function Code

sgnint:
    SYNCHK  ')'                   ; Require )
    call    push_hl_labbck        ; Stack = LABBCK, TxtPtr, RtnAdr
    call    FRCINT                ; DE = ArgVal
    jp      float_signed_int      ; Return as floated signed integer

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

;-----------------------------------------------------------------------------
; Enhanced STR$ function
; STR$(*array{,BYTE|INT}
;-----------------------------------------------------------------------------
; ToDo: Separate Array To String Logic into Callable Routine

; dim a(9):S$=STR$(*A)
FN_STRS:
    rst     CHRGET                ; Skip STR$
    SYNCHK  '('                   ; Require (
    cp      MULTK                 ;
    jr      z,.numeric_array;     ; If not *
    call    FRMEVL                ;   Evaluate argument
    SYNCHK  ')'                   ;   Require ')'
    jp      STR                   ;   Execute S3BASIC STR$ function
.numeric_array
    call    get_star_array        ; DE = AryDat, BC = AryLen
    call    CHKNUM                ; Type mismatch error if not numeric
    ld      (ARRAYPTR),de         ; ARRAYPTR = AryPtr
    ld      ix,.dofloat           ; Default to floating point
    ld      de,4                  ; SegLen = 4
    cp      ','
    jr      nz,.nocomma           ; If comma
    rst     CHRGET                ;
    rst     SYNCHR
    byte    XTOKEN                ;   Extended Tokens only
    ld      e,2                   ;   SegLen = 2
    ld      ix,.doword
    cp      WORDTK                ;   If not WORD
    jr      z,.skipchr
    dec     e                     ;   SegLen = 4
    ld      ix,.dobyte
    cp      BYTETK                ;   If not BYTE
    jp      nz,SNERR              ;     Syntax error
.skipchr
    rst    CHRGET                 ;   Skip BYTE/WORD
.nocomma
    SYNCHK  ')'                   ; Require ')'
    push    hl                    ; Stack = TxtPtr, RtnAdr
    sra     b
    rr      c                     ; BC = BC / 2
    sra     b
    jp      nz,LSERR              ; IF BC/4 is 255, String too long error
    rr      c                     ; BC = BC / 4
    ld      (ARRAYLEN),bc         ; ARRAYLEN = ArySiz
    call    mult_a_de             ; HL = StrLen
    ld      a,h
    or      a                     ; If StrLen > 255
    jp      nz,LSERR              ;   String too long error
    ld      a,l                   ; A = StrLen
    call    STRINI                ; DSCTMP = StrDsc, DE = StrAdr, A = StrLen
    ld      (BUFPTR),de           ; BUFPTR = StrPtr
    call    FREFAC                ; Free up temp
.loop
    call    jump_ix               ; Convert array element and write to string
    jr      nz,.loop              ; If not done, loop
    jp      PUTNEW                ; Else return the string
; Subroutines
.dobyte
    call    .array_to_facc
    call    CONINT                ; E = Byte
    ld      hl,(BUFPTR)           ; HL = StrPtr
    ld      (hl),e
    inc     hl
    jr      .next
.doword
    call    .array_to_facc
    call    FRCINT                ; DE = Word
    ld      hl,(BUFPTR)           ; HL = StrPtr
    ld      (hl),e
    inc     hl
    ld      (hl),d
    inc     hl                    ; Copy word to string
.next
    ld      (BUFPTR),hl           ; HL = StrPtr
    ld      hl,ARRAYLEN
    dec     (hl)                  ; ArySiz = ArySiz - 1
    ret
.dofloat:
    ld      hl,(ARRAYPTR)         ; HL = AryPtr
    ld      de,(BUFPTR)           ; DE = StrPtr
    ld      bc,4                  ; Copy 4 byte Float from Array to String
    ldir                          ; HL = AryPtr, DE = StrPtr
    ld      (ARRAYPTR),hl         ; ARRAYPTR = AryPtr
    ld      (BUFPTR),de           ; BUFPTR = StrPtr
    ret
; Copy array element to FACC
.array_to_facc
    ld      hl,(ARRAYPTR)         ; HL = AryPtr
    ld      de,FACLO              ; DstAdr = FACC
    ld      bc,4
    ldir                          ; Do copy
    ld      (ARRAYPTR),hl         ; ARRAYPTR = AryPtr
    ret
