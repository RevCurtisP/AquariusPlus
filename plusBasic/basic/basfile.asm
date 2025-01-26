;====================================================================
; File I/O Statements and Functions
;====================================================================

;-----------------------------------------------------------------------------
; CD - Change directory
;
; No argument -> Show current directory
; With argument -> Change current directory
;-----------------------------------------------------------------------------
; SET FILE ERROR OFF:CD "x":PRINT ERR,ERR$
; SET FILE ERROR OFF:CD "/":PRINT ERR,ERR$
; Currently CD with no arg and CD$ do not error, even if no SD-card
; SET FILE ERROR OFF:CD:PRINT ERR,ERR$
; SET FILE ERROR OFF:PRINT CD$;ERR,ERR$
ST_CD:
    call    _clear_errflag
    jr      nz, .change_dir       ; If no argument
    push    hl                    ;   Stack = TxtPtr, RtnAdr
    call    _get_cd               ;   StrBuf = DirName
    call    print_c_string        ;   Print StrBuf
    pop     hl                    ;   HL = TxtPtr; Stack = RtnAdr
    ret                           ; Else
.change_dir:
    call    get_string_direct     ; BC = StrLen, DE = StrAdr, Stack = TxtPtr, RtnAdr
    ld      iy,dos_change_dir
_aux_call_hl_error:
    call    aux_call
_pop_hl_doserror:
    pop     hl
_ret_p_doserror:
    ret     p
dos_error:
_dos_error:
    cpl                           ; Convert -1 to 0, -1 to 2, etc
    cp      ERRBDF
    jr      z,.badfile            ; If not Bad file error
    add     a,a                   ;   Multiply by 2 to get offset
    add     a,ERRFNF              ;   Add to start of DOS errors
.badfile
    ld      e,a
    call    esp_close_all
    ld      a,(EXT_FLAGS)
    and     FERR_FLAG
    jp      nz,ERROR
    push    hl
    call    set_error
    pop     hl
    ret

_not_eof_error:
    cp      ERR_EOF
    ret     z
    jr      _dos_error

_pop_pop_doserror:
    pop     hl                    ; Stack = TxtPtr
    jr      _pop_hl_doserror

;-----------------------------------------------------------------------------
; CD$ - Get Current Directory
;-----------------------------------------------------------------------------
FN_CD:
    rst     CHRGET                ; Skip Token
    SYNCHK  '$'                   ; Require Dollar Sign
    push    hl                    ; Stack = TxtPtr, RtnAdr
    call    _get_cd
    ld      bc,LABBCK
    push    bc                    ; Stack = LABBCK, TxtPtr, RtnAdr
    jp      TIMSTR

; Read current directory into string buffer
_get_cd:
    call    get_strbuf_addr       ; HL = StrBuf
    ld      iy,dos_get_cwd        ; Read current directory into buffer
_aux_call_doserror:
    call    aux_call
    jr      _ret_p_doserror

;-----------------------------------------------------------------------------
; DEL - Delete file/directory
; Syntax: DEL filespec$
;-----------------------------------------------------------------------------
; SET FILE ERROR OFF: DEL xxx:PRINT ERR,ERR$
ST_DEL:
    call    get_string_direct     ; Stack = TxtPtr, RtnAdr
    ld      iy,dos_delete
    call    aux_call
    jp      m,_pop_hl_doserror
    call    in_direct
    pop     hl                    ; HL = TxtPtr
    ret     c                     ; Return if not in direct mode
    call    CHRGT2                ; Reget current character
    ret     z                     ; Return if terminator
    jr      ST_DEL

;-----------------------------------------------------------------------------
; MKDIR - Create directory
; Syntax: MKDIR dirname$
;-----------------------------------------------------------------------------
; SET FILE ERROR OFF: MKDIR ?:PRINT ERR,ERR$
; SET FILE ERROR OFF: MKDIR xxx/yyy:PRINT ERR,ERR$
; SET FILE ERROR OFF: MKDIR xxx:PRINT ERR,ERR$
ST_MKDIR:
    call    get_string_direct     ; Stack = TxtPtr, RtnAdr
    ld      iy,dos_create_dir
    jp      _aux_call_hl_error

;-----------------------------------------------------------------------------
; RENAME - Rename a filmke
; Syntax: RENAME oldfile$ TO newfile$
;-----------------------------------------------------------------------------
; SET FILE ERROR OFF: RENAME "x" TO "y":PRINT ERR,ERR$
ST_RENAME:
    ld      de,dos_rename         ; DE = CallAdr
    call    _file_from_to         ; A = Result
    jr      _ret_p_doserror

;-----------------------------------------------------------------------------
; COPY FILE - Copy a file
; Syntax: COPY FILE oldfile$ TO newfile$
;-----------------------------------------------------------------------------
; COPY FILE "dump.bin" TO "copy.bin"
; COPY FILE "test.bin" TO "copy.bin"
; SET FILE ERROR OFF: COPY "x" TO "y":PRINT ERR,ERR$
ST_COPY_FILE:
    rst     CHRGET                ; Skip FILE
    ld      de,file_copy
    call    _file_from_to
    jp      m,_not_eof_error      ; Check for error and return
    ret

_file_from_to:
    push    de                    ; Stack = CallAdr, RtnAdr
    call    FRMEVL                ; Parse oldname
    call    CHKSTR                ; Gotta be a string
    call    SYNCHR                ; Require TO token
    byte    TOTK
    push    hl                    ; Stack = TxtPtr, CallAdr, RtnAdr
    ld      hl,(FACLO)            ; HL = OldDsc
    ex      (sp),hl               ; HL = TxtPtr, Stack = OldDsc, CallAdr, RtnAdr
    call    FRMEVL                ; Parse newname
    push    hl                    ; Stack = TxtPtr, OldDsc, CallAdr, RtnAdr
    call    FRESTR                ; HL = NewDsc
    ex      de,hl                 ; DE = NewDsc
    pop     bc                    ; BC = TxtPtr, Stack = OldDsc, CallAdr, RtnAdr
    pop     hl                    ; HL = OldDsc; Stack = CallAdr, RtnAdr
    pop     iy                    ; IY = CallAdr; Stack = RtnAdr
    push    bc                    ; Stack = TxtPtr, RtnAdr
    push    de                    ; Stack = NewDsc, TxtPtr, RtnAdr
    call    FRETM2                ; HL = OldDsc
    pop     de                    ; DE = NewDsc, Stack = TxtPtr, RtnAdr
    call    aux_call
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; FILE Functions stub
;-----------------------------------------------------------------------------
FN_FILE:
    rst     CHRGET                ; Skip FILE
    cp      DIRTK                 ; If DIR
    jr      z,FN_FILEDIR          ;   Do FILEDIR$()
    cp      DATETK                ; If DATE
    jr      Z,FN_FILEDATETIME     ;   Do FILEDATETIME$()
    cp      LENTK                 ; If LEN
    jr      Z,FN_FILELEN          ;   Do FILELEN()
    rst     SYNCHR
    byte    XTOKEN                ; Must be extended Token
    cp      ATTRTK                ; If ATTR
    jr      z,FN_FILEATTR         ;   Do FILEATTR$()
    cp      EXTTK                 ; If EXT
    jr      z,FN_FILEEXT          ;   Do FILEEXT$()
    cp      STATK                 ; If STAT
    jr      z,FN_FILESTATUS       ;   Do FILESTAT$()
    jp      SNERR

;-----------------------------------------------------------------------------
; FILEEXT$(filespec$)
;-----------------------------------------------------------------------------
; ? FILEEXT$("file.ext")
; ? FILEEXT$("no_ext")
; ? FILEEXT$("a/b/file.tmp")
FN_FILEEXT:
    ld      iy,file_get_ext
    jr      _file_trim

;-----------------------------------------------------------------------------
; FILEDIR$(filespec$)
;-----------------------------------------------------------------------------
; ? FILEDIR$("file.ext")
; ? FILEDIR$("/no_ext")
; ? FILEDIR$("/a/b/file.tmp")
; ? FILEDIR$("a/")
; ? FILEDIR$("/a/b/")
; D$ = FILEDIR$("/a/b/file.tmp")+"sub"
FN_FILEDIR:
    ld      iy,file_get_dir
    jr      _file_trim

;-----------------------------------------------------------------------------
; FILELEN(filespec$)
;-----------------------------------------------------------------------------
; ? FILELEN("1.palt")
FN_FILELEN:
    rst     CHRGET                ; Skip ATTR
    call    PARCHK                ; Parse agument
    call    CHKSTR                ; Error if not string
    call    push_hl_labbck        ; Stack = LABBCK, TxtPtr, RtnAdr
    call    FRESTR                ; HL = StrDsc
    ld      iy,file_size
    call    _aux_call_doserror    ; Get FilSiz
    jp      FLOAT_DEBC            ; Float it and return

;-----------------------------------------------------------------------------
; FILEATTR(filespec$)
;-----------------------------------------------------------------------------
; ? FILEATTR("1.palt")
FN_FILEATTR:
    rst     CHRGET                ; Skip ATTR
    call    PARCHK                ; Parse agument
    call    CHKSTR                ; Error if not string
    call    push_hl_labbck        ; Stack = LABBCK, TxtPtr, RtnAdr
    call    FRESTR                ; HL = StrDsc
    ld      iy,file_attrs
    call    _aux_call_doserror    ; Get AtrByt
    jp      SNGFLB                ; Float it and return

;-----------------------------------------------------------------------------
; FILEDATETIME$(filespec$)
;-----------------------------------------------------------------------------
; ? FILEDATETIME$("1.palt")
FN_FILEDATETIME:
    inc     hl                    ; Skip DATE
    rst     SYNCHR
    byte    TIMETK
    ld      iy,file_datetime
    push    iy                    ; Stack = AuxRtn, RtnAdr
    ld      a,14                  ; A = StrSiz
    jr      _file_stat            ; Go do it
  
;-----------------------------------------------------------------------------
; FILESTATUS$(filespec$)
;-----------------------------------------------------------------------------
; S$=FILESTATUS$("1.palt")
; ? HEX$(S$)
FN_FILESTATUS:
    ld      iy,dos_stat
    push    iy                    ; Stack = AuxRtn, RtnAdr
    ld      a,9                   ; A = StrSiz
    inc     hl                    ; Skip STATUS
_file_stat:
    push    af                    ; Stack = StrSiz, AuxRtn, RtnAdr
    SYNCHK  '$'                   ; Require $
    call    PARCHK                ; Parse agument
    call    CHKSTR                ; Error if not string
    pop     a                     ; A = StrSiz; Stack = AuxRtn, RtnAdr
    pop     iy                    ; IY = AuxRtn; Stack = RtnAdr
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ld      hl,(FACLO)            ; HL = ArgDsc
    push    hl                    ; Stack = ArgDsc, TxtPtr, RtnAdr
    call    GETSPA                ; DE = StrAdr
    call    FRETMS                ; Free temporary but not string space
    pop     hl                    ; HL = ArgDsc; Stack = TxtPtr, RtnAdr
    push    de                    ; Stack = StrAdr, TxtPtr, RtnAdr
    call    FRETM2                ; Free ArgDsc
    pop     de                    ; DE = StrAdr; Stack = TxtPtr, RtnAdr
    call    _aux_call_doserror    ; Populate string
    jp      STRNEL                ; Return the string

;-----------------------------------------------------------------------------
; TRIMDIR$(filespec$)
;-----------------------------------------------------------------------------
; ? TRIMDIR$("file.ext")
; ? TRIMDIR$("a/no_ext")
; ? TRIMDIR$("/a/b/file.tmp")
; ? TRIMDIR$("a/")
; ? TRIMDIR$("/a/b/")
FN_TRIMDIR:
    ld      iy,file_trim_dir
    jr      _file_trim

;-----------------------------------------------------------------------------
; TRIMEXT$(filespec$)
;-----------------------------------------------------------------------------
; ? TRIMEXT$("file.ext")
; ? TRIMEXT$("no_ext")
; ? TRIMEXT$("a/b/file.tmp")
FN_TRIMEXT:
    ld      iy,file_trim_ext
_file_trim:
    push    iy                    ; Stack = TrmRtn, RtnAdr
    rst     CHRGET                ; Skip EXT/DIR
    SYNCHK  '$'                   ; Require string
    call    PARCHK                ; Parse agument
    pop     iy                    ; IY = TrmRtn; Stack = RtnAdr
    push    hl                    ; Stack = TxtPtr, RtnAdr
    call    FRETMS                ; Free temporary but not string space
    call    faclo_addr_len        ; DE = StrAdr, BC = StrLen
    call    aux_call              ; DE = ExtAdr, A = ExtLen
    jr      nz,.ret_str
    ld      de,REDDY-1
.ret_str
    jp      STRNEW                ; Create temporary and return it

;-----------------------------------------------------------------------------
; DIR - Directory listing
;
; No argument -> List current directory
; With argument -> List given path
;-----------------------------------------------------------------------------
; SET FILE ERROR OFF:DIR:PRINT ERR.ERR$
ST_DIR:

    call    _clear_errflag

    ; Argument given?
    call    CHRGT2
    jr      nz, .witharg          ; Yes

    push    hl                    ; Save Text Pointer
    ld      hl,null_desc          ; Empty String
    jr      .esp_command

.witharg:
    call    get_string_direct     ; HL = StrDsc; Stack = TxtPtr, RtnAdr

.esp_command:
    ld      iy,aux_open_dir
    call    aux_call
    jp      m,dos_error
    push    af                    ; Stack = DirSpc, TxtPtr, RtnAdr
    ; Set initial number of lines per page
    ld      a, 24
    ld      (CNTOFL), a

    call    get_strbuf_addr       ; HL = StrBuf
    ld      iy,file_read_dir_asc

.dirloop
    pop     af                    ; A = DirSpc; Stack = TxtPtr
    push    af                    ; Stack = DirSpc, TxtPtr, RtnAdr
    call    aux_call
    jp      m,.direrr
    push    hl                    ; Stack = BufAdr, TxtPtr, RtnAdr
    call    print_c_string        ; Print directory entry
    call    CRDO                  ; Print CR/LF
    pop     hl                    ; HL = BufAdr; Stack = TxtPtr, RtnAdr
    jr      .dirloop
.direrr
    pop     de                    ; D = FilDsc
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    push    af                    ; Stack = Result, RtnAdr
    ld      a,d                   ; A = FilDsc
    ld      iy,dos_close_dir
    call    aux_call
    pop     af                    ; A = Result; Stack = RtnAdr
    cp      ERR_EOF               ; If End of File
    ret     z                     ;   Return
    jp      dos_error

;-----------------------------------------------------------------------------
; LOAD "filename"                 Load BASIC program
; LOAD "filename",address         Load file as raw binary to address
; LOAD "filename",@page,address   Load file as raw binary to address in page
; LOAD "filename",*var            Load data into numeric array var
; LOAD "filename",*var$           Load binary data into string array var$
; LOAD "filename",*var$,ASC       Load ASCII data into string array var$
; LOAD BITMAP "filename"          Load bitmap file into Video RAM
; LOAD CHRSET "filename"          Load character set into character RAM buffer
; LOAD DIR "filename",*var$       Load raw directory entries into string array
; LOAD DIR "filename",*var$,ASC   Load formatted directory into string array
; LOAD FNKEYS "filename"          Load function key definitions
; LOAD PALETTE p,"filename"       Load one or all palettes
; LOAD SCREEN "filename"          Load screen with optional embedded palette
; LOAD TILEMAP "filename"         Load tilemap into Video RAM
; LOAD TILESET index,"filename"   Load tile data into Video RAM
;-----------------------------------------------------------------------------
ST_LOAD:
    call    _clear_errflag

    ld      a,(hl)
    cp      BITTK                 ; $EB
    jp      z,_load_bitmap

    cp      FNTK                  ; $A2
    jp      z,_load_fnkeys

    cp      DIRTK                 ; $DD
    jp      z,load_dir_array

    cp      TILETK                ; $F0
    jp      z,_load_tile

    cp      COLTK                 ; $F6
    jp      z,_load_colormap

    cp      SCRNTK                ; $F6
    jp      z,_load_screen

    cp      XTOKEN                ; $FE
    jp      z,_load_extended

    ; Get string parameter with path

    call    get_strdesc_arg       ; H: = StrDsc; Stack = TxtPtr, RtnAdr

    ex      (sp),hl               ; HL = TxtPtr, Stack = StrDsc, RtnAdr

    ; Check for second parameter
    call    CHRGT2
    cp      ','
    jr      nz,.basic             ; No parameter -> load as basic program
    rst     CHRGET
    cp      MULTK                 ; Token for '*'
    jp      z,load_array          ; Array parameter -> load as array
    cp      EXPTK
    jr      z,_load_string


; Load raw binary to address
; LOAD "tron.bin",$4200
; LOAD "tron.bim",$4200
    cp      '!'
    jr      nz,.not_ext
    call    get_ext_addr          ;   AF = PgFlg, DE = Addr    
    jr      .load_bin
.not_ext
    call    get_page_arg          ; Check for page specifier
    push    af                    ; Stack = Page, String Descriptor
    jr      nc,.bin_params        ; If page specified
    call    CHRGT2                ;   Reget character and set flags
    jr      nz,.page_params       ;   If @page only
    ld      de,0
    ld      bc,$4000
    jr      .pop_load_bin         ;   Else
.page_params
    SYNCHK  ','
.bin_params
    call    GETINT                ;     DE = Addr
    ld      bc,$FFFF              ;     Load up to 64k
.pop_load_bin
    ; Get back page filespec
    pop     af                    ; AF = Page, Stack = String Descriptor
.load_bin
    ex      (sp),hl               ; HL = String Descriptor, Stack = Text Pointer
    jr      c,.load_paged
    ld      iy,file_load_binary
    jp      _aux_call_hl_error

; Load raw binary into paged memory
; LOAD "/roms/logo.rom",@40,$1234
; LOAD "/roms/bio.rom",@63
.load_paged
    ld      bc,$FFFF              ; Load up to 64k
    call    check_paged_address   ; Verify pages addres is between 0 and 16383
    ld      iy,file_load_paged
    call    aux_call
    jp      z,FCERR
    jp      c,OVERR
    jp      _pop_hl_doserror

.basic
    ex      (sp),hl               ; HL = String Descriptor, Stack = Text Pointer
    jp      load_basic_program

; LOAD "t/stringtest.str",^L$:PRINT L$
; LOAD "t/xxx.xxx",^L$
; SET FILE ERROR OFF:LOAD "t/xxx.xxx",^L$:PRINT ERR
; SET FILE ERROR OFF:LOAD "t/stringtest.str",^L$:PRINT ERR
_load_string:
    rst     CHRGET                ; Skip ^
    call    get_stringvar         ; DE = VarPtr
    ex      (sp),hl               ; HL = NamDsc; Stack = TxtPtr, RtnAdr
    push    de                    ; Stack = VarPtr, TxtPtr, RtnAdr
    ld      iy,file_load_strbuf   ; Load file into StrBuf
    call    aux_call              ; A = Result, BC = StrLen, HL = BufAdr
    jp      m,_pop_pop_doserror   ; Abort if error
    ld      a,c
    call    strbuf_temp_str       ; BC = StrLen, DE = StrAdr, HL = StrDsc
    push    hl                    ; Stack = StrDsc, VarPtr, TxtPtr
    jp      INBUFC                ; Copy Temporary to Variable and return

;-----------------------------------------------------------------------------
; Load BASIC Program in ASCII format
; Input: HL: String descriptor address
;     Stack: TxtPtr, RtnAdr
;-----------------------------------------------------------------------------
;; RUN "/t/memvars.baq"
;; RUN "/t/ascprog.bas"
;; RUN "/t/asczero.bas
;; RUN "/t/ascbad.bas
;; RUN "/t/ascdup.bas
;; RUN "/t/ascsnerr.bas
_load_ascii:
    ld      hl,(TXTTAB)
    inc     hl
    inc     hl
    ld      (VARTAB),hl           ; Start new program
    ld      bc,0                  ; Init Previous Line to 0
    push    bc                    ; Stack = PrvLin, TxtPtr, RtnAdr

.lineloop
    call    get_strbuf_addr       ; HL = StrBuf
    call    esp_read_line         ; BC = LinLen
    jp      m,.done
    ld      d,h                   ; DE = StrBuf
    ld      e,l
    call    tokenize_line         ; BC = LinNum
    jr      z,.lineloop           ; If blank, read next line
    ex      de,hl                 ; HL = StrBuf
    ld      d,b
    ld      e,c                   ; DE = LinNum
    inc     de                    ; DE = Bump LinNum for COMPAR
    ex      (sp),hl               ; HL = PrvLin; Stack = StrBuf, TxtPtr, RtnAdr
    rst     COMPAR                ; If PrvLin >= LinNum
    jr      nc,.badline           ;   Issue Error
    ex      de,hl                 ; HL = LinNum+1
    ex      (sp),hl               ; HL = StrBuf; Stack = LinNum+1, TxtPtr, RtnAdr
    ex      de,hl                 ; DE = StrBuf
    call    basic_append_line     ; Add line to BASIC program
    jr      .lineloop
.badline
    ld      a,ERRUS
    or      a
.done
    pop     bc                    ; Discard PrvLin; Stack = TxtPtr, RtnAdr
    push    af                    ; Stack = Status, TxtPtr, RtnAdr
    call    init_basic_program
    pop     af                    ; AF = Status; Stack = TxtPtr, RtnAdr
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret     z                     ; Return if no errors
    jp      m,_not_eof_error      ; If DOS error, process it
    ld      e,a
    jp      ERROR                 ; Else issue BASIC error


;-----------------------------------------------------------------------------
; AQX Header
;  0 - 5    "AQPLUS"
;  6 - 9    "EXEC"
; 10 - 11   Load address
; 11 - 13   Program Length
; 14 - 15   Execute address
;-----------------------------------------------------------------------------
run_aqx_program:
    call    ferr_flag_on
    call    _open_read            ; A = FilDsc
    
    ld      bc,16
    ld      de,FBUFFR
    call    esp_read_bytes        ; Read aqx header
    ld      a,l
    ex      af,af'                ; AF' = FilDsc
    ld      a,16
    cp      c                     ; If full header not read
    jp      nz,.badfile           ;   Bad file error
; Validate load address and length
    ld      de,(FBUFFR+10)        ; DE = Load address
    ld      bc,(FBUFFR+12)        ; BC = Program Length
    ld      hl,$4000              
    rst     COMPAR                ; If Load Address < $4000
    jp      c,.badfile            ;   Bad file error
    add     hl,bc                 ; 
    ex      de,hl                 ; DE = PrgEnd
    ld      hl,-512
    add     hl,sp                 ; HL = StkPtr - 512
    rst     COMPAR                ; If TopAdr < PrgEnd
    jp      c,.badfile            ;   Bad file error
; Load the executable
 ;   dec     de                    ; Back up to last byte of program
    push    de                    ; Stack = EndAdr
    ex      af,af'                ; AF = FilDsc
    ld      de,(FBUFFR+10)        ; DE = Load Address
    call    esp_read_bytes        ; DE = EndAdr
    ld      a,l                   ; L = FilDsc
    ld      iy,dos_close
    call    aux_call              ; Close file
    pop     hl                    ; HL = PrgEnd
; Check execution address
    rst     COMPAR                ; If EndAdr < PrgEnd
    jp      c,.badfile            ;   Bad file error
    ld      de,(FBUFFR+14)        ; DE = ExeAdr
    rst     COMPAR                ; If ExeAdr >= PrgEnd
    jp      c,.badfile            ;   Bad file error
    ld      hl,(FBUFFR+10)        ; HL = BgnAdr
    rst     COMPAR                ; If ExeAdr < BgnAdr
    jp      c,.badfile            ;   Bad file error
; Start the program    
    ex      de,hl                 ; HL = ExeAdr
    call    jump_hl
    pop     hl
    ret
.badfile
    call    _badfile
    jp      _pop_hl_doserror    
    
;-----------------------------------------------------------------------------
; Load CAQ/BAS file
; Input: HL: String descriptor address
; Clobbered registers: A, DE
;-----------------------------------------------------------------------------
; LOAD "/t/memvars.baq"
; SET FILE ERROR OFF:LOAD "xxxxx"
load_basic_program:
    ; Open file
    
    call    ferr_flag_on
    call    _open_read

    call    esp_read_byte         ; B = First byte of file
    jp      m,_dos_error          ; Check for error
    call    esp_close_all         ; Close and re-open file
    call    _open_read
    inc     b                     ; If first byte = $FF
    jr      z,.load_caq_prog      ;   Load BAQ/CAQ
    dec     b                     ; If first byte <> 0
    jp      nz,_load_ascii        ;   Load program in ASCII format
; LOAD "/t/savetok.bas"
    ld      de,(TXTTAB)           ; Else
    dec     de                    ;   DE = LoadAdr(0 before BASIC program)
    jr      .load_prog

.load_caq_prog
    call    check_sync_bytes      ; Sync bytes
    ld      bc, 6                 ; Read filename
    ld      de, FILNAM
    call    esp_read_bytes
    call    check_sync_bytes      ; Sync bytes
    ld      de, (TXTTAB)
.load_prog
    ld      bc, $FFFF
    call    esp_read_bytes

    ; Close file
    call    esp_close_all

    ; Back up to last line of BASIC program
    ex      de,hl                 ; HL = EndAdr
.backup:
    dec     hl                    ; Back up to last byte loaded
    xor     a
    cp      (hl)                  ; Back up to last non-zero byte
    jr      z, .backup
    inc     hl

    ; Skip past 3 zeros at end of BASIC program
    ld      b,3
.zeros
    cp      (hl)
    ;jp      nz,BDFERR
    inc     hl
    djnz    .zeros

    ; Set end of BASIC program
    ld      (VARTAB), hl

    ; Initialize BASIC program
    call    init_basic_program

    pop     hl                    ; Discard Callers Return Address
    ret

sync_bytes:
    db $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00

;-----------------------------------------------------------------------------
; Initialize BASIC Program
;
; Resets variables, arrays, string space etc.
; Updates nextline pointers to match location of BASIC program in RAM
;-----------------------------------------------------------------------------
init_basic_program:
    ; Set next statement to start of program
    ld      hl, (TXTTAB)
    dec     hl
    ld      (SAVTXT), hl

    ; Set DATPTR to start of program
    ld      (DATPTR), hl

    ; Clear string space
    ld      hl, (MEMSIZ)
    ld      (FRETOP), hl

    ; Clear simple variables
    ld      hl, (VARTAB)
    ld      (ARYTAB), hl

    ; Clear array table
    ld      (STREND), hl

    ; Clear string buffer
    ld      hl, TEMPPT + 2
    ld      (TEMPPT), hl

    ; Set CONTinue position to 0
    xor     a
    ld      l, a
    ld      h, a
    ld      (OLDTXT), hl

    ; Clear locator flag
    ld      (SUBFLG), a

    ; Clear Variable Name
    ld      (VARNAM), hl

    jp      basic_link_lines      ; Link lines and return

; Load file contents into array
; Stack: StrDsc, RtnAdr
; free_rom_ext   = 2601
load_array:
    call    skip_star_array    ;  A = NxtChr, DE = AryPtr, BC = AryLen
    ld      iy,aux_load_caq_array
    jr      nz,.load_array
    ld      iy,aux_load_str_array
    call    CHRGT2
    jr      z,.load_array
    SYNCHK  ','
    rst     SYNCHR
    byte    ASCTK
    ld      iy,aux_load_asc_array
.load_array
    ex      (sp),hl               ; HL = StrDsc, Stack = TxtPtr, RtnAdr
    call    aux_call
    jp      _pop_hl_doserror      ; Pop TxtPtr and check for error

load_dir_array:
    rst     CHRGET                ; Skip DIR
    cp      MULTK                 ; If *
    jr      nz,.dir_arg
    push    hl                    ;   Stack = TxtPtr, RtnAdr
    ld      hl,null_desc          ;   HL = NulDsc
    ex      (sp),hl               ;   HL = TxtPtr, Stack = StrDsc, RtnAdr
    jr      .dir_cont             ; Else
.dir_arg
    call    get_strdesc_arg       ;   HL = StrDsc; Stack = TxtPtr, RtnAdr
    ex      (sp),hl               ;   HL = TxtPtr, Stack = StrDsc, RtnAdr
    SYNCHK  ','                   ;   Require ,
.dir_cont
    call    get_star_array        ; A = NxtChr, DE = AryPtr, BC = AryLen
    call    CHKSTR                ; Type mismatch error if not string array
    ld      iy,aux_load_dir
    call    CHRGT2
    jr      z,.load_dir
    SYNCHK  ','
    ld      iy,aux_load_dir_ascii
    cp      ASCTK
    jr      z,.skip_load_dir
    rst     SYNCHR
    byte    XTOKEN
    ld      iy,aux_load_dir_bin
    rst     SYNCHR     
    byte    BINTK
    byte    $3E                   ; LD A over RST
.skip_load_dir
    rst     CHRGET                ; Skip ASC or BIN
.load_dir
    ex      (sp),hl               ; HL = StrDsc, Stack = TxtPtr, RtnAdr
    call    aux_call              ; Open directory
    jp      _pop_hl_doserror      ; Pop TxtPtr and check for error

_load_fnkeys:
    call    _set_up_fnkeys
    ld      iy,xfile_load_paged
    jp      _aux_call_hl_error

_set_up_fnkeys:
    rst     CHRGET                ; Skip FN
    rst     SYNCHR
    byte    XTOKEN
    rst     SYNCHR                ; Require KEY
    byte    KEYTK
    SYNCHK  'S'                   ; Require S
    call    get_strdesc_arg       ; HL = FilDsc; Stack = TxtPtr, RtnAdr
    ex      de,hl                 ; DE = FilDsc
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ex      (sp),hl               ; HL = RtnAdr; Stack = TxtPtr
    push    hl                    ; Stack = RtnAdr, TxtPtr
    ex      de,hl                 ; HL = FilDsc
    ld      a,BAS_BUFFR
    ld      de,FKEYDEFS
    ld      bc,512
    ret

_load_extended:
    rst     CHRGET                ; Skip XTOKEN
    cp      PALETK                ; $81
    jr      z,_load_palette
    cp      PT3TK                 ; $8C
    jr      z,_load_pt3
    rst     SYNCHR      
    byte    CHRTK                 ; $85
    rst     SYNCHR                ; Must be CHRSET
    byte    SETTK
; load chrset "/chrsets/linedraw.char"
; load chrset "/chrsets/seraphim.char"
_load_chrset:
    call    get_strdesc_arg       ; HL = FileSpec StrDsc; Stack = TxtPtr
; Pops HL before returning
    byte    $3E                   ; LD A, over PUSH HL
load_chrset:
    push    hl
    ld      iy,file_load_chrset   ; Load into alternate character set buffer
    jp      _aux_call_bdf

; SAVE CHRSET "/t/test.chrs"
; PRINT COMPARE "/t/test.chrs" TO "esp:/default.chr"
_save_chrset:
    call    get_strdesc_arg       ; HL = FilStd; Stack = TxtPtr, PalNum, FilRtn, RtnAdr
    ld      iy,file_save_chrset
    jr      _aux_call

_save_palette:
; save palette 0,"/t/default.pal"
; save palette 1,"/t/palette.txt",ASC
    ld      iy,file_save_palette
    jr      _do_palette
; load palette 0,"/t/gray.pal"
; load palette 0,"/t/palette.txt",ASC
_load_palette:
    ld      iy,file_load_palette
_do_palette:
    rst     CHRGET                ; Skip PALETTE
    push    iy                    ; Stack = FilRtn
    call    get_byte4             ; A = PalNum
    push    af                    ; Stack = PalNum, FilRtn, RtnAdr
    SYNCHK  ','                   ; Require comma
    call    get_strdesc_arg       ; HL = FilStd; Stack = TxtPtr, PalNum, FilRtn, RtnAdr
    ex      (sp),hl               ; HL = TxtPtr; Stack = FilStd, PalNum, FilRtn, RtnAdr
    call    CHRGT2                ; Reget Character
    ld      b,0
    jr      z,.do_save_load       ; If not terminator
    SYNCHK  ','                   
    rst     SYNCHR                ; Require ,ASC
    byte    ASCTK   
    dec     b
.do_save_load:    
    ex      (sp),hl               ; HL = FilStd; Stack = TxtPtr, PalNum, FilRtn, RtnAdr
    pop     de                    ; DE = TxtPtr
    pop     af                    ; A = PalNum; Stack = FilRtn, RtnAdr
    pop     iy                    ; IY = FilRtn; Stack = RtnAdr
    push    de                    ; Stack = TxtPtr, RtnAdr
    jp      _aux_call_bdf

; load pt3 "/music/songs1/dontstop.pt3"
; load pt3 "/music/songs1/dance.pt3"
load_pt3:
    dec     hl                    ; Back up for skip
_load_pt3:
    ld      iy,file_load_pt3      ; Load character set and copy to character RAM
    jp      skip_strdesc_auxcall

_aux_call
    call    aux_call
    jp      m,_dos_error
    pop     hl
    ret


; SAVE TILEMAP "/t/savemap.tmap"
; SAVE TILESET 128,249,"/t/savetiles.tile"
_save_tile:
    rst     CHRGET                ; Skip TILE
    cp      MAPTK
    jr      nz,_save_tileset      ; If SAVE TILEMAP
    call    chrget_strdesc_arg    ;   HL = StrDsc; Stack = TxtPtr, TileNo, RtnAdr
    ld      iy,file_save_tilemap  ;   Load tilemap
    jr      _aux_call             ; Else
_save_tileset
    rst     SYNCHR
    byte    SETTK                 ; Require SET
    call    get_int512            ; DE = TileNo
    call    get_comma             ; Require Comma
    push    de                    ; Stack = TileNo, RtnAdr
    call    get_int512            ; DE = TilCnt
    call    get_comma             ; Require Comma
    push    de                    ; Stack = TilCnt, TileNo, RtnAdr
    call    get_strdesc_arg       ; HL = StrDsc; Stack = TxtPtr, TilCnt, TileNo, RtnAdr
    pop     ix                    ; IX = TxtPtr; Stack = TilCnt, TileNo, RtnAdr
    pop     bc                    ; BC = TilCnt; Stack = TileNo, RtnAdr
    pop     de                    ; DE = TileNo; Stack = RtnAdr
    push    ix                    ; Stack = TxtPtr, RtnAdr
    ld      iy,file_save_tileset  ; Save tiles
    jr      _aux_call

; LOAD TILEMAP "/t/level1.tmap"
; LOAD TILESET "/au/assets/tiles.tset"
; LOAD TILESET INDEX 128,"/au/assets/tiles.tset"
; LOAD TILESET OFFSET 1,"/au/assets/tiles.tset"
_load_tile:
    rst     CHRGET                ; Skip TILE
    cp      MAPTK
    jr      nz,_load_tileset      ; If LOAD TILEMAP
    ld      iy,file_load_tilemap  ;   Load tilemap
    jp      skip_strdesc_auxcall  ; Else
_load_tileset:
    rst     SYNCHR
    byte    SETTK                 ;   Require SET
    ld      de,0
    call    _index_offset
    jr      nz,.skip
    push    bc
    call    get_int512            ; DE = TileNo
    call    get_comma             ; Require Comma
    pop     bc
    ex      de,hl
    add     hl,bc
    ex      de,hl                 ; DE = Index + Offset
.skip    
    push    de                    ;   Stack = TileNo, RtnAdr
    call    get_strdesc_arg       ;   HL = StrDsc; Stack = TxtPtr, TileNo, RtnAdr
    pop     bc                    ;   BC = TxtPtr; Stack = TileNo, RtnAdr
    pop     de                    ;   DE = TileNo; Stack = RtnAdr
    push    bc                    ;   Stack = TxtPtr, RtnAdr
    ld      iy,file_load_tileset  ;   Load tiles
    jr      _aux_call_bdf



;-----------------------------------------------------------------------------
; Bitmap file formats
;-----------------------------------------------------------------------------
_save_bitmap:
    rst     CHRGET                ; Skip BIT
    rst     SYNCHR                ; Require MAP
    byte    MAPTK
    call    get_strdesc_arg       ; HL = StrDsc, Stack = TxtPtr, RtnAdr
    ld      a,(EXT_FLAGS)
    and     GFXM_MASK
    sub     2                     ; 0 = 1bpp, 1 = 4bpp
    jp      c,IMERR               ; If GfxMode < 2, Invalid mode error
    ld      iy,file_save_bitmap
    jr      _aux_call_bdf
_load_bitmap:
    ld      iy,file_load_bitmap
_load__map:
    rst     CHRGET                ; Skip BIT
    rst     SYNCHR                ; Require MAP
    byte    MAPTK
    jr      _strdesc_auxcall



;-----------------------------------------------------------------------------
; Load Text Screen Color Matrix
; LOAD COLOR filename$
; Load/Save 1bpp Bitmap Color Matrix
; LOAD COLORMAP filename$
; SAVE COLORMAP filename$
;-----------------------------------------------------------------------------
;; ToDo: Implement file_save_colormap, SAVE COLORMAP
_save_colormap:
    jp       GSERR
    ld      iy,file_save_colormap
    jr      _do_colormap
_load_colormap:
    ld      iy,file_load_colormap
_do_colormap:
    rst     CHRGET
    rst     SYNCHR
    byte    ORTK                  ; Require OR
    rst     SYNCHR
    byte    MAPTK                 ; Require MAP
    jr      nz,_strdesc_auxcall
    jr      _strdesc_auxcall


;-----------------------------------------------------------------------------
; .SCR format: 2048 byte Screen+Color RAM ($3000-$3FFF)
; .SCRN format
;  40 column: 1024 byte Screen RAM + 1024 byte Color RAM {+ 32 byte palette} {+ 1 byte border flag}
;  80 column: 2048 byte Screen RAM + 2048 byte Color RAM {+ 32 byte palette} {+ 1 byte border flag}
;-----------------------------------------------------------------------------
;; ToDo: Add SAVE/LOAD SCREEN CHR/ATTR
; SCREEN 1:SAVE SCREEN "/t/test41.scrn"
; SCREEN 2:SAVE SCREEN "/t/test42.scrn"
; SCREEN 3:SAVE SCREEN "/t/test80.scrn"
; SCREEN 1:LOAD SCREEN "/t/test41.scrn":PAUSE
; SCREEN 2:LOAD SCREEN "/t/test42.scrn":PAUSE
; SCREEN 3:LOAD SCREEN "/t/test80.scrn":PAUSE
; SCREEN 2:LOAD SCREEN "/t/testrm.scrn":PAUSE
; SCREEN 2:LOAD SCREEN "/t/testpl.scrn":PAUSE
; SET FILE ERROR OFF:SCREEN 1:LOAD SCREEN "/t/test42.scrn":PRINT ERR
; SET FILE ERROR OFF:LOAD SCREEN "/t/array.caq":PRINT ERR
_save_screen:
    ld      iy,file_save_screen
    jr      skip_strdesc_auxcall
_load_screen:
    rst     CHRGET                ; Skip SCREEN
    call    screen_suffix
    dec     b                     ; CHR
    jp      z,GSERR               
    ld      iy,file_load_color
    dec     b                     ; ATTR
    jr      z,_strdesc_auxcall
    ld      iy,file_load_screen   
    byte    $3E                   ; LD A, over CHRGET
skip_strdesc_auxcall:
    rst     CHRGET                ; Skip SCREEN
_strdesc_auxcall:
    call    get_strdesc_arg       ; HL = FilDsc; Stack = TxtPtr
_aux_call_bdf:
    call    aux_call
    call    c,_badfile
    jp      _pop_hl_doserror

;-----------------------------------------------------------------------------
; RUN command - hook 24
;-----------------------------------------------------------------------------
run_cmd:
    push    af
    ld      a,KB_ENABLE | KB_ASCII
    call    key_set_keymode       ; Turn off key repeat
    call    clear_all_errvars
    pop     af

    jr      z,run_c               ; If no argument then RUN from 1st line
    jr      c,con_run             ; If digit
    cp      '_'                   ; or label
    jr      z,con_run             ;   RUN line#

    ld      (SUBFLG),a            ; Stash FileSpec start character
    call    get_string_direct     ; HL = StrDsc, DE = TxtAdr, BC = StrLen; Stack = TxtPtr
    call    run_args              ; HL = StrDsc; A, BC, DE clobbered
    jr      run_file              ; Load and run file

con_run:
    jp      CONRUN

run_c:
    jp      RUNC                  ; GOTO line number


;-----------------------------------------------------------------------------
; Run file
;-----------------------------------------------------------------------------
;; ToDo: Enable and debug lookup file

run_file:
    ; Close any open files
    call    esp_close_all

;    ld      a,(SUBFLG)            ; Restore FileSpec start character
;    cp      '"'
;    jr      z,.quoted             ; If not quoted
;    call    in_direct             ; and in direct mode
;    call    nc,_lookup_file       ;   Find matching file if no extensions
.quoted
    call    string_addr_len       ; DE = StrAdr, BC = StrLen

;For (16k) cartridge binaries
;the bytes at 0x2003...05...07...09...0B...0D...0F are consistent.
;2A, 9C, B0, 6C, 64, A8, 70
    ; Check for .ROM extension
    ld      a, c                  ; A = String Length
    cp      a, 5                  ; If less than 5
    jr      c, .load_basic        ; Too short to have ROM extension
    sub     a, 4                  ; Position of last four characters of String
    ld      c, a
    push    hl                    ; Save String Descriptor
    ex      de,hl                 ; HL = String Address
    add     hl,bc                 ; Point tho last four characters
    ex      de,hl                 ; DE = String Address
    ld      hl,.romext            ; HL = ".ROM"
    ld      b,4                   ; Comparing 4 bytes
    push    de
    call    string_cmp_upper      ; Compare Them
    pop     de
    pop     hl
    jr      z,.load_rom
    push    hl
    dec     de
    ld      hl,.coreext
    ld      b,5
    call    string_cmp_upper      ; Compare Them
    pop     hl
    jr      z,.load_core

.load_basic:
    pop     bc                    ; Discard Text Pointer
    ld      bc,run_c
    push    bc                    ; Return to RUNC
    call    load_basic_program


; RUN /roms/bio.rom
; RUN /roms/astro.rom
; RUN /games/templeman/bcii/bc2-se.rom
.load_rom:
    ld      iy,file_load_rom
    call    aux_call
    jp      m,_dos_error
    ld      iy,file_load_boot
    call    aux_call
    jp      m,_pop_hl_doserror    ; A will be 0
    xor     a
    push    af
    ld      a,BOOT_BUFR
    ex      af,af'
    ld      a,TMP_BUFFR
    pop     hl                    ; HL = ColdBt
    ld      iy,$C003
    jp      exec_page

.load_core:
    call    string_addr_len
    call    esp_load_fpga
    jp      _dos_error

.coreext db ".CORE",0
.romext: db ".ROM",0

.cartsig
    byte    $2A, $9C, $B0, $6C, $64, $A8, $70

;Keep 
.aqxsig
    byte    "AQPLUSEXEC"
.basromsig
    byte    $82,$06,$22,11,0

_lookup_file:
    push    hl                    ; Stack = StrDsc, RtnAdr
    ld      iy,file_get_ext
    call    aux_call              ; A = ExtLen
    pop     hl                    ; HL = StrDsc; Stack = RtnAdr
    ret     nz                    ; Return if extension specified
; Add wildcard
    call    string_addr_len       ; DE = StrAdr, BC = StrLen
    push    bc                    ; Stack = StrLen, RtnAdr
    push    bc                    ; Stack = StrLen, StrLen, RtnAdr
    call    get_strbuf_addr       ; HL = StrBuf
    pop     bc                    ; BC = StrLen; Stack = StrLen, RtnAdr
    push    hl                    ; Stack = StrBuf, StrLen, RtnAdr
    ex      de,hl                 ; DE = StrBuf, HL = StrAdr
    ldir                          ; DE = StrEnd
    pop     hl                    ; HL = StrBuf; Stack = StrLen, RtnAdr
    pop     bc                    ; BC = StrLen; Stack = RtnAdr
    ld      a,'.'
    call    .add_char
    ld      a,'*'                 ; Add .* to filename
    call    .add_char
    ex      de,hl                 ; DE = StrBuf
    ld      iy,dos_open_dir
    call    aux_call
    jp      m,_dos_error
    call    get_strbuf_addr       ; HL = BufAdr
.read_loop:
    push    af                    ; Stack = FilDsc, RtnAdr
    ld      iy,dos_read_dir
    call    aux_call              ; A = Result, B = NamLen, C = EntLen, DE = NamAdr, HL = BufAdr
    jp      c,LSERR               ; Error if overflow
    jp      m,.error

.ret_filename
    pop     af                    ; A = FilDsc
    ld      iy,dos_close_all
    call    aux_call              ; Close file
    ld      a,b                   ; A = NamLen, DE = NamAdr
    jp      STRAD2                ; Build string descriptor and return

.error
    cp      ERR_EOF               ; If not EOF
    jp      nz,_dos_error         ;   Error out
    pop     af                    ; A = FilDsc
    ld      iy,dos_close
    call    aux_call              ; Close file
    pop     hl                    ; HL = FilDsc; Stack = RtnAdr
    ret

.add_char:
    inc     c                     ; Bump StrLen for *
    jp      z,LSERR               ; Error if > 255
    ld      (de),a                ; Append * to filename
    inc     de                    ; Bump BUFPTR
    ret

;-----------------------------------------------------------------------------
; APPEND
;
; ToDo: APPEND "filename"{,ASC|CAQ|TOK)       Append paged binary data
; APPEND "filename",addr,len                  Append binary data
; ToDo: APPEND "filename",@page,addr,len      Append paged binary data
; ToDo: APPEND "filename",!extaddr,addr,len   Append paged binary data
; ToDo: APPEND "filename",*a                  Append numeric array a
; APPEND "filename",^a$                       Append string array a
;-----------------------------------------------------------------------------
ST_APPEND:
    call    chrget_strdesc_arg    ; Get FileSpec pointer in HL
    ex      (sp),hl               ; HL = TxtPtr, Stack = StrDsc, RtnAdr
    call    get_comma
    cp      EXPTK                 ; If ^
    jp      z,append_string       ;   Append to binary file
    jp      SNERR

;-----------------------------------------------------------------------------
; SAVE
;
; SAVE "filename"                 Save BASIC program
; SAVE "filename",ASC             Save BASIC program as ASCII text
; SAVE "filename",addr,len        Save binary data
; SAVE "filename",@page,addr,len  Save paged binary data
; SAVE "filename",!extaddr,len  Save paged binary data
; SAVE "filename",*a              Save numeric array a
; SAVE "filename",^a$             Save string
;-----------------------------------------------------------------------------
ST_SAVE:
    cp      FNTK
    jp      z,.save_fnkeys
    cp      SCRNTK
    jp      z,_save_screen
    cp      TILETK
    jp      z,_save_tile
    cp      BITTK
    jp      z,_save_bitmap
    cp      XTOKEN
    jp      z,.save_extended

    call    get_strdesc_arg       ; Get FileSpec pointer in HL
    ex      (sp),hl               ; HL = TxtPtr, Stack = StrDsc, RtnAdr

    ; Check for second parameter
    call    CHRGT2
    cp      ','
    jp      nz,save_basic_program
    rst     CHRGET
    cp      EXPTK                 ; If ^
    jp      z,save_string             ;   Load to string variable
    cp      MULTK                 ; If *
    jr      z,.array              ;   Load to array
    cp      ASCTK
    jp      z,save_ascii_program
    cp      BINTK
    jp      z,.save_bin_program
    cp      XTOKEN
    jp      z,.save_xtoken
    ; Save binary data
    cp      '!'
    jr      nz,.not_ext           ; If !
    call    get_ext_addr          ;   AF = PgFlg, DE = Addr 
    push    af                    ;   Stack = PgFlg, StrDsc, RtnAdr
    push    de                    ;   Stack = BinAdr, PgFlg, StrDsc, RtnAdr
    jr      .get_len              ; Else
.not_ext
    call    get_page_arg          ;   Check for page specifier
    push    af                    ;   Stack = Page, StrDsc, RtnAdr
    jr      nc,.bin_params

    ld      a,(hl)                ;   If @page only
    or      a
    jr      nz,.page_params
    ld      de,0                  ;     DE = BinAdr
    ld      bc,$4000              ;     BC = BinLen
    jp      _pop_save_bin         ;     Save the page
.page_params
    call    get_comma             ;   MO error if no comma
.bin_params
    call    GETINT                ;   DE = BinAdr
    push    de                    ;   Stack = BinAdr, Page, StrDsc, RtnAdr
.get_len
    call    get_comma             ; MO error if no comma
    ; Get second parameter: length
    call    GETINT                ; DE = length
    ld      b,d
    ld      c,e                   ; BC = BinLen
    pop     de                    ; DE = BinAdr; Stack
    jr      _pop_save_bin      

.save_extended
    rst     CHRGET                ; Skip XTOKEN
    cp      PALETK
    jp      z,_save_palette       ; If not PALETTE
    rst     SYNCHR
    byte    CHRTK                 ;   Require CHRSET
    rst     SYNCHR
    byte    SETTK
    jp      _save_chrset

.save_fnkeys
    call    _set_up_fnkeys        ; A = BAS_BUFFR, BC = 512, DE = FKEYDEFS, HL = FilDsc
    ld      iy,file_save_paged
    jp      _aux_call_hl_error

    ; Save array
.array:
    call    skip_star_array
    jp      save_caq_array


;-----------------------------------------------------------------------------
; Save basic program in tokenized format
;-----------------------------------------------------------------------------
; 10 SAVE "/t/savetok.bas",TOK
.save_bin_program
    rst     CHRGET                ; Skip BIN
    ex      (sp),hl               ; HL = NamDsc; Stack = TxtPtr, RtnAdr
    push    hl                    ; Stack = NamDsc, TxtPtr, RtnAdr
    ld      de,(TXTTAB)
    dec     de                    ; DE = SavAdr (0 before BASIC program)
    ld      hl,(VARTAB)
    xor     a                     ; Clear carry
    sbc     hl,de                 ; HL = SavLen
    ld      b,h
    ld      c,l                   ; BC = StrLen
    pop     hl                    ; HL = NamDsc; Stack = TxtPtr, RtnAdr
    jr      _save_bin             ; Save it

; 10 SAVE "/t/savecaq.baq",CAQ
.save_xtoken
    rst     CHRGET                ; Skip XTOKEN
    rst     SYNCHR
    byte    CAQTK                 ; Require CAQ
    jp      save_caq_program

; SAVE "t/paged.bin",@63,0,16384
; SAVE "t/paged.bin",@63
; SAVE "t/paged.bin",@19
_save_paged
    call    check_paged_address   ; Verify pages address is between 0 and 16383
    ld      iy,file_save_paged
    call    aux_call
    jp      m,_dos_error
    jp      z,IQERR
    jp      c,OVERR
    pop     hl
    ret

; S$="String 1":SAVE "/t/append.bin",^S$
; A$="String 2":APPEND "/t/append.bin",^A$
append_string:
    call    skip_get_stringvar    ; DE = VarAdr
    ld      iy,file_append_binary
    jr      _file_string

; S$="Test string"
; SAVE "/t/stringtest.str",^S$
save_string:
    call    skip_get_stringvar    ; DE = VarAdr
    ld      iy,file_save_binary
_file_string:
    ex      (sp),hl               ; HL = NamDsc; Stack = TxtPtr, RtnAdr
    push    hl                    ; Stack = NamDsc, TxtPtr, RtnAdr
    ex      de,hl                 ; HL = VarAdr
    call    string_addr_len       ; BC = StrLen, DE = StrAdr
    pop     hl                    ; HL = NamDsc; Stack = TxtPtr, RtnAdr
    jr      _save_append_bin      ; Save it

_pop_save_bin:
    pop     af                    ; AF = Page, Stack = StrDsc, RtnAdr
_save_bin:
    ld      iy,file_save_binary
    ex      (sp),hl               ; HL = StrDsc, Stack = TxtPtr
    jr      c,_save_paged
_save_append_bin:
    call    aux_call
    jp      _pop_hl_doserror

;-----------------------------------------------------------------------------
; Save basic program in ASCII format
;-----------------------------------------------------------------------------
; 10 SAVE "/t/saveasc.bas",ASC
save_ascii_program:
    rst     CHRGET                ; Skip ASC
_save_ascii:
    ex      (sp),hl               ; HL = StrDsc; Stack = TxtPtr, RtnAdr
    call    _open_write           ; Create file
    call    get_strbuf_addr       ; HL = StrBuf
    ld      (BUFADR),hl           ;
    ld      hl,(TXTTAB)           ; HL = LinPtr
.loop
    ld      c,(hl)
    inc     hl
    ld      b,(hl)
    inc     hl
    ld      a,b
    or      c
    jr      z,.done               ; If LinLnk <> 0
    push    bc                    ;   Stack = LinLnk, TxtPtr, RtnAdr
    ld      de,(BUFADR)
    call    set_outdo_buffer      ; Force output to buffer
    ld      e,(hl)
    inc     hl
    ld      d,(hl)                ;   DE = LinNum
    inc     hl
    push    hl                    ;   Stack = LinPtr, LinLnk, TxtPtr, RtnAdr
    ex      de,hl                 ;   HL = LinNum
    call    LINPRT                ;   Print line# to buffer
    ld      a,' '
    rst     OUTCHR                ;   Print space to buffer
    ld      de,(BUFPTR)           ;   DE = BufPtr (byte after space)
    pop     hl                    ;   HL = LinPtr; Stack = LinLnk, TxtPtr, RtnAdr
    call    unpack_line           ;   Unpack code into buffer
    ld      hl,(BUFPTR)
    ld      (hl),$0D
    inc     hl
    ld      (hl),$0A
    inc     hl
    ld      de,(BUFADR)           ;   DE = BufAdr
    sbc     hl,de                 ;   HL = LinLen
    ld      b,h
    ld      c,l                   ;   BC = LinLen
    call    esp_write_bytes       ;   Write line to file
    pop     hl                    ;   HL = LinLnk; Stack = TxtPtr, RtnAdr
    jp      .loop
.done
    call    esp_close_all
    pop     hl
    ret

;-----------------------------------------------------------------------------
; Save basic program
;-----------------------------------------------------------------------------
save_basic_program:
    ld      a,(BASYSCTL)
    and     BASSAVASC             ; If SET SAVE ASC ON
    jr      nz,_save_ascii        ;   SAVE as ASCII
save_caq_program:
    ex      (sp),hl               ; HL = String Descriptor, Stack = Text Pointer
    call    _open_write           ; Create file

    ; Write CAQ header
    ld      de, sync_bytes        ; Sync bytes
    ld      bc, 13
    call    esp_write_bytes
    ld      de, .caq_filename     ; Filename
    ld      bc, 6
    call    esp_write_bytes
    ld      de, sync_bytes        ; Sync bytes
    ld      bc, 13
    call    esp_write_bytes

    ; Write BASIC data
    ld      de, (TXTTAB)            ; DE = start of BASIC program
    ld      hl, (VARTAB)            ; HL = end of BASIC program
    sbc     hl, de
    ld      b,h                     ; BC = length of BASIC program
    ld      c,l
    call    esp_write_bytes


    ; Write trailer
    ld      bc,15
    ld      e,0
    call    esp_write_repbyte

    ; Close file
    call    esp_close_all

    pop     hl
    ret

.caq_filename: db "BASPRG"

;-----------------------------------------------------------------------------
; Save array
;-----------------------------------------------------------------------------
; DIM A(99)
; SAVE "/t/array.caq",*A
save_caq_array:
    ex      (sp),hl               ; HL = StrDsc, Stack = TxtPtr, RtnAdr
    push    bc                    ; Stack = AryLen, TxtPtr, RtnAdr
    push    de                    ; Stack = AryAdr, AryLen, TxtPtr, RtnAdr
    jr      z,save_string_array
    call    _open_write           ; Create file

    ; Write CAQ header
    ld      de, sync_bytes        ; Sync bytes
    ld      bc, 13
    call    esp_write_bytes
    ld      bc, 6
    ld      de, _array_filename   ; Filename
    call    esp_write_bytes

    ; Write array data
    pop     de                    ; DE = AryAdr; Stack = AryLen, TxtPtr, RtnAdr
    pop     bc                    ; BC = AryLen; Stack = TxtPtr, RtnAdr
    call    esp_write_bytes
    ; Write trailer
    ld      bc,15
    ld      e,0
    call    esp_write_repbyte

_close_pop_ret:
    call    esp_close_all
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

_array_filename: db "######"

;-----------------------------------------------------------------------------
; Save string array
;-----------------------------------------------------------------------------
; DIM A$(9)
; SAVE "/t/sa.sta",*A$
save_string_array:
    call    _open_write           ; Create file
    pop     hl                    ; HL = AryPtr; Stack = AryLen, TxtPtr, RtnAdr
.strloop
    call    string_addr_len       ; DE = StrAdr, BC = StrLen
    push    hl                    ; Stack = AryPtr, AryLen, TxtPtr, RtnAdr
    ld      a,c
    call    esp_write_byte        ; Write string length
    call    esp_write_bytes       ; Write string data
    pop     hl                    ; HL = AryPtr; Stack = AryLen, TxtPtr, RtnAdr
    pop     de                    ; DE = AryLen; Stack = TxtPtr, RtnAdr
    ld      b,4
.nextloop
    inc     hl
    dec     de
    djnz    .nextloop
    ld      a,d
    or      e
    jr      z,_close_pop_ret
    push    de                    ; Stack = AryLen, TxtPtr, RtnAdr
    jr      .strloop




;-----------------------------------------------------------------------------
; Check for sync sequence (12x$FF, 1x$00)
;-----------------------------------------------------------------------------
check_sync_bytes:
    ; Read 13 bytes into FBUFFR
    ld      bc, 13
    ld      de, FBUFFR
    call    esp_read_bytes
    ld      a, c
    cp      13
    jp      nz, BDFERR

    ; Check for 12x$FF
    ld      b, 12
    ld      de, FBUFFR
.backup:
    ld      a, (de)
    cp      $FF
    jp      nz, BDFERR
    inc     de
    djnz    .backup

    ; Check for $00
    ld      a, (de)
    or      a
    jp      nz, BDFERR
    ret

;-----------------------------------------------------------------------------
; Default SAVE to ASCII mode
; SET SAVE ASC ON/OFF
;-----------------------------------------------------------------------------
ST_SET_SAVE:
    rst     CHRGET                ; Skip SAVE
    rst     SYNCHR
    byte    ASCTK                 ; Require ASC
    call    check_on_off          ; ON = $FF, OFF = 0
    and     BASSAVASC             ; Isolate to SAVE ASC control bit
    ld      b,a                   ; B = Control bit
    ld      a,(BASYSCTL)          ; Get System control bits
    and     $FF-BASSAVASC         ; Clear SAVE ASC control bit
    or      b                     ; OR new bit value in
    ld      (BASYSCTL),a          ; Write it back out
    ret


;-----------------------------------------------------------------------------
; Open File
; Syntax: OPEN var TO filename$ FOR INPUT/OUTPUT
;-----------------------------------------------------------------------------
; ToDo: Add APPEND and RANDOM

ST_OPEN:
    call    PTRGET                ; DE = VarPtr
    call    CHKNUM                ; Error if not numeric
    push    de                    ; Stack = VarPtr, RtnAdr
    rst     SYNCHR
    byte    TOTK                  ; Require TO
    call    get_strdesc_arg       ; HL = StrDsc; Stack = TxtPtr, VarPtr, RtnAdr
    ex      (sp),hl               ; HL = TxtPtr; Stack = StrDsc, VarPtr, RtnAdr
    rst     SYNCHR
    byte    FORTK
    ld      bc,.setvar
    push    bc                    ; Stack = SETVAR, TxtPtr, VarPtr, RtnAdr
    cp      INPUTK
    jr      z,_open_read
;   append
;   random
    rst     SYNCHR                ;   Require `OUTPUT`
    byte    OUTTK
    rst     SYNCHR
    byte    PUTTK
    jr      _open_write
.setvar
    inc     a                     ; A = FilChn
    call    SNGFLT                
    pop     hl                    ; HL = TxtPtr; Stack = VarPtr, RtnAdr
    ex      (sp),hl               ; HL = VarPtr; Stack = TxtPtr, RtnAdr
    call    MOVMF
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret
    

;-----------------------------------------------------------------------------
; Open File
; Syntax: OPEN(filename$ FOR INPUT)
;-----------------------------------------------------------------------------
; A=OPEN("autokeys" FOR INPUT)
; A=OPEN("autokeys" FOR OUTPUT)
FN_OPEN:
    call    esp_close_all         ; Only one file at a time, for now
    rst     CHRGET                ; Skip OPEN Token
    SYNCHK  '('                   ; `OPEN(filename$ `
    call    get_strdesc_arg       ; HL = StrDsc; Stack = TxtPtr, RtnAdr
    ex      (sp),hl               ; HL = TxtPtr; Stack = StrDsc, RtnAdr
    rst     SYNCHR                ; `FOR`
    byte    FORTK
    jp      z,SNERR               ; Syntax error if end of statement
    push    af                    ; Stack = IN/PUT, Stack = StrDsc, RtnAdr
    cp      INPUTK
    jr      nz,.notinput          ; If `INPUT`
    rst     CHRGET                ;   Skip INPUT`
    jr      .paren                ; else
.notinput
    rst     SYNCHR                ;   Require `OUTPUT`
    byte    OUTTK
    rst     SYNCHR
    byte    PUTTK
.paren
    SYNCHK  ')'                   ; `)`
    pop     af                    ; A = IN/OUT, StrDsc, RtnAdr
    ex      (sp),hl               ; HL = StrDsc; Stack = TxtPtr, RtnAdr
    ld      bc,LABBCK
    push    bc                    ; Stack = LABBCK, TxtPtr, RtnAdr
    cp      INPUTK                ; If INPUT
    jr      nz,.notread
    call    _open_read            ;   Open for Read
    jr      .done                 ; Else
.notread
    call    _open_write           ;   Open for Write
.done
    jp      m,_dos_error
    jp      SNGFLT

_open_dir:
    ld      iy,dos_open_dir
    jr      _open_aux_call
_open_write:
    ld      iy,dos_open_write
    jr      _open_aux_call
_open_read:
    ld      iy,dos_open_read
_open_aux_call:
    call    aux_call
_open_error:
    jp      m,dos_error
    ret


;-----------------------------------------------------------------------------
; Input full line
; Syntax: LINE INPUT# channel,var$
;-----------------------------------------------------------------------------
; A=OPEN("/t/test.txt" FOR INPUT)
; LINE INPUT# 0, A$
; PRINT A$

ST_LINE_INPUT:
    rst     CHRGET                ; Skip INPUT
    SYNCHK  '#'                   ; Require #
    call    GETINT                ; Parse FilDsc
    or      a                     ; If not 0
    jp      nz,FCERR              ;   Error - For now
    push    af                    ; Stack = FilDsc, RtnAdr
    SYNCHK  ','                   ; Require comma
    call    PTRGET                ; Get pointer to variable
    call    GETYPE                ; If not string
    jp      nz,TMERR              ;   Type mismatch error
    pop     af                    ; A = FilDsc; Stack = TxtPtr
    push    hl                    ; Stack = TxtPtr, RtnAdr
    push    de                    ; Stack = VarPtr, TxtPtr, RtnAdr
    ld      bc,256                ; Maximum bytes
    ld      hl,(TOPMEM)
    add     hl,bc                 ; Work buffer
    call    esp_read_line
    jp      m,_dos_error
    call    TIMSTR                ; Copy buffer into temporary string
    call    FREFAC                ; HL = TmpDsc
    ex      de,hl                 ; DE = TmpDsc
    pop     hl                    ; HL = VarPtr; Stack = TxtPtr, RtnAdr
    call    MOVE                  ; Copy descriptor to variable
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

_clear_errflag:
    ld      a,(EXT_FLAGS)
    and     FERR_FLAG
    jr      nz,.skip              ; If SET FILE ERROR OFF
    xor     a
    ld      (ERRFLG),a            ;   Set Error# to 0
.skip
    jp      CHRGT2

;-----------------------------------------------------------------------------
; Parse literal string only in Direct Mode
; Parse string expression only during RUN
;  Input: HL = Text Pointee
; Output: BC = String Length
;         DE = String Address
;         HL = String Descriptor
;         Text Pointer on Stack
;-----------------------------------------------------------------------------
get_string_direct:
    call    in_direct             ; If not direct mode
    jr      c,get_string_arg      ;   Parse string expression
    call    _clear_errflag        ; Set BASIC error to 0
    ld      a,(hl)                ; A = First character of argument
    cp      '"'                   ;
    
    jr      z,get_string_arg      ; If not a quote
    dec     hl                    ;   Back up text pointer for STRLT2
    ld      b,' '                 ;   Delimiters are space
    ld      d,':'                 ;   and colon
    call    STRLT2                ;   Build temp string from literal
    xor     a                     ;   Quoted = False
    jr      _proc_string_arg      ;   and process it

;-----------------------------------------------------------------------------
; Parse string at text pointer, return String Length and Text Address
;  Input: HL: Text Pointee
;         NZ to allow null string
; Output: BC: String Length
;         DE: String Address
;         HL: String Descriptor
;         Text Pointer on Stack
;-----------------------------------------------------------------------------
get_string_arg:
    or      $FF                   ; Quoted = True
    call    FRMEVL                ; Get Path
_proc_string_arg:
    pop     IX                    ; IX = RtnAdr
    push    hl                    ; Stack = TxtPtr
    push    af                    ; Stack = Quoted, TxtPtr
    call    FRESTR                ; HL = StrDsc
    call    string_addr_len       ; BC = StrLen, DE = StrAdr
    xor     a
    cp      c
    jp      z,FCERR
    pop     af                    ; A = Quoted; Stack = TxtPtr
    jp      (IX)                  ; Fast Return

;-----------------------------------------------------------------------------
; Parse string at text pointer, return String Descriptor
; Input: HL = Text Pointee
; Output: HL = String Descriptor
;         Text Pointer on Stack
;-----------------------------------------------------------------------------
chrget_strdesc_arg:
    rst     CHRGET
get_strdesc_arg:
    push    iy                    ; Stack = IY, RtnAdr
    call    FRMEVL                ; Get String at HL
    pop     iy                    ; Stack = RtnAdr
    pop     ix                    ; IX = RtnAdr
    push    hl                    ; Stack = TxtPtr
    call    FRESTR                ; Free Temporary String
    jp      (ix)                  ; Fast Return

_index_offset:
    ld      bc,0
    push    hl
    call    .index
    call    nz,.offset
    jp      nz,POPHRT
    pop     de                    ; Discard Old TxtPtr
    ret
.index:
    cp      INTK
    ret     nz
    rst     CHRGET
    cp      XTOKEN
    ret     nz
    rst     CHRGET
    cp      DEXTK
    ret     nz
    jr      .done
.offset:
    cp      XTOKEN
    ret     nz
    rst     CHRGET
    cp      OFFTK
    ret     nz
    rst     CHRGET
    cp      SETTK
    ret     nz
    call    tile_offset
.done
    rst     CHRGET
    xor     a
    ret

tile_offset:
    ld      a,(EXT_FLAGS)
tile_offset_a:
    and     3
    ld      bc,320
    sub     a,2
    ret     z
    ld      bc,500
    dec     a
    ret     z
    ld      bc,128                  ; Tilemap mode
    ret

;-----------------------------------------------------------------------------
; Bad file error
;-----------------------------------------------------------------------------
_badfile:
    ld      a,$FF-ERRBDF
    or      a
    ret

BDFERR:
    ld      e,ERRBDF
    jp      ERROR

;-----------------------------------------------------------------------------
; Invalid mode error
;-----------------------------------------------------------------------------
IMERR:
    ld      e,ERRIM
    jp      ERROR

