;====================================================================
; File I/O Statements and Functions
;====================================================================

;-----------------------------------------------------------------------------
; CD - Change directory
;
; No argument -> Show current directory
; With argument -> Change current directory
;-----------------------------------------------------------------------------
ST_CD:
    ; Argument given?
    or      a
    jr      nz, .change_dir     ; Yes

    push    hl
    call    _get_cd
    call    print_c_string        ; Print it
    pop     hl
    ret

    ; -- Argument given -> change directory ----------------------------------
.change_dir:
    call    get_string_direct     ; Get String Argument
    call    dos_change_dir
_check_error
    jp      m,_dos_error
    pop     hl                    ; Restore Text Pointer
    ret     z
_dos_error:
    cpl                           ; Convert -1 to 0, -1 to 2, etc
    add     a,a                   ; Multiply by 2 to get offset
    add     a,ERRFNF              ; Add to start of DOS errors
    ld      e,a
    call    esp_close_all
    jp      ERROR

_get_cd:
    ld      ix,dos_get_cwd        ; Read current directory into buffer
    call    bas_read_to_buff      ; Set buffer address and call routine
    jp      m,_dos_error
    ret
;-----------------------------------------------------------------------------
; CD$ - Get Current Directory
;-----------------------------------------------------------------------------
FN_CD:
    rst     CHRGET                ; Skip Token
    SYNCHK  '$'                   ; Require Dollar Sign
    push    hl
    ld      bc,LABBCK
    push    bc
    call    _get_cd
    jp      TIMSTR

;-----------------------------------------------------------------------------
; DEL - Delete file/directory
; Syntax: DEL filespec$
;-----------------------------------------------------------------------------
ST_DEL:
    call    get_string_direct
    call    dos_delete_file
    jr      _check_error

;-----------------------------------------------------------------------------
; MKDIR - Create directory
; Syntax: MKDIR dirname$
;-----------------------------------------------------------------------------
ST_MKDIR:
    call    get_string_direct
    call    dos_create_dir
    jr      _check_error

;-----------------------------------------------------------------------------
; RENAME - Rename a file
; Syntax: RENAME oldfile$ TO newfile$
;-----------------------------------------------------------------------------
ST_RENAME:
    call    FRMEVL                ; Parse oldname                                                     
    call    CHKSTR                ; Gotta be a string
    call    SYNCHR                ; Require TO token
    byte    TOTK                    
    push    hl                    ; Stack = textptr
    ld      hl,(FACLO)            ; HL = olddesc
    ex      (sp),hl               ; HL = textptr, HL = olddesc
    call    FRMEVL                ; Parse newname
    push    hl                    ; Stack = textptr, olddesc
    call    FRESTR                ; HL = newdesc
    ex      de,hl                 ; DE = newdesc. 
    pop     bc                    ; BC = textptr, Stack = olddesc
    pop     hl                    ; HL = olddesc
    push    bc                    ; Stack = textptr
    push    de                    ; Stack = newdesc, TxtPtr
    call    FRETM2                ; HL = olddesc
    pop     de                    ; DE = newdesc, Stack = TxtPtr
    call    dos_rename_file       ; Do the rename
    jr      _check_error          ; Restore textptr and check for error

;-----------------------------------------------------------------------------
; DIR - Directory listing
;
; No argument -> List current directory
; With argument -> List given path
;-----------------------------------------------------------------------------
ST_DIR:

    ; Preserve BASIC text pointer

    ; Argument given?
    or      a
    jr      nz, .witharg     ; Yes

    push    hl                    ; Save Text Pointer
    ld      bc,0                  ; Empty String
    jr      .esp_command

.witharg:
    call    get_string_direct        ; Get FileSpec pointer in HL

.esp_command:
    call    esp_close_all

    call    dos_open_dir
    jp      m,_dos_error

    ; Set initial number of lines per page
    ld      a, 24
    ld      (CNTOFL), a

.next_entry:
    ; Read entry
    ld      a, ESPCMD_READDIR
    call    esp_cmd
    xor     a
    call    esp_send_byte
    call    esp_get_byte

    cp      ERR_EOF
    jp      z, .done
    or      a
    jp      p, .ok2
    pop     hl              ; Restore BASIC text pointer
    jp      _dos_error

.ok2:
    ;-- Date -----------------------------------------------------------------
    call    esp_get_de

    ; Extract year
    srl     a
    add     80
    call    print_a_2digits

    ld      a, '-'
    rst     OUTCHR

    ; Extract month
    ld      a, d
    rra                     ; Lowest bit in carry
    ld      a, e
    rra
    call    srl4out

    ld      a, '-'
    rst     OUTCHR

    ; Extract day
    ld      a, e
    and     $1F
    call    print_a_2digits

    ld      a,' '
    rst     OUTCHR

    ;-- Time -----------------------------------------------------------------
    ; Get time (hhhhhmmm mmmsssss)
    call    esp_get_de

    ; Hours
    call    srl3out

    ld      a, ':'
    rst     OUTCHR

    ; Minutes
    ld      a, d
    and     $07
    ld      c, a
    ld      a, e

    ld      b,5
.srlrra
    srl     c
    rra
    djnz    .srlrra

    call    print_a_2digits

    ;-- Attributes -----------------------------------------------------------
    call    esp_get_byte
    bit     0, a
    jr      z, .no_dir

    ;-- Directory ------------------------------------------------------------
    call    print_string_immd
    byte    " <DIR>",0

    ; Skip length bytes
    call    esp_get_long

    jr      .get_filename

    ;-- Regular file: file size ----------------------------------------------
.no_dir:

    call    esp_get_long

    ; Megabytes range?
    ld      a,d           
    or      a                     
    jr      nz,.mb
    ld      a,e
    cp      $9C                   ; If >=10,240,000 show Megabytes
    jr      c,.notmb              
    ld      a,b
    cp      $40
    jr      nc, .mb

.notmb
    ; Kilobytes range?
    ld      a,e
    or      a
    jr      nz,.kb
    ld      a,b
    cp      $27                   ; If >=10,000 show Kilobytes
    jr      c,.notkb
    cp      $10
    jr      nc,.kb

.notkb
    ; Bytes range (<10,000)
    ld      h,b
    ld      l,c
    call    print_hl_4digits
    ld      a, 'B'
    rst     OUTCHR
    jr      .get_filename

.kb:
    ld      a,e
    ;and     a, $0F
    ld      h, a
    ld      a,b
    ld      l, a
    call    srlh_rrl_out2
    ld      a, 'K'
    rst     OUTCHR
    jr      .get_filename

.mb:
    ld      h,d
    ld      l,e
    call    srlh_rrl_out4
    ld      a, 'M'
    rst     OUTCHR
    jr      .get_filename

    ;-- Filename -------------------------------------------------------------
.get_filename:
    ld      a,' '
    rst     OUTCHR

.filename:
    call    esp_get_byte
    or      a
    jr      z, .name_done
    rst     OUTCHR
    jr      .filename

.name_done:
    call    CRDO
    jp      .next_entry

.done:
    ; Close directory
    ld      a, ESPCMD_CLOSEDIR
    call    esp_cmd
    xor     a
    call    esp_send_byte
    call    esp_get_byte
    or      a
    jp      m, _dos_error

    pop     hl      ; Restore BASIC text pointer
    ret

;-----------------------------------------------------------------------------
; Output 2 number digit in A
;-----------------------------------------------------------------------------

srl4out:
    srl     a
srl3out:
    srl     a
    srl     a
    srl     a


;-----------------------------------------------------------------------------
; Output 2 number digit in A
;-----------------------------------------------------------------------------
print_a_2digits:
    cp      100
    jr      c, .l0
    sub     a, 100
    jr      print_a_2digits
.l0:
    ld      c, 0
.l1:
    inc     c
    sub     a, 10
    jr      nc, .l1
    add     a, 10
    push    a

    ld      a, c
    add     '0'-1
    call    TTYCHR
    pop     a
    add     '0'
    call    TTYCHR
    ret


;-----------------------------------------------------------------------------
; Output 4 number digit in HL
;-----------------------------------------------------------------------------

;Enter with B = Number of times to rotate
srlh_rrl_out4:
    srl     h
    rr      l
    srl     h
    rr      l
srlh_rrl_out2:
    srl     h
    rr      l
    srl     h
    rr      l

print_hl_4digits:
    ld      e,'0'                
    ld	    bc,-10000
    call	  .num1
    ld	    bc,-1000
    call	  .num1
    ld	    bc,-100
    call	  .num1
    ld	    c,-10
    call	  .num1
    ld	    c,-1
.num1:	
    ld	    a,-1
.num2:	
    inc	    a
    add	    hl,bc
    jr	    c,.num2
    sbc	    hl,bc
    add     a,'0'
    cp      e
    jr      nz,.num3
    add     a,' '-'0'
    byte    $16                   ; LD D over DEC E
.num3
    dec     e
    rst     OUTCHR
    ret 

;-----------------------------------------------------------------------------
; LOAD "filename"                 Load BASIC program
; LOAD "filename",address         Load file as raw binary to address
; LOAD "filename",@page,address   Load file as raw binary to address in page
; LOAD "filename",*a              Load data into numeric array a
; LOAD CHRSET "filename"          Load character set into character RAM buffer
; LOAD PALETTE p,"filename"       Load one or all palettes
; LOAD SCREEN "filename"          Load screen with optional embedded palette
;-----------------------------------------------------------------------------
ST_LOAD:
    ; Close any open files
    call    esp_close_all

    ld      a,(hl)
    cp      SCRNTK
    jp      z,_load_screen

    cp      FNTK
    jp      z,_load_fnkeys

    cp      XTOKEN
    jp      z,_load_extended

    ; Get string parameter with path

    call    get_strdesc_arg        ; Get FileSpec pointer in HL

    ex      (sp),hl                 ; HL = Text Pointer, Stack = String Descriptor

    ; Check for second parameter
    call    CHRGT2
    cp      ','
    jr      nz, .basic              ; No parameter -> load as basic program
    rst     CHRGET
    cp      MULTK                   ; Token for '*'
    jp      z, .array               ; Array parameter -> load as array

; Load raw binary to address
; LOAD "tron.bin",$4200
; LOAD "tron.bim",$4200
    call    parse_page_arg          ; Check for page specifier
    push    af                      ; Stack = Page, String Descriptor
    call    FRMNUM                  ; Get number
    call    FRCINT                  ; Convert to 16 bit integer
    ; Get back page filespec
    pop     af                      ; AF = Page, Stack = String Descriptor
    ex      (sp),hl                 ; HL = String Descriptor, Stack = Text Pointer
    jr      c,.load_paged
    ld      bc,$FFFF                ; Load up to 64k   
    call    file_load_binary
    jp      m,_dos_error
    call    esp_close_all
    pop     hl                      ; Get Back Text Pointer
    ret
    
; Load raw binary into paged memory
; LOAD "tron.bin",@40,$1234
.load_paged
    ld      bc,$FFFF                ; Load up to 64k
    call    check_paged_address     ; Verify pages addres is between 0 and 16383
    call    file_load_paged
    jp      z,FCERR
    jp      c,OVERR
    jp      m,_dos_error
    pop     hl
    ret
    
    ; Load into array
.array:
    call    get_array_argument
    jp      load_caq_array

.basic:
    ex      (sp),hl               ; HL = String Descriptor, Stack = Text Pointer
    jp      load_basic_program

;-----------------------------------------------------------------------------
; Load CAQ/BAS file
; Input: HL: String descriptor address
; Clobbered registers: A, DE
;-----------------------------------------------------------------------------
load_basic_program:
    ; Open file
    call    dos_open_read
    jp      m,_dos_error

    ; Check CAQ header
    call    check_sync_bytes    ; Sync bytes
    ld      bc, 6               ; Read filename
    ld      de, FILNAM
    call    esp_read_bytes
    call    check_sync_bytes    ; Sync bytes

    ; Load actual program
    ld      de, (TXTTAB)
    ld      bc, $FFFF
    call    esp_read_bytes

    ; Close file
    call    esp_close_all

    ; Back up to last line of BASIC program
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

;-----------------------------------------------------------------------------
; Load CAQ array file in File into BINSTART (BINLEN length)
; Input: HL: String descriptor address
; Clobbered registers: A, DE
;-----------------------------------------------------------------------------
load_caq_array:
    ex      (sp),hl               ; HL = String Descriptor, Stack = Text Pointer

    ; Open file
    call    dos_open_read
    jp      m,_dos_error
    
    ; Check CAQ header
    call    check_sync_bytes    ; Sync bytes
    ld      bc, 6               ; Check that filename is '######'
    ld      de, FILNAM
    call    esp_read_bytes
    ld      b, 6
    ld      de,FILNAM
.backup:
    ld      a, (de)
    cp      '#'
    jp      nz, BDFERR
    inc     de
    djnz    .backup

    ; Load data into array
    ld      de, (BINSTART)
    ld      bc, (BINLEN)
    call    esp_read_bytes

    ; Close file
    call    esp_close_all

    pop     hl
    ret

;-----------------------------------------------------------------------------
; Get array argument
;-----------------------------------------------------------------------------
get_array_argument:
    ; Skip '*' token
    inc     hl

    ; Get pointer to array variable
    ld      a, 1
    ld      (SUBFLG), a         ; Set array flag
    call    PTRGET              ; Get array (out: BC = pointer to number of dimensions, DE = next array entry)
    ld      (SUBFLG), a         ; Clear array flag
    jp      nz,UDERR            ; FC Error if array not found
    call    CHKNUM              ; TM error if not numeric

    ; Get start address and length of array
    push    hl                  ; Push BASIC text pointer
    ld      h, b
    ld      l, c                ; HL = address
    ld      c, (hl)
    ld      b, 0                ; BC = index
    add     hl, bc
    add     hl, bc
    inc     hl                  ; HL = array data
    ld      (BINSTART), hl
    dec     de
    dec     de                  ; Subtract array header to get data length
    dec     de
    ld      (BINLEN), de
    pop     hl                  ; Pop text pointer
    ret

_load_fnkeys:
    call    _set_up_fnkeys
    call    file_load_paged
    jp      m,_dos_error
    pop     hl
    ret

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
    ld      (BINSTART),de
    ld      (BINLEN),de
    ret

;;;ToDo: Add LOAD PALETTE
_load_extended:
    rst     CHRGET                ; Skip XTOKEN
    cp      PALETK                ; 
    jr      z,_load_palette
    rst     SYNCHR
    byte    CHRTK                 ;   Must be CHRSET
    rst     SYNCHR
    byte    SETTK
; load chrset "demos/charmaps/charmaps/bold.chr
; load chrset "future.chr
_load_chrset:
    call    get_strdesc_arg       ; HL = FileSpec StrDsc; Stack = TxtPtr
    call    file_load_chrset      ; Load character set and copy to character RAM
    jp      m,_dos_error
    pop     hl
    ret

_load_palette:
    jp      GSERR


;-----------------------------------------------------------------------------
; .SCR format: 2048 byte Screen+Color RAM ($3000-$3FFF)
; .SCP format
;  40 column: 2048 byte Screen+Color RAM + 32 byte palette + 1 byte border flag
;  80 column: 2048 byte Screen + 2048 byte Color + 32 byte palette + 1 byte border flag
;-----------------------------------------------------------------------------
_load_screen:
    rst     CHRGET                ; Skip SCREEN
    call    get_strdesc_arg       ; HL = FileSpec StrDsc; Stack = TxtPtr
    call    file_load_screen      ; Load character set and copy to character RAM
    jp      m,_dos_error
    pop     hl
    ret

;-----------------------------------------------------------------------------
; RUN command - hook 24
;-----------------------------------------------------------------------------
run_cmd:
    push    af
    ld      a,KB_ENABLE | KB_ASCII
    call    key_set_keymode       ; Turn off key repeat
    pop     af

    jp      z, RUNC            ; If no argument then RUN from 1st line

    cp      '_'                ; If it's a label
    jp      z,CONRUN           ;   Do RUN line#
    
    push    hl
    call    FRMEVL             ; Get argument type
    ld      a, (VALTYP)
    dec     a                  ; 0 = string
    jr      nz,.not_file
    call    FRESTR
    pop     hl
    jr      run_file

.not_file:
    pop     hl

    ; RUN with line number
    call    CLEARC             ; Init BASIC run environment
    ld      bc, NEWSTT
    jp      RUNC2              ; GOTO line number

;-----------------------------------------------------------------------------
; Run file
;-----------------------------------------------------------------------------
run_file:
    ; Close any open files
    call    esp_close_all

    call    get_strdesc_arg       ; Get FileSpec
    push    hl                    ; Save String Descriptor

    ; Check for .ROM extension
    call    string_addr_len       ; Get String Length in BC, Address in DE
    ld      a, c                  ; A = String Length
    cp      a, 5                  ; If less thsn 5
    jr      c, .load_basic        ; Too short to have ROM extension
    sub     a, 4                  ; Position of last four characters of String
    ld      c, a
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
    call    clear_all_errvars     ; Clear ON ERROR sytem variables
    pop     bc                    ; Discard Text Pointer
    ld      bc,RUNC
    push    bc                    ; Return to RUNC
    call    load_basic_program

.load_rom:
    call    dos_load_rom
    jp      _dos_error

.load_core:
    call    string_addr_len
    call    esp_load_fpga
    jp      _dos_error

.coreext db ".CORE",0
.romext: db ".ROM",0

;-----------------------------------------------------------------------------
; SAVE
;
; SAVE "filename"                 Save BASIC program
; SAVE "filename",addr,len        Save binary data
; SAVE "filename",@page,addr,len  Save paged binary data
; SAVE "filename",*a              Save numeric array a
;-----------------------------------------------------------------------------
ST_SAVE:
    ; Close any open files
    call    esp_close_all

    ld      a,(hl)
    cp      FNTK
    jp      z,.save_fnkeys

    call    get_strdesc_arg         ; Get FileSpec pointer in HL
    ex      (sp),hl                 ; HL = Text Pointer, Stack = String Descriptor

    ; Check for second parameter
    call    CHRGT2
    cp      ','
    jp      nz, save_basic_program
    rst     CHRGET
    cp      $AA                     ; Token for '*'
    jp      z, .array               ; Array parameter -> save array

    ; Save binary data

    ; Get first parameter: address
    call    parse_page_arg          ; Check for page specifier
    push    af                      ; Stack = Page, String Descriptor
    call    FRMNUM                  ; Get number
    call    FRCINT                  ; Convert to 16 bit integer
    ld      (BINSTART), de

    ; Expect comma
    call    CHRGT2
    cp      ','
    jp      nz, MOERR
    inc     hl

    ; Get second parameter: length
    call    FRMNUM                  ; Get number
    call    FRCINT                  ; Convert to 16 bit integer
    ld      (BINLEN), de

     ; Get back page filespec
    pop     af                      ; AF = Page, Stack = String Descriptor
    ex      (sp),hl                 ; HL = String Descriptor, Stack = Text Pointer

    ; Do the save
    jr      c,.save_paged
    call    dos_save_binary
    jp      m,_dos_error
    pop     hl
    ret

.save_fnkeys
    call    _set_up_fnkeys        ; A = BAS_BUFFR, BC = 512, DE = FKEYDEFS, HL = FilDsc

.save_paged
    call    check_paged_address   ; Verify pages addres is between 0 and 16383
    call    dos_save_paged
    jp      m,_dos_error
    jp      z,IQERR
    jp      c,OVERR
    pop     hl
    ret

    ; Save array
.array:
    call    get_array_argument
    jp      save_caq_array

;-----------------------------------------------------------------------------
; Save basic program
;-----------------------------------------------------------------------------
save_basic_program:
    ex      (sp),hl               ; HL = String Descriptor, Stack = Text Pointer
    call    dos_open_write        ; Create file
    jp      m,_dos_error

    ; Write CAQ header
    ld      de, sync_bytes      ; Sync bytes
    ld      bc, 13
    call    esp_write_bytes
    ld      de, .caq_filename   ; Filename
    ld      bc, 6
    call    esp_write_bytes
    ld      de, sync_bytes      ; Sync bytes
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
save_caq_array:
    ex      (sp),hl               ; HL = String Descriptor, Stack = Text Pointer
    call    dos_open_write       ; Create file
    jp      m,_dos_error

    ; Write CAQ header
    ld      de, sync_bytes      ; Sync bytes
    ld      bc, 13
    call    esp_write_bytes
    ld      de, .array_filename ; Filename
    ld      bc, 6
    call    esp_write_bytes

    ; Write array data
    ld      de, (BINSTART)
    ld      bc, (BINLEN)
    call    esp_write_bytes

    ; Write trailer
    ld      bc,15
    ld      e,0
    call    esp_write_repbyte

    ; Close file
    call    esp_close_all

    pop     hl
    ret

.array_filename: db "######"

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
; Parse literal string only in Direct Mode
; Parse string expression only during RUN
;-----------------------------------------------------------------------------
get_string_direct:
    call    in_direct             ; If not direct mode
    jr      c,get_string_arg      ;   Parse string expression
    ld      a,(hl)                ; A = First character of argument
    cp      '"'                   ; 
    jr      z,get_string_arg      ; If not a quote
    dec     hl                    ;   Back up text pointer for STRLT2
    ld      b,' '                 ;   Delimters are space
    ld      d,':'                 ;   and colon
    call    STRLT2                ;   Build temp string from literal
    jr      _proc_string_arg      ;   and process it

;-----------------------------------------------------------------------------
; Parse string at text pointer, return String Length and Text Address
;         HL = Text Pointee
; Output: BC = String Length
;         DE = String Address
;         HL = String Descriptor
;         Text Pointer on Stack
;-----------------------------------------------------------------------------
get_string_arg:
    call    FRMEVL                ; Get Path
_proc_string_arg:
    pop     IX                    ; IX = Return Address
    push    hl                    ; Text Pointer on stack
    call    FRESTR                ; HL = StrDsc
    call    string_addr_len       ; BC = StrLen, DE = StrAdr
    xor     a
    cp      c
    jp      z,FCERR
    jp      (IX)                  ; Fast Return

;-----------------------------------------------------------------------------
; Parse string at text pointer, return String Descriptor
; Input: HL = Text Pointee
; Output: HL = String Descriptor
;         Text Pointer on Stack
;-----------------------------------------------------------------------------

get_strdesc_arg:
    call    FRMEVL                ; Get String at HL
    pop     IX                    ; IX = Return Address
    push    hl                    ; Text Pointer on stack
    call    FRESTR                ; Free Temporary String
    jp      (IX)                  ; Fast Return

;-----------------------------------------------------------------------------
; Bad file error
;-----------------------------------------------------------------------------
BDFERR:
    ld      e,ERRBDF
    jp      ERROR