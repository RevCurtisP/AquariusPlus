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

    ; Push BASIC text pointer
    push    hl

    ; -- No argument -> show current path ------------------------------------
.show_path:
    ld      a, ESPCMD_GETCWD
    call    esp_cmd
    call    esp_get_result

    ; Print current working directory
.print_cwd:
    call    esp_get_byte
    or      a
    jr      z, .print_done
    rst     OUTCHR
    jr      .print_cwd
.print_done:
    call    CRDO

.done:
    ; Restore BASIC text pointer
    pop     hl
    ret

    ; -- Argument given -> change directory ----------------------------------
.change_dir:
    call    get_string_arg       ; Get String Ar
    call    dos_change_dir

_done
    pop     hl                    ; Restore Text Pointrt
    ret     z
file_error:
    jp      esp_error

;-----------------------------------------------------------------------------
; CD$ - Get Current Directory
;-----------------------------------------------------------------------------
FN_CD:
    rst     CHRGET                ; Skip Token
    SYNCHK  '$'                   ; Require Dollar Sign
    push    hl
    ld      bc,LABBCK
    push    bc
    ld      ix,dos_get_cwd        ; Read current directory into buffer
    call    bas_read_to_buff      ; Set buffer address and call routine
    jp      TIMSTR

;-----------------------------------------------------------------------------
; DEL - Delete file/directory
;-----------------------------------------------------------------------------
ST_DEL:
    call    get_string_arg        
    call    dos_delete_file
    jr      _done                  

;-----------------------------------------------------------------------------
; MKDIR - Create directory
;-----------------------------------------------------------------------------
ST_MKDIR:
    call    get_string_arg        
    call    dos_create_dir
    jr      _done                  

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
    ld      hl,0
    jr      .esp_command

.witharg:
    call    get_strdesc_arg        ; Get FileSpec pointer in HL

.esp_command:
    call    esp_close_all

    ld      a, ESPCMD_OPENDIR     ; Set ESP Command
    call    esp_cmd_strdesc       ; Get FileSpec and Do Command

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
    jp      esp_error

.ok2:
    ;-- Date -----------------------------------------------------------------
    call    esp_get_de

    ; Extract year
    srl     a
    add     80
    call    out_number_2digits

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
    call    out_number_2digits

    call    print_space

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

    call    out_number_2digits

    ;-- Attributes -----------------------------------------------------------
    call    esp_get_byte
    bit     0, a
    jr      z, .no_dir

    ;-- Directory ------------------------------------------------------------
    call    STRPRI
    byte    " <DIR>",0

    ; Skip length bytes
    call    esp_get_long

    jr      .get_filename

    ;-- Regular file: file size ----------------------------------------------
.no_dir:
    ; aaaaaaaa bbbbbbbb cccccccc dddddddd

     call    esp_get_long 

    ; Megabytes range?
    or      a
    jr      nz, .mb
    ld      a, e
    and     $F0
    jr      nz, .mb

    ; Kilobytes range?
    ld      a, e
    or      a
    jr      nz, .kb
    ld      a, b
    and     $FC
    jr      nz, .kb

    ; Bytes range (aaaaaaaa bbbbbbbb ccccccCC DDDDDDDD)
.bytes:
    ld      h,b
    ld      l,c
    call    out_number_4digits
    ld      a, 'B'
    rst     OUTCHR
    jr      .get_filename

    ; Kilobytes range: aaaaaaaa bbbbBBBB CCCCCCcc dddddddd
.kb:
    ld      a,e
    and     a, $0F
    ld      h, a
    ld      a,b
    ld      l, a
    ld      b, 2
    call    srlh_rrl_out
    ld      a, 'K'
    rst     OUTCHR
    jr      .get_filename

    ; Megabytes range: AAAAAAAA BBBBbbbb cccccccc dddddddd
.mb:
    ld      h,d
    ld      h,e
    ld      b, 4
    call    srlh_rrl_out
    ld      a, 'M'
    rst     OUTCHR
    jr      .get_filename

    ;-- Filename -------------------------------------------------------------
.get_filename:
    call    print_space

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
    jp      m, esp_error

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

;;; Previously 31 bytes, now 28 bytes
out_number_2digits:
    push    de
.loop
    cp      100
    jp      c,.check10s
    sub     a,100
    jr      .loop
.check10s
    cp      10
    jr      nc,.print
    push    a
    ld      a,'0'
    rst     OUTCHR
    pop     a
.print
    call    BYTPRT
    pop     de
    ret

print_space:
    ld      a,' '
    jp      OUTDO

;-----------------------------------------------------------------------------
; Output 4 number digit in HL
;-----------------------------------------------------------------------------

;Enter with B = Number of times to rotate
srlh_rrl_out:
    srl     h
    rr      l
    djnz    srlh_rrl_out

;;; Previously 58 bytes, now 31 bytes
out_number_4digits:
    ld      de,10000
    rst     COMPAR
    call    c,print_space
    ld      de,1000
    rst     COMPAR
    call    c,print_space
    ld      de,100
    rst     COMPAR
    call    c,print_space
    ld      de,10
    rst     COMPAR
    call    c,print_space
    jp      LINPRT

;-----------------------------------------------------------------------------
; LOAD
;
; LOAD "filename"        Load BASIC program
; LOAD "filename",12345  Load file as raw binary to address 12345
; LOAD "filename",*a     Load data into numeric array a
;-----------------------------------------------------------------------------
ST_LOAD:
    ; Close any open files
    call    esp_close_all

    ; Get string parameter with path
    call    get_strdesc_arg        ; Get FileSpec pointer in HL
    ex      (sp),hl               ; HL = Text Pointer, Stack = String Descriptor

    ; Check for second parameter
    call    CHRGT2
    cp      ','
    jr      nz, .basic              ; No parameter -> load as basic program
    rst     CHRGET
    cp      $AA                     ; Token for '*'
    jr      z, .array               ; Array parameter -> load as array

    ; Load as binary to address
    call    parse_page_arg          ; Check for page specifier
    push    af                      ; Save it
    call    FRMNUM                  ; Get number
    call    FRCINT                  ; Convert to 16 bit integer
    ld      (BINSTART), de
    pop     af                      ; Get back page
    jp      nz,page_load_binary 
    jp      load_binary

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
    call    esp_open

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
.loop:
    dec     hl
    xor     a
    cp      (hl)
    jr      z, .loop
    inc     hl

    ; Skip past 3 zeros at end of BASIC program
    inc     hl
    inc     hl
    inc     hl

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

    ; Clear array pointer???
    ld      (VARNAM), hl


    ; Fix up next line addresses in loaded BASIC program
.link_lines:
    ld      de, (TXTTAB)        ; DE = start of BASIC program
    jp      CHEAD

;-----------------------------------------------------------------------------
; Load CAQ array file in File into BINSTART (BINLEN length)
; Input: HL: String descriptor address
; Clobbered registers: A, DE
;-----------------------------------------------------------------------------
load_caq_array:
    ex      (sp),hl               ; HL = String Descriptor, Stack = Text Pointer

    ; Open file
    call    esp_open

    ; Check CAQ header
    call    check_sync_bytes    ; Sync bytes
    ld      bc, 6               ; Check that filename is '######'
    ld      de, FILNAM
    call    esp_read_bytes
    ld      b, 6
    ld      de,FILNAM
.loop:
    ld      a, (de)
    cp      '#'
    jp      nz, err_bad_file
    inc     de
    djnz    .loop

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
    jp      nz, FCERR           ; FC Error if array not found
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

;-----------------------------------------------------------------------------
; Run file
;-----------------------------------------------------------------------------
run_file:
    ; Close any open files
    call    esp_close_all

    call    get_strdesc_arg        ; Get FileSpec
    push    hl                    ; Save String Descriptor

    ; Check for .ROM extension
    call    STRADL                ; Get String Length in BC, Address in DE
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
    call    UPRCMP                ; Compare Them
    pop     hl                    ; Get String Descriptor
    jp      z, load_rom

.load_basic:
    pop     bc                    ; Discard Text Pointer
    ld      bc,RUNC
    push    bc                    ; Return to RUNC     
    call    load_basic_program

.romext: db ".ROM",0

;-----------------------------------------------------------------------------
; SAVE
;
; SAVE "filename"             Save BASIC program
; SAVE "filename",addr,len    Save binary data
; SAVE "filename",*a          Save numeric array a
;-----------------------------------------------------------------------------
ST_SAVE:
    ; Close any open files
    call    esp_close_all

    call    get_strdesc_arg         ; Get FileSpec pointer in HL
    ex      (sp),hl                 ; HL = Text Pointer, Stack = String Descriptor

    ; Check for second parameter
    call    CHRGT2
    cp      ','
    jp      nz, save_basic_program
    rst     CHRGET
    cp      $AA                     ; Token for '*'
    jr      z, .array               ; Array parameter -> save array

    ; Save binary data
    
    ; Get first parameter: address
    call    parse_page_arg          ; Check for page specifier
    push    af                      ; Save it
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
    pop     af                      ; Get back page
    jp      nz,page_save_binary 
    jp      save_binary


    ; Save array
.array:
    call    get_array_argument
    jp      save_caq_array

;-----------------------------------------------------------------------------
; Save basic program
;-----------------------------------------------------------------------------
save_basic_program:
    ex      (sp),hl               ; HL = String Descriptor, Stack = Text Pointer
    call    esp_create            ; Create file

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
    call    esp_create            ; Create file

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
    jp      nz, err_bad_file

    ; Check for 12x$FF
    ld      b, 12
    ld      de, FBUFFR
.loop:
    ld      a, (de)
    cp      $FF
    jp      nz, err_bad_file
    inc     de
    djnz    .loop

    ; Check for $00
    ld      a, (de)
    or      a
    jp      nz, err_bad_file
    ret

;-----------------------------------------------------------------------------
; Parse string at text pointer, return String Length and Text Address
; Input: HL = Text Pointee
; Output: BC = String Length
;         DE = String Address 
;         HL = String Descriptor
;         Text Pointer on Stack
;-----------------------------------------------------------------------------

get_string_arg:
    call    FRMEVL                ; Get Path
    pop     IX                    ; IX = Return Address
    push    hl                    ; Text Pointer on stack
    call    FRESTR                ; Free Temporary String
    call    STRADL                ; Get Length and Text Pointer
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
