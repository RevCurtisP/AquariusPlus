;-----------------------------------------------------------------------------
; Parse string at text pointer 
; Returns: HL = String Descriptor
;               Text Pointer on Stack
;-----------------------------------------------------------------------------

get_string_arg:
    call    FRMEVL                ; Get Path
    pop     IX                    ; IX = Return Address
    push    hl                    ; Text Pointer on stack
    call    FRESTR                ; Free Temporary String
    jp      (IX)                  ; Fast Return


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
    call    get_string_arg        ; Get FileSpec pointer in HL
    ex      (sp),hl               ; HL = Text Pointer, Stack = String Descriptor

    ; Check for second parameter
    call    get_arg
    cp      ','
    jr      nz, .basic              ; No parameter -> load as basic program
    call    get_next
    cp      $AA                     ; Token for '*'
    jr      z, .array               ; Array parameter -> load as array

    ; Load as binary to address
    call    FRMNUM                  ; Get number
    call    FRCINT                  ; Convert to 16 bit integer
    ld      (BINSTART), de
    jp      load_binary

    ; Load into array
.array:
    call    get_array_argument
    jp      load_caq_array

.basic:
    ex      (sp),hl               ; HL = String Descriptor, Stack = Text Pointer
    jp      load_basic_program

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
; SAVE
;
; SAVE "filename"             Save BASIC program
; SAVE "filename",addr,len    Save binary data
; SAVE "filename",*a          Save numeric array a
;-----------------------------------------------------------------------------
ST_SAVE:
    ; Close any open files
    call    esp_close_all

    call    get_string_arg        ; Get FileSpec pointer in HL
    ex      (sp),hl               ; HL = Text Pointer, Stack = String Descriptor

    ; Check for second parameter
    call    get_arg
    cp      ','
    jp      nz, save_basic_program
    call    get_next
    cp      $AA                     ; Token for '*'
    jr      z, .array               ; Array parameter -> save array

    ; Save binary data
    
    ; Get first parameter: address
    call    FRMNUM                  ; Get number
    call    FRCINT                  ; Convert to 16 bit integer
    ld      (BINSTART), de

    ; Expect comma
    call    get_arg
    cp      ','
    jp      nz, MOERR
    inc     hl

    ; Get second parameter: length
    call    FRMNUM                  ; Get number
    call    FRCINT                  ; Convert to 16 bit integer
    ld      (BINLEN), de
    jp      save_binary

    ; Save array
.array:
    call    get_array_argument
    jp      save_caq_array


;-----------------------------------------------------------------------------
; MKDIR - Create directory
;-----------------------------------------------------------------------------
ST_MKDIR:

    ld      a, ESPCMD_MKDIR       ; Set ESP Command
    jr      _do_string_arg_cmd    ; Get FileSpec and Do Command

;-----------------------------------------------------------------------------
; DEL - Delete file/directory
;-----------------------------------------------------------------------------
ST_DEL:

    ld      a, ESPCMD_DELETE      ; Set ESP Command
_do_string_arg_cmd:
    push    af                    ; Save it
    call    get_string_arg        ; Get FileSped
    pop     hl                    ; Get Back Text Point
    ex      (sp),hl               ; Swap with ESP Command
    ld      a,h                   ; ESP Command inro A
    call    esp_cmd_strdesc       ; Issue ESP command
    pop     hl                    ; Restore BASIC text pointer
    ret

;-----------------------------------------------------------------------------
; CD - Change directory
;
; No argument -> Show current directory
; With argument -> Change current directory
;-----------------------------------------------------------------------------
ST_CD:
    ; Push BASIC text pointer
    push    hl

    ; Argument given?
    or      a
    jr      nz, .change_dir     ; Yes

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
    pop     hl                    ; Pop BASIC text pointer
    ld      a, ESPCMD_CHDIR       ; Set ESP Command
    jr      _do_string_arg_cmd    ; Get FileSpec and Do Command

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
    call    get_string_arg        ; Get FileSpec pointer in HL

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
    call    esp_get_word

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
    call    esp_get_word

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
;    ld      a, (XTEMP2)
    and     $F0
    jr      nz, .mb

    ; Kilobytes range?
    ld      a, e
;    ld      a, (XTEMP2)
    or      a
    jr      nz, .kb
    ld      a, b
;    ld      a, (XTEMP1)
    and     $FC
    jr      nz, .kb

    ; Bytes range (aaaaaaaa bbbbbbbb ccccccCC DDDDDDDD)
.bytes:
    ld      h,b
;    ld      a, (XTEMP1)
;    ld      h, a
    ld      l,c
;    ld      a, (XTEMP0)
;    ld      l, a
    call    out_number_4digits
    ld      a, 'B'
    rst     OUTCHR
    jr      .get_filename

    ; Kilobytes range: aaaaaaaa bbbbBBBB CCCCCCcc dddddddd
.kb:
    ld      a,e
;    ld      a, (XTEMP2)
    and     a, $0F
    ld      h, a
    ld      a,b
;    ld      a, (XTEMP1)
    ld      l, a
    ld      b, 2
    call    srlh_rrl_out
    ld      a, 'K'
    rst     OUTCHR
    jr      .get_filename

    ; Megabytes range: AAAAAAAA BBBBbbbb cccccccc dddddddd
.mb:
    ld      h,d
;    ld      a, (XTEMP3)
;    ld      h, a
    ld      h,e
;    ld      a, (XTEMP2)
;    ld      l, a
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
; esp_error
;-----------------------------------------------------------------------------
esp_error:
    neg
    dec     a
    cp      -ERR_NOT_EMPTY
    jr      c, .ok
    ld      a, -ERR_OTHER - 1

.ok:
    ld      hl, .error_msgs
    add     a,a
    add     l
    ld      l, a
    ld      a, h
    adc     a, 0
    ld      h, a
    ld      a, (hl)
    inc     hl
    ld      h, (hl)
    ld      l, a

    ; Print error message
    ld      a, '?'
    rst     OUTCHR
    jp      ERRFN1

.error_msgs:
    dw .msg_err_not_found     ; -1: File / directory not found
    dw .msg_err_too_many_open ; -2: Too many open files / directories
    dw .msg_err_param         ; -3: Invalid parameter
    dw .msg_err_eof           ; -4: End of file / directory
    dw .msg_err_exists        ; -5: File already exists
    dw .msg_err_other         ; -6: Other error
    dw .msg_err_no_disk       ; -7: No disk
    dw .msg_err_not_empty     ; -8: Not empty

.msg_err_not_found:     db "Not found",0
.msg_err_too_many_open: db "Too many open",0
.msg_err_param:         db "Invalid param",0
.msg_err_eof:           db "EOF",0
.msg_err_exists:        db "Already exists",0
.msg_err_other:         db "Unknown error",0
.msg_err_no_disk:       db "No disk",0
.msg_err_not_empty:     db "Not empty",0

;-----------------------------------------------------------------------------
; Bad file error
;-----------------------------------------------------------------------------
err_bad_file:
    ld      hl, .msg_bad_file

    ; Print error message
    ld      a, '?'
    rst     OUTCHR
    jp      ERRFN1

.msg_bad_file:       db "Bad file",0


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

; Set Return Address for CHEAD (callable in aqplus S3BASIC)
set_chead_return:
    ld      bc,MAIN             ; Make CHEAD return to MAIN
    push    bc
    jmp     HOOK5+1

;-----------------------------------------------------------------------------
; Close any open file/directory descriptor
;
; Clobbered registers: A
;-----------------------------------------------------------------------------
esp_close_all:
    ld      a, ESPCMD_CLOSEALL
    call    esp_cmd
    jp      esp_get_result


;-----------------------------------------------------------------------------
; Create file from string descriptor in HL
;-----------------------------------------------------------------------------
esp_create:
    ld      a, ESPCMD_OPEN
    call    esp_cmd
    ld      a, FO_WRONLY | FO_CREATE | FO_TRUNC
    call    esp_send_byte
    call    esp_send_strdesc
    jp      esp_get_result

;-----------------------------------------------------------------------------
; Read bytes
; Input:  HL: destination address
;         DE: number of bytes to read
; Output: HL: next address (start address if no bytes read)
;         DE: number of bytes actually read
;
; Clobbered registers: A, HL, DE
;-----------------------------------------------------------------------------
esp_read_bytes:
    ld      a, ESPCMD_READ
    call    esp_cmd

    ; Send file descriptor
    xor     a
    call    esp_send_byte

    ; Send read size
    ld      a, e
    call    esp_send_byte
    ld      a, d
    call    esp_send_byte

    ; Get result
    call    esp_get_result

    ; Get number of bytes actual read
    call    esp_get_word

    push    de

.loop:
    ; Done reading? (DE=0)
    ld      a, d
    or      a, e
    jr      z, .done

    call    esp_get_byte
    ld      (hl), a
    inc     hl
    dec     de
    jr      .loop

.done:
    pop     de
    ret


;-----------------------------------------------------------------------------
; Send bytes
; Input:  HL: source address
;         DE: number of bytes to write
; Output: HL: next address
;         DE: number of bytes actually written
;-----------------------------------------------------------------------------
esp_send_bytes:
    push    de

.loop:
    ; Done sending? (DE=0)
    ld      a, d
    or      a, e
    jr      z, .done

    ld      a, (hl)
    call    esp_send_byte
    inc     hl
    dec     de
    jr      .loop

.done:
    pop     de
    ret

;-----------------------------------------------------------------------------
; Write bytes
; Input:  HL: source address
;         DE: number of bytes to write
; Output: HL: next address
;         DE: number of bytes actually written
;
; Clobbered registers: A, HL, DE
;-----------------------------------------------------------------------------
esp_write_bytes:
    ld      a, ESPCMD_WRITE
    call    esp_cmd

    ; Send file descriptor
    xor     a
    call    esp_send_byte

    ; Send write size
    ld      a, e
    call    esp_send_byte
    ld      a, d
    call    esp_send_byte

    ; Send bytes
    call    esp_send_bytes

    ; Get result
    call    esp_get_result

    ; Get number of bytes actual written
    call    esp_get_byte
    ld      e, a
    call    esp_get_byte
    ld      d, a

    ret

;-----------------------------------------------------------------------------
; Seek
; Input:  DE: offset
;
; Clobbered registers: A, DE
;-----------------------------------------------------------------------------
esp_seek:
    ld      a, ESPCMD_SEEK
    call    esp_cmd

    ; Send file descriptor
    xor     a
    call    esp_send_byte

    ; Send offset
    ld      a, e
    call    esp_send_byte
    ld      a, d
    call    esp_send_byte
    xor     a
    call    esp_send_byte
    call    esp_send_byte

    ; Get result
    call    esp_get_result
    ret

;-----------------------------------------------------------------------------
; Get next character, skipping spaces
;  in: HL = text pointer
; out: NZ, A = next non-space char, HL = address of char in text
;      Z,  A = 0, HL = end of text
;-----------------------------------------------------------------------------
get_next:                   ; Starting at next location
    inc     hl
get_arg:                    ; Starting at current location
    ld      a, (hl)
    or      a
    ret     z               ; Return Z if NULL
    cp      ' '
    ret     nz              ; Return NZ if not SPACE
    jr      get_next

;-----------------------------------------------------------------------------
; Check for sync sequence (12x$FF, 1x$00)
;-----------------------------------------------------------------------------
check_sync_bytes:
    ; Read 13 bytes into TMPBUF
    ld      de, 13
    ld      hl, TMPBUF
    call    esp_read_bytes
    ld      a, e
    cp      13
    jp      nz, err_bad_file

    ; Check for 12x$FF
    ld      c, 12
    ld      hl, TMPBUF
.loop:
    ld      a, (hl)
    cp      $FF
    jp      nz, err_bad_file
    inc     hl
    dec     c
    jr      nz, .loop

    ; Check for $00
    ld      a, (hl)
    or      a
    jp      nz, err_bad_file
    ret

;-----------------------------------------------------------------------------
; Load binary data from File into BINSTART
; Input: HL: String descriptor address
; Clobbered registers: A, DE
;-----------------------------------------------------------------------------
load_binary:
    ex      (sp),hl               ; HL = String Descriptor, Stack = Text Pointer

    ; Load file into memory
    call    esp_open
    ld      hl, (BINSTART)
    ld      de, $FFFF
    call    esp_read_bytes
    call    esp_close_all

    pop     hl                    ; Get Back Text Pointer
    ret

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
    ld      de, 6               ; Check that filename is '######'
    ld      hl, TMPBUF
    call    esp_read_bytes
    ld      c, 6
    ld      hl, TMPBUF
.loop:
    ld      a, (hl)
    cp      '#'
    jp      nz, err_bad_file
    inc     hl
    dec     c
    jr      nz, .loop

    ; Load data into array
    ld      hl, (BINSTART)
    ld      de, (BINLEN)
    call    esp_read_bytes

    ; Close file
    call    esp_close_all

    pop     hl
    ret

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
    ld      de, 6               ; Skip filename
    ld      hl, TMPBUF
    call    esp_read_bytes
    call    check_sync_bytes    ; Sync bytes
    
    ; Load actual program
    ld      hl, (TXTTAB)
    ld      de, $FFFF
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
; Save basic program
;-----------------------------------------------------------------------------
save_basic_program:
    ex      (sp),hl               ; HL = String Descriptor, Stack = Text Pointer
    call    esp_create            ; Create file

    ; Write CAQ header
    ld      hl, sync_bytes      ; Sync bytes
    ld      de, 13
    call    esp_write_bytes
    ld      hl, .caq_filename   ; Filename
    ld      de, 6
    call    esp_write_bytes
    ld      hl, sync_bytes      ; Sync bytes
    ld      de, 13
    call    esp_write_bytes

    ; Write BASIC data
    ld      de, (TXTTAB)            ; DE = start of BASIC program
    ld      hl, (VARTAB)            ; HL = end of BASIC program
    sbc     hl, de
    ex      de, hl                  ; HL = start, DE = length of BASIC program
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
    ld      hl, sync_bytes      ; Sync bytes
    ld      de, 13
    call    esp_write_bytes
    ld      hl, .array_filename ; Filename
    ld      de, 6
    call    esp_write_bytes

    ; Write array data
    ld      hl, (BINSTART)
    ld      de, (BINLEN)
    call    esp_write_bytes

    ; Close file
    call    esp_close_all

    pop     hl
    ret

.array_filename: db "######"

;-----------------------------------------------------------------------------
; Save binary
;-----------------------------------------------------------------------------
save_binary:
    push    hl

    ; Create file
    call    esp_create

    ; Write binary data
    ld      hl, (BINSTART)
    ld      de, (BINLEN)
    call    esp_write_bytes

    ; Close file
    call    esp_close_all

    pop     hl
    ret

;-----------------------------------------------------------------------------
; Run file
;-----------------------------------------------------------------------------
run_file:
    ; Close any open files
    call    esp_close_all

    call    get_string_arg        ; Get FileSpec
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
    jr      z, load_rom

.load_basic:
    pop     bc                    ; Discard Text Pointer
    ld      bc,RUNC
    push    bc                    ; Return to RUNC     
    call    load_basic_program

.romext: db ".ROM",0

;-----------------------------------------------------------------------------
; Load ROM file
;-----------------------------------------------------------------------------
load_rom:
    ; Open file
    call    esp_open

    ; Map RAM in bank3
    ld      a, 35
    out     (IO_BANK3), a

    ; Load file
    ld      hl, $C000
    ld      de, $4000
    call    esp_read_bytes

    ; Check length
    ld      a, d
    cp      $20         ; 8KB ROM?
    jr      nz, .ok

    ; 8KB ROM, duplicate data to second 8KB area
    ld      hl, $C000
    ld      de, $E000
    ld      bc, $2000
    ldir
.ok:
    call    esp_close_all

descramble_rom:
    ; Determine scramble value
    xor     a
    ld      hl, $E003
    ld      b, 12
.loop:
    add     a, (hl)
    inc     hl
    add     a, b
    dec     b
    jr      nz, .loop
    xor     (hl)
    ld      b, a

    ; Descramble ROM
    ld      hl, $C000
    ld      de, $4000
.loop2:
    ld      a, b
    xor     (hl)
    ld      (hl), a

    inc     hl
    dec     de
    ld      a, d
    or      e
    jr      nz, .loop2

    ; Reinit banks
    ld      a, 33
    out     (IO_BANK1), a
    ld      a, 34
    out     (IO_BANK2), a

    ; Bank3 -> readonly
    ld      a, 35 | BANK_READONLY
    out     (IO_BANK3), a

    ; Reinit stack pointer
    ld      sp, $38A0

    ; Start ROM
    jp      $E010
