;-----------------------------------------------------------------------------
; dos_change_dir - Change Directory
; Input: BC: String Length
;        DE: Address of Byte after String
; Output:  A: Result
; Clobbered: BC, DE
;-----------------------------------------------------------------------------
dos_change_dir:
    ld      a, ESPCMD_CHDIR      ; Set ESP Command
    jp      esp_cmd_string        ; Issue ESP command


;-----------------------------------------------------------------------------
; dos_delete_file - Delete file/directory
; Input: BC: String Length
;        DE: Address of Byte after String
; Output:  A: Result
; Clobbered: BC, DE
;-----------------------------------------------------------------------------
dos_delete_file:
    ld      a, ESPCMD_DELETE      ; Set ESP Command
    jp      esp_cmd_string        ; Issue ESP command

;-----------------------------------------------------------------------------
; dos_create_dir - Delete file/directory
; Input: BC: String Length
;        DE: Address of Byte after String
; Output:  A: Result
; Clobbered: BC, DE
;-----------------------------------------------------------------------------
dos_create_dir:
    ld      a, ESPCMD_MKDIR       ; Set ESP Command
    jp      esp_cmd_string        ; Issue ESP command


;-----------------------------------------------------------------------------
; dos_get_cwd - Get Current Directory
; Sets: string_buff: Current Directory
; Output:  A: Result, E: String Length, DE = End of String, HL = Buffer Address
;-----------------------------------------------------------------------------
dos_get_cwd:
    ld      a,ESPCMD_GETCWD       ; Issue CWD command
    call    esp_cmd
    call    esp_get_result
    ret     m                     ; Return if Error

;-----------------------------------------------------------------------------
; dos_read_to_buff - Get Current Directory
; Assumes string_buff is on a 256 byte boundary
; Sets: string_buff: Current Directory
; Output:  E: String Length, DE = End of String, HL = Buffer Address
;-----------------------------------------------------------------------------
dos_read_to_buff:
    push    af                    ; Save A
    ld      b,255                 ; Maximum Length, Length Counter
    ld      hl,string_buff        ; BASIC String BUFFER
    ld      d,h
    ld      e,l
.loop
    call    esp_get_byte          ; Get character
    ld      (de),a                ; Store in Buffer
    inc     de
    or      a
    jr      z,.done               ; Return if end of String
    djnz    .loop     
    xor     a
    ld      (de),a                ; Add Null Terminator
.done
    ld      a,e
    ld      (buff_strlen),a
    pop     af                    ; Restore Result
    ret

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
; HOOK 5 - Set Return Address for CHEAD when executed from S3BASIC
;-----------------------------------------------------------------------------
set_chead_return:
    ld      bc,MAIN             ; Make CHEAD return to MAIN
    push    bc
    jmp     HOOK5+1


;-----------------------------------------------------------------------------
; Check for sync sequence (12x$FF, 1x$00)
;-----------------------------------------------------------------------------
check_sync_bytes:
    ; Read 13 bytes into FBUFFR
    ld      de, 13
    ld      hl, FBUFFR
    call    esp_read_bytes
    ld      a, e
    cp      13
    jp      nz, err_bad_file

    ; Check for 12x$FF
    ld      c, 12
    ld      hl, FBUFFR
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
    ld      hl, FILNAM
    call    esp_read_bytes
    ld      c, 6
    ld      hl, FILNAM
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
    ld      de, 6               ; Read filename
    ld      hl, FILNAM
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
