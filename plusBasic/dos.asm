;-----------------------------------------------------------------------------
; dos_change_dir - Change Directory
; Input: BC: String Length
;        DE: String Address
; Output:  A: Result
; Clobbered: BC, DE
;-----------------------------------------------------------------------------
dos_change_dir:
    ld      a, ESPCMD_CHDIR       ; Set ESP Command
    jp      esp_cmd_string        ; Issue ESP command


;-----------------------------------------------------------------------------
; dos_delete_file - Delete file/directory
; Input: BC: String Length
;        DE: String Address
; Output:  A: Result
; Clobbered: BC, DE
;-----------------------------------------------------------------------------
dos_delete_file:
    ld      a, ESPCMD_DELETE      ; Set ESP Command
    jp      esp_cmd_string        ; Issue ESP command

;-----------------------------------------------------------------------------
; dos_create_dir - Delete file/directory
; Input: BC: String Length
;        DE: String Address
; Output:  A: Result
; Clobbered: BC, DE
;-----------------------------------------------------------------------------
dos_create_dir:
    ld      a, ESPCMD_MKDIR       ; Set ESP Command
    jp      esp_cmd_string        ; Issue ESP command


;-----------------------------------------------------------------------------
; dos_get_cwd - Get Current Directory
; Input: Buffer address
; Output:  A: Result, E: String Length, DE = End of String, HL = Buffer Address
;-----------------------------------------------------------------------------
dos_get_cwd:
;    call    swap_basic_buffs      ; Swap in Extended Work Area
    ld      a,ESPCMD_GETCWD       ; Issue CWD command
    call    esp_cmd
    call    esp_get_result
    jp      m,.done               ; Return if Error
    call    esp_read_to_buff      ; Get current directory and write to buffer
.done
;    jp     restore_bank3         ; Back to Original Bank 3
    ret

;-----------------------------------------------------------------------------
; dos_create_dir - Delete file/directory
; Input: DE: New name string descriptor
;        HL: Old name string descriptor
; Output:  A: Result
; Clobbered: BC, DE
;-----------------------------------------------------------------------------
dos_rename_file:
    push    de                    ; Save new name descriptor
    ld      a, ESPCMD_RENAME      ; Set ESP Command
    call    esp_cmd               ; Issue ESP command and send old name
    call    esp_send_strdesc      ; Send old name    
    pop     hl                    ; HL = new name descriptor
    call    esp_send_strdesc      ; Send new name
    jp      esp_get_result        ; Get result and return

;-----------------------------------------------------------------------------
; dos_get_file_stat - Return File Status
; Input: BC: String Length
;        DE: String Address
; Output:  A: Result
; Clobbered: BC, DE
;-----------------------------------------------------------------------------
dos_get_filestat:
    ld      a, ESPCMD_STAT       ; Set ESP Command
    jp      esp_cmd_string        ; Issue ESP command
    jp      m,.done               ; Return if Error
    call    esp_get_de
    ld      (FILEDATE),de
    call    esp_get_de
    ld      (FILETIME),de
    call    esp_get_byte
    ld      (FILEATTR),de
    call    esp_get_long
    ld      (FILESIZE),bc
    ld      (FILESIZE+2),de
    call    esp_get_result
.done
    ret

page_set_binvars:
    ld      de,(BINSTART)
    call    page_set_address
    ld      (BINSTART),de
    ret

;-----------------------------------------------------------------------------
; Load binary data from File into BINSTART in page A
; Input: A: Page
;        HL: String descriptor address
; Clobbered registers: A, DE
;-----------------------------------------------------------------------------
dos_load_paged:
    call    page_set_binvars

;-----------------------------------------------------------------------------
; Load binary data from File into BINSTART
; Input: HL: String descriptor address
; Clobbered registers: A, DE
;-----------------------------------------------------------------------------
dos_load_binary:
    ex      (sp),hl               ; HL = String Descriptor, Stack = Text Pointer
    ; Load file into memory
    call    esp_open
    ld      de, (BINSTART)
    ld      bc, $FFFF
    call    esp_read_bytes
    call    esp_close_all
    call    page_restore_plus
    pop     hl                    ; Get Back Text Pointer
    ret


;-----------------------------------------------------------------------------
; Load binary data of length BINSIIZE from BINSTART in page A to file
; Input: A: Page
;        HL: String descriptor address
; Clobbered registers: A, DE
;-----------------------------------------------------------------------------
dos_save_paged:
    call    page_set_binvars

;-----------------------------------------------------------------------------
; Save binary
;-----------------------------------------------------------------------------
dos_save_binary:
    ex      (sp),hl               ; HL = String Descriptor, Stack = Text Pointer

    ; Create file
    call    esp_create

    ; Write binary data
    ld      de, (BINSTART)
    ld      bc, (BINLEN)
    call    esp_write_bytes

    ; Close file
    call    esp_close_all
    call    page_restore_plus
    pop     hl
    ret

;-----------------------------------------------------------------------------
; Load ROM file
;-----------------------------------------------------------------------------
dos_load_rom:
    ; Open file
    call    esp_open

    ; Map RAM in bank3
    ld      a, 35
    out     (IO_BANK3), a

    ; Load file
    ld      de, $C000
    ld      bc, $4000
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

    ; Descramble and start ROM
    jp      descramble_rom
