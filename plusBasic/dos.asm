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

dos_get_cwd:
    call    page_map_auxrom
    jp      dosx_get_cwd

dos_rename_file:
    call    page_map_auxrom
    jp      dosx_rename_file

dos_rewind_file:
    call    page_map_auxrom
    jp      dosx_rewind_file

;-----------------------------------------------------------------------------
; dos_get_file_stat - Return File Status
; Input: BC: String Length
;        DE: String Address
; Output:  A: Result
; Clobbered: BC, DE
;-----------------------------------------------------------------------------
dos_get_filestat:
    call    page_map_auxrom
    jp      dosx_get_filestat

;-----------------------------------------------------------------------------
; dos_open_dir - Open Directory for Read
; Input: BC: String Length
;        DE: String Address
; Output:  A: Result
; Clobbered: BC, DE
;-----------------------------------------------------------------------------
dos_open_dir:
    ld      a, ESPCMD_OPENDIR     ; Set ESP Command
    jp      esp_cmd_string        ; Get FileSpec and Do Command

;-----------------------------------------------------------------------------
; Open file for write to string descriptor
; Input: HL: string descriptor
; Output: A: file descriptor
;-----------------------------------------------------------------------------
dos_open_write:
    ld      a, FO_WRONLY | FO_CREATE | FO_TRUNC
    jr      dos_open_file

;-----------------------------------------------------------------------------
; Open file for read_only to string descriptor
; Input: HL: string descriptor
; Output: A: file descriptor
;-----------------------------------------------------------------------------
dos_open_read:
    ld      a,FO_RDONLY

;-----------------------------------------------------------------------------
; Open file to string descriptor
; Input: A: File Open Mode 
;       HL: string descriptor
; Output: A: file descriptor
;-----------------------------------------------------------------------------
dos_open_file:
    push    af                    ; 
    ld      a,ESPCMD_OPEN
    call    esp_cmd
    pop     af
    call    esp_send_byte
    call    esp__send_strdesc
    jp      esp_get_result 

;-----------------------------------------------------------------------------
; Close file pointed to by descriptor
; Input: A: file descriptor
; Output: A: result
;-----------------------------------------------------------------------------
dos_close:
    push    af
    ld      a, ESPCMD_CLOSE
    call    esp_cmd
    pop     af
    call    esp_send_byte
    jp      esp_get_result 

;-----------------------------------------------------------------------------
; Save binary data from paged memory to file
; Input: A: Page
;        HL: Filename atring descriptor address
; Uses: BINSTART: Save address
;       BINLEN: Data ength
; Clobbered registers: A, DE
;-----------------------------------------------------------------------------
dos_save_paged:
    push    af                    
    call    dos_open_write
    jp      m,discard_ret
    pop     af
    ld      de, (BINSTART)
    ld      bc, (BINLEN)
    call    esp_write_paged
    push    af
    call    esp_close_all
    pop     af
    ret

;-----------------------------------------------------------------------------
; Save binary data from main memory to file
; Input: HL: Filename atring descriptor address
; Uses: BINSTART: Save address
;       BINLEN: Data ength
; Clobbered registers: A, DE
;-----------------------------------------------------------------------------
dos_save_binary:
    ; Create file
    call    dos_open_write
    ret     m

    ; Write binary data
    ld      de, (BINSTART)
    ld      bc, (BINLEN)
    call    esp_write_bytes

    ; Close file
    push    af
    call    esp_close_all
    pop     af
    and     $01                   ; Clear zero and carry flags
    ret

;-----------------------------------------------------------------------------
; Load ROM file
;-----------------------------------------------------------------------------
dos_load_rom:
    ; Open file
    call    dos_open_read
    ret     m

    ; Map RAM in bank3
    ld      a, 35
    out     (IO_BANK3), a

    ; Load file
    ld      de, $C000
    ld      bc, $4000
    call    esp_read_bytes

    ; Check length
    ld      a, b
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
