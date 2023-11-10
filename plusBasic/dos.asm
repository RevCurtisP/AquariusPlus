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
; Input: HL: Buffer address
; Output:  A: Result, E: String Length, DE = End of String, HL = Buffer Address
;-----------------------------------------------------------------------------
dos_get_cwd:
    ld      a,ESPCMD_GETCWD       ; Issue CWD command
    call    esp_cmd
    call    esp_get_result 
    jp      m,.done               ; Return if Error
    call    esp_read_to_buff      ; Get current directory and write to buffer
.done
    ret

;-----------------------------------------------------------------------------
; dos_rename_dir - Delete file/directory
; Input: DE: New name string descriptor
;        HL: Old name string descriptor
; Output:  A: Result
; Clobbered: BC, DE
;-----------------------------------------------------------------------------
dos_rename_file:
    push    de                    ; Save new name descriptor
    ld      a, ESPCMD_RENAME      ; Set ESP Command
    call    esp_cmd               ; Issue ESP command 
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
    ld      a, ESPCMD_STAT        ; Set ESP Command
    call    esp_cmd               ; Issue ESP command 
    jp      m,.done               ; Return if Error
    call    esp_send_string       ; Send filename  
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
