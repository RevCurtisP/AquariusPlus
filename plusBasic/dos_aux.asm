;-----------------------------------------------------------------------------
; dos_get_cwd - Get Current Directory
; Input: HL: Buffer address
; Output:  A: Result, E: String Length, DE = End of String, HL = Buffer Address
; Clobbered: AF'
;-----------------------------------------------------------------------------
dosx_get_cwd:
    ld      a,ESPCMD_GETCWD       ; Issue CWD command
    call    esp_cmd
    call    esp_get_result 
    jp      m,.done               ; Return if Error
    call    espx_read_to_buff     ; Get current directory and write to buffer
.done
    jp      page_restore_bank3

;-----------------------------------------------------------------------------
; dos_get_file_stat - Return File Status
; Input: BC: String Length
;        DE: String Address
; Output:  A: Result
; Clobbered: BC, DE
;-----------------------------------------------------------------------------
dosx_get_filestat:
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
    jp      page_restore_bank3


;-----------------------------------------------------------------------------
; dos_rename_dir - Delete file/directory
; Input: DE: New name string descriptor
;        HL: Old name string descriptor
; Output:  A: Result
; Clobbered: AF' BC, DE
;-----------------------------------------------------------------------------
dosx_rename_file:
    push    de                    ; Save new name descriptor
    ld      a, ESPCMD_RENAME      ; Set ESP Command
    call    esp_cmd               ; Issue ESP command 
    call    esp_send_strdesc      ; Send old name    
    pop     hl                    ; HL = new name descriptor
    call    esp_send_strdesc      ; Send new name
    jp      espx_get_result       ; Get result and return
