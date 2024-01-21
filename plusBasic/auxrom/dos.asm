;=============================================================================
; DOS Routines 
;=============================================================================

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

;------------------------------;-----------------------------------------------------------------------------
; Close all open files
; Input: A: file descriptor
; Output: A: result
;-----------------------------------------------------------------------------
dos_close_all:
    ld      a, ESPCMD_CLOSEALL
    call    esp_cmd
    jp      esp_get_result 

;------------------------------;-----------------------------------------------------------------------------
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

;-----------------------------------------------
; dos_delete - Delete file/directory
; Input: BC: String Length
;        DE: String Address
; Output:  A: Result
; Clobbered: BC, DE
;-----------------------------------------------------------------------------
dos_delete:
    ld      a, ESPCMD_DELETE      ; Set ESP Command
    jp      esp_cmd_string        ; Issue ESP command

;------------------------------------------------------------------------------------------
; dos_create_dir - Delete file/directory
; Input: BC: String Length
;        DE: String Address
; Output:  A: Result
; Clobbered: BC, DE
;-----------------------------------------------------------------------------
dos_create_dir:
    ld      a, ESPCMD_MKDIR       ; Set ESP Command
    jp      esp_cmd_string        ; Issue ESP command

;---------------------------------------------------------------
; dos_get_cwd - Get Current Directory
; Input: HL: Buffer address
; Output:  A: Result, C: String Length, DE = End of String, HL = Buffer Address
; Clobbered: AF'
;-----------------------------------------------------------------------------
dos_get_cwd:
    ld      a,ESPCMD_GETCWD       ; Issue CWD command
    call    esp_cmd
    call    esp_get_result 
    ret     m
    call    get_strbuf_addr       ; HL = StrBuf
    jp      espx_read_buff        ; Read into StrBuf and Return

;-----------------------------------------------------------------------------
; dos_get_file_stat - Return File Status
; Input: BC: String Length
;        DE: String Address
; Output:  A: Result
; Clobbered: BC, DE
;-----------------------------------------------------------------------------
;; FILEDATE$(#filenum/filespec$)
;; FILETIME$(#filenum/filespec$)
;; FILEATTR(#filenum/filespec$)
;; FILELEN(#filenum/filespec$)
dos_filestat:
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
; Open file for append to string descriptor
; Input: HL: string descriptor
; Output: A: file descriptor
;-----------------------------------------------------------------------------
dos_open_append:
    ld      a, FO_WRONLY | FO_CREATE | FO_TRUNC
    jr      dos_open

;-----------------------------------------------------------------------------
; Open file for write to string descriptor
; Input: HL: string descriptor
; Output: A: file descriptor
;-----------------------------------------------------------------------------
dos_open_write:
    ld      a, FO_WRONLY | FO_CREATE | FO_TRUNC
    jr      dos_open

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
dos_open:
    push    af                    ; 
    ld      a,ESPCMD_OPEN
    call    esp_cmd
    pop     af
    call    esp_send_byte
    call    esp_send_strdesc
    jp      esp_get_result 

;-----------------------------------------------------------------------------
; dos_read_dir - Read Directory entry
; Input: A: File descriptor
;       HL: Buffer address
; Output: A: Result
;         B: Filename length
;         C: Entry length
;        DE: Filename address
;        HL: Buffer address
; Flags: Carry set if buffer overflow
; Bytes Contents
;  0-1  Date
;  2-3  Time
;   4   Attribute
;  5-8  File Size
;   9+  Null-terminated filename
;-----------------------------------------------------------------------------
dos_read_dir:
    push    af                    ; Stack = FilDsc, RtnAdr
    ld      a,ESPCMD_READDIR
    call    esp_cmd               ; Send READDIR command
    pop     af                    ; A = FilDsc; Stack = RtnAdr
    call    esp_send_byte         ; Send FilDsc
    call    esp_get_byte          ; A = Status
    or      a                     ; Set flags
    ret     m                     ; Return if error

    push    hl                    ; Stack = BufAdr, RtnAdr
    push    hl                    ; Stack = BufAdr, BufAdr, RtnAdr
    ld      bc,$0900              ; B = Counter, C = ResLen
.bloop
    call    esp_get_byte          ; Read byte
    ld      (hl),a                ; Write to buffer
    inc     hl                    ; Bump BufPtr
    inc     c                     ; Bump ResLen
    djnz    .bloop                ; Countdown and loop
.sloop
    call    esp_get_byte          ; Read character
    ld      (hl),a                ; Write to buffer
    or      a
    jr      z,.done               ; If not NUL
    inc     hl                    ;   Bump BufPtr
    inc     c                     ;   Bump ResLen
    jr      nz,.sloop             ;   Loop if < 256
    scf                           ; Else set carry
.done
    ld      a,c
    sub     9
    ld      b,a                   ; B = NamLen
    pop     hl                    ; HL = BufAdr, Stack = BufAdr, RtnAdr
    ld      de,9
    add     hl,de
    ex      de,hl                 ; DE = NamAdr
    pop     hl                    ; HL = BufAdr; Stack = RtnAdr
    xor     a                     ; Return success
    ret

;-----------------------------------------------------------------------------
; dos_rename - Rename file/directory
; Input: DE: New name string descriptor
;        HL: Old name string descriptor
; Output:  A: Result
; Clobbered: AF' BC, DE
;-----------------------------------------------------------------------------
dos_rename:
    push    de                    ; Stack = NewDsc, RtnAdr
    ld      a, ESPCMD_RENAME      ; Set ESP Command
    call    esp_cmd               ; Issue ESP command 
    call    esp_send_strdesc      ; Send OldDsc
    pop     hl                    ; HL = NewDsc; Stack = RtnAdr
    call    esp_send_strdesc      ; Send NewDsc
    jp      esp_get_result        ; Get result and return

;-----------------------------------------------------------------------------
; dos_rewind_file
; Move to beginning of file
;-----------------------------------------------------------------------------
dos_rewind:
    push    bc
    push    de                    
    ld      bc,0                  ; Seeking position 0
    ld      de,0
    call    dos_seek
    pop     de
    pop     bc
    ret

;-----------------------------------------------------------------------------
; Move to position in open file
; Input:  BC = Offset low 16 bits
;         DE = Offset high 16 bits
; Clobbered registers: A
;-----------------------------------------------------------------------------
;; FILE #filenum GOTO position
dos_seek:
    push    af                    ; Stack = FilDsc, RtnAdr
    ld      a, ESPCMD_SEEK
    call    esp_cmd               ; Send Seek command
    pop     af                    ; A = FilDsc; Stack = RtnAdr
    call    esp_send_byte         ; Send file descriptor
    call    esp_send_long         ; Send offset
    jp      esp_get_result 

;-----------------------------------------------------------------------------
; Get current position in open file
; Output: BC = Offset low 16 bits
;         DE = Offset high 16 bits
; Clobbered registers: A
;-----------------------------------------------------------------------------
; FILEPOS(#filenum)
dos_tell:
    ret
