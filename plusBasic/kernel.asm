; aqplusbas.asm
    jp      clear_default         ; Clear to default colors, home
    jp      clear_home            ; Clear text screen, home cursor
    jp      clear_screen          ; Clear the text screen
    jp      move_cursor           ; Reposition text cursor
    jp      byte_to_hex           ; Convert byte to hex

; dos.asm
    jp      dos_change_dir        ; Change Directory
    jp      dos_delete_file       ; Delete file/directory
    jp      dos_create_dir        ; Create directory
    jp      dos_get_cwd           ; Get Current Directory
    jp      dos_rename_file       ; Delete file/directory
    jp      dos_get_file_stat     ; Return File Status
    jp      dos_load_paged        ; Load binary file into paged memory`
    jp      dos_load_binary       ; Load binary file into paged memory`
    jp      dos_save_paged        ; Save binary data from paged memort to file
    jp      dos_save_binary       ; Save binary data from main memory to file
    jp      dos_load_rom          ; Load and decrypt cartridge

; esp.asm
    jp      esp_cmd               ; Issue command 
    jp      esp_cmd_string        ; Issue ESP command with string argument
    jp      esp_get_result        ; Get first ESP result byte
    jp      esp_create            ; Create file
    jp      esp_open              ; Open file
    jp      esp_close             ; Close file or directory
    jp      esp_close_all         ; Close all files and directories
    jp      esp_get_byte          ; Get byte into A
    jp      esp_get_bc            ; Get word into BC
    jp      esp_get_de            ; Get word into DE
    jp      esp_get_long          ; Get long into BC,DE
    jp      esp_read_to_buff      ; Read bytes to string buffer
    jp      esp_read_bytes        ; Read bytes to main memory
    jp      esp_read_paged        ; Read bytes to paged memory
    jp      esp_send_byte         ; Send byte in A
    jp      esp_send_bc           ; Send word in BC
    jp      esp_send_de           ; Send word in DE
    jp      esp_send_long         ; Send long in BC,DE
    jp      esp_send_bytes        ; Send bytes from main memory
    jp      esp_send_string       ; Send String from main memory
    jp      esp_send_strdesc      ; Send BASIC string from main memory
    jp      esp_write_bytes       ; Write bytes from main memory
    jp      esp_write_repbyte     ; Write byte repeatedly
    jp      esp_write_paged       ; Write bytes from paged memory
    jp      esp_seek              ; Move to position in open file
    jp      esp_get_datetime      ; Read date and time into string buffer
    jp      esp_tell              ; Get current position in open file

; page.asm
    jp      page_copy             ; Copy entire Page to another Page
    jp      page_copy_bytes:      ; Copy Bytes from Page to another Page
    jp      page_read_byte        ; Read Byte from Page    
    jp      page_write_byte       ; Write Byte to Page
    jp      page_read_word        ; Read Word from Page
    jp      page_write_word       ; Write Word to Page
    jp      page_read_bytes       ; *** needs written ***
    jp      page_write_bytes      ; Write Bytes to Page
    jp      page_set4read_coerce  ; Map Page into Bank 3 and coerce address to bank 3
    jp      page_set4write_coerce ; Map Page into valid Bank 3 and coerce address to bank 3
    jp      page_restore_plus     ; Restore Bank 3 to Page 1
    jp      page_next_address     ; Map next Page into Bank 3 and coerce address to bank 3
    jp      page_coerce_address   ; Coerce address in to bank 3
    jp      page_set_for_write    ; Map Page into Bank 3 for write
    jp      page_set_for_read     ; Map Page into Bank 3 for write
    jp      page_check_write      ; Verify page is valid for Write
    jp      page_check_read       ; Verify page is valid for Read
    jp      page_inc_addr         ; Increment Page Write Address
    jp      page_next             ; Map next Page into Bank 3 

; sbuff.asm
    jp      sbuff_create_string   ; Create BASIC string from buffer
    jp      sbuff_get_len         ; Get Length of String in String Buffer
    jp      sbuff_read_byte       ; Read Byte from String Buffer
    jp      sbuff_read_word       ; Read Word from String Buffer
    jp      sbuff_init            ; Initialize String Buffer
    jp      sbuff_write_byte      ; Write Byte to String Buffer
    jp      sbuff_write_word      ; Write Word to String Buffer
    
; util.asm    
    jp      print_c_string        ; Print C style (null terminated) string   
    jp      print_string_immd     ; Print inline null terminated string 
    jp      free_addr_len         ; Free temporary string, return string address and length
    jp      string_addr_len       ; Get string address and length from string descriptor
    jp      string_cmp_upper      ; Compare uppercased string to another string
    jp      shift_hl_left         ; Shift HL left  

; gfx.asm    
    jp      gfx_exec              ; Call graphics module routine

