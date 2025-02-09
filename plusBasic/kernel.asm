;=====================================================================================
; Kernel jump table - Starts at $2100
;=====================================================================================
_jump_table:
; <<System Routines>>
    jp      aux_call              ; Call routine in auxillary ROM
    jp      ext_call              ; Call routine in extended ROM
    jp      gfx_kernel_call       ; Call graphics routine
    jp      str_copy              ; Copy null-terminated string
    jp      str_length            ; Get null-terminated string length
    jp      print_c_string        ; Print null-terminated string
    jp      print_string_immd     ; Print inline null terminated string
    jp      str_stringdesc        ; Build descriptor for null terminated string
    jp      str_tempdesc          ; Build descriptor for null terminated string in DSCTMP
    jp      uppercase_char        ; Convert character to lowercase
    jp      lowercase_char        ; Convert character to uppercase
    jp      sys_fill_mem          ; Fill main memory with byte
    jp      sys_swap_mem          ; Swap bytes
    jp      key_clear_fifo        ; Clear alternate keyboard buffer
    jp      key_read_ascii        ; Read from alternate keyboard buffer
    jp      key_read_scancode     ; Read scan code sequence from alternate keyboard buffer
    jp      key_set_keymode       ; Set alternate keyboard buffer mode
    jp      key_pressed           ; Check matrix for keypress
    jp      sys_ver_plusbasic     ; Get plusBASIC version string
    jp      sys_num_ver           ; Convert version string to 24-bit number
    
; esp.asm <<ESP Interface>>
    jp      esp_cmd               ; Issue command 
    jp      esp_cmd_string        ; Issue ESP command with string argument
    jp      esp_get_result        ; Get first ESP result byte
    jp      esp_close_all         ; Close all files and directories
    jp      esp_get_byte          ; Get byte into A
    jp      esp_get_bc            ; Get word into BC
    jp      esp_get_de            ; Get word into DE
    jp      esp_get_long          ; Get long into BC,DE
    jp      esp_get_datetime      ; Read date and time into string buffer
    jp      esp_get_mouse         ; Read mouse position
    jp      esp_get_version       ; Read system version
    jp      esp_load_fpga         ; Load FPGA Core  
    jp      esp_read_buff         ; Read bytes to string buffer
    jp      esp_read_byte         ; Read byte from ESP
    jp      esp_read_bytes        ; Read bytes to main memory
    jp      esp_read_line         ; Read line from text file
    jp      esp_read_paged        ; Read bytes to paged memory
    jp      esp_send_byte         ; Send byte in A
    jp      esp_send_bc           ; Send word in BC
    jp      esp_send_de           ; Send word in DE
    jp      esp_send_long         ; Send long in BC,DE
    jp      esp_send_bytes        ; Send bytes from main memory
    jp      esp_send_string       ; Send String from main memory
    jp      esp_send_strdesc      ; Send BASIC string from main memory
    jp      esp_set_keymode       ; Set alternate keyboard port mode
    jp      esp_write_byte        ; ** Not implemented **
    jp      esp_write_bytes       ; Write bytes from main memory
    jp      esp_write_repbyte     ; Write byte repeatedly
    jp      esp_write_paged       ; Write bytes from paged memory
    jp      just_ret
    jp      just_ret
    jp      just_ret

; page.asm <<Paged Memory>>
    jp      page_check_next4read  ; Verify next page is valid for read
    jp      page_check_next4write ; Verify next page is valid for write
    jp      page_check_read       ; Verify page is valid for Read
    jp      page_check_write      ; Verify page is valid for Write
    jp      page_coerce_de_addr   ; Coerce address in to bank 3
    jp      page_coerce_hl_addr   ; Coerce address in to bank 1
    jp      page_copy_bytes       ; Copy Bytes from Page to another Page
    jp      page_fast_copy        ; Copy bytes between pages with no rollover
    jp      page_fast_read_bytes  ; Read bytes from page with no rollover
    jp      page_fast_write_bytes ; Write bytes to page with no rollover
    jp      page_fill_all_byte    ; Fill entire page with byte
    jp      page_fill_all_word    ; Fill entire page with word
    jp      page_fill_byte        ; Fill paged memory with byte
    jp      page_fill_word        ; Fill paged memory with word
    jp      page_full_copy        ; Copy entire page to another page
    jp      page_inc_de_addr      ; Increment Page 3 Write Address
    jp      page_inc_hl_addr      ; Increment Page 1 Write Address
    jp      page_map_auxrom       ; Map Auxillary ROM into bank 3, saving original page
    jp      page_map_extrom       ; Map Extended ROM into bank 3, saving original page
    jp      page_map_bank1        ; Map page into bank 1, saving original page
    jp      page_map_bank3        ; Map page into bank 3, saving original page
    jp      page_map_vidram       ; Map Video RAM into bank 1, saving original page
    jp      page_mem_compare      ; Compare main memory with paged memory
    jp      page_mem_swap_bytes   ; Swap paged memory with main memory
    jp      page_next_bank1       ; Map next Page into Bank 1
    jp      page_next_bank3       ; Map next Page into Bank 3 
    jp      page_next_de_address  ; Map next Page into Bank 3 and coerce address
    jp      page_next_hl_address  ; Map next Page into Bank 1 and coerce address
    jp      page_page_compare     ; Compare paged memory with paged memory
    jp      page_read_byte        ; Read Byte from Page    
    jp      page_read_bytes       ; Read bytes from paged memory to main memory
    jp      page_read_word        ; Read Word from Page
    jp      page_restore_bank1    ; Restore original page to bank 1 and return 
    jp      page_restore_bank3    ; Restore original page to bank 3 and return
    jp      page_restore_two      ; Restore original pages to banks 1 and 3
    jp      page_set_for_read     ; Map Page into Bank 3 for write
    jp      page_set4read_coerce  ; Map Page into Bank 3 and coerce address to bank 3       ;
    jp      page_set_for_write    ; Map Page into Bank 3 for write
    jp      page_set4write_coerce ; Map Page into valid Bank 3 and coerce address to bank 3
    jp      page_set_aux          ; Set bank 3 to Auxillary ROM page
    jp      page_set_basbuf       ; Set bank 3 to BASIC buffer RAM page
    jp      page_set_plus         ; Set bank 3 to Extended ROM page
    jp      page_swap_two         ; Map pages into banks 1 and 3, saving original pages
    jp      page_write_byte       ; Write Byte to Page
    jp      page_write_word       ; Write Word to Page
    jp      page_write_bytes      ; Write Bytes to Page
    jp      just_ret
    jp      just_ret

_end_jump_table: