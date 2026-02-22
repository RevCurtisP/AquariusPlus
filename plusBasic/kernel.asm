;=====================================================================================
; Kernel jump table - Starts at $2100
;=====================================================================================
_jump_table:
; <<System Routines>>
    jp      aux_call              ; 2100 Call routine in auxillary ROM
    jp      ext_call              ; 2103 Call routine in extended ROM
    jp      gfx__call             ; 2106 Call graphics routine
    jp      str_copy              ; 2109 Copy null-terminated string
    jp      str_length            ; 210C Get null-terminated string length
    jp      print_c_string        ; 210F Print null-terminated string
    jp      print_string_immd     ; 2112 Print inline null terminated string
    jp      str_stringdesc        ; 2115 Build descriptor for null terminated string
    jp      str_tempdesc          ; 2118 Build descriptor for null terminated string in DSCTMP
    jp      uppercase_char        ; 211B Convert character to lowercase
    jp      lowercase_char        ; 211E Convert character to uppercase
    jp      sys_fill_mem          ; 2121 Fill main memory with byte
    jp      sys_swap_mem          ; 2124 Swap bytes
    jp      key_clear_fifo        ; 2127 Clear alternate keyboard buffer
    jp      key_read              ; 212A Read from alternate keyboard buffer
    jp      just_ret              ; 212D
    jp      key_set_keymode       ; 2130 Set alternate keyboard buffer mode
    jp      page_call             ; 2133 Call subroutine in paged memory
    jp      irq_done              ; 2136 Exit user IRQ routine
    jp      just_ret              ; 2139

; esp.asm <<ESP Interface>>
    jp      esp_cmd               ; 213C Issue command
    jp      esp_cmd_string        ; 213F Issue ESP command with string argument
    jp      esp_get_result        ; 2142 Get first ESP result byte
    jp      esp_close_all         ; 2145 Close all files and directories
    jp      esp_get_byte          ; 2148 Get byte into A
    jp      esp_get_bc            ; 214B Get word into BC
    jp      esp_get_de            ; 214E Get word into DE
    jp      esp_get_long          ; 2151 Get long into BC,DE
    jp      esp_get_datetime      ; 2154 Read date and time into string buffer
    jp      esp_get_mouse         ; 2157 Read mouse position
    jp      just_ret              ; 215A
    jp      esp_load_fpga         ; 215D Load FPGA Core
    jp      esp_read_buff         ; 2160 Read bytes to string buffer
    jp      esp_read_byte         ; 2163 Read byte from ESP
    jp      esp_read_bytes        ; 2166 Read bytes to main memory
    jp      esp_read_line         ; 2169 Read line from text file
    jp      esp_read_paged        ; 216C Read bytes to paged memory
    jp      esp_send_byte         ; 216F Send byte in A
    jp      esp_send_bc           ; 2172 Send word in BC
    jp      esp_send_de           ; 2175 Send word in DE
    jp      esp_send_long         ; 2178 Send long in BC,DE
    jp      esp_send_bytes        ; 217B Send bytes from main memory
    jp      esp_send_string       ; 217E Send String from main memory
    jp      esp_send_strdesc      ; 2181 Send BASIC string from main memory
    jp      esp_set_keymode       ; 2184 Set alternate keyboard port mode
    jp      esp_write_byte        ; 2187 Write single byte
    jp      esp_write_bytes       ; 218A Write bytes from main memory
    jp      esp_write_repbyte     ; 218D Write byte repeatedly
    jp      esp_write_paged       ; 2190 Write bytes from paged memory
    jp      just_ret              ; 2193
    jp      just_ret              ; 2196
    jp      just_ret              ; 2199

; page.asm <<Paged Memory>>
    jp      page_check_next4read  ; 219C Verify next page is valid for read
    jp      page_check_next4write ; 219F Verify next page is valid for write
    jp      page_check_read       ; 21A2 Verify page is valid for Read
    jp      page_check_write      ; 21A5 Verify page is valid for Write
    jp      page_coerce_de_addr   ; 21A8 Coerce address in to bank 3
    jp      page_coerce_hl_addr   ; 21AB Coerce address in to bank 1
    jp      page_copy_bytes       ; 21AE Copy Bytes from Page to another Page
    jp      page_fast_copy        ; 21B1 Copy bytes between pages with no rollover
    jp      page_fast_read_bytes  ; 21B4 Read bytes from page with no rollover
    jp      page_fast_write_bytes ; 21B7 Write bytes to page with no rollover
    jp      page_fill_all_byte    ; 21BA Fill entire page with byte
    jp      page_fill_all_word    ; 21BD Fill entire page with word
    jp      page_fill_byte        ; 21C0 Fill paged memory with byte
    jp      page_fill_word        ; 21C3 Fill paged memory with word
    jp      page_full_copy        ; 21C6 Copy entire page to another page
    jp      page_inc_de_addr      ; 21C9 Increment Page 3 Write Address
    jp      page_inc_hl_addr      ; 21CC Increment Page 1 Write Address
    jp      page_map_auxrom       ; 21CF Map Auxillary ROM into bank 3, saving original page
    jp      page_map_extrom       ; 21D2 Map Extended ROM into bank 3, saving original page
    jp      page_map_bank1        ; 21D5 Map page into bank 1, saving original page
    jp      page_map_bank3        ; 21D8 Map page into bank 3, saving original page
    jp      page_map_vidram       ; 21DB Map Video RAM into bank 1, saving original page
    jp      page_mem_compare      ; 21DE Compare main memory with paged memory
    jp      page_mem_swap_bytes   ; 21E1 Swap paged memory with main memory
    jp      page_next_bank1       ; 21E4 Map next Page into Bank 1
    jp      page_next_bank3       ; 21E7 Map next Page into Bank 3
    jp      page_next_de_address  ; 21EA Map next Page into Bank 3 and coerce address
    jp      page_next_hl_address  ; 21ED Map next Page into Bank 1 and coerce address
    jp      page_page_compare     ; 21F0 Compare paged memory with paged memory
    jp      page_read_byte        ; 21F3 Read Byte from Page
    jp      page_read_bytes       ; 21F6 Read bytes from paged memory to main memory
    jp      page_read_word        ; 21F9 Read Word from Page
    jp      page_restore_bank1    ; 21FC Restore original page to bank 1 and return
    jp      page_restore_bank3    ; 21FF Restore original page to bank 3 and return
    jp      page_restore_two      ; 2202 Restore original pages to banks 1 and 3
    jp      page_set_for_read     ; 2205 Map Page into Bank 3 for write
    jp      page_set4read_coerce  ; 2208 Map Page into Bank 3 and coerce address to bank 3       ;
    jp      page_set_for_write    ; 220B Map Page into Bank 3 for write
    jp      page_set4write_coerce ; 220E Map Page into valid Bank 3 and coerce address to bank 3
    jp      page_set_aux          ; 2211 Set bank 3 to Auxillary ROM page
    jp      page_set_basbuf       ; 2214 Set bank 3 to BASIC buffer RAM page
    jp      page_set_plus         ; 2217 Set bank 3 to Extended ROM page
    jp      page_swap_two         ; 221A Map pages into banks 1 and 3, saving original pages
    jp      page_write_byte       ; 221D Write Byte to Page
    jp      page_write_word       ; 2220 Write Word to Page
    jp      page_write_bytes      ; 2223 Write Bytes to Page
    jp      just_ret              ; 2226
    jp      just_ret              ; 2229

; buffer.asm <<BASIC Buffers>>
    jp      buffer_read_byte      ; 222C Read byte from BASIC buffer
    jp      buffer_read_bytes     ; 222F Read bytes from BASIC buffer
    jp      buffer_read_word      ; 2232 Read word from BASIC buffer
    jp      buffer_write_byte     ; 2235 Write byte to BASIC buffer
    jp      buffer_write_bytes    ; 2238 Write bytes to BASIC buffer
    jp      buffer_write_word     ; 223B Write word to BASIC buffer
    jp      basbuf_read_byte      ; 223E Read byte from BASIC buffer
    jp      basbuf_read_bytes     ; 2241 Read bytes from BASIC buffer
    jp      basbuf_read_word      ; 2244 Read word from BASIC buffer
    jp      basbuf_write_byte     ; 2247 Write byte to BASIC buffer
    jp      basbuf_write_bytes    ; 224A Write bytes to BASIC buffer
    jp      basbuf_write_word     ; 224D Write word to BASIC buffer
    jp      just_ret              ; 2250
    jp      just_ret              ; 2253

; <<Utility routines>>
    jp      get_linbuf_addr       ; 2256 Get Line Buffer Address
    jp      get_strbuf_addr       ; 2259 Get String Buffer Address

_end_jump_table:
