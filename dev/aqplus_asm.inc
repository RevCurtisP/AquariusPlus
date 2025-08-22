;Aquarius+ Kernel Routines

asm_aux_call               equ $2100 ; 2100 Call routine in auxillary ROM
asm_ext_call               equ $2103 ; 2103 Call routine in extended ROM
asm_gfx_call               equ $2106 ; 2106 Call graphics routine
asm_str_copy               equ $2109 ; 2109 Copy null-terminated string
asm_str_length             equ $210C ; 210C Get null-terminated string length
asm_print_c_string         equ $210F ; 210F Print null-terminated string
asm_print_string_immd      equ $2112 ; 2112 Print inline null terminated string
asm_str_stringdesc         equ $2115 ; 2115 Build descriptor for null terminated string
asm_str_tempdesc           equ $2118 ; 2118 Build descriptor for null terminated string in DSCTMP
asm_uppercase_char         equ $211B ; 211B Convert character to lowercase
asm_lowercase_char         equ $211E ; 211E Convert character to uppercase
asm_sys_fill_mem           equ $2121 ; 2121 Fill main memory with byte
asm_sys_swap_mem           equ $2124 ; 2124 Swap bytes
asm_key_clear_fifo         equ $2127 ; 2127 Clear alternate keyboard buffer
asm_key_read               equ $212A ; 212A Read from alternate keyboard buffer
asm_key_set_keymode        equ $2130 ; 2130 Set alternate keyboard buffer mode
asm_page_call              equ $2133 ; 2133 Call subroutine in paged memory

asm_esp_cmd                equ $213C ; 213C Issue command
asm_esp_cmd_string         equ $213F ; 213F Issue ESP command with string argument
asm_esp_get_result         equ $2142 ; 2142 Get first ESP result byte
asm_esp_close_all          equ $2145 ; 2145 Close all files and directories
asm_esp_get_byte           equ $2148 ; 2148 Get byte into A
asm_esp_get_bc             equ $214B ; 214B Get word into BC
asm_esp_get_de             equ $214E ; 214E Get word into DE
asm_esp_get_long           equ $2151 ; 2151 Get long into BC,DE
asm_esp_get_datetime       equ $2154 ; 2154 Read date and time into string buffer
asm_esp_get_mouse          equ $2157 ; 2157 Read mouse position
asm_esp_load_fpga          equ $215D ; 215D Load FPGA Core
asm_esp_read_buff          equ $2160 ; 2160 Read bytes to string buffer
asm_esp_read_byte          equ $2163 ; 2163 Read byte from ESP
asm_esp_read_bytes         equ $2166 ; 2166 Read bytes to main memory
asm_esp_read_line          equ $2169 ; 2169 Read line from text file
asm_esp_read_paged         equ $216C ; 216C Read bytes to paged memory
asm_esp_send_byte          equ $216F ; 216F Send byte in A
asm_esp_send_bc            equ $2172 ; 2172 Send word in BC
asm_esp_send_de            equ $2175 ; 2175 Send word in DE
asm_esp_send_long          equ $2178 ; 2178 Send long in BC,DE
asm_esp_send_bytes         equ $217B ; 217B Send bytes from main memory
asm_esp_send_string        equ $217E ; 217E Send String from main memory
asm_esp_send_strdesc       equ $2181 ; 2181 Send BASIC string from main memory
asm_esp_set_keymode        equ $2184 ; 2184 Set alternate keyboard port mode
asm_esp_write_byte         equ $2187 ; 2187 ** Not implemented **
asm_esp_write_bytes        equ $218A ; 218A Write bytes from main memory
asm_esp_write_repbyte      equ $218D ; 218D Write byte repeatedly
asm_esp_write_paged        equ $2190 ; 2190 Write bytes from paged memory

asm_page_check_next4read   equ $219C ; 219C Verify next page is valid for read
asm_page_check_next4write  equ $219F ; 219F Verify next page is valid for write
asm_page_check_read        equ $21A2 ; 21A2 Verify page is valid for Read
asm_page_check_write       equ $21A5 ; 21A5 Verify page is valid for Write
asm_page_coerce_de_addr    equ $21A8 ; 21A8 Coerce address in to bank 3
asm_page_coerce_hl_addr    equ $21AB ; 21AB Coerce address in to bank 1
asm_page_copy_bytes        equ $21AE ; 21AE Copy Bytes from Page to another Page
asm_page_fast_copy         equ $21B1 ; 21B1 Copy bytes between pages with no rollover
asm_page_fast_read_bytes   equ $21B4 ; 21B4 Read bytes from page with no rollover
asm_page_fast_write_bytes  equ $21B7 ; 21B7 Write bytes to page with no rollover
asm_page_fill_all_byte     equ $21BA ; 21BA Fill entire page with byte
asm_page_fill_all_word     equ $21BD ; 21BD Fill entire page with word
asm_page_fill_byte         equ $21C0 ; 21C0 Fill paged memory with byte
asm_page_fill_word         equ $21C3 ; 21C3 Fill paged memory with word
asm_page_full_copy         equ $21C6 ; 21C6 Copy entire page to another page
asm_page_inc_de_addr       equ $21C9 ; 21C9 Increment Page 3 Write Address
asm_page_inc_hl_addr       equ $21CC ; 21CC Increment Page 1 Write Address
asm_page_map_auxrom        equ $21CF ; 21CF Map Auxillary ROM into bank 3, saving original page
asm_page_map_extrom        equ $21D2 ; 21D2 Map Extended ROM into bank 3, saving original page
asm_page_map_bank1         equ $21D5 ; 21D5 Map page into bank 1, saving original page
asm_page_map_bank3         equ $21D8 ; 21D8 Map page into bank 3, saving original page
asm_page_map_vidram        equ $21DB ; 21DB Map Video RAM into bank 1, saving original page
asm_page_mem_compare       equ $21DE ; 21DE Compare main memory with paged memory
asm_page_mem_swap_bytes    equ $21E1 ; 21E1 Swap paged memory with main memory
asm_page_next_bank1        equ $21E4 ; 21E4 Map next Page into Bank 1
asm_page_next_bank3        equ $21E7 ; 21E7 Map next Page into Bank 3
asm_page_next_de_address   equ $21EA ; 21EA Map next Page into Bank 3 and coerce address
asm_page_next_hl_address   equ $21ED ; 21ED Map next Page into Bank 1 and coerce address
asm_page_page_compare      equ $21F0 ; 21F0 Compare paged memory with paged memory
asm_page_read_byte         equ $21F3 ; 21F3 Read Byte from Page
asm_page_read_bytes        equ $21F6 ; 21F6 Read bytes from paged memory to main memory
asm_page_read_word         equ $21F9 ; 21F9 Read Word from Page
asm_page_restore_bank1     equ $21FC ; 21FC Restore original page to bank 1 and return
asm_page_restore_bank3     equ $21FF ; 21FF Restore original page to bank 3 and return
asm_page_restore_two       equ $2202 ; 2202 Restore original pages to banks 1 and 3
asm_page_set_for_read      equ $2205 ; 2205 Map Page into Bank 3 for write
asm_page_set4read_coerce   equ $2208 ; 2208 Map Page into Bank 3 and coerce address to bank 3       ;
asm_page_set_for_write     equ $220B ; 220B Map Page into Bank 3 for write
asm_page_set4write_coerce  equ $220E ; 220E Map Page into valid Bank 3 and coerce address to bank 3
asm_page_set_aux           equ $2211 ; 2211 Set bank 3 to Auxillary ROM page
asm_page_set_basbuf        equ $2214 ; 2214 Set bank 3 to BASIC buffer RAM page
asm_page_set_plus          equ $2217 ; 2217 Set bank 3 to Extended ROM page
asm_page_swap_two          equ $221A ; 221A Map pages into banks 1 and 3, saving original pages
asm_page_write_byte        equ $221D ; 221D Write Byte to Page
asm_page_write_word        equ $2220 ; 2220 Write Word to Page
asm_page_write_bytes       equ $2223 ; 2223 Write Bytes to Page

asm_get_linbuf_addr        equ $C200 ; Get Line Buffer Address
asm_get_strbuf_addr        equ $C203 ; Get String Buffer Address
asm_FLOAT_BC               equ $C206 ; Convert BC to unsigned float in FACC
asm_FLOAT_DE               equ $C209 ; Convert DE to unsigned float in FACC
asm_FLOAT_CDE              equ $C20C ; Convert CDE to unsigned float in FACC

; *****************
; * Auxiliary ROM *
; * LD   IY,label *
; * CALL aux_call *
; *****************

asm_dos_change_dir         equ $C000 ; Change Directory
asm_dos_close              equ $C003 ; Close file or directory
asm_dos_close_all          equ $C006 ; Close all files
asm_dos_create_dir         equ $C009 ; Create directory
asm_dos_delete             equ $C00C ; Delete file/directory
asm_dos_stat               equ $C00F ; Return File Status
asm_dos_get_cwd            equ $C012 ; Get Current Directory
asm_dos_open               equ $C015 ; Open file
asm_dos_open_append        equ $C018 ; Open file for write
asm_dos_open_dir           equ $C01B ; Open directory for read
asm_dos_open_read          equ $C01E ; Open file for read
asm_dos_open_write         equ $C021 ; Open file for write
asm_dos_read_dir           equ $C024 ; Read directory entry
asm_dos_rename             equ $C027 ; Delete file/directory
asm_dos_rewind             equ $C02A ; Move to beginning of file
asm_dos_seek               equ $C02D ; Move to position in file
asm_dos_tell               equ $C030 ; Get current position in file

asm_file_get_dir           equ $C033 ; Extract path from filespec
asm_file_get_ext           equ $C036 ; Extract extension from filespec
asm_file_trim_dir          equ $C03C ; Trim path from filespec
asm_file_trim_ext          equ $C03F ; Trim extension from filespec
asm_file_load_binary       equ $C042 ; Load binary file into main memory
asm_file_load_chrset       equ $C045 ; Load file into alternate chrset buffer
asm_file_load_paged        equ $C048 ; Load binary file into paged memory
asm_file_load_palette      equ $C04B ; Load and set palette
asm_file_load_pt3          equ $C04E ; Load PT3 file into PT3 buffer
asm_file_load_rom          equ $C051 ; Load ROM file into page 35
asm_file_load_screen       equ $C054 ; Load screen image
asm_file_load_strbuf       equ $C057 ; Load file into BASIC string buffer
asm_file_save_binary       equ $C060 ; Save binary file from main memory
asm_file_save_paged        equ $C063 ; Save file from paged memory
asm_file_save_palette      equ $C066 ; Get and save palette
asm_file_save_screen       equ $C069 ; Save screen image
asm_file_save_strbuf       equ $C06C ; Save BASIC string buffer to file
asm_file_load_defchrs      equ $C06F ; Load file into default chrset buffer
asm_file_load_tilemap      equ $C072 ; Load tilemap
asm_file_save_tilemap      equ $C075 ; Save tilemap
asm_file_save_chrset       equ $C078 ;
asm_file_load_tileset      equ $C07B ; Load tiles
asm_file_save_tileset      equ $C07E ; Save tiles
asm_file_read_dir          equ $C081 ; Read directory entry as ASCII string

asm_string_trim            equ $C090 
asm_string_trim_left       equ $C093 
asm_string_trim_right      equ $C096 
asm_string_search          equ $C099 
asm_string_search_array    equ $C09C 

asm_basbuf_read_byte       equ $C0A8 
asm_basbuf_read_bytes      equ $C0AB 
asm_basbuf_read_word       equ $C0AE 
asm_basbuf_write_byte      equ $C0B1 
asm_basbuf_write_bytes     equ $C0B4 
asm_basbuf_write_word      equ $C0B7 
asm_runarg_count           equ $C0BA 
asm_runarg_get             equ $C0BD 

asm_pause_jiffies          equ $C0CC 
asm_read_gamepad           equ $C0CF ; Read gamepad(s)
asm_get_gamectrl           equ $C0D2 ; Read full Xbox controller status
asm_key_pressed            equ $C0D5 ; Check key matrix for key press

asm_play_sample            equ $C0EA ; Play digitized sound sample

asm_get_plusbas_version    equ $C102 ; Return plusBASIC version string
asm_get_system_version     equ $C105 ; Read system version string from ESP
asm_version_to_long        equ $C108 ; Convert string version to long int

; *****************
; * Graphics ROM  *
; * LD   IY,label *
; * CALL gfx_call *
; *****************

asm_bitmap_init_vars       equ $C000 ; C000 Initialize bitmap system variables
asm_bitmap_set_mode        equ $C003 ; C003 Set bitmap mode system variable from video control register
asm_bitmap_read_sysvars    equ $C006 ; C006 Read bitmap system variables
asm_bitmap_read_color      equ $C009 ; C009 Get Bitmap Draw Colors
asm_bitmap_write_color     equ $C00C ; C00C Set Bitmap Draw Colors
asm_bitmap_clear_screen    equ $C00F ; C00F Clear Bitmap
asm_bitmap_fill_byte       equ $C012 ; C012 Fill Bitmap with Byte
asm_bitmap_fill_color      equ $C015 ; C015 Fill Bitmap Color RAM
asm_bitmap_line            equ $C018 ; C018 [Future]
asm_bitmap_setpixel        equ $C02D ; C02D Draw pixel
asm_bitmap_resetpixel      equ $C030 ; C030 Erase pixel on 1 bpp bitmap screen
asm_bitmap_getpixel        equ $C033 ; C033 Return pixel/bloxel at position
asm_bitmap_get             equ $C036 ; C036 [future] Read Bitmap Screen Section into Buffer
asm_colormap_fill          equ $C045 ; C045 Fill 1bpp Color Map Rectangle with Byte

asm_palette_reset          equ $C054 ; C054 Reset palette to default colors
asm_palette_set            equ $C057 ; C057 palette_set
asm_palette_get            equ $C05A ; C05A Get palette
asm_rgb_to_asc             equ $C060 ; C060 Convert Binary RGB list to ASCII
asm_asc_to_rgb             equ $C063 ; C063 Convert ASCII RRGGBB to binary GB0R

asm_screen_clear_color     equ $C078 ; C078 Fill Color RAM with current/default colors
asm_screen_read_byte       equ $C084 ; C084 Read byte from screen
asm_screen_write_byte      equ $C087 ; C087 Write byte to screen
asm_screen_read_string     equ $C08A ; C08A Read string from Screen RAM
asm_screen_write_string    equ $C08D ; C08D Write string to Screen RAM
asm_screen_read_paged      equ $C096 ; C096
asm_screen_write_paged     equ $C099 ; C099
asm_screen_read_fast       equ $C09C ; C09C
asm_screen_write_fast      equ $C09F ; C09F
asm_oolor_read_byte        equ $C0A8 ; C0A8 Read byte from Color RAM
asm_color_write_byte       equ $C0AB ; C0AB Write byte to Color RAM
asm_color_read_string      equ $C0AE ; C0AE Read string from Color RAM
asm_color_write_string     equ $C0B1 ; C0B1 Write string to Color RAM
asm_screen_clear_cursor    equ $C0C0 ; C0C0 Remove cursor from screen
asm_screen_invert_color    equ $C0C6 ; C0C6
asm_screen_invert_cursor   equ $C0C9 ; C0C9
asm_screen_get             equ $C0D2 ; C0D2 Read Text Screen Section into Buffer
asm_screen_put             equ $C0D5 ; C0D5 Write Text Screen Section from Buffer
asm_screen_fill            equ $C0D8 ; C0D8 Fill Text or Color Screen Rectangle with Byte

asm_init_screen_buffers    equ $C0E4 ; C0E4
asm_init_screen_vars       equ $C0E7 ; C0E7
asm_screen_reset           equ $C0F0 ; C0F0
asm_screen_status          equ $C0F3 ; C0F3
asm_screen_restore         equ $C0FC ; C0FC
asm_screen_stash           equ $C0FF ; C0FF
asm_screen_swap            equ $C102 ; C102
asm_screen_switch          equ $C105 ; C105

asm_tile_set               equ $C10E ; C10E
asm_tile_get               equ $C111 ; C111
asm_tile_from_chrrom       equ $C114 ; C114
asm_tile_combine_props     equ $C117 ; C117
asm_tilemap_set_offset     equ $C120 ; C120
asm_tilemap_get_offset     equ $C123 ; C123
asm_tilemap_set_tile       equ $C126 ; C126
asm_tilemap_get_tile       equ $C129 ; C129
asm_tilemap_fill           equ $C12C ; C12C
asm_tilemap_get            equ $C12F ; C12F
asm_tilemap_put            equ $C132 ; C132
asm_sprite_define          equ $C141 ; C141
asm_sprite_defrect         equ $C144 ; C144
asm_sprite_set_attrs       equ $C147 ; C147
asm_sprite_get_attrs       equ $C14A ; C14A
asm_spritle_set_pos        equ $C14D ; C14D
asm_spritle_get_pos        equ $C150 ; C150
asm_spritle_string_attrs   equ $C153 ; C153
asm_spritle_reset          equ $C156 ; C156
asm_spritle_reset_all      equ $C159 ; C159
asm_file_load_altchrs      equ $C43F 
asm_page_fast_read_bytes   equ $C471 
asm_buffer_write_bytes     equ $C481 
asm_basbuf_write_byte      equ $C4B0 
asm_nz,FCERR               equ $C560 ;     Illegal quantity error
asm_key_clear_fifo         equ $C5A0 ; Clear keyboard buffer and return
asm_c,FCERR                equ $C5EF ;   Error if invalid keycode
asm_z,FCERR                equ $C5F6 ; Error if NULL string
asm_SNERR                  equ $C629 
asm_basbuf_write_byte      equ $C658 
asm_c,FCERR                equ $C660 ;   Illegal Quantity error
asm_c,FCERR                equ $C666 ;   Illegal Quantity error
asm_z,TMERR                equ $C674 ; No Strings (for now)
asm_nz,FCERR               equ $C679 ;   Illegal Quantity Error
asm_c,FCERR                equ $C68A 
asm_nz,TMERR               equ $C690 
asm_nz,FCERR               equ $C6A8 ;   Error
asm_z,FCERR                equ $C6AC ;   Error
asm_nz,TMERR               equ $C6DA ;       Type mismatch error
asm_c,LSERR                equ $C736 ;     String to long error
asm_z,aux_split_fill       equ $C776 ; If null string, pop TxtPtr and return
asm_sys_fill_zero          equ $C806 ; Fill to end with 0 bytes
asm_c,discard2ret          equ $C85D ; If error, clean stack and return Carry set
asm_STR                    equ $C8A1 ;   Execute S3BASIC STR$ function
asm_nz,SNERR               equ $C8CD ;     Syntax error
asm_nz,LSERR               equ $C8DA ; IF BC/4 is 255, String too long error
asm_nz,LSERR               equ $C8E8 ;   String too long error
asm_PUTNEW                 equ $C8FB ; Else return the string
asm_esp_writec_bytes       equ $C958 ; Write and return
asm_m,.string              equ $C99A ; If float
asm_z,.string              equ $C9DE ; If float
asm_CRDO                   equ $C9F1 ; Newline
asm__out_space             equ $CB36 
asm__out_buff              equ $CBB2 
asm_esp_writec_bytes       equ $CC29 
asm_esp_cmd_string         equ $CC87 ; Issue ESP command
asm_esp_get_result         equ $CC8F 
asm_esp_get_result         equ $CCA3 
asm_esp_cmd_string         equ $CCA8 ; Issue ESP command
asm_esp_cmd_string         equ $CCAD ; Issue ESP command
asm_esp_get_result         equ $CCC6 ; Return A = Result
asm_espx_read_buff         equ $CCD2 ; Read into StrBuf and Return
asm_esp_get_result         equ $CD52 
asm_espx_read_to_buff      equ $CD75 
asm_page_restore_bank3     equ $CD99 
asm_page_restore_bank3     equ $CDD9 
asm_page_restore_bank3     equ $CDFF 
asm_nz,SNERR               equ $CE32 
asm_(ix)                   equ $D00F ; Return
asm_gfx_call               equ $D02F 
asm_page_copy_bytes_sys    equ $D065 ; Copy and return
asm_file_load_buffer       equ $D075 
asm_xfile_load_paged       equ $D0CD 
asm_page_restore_bank1     equ $D103 
asm__load_read_gfx         equ $D10A 
asm_nz,ret_carry_set       equ $D153 ;   Bad file error
asm_gfx_call               equ $D15C 
asm__load_read_gfx         equ $D17F 
asm_gfx_call               equ $D18C 
asm_shift_hl_left          equ $D197 ; TilAdr = TileNo * 32
asm_m,aux_ret_zero         equ $D19D ; If no error
asm_run_exec               equ $D2AA ; Run the program
asm__close                 equ $D2D1 
asm_file_save_tmpbuffr     equ $D2D9 ; Save bitmap data to file
asm_m,discard_ret          equ $D2EA 
asm_file_save_paged        equ $D332 
asm_c,POPHRT               equ $D347 ; If TileNo > 511 Return Carry Set
asm_file_save_paged        equ $D35F 
asm_(ix)                   equ $D3A8 ; Return
asm_nz,FCERR               equ $D3DC 
asm_z,decode_game_joystk   equ $D3EF ;     Decode joystick direction
asm_z,decode_game_button   equ $D3F4 ;     Decode button
asm_z,decode_game_dpad     equ $D3FF ;     Decode dpad direction
asm_SNERR                  equ $D40E ;   Else Syntax error
asm_z,ESERR                equ $D438 ;   Empty string error
asm_nz,SLERR               equ $D43D ;   String Length Error
asm_SNERR                  equ $D455 
asm_SNERR                  equ $D46B 
asm_SNERR                  equ $D481 
asm_SNERR                  equ $D4EC 
asm_SNERR                  equ $D512 
asm_SNERR                  equ $D51D 
asm_espx_get_gamectrl      equ $D52A ; Read comtroller and return
asm_m,_load_array_error    equ $D539 ;   Return error
asm_m,_load_array_error    equ $D53F ;   Return error
asm__load_array_done       equ $D548 
asm_m,_load_array_error    equ $D557 
asm_z,_load_array_done     equ $D579 
asm_m,.error               equ $D5A4 
asm_nz,_load_asc_ary_error equ $D5D2 
asm__asc_array             equ $D61A 
asm__asc_array             equ $D627 
asm__asc_array             equ $D634 
asm_c,POPHRT               equ $D6CB ;   Return Carry Set
asm_p,.lookup              equ $D787 
asm_z,CHRGTR               equ $D7E6 ;   Return next character with flags set
asm_m,.extended            equ $D819 
asm_z,_strng               equ $D901 ; Do the same for Single Quote
asm_nz,_stuff_label        equ $D90A ;   See if it's a label
asm_c,_strngr              equ $D932 ;   Finish CRUNCH
asm_z,.done                equ $D9AF ; 10  35
asm_nz,.notfound           equ $DA10 ;    Return 0
asm_z,.found               equ $DA15 ; If not end of NdlStr
asm_nz,.ofs_loop           equ $DA1A ;   If HayCnt <> 0, check next character
asm_nz,.cnt_loop           equ $DA31 ; If not at end of NdlTxt, check next character
asm_z,discard_ret          equ $DA3B ; Return if -1
asm_.loop                  equ $DA68 
asm_.trim                  equ $DA88 
asm_.trim                  equ $DAB7 
asm_c,.padleft             equ $DAC9 ; If BC >0
asm_esp_read_buff          equ $DB4F 
asm_page_read_bytes        equ $DBFD ; Write to Character RAM and return
asm_page_write_bytes       equ $DC03 ; Write to Character RAM and return
asm_SNERR                  equ $DC62 ; Else Syntax error
asm_nc,FCERR               equ $DC6A ;     Illegal Quantity error
asm_spritle_reset          equ $DC6D ;   Reset spritle
asm_z,spritle_reset_all    equ $DC72 ;   Reset all spritles
asm_z,ESERR                equ $DC7A ; If StrLen = 0, Empty String Error
asm_c,SLERR                equ $DC80 
asm_z,ESERR                equ $DC8A ; If StrLen = 0, Empty string error
asm_nz,SLERR               equ $DC9F ;     String length error
asm_c,SLERR                equ $DCBE ; Error if string too long
asm_c,FCERR                equ $DCC5 ; Error if not "rrr ggg bbb"
asm_c,FCERR                equ $DCD0 
asm_nz,SLERR               equ $DCEC ;   Strnig length error
asm_c,FCERR                equ $DD19 
asm_(ix)                   equ $DD1E ; Return
asm_c,LSERR                equ $DD2C ; Error if too long
asm_nz,OVERR               equ $DD3D ; Sprite and Buffer Size Mismtch
asm_bitmap_put_char        equ $DD5A ; Put char to bitmap and return
asm_gfx_get_char_def       equ $DD69 
asm_nc,FCERR               equ $DD7E ; Error if > 3
asm_FCERR                  equ $DDD0 
asm_nz,TMERR               equ $DDFE ; Error if not string array
asm_byte_to_hex            equ $DED0 
asm_c,discard_ret          equ $DF44 
asm_c,discard_ret          equ $DF50 
asm_z,sys_fill_mem_d       equ $E06E 
asm_z,sys_fill_mem_d       equ $E075 
asm_z,sys_fill_mem_d       equ $E0BC 
asm_page_restore_bank1     equ $E101 
asm_nz,ret_carry_set       equ $E301 
asm_copy_tmpbfr_vidram     equ $E349 
asm_page_restore_bank1_af  equ $E358 
asm_nz,ret_carry_set       equ $E361 
asm_page_fast_copy         equ $E374 
asm_page_fast_copy         equ $E3B1 
asm_page_restore_bank1_af  equ $E3BF 
asm_page_skip_write        equ $E3D8 
asm_TTYFIS                 equ $E435 ; Save cursor position and return
asm_sys_fill_mem           equ $E46C 
asm_z,jump_iy              equ $E4A2 ; If 40-columns, do it an return
asm_c,discard3ret          equ $E57B ; If overflow, clean stack and return carry
asm_z,discard3ret          equ $E57E ; If null string, pop RW flag and return
asm_nz,.write80            equ $E593 ; If 40cols
asm_c,discard3ret          equ $E5B1 ; If overflow, clean stack and return carry
asm_z,discard3ret          equ $E5B4 ; If null string, pop RW flag and return
asm_(ix)                   equ $E6A8 
asm_(ix)                   equ $E726 
asm_page_restore_bank1     equ $E7E8 
asm_page_restore_bank1     equ $E7FC 
asm_palette_reset          equ $E83D 
asm_z,page_restore_bank1   equ $E890 ; If 80-columns
asm_z,page_restore_bank1   equ $E8C4 ; If 80-columns
asm_page_restore_bank1     equ $E8D9 ;   Restore page and return
asm_z,page_restore_bank1   equ $E8ED ; If 80-columns
asm_set_linlen             equ $E924 
asm_page_restore_bank1     equ $EA2E 
asm_page_restore_bank1     equ $EAF3 
asm_page_map_bank1         equ $EB29 
asm_select_chrset          equ $EB31 
asm_(ix)                   equ $EC24 
asm_c,.loop                equ $EC5D 
asm_  page_read_bytes      equ $EC9E ; Read data and return
asm_page_restore_bank1     equ $ECE7 ; Restore original page and return
asm_page_write_word        equ $ED26 ; Write to Video RAM and return
asm_page_read_word         equ $ED2F ; Read word and return
asm_copy_tmpbase_vidbase   equ $EDBB ;   Copy from bottom of TMP_BUFFR to bottom of VIDEO_RAM
asm_copy_tmpbase_vidram    equ $EDF4 ; Copy tiles and return
