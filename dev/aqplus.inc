;Aquarius+ Kernel Routines

; System Routines
aux_call               equ $2100 ; 2100 Call routine in auxillary ROM
ext_call               equ $2103 ; 2103 Call routine in extended ROM
gfx_call               equ $2106 ; 2106 Call graphics routine
str_copy               equ $2109 ; 2109 Copy null-terminated string
str_length             equ $210C ; 210C Get null-terminated string length
print_c_string         equ $210F ; 210F Print null-terminated string
print_string_immd      equ $2112 ; 2112 Print inline null terminated string
str_stringdesc         equ $2115 ; 2115 Build descriptor for null terminated string
str_tempdesc           equ $2118 ; 2118 Build descriptor for null terminated string in DSCTMP
uppercase_char         equ $211B ; 211B Convert character to lowercase
lowercase_char         equ $211E ; 211E Convert character to uppercase
sys_fill_mem           equ $2121 ; 2121 Fill main memory with byte
sys_swap_mem           equ $2124 ; 2124 Swap bytes
key_clear_fifo         equ $2127 ; 2127 Clear alternate keyboard buffer
key_read               equ $212A ; 212A Read from alternate keyboard buffer
key_set_keymode        equ $2130 ; 2130 Set alternate keyboard buffer mode
page_call              equ $2133 ; 2133 Call subroutine in paged memory

; ESP Interface
esp_cmd                equ $213C ; 213C Issue command
esp_cmd_string         equ $213F ; 213F Issue ESP command with string argument
esp_get_result         equ $2142 ; 2142 Get first ESP result byte
esp_close_all          equ $2145 ; 2145 Close all files and directories
esp_get_byte           equ $2148 ; 2148 Get byte into A
esp_get_bc             equ $214B ; 214B Get word into BC
esp_get_de             equ $214E ; 214E Get word into DE
esp_get_long           equ $2151 ; 2151 Get long into BC,DE
esp_get_datetime       equ $2154 ; 2154 Read date and time into string buffer
esp_get_mouse          equ $2157 ; 2157 Read mouse position
esp_load_fpga          equ $215D ; 215D Load FPGA Core
esp_read_buff          equ $2160 ; 2160 Read bytes to string buffer
esp_read_byte          equ $2163 ; 2163 Read byte from ESP
esp_read_bytes         equ $2166 ; 2166 Read bytes to main memory
esp_read_line          equ $2169 ; 2169 Read line from text file
esp_read_paged         equ $216C ; 216C Read bytes to paged memory
esp_send_byte          equ $216F ; 216F Send byte in A
esp_send_bc            equ $2172 ; 2172 Send word in BC
esp_send_de            equ $2175 ; 2175 Send word in DE
esp_send_long          equ $2178 ; 2178 Send long in BC,DE
esp_send_bytes         equ $217B ; 217B Send bytes from main memory
esp_send_string        equ $217E ; 217E Send String from main memory
esp_send_strdesc       equ $2181 ; 2181 Send BASIC string from main memory
esp_set_keymode        equ $2184 ; 2184 Set alternate keyboard port mode
esp_write_byte         equ $2187 ; 2187 ** Not implemented **
esp_write_bytes        equ $218A ; 218A Write bytes from main memory
esp_write_repbyte      equ $218D ; 218D Write byte repeatedly
esp_write_paged        equ $2190 ; 2190 Write bytes from paged memory

; Paged Memory
page_check_next4read   equ $219C ; 219C Verify next page is valid for read
page_check_next4write  equ $219F ; 219F Verify next page is valid for write
page_check_read        equ $21A2 ; 21A2 Verify page is valid for Read
page_check_write       equ $21A5 ; 21A5 Verify page is valid for Write
page_coerce_de_addr    equ $21A8 ; 21A8 Coerce address in to bank 3
page_coerce_hl_addr    equ $21AB ; 21AB Coerce address in to bank 1
page_copy_bytes        equ $21AE ; 21AE Copy Bytes from Page to another Page
page_fast_copy         equ $21B1 ; 21B1 Copy bytes between pages with no rollover
page_fast_read_bytes   equ $21B4 ; 21B4 Read bytes from page with no rollover
page_fast_write_bytes  equ $21B7 ; 21B7 Write bytes to page with no rollover
page_fill_all_byte     equ $21BA ; 21BA Fill entire page with byte
page_fill_all_word     equ $21BD ; 21BD Fill entire page with word
page_fill_byte         equ $21C0 ; 21C0 Fill paged memory with byte
page_fill_word         equ $21C3 ; 21C3 Fill paged memory with word
page_full_copy         equ $21C6 ; 21C6 Copy entire page to another page
page_inc_de_addr       equ $21C9 ; 21C9 Increment Page 3 Write Address
page_inc_hl_addr       equ $21CC ; 21CC Increment Page 1 Write Address
page_map_auxrom        equ $21CF ; 21CF Map Auxillary ROM into bank 3, saving original page
page_map_extrom        equ $21D2 ; 21D2 Map Extended ROM into bank 3, saving original page
page_map_bank1         equ $21D5 ; 21D5 Map page into bank 1, saving original page
page_map_bank3         equ $21D8 ; 21D8 Map page into bank 3, saving original page
page_map_vidram        equ $21DB ; 21DB Map Video RAM into bank 1, saving original page
page_mem_compare       equ $21DE ; 21DE Compare main memory with paged memory
page_mem_swap_bytes    equ $21E1 ; 21E1 Swap paged memory with main memory
page_next_bank1        equ $21E4 ; 21E4 Map next Page into Bank 1
page_next_bank3        equ $21E7 ; 21E7 Map next Page into Bank 3
page_next_de_address   equ $21EA ; 21EA Map next Page into Bank 3 and coerce address
page_next_hl_address   equ $21ED ; 21ED Map next Page into Bank 1 and coerce address
page_page_compare      equ $21F0 ; 21F0 Compare paged memory with paged memory
page_read_byte         equ $21F3 ; 21F3 Read Byte from Page
page_read_bytes        equ $21F6 ; 21F6 Read bytes from paged memory to main memory
page_read_word         equ $21F9 ; 21F9 Read Word from Page
page_restore_bank1     equ $21FC ; 21FC Restore original page to bank 1 and return
page_restore_bank3     equ $21FF ; 21FF Restore original page to bank 3 and return
page_restore_two       equ $2202 ; 2202 Restore original pages to banks 1 and 3
page_set_for_read      equ $2205 ; 2205 Map Page into Bank 3 for write
page_set4read_coerce   equ $2208 ; 2208 Map Page into Bank 3 and coerce address to bank 3       ;
page_set_for_write     equ $220B ; 220B Map Page into Bank 3 for write
page_set4write_coerce  equ $220E ; 220E Map Page into valid Bank 3 and coerce address to bank 3
page_set_aux           equ $2211 ; 2211 Set bank 3 to Auxillary ROM page
page_set_basbuf        equ $2214 ; 2214 Set bank 3 to BASIC buffer RAM page
page_set_plus          equ $2217 ; 2217 Set bank 3 to Extended ROM page
page_swap_two          equ $221A ; 221A Map pages into banks 1 and 3, saving original pages
page_write_byte        equ $221D ; 221D Write Byte to Page
page_write_word        equ $2220 ; 2220 Write Word to Page
page_write_bytes       equ $2223 ; 2223 Write Bytes to Page

; BASIC Buffers
buffer_read_byte       equ $222C ; 222C Read byte from BASIC buffer
buffer_read_bytes      equ $222F ; 222F Read bytes from BASIC buffer
buffer_read_word       equ $2232 ; 2232 Read word from BASIC buffer
buffer_write_byte      equ $2235 ; 2235 Write byte to BASIC buffer
buffer_write_bytes     equ $2238 ; 2238 Write bytes to BASIC buffer
buffer_write_word      equ $223B ; 223B Write word to BASIC buffer
basbuf_read_byte       equ $223E ; 223E Read byte from BASIC buffer
basbuf_read_bytes      equ $2241 ; 2241 Read bytes from BASIC buffer
basbuf_read_word       equ $2244 ; 2244 Read word from BASIC buffer
basbuf_write_byte      equ $2247 ; 2247 Write byte to BASIC buffer
basbuf_write_bytes     equ $224A ; 224A Write bytes to BASIC buffer
basbuf_write_word      equ $224D ; 224D Write word to BASIC buffer

; Utility routines
get_linbuf_addr        equ $2256 ; 2256 Get Line Buffer Address
get_strbuf_addr        equ $2259 ; 2259 Get String Buffer Address

; ******************************************************
; * Extended ROM                                       *
; * Running from paged memoty in bank 3 ($C000-$FFFF): *
; *   LD   IY,label                                    *
; *   CALL ext_call                                    *
; ******************************************************

; Extended ROM
FLOAT_BC               equ $C206 ; C206 Convert BC to unsigned float in FACC
FLOAT_DE               equ $C209 ; C209 Convert DE to unsigned float in FACC
FLOAT_CDE              equ $C20C ; C20C Convert CDE to unsigned float in FACC

; *****************
; * Auxiliary ROM *
; * LD   IY,label *
; * CALL aux_call *
; *****************

; DOS
dos_change_dir         equ $C000 ; C000 Change Directory
dos_close              equ $C003 ; C003 Close file or directory
dos_close_all          equ $C006 ; C006 Close all files
dos_create_dir         equ $C009 ; C009 Create directory
dos_delete             equ $C00C ; C00C Delete file/directory
dos_stat               equ $C00F ; C00F Return File Status
dos_get_cwd            equ $C012 ; C012 Get Current Directory
dos_open               equ $C015 ; C015 Open file
dos_open_append        equ $C018 ; C018 Open file for write
dos_open_dir           equ $C01B ; C01B Open directory for read
dos_open_read          equ $C01E ; C01E Open file for read
dos_open_write         equ $C021 ; C021 Open file for write
dos_read_dir           equ $C024 ; C024 Read directory entry
dos_rename             equ $C027 ; C027 Delete file/directory
dos_rewind             equ $C02A ; C02A Move to beginning of file
dos_seek               equ $C02D ; C02D Move to position in file
dos_tell               equ $C030 ; C030 Get current position in file

; File I/O
file_get_dir           equ $C033 ; C033 Extract path from filespec
file_get_ext           equ $C036 ; C036 Extract extension from filespec
file_trim_dir          equ $C03C ; C03C Trim path from filespec
file_trim_ext          equ $C03F ; C03F Trim extension from filespec
file_load_binary       equ $C042 ; C042 Load binary file into main memory
file_load_chrset       equ $C045 ; C045 Load file into alternate chrset buffer
file_load_paged        equ $C048 ; C048 Load binary file into paged memory
file_load_palette      equ $C04B ; C04B Load and set palette
file_load_pt3          equ $C04E ; C04E Load PT3 file into PT3 buffer
file_load_rom          equ $C051 ; C051 Load ROM file into page 35
file_load_screen       equ $C054 ; C054 Load screen image
file_load_strbuf       equ $C057 ; C057 Load file into BASIC string buffer
file_save_binary       equ $C060 ; C060 Save binary file from main memory
file_save_paged        equ $C063 ; C063 Save file from paged memory
file_save_palette      equ $C066 ; C066 Get and save palette
file_save_screen       equ $C069 ; C069 Save screen image
file_save_strbuf       equ $C06C ; C06C Save BASIC string buffer to file
file_load_defchrs      equ $C06F ; C06F Load file into default chrset buffer
file_load_tilemap      equ $C072 ; C072 Load tilemap
file_save_tilemap      equ $C075 ; C075 Save tilemap
file_save_chrset       equ $C078 ; C078
file_load_tileset      equ $C07B ; C07B Load tiles
file_save_tileset      equ $C07E ; C07E Save tiles
file_read_dir          equ $C081 ; C081 Read directory entry as ASCII string

; String Operations
string_trim            equ $C090 ; C090
string_trim_left       equ $C093 ; C093
string_trim_right      equ $C096 ; C096
string_search          equ $C099 ; C099
string_search_array    equ $C09C ; C09C

; BASIC Line Buffer
autokey_write_buffer   equ $C0A8 ; C0A8
fnkey_write_buffer     equ $C0AB ; C0AB
runarg_count           equ $C0BA ; C0BA
runarg_get             equ $C0BD ; C0BD

; Miscellaneous
pause_jiffies          equ $C0CC ; C0CC    `
read_gamepad           equ $C0CF ; C0CF Read gamepad(s)
get_gamectrl           equ $C0D2 ; C0D2 Read full Xbox controller status
key_pressed            equ $C0D5 ; C0D5 Check key matrix for key press

; Sounds
play_sample            equ $C0EA ; C0EA Play digitized sound sample
set_soundfast          equ $C0ED ; C0ED Set SOUND speed flag
get_soundfast          equ $C0F0 ; C0F0 Return SOUND speed flag

; System and plusBASIC versions
get_plusbas_version    equ $C102 ; C102 Return plusBASIC version string
get_system_version     equ $C105 ; C105 Read system version string from ESP
version_to_long        equ $C108 ; C108 Convert string version to long int

; *****************
; * Graphics ROM  *
; * LD   IY,label *
; * CALL gfx_call *
; *****************

; Bitmap Graphics
bitmap_init_vars       equ $C000 ; C000 Initialize bitmap system variables
bitmap_set_mode        equ $C003 ; C003 Set bitmap mode system variable from video control register
bitmap_read_sysvars    equ $C006 ; C006 Read bitmap system variables
bitmap_read_color      equ $C009 ; C009 Get Bitmap Draw Colors
bitmap_write_color     equ $C00C ; C00C Set Bitmap Draw Colors
bitmap_clear_screen    equ $C00F ; C00F Clear Bitmap
bitmap_fill_byte       equ $C012 ; C012 Fill Bitmap with Byte
bitmap_fill_color      equ $C015 ; C015 Fill Bitmap Color RAM
bitmap_line            equ $C018 ; C018 [Future]
bitmap_setpixel        equ $C02D ; C02D Draw pixel
bitmap_resetpixel      equ $C030 ; C030 Erase pixel on 1 bpp bitmap screen
bitmap_getpixel        equ $C033 ; C033 Return pixel/bloxel at position
bitmap_get             equ $C036 ; C036 [future] Read Bitmap Screen Section into Buffer
colormap_fill          equ $C045 ; C045 Fill 1bpp Color Map Rectangle with Byte

; Palette and Colors
palette_reset          equ $C054 ; C054 Reset palette to default colors
palette_set            equ $C057 ; C057 Write one or more entries to palette
palette_get            equ $C05A ; C05A Read entire palette
palette_get_entry      equ $C05D ; C05D Read single palette entry

; Text Screen Read and Write
screen_clear_color     equ $C078 ; C078 Fill Color RAM with current/default colors
screen_read_byte       equ $C084 ; C084 Read byte from screen
screen_write_byte      equ $C087 ; C087 Write byte to screen
screen_read_bytes      equ $C08A ; C08A Read string from Screen RAM
screen_write_bytes     equ $C08D ; C08D Write string to Screen RAM
screen_read_paged      equ $C096 ; C096
screen_write_paged     equ $C099 ; C099
screen_read_fast       equ $C09C ; C09C
screen_write_fast      equ $C09F ; C09F
oolor_read_byte        equ $C0A8 ; C0A8 Read byte from Color RAM
color_write_byte       equ $C0AB ; C0AB Write byte to Color RAM
color_read_bytes       equ $C0AE ; C0AE Read string from Color RAM
color_write_bytes      equ $C0B1 ; C0B1 Write string to Color RAM
screen_clear_cursor    equ $C0C0 ; C0C0 Remove cursor from screen
screen_invert_color    equ $C0C6 ; C0C6
screen_invert_cursor   equ $C0C9 ; C0C9
screen_get             equ $C0D2 ; C0D2 Read Text Screen Section into Buffer
screen_put             equ $C0D5 ; C0D5 Write Text Screen Section from Buffer
screen_fill            equ $C0D8 ; C0D8 Fill Text or Color Screen Rectangle with Byte
screen_mode            equ $C0DE ; C0DE Return current Text Screen

; Text Screen Switch and Swap
init_screen_buffers    equ $C0E4 ; C0E4
init_screen_vars       equ $C0E7 ; C0E7
screen_reset           equ $C0F0 ; C0F0
screen_status          equ $C0F3 ; C0F3
screen_restore         equ $C0FC ; C0FC
screen_stash           equ $C0FF ; C0FF
screen_swap            equ $C102 ; C102
screen_switch          equ $C105 ; C105

; Tilemap
tile_set               equ $C10E ; C10E
tile_get               equ $C111 ; C111
tile_from_chrrom       equ $C114 ; C114
tile_combine_props     equ $C117 ; C117
tilemap_set_offset     equ $C120 ; C120
tilemap_get_offset     equ $C123 ; C123
tilemap_set_tile       equ $C126 ; C126
tilemap_get_tile       equ $C129 ; C129
tilemap_fill           equ $C12C ; C12C
tilemap_get            equ $C12F ; C12F
tilemap_put            equ $C132 ; C132
sprite_define          equ $C141 ; C141
sprite_defrect         equ $C144 ; C144
sprite_set_attrs       equ $C147 ; C147
sprite_get_attrs       equ $C14A ; C14A
spritle_set_pos        equ $C14D ; C14D
spritle_get_pos        equ $C150 ; C150
spritle_string_attrs   equ $C153 ; C153
spritle_reset          equ $C156 ; C156
spritle_reset_all      equ $C159 ; C159
