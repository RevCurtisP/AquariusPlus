;=====================================================================================
; Auxiliary ROM jump table - Starts at $C000
;=====================================================================================
;;;ToDo: Add comments re: aux_call and update makeinc.py to parse them

_jump_table:
; .
; *****************
; * Auxiliary ROM *
; * LD   IY,label *
; * CALL aux_call *
; *****************
; dos <<DOS>>
    jp      dos_change_dir        ; C000 Change Directory
    jp      dos_close             ; C003 Close file or directory
    jp      dos_close_all         ; C006 Close all files
    jp      dos_create_dir        ; C009 Create directory
    jp      dos_delete            ; C00C Delete file/directory
    jp      dos_stat              ; C00F Return File Status
    jp      dos_get_cwd           ; C012 Get Current Directory
    jp      dos_open              ; C015 Open file
    jp      dos_open_append       ; C018 Open file for write
    jp      dos_open_dir          ; C01B Open directory for read
    jp      dos_open_read         ; C01E Open file for read
    jp      dos_open_write        ; C021 Open file for write
    jp      dos_read_dir          ; C024 Read directory entry
    jp      dos_rename            ; C027 Delete file/directory
    jp      dos_rewind            ; C02A Move to beginning of file
    jp      dos_seek              ; C02D Move to position in file
    jp      dos_tell              ; C030 Get current position in file
; file_io <<File I/O>>
    jp      file_get_dir          ; C033 Extract path from filespec
    jp      file_get_ext          ; C036 Extract extension from filespec
    jp      just_ret              ; C039 
    jp      file_trim_dir         ; C03C Trim path from filespec
    jp      file_trim_ext         ; C03F Trim extension from filespec
    jp      file_load_binary      ; C042 Load binary file into main memory
    jp      file_load_chrset      ; C045 Load file into alternate chrset buffer
    jp      file_load_paged       ; C048 Load binary file into paged memory
    jp      file_load_palette     ; C04B Load and set palette 
    jp      file_load_pt3         ; C04E Load PT3 file into PT3 buffer
    jp      file_load_rom         ; C051 Load ROM file into page 35
    jp      file_load_screen      ; C054 Load screen image
    jp      file_load_strbuf      ; C057 Load file into BASIC string buffer
    jp      just_ret              ; C05A 
    jp      just_ret              ; C05D 
    jp      file_save_binary      ; C060 Save binary file from main memory
    jp      file_save_paged       ; C063 Save file from paged memory
    jp      file_save_palette     ; C066 Get and save palette
    jp      file_save_screen      ; C069 Save screen image 
    jp      file_save_strbuf      ; C06C Save BASIC string buffer to file
    jp      file_load_defchrs     ; C06F Load file into default chrset buffer
    jp      file_load_tilemap     ; C072 Load tilemap
    jp      file_save_tilemap     ; C075 Save tilemap
    jp      file_save_chrset      ; C078 
    jp      file_load_tileset     ; C07B Load tiles
    jp      file_save_tileset     ; C07E Save tiles
    jp      file_read_dir         ; C081 Read directory entry as ASCII string
    jp      just_ret              ; C084 
    jp      just_ret              ; C087 
    jp      just_ret              ; C08A 
    jp      just_ret              ; C08D 
; string.asm <<String Operations>>
    jp      string_trim           ; C090
    jp      string_trim_left      ; C093
    jp      string_trim_right     ; C096
    jp      string_search         ; C099
    jp      string_search_array   ; C09C
    jp      just_ret              ; C09F
    jp      just_ret              ; C0A2
    jp      just_ret              ; C0A5
; basbuf.asm <<BASIC Line Buffer>>
    jp      autokey_write_buffer  ; C0A8
    jp      fnkey_write_buffer    ; C0AB 
    jp      just_ret              ; C0AE
    jp      just_ret              ; C0B1
    jp      just_ret              ; C0B4
    jp      just_ret              ; C0B7
    jp      runarg_count          ; C0BA
    jp      runarg_get            ; C0BD
    jp      just_ret              ; C0C0
    jp      just_ret              ; C0C3
    jp      just_ret              ; C0C6
    jp      just_ret              ; C0C9
; misc.asm <<Miscellaneous>>`
    jp      pause_jiffies         ; C0CC    `
    jp      read_gamepad          ; C0CF Read gamepad(s)
    jp      get_gamectrl          ; C0D2 Read full Xbox controller status
    jp      key_pressed           ; C0D5 Check key matrix for key press
    jp      just_ret              ; C0D8 
    jp      just_ret              ; C0DB 
    jp      just_ret              ; C0DE 
    jp      just_ret              ; C0E1 
    jp      just_ret              ; C0E4 
    jp      just_ret              ; C0E7 
; sound.asm <<Sounds>>            
    jp      play_sample           ; C0EA Play digitized sound sample
    jp      set_soundfast         ; C0ED Set SOUND speed flag
    jp      get_soundfast         ; C0F0 Return SOUND speed flag
    jp      just_ret              ; C0F3 
    jp      just_ret              ; C0F6 
    jp      just_ret              ; C0F9 
    jp      just_ret              ; C0FC 
    jp      just_ret              ; C0FF 
; version.asm <<System and plusBASIC versions>>
    jp      get_plusbas_version   ; C102 Return plusBASIC version string
    jp      get_system_version    ; C105 Read system version string from ESP
    jp      version_to_long       ; C108 Convert string version to long int
_end_jump_table:
