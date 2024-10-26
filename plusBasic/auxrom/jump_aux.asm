;=====================================================================================
; Auxiliary ROM jump table - Starts at $C000
;=====================================================================================
;;;ToDo: Add comments re: aux_call and update makeinc.py to parse them

_jump_table:
; dos <<DOS>>
    jp      dos_change_dir        ; Change Directory
    jp      dos_close             ; Close file or directory
    jp      dos_close_all         ; Close all files
    jp      dos_create_dir        ; Create directory
    jp      dos_delete            ; Delete file/directory
    jp      dos_stat              ; Return File Status
    jp      dos_get_cwd           ; Get Current Directory
    jp      dos_open              ; Open file
    jp      dos_open_append       ; Open file for write
    jp      dos_open_dir          ; Open directory for read
    jp      dos_open_read         ; Open file for read
    jp      dos_open_write        ; Open file for write
    jp      dos_read_dir          ; Read directory entry
    jp      dos_rename            ; Delete file/directory
    jp      dos_rewind            ; Move to beginning of file
    jp      dos_seek              ; Move to position in file
    jp      dos_tell              ; Get current position in file
; file_io <<File I/O>>
    jp      file_get_dir          ; Extract path from filespec
    jp      file_get_ext          ; Extract extension from filespec
    jp      just_ret
    jp      file_trim_dir         ; Trim path from filespec
    jp      file_trim_ext         ; Trim extension from filespec
    jp      file_load_binary      ; Load binary file into main memory
    jp      file_load_chrset      ; Load file into alternate chrset buffer
    jp      file_load_paged       ; Load binary file into paged memory
    jp      file_load_palette     ; Load and set palette 
    jp      file_load_pt3         ; Load PT3 file into PT3 buffer
    jp      file_load_rom         ; Load ROM file into page 35
    jp      file_load_screen      ; Load screen image
    jp      file_load_strbuf      ; Load file into BASIC string buffer
    jp      just_ret
    jp      just_ret
    jp      file_save_binary      ; Save binary file from main memory
    jp      file_save_paged       ; Save file from paged memory
    jp      file_save_palette     ; Get and save palette
    jp      file_save_screen      ; Save screen image 
    jp      file_save_strbuf      ; Save BASIC string buffer to file
    jp      file_load_defchrs     ; Load file into default chrset buffer
    jp      file_load_tilemap     ; Load tilemap
    jp      file_save_tilemap     ; Save tilemap
    jp      file_save_chrset      ;
    jp      just_ret
    jp      just_ret
    jp      file_read_dir         ; Read directory entry as ASCII string
    jp      just_ret
    jp      just_ret
    jp      just_ret
    jp      just_ret
; string.asm <<String Operations>>
    jp      string_trim
    jp      string_trim_left
    jp      string_trim_right
    jp      string_search
    jp      string_search_array
    jp      just_ret
    jp      just_ret
    jp      just_ret
; basbuf.asm <<BASIC Line Buffer>>
    jp      basbuf_read_byte
    jp      basbuf_read_bytes
    jp      basbuf_read_word
    jp      basbuf_write_byte
    jp      basbuf_write_bytes
    jp      basbuf_write_word
    jp      runarg_count
    jp      runarg_get
    jp      just_ret
    jp      just_ret
    jp      just_ret
    jp      just_ret
; misc.asm <<Miscellaneous>>
    jp      pause_jiffies
    jp      read_gamepad
    
_end_jump_table:
