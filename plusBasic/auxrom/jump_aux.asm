aux_jump_table:
    jp      dos_open              ; Open file
    jp      dos_open_read         ; Open file for read
    jp      dos_open_write        ; Open file for write
    jp      dos_open_append       ; Open file for write
    jp      dos_open_dir          ; Open directory for read
    jp      dos_close             ; Close file or directory
    jp      dos_close_all         ; Close all files
    jp      dos_change_dir        ; Change Directory
    jp      dos_delete            ; Delete file/directory
    jp      dos_create_dir        ; Create directory
    jp      dos_get_cwd           ; Get Current Directory
    jp      dos_rename            ; Delete file/directory
    jp      dos_rewind            ; Move to beginning of file
    jp      dos_seek              ; Move to position in file
    jp      dos_tell              ; Get current position in file
    jp      dos_filestat          ; Return File Status

    jp      file_load_binary      ; Load binary file into main memory
    jp      file_load_chrset      ; Load file into character RAM buffer
    jp      file_load_paged       ; Load binary file into paged memory
    jp      file_load_palette     ; Load and set palette 
    jp      file_load_pt3         ; Load PT3 file into PT3 buffer
    jp      file_load_rom         ; Load ROM file into page 35
    jp      file_load_screen      ; Load screen image
    jp      file_load_strbuf      ; Load file into BASIC string buffer
    jp      file_save_binary      ; Save binary file from main memory
    jp      file_save_paged       ; Save file from paged memory
    jp      file_save_palette     ; Get and save palette
    jp      file_save_screen      ; Save screen image
    jp      file_save_strbuf      ; Save BASIC string buffer to file

_end_jump_table:
