aux_jump_table:
    jp      dos_open_file         ; Open file
    jp      dos_open_read         ; Open file for read
    jp      dos_open_write        ; Open file for write
    jp      dos_close_file        ; Close file or directory
    jp      dos_close_all         ; Close all files
    jp      dos_change_dir        ; Change Directory
    jp      dos_delete_file       ; Delete file/directory
    jp      dos_create_dir        ; Create directory
    jp      dos_get_cwd           ; Get Current Directory
    jp      dos_rename_file       ; Delete file/directory
    jp      dos_filestat          ; Return File Status
    jp      file_load_binary      ; Load binary file into main memory
    jp      file_load_chrset      ; Load file into character RAM buffer
    jp      file_load_paged       ; Load binary file into paged memory`
    jp      file_load_rom         ; Load ROM file into page 35
    jp      file_load_screen      ; Load screen image
_end_jump_table:
