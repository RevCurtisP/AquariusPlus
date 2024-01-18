;; Save/Load binary test/demo

;zmac --zmac -o saveload.cim -o saveload.lst saveload.asm

aux_call          equ $2100 ; Call routine in auxillary ROM
str_tempdesc      equ $2115 ; Build descriptor for null terminated string in DSCTMP
file_load_binary  equ $C042 ; Load binary file into main memory
file_save_binary  equ $C05A ; Save binary file from main memory

save_addr equ $0100
load_addr equ $B100
file_len  equ 200
  
    org           $B000     ; This should be below stack and above arrays
save_file:
    push    hl                    ; Save text pointer
    ld      de,filename           ; DE = Filename address

    call    str_tempdesc          ; Build string descriptor for save
    ld      de,save_addr          ; DE = Data start address
    ld      bc,file_len           ; BC = Data length
    ld      iy,file_save_binary   ; Call save routine
    call    aux_call              ; in auxillary ROM
    pop     hl                    ; Restore text pointer
    ret
    
    org           $B020     ; This should be below stack and above arrays
load_file:    
    push    hl                    ; Save text pointer
    ld      de,filename           ; DE = Filename address
    call    str_tempdesc          ; Build string descriptor for save
    ld      de,load_addr          ; DE = Data start address
    ld      bc,file_len           ; BC = Data length
    ld      iy,file_load_binary   ; Call save routine
    call    aux_call              ; in auxillary ROM
    pop     hl                    ; Restore text pointer
    ret
    
    
filename:
    byte    "testfile.bin",0