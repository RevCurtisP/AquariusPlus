;=============================================================================
; Cold boot initialialization code
;=============================================================================
do_coldboot:

    ; Fill BASIC RAM with 0
    xor     a                   ; Fill with 0
    ld      hl,$3900            ; From beginning of BASIC RAM
    ld      bc,$C000-$3900      ; to end of BASIC RAM
    call    sys_fill_mem

    ; Set memory size
    ld      hl, BASIC_RAM_END   ; Top of public RAM
    ld      (MEMSIZ), hl        ; MEMSIZ, Contains the highest RAM location
    ld      de,-1023            ; Subtract 1023 bytes for string space
    add     hl, de
    ld      (STRSPC),hl
    ld      de, -512           ; Subtract another 512 for buffers
    add     hl, de
    ld      (TOPMEM), hl        ; TOPMEM, Top location to be used for stack
    ld      hl, BASTXT-1
    ld      (hl), $00           ; NULL at start of BASIC program
    inc     hl
    ld      (TXTTAB), hl        ; Beginning of BASIC program text
    call    SCRTCH              ; ST_NEW2 - NEW without syntax check

    ld      iy,init_screen_buffers
    call    aux_call
    ld      iy,init_screen_vars
    call    aux_call

    ld      d,$10                 ; Row 5 = Shift
    call    _modkey_check         ; If shift held down
    jp      nz,CLDCON             ;   Boot into S3 BASIC  

; Install BASIC HOOK
    ld      hl,fast_hook_handler
    ld      (HOOK), hl

    ; Zero out plusBASIC system vars
    xor     a
    ld      b,BAS_FDESC-RNDTAB
    ld      hl,RNDTAB
.sysvar_loop
    ld      (hl),a
    inc     hl
    djnz    .sysvar_loop

    ; Install Interrupt Handler
    ld      a,$C3               ; Jump Instruction
    ld      (INTJMP),a          ;
    ld      hl,irq_handler      ; Interrupt Address
    ld      (INTJMP+1), hl

    ; Set Default Interrupt Vector
    ld      a,$C3               ; Jump Instruction
    ld      (BASINTJP),a
    ld      hl,$0025            ; RET in COMPAR
    ld      (BASINTJP+1), hl

    ld      a,$FF                 ; Set TIMER to stopped
    ld      (TIMERCNT+2),a

    ; Default direct mode to keyrepeat on
    ld      a,KB_REPEAT
    ld      (BASYSCTL),a

    call    print_copyright
    call    check_autoexec        ; Check for autoexec file

    jp      INITFF

; Show our copyright message
print_copyright:
    call    PRNTIT              ; Print copyright string in ROM
    call    print_string_immd
    db $0D, $0A
.systext:
    db "Aquarius+ System ", 0
.syslen = $ - .systext
    ld      a, ESPCMD_VERSION
    call    esp_cmd
    ld      b,.syslen
.print_version:
    call    esp_get_byte
    or      a
    jr      z, .print_done
    call    TTYCHR
    inc     b
    jr      .print_version
.print_done:
    ld      a,38-plus_len         ; Print spaces to right justify +BASIC text
    sub     b
    jr      nc,.space_it
    call    CRDO
    jr      .print_basic
.space_it
    ld      b,a
    ld      a,' '
.space_loop
    call    TTYCHR
    djnz    .space_loop
.print_basic
    ld      hl,plus_text
    call    print_c_string
    call    CRDO
    jp      CRDO

; If autorun exists, push RUN "autoexec to key buffer
; ToDo: make esp functions return error code instead of generating BASIC error
check_autoexec:
    ld      d,$20                 ; Checking for CTL
    call    _modkey_check
    ret     nz
    ld      hl,auto_desc
    
    ld      iy,dos_open_read
    call    aux_call
    ret     m
    call    esp_close_all
    ld      hl,auto_cmd-1
    ld      (RESPTR),hl
.nope
    ret

; See if control is currently pressed
_modkey_check:
    ld      bc,$7FFF              ; Scan column 8
    in      a,(c)
    xor     $FF
    and     d                     ; Isolate control key
    ret

; If Alt key pressed during reset, copy cartridge and BASIC RAM to end of paged memory
; SrcPg: 19 20 21 32 33 34 35
; DstPg: 57 58 59 60 61 62 63
core_dump:
    ld      bc,$7FFF              ; Scan column 8
    in      a,(c)
    xor     $FF
    and     $40
    ;jp      z,init_banks
    ld      a,RAM_END_PG
    ex      af,af'                ; A' = DstPg
    ld      a,RAM_BAS_3           ; A = SrcPg
.loop
    out     (IO_BANK1),a
    ex      af,af'                ; A = DstPg, A' = SrcPg
    out     (IO_BANK2),a
    ld      hl,BANK1_BASE
    ld      de,BANK2_BASE
    ld      bc,PAGE_SIZE
    ldir
    dec     a                     ; Next Dest Page
    ex      af,af'                ; A = SrcPg, A' = DstPg
    dec     a                     ; Next Source Page
    cp      ROM_CART-1            ; If done with Cartridge
    jp      z,init_banks          ;   Continue reset
    cp      RAM_BAS_0-1           
    jr      nz,.loop
    ld      a,CHAR_RAM
    jr      .loop
