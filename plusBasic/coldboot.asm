;=============================================================================
; Cold boot initialization code
;=============================================================================
do_coldboot:
      
    ; Fill BASIC RAM with 0
    xor     a                   ; Fill with 0
    ld      hl,$3900            ; From beginning of BASIC RAM
    ld      bc,$C000-$3900      ; to end of BASIC RAM
    call    sys_fill_mem

    ; Zero out plusBASIC system vars
    ld      b,RNDX-RNDTAB
    ld      hl,RNDTAB
.sysvar_loop
    ld      (hl),a
    inc     hl
    djnz    .sysvar_loop

    ; Set memory size
    ld      hl, BASIC_RAM_END   ; Top of public RAM
    ld      (MEMSIZ), hl        ; MEMSIZ, Contains the highest RAM location
    ld      de,-1023            ; Subtract 1023 bytes for string space
    add     hl, de
    ld      (STRSPC),hl
    ld      (TBFTOP),hl         ; Init Temp String Buffers
    ld      de, -512            ; Subtract another 512 for buffers
    add     hl, de
    ld      (TOPMEM), hl        ; TOPMEM, Top location to be used for stack
    ld      hl, BASTXT-1
    ld      (hl), $00           ; NULL at start of BASIC program
    inc     hl
    ld      (TXTTAB), hl        ; Beginning of BASIC program text
    call    SCRTCH              ; ST_NEW2 - NEW without syntax check

    ; Default direct mode to keyrepeat off, use SET KEY REPEAT on in autoexec to default to keyrepeat on
    ; ld      a,KB_REPEAT
    ; ld      (BASYSCTL),a

    ld      iy,init_basbuf_vars
    call    aux_call
    ld      iy,gfx_init
    call    gfx_call

;    ld      d,$10                 ; Row 5 = Shift
;    call    _modkey_check         ; If shift held down
;    jp      nz,CLDCON             ;   Boot into S3 BASIC  

; Install BASIC HOOK
    ld      hl,fast_hook_handler
    ld      (HOOK), hl

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


    call    print_copyright
    call    check_autoexec        ; Check for autoexec file

    xor     a
    out     (IO_SYSCTRL),a        ; Disable unlimited turbo mode

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
    call    CRDO
.print_basic
    ld      hl,plus_text
    call    print_c_string
    call    CRDO
    jp      CRDO

; If autorun exists, push RUN "autoexec to key buffer
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
