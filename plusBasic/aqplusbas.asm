;=====================================================================================
; Aquarius+ System ROM and plusBASIC
;=====================================================================================
; By Curtis F Kaylor and Frank van den Hoef
;
; Based on AQUBASIC source code by Bruce Abbott:
; http://bhabbott.net.nz/micro_expander.html
;
; Useful links:
; - Excellent Aquarius S2 ROM disassembly by Curtis F Kaylor:
; https://github.com/RevCurtisP/Aquarius/blob/main/disassembly/aquarius-rom.lst
;
; Extra BASIC commands:
; EDIT   - ** Not supported **
; CLS    - Clear screen
; LOCATE - Position on screen
; OUT    - Output data to I/O port
; PSG    - Program PSG register, value
; DEBUG  - ** Not supported **
; CALL   - Call machine code subroutine
; LOAD   - Load file from USB disk
; SAVE   - Save file to USB disk
; DIR    - Display USB disk directory with wildcard
; MKDIR  - Create directory
; DEL    - Delete file
; CD     - Change directory
;
; Extra BASIC functions:
; IN()   - Get data from I/O port
; JOY()  - Read joystick
; HEX$() - Convert number to hexadecimal string
;-----------------------------------------------------------------------------

; To assemble:
;   zmac --zmac  -I basic -I gfx -o aqplusbas.cim -o aqplusbas.lst aqplusbas.asm

    include "sbasic.inc"
    include "plus.inc"

;Jump Table: S3BASIC interface
    org     $2000
    jp      _reset          ; $2000 Called from main ROM at reset vector
    jp      _coldboot       ; $2003 Called from main ROM for cold boot
    jp      _start_cart     ; $2006
    jp      _interrupt      ; $2009
    jp      _warm_boot      ; $200C Called from main ROM for warm boot
    jp      _scan_label     ; $200F Called from GOTO and RESTORE
    jp      _keyread        ; $2012 Called from COLORS
    jp      _ctrl_keys      ; $2015
    jp      _iscntc_hook    ; $2018
    jp      _main_ext       ; $201B Save Line# Flag`
    jp      _stuffh_ext     ; $201E Check for additional tokens when stuffing
    jp      do_cls_default  ; $2021 Clear both text screens
    jp      _check_topmem   ; $2024 Verify TOPMEM is in Bank 2
    jp      _ptrget_hook    ; $2027 Allow _Alphanumunder sftar var name 
    jp      _inlin_hook     ; $20?? Jump from INLIN for command history recall
    jp      _inlin_done     ; $20?? Jumped from FININL to save command to history


;-----------------------------------------------------------------------------
; Reset vector
;
; CAUTION: stack isn't available at this point, so don't use any instruction
;          that uses the stack.
;-----------------------------------------------------------------------------
_reset:
    ; Disable interrupts and turbo mode
    di      
    xor     a
    out     (IO_IRQMASK),a
    out     (IO_SYSCTRL),a

    ; Set up temp stack in text line buffer
    ld      sp, $38A0

    ; Initialize banking registers
    ld      a, 0 | BANK_OVERLAY | BANK_READONLY
    out     (IO_BANK0), a
    ld      a, 33
    out     (IO_BANK1), a
    ld      a, 34
    out     (IO_BANK2), a
    ld      a, 19                 ; Cartridge Port
    out     (IO_BANK3), a

    ; Init video mode
    call    reset_screen

    ; Initialize character RAM
    call    init_charram

    ; Initialize ESP
    ld      a, ESPCMD_RESET
    call    esp_cmd

    ; Install Interrupt Handler
    ld      a,$C3               ; Jump Instruction
    ld      (INTJMP),a          ;
    ld      hl,_interrupt       ; Interrupt Address
    ld      (INTJMP+1), hl

    ; Turn on Keyboard Buffer
    ld      a,KB_ENABLE | KB_ASCII
    call    key_set_keymode

    ; Back to system ROM init
    jp      JMPINI


;-----------------------------------------------------------------------------
; Cold boot entry point
; Executed on RESET if no cartridge detected
;-----------------------------------------------------------------------------
_coldboot:
    
    ld      a,ROM_AUX_PG        ; System Auxiliary ROM
    out     (IO_BANK3),a
    call    init_screen_buffers
    call    init_screen_vars

    ld      a,ROM_EXT_PG        ; plusBASIC extended ROM
    out     (IO_BANK3), a

    Call    _clear_basic_ram    ; Init BASIC RAM to zeroes

    ; Set memory size
    ld      hl, BASIC_RAM_END   ; Top of public RAM
    ld      (MEMSIZ), hl        ; MEMSIZ, Contains the highest RAM location
    ld      de, -1024           ; Subtract 1k for strings space
    add     hl, de
    ld      (TOPMEM), hl        ; TOPMEM, Top location to be used for stack
    ld      hl, BASTXT-1
    ld      (hl), $00           ; NULL at start of BASIC program
    inc     hl
    ld      (TXTTAB), hl        ; Beginning of BASIC program text
    call    SCRTCH              ; ST_NEW2 - NEW without syntax check

    ; Install BASIC HOOK
    ld      hl,fast_hook_handler
    ld      (HOOK), hl

    ; Zero out plusBASIC system vars
    xor     a
    ld      b,ESP_FDESC-RNDTAB
    ld      hl,RNDTAB
.sysvar_loop
    ld      (hl),a
    inc     hl
    djnz    .sysvar_loop

    ; Default direct mode to keyrepeat on
    ld      a,KB_REPEAT
    ld      (BASYSCTL),a

    call    clear_esp_fdesc
    call    spritle_clear_all     ; Clear all sprite properties

    call    print_copyright

    jp      INITFF              ; Continue in ROM

    dc $2100-$,$76

ifdef _____   ; Waiting for modules to stablize
    org     $2100
    include "kernel.asm"    ; Kernal jump table
endif

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
    ld      a,38-_plus_len        ; Print spaces to right justify +BASIC text
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
    call    print_string_immd
_plus_text:
    db "plusBASIC "
_plus_version:
    db "v0.17b", 0
_plus_len   equ   $ - _plus_text
    call    CRDO
    jp      CRDO

; If autorun exists, push RUN "autoexec to key buffer
; ToDo: make esp functions return error code instead of generating BASIC error
check_autoexec:
    ld      hl,_autodesc
    call    esp_open
    ret     m
    call    esp_close_all
    ld      hl,_autocmd-1
    ld      (RESPTR),hl
.nope
    ret
_autocmd:
    db      'RUN "'
_autotext
    db      "autoexec"
_autolen = $ - _autotext
    db      $0D
_autodesc
    dw      _autolen,_autotext

;-----------------------------------------------------------------------------
; Issue OV Error if TOPMEM will put stack in Bank 1
; Calle from CLEARS
;-----------------------------------------------------------------------------
_check_topmem:
    ld      de,$8000+1024         ; Leave 1024 bytes for stack
    rst     COMPAR                ; If new TOPMEM < Minimum
    jp      c,OVERR               ;   Overflow error
    ld      (TOPMEM),hl           ; Else Set TOPMEM
    ret                           ;   and Return

;-----------------------------------------------------------------------------
; Extended line editor function keys
; Jumped to from INLNC
; On entry: A,C = character typed, B = input buffer character count
;-----------------------------------------------------------------------------
_ctrl_keys:
    push    bc                    ; Save character count
    call    in_direct
    jr      c,.dontscreen         ; If Not in Direct Mode
    xor     a
    cp      b
    jr      nz,.dontscreen        ; and Input Buffer is empty
    ld      a,c
;ToDo: Add Ctrl-W = 80 columns, Crtl-T = 40 col screen 0, Ctrl-Y is 40 col screen 1
;Save cursor position and character under RAM in screen RAM hole

.dontscreen
    ld      a,c                   ; Get typed character
    sub     a,'K'-64              ;
    jr      c,.notrub             ;
    cp      'M'-'K'               ;
    jr      z,.notrub             ;
    jr      nc,.notrepeat         ; If ^K or ^L
    dec     a                     ;   ^K = $FF, ^L = 0
    and     KB_REPEAT             ;   ^K = Repeat on, ^L = off
    ld      b,a                   ;   Save it
    ld      a,(BASYSCTL)          ;   Get current Flags
    and     $FF-KB_REPEAT         ;   Mask out Repeat Bit
    or      b                     ;   OR new value back in
    ld      (BASYSCTL),a          ;   And write it back out
    ld      a,KB_ENABLE | KB_ASCII
    or      b                     ;
    call    key_set_keymode       ;   Now set new keybuffer mode
    jr      .inlinc               ;   Wait for next key
.notrepeat:
    cp      'Q'-'K'               ; If not ^N through ^P
    jr      c,.charset
.notrub
    pop     bc                    ;   Restore character count
    jp      NOTRUB                ;   Continue standard Ctrl-key check
.charset
    sub     a,'N'-'K'             ; ^N = 0, ^O = 1, ^P = 2
    xor     1                     ; ^O = 1, ^O = 0, ^P = 2
    push    hl                    ;
    call    select_chrset         ; Select the character set
    pop     hl
.inlinc
    pop     bc                    ;   Restore character count
    jp      INLINC                ;   Wait for next key

;-----------------------------------------------------------------------------
; Check for Direct Mode
; Output: Carry clear if in Direct Mode
; Clobbered: B
;-----------------------------------------------------------------------------
in_direct:
    ld      a,(CURLIN)
    ld      b,a
    ld      a,(CURLIN+1)
    and     b
    cp      $FE
    ret

;-----------------------------------------------------------------------------
; Check for Control Keys before fetching next statement
; returns to NEWSTT
;-----------------------------------------------------------------------------
_iscntc_hook:
    call    CNTCCN                ; Check for Control-C
    ret     z                     ; Return if no keypress
    cp      'D'-64                ; If Ctrl-D
    jr      z,.turbo_off          ;   Disable Turbo Mode
    cp      'F'-64                ; If not Ctrl-F
    jp      nz,ISCNTS             ;   Back to INCNTC
    byte    $3E                   ; LD A,$AF to Enable Turbo Mode
.turbo_off
    xor     a

;-----------------------------------------------------------------------------
; Enable/Disable Turbo mode
; Input: A = Bit 2 set for On, reset for Off
; Clobbers: B
;-----------------------------------------------------------------------------
sys_turbo_mode:
    and     SYSCTRL_TURBO         ; Isolate Fast Mode bit
    ld      b,a                   ;   and copy to B
_turbo_mode
    in      a,(IO_SYSCTRL)        ; Read SYSCTRL
    and     ~SYSCTRL_TURBO        ;   mask out Fast Mode bit
    or      b                     ;   and copy the new Fast Mode bit in
    out     (IO_SYSCTRL),a        ; Write back to SYSCTRL
    ret

;-----------------------------------------------------------------------------
; Return BASIC Version
; Input: HL: String Buffer address
; Output: BC: Version String Length
; Clobbers: A,DE
;-----------------------------------------------------------------------------
sys_ver_basic:
    ex      de,hl                 ; DE = BufAdr
    ld      hl,_plus_version      ; HL = VerAdr
    call    string_copy           ; Copy VerStr to StrBuf
    ex      de,hl                 ; HL = BufAdr
    ret


;-----------------------------------------------------------------------------
; Fill BASIC RAM with 0
; Clobbers: AF, BC, DE, HL
;-----------------------------------------------------------------------------
_clear_basic_ram:
    xor     a                   ; Fill with 0
    ld      hl,$3900            ; From beginning of BASIC RAM
    ld      bc,$C000-$3900      ; to end of BASIC RAM

;-----------------------------------------------------------------------------
; Fast Fill memory
; Input: A: Fill Byte
;       BC: Byte Count
;       HL: Start Address
; Clobbers: BC, DE, HL
;-----------------------------------------------------------------------------
sys_fill_mem:
    ld      (hl),a                ; Set first byte
    ld      d,h
    ld      e,l
    dec     bc                    ; First byte already filled
    inc     de                    ; DstAdr = SrcAdr + 1
    ldir                          ; Overlap will propogate start byte
    ret

;-----------------------------------------------------------------------------
; Copy selected character set into Character RAM
; Input: A: Character set (0: Standard, 1: Latin-1, 2: Custom)
;-----------------------------------------------------------------------------
select_chrset:
    or      a                     ; If A = 0
    jr      z,_reset_charram      ;   Copy standard character set
    dec     a                     ; If A = 1
    ld      hl,CHAR_ROM_L1        ;   Copy Latin-1 character set
    jr      z,_copy_charram       ; Else
custom_chrset:
    ld      hl,CHRSETBUF          ;   Copy Custom Character Set
    ld      a,BAS_BUFFR
    jr      copy_char_ram         ;

;-----------------------------------------------------------------------------
; Character RAM initialization
;-----------------------------------------------------------------------------
init_charram:
    ld      hl,CHAR_ROM_AQ        ; Copy AQUASCII in to Custom Char ROM buffer
    ld      a,ROM_SYS_PG
    ld      de,CHRSETBUF
    ex      af,af'
    ld      a,BAS_BUFFR
    call    page_fast_copy
_reset_charram
    ld      hl,CHAR_ROM_AQ        ; and fall into _copy_charram
_copy_charram:
    ld      a,ROM_SYS_PG          ; Set source page and address

;-----------------------------------------------------------------------------
; Copy Character ROM into Character RAM
; Input: A = Source Page
;       HL = Source Address
; Clobbered: AF',BC,DE,HL,IX
;-----------------------------------------------------------------------------
copy_char_ram:
    ex      af,af'
    ld      a,CHAR_RAM
    ld      bc,2048
    ld      de,0
    jp      page_fast_copy        ; Copy It

;-----------------------------------------------------------------------------
; Cartridge start entry point - A hold scramble value
;-----------------------------------------------------------------------------
_start_cart:
    cp      $00
    jp      nz, .descramble
    jp      XINIT

.descramble:
    ; Map destination RAM in bank2
    ld      a, 35
    out     (IO_BANK2), a

    ; Copy ROM cartridge to RAM
    ld      de, $8000
    ld      hl, $C000
    ld      bc, $4000
    ldir

    ; Map RAM in bank3
    ld      a, 35
    out     (IO_BANK3), a

descramble_rom:
    ; Determine scramble value
    xor     a
    ld      hl, $E003
    ld      b, 12
.loop:
    add     a, (hl)
    inc     hl
    add     a, b
    dec     b
    jr      nz, .loop
    xor     (hl)
    ld      b, a

    ; Descramble ROM
    ld      hl, $C000
    ld      de, $4000
.loop2:
    ld      a, b
    xor     (hl)
    ld      (hl), a

    inc     hl
    dec     de
    ld      a, d
    or      e
    jr      nz, .loop2

    ; Reinit banks
    ld      a, 33
    out     (IO_BANK1), a
    ld      a, 34
    out     (IO_BANK2), a

    ; Bank3 -> readonly
    ld      a, 35 | BANK_READONLY
    out     (IO_BANK3), a

    ; Reinit stack pointer
    ld      sp, $38A0

    ; Start ROM
    jp      $E010

;-----------------------------------------------------------------------------
; VBLANK Interrupt Handler
;
;-----------------------------------------------------------------------------
_interrupt:




    ret

;-----------------------------------------------------------------------------
; Intercept WRMCON call
;-----------------------------------------------------------------------------
_warm_boot:
    ld      a, ROM_EXT_PG          ; Page 1 ROM
    out     (IO_BANK3), a         ; into Bank 3
    jp      WRMCON                ; Go back to S3 BASIC

;-----------------------------------------------------------------------------
; Directly read alternate keyboard port
;-----------------------------------------------------------------------------
_keyread:
    jp      key_read_ascii

;-----------------------------------------------------------------------------
; INLIN enhancement stubs
;-----------------------------------------------------------------------------
_inlin_hook:
_inlin_done:
    ret

;-----------------------------------------------------------------------------
; Hook 2 - READY (Enter Direct Mode
;-----------------------------------------------------------------------------
direct_mode:
    call    text_screen

    ld      a,(BASYSCTL)
    and     KB_REPEAT
    or      KB_ENABLE | KB_ASCII
    call    key_set_keymode       ; Turn on keybuffer, ASCII mode, no repeat

    jp      HOOK2+1

; To clean up after aborted DOS command after allowing multiple files open
    ld      a,(ESP_FDESC)         ; Get File Descriptor in Use
    or      a
    jp      m,.no_fdesc           ; If Valid Descriptor
    call    esp_close             ;   Close the File
    call    clear_esp_fdesc       ;
.no_fdesc
    jp      HOOK2+1

clear_esp_fdesc:
    ld      a,128
    ld      (ESP_FDESC),a         ; Set to No File
    ret

text_screen:
    in      a,(IO_VCTRL)
    and     a,~VCTRL_MODE_MC
    or      a,VCTRL_TEXT_EN
    out     (IO_VCTRL),a
    ret

reset_screen:
    ld      a,VCTRL_TEXT_EN
    out     (IO_VCTRL),a
    call    reset_palette
    
set_ttywid:
    in      a,(IO_VCTRL)
set_ttywid_a:
    and     VCRTL_80COL_EN
    ld      a,40                  ; A = 40
    jr      z,.not80              ; If 80 column bitset
    rla                           ;   A = 80
.not80
    ld      (TTYWID),a            ; Save it
    ret

reset_palette:
    ld      hl, .default_palette
    ld      c, IO_VPALSEL
    ld      b, 0
    ld      d, 32
.palloop:
    out     (c), b
    ld      a, (hl)
    out     (IO_VPALDATA), a
    inc     hl
    inc     b
    dec     d
    jr      nz, .palloop
    ret


.default_palette:
    dw $111, $F11, $1F1, $FF1, $22E, $F1F, $3CC, $FFF
    dw $CCC, $3BB, $C2C, $419, $FF7, $2D4, $B22, $333

;-----------------------------------------------------------------------------
; Hook 12 - SCRTCH (Execute NEW statement)
;-----------------------------------------------------------------------------
_scratch:
    call    page_restore_plus
    call    clear_all_errvars
    ld      c,0
    call    spritle_toggle_all    ; Disable all sprite
    jp      HOOK12+1

;-----------------------------------------------------------------------------
; Hook 18 - INCHRC (Get character from keyboard)
;-----------------------------------------------------------------------------
read_key:
    jp      key_read_ascii        ; Skip autotype for now
    exx
.autotype
    ld      hl,(RESPTR)
    ld      a,h
    or      a
    jr      z,.readkey
    inc     hl
    ld      a,(hl)
    ld      (RESPTR),hl
    or      a
    jp      nz,.done
    xor     a
    ld      (RESPTR+1),a
.done
    exx
    ret
.readkey
    exx
    jp      key_read_ascii    ; Read key from keyboard and return

;-----------------------------------------------------------------------------
; Allow underscore after 1 or 2 letter variable name, then skip all
; letters, numbers, underscores, and tokens for aphabetic keywords
; On entry, B = second character of variable name, initialized to NUL
;-----------------------------------------------------------------------------
_ptrget_hook
    rst     CHRGET                ; Get character after first letter variable name
    jr      c,.is_second          ; If not a digit
    cp      '~'
    jr      z,.eat_suffix         ; or tilde
    call    ISLETC                ; or letter
    jr      c,.nosec              ;   Get out
.is_second
    ld      b,a                   ; Make it the second character 
    rst     CHRGET                ; Get following character
    cp      '~'                   ; 
    jr      z,.eat_suffix         ; If not underscore
    dec     hl                    ;  Back up
    jp      EATEM                 ;  and continue with normal skip
.eat_suffix
    inc     hl
    ld      a,(hl)                ; Get next character
    or      a                     ; If NUL
    jr      z,.nosec              ;   Get out
    cp      ' '                   ; If Space
    jp      z,SKIPDS              ;   Get out
    cp      '~'                   ; If underscore
    jr      z,.eat_suffix         ;   Skip it
    cp      '0'                   ; If < '0'
    jp      c,NOSEC               ;   Get out
    cp      ':'                   ; If ':'
    jr      z,.nosec              ;   Get out
    jr      c,.eat_suffix         ; If <= '9', skip it
    cp      'A'                   ; If < 'A'
    jp      c,NOSEC               ;   Get out
    cp      'Z'+1                 ; If <= 'Z'
    jr      c,.eat_suffix         ;   Skip it
    cp      $80                   ; If not token
    jp      c,NOSEC               ;   Get out
    cp      PLUSTK                ; If < '+'
    jr      c,.eat_suffix         ;   Skip it
    cp      EXPTK+1               ; If <= '^'
    jp      c,NOSEC               ;   Get out
    cp      ORTK+1                ; If AND or OR
    jr      c,.eat_suffix         ;   Skip it
    cp      LESSTK+1              ; If < '<'
    jp      c,NOSEC               ;   Get out
    jr      .eat_suffix           ; Else skip it
.nosec
    scf
    jp      NOSEC                 ;   jump to NOSEC with carry set


;-----------------------------------------------------------------------------
; GOTO and RESUME hack
; Check for label at beginning of line, then search for it's line
;-----------------------------------------------------------------------------
_scan_label:
    ld      a,(hl)                ; Reget current character
    cp      '_'
    jr      nz,.not_label         ; If not underscore
    ex      de,hl                 ; DE = Text Pointer
    ld      (TEMP8),de            ; Save it
    ld      hl,(TXTTAB)           ; HL = start of program,
.line_loop:
    ld      (TEMP2),hl            ; Save pointer to line
    ld      c,(hl)                ; BC = link to next line
    inc     hl
    ld      b,(hl)
    inc     hl
    ld      (TEMP3),bc            ; Save for later
    inc     hl                    ; Skip line number
    inc     hl                    ;
    ld      a,(hl)                ; Get first character
    cp      '_'                   ; If not underscore
    jr      nz,.next_line         ;   Skip to next lines
.label_loop:
    ld      a,(de)                ; Next character from label
.not_lspace
    call    .check_colon          ; Treat colon as terminator
    ld      b,a                   ; Put in B for compare
.text_loop:
    ld      a,(hl)                ; Get character from line
.not_tspace
    call    .check_colon          ; Treat colon as terminator
    cp      b                     ; If characters don't match
    jp      nz,.no_match          ;
    or      a                     ; If both are terminators
    jr      z,.found_it           ;   Finish up
    inc     hl                    ; HL is Text Pointer
    inc     de                    ; Move it on up
    jr      .label_loop           ; Check next character
.no_match
    ld      de,(TEMP8)            ; Restore pointer to label
.next_line
    ld      hl,(TEMP3)            ; Get Link to next linr
    ld      a,h                   ; If link to next line is $0000
    or      l                     ;   End of program
    jp      z,ULERR               ;   So Undefined Line error
    jr      .line_loop            ; Scan the next line
.found_it
    pop     af                    ; Get return address
    cp      $06                   ; If we came from GOTO
    ret     z                     ;   Return to NEWSTT
    cp      $0C                   ; If we came from RESTORE
    jp      z,BGNRST              ;   Load DATPTR, HL = Text Pointer, and Return
    ex      de,hl                 ; HL = New text pointer
    ld      de,(TEMP2)            ; DE = Pointer to Line
    jp      reset_trap            ; Finish ON ERROR GOTO

.not_label:
    jp      SCNLIN                ;   Scan line number and return to GOTO

.check_colon:
    cp      ' '                   ; Check A
    jr      z,.ret_zero           ; If colon
    cp      ':'                   ; Check A
    ret     nz
.ret_zero
    xor     a                     ;   Treat like terminator
    ret

ULERR:
    ld      e,ERRUL
    jp      force_error

    
;-----------------------------------------------------------------------------
; bas_read_to_buff - Read String from ESP to BASIC String Buffer
; Input: IX: DOS or ESP routine to call
; Output: E: String Length
;        DE: Address of Terminator
;        HL: Buffer Address
; Clobbers: 
;-----------------------------------------------------------------------------
bas_read_to_buff:
    ld      hl,FBUFFR             ; Use FBUFFR for now
jump_ix:
    jp      (ix)                  ; Execute routine and return

do_cls_default:
    ld      a,6                   ; default to black on cyan

;-----------------------------------------------------------------------------
; Clear Screen and init cursor
; Input: A: Color Attributes
;-----------------------------------------------------------------------------
do_cls:
;-----------------------------------------------------------------------------
; Clear Screen
; Input: A: Color Attributes
;-----------------------------------------------------------------------------
    push    hl
    call    clear_screen
    pop     hl
_init_cursor
    ld      a,' '
    ld      (CURCHR),a            ; SPACE under cursor
    ld      de,(TTYWID)
    inc     de
    ld      d,high(SCREEN)
    ld      (CURRAM),de
    xor     a
    ld      (TTYPOS),a            ; column 0
    ret

clear_screen:
    ld      c,IO_VCTRL
    in      b,(c)
    bit     6,b
    jr      nz,clear_screen80
clear_screen40:
    ld      hl,SCREEN
    ld      c,25
.line:
    ld      b,40
.char:
    ld      (hl),' '
    set     2,h
    ld      (hl),a
    res     2,h
    inc     hl
    djnz    .char
    dec     c
    jr      nz,.line
    ret

clear_screen80:
    set     7,b
    push    bc
    call    .fill80               ; Fill COLOR RAM
    pop     bc
    res     7,b
    ld      a,' '
.fill80
    out     (c),b
    ld      hl,SCREEN
    ld      bc,2048
    jp      sys_fill_mem


;-----------------------------------------------------------------------------
; DOS routines
;-----------------------------------------------------------------------------
    include "dos.asm"

;-----------------------------------------------------------------------------
; ESP routines
;-----------------------------------------------------------------------------
    include "esp.asm"

;-----------------------------------------------------------------------------
; Paged memory routines
;-----------------------------------------------------------------------------
    include "paged.asm"

;-----------------------------------------------------------------------------
; Alternate keyboard port routines
;-----------------------------------------------------------------------------
    include "keyread.asm"

;-----------------------------------------------------------------------------
; Extended string buffer routines
;-----------------------------------------------------------------------------
    include "sbuff.asm"

;-----------------------------------------------------------------------------
; Utility routines
;-----------------------------------------------------------------------------
    include "util.asm"

free_rom_2k = hook_table - $

;------------------------------------------------------------------------------
; Hook, Dispatch Tables and Handlers
;------------------------------------------------------------------------------

    include "tokens.asm"        ; Keyword lists and tokenize/expand routines

; ------------------------------------------------------------------------------
;  Hook Jump Table
; ------------------------------------------------------------------------------

    assert !($2DFF<$)   ; ROM full!
    dc $2E00-$,$76

; BASIC Hook Jump Table
; 58 Bytes
hook_table:                     ; ## caller   addr  performing function
    dw      _trap_error         ;  0 ERROR    03DB  Initialize Stack, Display Error, and Stop Program
    dw      _force_error        ;  1 ERRCRD   03E0  Print Error Message
    dw      direct_mode         ;  2 READY    0402  BASIC command line (immediate mode)
    dw      HOOK3+1             ;  3 EDENT    0428  Save Tokenized Line
    dw      HOOK4+1             ;  4 FINI     0480  Finish Adding/Removing Line or Loading Program
    dw      HOOK5+1             ;  5 LINKER   0485  Update BASIC Program Line Links
    dw      HOOK6+1             ;  6 PRINT    07BC  Execute PRINT Statement
    dw      HOOK7+1             ;  7 FINPRT   0866  End of PRINT Statement
    dw      HOOK8+1             ;  8 TRMNOK   0880  Improperly Formatted INPUT or DATA handler
    dw      _eval_extension     ;  9 EVAL     09FD  Evaluate Number or String
    dw      keyword_to_token    ; 10 NOTGOS   0536  Converting Keyword to Token
    dw      HOOK11+1            ; 11 CLEAR    0CCD  Execute CLEAR Statement
    dw      _scratch            ; 12 SCRTCH   0BBE  Execute NEW Statement
    dw      HOOK13+1            ; 13 OUTDO    198A  Execute OUTCHR
    dw      HOOK14+1            ; 14 ATN      1985  ATN() function
    dw      HOOK15+1            ; 15 DEF      0B3B  DEF statement
    dw      HOOK16+1            ; 16 FNDOER   0B40  FNxx() call
    dw      HOOK17+1            ; 17 LPTOUT   1AE8  Print Character to Printer
    dw      read_key            ; 18 INCHRH   1E7E  Read Character from Keyboard
    dw      HOOK19+1            ; 19 TTYCHR   1D72  Print Character to Screen
    dw      HOOK20+1            ; 20 CLOAD    1C2C  Load File from Tape
    dw      HOOK21+1            ; 21 CSAVE    1C09  Save File to Tape
    dw      token_to_keyword    ; 22 LISPRT   0598  expanding a token
    dw      _next_statement     ; 23 GONE2    064B  interpreting next BASIC statement
    dw      _run_cmd            ; 24 RUN      06BE  starting BASIC program
    dw      on_error            ; 25 ONGOTO   0780  ON statement
    dw      HOOK26+1            ; 26 INPUT    0893  Execute INPUT, bypassing Direct Mode check
    dw      execute_function    ; 27 ISFUN    0A5F  Executing a Function
    dw      HOOK28+1            ; 28 DATBK    08F1  Doing a READ from DATA
    dw      oper_extension      ; 29 NOTSTV   099E  Evaluate Operator (S3 BASIC Only)

; ------------------------------------------------------------------------------
;  Execute Hook Routine
; ------------------------------------------------------------------------------

fast_hook_handler:
    ex      af,af'              ; save AF
    exx                         ; save BC,DE,HL
    pop     hl                  ; get hook return address
    ld      a,(hl)              ; A = byte (RST $30 parameter)
    add     a,a                 ; A * 2 to index WORD size vectors
    ld      l,a
    ld      h,high(hook_table)
    ld      a,(hl)
    ld      ixl,a
    inc     hl
    ld      a,(hl)
    ld      ixh,a
    exx                         ; Restore BC,DE,HL
    ex      af,af'              ; Restore AF
    jp      (ix)

;-----------------------------------------------------------------------------
; S3 BASIC extensions routines in Auxiliary ROM Page
;-----------------------------------------------------------------------------

_eval_extension:
    call    page_restore_plus     
    jp      eval_extension        

_force_error:                     
    call    page_restore_plus     
    jp      force_error

_main_ext:
    call    page_map_aux          
    jp      s3_main_ext

_next_statement:
    call    page_restore_plus
    jp      exec_next_statement   ; Go do the Statement
                                  
_run_cmd:                         
    call    page_restore_plus     
    jp      run_cmd        
                                  
_stuffh_ext:
    call    page_map_aux          
    jp      s3_stuffh_ext
    
_trap_error:                      
    call    page_restore_plus     
    jp      trap_error            

aux_rom_call:
    call    page_map_aux          
    call    jump_ix
    jp      page_restore_plus
                                  
;-----------------------------------------------------------------------------
; Keyboard Decode Tables for S3 BASIC with Extended Keyboard Support
; $2F00 - $2FFF
;-----------------------------------------------------------------------------

    assert !($2EFF<$)   ; ROM full!
    dc $2F00-$,$76

    include "keytable.asm"

;-----------------------------------------------------------------------------
; Verify ROM is correct length
;-----------------------------------------------------------------------------

    assert !($3000<>$)   ; Incorrect ROM! length

;-----------------------------------------------------------------------------
; plusBASIC Statements, Functions, and Operators
;-----------------------------------------------------------------------------

    phase   $C000     ;Assemble in ROM Page 1 which will be in Bank 3

    include "gfxjump.asm"
    ; dispatch and error tables align to 256 byte boundary
    include "dispatch.asm"      ; Statement/Function dispatch tables and routiness
    include "error.asm"         ; Error lookup table, messages and handling routines
    include "args.asm"          ; ARGS statement and function
    include "basic80.asm"       ; Statements and functions from MBASIC 80
    include "draw.asm"          ; Bitmap drawing statements and functions
    include "enhanced.asm"      ; Enhanced stardard BASIC statements and functions
    include "evalext.asm"       ; EVAL extension - hook 9
    include "extended.asm"      ; Statements and functions from Aquarius Extended BASIC
    include "fileio.asm"        ; Disk and File I/O statements and functions
    include "graphics.asm"      ; Graphics statements and functions
    include "plus.asm"          ; plusBASIC unique statements and functions
    include "usbbas.asm"        ; Statements and functions from USB BASIC
    include "shared.asm"        ; Shared subroutines

    ; Graphics modules
    include "gfx.asm"           ; Main graphics module
    include "color.asm"         ; Color palette module
    include "common.asm"        ; Shared graphics subroutines
    include "screen.asm"        ; Text screen graphics subroutines
    include "sprite.asm"        ; Sprite graphics module
    include "tile.asm"          ; Tile graphics module

    assert !($FFFF<$)   ; ROM full!

    free_rom_16k = $10000 - $

    dc $10000-$,$76

    dephase

;-----------------------------------------------------------------------------
; Auxiliary ROM Routines
;-----------------------------------------------------------------------------

    phase   $C000     ;Assemble in ROM Page 1 which will be in Bank 3

    include "editor.asm"        ; Advanced line editor
    include "s3hooks.asm"       ; S3 BASIC direct mode hooks  
    include "screen_aux.asm"    ; Auxiliary 

    free_rom_8k = $E000 - $

    dc $E000-$,$76

      

    end

