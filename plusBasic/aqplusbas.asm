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
    jp      irq_handler     ; $2009 interrupt haandler
    jp      _warm_boot      ; $200C Called from main ROM for warm boot
    jp      _scan_label     ; $200F Called from GOTO and RESTORE
    jp      _keyread        ; $2012 Called from COLORS
    jp      _ctrl_keys      ; $2015
    jp      _iscntc_hook    ; $2018
    jp      _main_ext       ; $201B MAINX: Save Line# Flag`
    jp      _stuffh_ext     ; $201E Check for additional tokens when stuffing
    jp      clear_default   ; $2021 Clear both text screens
    jp      _check_topmem   ; $2024 Verify TOPMEM is in Bank 2
    jp      _ptrget_hook    ; $2027 Allow _Alphanumunder sftar var name
    jp      _stuff_label    ; $202A Don't tokenize label at beginning of Line (STFLBL)
    jp      _skip_label     ; $202D Skip label at beginning of line (SKPLBL)
    jp      _skip_on_label  ; $2030 Skip label in ON GOTO
    jp      _s3_string_ext  ; $2033 Don't capitalize letters between single quotes (STRNGX)
    jp      _sounds_hook    ; $2036 Adjust SOUNDS for turbo mode
    jp      _ttymove_hook   ; $2039 TTYMOV extension - set screen colors if SYSCTRL bit set
    jp      _scroll_hook    ; $203C SCROLL extension - scroll color memory if SYSCTRL bit set
    jp      _line_edit      ; $203F Advanced line editor
    jp      _inlin_hook     ; $20?? Jump from INLIN for command history recall
    jp      _inlin_done     ; $20?? Jumped from FININL to save command to history

plus_text:
    db "plusBASIC "
plus_version:
    db "v0.18u",0
plus_len   equ   $ - plus_text

auto_cmd:
    db      'RUN "'
auto_text
    db      "autoexec"
auto_len = $ - auto_text
    db      $0D
auto_desc
    dw      auto_len,auto_text

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

; Fill BASIC Buffers Page with zeroes from 0 to $2FFF
; leaving custom character set buffer untouched
    call    page_map_basbuf
    ld      hl,$C000
    ld      bc,CHRSETBUF-1
    call    sys_fill_zero

    call    page_restore_plus
    call    spritle_clear_all     ; Clear all sprite properties

    call    page_map_aux
    jp      do_coldboot
coldboot_done:    
    call    page_restore_plus

    jp      INITFF                ; Continue in ROM

;-----------------------------------------------------------------------------
; Intercept WRMCON call
;-----------------------------------------------------------------------------
_warm_boot:
    ld      a, ROM_EXT_PG         ; Page 1 ROM
    out     (IO_BANK3), a         ; into Bank 3
    jp      WRMCON                ; Go back to S3 BASIC


;-----------------------------------------------------------------------------
; Default Interrupt Handler
; Save registers, Calls address in BASINTJP
; Restores registers and re-enables interrupts on return
;-----------------------------------------------------------------------------
; ; ToDo: Save stack position, so interrupt can jump back if needed
irq_handler:
    push    af
    ld      a,(IRQACTIVE)
    or      a
    jp      z,.stop_irqs
    push    bc
    push    de
    push    hl

    call    _timer
    call    BASINTJP
    ld      a,IRQ_VBLANK
    out     (IO_IRQSTAT),a
    pop     hl
    pop     de
    pop     bc
    pop     af
    ei
    reti

.stop_irqs
    out     (IO_IRQMASK)
    pop     af
    reti

_timer:
    call    timer_tick
    ret     nc
    ld      a,(BASYSCTL)
    or      $80
    ld      (BASYSCTL),a
    ret

    assert !($20FF<$)   ; Overflow into Kernel jump table

;=====================================================================================
; KERNEL JUMP TABLE GOES HERE
;=====================================================================================
    dc $2100-$,$76

    include "kernel.asm"

;-----------------------------------------------------------------------------
; Issue OV Error if TOPMEM will put stack in Bank 1
; Called from CLEARS
; Input: HL = Bottom of String Space
;-----------------------------------------------------------------------------
_check_topmem:
    push    hl                    ; Stack = StrBottom, RtnAdr
    ld      bc,-512               ; 
    add     hl,bc                 ; HL = StrBottom - 512
    ld      de,$8000+1024         ; Leave 1024 bytes for stack
    rst     COMPAR                ; If new TOPMEM < Minimum
    jp      c,OVERR               ;   Overflow error
    ld      (TOPMEM),hl           ; Else Set TOPMEM
    pop     hl                    ;   HL = StrBottom; Stack = RtnAdr
    ld      (STRSPC),hl           ;   Set bottom of string space
    ret                           ;   and Return

;-----------------------------------------------------------------------------
; Extended line editor function keys
; Jumped to from INLNC
; On entry: A,C = character typed, B = input buffer character count
;-----------------------------------------------------------------------------
_ctrl_keys:
    cp      ' '                   ; 
    jr      c,.is_ctrl            ; If >= ' ' and and < DEL
    cp      $7F                     
    jp      c,GOODCH              ;    Stuff in Input Buffer
.is_ctrl    
    call    page_map_aux
    jp      s3_ctrl_keys

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

;;; Todo: SET BREAK ON/OFF - BASYSCTL bit 5    
_check_ctrl_c:    
    ld      a,(BASYSCTL)
    and     $20
    jp      z,CNTCCN
    call    INCHRH
    or      a
    ret

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
; Double SOUNDS delay timer if in Turbo Mode
;-----------------------------------------------------------------------------
_sounds_hook:
    in      a,(IO_SYSCTRL)
    and     SYSCTRL_TURBO
    jr      z,.not_turbo
    ld      hl,2
    add     hl,de
    add     hl,de                 ; HL = DE + DE
    ex      de,hl                 ; DE = HL
.not_turbo
    ; Normal: 1102 Hz [3.579545 MHz / (149 + 3100) cycles] - Divisor 3249 
    ; New Turbo: 1105 Hz [7.157090 Mhz / (149 + 6324) cycles] - Divisor 6473
    ; Old Turbo: 1127 Hz [7.157090 Mhz / (149 + 6200) cycles] - Divisor 6349
    ; Unmodified: 2203 Hz  [7.157090 Mhz / (149 + 3100) cycles] - Divisor 3249 
    jp      SOUNDS                ; Total wavelength: 148 + DE * 62 cycles 

;-----------------------------------------------------------------------------
; Return BASIC Version
; Input: HL: String Buffer address
; Output: BC: Version String Length
; Clobbers: A,DE
;-----------------------------------------------------------------------------
sys_ver_basic:
    ex      de,hl                 ; DE = BufAdr
    ld      hl,plus_version       ; HL = VerAdr
    call    string_copy           ; Copy VerStr to StrBuf
    ex      de,hl                 ; HL = BufAdr
    ret

;-----------------------------------------------------------------------------
; Fast Fill memory
; Input: A: Fill Byte
;       BC: Byte Count
;       HL: Start Address
; Clobbers: BC, DE, HL
;-----------------------------------------------------------------------------
sys_fill_zero:
    xor     a
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
;;; ToDo: When starting cart from power up, clear BASIC RAM

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

    ; Turn off Interrupts, Reset Screen, and Remove Hook
    di
    xor     a
    out     (IO_IRQMASK),a
    out     (IO_KEYBUFF),a
    inc     a
    out     (IO_VCTRL)
    ld      hl,NOHOOK
    ld      (HOOK),hl

    ; Reinit stack pointer
    ld      sp, $38A0

    ; Start ROM
    jp      $E010

;-----------------------------------------------------------------------------
; Enable VBLANK Interrupts
; Input: B: IRQ Routine Bit(s)
; Clobbers: A
;-----------------------------------------------------------------------------
enable_vblank_irq:
    ld      a,(IRQACTIVE)
    or      b
    ld      (IRQACTIVE),a
    im1
    ei
    ld      a,IRQ_VBLANK
    out     (IO_IRQMASK),a        ; Turn on VBLANK interrupts
    ret



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
    ld      hl,HOOK2+1            ; Make s3direct_mode return to HOOK2+1
    push    hl                  
    call    page_map_auxrom
    jp      s3direct_mode

reset_screen:
    ld      a,VCTRL_TEXT_EN
    out     (IO_VCTRL),a
    call    reset_palette

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
;    jp      key_read_ascii        ; Skip autotype for now
    exx
.autotype
    ld      hl,(RESPTR)
    ld      a,h
    or      a
    jr      z,.readkey
    ld      c,IO_BANK3
    in      b,(c)
    cp      $C0
    jr      c,.not_fkey
    ld      a,BAS_BUFFR
    out     (c),a
.not_fkey
    inc     hl
    ld      a,(hl)
    ld      (RESPTR),hl
    or      a
    jp      nz,.done
    xor     a
    ld      (RESPTR+1),a
.done
    out     (c),b
    exx
    ret
.readkey
    exx
    jp      key_read_ascii    ; Read key from keyboard and return


;-----------------------------------------------------------------------------
; Hook 5: Push MAIN onto stack so modified CHEAD will return to it
;-----------------------------------------------------------------------------
_linker_hook:
    ld    de,MAIN                 ; DE will get trashed anyway
    push  de                      ; Make MAIN the return address
    jp    LINKIT                  ; Link the lines

;-----------------------------------------------------------------------------
; Invoke advanced line editor
; Currently writtec to be executed via BASIC CALL for development
;-----------------------------------------------------------------------------
_line_edit:
    push    hl
    ld      hl,(TOPMEM)       ; BufAdr = Top of string space
    ld      c,0               ; C = ChrCnt
    call    page_map_aux
    call    edit              ; Edit the line
    pop     hl
    ret

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
    dec     hl                    ; Back up in case of space
    rst     CHRGET                ; Reget current character
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
    ld      c,(hl)                ; BC = line#
    inc     hl
    ld      b,(hl)
    inc     hl
    ld      (OLDLIN),bc           ; Save it
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
    ld      hl,(TEMP3)            ; Get Link to next line
    ld      a,h                   ; If link to next line is $0000
    or      l                     ;   End of program
    jp      z,ULERR               ;   So Undefined Line error
    jr      .line_loop            ; Scan the next line
.found_it
    pop     af                    ; Get return address
    cp      $06
    jr      nz,.not_goto          ; If we came from GOTO
    ld      bc,(OLDLIN)           ;   Retrieve the line #
    ld      (CURLIN),bc           ;   and make it the current line
    ret                           ;   Return to NEWSTT
.not_goto
    cp      $05
    jr      nz,.not_list          ; If we came from LIST
    ld      bc,(TEMP2)            ;   BC = Pointer to Line
    jp      LIST3                 ;   Start listing
.not_list
    cp      $0C                   ; If we came from RESTORE
    jp      z,BGNRST              ;   Load DATPTR, HL = Text Pointer, and Return
    ex      de,hl                 ; HL = New text pointer
    ld      de,(TEMP2)            ; DE = Pointer to Line
    jp      reset_trap            ; Finish ON ERROR GOTO

.not_label:
    jp      SCNLIN                ;   Scan line number and return to GOTO

.check_colon:
    cp      ' '                   ; If space
    jr      z,.ret_zero           ;
    cp      ','                   ; or comma
    jr      z,.ret_zero           ;
    cp      ':'                   ; or colon
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
; Output: A: Result
;        E: String Length
;       DE: Address of Terminator
;       HL: Buffer Address
; Clobbers: B
;-----------------------------------------------------------------------------
bas_read_to_buff:
    ld      hl,FBUFFR             ; Use FBUFFR for now
jump_ix:
    jp      (ix)                  ; Execute routine and return

do_cls:
    ld      a,(BASYSCTL)
    rra     
    jr      nc,clear_default
    ld      a,(SCOLOR)
    jr      clear_home
clear_default:
    ld      a,6                   ; default to black on cyan
;-----------------------------------------------------------------------------
; Clear Screen and init cursor
; Input: A: Color Attributes
;-----------------------------------------------------------------------------
clear_home:
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
    ld      de,(LINLEN)
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

_ttychr_hook:
    push    af
    cp      11
    jr      nz,.not_cls
    ld      a,(BASYSCTL)
    and     $FE
    ld      (BASYSCTL),a
.not_cls
    in      a,(IO_VCTRL)
    and     VCRTL_80COL_EN
    jp      nz,ttychr80pop
    pop     af
    jp      TTYCH
    
_ttymove_hook:
    ld      hl,(CURRAM)
    ld      a,(BASYSCTL)
    rra     
    ret     nc
    ld      a,(SCOLOR)            ; Get screen color
    set     2,h
    ld      (hl),a                ; Write to color RAM
    res     2,h
    ret     

_scroll_hook:
    push    af
    ld      a,(BASYSCTL)
    rra     
    jr      nc,.nocolor
    ld      bc,920                ; Move 23 * 40 bytes
    ld      de,COLOR+40           ; To Row 1 Column 0
    ld      hl,COLOR+80           ; From Row 2 Column 1
    ldir                          ;
    ld      a,(SCOLOR)
    ld      b,40                  ; Loop 40 Times
    ld      hl,COLOR+961          ; Starting at Row 23, Column 0
.loop:      
    ld      (hl),a                ; Put Space
    inc     hl                    ; Next Column
    djnz    .loop                 ; Do it again
.nocolor
    pop     af
    jp      SCROLL
    
;-----------------------------------------------------------------------------
; 80 column screen driver
;-----------------------------------------------------------------------------
    include "text80.asm"

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

    include "tokens.asm"        ; Keyword lists and tokenize/expand routines


free_rom_2k = hook_table - $

; ------------------------------------------------------------------------------
;  Hook Jump Table
; ------------------------------------------------------------------------------

    assert !($2EFF<$)   ; ROM full!
    dc $2F00-$,$76

; BASIC Hook Jump Table
; 58 Bytes
hook_table:                     ; ## caller   addr  performing function
    dw      _trap_error         ;  0 ERROR    03DB  Initialize Stack, Display Error, and Stop Program
    dw      _force_error        ;  1 ERRCRD   03E0  Print Error Message
    dw      direct_mode         ;  2 READY    0402  BASIC command line (immediate mode)
    dw      HOOK3+1             ;  3 EDENT    0428  Save Tokenized Line
    dw      HOOK4+1             ;  4 FINI     0480  Finish Adding/Removing Line or Loading Program
    dw      _linker_hook        ;  5 LINKER   0485  Update BASIC Program Line Links
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
    dw      _ttychr_hook        ; 19 TTYCHR   1D72  Print Character to Screen
    dw      HOOK20+1            ; 20 CLOAD    1C2C  Load File from Tape
    dw      HOOK21+1            ; 21 CSAVE    1C09  Save File to Tape
    dw      token_to_keyword    ; 22 LISPRT   0598  expanding a token
    dw      _next_statement     ; 23 GONE2    064B  interpreting next BASIC statement
    dw      _run_cmd            ; 24       06BE  starting BASIC program
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

; $202A Don't tokenize label at beginning of Line (STFLBL)
_stuff_label
    call    page_map_aux
    jp      stuff_label

; $202D Skip label at beginning of line (SKPLBL)
_skip_label
    call    page_map_aux
    jp      skip_label

; $2030 Skip label in ON GOTO
_skip_on_label
    call    page_restore_plus
    jp      skip_on_label

_s3_string_ext
    call    page_map_aux
    jp      s3_string_ext

;-----------------------------------------------------------------------------
; Pad ROM
;-----------------------------------------------------------------------------
    assert !($2FFF<$)   ; ROM full!
    dc $3000-$,$76

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
    include "baslines.asm"      ; (De)tokenize, add, insert, delete program lines
    include "error.asm"         ; Error lookup table, messages and handling routines
    include "args.asm"          ; ARGS statement and function
    include "basic80.asm"       ; Statements and functions from MBASIC 80
    include "draw.asm"          ; Bitmap drawing statements and functions
    include "enhanced.asm"      ; Enhanced stardard BASIC statements and functions
    include "evalext.asm"       ; EVAL extension - hook 9
    include "extended.asm"      ; Statements and functions from Aquarius Extended BASIC
    include "files.asm"         ; Disk and File I/O machine assembly routines
    include "fileio.asm"        ; Disk and File I/O statements and functions
    include "graphics.asm"      ; Graphics statements and functions
    include "misc.asm"          ; Miscellaneous subroutines
    include "play.asm"
    include "plus.asm"          ; plusBASIC unique statements and functions
    include "shared.asm"        ; Shared subroutines
    include "usbbas.asm"        ; Statements and functions from USB BASIC

    ; Graphics modules
    include "gfx.asm"           ; Main graphics module
    include "color.asm"         ; Color palette module
    include "common.asm"        ; Shared graphics subroutines
    include "screen.asm"        ; Text screen graphics subroutines
    include "sprite.asm"        ; Sprite graphics module
    include "tile.asm"          ; Tile graphics module
    include "sound.asm"         ; Sound and Music


    assert !($FFFF<$)   ; ROM full!

    free_rom_16k = $10000 - $

    dc $10000-$,$76

    dephase

;-----------------------------------------------------------------------------
; Auxiliary ROM Routines
;-----------------------------------------------------------------------------

    phase   $C000     ;Assemble in ROM Page 1 which will be in Bank 3

    include "coldboot.asm"      ; Cold boot code
    include "dos_aux.asm"
    include "editor.asm"        ; Advanced line editor
    include "esp_aux.asm"       ; ESP routines in auxiliary ROM
    include "s3hooks.asm"       ; S3 BASIC direct mode hooks
    include "screen_aux.asm"    ; Auxiliary

    free_rom_8k = $E000 - $

    dc $E000-$,$76

    end


