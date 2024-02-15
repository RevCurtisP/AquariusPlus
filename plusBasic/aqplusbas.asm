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

;Internal Jump Table: S3BASIC interface
    org     $2000
    jp      _reset          ; $2000 Called from main ROM at reset vector
    jp      _coldboot       ; $2003 Called from main ROM for cold boot
    jp      _start_cart     ; $2006
    jp      irq_handler     ; $2009 interrupt handler
    jp      _warm_boot      ; $200C Called from main ROM for warm boot
    jp      _keyread        ; $200F Called from COLORS
    jp      _sounds_hook    ; $2012 SOUNDX Adjust SOUNDS for turbo mode
    jp      _ttymove_hook   ; $2015 TTYMOX TTYMOV extension - set screen colors if SYSCTRL bit set
    jp      _scroll_hook    ; $2018 SCROLX SCROLL extension - scroll color memory if SYSCTRL bit set
    jp      _start_screen   ; $201B RESETX Start-up screen extension

    dc $2030-$,$76
    
    rst     HOOKDO                ; $2030 SCNLBL scan_label: Scan line label or line number
    byte    30                    ; 
    jp      SCNLIN                
      
    rst     HOOKDO                ; $2035 XFUNKY _ctrl_keys: Evaluate extended function keys
    byte    31                    ;        
    jp      CHKFUN                ;

    rst     HOOKDO                ; $203A XCNTC  _iscntc_hook: Intercept Ctrl-C check
    byte    32      
    jp      CNTCCN                
      
    rst     HOOKDO                ; $203F XMAIN  _main_ext: Save Line# Flag in TEMP3
    byte    33      
    jp      SCNLIN                
      
    rst     HOOKDO                ; $2044 XSTUFF _stuffh_ext: Check for additional tokens when stuffing
    byte    34      
    cp      DATATK-':'            ; If not DATA
    jp      nz,NODATT             ;    Check for REM
    jp      COLIS                 ; Set up for DATA
    
    rst     HOOKDO                ; $204E XCLEAR _check_topmem: Verify TOPMEM is in Bank 2
    byte    35    
    ld      (TOPMEM),hl           ; Set up new top of stack
    ret                           ; Continue clears

    rst     HOOKDO                ; $2054 XPTRGT ptrget_hook: Allow _Alphanumunder sftar var name
    byte    36    
    rst     CHRGET                ; Get next character
    jp      c,ISSEC               ; If digit, save it and eat rest of digits
    jp      CHKLET                ; Else check for letter

    rst     HOOKDO                ; $205D SKPLBL  skip_label: Skip label at beginning of line (SKPLBL)
    byte    37    
    ld      (CURLIN),hl           ; Set CURLIN to curent line#    
    jp      GONCON                ; Keep going to GONE

    rst     HOOKDO                ; $2065 SKPLOG  skip_on_label: Skip label in ON GOTO
    byte    38
    jp      LINGET

    rst     HOOKDO                ; $206A STRNGX  _string_ext      Don't capitalize letters between single quotes (STRNGX)
    byte    39
    jp      z,STRNG               ; If A is '"', process string literal
    jp      STRNGR                ; Else carry on

just_ret:
    ret

plus_text:
    db "plusBASIC "
plus_version:
    db "v0.21v"
ifdef coredump
    db "_coredump"
endif
    db 0
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
;-----------------------------------------------------------------------------5
_reset:
    ; Disable interrupts and turbo mode
    di
    xor     a
    out     (IO_IRQMASK),a
    out     (IO_SYSCTRL),a
    inc     a
    out     (IO_VCTRL),a

    ; Set up temp stack in text line buffer
    ld      sp, $38A0

ifdef coredump
    ; Core dump check
    ld      a,ROM_EXT_RO
    out     (IO_BANK3),a
    jp      core_dump
endif

init_banks:

    ; Initialize Banks 1 to 3
    ld      a,ROM_SYS_PG | BANK_OVERLAY | BANK_READONLY
    out     (IO_BANK0), a
    ld      a,RAM_BAS_1
    out     (IO_BANK1), a
    ld      a,RAM_BAS_2
    out     (IO_BANK2), a

    ; Call routines in Aux ROM
    ld      a,ROM_AUX_RO
    out     (IO_BANK3), a
    call    screen_reset          ; Init video mode

    ; Initialize ESP
    ld      a, ESPCMD_RESET
    call    esp_cmd

    ; Turn on Keyboard Buffer
    ld      a,KB_ENABLE | KB_ASCII
    call    key_set_keymode

    ; Initialize Bank 3
    ld      a, ROM_CART                 ; Cartridge Port
    out     (IO_BANK3), a

    ; Back to system ROM init
    jp      JMPINI

;-----------------------------------------------------------------------------
; Cold boot entry point
; Executed on RESET if no cartridge detected
;-----------------------------------------------------------------------------
_coldboot:

; Fill BASIC Buffers Page with zeroes from 0 to $2FFF
; leaving custom character set buffer untouched
    call    page_set_basbuf
    ld      hl,$C000
    ld      bc,DEFCHRSET-1
    call    sys_fill_zero

    call    init_chrsets
    call    load_ptplay
    call    page_set_plus
    call    spritle_clear_all     ; Clear all sprite properties
    jp      do_coldboot

; ROM Signature
    assert !($20F6<$)   ; Overflow into Kernel jump table
    dc $20F7-$,$76
    byte    "Aquarius+"

; Start Kernel jump table at $2100
    include "kernel.asm"

;-----------------------------------------------------------------------------
; Intercept WRMCON call
;-----------------------------------------------------------------------------
_warm_boot:
    ld      a, ROM_EXT_RO         ; Page 1 ROM
    out     (IO_BANK3), a         ; into Bank 3
    call    esp_close_all
    call    init_bas_fdesc
    ld      a,128
    out     (IO_PCMDAC),a
    jp      WRMCON                ; Go back to S3 BASIC

;-----------------------------------------------------------------------------
; Default Interrupt Handler
; Save registers, Calls address in BASINTJP
; Restores registers and re-enables interrupts on return
;-----------------------------------------------------------------------------
; ; ToDo: Save stack position, so interrupt can jump back if needed
irq_handler:
    push    af                    ; Stack = AF
    ld      a,(IRQACTIVE)
    or      a
    jp      z,.stop_irqs
    ex      af,af'
    push    af
    ex      af,af'
    push    bc                    ; Stack = BC, AF
    push    de                    ; Stack = DE, BC, AF
    push    hl                    ; Stack = HL, DE, BC, AF

    rra                           ; Carry = IRQ_TIMER
    call    c,_timer
    rra                           ; Carry = IRQ_PT3PLAY
    call    c,_pt3tick
    
    call    BASINTJP
    ld      a,IRQ_VBLANK
    out     (IO_IRQSTAT),a

    pop     hl                    
    pop     de
    pop     bc
    pop     af
    ex      af,af'
    pop     af
    ei
    reti

.stop_irqs
    out     (IO_IRQMASK)
    pop     af
    reti

_timer:
    ex      af,af'      
    call    timer_tick
    ex      af,af'
    ret     

_pt3tick
    ld      iy,pt3tick
pt3call:
    push    ix
    in      a,(IO_BANK1)
    push    af                    ; Stack = Bnk1pg, RtnAdr
    ld      a,PT3_BUFFR
    out     (IO_BANK1),a
    in      a,(IO_BANK3)
    push    af                    ; Stack = Bnk3pg, Bnk1pg, RtnAdr
    ld      a,PT3_BUFFR
    out     (IO_BANK3),a
    call    (jump_iy)
    call    nz,.pt3done
    pop     af                    ; A = Bnk3pg; Stack = Bnk1pg, RtnAdr
    out     (IO_BANK3),a
    pop     af                    ; A = Bnk1pg; Stack = RtnAdr
    out     (IO_BANK1),a
    pop     ix
    ret

.pt3done
    call    pt3_reset
    ld      a,(EXT_FLAGS)
    and     PT3_LOOPS
    ret     z
    jp      pt3_start

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
; Skip start screen and cold boot if : was pressed
; Called from RESET
;-----------------------------------------------------------------------------
_start_screen:
    call    key_read_ascii        ; See if key pressed
    cp      13                    ; If colon 
    jp      z,COLDST              ;   Straight to cold start
    ld      de,SCREEN+417         ; DE = Screen location for "BASIC"
    jp      RESETC


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
    call    page_set_aux
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
    inc     sp                    ; Discard CALL XCNTC return address
    inc     sp
    call    CNTCCN                ; Check for Control-C
    ret     z                     ; Return if no keypress
    cp      'D'-64                ; If Ctrl-D
    jr      z,.turbo_off          ;   Disable Turbo Mode
    cp      'F'-64                ; If not Ctrl-F
    jp      nz,ISCNTS             ;   Back to INCNTC
    byte    $3E                   ; LD A,$AF to Enable Turbo Mode
.turbo_off
    xor     a
    jp      sys_turbo_mode

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
sys_ver_plusbasic:
    ex      de,hl                 ; DE = BufAdr
    ld      hl,plus_version       ; HL = VerAdr
    call    str_copy              ; Copy VerStr to StrBuf
    ex      de,hl                 ; HL = BufAdr
    ret

;-----------------------------------------------------------------------------
; Return BASIC Version
; Input: HL: String Buffer address
; Output: BC: Version String Length
; Clobbers: A,DE
;-----------------------------------------------------------------------------
sys_ver_s3basic:
    push    hl                    ; Stack = BufAdr, RtnAdr
    push    hl                    ; Stack = BufAdr, BufAdr, RtnAdr
    ld      de,S3VER              ; DE = Version bytes
    call    .str_byte             ; YY
    call    .dash_byte            ; -MM
    call    .dash_byte            ; -DD
    ld      a,'r'
    call    .prefix_byte          ; rREV
    xor     a
    ld      (hl),a                ; Terminate string
    pop     de                    ; DE = BufAdr; Stack = BufAdr, RtnAdr           ; 
    sbc     hl,bc                 ; HL = Length
    ld      b,h 
    ld      c,l                   ; BC = Length
    pop     hl                    ; HL = BufAdr; Stack = RtnAdr
    ret

.dash_byte
    ld      a,'-'
.prefix_byte
    ld      (hl),a                ;   Add dash to 
    inc     hl
.str_byte
    ld      a,(de)                ;   Get BCD byte
    call    byte_to_hex           ;   Convert BCD to ASCII
    inc     de                    ;   Bump to next BCD byte
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
; Fill memory with integer
; Input:BC: Byte Count
;       DE: Fill value
;       HL: Start Address
; Clobbers: A, BC, DE, HL
;-----------------------------------------------------------------------------
sys_fill_word:
    ld      a,b
    or      c                     ; If BC = 0
    ret     z                     ;   Return
    ld      (hl),e                ; Write LSB
    inc     hl                    ; Bump Address
    ld      (hl),d                ; Write MSB
    inc     hl                    ; Bump Address
    dec     bc                    ; Count down
    jr      sys_fill_word         ; Do it again


default_chrset:
   xor      a
;-----------------------------------------------------------------------------
; Copy selected character set into Character RAM
; Input: A: Character set (0: Default, 1: Custom)
;-----------------------------------------------------------------------------
select_chrset:
    ld      hl,DEFCHRSET          ; If A = 0
    or      a                     ;   Copy standard character set
    jr      z,.copy               ; Else
.alt
    ld      hl,ALTCHRSET          ;   Copy Custom Character Set
.copy
    ld      a,BAS_BUFFR           
    jr      copy_char_ram         

;-----------------------------------------------------------------------------
; Character ROM buffers initialization
;-----------------------------------------------------------------------------
init_chrsets:
; Copy standard character set into buffer
    ld      hl,.defdesc
    ld      iy,file_load_defchrs
    call    aux_call
    ld      hl,.altdesc
    ld      iy,file_load_altchrs
    jp      aux_call

.defset:
    byte    "esp:default.chr"
.defdesc:
    word    $-.defset,.defset
.altset:
    byte    "esp:latin1b.chr"
.altdesc:
    word    $-.altset,.altset

;-----------------------------------------------------------------------------
; Load PT3 player
;-----------------------------------------------------------------------------
load_ptplay:
    ld      a,PT3_BUFFR           ; Page
    ld      l,0
    call    page_fill_all_byte    ; Zero out buffer

    ld      a,PT3_BUFFR           ; Page
    ld      bc,$4000              ; Load up to 16k
    ld      de,PT3_BASE           ; Start address
    ld      hl,.ptdesc
    ld      iy,file_load_paged
    jp      aux_call

.ptplay:
    byte    "esp:ptplay.bin"
.ptdesc:
    word    $-.ptplay,.ptplay

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

    ; Map destination RAM in bank2
    ld      a, RAM_BAS_3
    out     (IO_BANK2), a

    ; Copy ROM cartridge to RAM
    ld      de, $8000
    ld      hl, $C000
    ld      bc, $4000
    ldir

descramble_rom:
    ; Map RAM in bank3
    ld      a, RAM_BAS_3
    out     (IO_BANK3), a

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

    cp      $00
    jr      z,.exec_cart

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

.exec_cart:
    ; Reinit banks
    ld      a, RAM_BAS_1
    out     (IO_BANK1), a
    ld      a, RAM_BAS_2
    out     (IO_BANK2), a

ifdef coredump
    ld      a,ROM_EXT_RO
    out     (IO_BANK3),a
    jp      RESET
endif


    ; Bank3 -> readonly
    ld      a, RAM_BAS_3 | BANK_READONLY
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

    ; Clear BASIC RAM
    xor     a                     ; Fill with 0
    ld      hl,$3900              ; From beginning of BASIC RAM
    ld      bc,$C000-$3900        ; to end of BASIC RAM
    call    sys_fill_mem          ; A = SCRMBL; Stack = RtnAdr

    ; Start ROM
    jp      XINIT

;-----------------------------------------------------------------------------
; Enable VBLANK Interrupts
; Input: B: IRQ Routine Bit(s)
; Clobbers: A
;-----------------------------------------------------------------------------
enable_vblank_irq:
    call    set_vblank_irq
    im1
    ei
    ld      a,250
    out     (IO_VIRQLINE),a
    ld      a,IRQ_VBLANK
    out     (IO_IRQMASK),a        ; Turn on VBLANK interrupts
    ret

set_vblank_irq:
    ld      a,(IRQACTIVE)
    or      b
    ld      (IRQACTIVE),a
    ret     

;-----------------------------------------------------------------------------
; Disable a VBLANK Interrupt
; Input: B: IRQ Routine Bit(s)
; Clobbers: A,C
;-----------------------------------------------------------------------------
clear_vblank_irq:
    ld      a,$FF
    xor     b
    ld      c,a
    ld      a,(IRQACTIVE)
    and     c
    ld      (IRQACTIVE),a
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
; Invoke advanced line editor
; Currently writtec to be executed via BASIC CALL for development
;-----------------------------------------------------------------------------
_line_edit:
    push    hl
    call    get_linbuf_addr   ; HL = Line Buffer addresx
    ld      c,0               ; C = ChrCnt
    call    edit              ; Edit the line
    pop     hl
    ret

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

jump_iy:
    jp      (iy)

do_cls:
    ld      a,(BASYSCTL)
    rla     
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

_ttymove_hook:
    ld      hl,(CURRAM)
    ld      a,(HOOK+1)
    cp      $2F                   ; If plusBASIC not hooked
    jp      nz,TTYMOP             
    ld      a,(BASYSCTL)          ; or color PRINT not enabled
    rla                           ;   Continue normally
    jp      nc,TTYMOP             ; Else
    ld      a,(SCOLOR)            ;   Get screen color
    set     2,h
    ld      (hl),a                ;   Write to color RAM
    res     2,h
    jp      TTYMOP                ;   and continue

_scroll_hook:
    push    af
    ld      a,(HOOK+1)
    cp      $2F                   ; If plusBASIC not hooked
    ld      a,(BASYSCTL)          ; or color PRINT not enabled
    rla                           ;   Continue normally
    jr      nc,.nocolor           ; Else
    ld      bc,920                ;   Move 23 * 40 bytes
    ld      de,COLOR+40           ;   To Row 1 Column 0
    ld      hl,COLOR+80           ;   From Row 2 Column 1
    ldir                          ; 
    ld      a,(SCOLOR)  
    ld      b,40                  ;   Loop 40 Times
    ld      hl,COLOR+961          ;   Starting at Row 23, Column 0
.loop:        
    ld      (hl),a                ;   Put Space
    inc     hl                    ;   Next Column
    djnz    .loop                 ;   Do it again
.nocolor
    pop     af
    jp      SCROLL
    
;-----------------------------------------------------------------------------
; 80 column screen driver
;-----------------------------------------------------------------------------
    include "text80.asm"

;-----------------------------------------------------------------------------
; Cold Boot
;-----------------------------------------------------------------------------

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
; Utility routines
;-----------------------------------------------------------------------------
    include "util.asm"

;-----------------------------------------------------------------------------
; Hook 18 - INCHRC (Get character from keyboard)
;-----------------------------------------------------------------------------
_read_key:
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


; ------------------------------------------------------------------------------
;  Hook Jump Table
; ------------------------------------------------------------------------------

    assert !($2EFF<$)   ; ROM full!
    dc $2F00-$,$76

; BASIC Hook Jump Table
; 58 Bytes
hook_table:                     ; ## caller   addr  performing function
    dw      trap_error          ;  0 ERROR    03DB  Initialize Stack, Display Error, and Stop Program
    dw      force_error         ;  1 ERRCRD   03E0  Print Error Message
    dw      direct_mode         ;  2 READY    0402  BASIC command line (immediate mode)
    dw      HOOK3+1             ;  3 EDENT    0428  Save Tokenized Line
    dw      HOOK4+1             ;  4 FINI     0480  Finish Adding/Removing Line or Loading Program
    dw      linker_hook         ;  5 LINKER   0485  Update BASIC Program Line Links
    dw      HOOK6+1             ;  6 PRINT    07BC  Execute PRINT Statement
    dw      HOOK7+1             ;  7 FINPRT   0866  End of PRINT Statement
    dw      HOOK8+1             ;  8 TRMNOK   0880  Improperly Formatted INPUT or DATA handler
    dw      eval_extension      ;  9 EVAL     09FD  Evaluate Number or String
    dw      keyword_to_token    ; 10 NOTGOS   0536  Converting Keyword to Token
    dw      HOOK11+1            ; 11 CLEAR    0CCD  Execute CLEAR Statement
    dw      new_hook            ; 12 SCRTCH   0BBE  Execute NEW Statement
    dw      outdo_hook          ; 13 OUTDO    198A  Execute OUTCHR
    dw      HOOK14+1            ; 14 ATN      1985  ATN() function
    dw      HOOK15+1            ; 15 DEF      0B3B  DEF statement
    dw      FN_FN               ; 16 FNDOER   0B40  FNxx() call
    dw      HOOK17+1            ; 17 LPTOUT   1AE8  Print Character to Printer
    dw      _read_key           ; 18 INCHRH   1E7E  Read Character from Keyboard
    dw      ttychr_hook         ; 19 TTYCHR   1D72  Print Character to Screen
    dw      HOOK20+1            ; 20 CLOAD    1C2C  Load File from Tape
    dw      HOOK21+1            ; 21 CSAVE    1C09  Save File to Tape
    dw      token_to_keyword    ; 22 LISPRT   0598  expanding a token
    dw      exec_next_statement ; 23 GONE2    064B  interpreting next BASIC statement
    dw      run_cmd             ; 24 RUN      06BE  starting BASIC program
    dw      on_error            ; 25 ONGOTO   0780  ON statement
    dw      HOOK26+1            ; 26 INPUT    0893  Execute INPUT, bypassing Direct Mode check
    dw      execute_function    ; 27 ISFUN    0A5F  Executing a Function
    dw      HOOK28+1            ; 28 DATBK    08F1  Doing a READ from DATA
    dw      oper_extension      ; 29 NOTSTV   099E  Evaluate Operator (S3 BASIC Only)
    dw      scan_label          ; 30 SCNLBL   2040  GOTO/GOSUB/LIST/RESTORE/RUN label
    dw      _ctrl_keys          ; 31 XFUNKY   2045  Evaluate extended function keys
    dw      _iscntc_hook        ; 32 XCNTC    204A  Intercept check for Ctrl-C
    dw      main_ext            ; 33 XMAIN    204F  Save Line# Flag`in TEMP3
    dw      _stuffh_ext         ; 34 XSTUFF   2054  Check for additional tokens when stuffing
    dw      _check_topmem       ; 35 XCLEAR   204E  Verify TOPMEM is in Bank 2
    dw      ptrget_hook         ; 36 XPTRGT   2054  Allow _Alphanumunder sftar var name Check for pressed 
    dw      skip_label          ; 37 SKPLBL   205D  Skip label at beginning of line (SKPLBL) Check for pressed 
    dw      skip_on_label       ; 38 SKPLOG   2065  Skip label in ON GOTO Check for pressed 
    dw      _string_ext         ; 39 STRNGX   206A  Don't capitalize letters between single quotes (STRNGX) Check for pressed 

; ------------------------------------------------------------------------------
;  Execute Hook Routine
; ------------------------------------------------------------------------------

fast_hook_handler:
    ex      af,af'              ; save AF
    exx                         ; save BC,DE,HL
    ld      a,ROM_EXT_RO        ; Ensure Extended ROM is paged in
    out     (IO_BANK3),a
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
; Extend ROM assembly callable routines
;-----------------------------------------------------------------------------
str_instr:
    ld      iy,ext_instr
    jr      ext_call    

;-----------------------------------------------------------------------------
; S3 BASIC extensions routines in Auxiliary ROM Page
;-----------------------------------------------------------------------------

_main_ext:
    halt

_next_statement:
    call    page_set_plus
    jp      exec_next_statement   ; Go do the Statement

_run_cmd:
    call    page_set_plus
    jp      run_cmd

_stuffh_ext:
    call    page_set_aux
    jp      s3_stuffh_ext

aux_call:
    call    page_map_auxrom
    call    jump_iy
    jp      page_restore_bank3

ext_call:
    call    page_map_extrom
    call    jump_iy
    jp      page_restore_bank3

aux_rom_call:
    call    page_set_aux
    call    jump_ix
    jp      page_set_plus

_string_ext:
    call    page_set_aux
    jp      s3_string_ext

aux_line_print:
    call    page_set_plus         ; Map in Ext ROM
    call    LINPRT                ; Print the line number
    jp      page_set_aux          ; Remap Aux ROM and return


;-----------------------------------------------------------------------------
; Pad first 4k
;-----------------------------------------------------------------------------
    free_rom_sys = $3000 - $
    assert !($2FFF<$)   ; ROM full!
    dc $3000-$,$76

;-----------------------------------------------------------------------------
; Pad hidden 2k 
;-----------------------------------------------------------------------------
    free_rom_hid = $3800 - $
    assert !($37FF<$)   ; ROM full!
    dc $3800-$,$67

;-----------------------------------------------------------------------------
; 2k RAM for bank 0 overlay
;-----------------------------------------------------------------------------
    dc $4000-$,0
    assert !($4000<>$)   ; Incorrect ROM! length

;-----------------------------------------------------------------------------
; plusBASIC Statements, Functions, and Operators
;-----------------------------------------------------------------------------

    phase   $C000     ;Assemble in ROM Page 1 which will be in Bank 3

    include "tables.asm"        ; Lookup tables aligned to 256 byte boundaries
    include "dispatch.asm"      ; Statement/Function dispatch tables and routiness
    include "error.asm"         ; Error lookup table, messages and handling routines
    include "args.asm"          ; ARGS statement and function
    include "basic80.asm"       ; Statements and functions from MBASIC 80
    include "baslines.asm"      ; (De)tokenize, add, insert, delete program lines
    include "coldboot.asm"      ; Cold boot code
    include "draw.asm"          ; Bitmap drawing statements and functions
    include "editor.asm"        ; Advanced line editor
    include "enhanced.asm"      ; Enhanced stardard BASIC statements and functions
    include "evalext.asm"       ; EVAL extension - hook 9
    include "extended.asm"      ; Statements and functions from Aquarius Extended BASIC
    include "files.asm"         ; Disk and File I/O statements and functions
    include "graphics.asm"      ; Graphics statements and functions
    include "hooks.asm"         ; Extended BASIC hooks
    include "misc.asm"          ; Miscellaneous subroutines
    include "play.asm"
    include "plus.asm"          ; plusBASIC unique statements and functions
    include "shared.asm"        ; Shared subroutines
    include "tokens.asm"        ; Keyword lists and tokenize/expand routines
    include "usbbas.asm"        ; Statements and functions from USB BASIC

    ; Graphics modules
    include "screen.asm"        ; Text screen graphics subroutines
    include "sprite.asm"        ; Sprite graphics module
    include "sound.asm"         ; Sound and Music

    assert !($FFFF<$)   ; ROM full!

    free_rom_ext = $10000 - $

    dc $10000-$,$76

    dephase

;-----------------------------------------------------------------------------
; Auxiliary ROM Routines
;-----------------------------------------------------------------------------

    phase   $C000     ;Assemble in ROM Page 1 which will be in Bank 3

    include "jump_aux.asm"      ; Auxiliary routines jump tables
    include "color.asm"         ; Color palette module
    include "dos.asm"           ; DOS routines
    include "esp_aux.asm"       ; ESP routines in auxiliary ROM
    include "fileio.asm"        ; Disk and File I/O machine assembly routines
    include "gfx.asm"           ; Main graphics module
    include "s3hooks.asm"       ; S3 BASIC direct mode hooks
    include "screen_gfx.asm"    ; Screen graphics routines
    include "screen_swap.asm"   ; Screen buffering routines
    include "tile.asm"          ; Tile graphics module

    free_rom_aux = $10000 - $

    dc $10000-$,$76              

    end


