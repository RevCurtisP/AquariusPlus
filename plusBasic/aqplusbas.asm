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
  
;    include "sbasic.inc"
    include "plus.inc"
    include "screen.inc"

    include "sbasic.asm"


;Internal Jump Table: S3BASIC interface
    org     $2000
    jp      _reset          ; $2000 XPLUS  Called from main ROM at reset vector
    jp      _coldboot       ; $2003 XCOLD  Called from main ROM for cold boot
    jp      just_ret        ; $2006
    jp      just_ret        ; $2009 
    jp      just_ret        ; $200C
    jp      _xkeyread       ; $200F
    jp      just_ret        ; $2012 
    jp      just_ret        ; $2015
    jp      just_ret        ; $2018
    jp      _check_cart     ; $201B RESETX Start-up screen extension

just_ret:
    ret

;-----------------------------------------------------------------------------
; Reset vector
;-----------------------------------------------------------------------------
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
    
    jp      init_banks

;-----------------------------------------------------------------------------
; Intercept WRMCON call
;-----------------------------------------------------------------------------
warm_boot:
    ld      a, ROM_EXT_RO         ; Page 1 ROM
    out     (IO_BANK3), a         ; into Bank 3
    call    esp_close_all
    call    init_bas_fdesc
    ld      a,128
    out     (IO_PCMDAC),a
    jp      WRMCON                ; Go back to S3 BASIC

;-----------------------------------------------------------------------------
; Cold boot entry point
; Executed on RESET if no cartridge detected
;-----------------------------------------------------------------------------
_coldboot:
    ld      a,SYSCTRL_TURBO     ; Enable unlimited turbo mode
    out     (IO_SYSCTRL),a      ; Disabled at the end of do_coldboot

; Fill BASIC Buffers Page with zeroes from 0 to $2FFF
; leaving custom character set buffer untouched
    call    page_set_basbuf
    ld      hl,$C000
    ld      bc,DEFCHRSET-1
    call    sys_fill_zero

    call    page_set_aux
    call    init_chrsets
    call    spritle_reset_all
    call    file_load_pt3play
    call    page_set_plus
    jp      do_coldboot

auto_cmd:
    db      ':RUN "'
auto_text
    db      "autoexec"
auto_len = $ - auto_text
    db      $0D
auto_desc
    dw      auto_len,auto_text

; Null string descriptor
null_desc:
    word    0,null_desc

; plusBASIC cold start messafe
plus_text:
    db "plusBASIC "
plus_version:
    db "v0.70a"
    db 0
plusver_len equ $ - plus_version
plus_len   equ   $ - plus_text

; ROM Signature
    assert !($20F6<$)   ; Overflow into Kernel jump table
    dc $20F7-$,$76
    byte    "Aquarius+"

; Start Kernel jump table at $2100
    include "kernel.asm"


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
    ld      a, ROM_EXT_RO          ; Soft Cartridge
    out     (IO_BANK3), a

    ; Back to system ROM init
    jp      JMPINI


;-----------------------------------------------------------------------------
; Default Interrupt Handler
; Save registers, Calls address in BASINTJP
; Restores registers and re-enables interrupts on return
;-----------------------------------------------------------------------------
irq_handler:
    push    af                    ; Stack = AF
    ld      a,(IRQACTIVE)
    or      a
    jp      z,_stop_irqs
    ex      af,af'
    push    af
    ex      af,af'
    push    bc                    ; Stack = BC, AF
    push    de                    ; Stack = DE, BC, AF
    push    hl                    ; Stack = HL, DE, BC, AF
    exx
    push    bc                    ; Stack = BC',
    push    de                    ; Stack = DE', BC'
    push    hl                    ; Stack = HL', DE', BC'
    push    ix
    push    iy

    ld      hl,0
    add     hl,sp
    ld      (IRQSAVSTK),hl        ; Save Stack Position

    rra                           ; Carry = IRQ_TIMER
    call    c,_timer
    rra                           ; Carry = IRQ_USER
    call    c,_user_irq
    rra                           ; Carry = IRQ_TRACKER
    call    c,_pt3tick
    rra                           ; Carry = IRQ_MOUSE

irq_done:
    ld      a,IRQ_VBLANK
    out     (IO_IRQSTAT),a

    pop     iy
    pop     ix
    pop     hl
    pop     de
    pop     bc
    exx
    pop     hl
    pop     de
    pop     bc
    pop     af
    ex      af,af'
    pop     af
    ei
    reti

_stop_irqs
    out     (IO_IRQMASK)
    pop     af
    reti

_timer:
    ex      af,af'
    call    timer_tick
    ex      af,af'
    ret

_user_irq:
    push    af
    or      a                     ; Clear Zero, Carry flag
    call    BASINTJP
    call    c,userint_disable
    pop     af
    ret

_pt3tick
    ld      iy,pt3tick
    call    pt3call
    ret     z
    ld      iy,track_reset
    call    aux_call
    ld      a,(EXT_FLAGS)
    and     TRK_LOOPS
    ret     z
    jp      pt3_start

pt3call:
    push    ix
    in      a,(IO_BANK1)
    push    af                    ; Stack = Bnk1pg, RtnAdr
    ld      a,TRK_BUFFR
    out     (IO_BANK1),a
    in      a,(IO_BANK3)
    push    af                    ; Stack = Bnk3pg, Bnk1pg, RtnAdr
    ld      a,TRK_BUFFR
    out     (IO_BANK3),a
    call    (jump_iy)
    ex      af,af'
    pop     af                    ; A = Bnk3pg; Stack = Bnk1pg, RtnAdr
    out     (IO_BANK3),a
    pop     af                    ; A = Bnk1pg; Stack = RtnAdr
    out     (IO_BANK1),a
    ex      af,af'
    pop     ix
    ret

;-----------------------------------------------------------------------------
; Issue OV Error if TOPMEM will put stack in Bank 1
; Called from CLEARS
; Input: HL = Bottom of String Space
;-----------------------------------------------------------------------------
check_topmem:
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
; Check for Aquarius+ cartridge and color cycle skip file
; Called from RESET
;-----------------------------------------------------------------------------
_check_cart:

check_aqplus_cart:
    ; Check for Aquarius+ specific cartridge goes here

_start_screen:
    ld      hl,.skipsplash_desc
    ld      de,BUF
    ld      iy,dos_stat
    call    aux_call              ; NZ if file exists
    jp      z,COLDST
    jp      RESET 

.skipsplash_name
    byte    "/system/plusbasic/_skipsplash"
.skipsplash_desc
    word    .skipsplash_desc-.skipsplash_name, .skipsplash_name

;-----------------------------------------------------------------------------
; Extended line editor function keys
; Jumped to from INLNC
; On entry: A,C = character typed, B = input buffer character count
;-----------------------------------------------------------------------------
ctrl_keys:
    cp      ' '                   ;
    jr      c,.is_ctrl            ; If >= ' ' and and <= DEL
    cp      $7F                   ;    If DEL
    jp      z,INLINC              ;      Ignore it
    jp      c,GOODCH              ;    Else Stuff in Input Buffer
.is_ctrl
    push    bc
    ld      iy,s3_ctrl_keys
    call    aux_call              ; IX = Jump Address
    pop     bc
    jp      (ix)          

;-----------------------------------------------------------------------------
; Check for Direct Mode
; Output: Carry clear if in Direct Mode
; Clobbered: BC
;-----------------------------------------------------------------------------
in_direct:
    ld      c,a
    ld      a,(CURLIN)
    ld      b,a
    ld      a,(CURLIN+1)
    and     b
    cp      $FE
    ld      a,c
    ret

wait_key:
    call    read_key
    or       a
    jr       z,wait_key
    ret

input_ctrl_c:
    ld      a,(BASYSCTL)
    and     BASBRKOFF             ; If BRK is on
    jp      z,STPEND              ;   Break out of program
    ld      a,3
    ld      (IEND_KEY),a
    push    bc                    ; Stack = TxtPtr, RtnAdr
    jp      DATAH                 ; Skip to end of INPUT statement

main_ctrl_c:
    jp      c,.break
    rst     CHRGET
    jp      MAIN0
.break
    or      $FF
    jp      ENDCON

finish_input:
    ld      (IEND_KEY),a
    jp      FININL

;-----------------------------------------------------------------------------
; Check for Control Keys before fetching next statement 
; returns to NEWSTT
;-----------------------------------------------------------------------------
incntc_hook:
    ld      a,(BASYSCTL)
    and     BASBRKOFF             ; If BRK is off
    jr      nz,.drain_buf         ;   Drain buffer
    ld      a,(CHARC)             ; A = BufKey
    or      a                     ; If no key buffered
    jr      z,_buffchr            ;   Buffer the next one
    cp      'C'-64                ; If Ctrl-C
    jp      z,WRMCON              ;   Break
    cp      'D'-64                ; If Ctrl-D
    ld      b,0
    jr      z,.turbo              ;   Disable Turbo Mode
    cp      'F'-64                ; If Ctrl-F
    ld      b,1
    jr      z,.turbo              ;   Enable Turbo Mode
    cp      'U'-64                ; If Ctrl-F
    ld      b,3
    jr      z,.turbo              ;   Unlimited Turbo Mode
    cp      'S'-64                ; If Cttl-S
    jr      z,.pause              ;   Wait for keystroke
.drain_buf:
    ld      a,(KCOUNT)
    dec     a
    ld      (KCOUNT),a
    jr      z,_buffchr
    ret
.turbo
    ld      a,b
    call    set_turbo_mode
    jr      _buffchr
.pause:
    call    read_key
    jr      z,.pause
    jr      _buffchr
    
;-----------------------------------------------------------------------------
; Get buffered key
; Called from INKEY$, INKEY, GETKEY, etc.
;-----------------------------------------------------------------------------
in_key:
    ld      a,(CHARC)             ; Get buffered key
    or      a
    ld      c,a
    jr      z,_buffchr            ; If not 0
    ld      a,(BASYSCTL)
    and     BASBRKOFF              
    jr      nz,_buffchr           ; and BREAK is on
    ld      a,c
    cp      'C'-64                ; and Ctrl-C
    jp      z,WRMCON              ;   Break   
_buffchr:
    call    _in_key               ; Read another key
    ld      (CHARC),a             ; and buffer it
    ld      a,c
    or      a                     ; Return previously buffered key
    ret

reset_turbo_mode:
    xor     a
;-----------------------------------------------------------------------------
; Enable/Disable Turbo mode
; Input: A: 0 = 3 Mhz, 1 = 7 Mhz, 2 = undefined, 3 = unlimited
; Returns: A = Turbo mode actually set
;  speed: 0 = 3.88 Mhz
;         1 = 7,16 Mhz
;         2 = Undefined (returns Carry set)
;         3 = Unlimited
;-----------------------------------------------------------------------------
;; ToDo: Move to AuxROM?
set_turbo_mode:
    cp      4                     ; If A > 4
    ccf
    ret     c                     ;   Return Carry Set
    cp      2                     ; or A = 2
    jr      z,ret_c               ;   Return Carry Set
.turbo
    rla
    rla                           ; Rotate bits into place
    and     SYSCTRL_TURBO         ; Isolate turbo bits
    push    bc
    ld      b,a                   ;   and copy to B
    in      a,(IO_SYSCTRL)        ; Read SYSCTRL
    and     ~SYSCTRL_TURBO & 127  ;   mask out Fast Mode bit
    or      b                     ;   and copy the new Fast Mode bit in
    out     (IO_SYSCTRL),a        ; Write back to SYSCTRL
    pop     bc
get_turbo_mode:
    in      a,(IO_SYSCTRL)        ; Read SYSCTRL
    and     SYSCTRL_TURBO         ;   mask out Fast Mode bit
    rra
    rra
    ret
ret_c:
    scf
    ret

;-----------------------------------------------------------------------------
; Enable/Disable Ctrl-C Break
; Input: A = Bit 5 set for Disabled, reset for Enabled
; Clobbers: B
;-----------------------------------------------------------------------------
sys_break_mode:
    and     BASBRKOFF             ; Isolate Fast Mode bit
    ld      b,a                   ;   and copy to B
    ld      a,(BASYSCTL)          ; Read BASIC system control bits
    and     ~BASBRKOFF            ;   mask out Break Mode bit
    or      b                     ;   and copy the new Break Mode bit in
    ld     (BASYSCTL),a           ; Write BASIC system control bits
    ret

;-----------------------------------------------------------------------------
; Double SOUNDS delay timer if in Turbo Mode
;-----------------------------------------------------------------------------
beep_hook:
    byte    $F6                   ; OR over SCF to clear carry
sound_hook:
    scf                           ; Carry Set = Respect Flag
    ld      iy,s3_sound_hook      ; Play the sound
    jp      aux_call

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
; Fast Fill memory
; Input: A: Fill Byte
;       BC: Byte Count
;       HL: Start Address
; Clobbers: BC, DE, HL
;-----------------------------------------------------------------------------
sys_fill_mem_d
    ld      a,d                   ; A = Byte
    byte    $1E                   ; LD E over XOR
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
; Clobbered: AF',BC,DE,HL,IX
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
    jp      page_fast_copy_sys    ; Copy It

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
; Read key for BASIC color cycle screen
;-----------------------------------------------------------------------------
_xkeyread:
    call    key_read
    cp      ' '
    ret     nz
.enter
    ld      a,13
    ret

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
;    push    hl
;    call    get_linbuf_addr      ; HL = Line Buffer address
;    ld      c,0                  ; C = ChrCnt
;    ld      iy,edit
;    call    aux_call             ; Edit the line
;    pop     hl
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

exec_page:
    ex      af,af'
    out     (IO_BANK3),a
    ex      af,af'
jump_iy:
    jp      (iy)

;-----------------------------------------------------------------------------
; 80 column screen driver
;-----------------------------------------------------------------------------
    include "ttyhooks.asm"

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
; Utility routines
;-----------------------------------------------------------------------------
    include "util.asm"

;-----------------------------------------------------------------------------
; BASIC Buffer routines
;-----------------------------------------------------------------------------
    include "buffer.asm"

;-----------------------------------------------------------------------------
; Wait for Key Press
;-----------------------------------------------------------------------------
get_key:
    call    in_key
    jr      z,get_key
    ret

;-----------------------------------------------------------------------------
; Clear alternate keyboard port FIFO buffer
; Cobbered: A
;-----------------------------------------------------------------------------
key_clear_fifo:
    xor       a
    out       (IO_KEYBUFF),a
    ret
    
;-----------------------------------------------------------------------------
; Read  key from alternate keyboard port
; Output: A: Key ASCII value or scsn code (0 = no key pressed)
;-----------------------------------------------------------------------------
key_read:
    in        a,(IO_KEYBUFF)
    or        a
    ret

;-----------------------------------------------------------------------------
; Set keyboard repeat
;  Input: A: Repeat ($FF = On, $00 = False) 
; Output: A: 0 if succesful, else error code
;-----------------------------------------------------------------------------
key_save_repeat:
    and     KB_REPEAT             ; Isolate Repeat bit
    ld      b,a                   ; B = RptBit
    ld      a,(BASYSCTL)
    and     ~KB_REPEAT
    or      b
    ld      (BASYSCTL),a
key_set_repeat:
    and     KB_REPEAT             ; Isolate Repeat bit
    or      KB_ENABLE | KB_ASCII
;-----------------------------------------------------------------------------
; Set keyboard mode
;  Input: A: Mode (KB_ENABLE | KB_ASCII | KB_REPEAT) 
; Output: A: 0 if succesful, else error code
;-----------------------------------------------------------------------------
key_set_keymode:
    jp      esp_set_keymode

;-----------------------------------------------------------------------------
; INCHRA - INCHRH Replacement
;-----------------------------------------------------------------------------
read_key:
    xor     a
    ld      (CHARC),a
_in_key:
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
    ld      (RESPTR+1),a
.done
    out     (c),b
    exx
    ret
.readkey
    exx
    jp      key_read              ; Read key from keyboard and return

;-----------------------------------------------------------------------------
; Input: BC: Byte Count
;     DE,HL: Start addresses
; Clobbers: AF,AF,BC,DE,HL
;-----------------------------------------------------------------------------
sys_swap_mem:
    ld      a,b
    or      c
    ret     z
    ld      a,(de)
    ex      af,af'
    ld      a,(hl)
    ld      (de),a
    ex      af,af'
    ld      (hl),a
    inc     hl
    inc     de
    dec     bc
    jp      sys_swap_mem

; On entry: HL = BufAdr; Returns B = LinLen, HL = BufAdr
get_prev_line:
    ld      iy,read_prevbuf
    jr      aux_call

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Call Graphics subsystem subroutine
; - Maps Auxilarry ROM into Bank 3
; - Executes routine, passing all registers except AF' and IX
; - Restores Bank 3 and returns all registers exoept AF'
; Input: IY = Routine address
; Clobbered: AF', IX
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gfx__call:
    inc     iy                    ; Index into gfx jump_table
    inc     iy
    jr      aux_call

gfx_call:
    jr      aux_call

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Call Auxilary ROM subroutine
; - Maps Auxilarry ROM into Bank 3
; - Executes routine, passing all registers except AF' and IX
; - Restores Bank 3 and returns all registers exoept AF'
; Input: IY = Routine address
; Clobbered: AF', IX
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aux_call:
    call    page_map_auxrom
_jumpiy:
    call    jump_iy
    jp      page_restore_bank3

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Call Extended ROM subroutine
; - Maps Extended ROM into Bank 3
; - Executes routine, passing all registers except AF' and IX
; - Restores Bank 3 and returns all registers exoept AF'
; Input: IY = Routine address
; Clobbered: AF', IX
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ext_call:
    call    page_map_extrom
    jr      _jumpiy

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Call subroutine in page
; - Maps Page into Bank 3
; - Executes routine, passing all registers except AF' and IX
; - Restores Bank 3 and returns all registers exoept AF'
; Input: A = Page
;       IY = Routine address
; Clobbered: AF', IX
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
page_call:
    call    page_map_bank3
    ld      a,iyh
    or      $C0                   ; Force address to Bank 3
    ld      iyh,a
    jr      _jumpiy

; Run M/L Executable
run_exec:
    call    page_set_plus         ; Put extended ROM back
    call    SCRTCH                ; Do a New
    ld      bc,READY              ; Return to Direct Mode
    push    bc
    ld      iy,(FILNAF)           ; IY = ExecAdr
    ld      a,(FILNAF+2)          ; A = ExecPg
    or      a                     ; If ExecPg <> 0
    jp      nz,page_call          ;   Run in page
    jp      (iy)                  ; Else jumop directly to it
    
ptrget_ext:
    cp      '~'                   ; If tilde
    jr      z,_eat_suffix         ;   Eat suffix and continue
    call    ISLETC                ; If not a letter
    jp      c,NOSEC               ;    Continue
issec_ext:
    ld      b,a                   ; Make it the second character
    rst     CHRGET                ; Get following character
    cp      '~'                   ; If not tilde
    jp      nz,DEATEM             ;   Backup and reat rest of variable
_eat_suffix
    ld      iy,aux_eat_suffix
    call    aux_call
    jp      NOSEC

; Allocate a 256 byte temporary string buffer
; Output: HL: Buffer Address
alloc_temp_buffer:
    push    af                    ; Stack = SaveAF, RtnAdr
    push    bc                    ; Stack = SaveBC, SaveAF, RtnAdr
    push    de                    ; Stack = SaveDE, SaveBC, SaveAF, RtnAdr
    ld      hl,(TBFTOP)           ; HL = TempBufPtr
    inc     h                     ; Add 256
    ld      de,(FRETOP)           ; DE = Bottom of String Data
    rst     COMPAR                ; If new pointer in string space
    push    hl
    call    nc,GARBA2             ;   Collect garbage
    pop     hl
    ld      de,(FRETOP)           ; Try again
    rst     COMPAR                ; If new pointer in string space
    jr      nc,OSERR              ;   Out of string space error
    ld      (TBFTOP),hl           ; Set new pointer address
    dec     h                     ; Back to current pointer address
    pop     de                    ; DE = SaveSE; Stack = SaveBC, SaveAF, RtnAdr
    pop     bc                    ; BC = SaveBC; Stack = SaveAF, RtnAdr
    pop     af                    ; AF = SaveAF; Stack = RtnAdr
    ret

; Get address of most recently allocated temp buffer
; Output: HL: Buffer Address
get_temp_buffer:
    ld      hl,(TBFTOP)           ; HL = TempBufPtr
    dec     h                     ; Add 256
    ret

; Deallocate most recent temporary string buffer
free_temp_buffer:
    push    hl
    push    de
    push    af
    ld      hl,(TBFTOP)           ; HL = TempBufPtr
    dec     h                     ; Subtract 256
    ld      de,(STRSPC)           ; DE = Bottom of String Space
    rst     COMPAR
    jr      nc,.set               ; If Pointer < Botton of String Space
    ex      de,hl                 ;   Pointer = Botton of String Space
.set
    ld      (TBFTOP),hl           ; Update it
    pop     af
    pop     de
    pop     hl
    ret

OSERR:
    ld      e,ERROS
    jp      ERROR

;-----------------------------------------------------------------------------
; Called from OUTDO through OUTDOH in sbasic.asm
;-----------------------------------------------------------------------------
outchr_hook:
  push    af
  ld      a,(BASYSCTL)            ; Get System Control Bits
  rra                             ; Carry = BASOUTBUF
  jr      c,.buffer               ; If bit set, write to buffer 
  jp      OUTCON
.buffer
  pop     af
  ld      iy,output_to_buffer
  jp      ext_call

ctrlx_hook:
  push    bc
  call    in_direct
  pop     bc
  jr      c,.nope                 ; If Direct Mode
  ld      a,b
  dec     a             
  jr      nz,.nope                ; and buffer empty
  ld      a,11
  rst     OUTCHR                  ;   Clear screen and start over
  jp      INLIN                   ; Else 
.nope
  ld      a,7                     ;   Ring the bell and continue   
  jp      OUTBEL

free_rom_sys = $2F00 - $

;------------------------------------------------------------------------------
; Hook Jump Table
; ------------------------------------------------------------------------------

    assert !($2EFF<$)   ; ROM full!
    dc $2F00-$,$76

; BASIC Hook Jump Table
; 58 Bytes
hook_table:                     ; ## caller   addr  performing function
    dw      0                   ;  0                Deprecated
    dw      force_error         ;  1 ERRCRD   03E0  Print Error Message
    dw      0                   ;  2                Deprecated 
    dw      0                   ;  3                Deprecated
    dw      0                   ;  4                Deprecated
    dw      linker_hook         ;  5 LINKER   0485  Update BASIC Program Line Links
    dw      print_hook          ;  6 PRINT    07BC  Execute PRINT Statement
    dw      0                   ;  7                Deprecated
    dw      0                   ;  8 
    dw      eval_extension      ;  9 EVAL     09FD  Evaluate Number or String
    dw      keyword_to_token    ; 10 NOTGOS   0536  Converting Keyword to Token
    dw      0                   ; 11                Deprecated
    dw      0                   ; 12                Deprecated
    dw      0                   ; 13                Deprecated
    dw      0                   ; 14                Deprecated
    dw      0                   ; 14                Deprecated
    dw      FN_FN               ; 16 FNDOER   0B40  FNxx() call
    dw      HOOK17+1            ; 17 LPTOUT   1AE8  Print Character to Printer
    dw      0                   ; 18                Deprecated
    dw      0                   ; 19                Deprecated
    dw      HOOK20+1            ; 20 CLOAD    1C2C  Load File from Tape
    dw      HOOK21+1            ; 21 CSAVE    1C09  Save File to Tape
    dw      token_to_keyword    ; 22 LISPRT   0598  expanding a token
    dw      exec_next_statement ; 23 GONE2    064B  interpreting next BASIC statement
    dw      0                   ; 24                Deprecated
    dw      on_error            ; 25 ONGOTO   0780  ON statement
    dw      HOOK26+1            ; 26 INPUT    0893  Execute INPUT, bypassing Direct Mode check
    dw      execute_function    ; 27 ISFUN    0A5F  Executing a Function
    dw      HOOK28+1            ; 28 DATBK    08F1  Doing a READ from DATA
    dw      0                   ; 29                Deprecated

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
; S3 BASIC extensions routines in Extended ROM Page
;-----------------------------------------------------------------------------

_next_statement:
    call    page_set_plus
    jp      exec_next_statement   ; Go do the Statement

_scan_label
    call    page_set_plus
    jp      scan_label

error_ext:
    call    page_set_plus         ; Bank 3 could be mapped to any page at this point.
    call    clear_inevalflg       ; Clear In EVAL Flag
    xor     a
    ld      (SUBFLG),a            ; In case it as a UD error
    jp      trap_error            ; so map to Extended ROM, before continuing

;-----------------------------------------------------------------------------
; S3 BASIC extensions routines in Auxiliary ROM Page
;-----------------------------------------------------------------------------

string_ext:
    ld      iy,s3_string_ext
    jr      auxcall_jumpix
stuffh_ext:
    ld      iy,s3_stuffh_ext
auxcall_jumpix:
    call    aux_call
    jp      (ix)

; 10 _label:PRINT "testing":'comment
; 20 'comment
check_for_comment:
    cp      39                    ; If '
    jr      z,.rem                ;   Treat as REM
    sub     $80                   ; If not token
    jp      c,LET                 ;   Do LET
    jp      GONEX                 ; Else evauate token
.rem
    rst     CHRGET                ; Skip '
    jp      REM                   ;   and do REM

VMOVE:  ex      de,hl             ;MOVE VALUE FROM (DE) TO (HL). ALTERS B,C,D,E,H,L
MOVVFM: ld      bc,4              ;MOVE VALUE FROM (HL) TO (DE)
        ldir
        ret

; Return Carry Clear if string address is in an input buffer
; On entry DE = String Address
in_buffer:
    ld      hl,(TXTTAB)
    rst     COMPAR            ; If String is below Program Text 
    ret     nc                ;   Return Carry Clear
    ld      hl,(TOPMEM)       
    rst     COMPAR
    ccf                       ; If String is below Buffers
    ret     c                 ;   Return Carry Set
    ld      hl,(STRSPC)       ; If String is below String Space
    rst     COMPAR            ;   Return Carry Clear
    ret

; Print the line buffer
print_linbuf:
    call    get_linbuf_hl
    dec     b
.loop
    ld      a,(hl)
    inc     hl
    rst     OUTCHR
    djnz    .loop
    ret

;-----------------------------------------------------------------------------
; SysROM File Name
;-----------------------------------------------------------------------------

    dc $2FEF-$+1,$FF
    byte    "sysrom.bin"
    dc $2FFF-$+1,$00

;-----------------------------------------------------------------------------
; Pad first 4k
;-----------------------------------------------------------------------------
    assert !($3000<>$)   ; ROM full!
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

    include "tables.asm"          ; Lookup tables aligned to 256 byte boundaries
    include "dispatch.asm"        ; Statement/Function dispatch tables and routiness
    include "error.asm"           ; Error lookup table, messages and handling routines
    include "args.asm"            ; ARGS statement and function
    include "arrays.asm"          ; Array handling extensions
    include "basic80.asm"         ; Statements and functions from MBASIC 80
    include "baslines.asm"        ; (De)tokenize, add, insert, delete program lines
    include "coldboot.asm"        ; Cold boot code
    include "basbitmap.asm"       ; Bitmap drawing statements and functions
    include "edit.asm"            ; Enhanced INPUT and line editor
    include "enhanced.asm"        ; Enhanced stardard BASIC statements and functions
    include "evalext.asm"         ; EVAL extension - hook 9
    include "extended.asm"        ; Statements and functions from Aquarius Extended BASIC
    include "basfile.asm"         ; Disk and File I/O statements and functions
    include "bascreen.asm"        ; Text Screen statements and functions
    include "bastring.asm"        ; String handling statements and functions
    include "graphics.asm"        ; Graphics statements and functions
    include "hooks.asm"           ; Extended BASIC hooks
    include "play.asm"            ; PT3 player routines
    include "plus.asm"            ; plusBASIC unique statements and functions
    include "shared.asm"          ; Shared subroutines
    include "tokens.asm"          ; Keyword lists and tokenize/expand routines
    include "usbbas.asm"          ; Statements and functions from USB BASIC

    assert !($FFFF<$)   ; ROM full!

    free_rom_ext = $10000 - $

    dc $10000-$,$76

    dephase

;-----------------------------------------------------------------------------
; Auxiliary ROM Routines
;-----------------------------------------------------------------------------

    phase   $C000                 ;Assemble in ROM Page 1 which will be in Bank 3
    include "jump_aux.asm"        ; Auxiliary routines jump tables
    assert !($C1FF<$)             ; ROM full!
    dc $C200-$,$76
    dephase

    phase   $C000                 ;Assemble in ROM Page 1 which will be in Bank 3
    include "jump_gfx.asm"        ; Graphics routines jump tables
    assert !($C2FF<$)             ; ROM full!
    dc $C200-$,$76
    dephase

    phase   $C400

    include "auxtables.asm"       ; Lookup tables
    include "auxboot.asm"         ; Cold Boot code moved to AuxROM
    include "arrayaux.asm"        ; Array auxiliary routines
    include "basbuf.asm"          ; Basic buffer read/write routines
    include "basicaux.asm"        ; BASIC auxiliary
    include "debug.asm"           ; Debugging routines
    include "dos.asm"             ; DOS routines
    include "esp_aux.asm"         ; ESP routines in auxiliary ROM
    include "evalaux.asm"         ; Core routine for evalext.asm
    include "fileaux.asm"         ; Disk and File BASIC auxilarry routines 
    include "fileio.asm"          ; Disk and File I/O kernel routines
    include "fileload.asm"        ; File LOAD I/O routines
    include "filemisc.asm"        ; BASIC File auxilarry routines
    include "fileplus.asm"        ; AQPLUS resource run/load routines
    include "filesave.asm"        ; File SAVE I/O routines
    include "filestr.asm"         ; File related string assembly routines
    include "joyaux.asm"          ; BASIC game controller auxiliary code
    include "loadaux.asm"         ; BASIC file operations auxiliary code
    include "misc.asm"            ; Miscellaneous subroutines
    include "s3hooks.asm"         ; S3 BASIC direct mode hooks
    include "sound.asm"           ; Sound and Music
    include "string.asm"          ; String manipulation routines
    include "version.asm"         ; System and plusBASIC version mani

;; Grsphics routines
    include "gfxvar.asm"          ; Graphics sysvars and lookup tables
    include "gfx.asm"             ; Main graphics module
    include "basicgfx.asm"        ; BASIC graphics.asm subcalls
    include "color.asm"           ; Color palette module
    include "gfxbitmap.asm"       ; Bitmap graphics routines
    include "screen.asm"          ; Text screen graphics subroutines
    include "screen_gfx.asm"      ; Screen graphics routines    
    include "screen_swap.asm"     ; Screen buffering routines
    include "sprite_aux.asm"      ; Sprite graphics module
    include "tile.asm"            ; Tile graphics module

    free_rom_aux = $10000 - $

    dc $10000-$,$76

    dephase

;-----------------------------------------------------------------------------
; Graphics ROM Routines
;-----------------------------------------------------------------------------

ifdef xxxxx

    ToDo: (Eventually) Modify boot.asm to load Graphics ROM into bank 3.

    phase   $C000                 ;Assemble in ROM Page 1 which will be in Bank 3
    ;jump_gfx here eventually

    assert !($C1FF<$)             ; ROM full!
    dc $C200-$,$76

    byte    "Graphics ROM"
    
    free_rom_gfx = $10000 - $

    dc $10000-$,$76
    
endif    
    
end

