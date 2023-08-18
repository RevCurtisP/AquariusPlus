;-----------------------------------------------------------------------------
; Aquarius+ system ROM and Extende BASIC
;-----------------------------------------------------------------------------
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
;   zmac --zmac -o aqplusbas.cim -o aqplusbas.lst aqplusbas.asm

; RAM Page Usage
; Page 0  - Systm ROM with overlay
; Page 33 - RAM in BANK 1
; Page 34 - RAM in BANK 2
; Page 35 - RAM in BANK 3 
;   If a cartrige is present, it is copied here unencrypted 
; Page 36 - BASIC Extended System Variables and Buffers, swapped in BANK 3
;   See regs.inc

    include "regs.inc"

    org     $2000
    jp      _reset          ; Called from main ROM at reset vector
    jp      _coldboot       ; Called from main ROM for cold boot
    jp      _start_cart
    jp      _interrupt      

;-----------------------------------------------------------------------------
; Reset vector
;
; CAUTION: stack isn't available at this point, so don't use any instruction
;          that uses the stack.
;-----------------------------------------------------------------------------
_reset:
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
    ld      a, 1
    out     (IO_VCTRL), a

    ; Init palette 0
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

    ; Set Extended Key Table Address
    ld      hl,key_table-1
    ld      (KEYADR), hl

    ; Back to system ROM init
    jp      JMPINI

.default_palette:
    dw $111, $F11, $1F1, $FF1, $22E, $F1F, $3CC, $FFF
    dw $CCC, $3BB, $C2C, $419, $FF7, $2D4, $B22, $333

;-----------------------------------------------------------------------------
; Character RAM initialization
;-----------------------------------------------------------------------------
init_charram:
    ; Save current bank 1/2
    in      a, (IO_BANK1)
    push    a
    in      a, (IO_BANK2)
    push    a

    ; Temporarily set up mappings for character RAM and character ROM
    ld      a, 21           ; Page 21: character RAM
    out     (IO_BANK1), a
    ld      a, 0            ; Page 0: first page of flash ROM
    out     (IO_BANK2), a

    ; Copy character ROM to character RAM
    ld      de, BANK1_BASE
    ld      hl, BANK2_BASE + $3000
    ld      bc, 2048
    ldir

    ; Restore bank 1/2
    pop     a
    out     (IO_BANK2), a
    pop     a
    out     (IO_BANK1), a
    ret

;-----------------------------------------------------------------------------
; Cold boot entry point
;-----------------------------------------------------------------------------
_coldboot:
    ; No Cartridge - Map RAM into Bank 3
    ld      a, 35               ; Third RAM Page
    out     (IO_BANK3), a


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

    call    clear_esp_fdesc
    
    ; Show our copyright message
    call    PRNTIT              ; Print copyright string in ROM
    call    STRPRI
    db $0D, $0A
    db "Aquarius+ System ", 0
    ld      a, ESPCMD_VERSION
    call    esp_cmd
.print_version:
    call    esp_get_byte
    or      a
    jr      z, .print_done
    call    TTYCHR
    jr      .print_version
.print_done:
    call    STRPRI
    db " PlusBasic v0.2", 0
    call    CRDO
    call    CRDO

    jp      INITFF              ; Continue in ROM

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

    ; Descramble and start ROM
    jp      descramble_rom

;-----------------------------------------------------------------------------
; VBLANK Interrupt Handler
;-----------------------------------------------------------------------------
_interrupt:
    ret

;-----------------------------------------------------------------------------
; Hook 2 - READY (Enter Direct Mode
;-----------------------------------------------------------------------------
direct_mode:
    ld      a,(ESP_FDESC)         ; Get File Descriptor in Use
    or      a
    jp      m,.no_fdesc           ; If Valid Descriptor
    call    esp_close             ;   Close the File
    call    clear_esp_fdesc       ;   
.no_fdesc
    jp      HOOK2+1
    
clear_esp_fdesc:
    ld      a,255
    ld      (ESP_FDESC),a         ; Set to No File
    ret

;-----------------------------------------------------------------------------
; Memory Management routines
;-----------------------------------------------------------------------------

swap_basic_buffs:
    ld      b,36

; Swap Page B into Bank 3
; Puts Current Page on Stack
; Clobbers: A, IX
swap_bank3:
    pop     ix                    ; Get Return Address
    ld      a,(IO_BANK3)          ; Get current Page
    push    af                    ; Save It
    ld      a,b
    ld      (IO_BANK3),a          ; Bank in Page

; Restore Bank3
restore_bank3:
    pop     ix                    ; Get Return Address
    pop     af                    ; Get S
    ld      (IO_BANK3),a          ; Bank in Page
    jp      (IX)                  ; Fast Return
    

;-----------------------------------------------------------------------------
; RUN command - hook 24
;-----------------------------------------------------------------------------
run_cmd:
    jp      z, RUNC            ; If no argument then RUN from 1st line

    push    hl
    call    FRMEVL             ; Get argument type
    pop     hl

    ld      a, (VALTYP)
    dec     a                  ; 0 = string
    jp      z, run_file

    ; RUN with line number
    call    CLEARC             ; Init BASIC run environment
    ld      bc, NEWSTT
    jp      RUNC2              ; GOTO line number

move_cursor:

;;;This is separate routine MOVEIT in Extended BASIC
    push    af

    ; Restore character behind cursor
    push    hl
    exx
    ld      hl, (CURRAM)        ; CHRPOS - address of cursor within matrix
    ld      a, (CURCHR)         ; BUFO - storage of the character behind the cursor
    ld      (hl), a             ; Put original character on screen
    pop     hl

    ; Calculate new cursor location
    ld      a, l
    add     a, a
    add     a, a
    add     a, l
    ex      de, hl
    ld      e, d
    ld      d, $00
    ld      h, d
    ld      l, a
    ld      a, e
    dec     a
    add     hl, hl
    add     hl, hl
    add     hl, hl              ; HL is now 40 * rows
    add     hl, de              ; Added the columns
    ld      de, SCREEN          ; Screen character-matrix (= 12288 dec)
    add     hl, de              ; Putting it all together
    jp      TTYFIS              ; Save cursor position and return

byte_to_hex:
    ld      b, a
    rra
    rra
    rra
    rra
    call    .hex
    ld      a, b
.hex:
    and     $0F
    cp      10
    jr      c, .chr
    add     7
.chr:
    add     '0'
    ld      (hl), a
    inc     hl
    ret

;-----------------------------------------------------------------------------
; DOS routines
;-----------------------------------------------------------------------------
    include "dos.asm"

;-----------------------------------------------------------------------------
; ESP routines
;-----------------------------------------------------------------------------
    include "esp.asm"

;-----------------------------------------------------------------------------
; Statements from ExtendedBASIC
;-----------------------------------------------------------------------------
    include "extended.asm"

;-----------------------------------------------------------------------------
; BASIC file I/O Statements
;-----------------------------------------------------------------------------
    include "fileio.asm"

;-----------------------------------------------------------------------------
; plusBASIC specific statements and functions
;-----------------------------------------------------------------------------
    include "plus.asm"

free_rom = $2C00 - $

;------------------------------------------------------------------------------
; Statement, BASIC Hook, and Function Dispatch Tables and Handlers
; $2C00 - $2EFF
;------------------------------------------------------------------------------

    assert !($2BFF<$)   ; ROM full!
    dc $2BFF-$+1,$FF

    include "dispatch.asm"

;-----------------------------------------------------------------------------
; Keyboard Decode Tables for S3 BASIC with Extended Keyboard Support
; $2F00 - $2FFF
;-----------------------------------------------------------------------------

    assert !($2EFF<$)   ; ROM full!
    dc $2EFF-$+1,$FF

    include "keytable.asm"

;-----------------------------------------------------------------------------
; Verify ROM is correct length
;-----------------------------------------------------------------------------

    assert !($3000<>$)   ; Incorrect ROM! length

    end
