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
; Page 1 -  ROM Page 1 in BANK 3 
;   After BASIC cold boots
; Page 36 - BASIC Extended System Variables and Buffers, swapped in BANK 3
;   See regs.inc

    include "regs.inc"

    org     $2000
    jp      _reset          ; $2000 Called from main ROM at reset vector
    jp      _coldboot       ; $2003 Called from main ROM for cold boot
    jp      _start_cart     ; $2006
    jp      _interrupt      ; $2009
    jp      _warm_boot      ; $200C Called from main ROM for warm boot

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
    ld      a, plus_page        ; plusBASIC extended ROM
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
    db " PlusBasic v0.7g", 0
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
;-----------------------------------------------------------------------------
_interrupt:
    ret

;-----------------------------------------------------------------------------
; Intercept WRMCON call
;-----------------------------------------------------------------------------
_warm_boot:
    ld      a, plus_page          ; Page 1 ROM
    out     (IO_BANK3), a         ; into Bank 3
    jp      WRMCON                ; Go back to S3 BASIC

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
    reti

;-----------------------------------------------------------------------------
; HOOK 5 - Set Return Address for CHEAD when executed from S3BASIC
;-----------------------------------------------------------------------------
set_chead_return:
    ld      bc,MAIN             ; Make CHEAD return to MAIN
    push    bc
    jmp     HOOK5+1

;-----------------------------------------------------------------------------
; Hook 23 - GONE2 (Handle Extended BASIC Statement Tokens)
;-----------------------------------------------------------------------------
_next_statement:
    push    af                    ; Save Token and Flags
    in      a,(IO_BANK3)          ; Get Current Page
    ld      (BANK3PAGE),a         ; Save It
    ld      a,plus_page           ; Page 1 - Extended BASIC 
    out     (IO_BANK3),a          ; Page it in
    pop     af                    ; Restore Token and Flags
    jp      exec_next_statement   ; Go do the Statement

statement_ret:
    ld      a,(BANK3PAGE)         ; Get Saved Page
    out     (IO_BANK3),a          ; Bank it Back in
    ret                           ; Return to NEWSTT
    

FLOAT_BC:
    ld      d,b                   ;  Copy into DE
    ld      e,c                   ;  
FLOAT_DE:
    push    hl
    xor     a                     ; Set HO to 0
    ld      (VALTYP),a            ; Force Return Type to numeric
    ld      b,$98                 ; Exponent = 2^24
    call    FLOATR                ; Float It
    pop     hl
    ret

;-----------------------------------------------------------------------------
; bas_read_to_buff - Read String from ESP to BASIC String Buffer
; Input: IX: DOS or ESP routine to call
; Output: E: String Length
;        DE: Address of Terminator
;        HL: Buffer Address
; Clobbers: B
;-----------------------------------------------------------------------------
bas_read_to_buff:
    ld      hl,FBUFFR             ; Use FBUFFR for now
jump_ix:
    jp      (ix)                  ; Execute routine and return

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
; Paged memory routines
;-----------------------------------------------------------------------------
    include "paged.asm"


;-----------------------------------------------------------------------------
; DOS routines
;-----------------------------------------------------------------------------
    include "dos.asm"

;-----------------------------------------------------------------------------
; ESP routines
;-----------------------------------------------------------------------------
    include "esp.asm"

;-----------------------------------------------------------------------------
; Primitive debugger
;-----------------------------------------------------------------------------
    include "debug.asm"



free_rom_2k = $2C00 - $

;------------------------------------------------------------------------------
; Hook, Dispatch Tables and Handlers
;------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
;  Hook Jump Table
; ------------------------------------------------------------------------------

    org ($ & $FF00) + 256

; BASIC Hook Jump Table
; 58 Bytes
hook_table:                     ; ## caller   addr  performing function
    dw      HOOK0+1             ;  0 ERROR    03DB  Initialize Stack, Display Error, and Stop Program
    dw      HOOK1+1             ;  1 ERRCRD   03E0  Print Error Message
    dw      HOOK2+1             ;  2 READY    0402  BASIC command line (immediate mode)
    dw      HOOK3+1             ;  3 EDENT    0428  Save Tokenized Line  
    dw      HOOK4+1             ;  4 FINI     0480  Finish Adding/Removing Line or Loading Program
    dw      HOOK5+1             ;  5 LINKER   0485  Update BASIC Program Line Links
    dw      HOOK6+1             ;  6 PRINT    07BC  Execute PRINT Statement
    dw      HOOK7+1             ;  7 FINPRT   0866  End of PRINT Statement
    dw      HOOK8+1             ;  8 TRMNOK   0880  Improperly Formatted INPUT or DATA handler
    dw      eval_extension      ;  9 EVAL     09FD  Evaluate Number or String
    dw      keyword_to_token    ; 10 NOTGOS   0536  Converting Keyword to Token
    dw      HOOK11+1            ; 11 CLEAR    0CCD  Execute CLEAR Statement
    dw      HOOK12+1            ; 12 SCRTCH   0BBE  Execute NEW Statement
    dw      HOOK13+1            ; 13 OUTDO    198A  Execute OUTCHR
    dw      HOOK14+1            ; 14 ATN      1985  ATN() function
    dw      HOOK15+1            ; 15 DEF      0B3B  DEF statement
    dw      HOOK16+1            ; 16 FNDOER   0B40  FNxx() call
    dw      HOOK17+1            ; 17 LPTOUT   1AE8  Print Character to Printer
    dw      HOOK18+1            ; 18 INCHRH   1E7E  Read Character from Keyboard
    dw      HOOK19+1            ; 19 TTYCHR   1D72  Print Character to Screen
    dw      HOOK20+1            ; 20 CLOAD    1C2C  Load File from Tape
    dw      HOOK21+1            ; 21 CSAVE    1C09  Save File to Tape
    dw      token_to_keyword    ; 22 LISPRT   0598  expanding a token
    dw      _next_statement     ; 23 GONE2    064B  interpreting next BASIC statement
    dw      run_cmd             ; 24 RUN      06BE  starting BASIC program
    dw      HOOK25+1            ; 25 ONGOTO   0780  ON statement
    dw      HOOK26+1            ; 26 INPUT    0893  Execute INPUT, bypassing Direct Mode check
    dw      execute_function    ; 27 ISFUN    0A5F  Executing a Function
    dw      HOOK28+1            ; 28 DATBK    08F1  Doing a READ from DATA
    dw      HOOK29+1            ; 29 NOTSTV   099E  Evaluate Operator (S3 BASIC Only)

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



    assert !($2BFF<$)   ; ROM full!
    dc $2BFF-$+1,$FF

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

;-----------------------------------------------------------------------------
; plusBASIC Statements, Functions, and Operators
;-----------------------------------------------------------------------------

    phase   $C000     ;Assemble in ROM Page 1 which will be in Bank 3

    include "dispatch.asm"      ; Statement/Function dispatch tables and routiness
    include "tokens.asm"        ; Keyword list and tokenize/expand routines-
    include "enhanced.asm"      ; Enhanced stardard BASIC statements and functions
    include "evalext.asm"       ; EVAL extension - hook 9 
    include "extended.asm"      ; Extended BASIC statements and functions
    include "fileio.asm"        ; Disk and File I/O statements and functions
    include "plus.asm"          ; plusBASIC unique statements and functions

free_rom_16k = $10000 - $

    end

