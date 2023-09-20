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
    jp      do_cls_default  ; $2012
    jp      _inlin_hook     ; $2015 Jump from INLIN for command history recall
    jp      _inlin_done     ; $2018 Jumped from FININL to save command to history
  

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

    ; Zero out plusBASIC system vars
    xor     a
    ld      b,ESP_FDESC-RNDTAB
    ld      hl,RNDTAB
.sysvar_loop    
    ld      (hl),a
    inc     hl
    djnz    .sysvar_loop

    call    clear_esp_fdesc

    ; Show our copyright message

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
    ld      a,38-.plus_len        ; Print spaces to right justify +BASIC text
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
.plus_text
    db "plusBASIC v0.12i", 0
.plus_len   equ   $ - .plus_text

    call    CRDO
    call    CRDO

    call    spritle_clear_all   ; Clear all sprite properties

; ToDo: Copy INITFF code into here, try to load AUTOEXEC basic program 
; RUNC if load successful, READY if not

    jp      INITFF              ; Continue in ROM

    dc $2100-$,$76


ifdef _____   ; Waiting for modules to stablize
    org     $2100
    include "kernel.asm"    ; Kernal jump table
endif

    dc $2100-$,$76
    
    
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
; Use screen holes for temporary stack
;
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
; INLIN enhancement stubs
;-----------------------------------------------------------------------------
_inlin_hook:
_inlin_done:
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
    ld      a,128
    ld      (ESP_FDESC),a         ; Set to No File
    reti

;-----------------------------------------------------------------------------
; Hook 12 - SCRTCH (Execute NEW statement)
;-----------------------------------------------------------------------------
_scratch:
    call    clear_all_errvars
    ld      c,0
    call    spritle_toggle_all    ; Disable all sprites
    jp      HOOK12+1

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
;    cp      ' '                   ; If space
;    jr      nz,.not_lspace        ;   Skip it
;    inc     de
;    jr      .label_loop
.not_lspace
    call    .check_colon          ; Treat colon as terminator
    ld      b,a                   ; Put in B for compare
.text_loop:
    ld      a,(hl)                ; Get character from line
;    cp      ' '                   ; If space
;    jr      nz,.not_tspace        ;   Skip it
;    inc     hl
;    jr      .text_loop
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


do_cls_default:
    ld      a,6                   ; default to black on cyan

do_cls:
    call    clear_screen
    ld      a,' '
    ld      (CURCHR),a            ; SPACE under cursor
    ld      de,$3000+41           ; Point Address for (0,0) 
    ld      (CURRAM),de       
    xor     a
    ld      (TTYPOS),a            ; column 0
    ret

clear_screen:
    push    hl
    ld      hl,$3000
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
    pop     hl
    ret

_trap_error:
    call    page_restore_plus     ; Map Extended ROM into bank 3
    jp      trap_error

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
; Extended string buffer routines
;-----------------------------------------------------------------------------
    include "sbuff.asm"

;-----------------------------------------------------------------------------
; Utility routines
;-----------------------------------------------------------------------------
    include "util.asm"

;-----------------------------------------------------------------------------
; plusBASIC tokens
;-----------------------------------------------------------------------------
    include "tokens.asm"        ; Keyword list and tokenize/expand routines

free_rom_2k = hook_table - $

;------------------------------------------------------------------------------
; Hook, Dispatch Tables and Handlers
;------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
;  Hook Jump Table
; ------------------------------------------------------------------------------

    assert !($2DFF<$)   ; ROM full!
    dc $2E00-$,$76

; BASIC Hook Jump Table
; 58 Bytes
hook_table:                     ; ## caller   addr  performing function
    dw      _trap_error         ;  0 ERROR    03DB  Initialize Stack, Display Error, and Stop Program
    dw      force_error         ;  1 ERRCRD   03E0  Print Error Message
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
    dw      _scratch            ; 12 SCRTCH   0BBE  Execute NEW Statement
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
    include "sprite.asm"        ; Sprite graphics module
    include "tile.asm"          ; Tile graphics module

    assert !($FFFF<$)   ; ROM full!

    free_rom_16k = $10000 - $

    dc $10000-$,$76

    dephase

    end

