;-----------------------------------------------------------------------------
; regs.inc
;
; This file contains locations of hardware registers and ROM/RAM locations.
;
; Locations to the original Aquarius ROM are taken from the dissassembly by
; Curtis F Kaylor, which can be found at:
; https://github.com/RevCurtisP/Aquarius/blob/main/disassembly/aquarius-rom.lst
;

;-----------------------------------------------------------------------------
; plusBASIC System variables
;-----------------------------------------------------------------------------
; Screen and Color RAM Holes
INTSTCK     equ $33FD   ; Top of Interrupt stemporary stack $33E8 - $33FF
BORDERCHR   equ $33FE   ; Border character alternate memory location
PLUSTCK     equ $37FD   ; Top of plusBASIC temporary stack $37E8 - $37FF
BORDERCLR   equ $37FE   ; Border character alternate memory location

; RNDTAB ($3821-$3840) - Deprecated RND Permutation Table
BASINTPG    equ $3821   ; BASIC Interrupt Routine Page (swapped into BANK 3)
BASINTJP    equ $3822   ; BASIC Interrupt Routine JP instruction
              ; $3823   ; BASIC Interrupt Routine JP Address
              ; $3825   ; unused

; Paging Variables
BANK0PAGE   equ $3826   ; Page that was in Banked 0
BANK1PAGE   equ $3827   ; Page that was in Banked 1
BANK2PAGE   equ $3828   ; Page that was in Banked 2
BANK3PAGE   equ $3829   ; Page that was in Banked 3
;Used by ON ERROR. Must be in in consecutive order: ERRLIN,ERRFLG,ONEFLG,ONELIN
ERRLIN      equ $382A   ; LINE NUMBER WHERE LAST ERROR OCCURED.
ERRFLG      equ $382C   ; USED TO SAVE THE ERROR NUMBER SO EDIT CAN BE
ONEFLG      equ $382D   ; ONEFLG=1 IF WERE ARE EXECUTING AN ERROR TRAP ROUTINE
ONELIN      equ $382E   ; THE pointer to the LINE TO GOTO WHEN AN ERROR OCCURS
; DOS Variables - These can get overwritten between BASIC file operations
; They might also be re-used by other modules
ESP_FDESC   equ $3830   ; ESP file descriptor to close on Error or Ctrl-C
BINPAGE     equ $3831   ; Binary file load/save page
BINSTART    equ $3832   ; Binary file load/save address
BINLEN:     equ $3834   ; Binary file load length (32-bits $3838-$383B)
FILESIZE    equ $3838   ; File Size (32-bits $3838-$383B)
FILEATTR    equ $383C   ; File Attribute
FILEDATE    equ $383D   ; File Datestamp ($383D-$383E)
FILETIME    equ $383F   ; File Timestamp ($383F-$3840)

plus_page   equ 1       ; plusBASIC ROM BANK3 page
aux_page    equ 2       ; Auxillary ROM page




;-----------------------------------------------------------------------------
; Variables
;-----------------------------------------------------------------------------
BASIC_RAM_END: equ $BFFF    ; End of BASIC RAM, $BF00-$BFFF is reserved for our extensions

;-----------------------------------------------------------------------------
; IO registers
;-----------------------------------------------------------------------------
IO_VCTRL:       equ $E0
IO_VSCRX_L:     equ $E1
IO_VSCRX_H:     equ $E2
IO_VSCRY:       equ $E3
IO_VSPRSEL:     equ $E4
IO_VSPRX_L:     equ $E5
IO_VSPRX_H:     equ $E6
IO_VSPRY:       equ $E7
IO_VSPRIDX:     equ $E8
IO_VSPRATTR:    equ $E9
IO_VPALSEL:     equ $EA
IO_VPALDATA:    equ $EB
IO_VLINE:       equ $EC
IO_VIRQLINE:    equ $ED
IO_IRQMASK:     equ $EE
IO_IRQSTAT:     equ $EF
IO_BANK0:       equ $F0
IO_BANK1:       equ $F1
IO_BANK2:       equ $F2
IO_BANK3:       equ $F3
IO_ESPCTRL:     equ $F4
IO_ESPDATA:     equ $F5
IO_PSG1DATA:    equ $F6
IO_PSG1ADDR:    equ $F7
IO_PSG2DATA:    equ $F8
IO_PSG2ADDR:    equ $F9
IO_KEYBUFF      equ $FA
IO_SYSCTRL:     equ $FB
IO_CASSETTE:    equ $FC
IO_CPM:         equ $FD     ; Write-only
IO_VSYNC:       equ $FD     ; Read-only
IO_PRINTER:     equ $FE
IO_SCRAMBLE:    equ $FF     ; Write-only
IO_KEYBOARD:    equ $FF     ; Read-only

VCTRL_TEXT_EN:   equ (1<<0)
VCTRL_MODE_OFF:  equ (0<<1)
VCTRL_MODE_TILE: equ (1<<1)
VCTRL_MODE_BM:   equ (2<<1)
VCTRL_SPR_EN:    equ (1<<3)
VCTRL_TEXT_PRIO: equ (1<<4)

;-----------------------------------------------------------------------------
; Bank and Page Definitions
;-----------------------------------------------------------------------------
BANK0_BASE: equ $0000
BANK1_BASE: equ $4000
BANK2_BASE: equ $8000
BANK3_BASE: equ $C000

BANK_READONLY:  equ $80     ; Bank readonly
BANK_OVERLAY:   equ $40     ; RAM overlay

; RAM Page Usage
ROM_SYS_PG  equ 0           ; Main System ROM, mapped into Bank 0 with overlay from $3000-$3FFF
ROM_EXT_PG  equ 1           ; plusBASIC extended ROM, mapped into Bank 3 
ROM_AUX_PG  equ 1           ; plusBASIC auxillary ROM, mapped into Bank 3 as needed

VIDEO_RAM   equ 20
CHAR_RAM    equ 21

RAM_BAS_1   equ 33          ; RAM for BASIC mapped into Bank 1
RAM_BAS_2   equ 34          ; RAM for BASIC mapped into Bank 2
RAM_BAS_3   equ 35          ; RAM for BASIC mapped into Bank 3, switched in as needed
                            ; If a cartrige is present, it is copied here unencrypted, then executed
BAS_BUFFR   equ 36          ; plusBASIC extended system variables and buffers, mapped in bank 3 as needed
                            ; See regs.inc for details
VID_BUFFR   equ 37          ; Video RAM Shadow Buffer
PT3_BUFFR   equ 38          ; PT3 Player Buffer

USER_RAM    equ 40

;-----------------------------------------------------------------------------
; Video RAM Offsets
; Page 20
;-----------------------------------------------------------------------------
BMP_COLORRAM    equ $2000
TILE_DATA       equ $0000
TILE_MAP        equ $3000

;-----------------------------------------------------------------------------
; BASIC Extended System Constants, Variables and Buffers
; Page 36, swapped into Bank 3
;-----------------------------------------------------------------------------
GFX_SYSVARS equ $8000   ; Extended BASIC Graphics System Variables
                        ; defined in gfx/gfx.asm
STRBUFFLEN  equ $00FF   ; Page Address: Length of String in Buffer
STRBUFF     equ $0100   ; Page Address: String Buffer  - up to 255 chars plus null terminator 
                        ; Must be on 256 byte boundary
SCREENBUFF  equ $F800   ; For saving screen memory or double-buffering


;-----------------------------------------------------------------------------
; Macros
;-----------------------------------------------------------------------------
SYNCHK macro value
    rst     $08
    db      'value'
endm

;-----------------------------------------------------------------------------
; ESP32 commands
;-----------------------------------------------------------------------------
ESPCMD_RESET:      equ $01     ; Reset ESP
ESPCMD_VERSION:    equ $02     ; Get version string
ESPCMD_DATETIME    equ $03     ; Get current date and time
ESPCMD_OPEN:       equ $10     ; Open / create file
ESPCMD_CLOSE:      equ $11     ; Close open file
ESPCMD_READ:       equ $12     ; Read from file
ESPCMD_WRITE:      equ $13     ; Write to file
ESPCMD_SEEK:       equ $14     ; Move read/write pointer
ESPCMD_TELL:       equ $15     ; Get current read/write
ESPCMD_OPENDIR:    equ $16     ; Open directory
ESPCMD_CLOSEDIR:   equ $17     ; Close open directory
ESPCMD_READDIR:    equ $18     ; Read from directory
ESPCMD_DELETE:     equ $19     ; Remove file or directory
ESPCMD_RENAME:     equ $1A     ; Rename / move file or directory
ESPCMD_MKDIR:      equ $1B     ; Create directory
ESPCMD_CHDIR:      equ $1C     ; Change directory
ESPCMD_STAT:       equ $1D     ; Get file status
ESPCMD_GETCWD:     equ $1E     ; Get current working directory
ESPCMD_CLOSEALL:   equ $1F     ; Close any open file/directory descriptor

ERR_NOT_FOUND:     equ -1   ; File / directory not found
ERR_TOO_MANY_OPEN: equ -2   ; Too many open files / directories
ERR_PARAM:         equ -3   ; Invalid parameter
ERR_EOF:           equ -4   ; End of file / directory
ERR_EXISTS:        equ -5   ; File already exists
ERR_OTHER:         equ -6   ; Other error
ERR_NO_DISK:       equ -7   ; No disk
ERR_NOT_EMPTY:     equ -8   ; Not empty

FO_RDONLY:      equ 0x00    ; Open for reading only
FO_WRONLY:      equ 0x01    ; Open for writing only
FO_RDWR:        equ 0x02    ; Open for reading and writing
FO_ACCMODE:     equ 0x03    ; Mask for above modes
FO_APPEND:      equ 0x04    ; Append mode
FO_CREATE:      equ 0x08    ; Create if non-existant
FO_TRUNC:       equ 0x10    ; Truncate to zero length
FO_EXCL:        equ 0x20    ; Error if already exists