;-----------------------------------------------------------------------------
; plus.inc
;
; plusBASIC System Variables, I/O Registers, and Memory Banks
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; Bank and Page Definitions
;-----------------------------------------------------------------------------
BANK0_BASE:     equ $0000
BANK1_BASE:     equ $4000
BANK2_BASE:     equ $8000
BANK3_BASE:     equ $C000

PAGE_SIZE       equ $4000

BANK_READONLY:  equ $80     ; Bank readonly
BANK_OVERLAY:   equ $40     ; RAM overlay

; RAM Page Usage
ROM_SYS_PG      equ 60      ; Main System ROM, mapped into Bank 0 with overlay from $3000-$3FFF
ROM_EXT_PG      equ 61      ; plusBASIC extended ROM, mapped into Bank 3 
ROM_AUX_PG      equ 62      ; plusBASIC auxillary ROM, mapped into Bank 3 as needed
ROM_GFX_PG      equ 62      ; plusBASIC graphics ROM, mapped into Bank 3 as needed
ROM_CRT_PG      equ 63      ; Legacy cartridge image

ROM_SYS_OV      equ ROM_SYS_PG | BANK_READONLY | BANK_OVERLAY
ROM_EXT_RO      equ ROM_EXT_PG | BANK_READONLY 
ROM_AUX_RO      equ ROM_AUX_PG | BANK_READONLY 
ROM_GFX_RO      equ ROM_GFX_PG | BANK_READONLY 
ROM_CRT_RO      equ ROM_CRT_PG | BANK_READONLY 

ROM_CART        equ 19

VIDEO_RAM       equ 20
CHAR_RAM        equ 21
    
RAM_BAS_0       equ 56      ; RAM for Bank 0 (character ROM cache)
RAM_BAS_1       equ 57      ; RAM for BASIC mapped into Bank 1
RAM_BAS_2       equ 58      ; RAM for BASIC mapped into Bank 2
RAM_BAS_3       equ 59      ; RAM for Bank 3 (unused by BASIC)

;                   48      ; Reesrved
PLUS_CART       equ 49      ; Aquarius+ cartridge image
TMP_BUFFR       equ 50      ; Transient data buffer 
BOOT_BUFR       equ 51      ; Boot ROM work area
SCR_BUFFR       equ 52      ; Screen Buffers
VID_BUFFR       equ 53      ; Video RAM Shadow Buffer
BAS_BUFFR       equ 54      ; plusBASIC extended system variables and buffers, mapped in bank 3 as needed
TRK_BUFFR       equ 55      ; Tracker Player Buffer
    
USER_RAM        equ 32      ; First page of User RAM
USER_END        equ 47      ; Last page of User RAM

RAM_END_PG      equ 63      ; Last page of RAM

ROM_EXT_BANK    equ 3       ; Bank Extended ROM is mapped into
ROM_AUX_BANK    equ 3       ; Bank Auxillary ROM is mapped into

CHAR_ROM_AQ     equ $3000   ; Standard Aquarius Character ROM Address in System ROM
CHAR_ROM_L1     equ $3800   ; Latin-1 Aquarius Character ROM Address in System ROM

;-----------------------------------------------------------------------------
; Video RAM Offsets 
; Page 20
;-----------------------------------------------------------------------------
BMP_BASE        equ $0000
BMP_COLORRAM    equ $2000
BMP_PAL_OFS     equ 8000    ; Offset of palette data in file
BMP_FILELEN     equ 9192    ; Length of 320x200 bitmap file
TILE_DATA       equ $0000
TILE_MAP_BASE   equ $0000
TILE_MAP_LEN    equ 64*32*2


;-----------------------------------------------------------------------------
; plusBASIC System variables
;-----------------------------------------------------------------------------
; Screen RAM Hole $33E8-$33FF exists only in 40 column mode
BORDERCHR   equ $33FF   ; Border character alternate memory location (40 column mode)
; Color RAM Hole
BORDERCLR   equ $37FF   ; Border color alternate memory location

; plusBASIC temporary sysvars

; RNDTAB ($3821-$3840) - Deprecated RND Permutation Table
BASINTPG    equ $3821   ; BASIC Interrupt Routine Page, 0=None (swapped into BANK 3)
BASINTJP    equ $3822   ; BASIC Interrupt Routine JP instruction
BASINTADR   equ $3823   ; BASIC Interrupt Routine JP Address ($3823-$3824)
BASYSCTL    equ $3825   ; System Control Bits 
IRQACTIVE   equ $3826   ; Active IRQ Routines Bits
                        ; 0: Timer
TIMERCNT    equ $3827   ; TIMER counter LSB = $3827, MSB = $3829
;Used by ON ERROR. Must be in in consecutive order: ERRLIN,ERRFLG,ONEFLG,ONELIN
ERRLIN      equ $382A   ; LINE NUMBER WHERE LAST ERROR OCCURED.
ERRFLG      equ $382C   ; USED TO SAVE THE ERROR NUMBER SO EDIT CAN BE
ONEFLG      equ $382D   ; ONEFLG=1 IF WERE ARE EXECUTING AN ERROR TRAP ROUTINE
ONELIN      equ $382E   ; THE pointer to the LINE TO GOTO WHEN AN ERROR OCCURS
EXT_FLAGS   equ $3830   ; Extended BASIC flags
BAS_FDESC   equ $3831   ; ESP file descriptor to close on Error or Ctrl-C
IEND_KEY    equ $3832   ; Key pressed to end input
SCREENCTL   equ $3833   ; Text Screen flags
GFX_FLAGS   equ $3834   ; Bitmap Graphics flags
BAS_FLAGS   equ $3835   ; Temporary BASIC flags
TBFTOP      equ $3836   ; Address after top temporary string buffer
IRQSAVSTK   equ $3838   ; Stack position after irq_handler has saved all registers
;       $383A - $3840   ; Unused (5 bytes)

; Temporary Variables stored in FILNAM+FILNAM ($3851-$385C), RESMO-RESLO ($38F7-$38F8)
; bitmap_get and bitmap_set
BMPMODE     equ $3851   ; Bitmap Mode 0=1bpp, 2=4bpp
STARTCOL    equ $3852
ENDCOL      equ $3854
STARTROW    equ $3856
ENDROW      equ $3858

; CALL ARGS
SAVEHL      equ $3852   ; [word] 
SAVEDE      equ $3854   ; [word]
SAVEBC      equ $3856   ; [word]

; DEF SPRITE
SPRTLCNT    equ $3851   ; Spritle Count
SPRTLNUM    equ $3852   ; Spritle number
SPRTCOLS    equ $3854   ; Width in spritles
SPRTROWS    equ $3856   ; Height in spritles

; GET/PUT SCREEN
GPSCMODE    equ $3851   ; GET/PUT screen mode (CHR/ATTR/both)

                        ;            SPLIT           DIM         JOIN       READ           LIST$()
                        ;          bas_split    dim_extension  bas_join read_extension   unpack_line
                        ;       aux_split_array   array_read  _ skipone                output_to_buffer
; FILNAM+FILNAF ($3851-$385C) - CLOAD/CSAVE File Name
ARRAYPTR    equ $3851   ; 383D         *               *           *         &
ARRAYADR    equ $3853   ; 383A         *
BUFADR      equ $3853   ; 3857                                     *
ARRAYLEN    equ $3855   ; 383F         *               *           *         *
BUFLEN      equ $3857   ; 385B                                     *
BUFRET      equ $3857   ; 385B                                                                *
BUFPTR      equ $3859   ; 3859                                     *                          *
SPLITDSC    equ $385B   ; 3838         *                           *
; RESMO+RESLO ($38F7-$38F8) - FMULT/FDIV Result
SPLITSEG    equ $38F7   ; 3839         *         
ARRAYREQ    equ $38F7   ; 3839                         *                     *
ARRAYTYP    equ $38F8   ; 383C                         *                     *
SPLITDEL    equ $38F8   ; 383C         *                           *

;-----------------------------------------------------------------------------
; Variables
;-----------------------------------------------------------------------------
BASIC_RAM_END: equ $BFFF    ; End of BASIC RAM, $BF00-$BFFF is reserved for our extensions

;-----------------------------------------------------------------------------
; Extended Tokens
;-----------------------------------------------------------------------------
XTOKEN = $FE
XPSVAR = $FF

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
IO_PCMDAC       equ $EC
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

VCTRL_TEXT_OFF  equ $00     ; xxxxxxx0  Disable Text Screen
VCTRL_TEXT_EN   equ $01     ; xxxxxxx1  Enable Text Screen
VCTRL_MODE_TILE equ $02     ; xxxxx01x  Select Tilemap Mode
VCTRL_MODE_BM   equ $04     ; xxxxx10x  Select 1bpp Bitmap Mode
VCTRL_MODE_MC   equ $06     ; xxxxx11x  Select 4bpp Bitmap Mode
VCTRL_SPR_EN    equ $08     ; xxxx1xxx  Enable Sprites
VCTRL_TEXT_PRIO equ $10     ; xxx1xxxx  Text Screen Priority 
VCTRL_REMAP_BC  equ $20     ; xx1xxxxx  Remap Text Border Characters
VCRTL_80COL_EN  equ $40     ; x1xxxxxx  Enable 80-column mode
VCTRL_TEXT_PAGE equ $80     ; 1xxxxxxx  Select Text Page

VCTRL_TEXT_BITS equ VCTRL_TEXT_EN + VCRTL_80COL_EN + VCTRL_TEXT_PAGE
VCTRL_TEXT_MASK equ $FF - VCTRL_TEXT_BITS
VCTRL_TEXT_DIS  equ $FF - VCTRL_TEXT_EN

SYSCTRL_XRG_DIS equ $01     ; xxxxxxx1  Disable registers $E0-$F5, $F8-$F9
SYSCTRL_PSG_DIS equ $02     ; xxxxxx1x  Disable PSGs
SYSCTRL_TURBO   equ $0C     ; xxxx11xx  Clock Speed (3.88,7.16,Unlimited)
SYSCTRL_WBOOT   equ $80     ; 1xxxxxxx  Warn Boot Flag

IRQ_VBLANK      equ $01     ; VBLANK interrupt
IRQ_VLINE       equ $02     ; Video LINE interrupt

PSG_RIGHTCON    equ 14      ; Right Hand Controller
PSG_LEFTCON     equ 15      ; Left Hand Controller

;-----------------------------------------------------------------------------
; BASIC Extended character ROM buffers
; Page 56 (RAM_BAS_0) 
; See screen_swap.asm
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; BASIC Extended System Constants, Variables and Buffers
; Page 54 (BAS_BUFFR), swapped into Bank 3
;-----------------------------------------------------------------------------
; Page 54 (BAS_BUFFR), swapped into Bank 3
BASBUF_BASE equ $C000   ; Base Address of BASIC Buffers
SCRNVBASE   equ $0000   ; Screen variable buffers (see screen.inc)
GFXVBASE    equ $0100   ; GFX SysVar Base, 256 byte aligned (see gfxvars.asm)
FKEYDEFS    equ $0200   ; Function Key Definitions (32 bytes each)
FKEYBASE    equ BASBUF_BASE + FKEYDEFS
LPRFILSPC   equ $0500   ; LPRINT File Spec (32 byte)
MOUSEWDLT   equ $0520   ; Accumulated change in mouse delta
;               $0521   ; Unused
HSTFILLEN   equ $0522   ; Length of "_history" file
HSTFILPOS   equ $0526   ; Current position in "_history" file
;         $052A-$07FF   ; Unused
RUNARGS     equ $0800   ; RUN arguments (see run_args in args.asm)
RUNARGEND   equ $08FF   ; End of RUN arguments
RUN_PATH    equ $0900   ; Search path for RUN programs
WRTKEYBUF   equ $0A00   ; user autotype buffer
;         $0B00-$1EFF   ; Unused
PRVDIRLIN   equ $1F00   ; Previous direct mode line entered
DEFCHRSET   equ $3000   ; Page Offset: Default Character Set Buffer
ALTCHRSET   equ $3800   ; Page Offset: Custom Character Set Buffer
CHRSETLEN   equ 2048    ; Size of character set ROM
ASCSETLEN   equ 768     ; Size of ASCII character set (' ' - DEL)
; BASIC interpreter control flags
BASOUTBUF   equ $01     ; BASYSCTL: redirect OUTput to BUFfer
BASSAVASC   equ $02     ; BASYSCTL: Default SAVe to ASC
BASKEYRPT   equ $04     ; BASYSCTL: KEY RePeaT on
BASBRKOFF   equ $20     ; BASYSCTL: BReak OFF (Disable Ctrl-C)
BASLPRNTR   equ $40     ; BASYSCTL: LPRINT device: (0 = Serial, 1 = ESP)
BASCOLOR    equ $80     ; BASYSCTL: Direct mode color print flag (must be same as SCRCOLOR)
; Temporary BASIC control flags
BASVARDEF   equ $02     ; BAS_FLAGS: PTRGET called from VARDEF
INEVALFLG   equ $04     ; BAS_FLAGS: Currently in EVAL function
; Screen Control Bitmask
; Screen printing control flags
EXTCTLCHR   equ $01     ; SCREENCTL: Extended control codes in TTYOUT
SCRCHRSET   equ $08     ; SCREENCTL: Character set in use (0 or 1)
SCRCHRMOD   equ $10     ; SCREENCTL: Character set in use has been modified
SCROLLOFF   equ $20     ; SCREENCTL: Disable screen scrolling
CRSR_OFF    equ $40     ; SCREENCTL: Display cursor (1 = No)
SCRCOLOR    equ $80     ; SCREENCTL: PRINT updates Color RAM

; Bitmap graphics mode: see gfx.asm for details
GFXM_TEXT   equ $00     ; GFX_FLAGS: 40 colum mode (bloxels) 
GFXM_WIDE   equ $01     ; GFX_FLAGS: 80 colum mode (bloxels) 
GFXM_1BPP   equ $02     ; GFX_FLAGS: Bitmap mode (1bpp)
GFXM_4BPP   equ $03     ; GFX_FLAGS: Bitmap mode (4bpp)
GFXM_MASK   equ $03     ; Graphics mode bit mask
GFXBUFLAG   equ $04     ; GFX_FLAGS: Bitmap operations are buffered
GFXSETMSK   equ $F8     ; Mask for writing GFX_FLAGS
; BASIC extended control flags

SOUNDSLOW   equ $08     ; Play SOUNDs at standard clock speed
BASOUTOVR   equ $10     ; EXT_FLAGS: Allow OUT to all ports
TRK_LOOPS   equ $20     ; EXT_FLAGS: PT3 songs repeat
FERR_FLAG   equ $40     ; EXT_FLAGS: File operations generate BASIC errors
TRON_FLAG   equ $80     ; EXT_FLAGS: Trace on/off

DFLTATTRS   equ $06     ; Default screen colors - Black on Cyan

;-----------------------------------------------------------------------------
; IRQACTIVE Bits
;-----------------------------------------------------------------------------
IRQ_TIMER   equ $01     ; Bit 0 - Timer active
IRQ_USER    equ $02     ; Bit 1 - User Interrupt active
IRQ_TRACKER equ $04     ; Bit 2 - Tracker Player running
IRQ_MOUSE   equ $08     ; Bit 3 - Mouse routine running

;-----------------------------------------------------------------------------
; PT3 Player
;-----------------------------------------------------------------------------
PT3_BASE    equ $C000
pt3init     equ PT3_BASE+$00    ; StartPlayer
pt3tick     equ PT3_BASE+$03    ; PlayQuark  
pt3stat     equ PT3_BASE+$06    ; CHECKLP
pt3mute     equ PT3_BASE+$09    ; MUTE
pt3fast     equ PT3_BASE+$0C    ; SetMode
pt3mode     equ PT3_BASE+$0F    ; GetMode
pt3song     equ $0A00           ; PT3 file load address

;-----------------------------------------------------------------------------
; Macros
;-----------------------------------------------------------------------------
SYNCHKC macro value
    rst     SYNCHK
    db      'value'
endm

SYNCHKT macro value
    rst     SYNCHK
    db      value
endm


;-----------------------------------------------------------------------------
; ESP32 commands
;-----------------------------------------------------------------------------
ESPCMD_RESET:      equ $01     ; Reset ESP
ESPCMD_VERSION:    equ $02     ; Get version string
ESPCMD_DATETIME    equ $03     ; Get current date and time
ESPCMD_KEYMODE     equ $08     ; Set keyboard buffer mode
ESPCMD_GETMOUSE    equ $0C     ; Get mouse position and button state
ESPCMD_GETGAMECTRL equ $0E     ; Get wireless game controller status
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
ESPCMD_OPENDIR83   equ $20     ; Open directory in 8.3 filename mode
ESPCMD_READLINE    equ $21     ; Read line from file
ESPCMD_OPENDIREXT  equ $22     ; Open directory in extended mode
ESPCMD_LOADFPGA    equ $40     ; Load FPGA core
; Not yet implemented
ESPCMD_WIFI_STATUS       equ $50  ; Get WiFi status
ESPCMD_WIFI_SCAN         equ $51  ; Scan for WiFi APs
ESPCMD_WIFI_CONFIG       equ $52  ; Configure WiFi
ESPCMD_WIFI_SET_HOSTNAME equ $53  ; Set hostname	

;-----------------------------------------------------------------------------
; ESP32 error codes
;-----------------------------------------------------------------------------
ERR_NOT_FOUND:     equ -1   ; File / directory not found
ERR_TOO_MANY_OPEN: equ -2   ; Too many open files / directories
ERR_PARAM:         equ -3   ; Invalid parameter
ERR_EOF:           equ -4   ; End of file / directory
ERR_EXISTS:        equ -5   ; File already exists
ERR_OTHER:         equ -6   ; Other error
ERR_NO_DISK:       equ -7   ; No disk
ERR_NOT_EMPTY:     equ -8   ; Not empty
ERR_WRITE_PROTECT  equ -9   ; Write protected

;-----------------------------------------------------------------------------
; ESP32 file open modes
;-----------------------------------------------------------------------------
FO_RDONLY:      equ 0x00    ; Open for reading only
FO_WRONLY:      equ 0x01    ; Open for writing only
FO_RDWR:        equ 0x02    ; Open for reading and writing
FO_ACCMODE:     equ 0x03    ; Mask for above modes
FO_APPEND:      equ 0x04    ; Append mode
FO_CREATE:      equ 0x08    ; Create if non-existant
FO_TRUNC:       equ 0x10    ; Truncate to zero length
FO_EXCL:        equ 0x20    ; Error if already exists

;-----------------------------------------------------------------------------
; ESP32 keyboard buffer modes
;-----------------------------------------------------------------------------
KB_ENABLE       equ 0x01    ; Enable keyboard buffer
KB_ASCII        equ 0x02    ; ASCII mode
KB_REPEAT       equ 0x04    ; Enable auto-repeat
