;=====================================================================================
; plusBASIC Screen Buffers
; The LSB of a variable buffer is also the MSB of the screen buffer
;
;=====================================================================================

;-----------------------------------------------------------------------------
; Screen Data Buffers
; Offsets into Page SCR_BUFFR
;-----------------------------------------------------------------------------
SCRN40BUF   equ $0000   ; Primary 40 column buffer for screen switching
SCRN41BUF   equ $0800   ; Secondary 40 column buffer for screen switching
SCRN80BUF   equ $1000   ; 80 column buffer for screen switching

SCRN40SWP   equ $2000   ; Used by screen save, restore and swap
SCRN41SWP   equ $2800   ; Used by screen save, restore and swap
SCRN80SWP   equ $3000   ; Used by screen save, restore and swap

;-----------------------------------------------------------------------------
; Screen Variable Buffers
; Offset from start of page BAS_BUFFR
;-----------------------------------------------------------------------------
SVBUFLEN    equ 8       ; Screen Variable Buffer Length
; Variable buffers for screen switching
BUFSCRN40 equ $00   ; TTYPOS 
            ; $01   ; CURCHR
         ; $02-03   ; CURRAM
            ; $04   ; BASYSCTL flags
            ; $05   ; IO_VCTRL
            ; $06   ; SCOLOR
            ; $07   ; SCREENCTL

BUFSCRN41 equ $08   ; TTYPOS
            ; $09   ; CURCHR
        ; $0A-$0B   ; CURRAM
            ; $0C   ; BASYSCTL flags
            ; $0D   ; IO_VCTRL
            ; $0E   ; SCOLOR
            ; $0F   ; SCREENCTL
  
BUFSCRN80 equ $10   ; TTYPOS
            ; $11   ; CURCHR
         ; $12-13   ; CURRAM
            ; $14   ; BASYSCTL flags
            ; $15   ; IO_VCTRL
            ; $16   ; SCOLOR
            ; $17   ; SCREENCTL

BUFSVARS  equ $18   ; TTYPOS 
            ; $19   ; CURCHR
        ; $1A-$1B   ; CURRAM
            ; $1C   ; BASYSCTL flags
            ; $1D   ; IO_VCTRL
            ; $1E   ; SCOLOR
            ; $1F   ; SCREENCTL

; Variable buffers for STASH, SWAP, and RESTORE
SWPSCRN40 equ $20   ; TTYPOS
            ; $21   ; CURCHR
         ; $22-23   ; CURRAM
            ; $24   ; BASYSCTL flags
            ; $25   ; IO_VCTRL
            ; $26   ; SCOLOR
            ; $27   ; SCREENCTL
               
SWPSCRN41 equ $28   ; TTYPOS
            ; $29   ; CURCHR
        ; $2A-$2B   ; CURRAM
            ; $2C   ; BASYSCTL flags
            ; $2D   ; IO_VCTRL
            ; $2E   ; SCOLOR
            ; $2F   ; SCREENCTL

SWPSCRN80 equ $30   ; TTYPOS
            ; $31   ; CURCHR
         ;$32-$33   ; CURRAM
            ; $34   ; BASYSCTL flags
            ; $35   ; IO_VCTRL
            ; $36   ; SCOLOR
            ; $37   ; SCREENCTL
               
SWPSVARS  equ $38   ; TTYPOS
            ; $39   ; CURCHR
        ; $3A-$3B   ; CURRAM
            ; $3C   ; BASYSCTL flags
            ; $3D   ; IO_VCTRL
            ; $3E   ; SCOLOR
            ; $3F   ; SCREENCTL

; Pallete Buffers     VarBuf
BUFPALT40 equ $40  ; $00
BUFPALT41 equ $60  ; $08
BUFPALT80 equ $80  ; $10
SWPPALT40 equ $A0  ; $20
SWPPALT41 equ $C0  ; $28
SWPPALT80 equ $E0  ; $30

