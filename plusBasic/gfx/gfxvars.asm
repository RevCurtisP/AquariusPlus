;=====================================================================================
; Graphics System Variable definitions`
;=====================================================================================

SGBASE  equ     $A0     ; Base Semigraphics Character

;-----------------------------------------------------------------------------
; Bitmap Graphics Variable Offsets
;-----------------------------------------------------------------------------
; Offsets in variable block
BMP_COLOR       equ   $00         ; Default draw color
BMP_LASTY       equ   $01         ; Last PSET/PRESET Y-coordinate
BMP_LASTX       equ   $02         ; Last PSET/PRESET X-coordinate
; Variable blocks 0: 40col, 1: 1bpp, 2: 40col, 3: 4bpp
BMP_40COL       equ   $00
BMP_80COL       equ   $04
BMP_1BPP        equ   $08
BMP_4BPP        equ   $0C

; $10 - $FF unused
