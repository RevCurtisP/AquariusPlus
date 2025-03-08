;====================================================================
; Auxillary ROM tables
;====================================================================

; This code must start on a 256 byte boundary

dpad_table:
    byte    0                     ; $00 - none
    byte    5                     ; $01 - P5    
    byte    1                     ; $02 - P1    
    byte    4                     ; $03 - P4    
    byte    13                    ; $04 - P13   
    byte    0                     ; $05 - none
    byte    16                    ; $06 - P16   
    byte    0                     ; $07 - none
    byte    9                     ; $08 - P9    
    byte    8                     ; $09 - P8    
    byte    0                     ; $0A - none
    byte    0                     ; $0B - none
    byte    12                    ; $0C - P12   
    byte    0                     ; $0D - none
    byte    0                     ; $0E - none
    byte    0                     ; $0F - none
    byte    0                     ; $10 - none
    byte    6                     ; $11 - P6    
    byte    2                     ; $12 - P2    
    byte    3                     ; $13 - P3    
    byte    14                    ; $14 - P14   
    byte    0                     ; $15 - none
    byte    15                    ; $16 - P15   
    byte    0                     ; $17 - none
    byte    10                    ; $18 - P10   
    byte    7                     ; $19 - P7    
    byte    0                     ; $1A - none
    byte    0                     ; $1B - none
    byte    11                    ; $1C - P11   
    byte    0                     ; $1D - none
    byte    0                     ; $1E - none
    byte    0                     ; $1F - none

button_table:
    byte    $81                   ; K6
    byte    $82                   ; K5    
    byte    0                     ; K4
    byte    $A0                   ; K3    
    byte    $84                   ; K2    
    byte    0                     ; K1
