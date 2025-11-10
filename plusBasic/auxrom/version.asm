;-----------------------------------------------------------------------------
; version.asm - Auxiliary kernel system and plusBASIC version routines
;-----------------------------------------------------------------------------


;-----------------------------------------------------------------------------
; Return BASIC Version
; Input: HL: String Buffer address
; Output: BC: Version String Length
; Clobbers: A,DE
;-----------------------------------------------------------------------------
get_plusbas_version:
    ex      de,hl                 ; DE = BufAdr
    ld      hl,plus_version       ; HL = VerAdr
    call    str_copy              ; Copy VerStr to StrBuf
    ex      de,hl                 ; HL = BufAdr
    ret

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Return system version
; - Populates a string buffer with the current firmwsre version
; Input: HL: String buffer address
; Output: BC: Length of version string
; Clobbered: A
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; This will replace esp_get_version in esp.asm
get_system_version:
    ld      a, ESPCMD_VERSION
    call    esp_cmd
    jp      esp_read_buff

;-----------------------------------------------------------------------------
; Convert null-terminated Version string to BCD integer
; Input: HL: Version String Address - ['V']major.minor[letter]
; Output: C,D,E = Major,Minor,Letter (SYSROM and plusBASIC)
;         HL = Address after Version string
; Clobbered: A, B
;-----------------------------------------------------------------------------
version_to_long:    
    ld      a,(hl)
    call    uppercase_char        ; Skip 'V'
    cp      'V'                 
    jr      nz,.notv
    inc     hl
.notv
    call    asc_to_bcd            ; Get Major number
    ld      c,e                   ;   and put in C
    ld      de,0                  ; Init minor and letter to 0
    cp      '.'                   ; If no dot
    ret     nz                    ;   Return
    inc     hl                    ; Skip .
    ld      a,(hl)                ; A = NxtChr
    ld      d,$FF                 ; Preload minor with value > 99
    cp      'd'                   ; If "d" after dot
    ret     z                     ;   Assume .dev and return > 99
    call    asc_to_bcd            ; Get Minor number
    ld      d,e                   ;   and put in D
    ld      e,0                   ; Init letter to none
    call    uppercase_char        ; Capitalize 
    cp      'A'                   ; If not a letter
    ret     c                     ;    Return
    cp      'Z'+1
    ret     nc                    
    sub     '@'                   ; Convert 'A'-'Z' to 1-26
    ld      e,a                   ; Else put in E
    ret

;-----------------------------------------------------------------------------
; Convert ASCII to BCD
;  Input: HL: String address
; Output: A: Character after last digit
;        DE: BCD Result
;        HL: Address after last digit 
; Clobbers: A, B
;----------------------------------------------------------------------------
asc_to_bcd:
    ld      de,0                  ; Init Result
    dec     hl                    ; Back up for chrget
.loop
    rst     CHRGET                ; Get next character
    ret     nc                    ; Return if not a digit
    sub     a,'0'                 ; Convert digit to BCD
    ld      b,4
.rollde
    rl      e                     ; Shift DE left 4 bits
    rl      d
    djnz    .rollde
    or      e
    ld      e,a                   ; Move digit into bottom nybble                  
    jr      .loop

;-----------------------------------------------------------------------------
; Convert BCD to Binary
;  Input: DE: BCD integer
; Output: A: Character after last digit
;        DE: Binary result Result
; Clobbers: A
;----------------------------------------------------------------------------
bcd_to_bin:
    push    hl
    ld      hl,0                  ; Init binary result
    ld      a,d                   ; A = First digit

;-----------------------------------------------------------------------------
; HL = HL * 10
mult_hl_10:
    push    bc
    ld      b,h                   ; BC = HL
    ld      c,l
    sla     l                     ; HL*4
    rl      d
    sla     l
    rl      d                     
    add     hl,bc                 ; HL*4+1
    sla     l
    rl      d                     ; HL*(4+1)*2
    pop     bc
    ret

