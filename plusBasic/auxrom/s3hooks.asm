;=====================================================================================
; S3BASIC Enhancement Hooks
; These routines are located in the 8K ROM in ROM Bank 2
; They called from S3BASIC and are routed through a jump table at $2000
;=====================================================================================

;-----------------------------------------------------------------------------
; Extended line editor function keys
; Jumped to from INLNC
; On entry: A,C = character typed, B = input buffer character count
;-----------------------------------------------------------------------------
s3_ctrl_keys:
    push    bc                    ; Stack = ChrCnt+ChrTyp
    call    in_direct
    pop     bc                    ; B = ChrCnt, C = ChrTyp
    jr      c,.dontscreen         ; If Not in Direct Mode
    ld      a,c
    or      a
    jp      m,.extended
    dec     b
    jr      nz,.dontscreen        ; and Input Buffer is empty
    ld      a,c
    ld      b,1
    cp      'T'-64
    jr      z,.switch_screen
    inc     b
    cp      'Y'-64
    jr      z,.switch_screen
    inc     b
    cp      'W'-64
    jr      z,.switch_screen
.dontscreen
    ld      a,c                   ; Get typed character
    sub     a,'K'-64              ;
    jr      c,.notrub             ;
    cp      'M'-'K'               ;
    jr      z,.notrub             ;
    jr      nc,.notrepeat         ; If ^K or ^L
    dec     a                     ;   ^K = $FF, ^L = 0
    and     KB_REPEAT             ;   ^K = Repeat on, ^L = off
    ld      b,a                   ;   Save it
    ld      a,(BASYSCTL)          ;   Get current Flags
    and     $FF-KB_REPEAT         ;   Mask out Repeat Bit
    or      b                     ;   OR new value back in
    ld      (BASYSCTL),a          ;   And write it back out
    ld      a,KB_ENABLE | KB_ASCII
    or      b                     ;
    call    key_set_keymode       ;   Now set new keybuffer mode
    jr      .inlinc               ;   Wait for next key
.notrepeat:
    cp      'P'-'K'               ; If not ^N through ^P
    jr      c,.charset
.notrub
    ld      ix,NOTRUB             ;   Continue standard Ctrl-key check
    ret
.charset
    sub     a,'N'-'K'             ; ^N = 0, ^O = 1, ^P = 2
    xor     1                     ; ^O = 1, ^O = 0, ^P = 2
    push    hl                    ;
    call    select_chrset         ; Select the character set
    pop     hl
.inlinc
    ld      ix,INLINC             ;   Wait for next key
    ret
.extended
    cp      $A0                   ; If international character
    jr      nc,.notrub            ;   use it
    call    fnkey_get_buff_addr   ; DE = Key Buffer Address
    jr      nz,.inlinc            ; If Function Key
    dec     de                    ;   Back up for autotype
    ld      (RESPTR),de           ;   Set pointer to buffer
    jr      .inlinc               ;   and return
.switch_screen
    ld      a,b                   ; A = Screen#
    push    de
    push    hl
    call    screen_switch
    ld      hl,(CURRAM)
    ld      (hl),$7F
    pop     hl
    pop     de
    jr      .inlinc

;-----------------------------------------------------------------------------
; Hook 34: Don't tokenize unquoted literal string after DOS command in direct mode
;-----------------------------------------------------------------------------
s3_stuffh_ext:
    cp      DATATK-':'            ; If DATA
    jr      z,.colis              ;   Continue STUFFH
    ex      af,af'                ;
    ld      a,(TEMP3)             ; Get Line# Flag
    and     $01                   ; If carry set
    jr      nz,.exaf_nodatt       ;   Continue STUFFH
    ex      af,af'
    cp      RUNTK-':'
    jr      z,.space_loop
    cp      DIRTK-':'             ; If Not DIRTK through CDTK
    jr      c,.nodatt
    cp      CDTK-':'+1
    jr      nc,.nodatt            ;   Continue STUFFH
.space_loop
    ld      a,(hl)                ; Eat Spaces
    cp      ' '
    jr      nz,.not_space
    call    _stuff_chr
    jr      .space_loop
.not_space
    ld      b,a                   ; Set up delimiter for STRNG
    cp      '"'                   ; If quotes
    jr      z,_strng              ;   Go stuff quoted string
    ld      ix,KLOOP
.string_loop
    ld      a,(hl)                ; Get character
    or      a                     ; If end of line
    jr      z,.crdone             ;   Finish it up
    cp      ' '                   ; If space
    ret     z                     ;   Stuff it and continue
    cp      ':'                   ; If colon
    ret     z                     ;   Stuff it and continue
    cp      '('                   ; If colon
    ret     z                     ;   Stuff it and continue
    call    _stuff_chr                ; Else Stuff it
    jr      .string_loop          ;   and check next character

.exaf_nodatt
    ex      af,af'
.nodatt
    ld      ix,NODATT
    ret
.colis
    ld      ix,COLIS
    ret
.crdone
    ld      ix,CRDONE
    ret
.strngr
    ld      ix,STRNGR
    ret


; If label at beginning of line: don't tokenize, just stuff it
stuff_label:
    cp      '_'                   ; If not a undersoore
    jp      nz,STRNGR             ;   Keep on truckin'
    call    _stuff_chr            ; Stuff character in KRUNCH buffer
.loop
    ld      a,(hl)                ; Get character from buf
    or      a                     ; If end of line
    jp      z,CRDONE              ;   Finish it up
    cp      ' '                   ; If Space
    jr      z,.stuff_it           ;   Stuff it and keep going
    cp      ':'                   ; If colon
    jp      z,STRNGR              ;   Stuff it and return
.stuff_it:
    call    _stuff_upper          ; Stuff character in KRUNCH buffer
    jr      .loop                 ; Next character

; Uppercase and stuff character
_stuff_upper:
    call    uppercase_char
; Stuff char in KRUNCH buffer
_stuff_chr:
    inc     hl                    ; Bump BUF pointer
    ld      (de),a                ; Save byte in KRUNCH buffer
    inc     de                    ; Bump KRUNCH pointer
    inc     c                     ; Increment buffer count
    ret

_strng:
    ld      ix,STRNG
    ret

;-----------------------------------------------------------------------------
; Hook 39: Extended string processing during CRUNCH
;-----------------------------------------------------------------------------
s3_string_ext:
    jp      z,STRNG               ; Special Handling for Double Quote
    cp      $27                   ;
    jp      z,STRNG               ; Do the same for Single Quote
    cp      '~'                   ; If tilde
    jr      z,.stuff_suffix       ;   Stuff variable suffix
    cp      $5C                   ; If not backslash
    jp      nz,stuff_label        ;   See if it's a label
; Stuff escaped string
    call    _stuff_chr            ; Copy backslash
    ld      a,(hl)                ; Get next character
    cp      '"'                   ; If not quotes`
    jp      nz,STRNGR             ;   Carry on
.escape_char
    call    _stuff_chr            ; Stuff quotes
.escape_loop
    ld      a,(hl)                ; Get next character
    call    _stuff_chr            ; Else stuff it
    or      a                     ; If EOL
    jp      z,STRNGR              ;   Finish CRUNCH
    cp      '"'                   ;
    jp      z,KLOOP               ; If quotes
    jr      nz,.not_quotes        ;   Continue crunching
.not_quotes
    cp      $5C                   ; If not escape
    jr      nz,.escape_loop       ;   check next character
    ld      a,(hl)                ; Else get next character
    jr      .escape_char          ;   escape it, and loop
;Stuff ~ and variable suffix
.stuff_suffix
    call    _stuff_chr            ; Stuff suffix character
    ld      a,(hl)                ;
    cp      '0'                   ; If < '0'
    jp      c,_strngr             ;   Finish CRUNCH
    cp      ':'                   ; If '0'...'9'
    jr      c,.stuff_suffix       ;   Stuff it
    call    ISLETC                ; If letter
    jr      nc,.stuff_suffix      ;   Stuff it
_strngr
    jp      STRNGR                ; Else finish crunch

;-----------------------------------------------------------------------------
; Get Function Key Buffer Address
; Input: A: Key ASCII Code
; Output: DE: Buffer Address
; Flags Set: Z if A = Function key
; Clobbered: A
;-----------------------------------------------------------------------------
fnkey_get_buff_addr:
    ld      e,a
    and     $97                   ;            100X0XXX
    cp      e
    ret     nz
    ld      d,FKEYBASE/512        ; 011??000
    rla                           ; 011??000 1 00X0XXX0
    ccf                           ; 011??000 0 00X0XXX0
    rla                           ; 011??000 0 0X0XXX00
    rla                           ; 011??000 0 X0XXX000
    rla                           ; 011??000 X 0XXX0000
    rl      d                     ; 11??000X 0 0XXX0000
    rla                           ; 11??000X 0 XXX00000
    ld      e,a
    xor     a
    ret

