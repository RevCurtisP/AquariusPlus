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
    ld      a,c                   ; A = Key
    or      a
    jp      p,.not_extended       ; If Key > 127
    cp      $A0                   ;   If international character
    jr      nc,.notrub            ;     use it
    call    fnkey_get_buff_addr   ;   DE = Key Buffer Address
    jr      nz,.inlinc            ;   If Function Key
    dec     de                    ;     Back up for autotype
    ld      (RESPTR),de           ;     Set pointer to buffer
    jr      .inlinc               ; Else
.not_extended
    push    bc                    ;   Stack = ChrCnt+ChrTyp
    call    in_direct
    pop     bc                    ;   B = ChrCnt, C = ChrTyp
    jr      c,.notrub             ;   If in Direct Mode
    dec     b
    jr      nz,.dontscreen        ;     If Input Buffer is empty
    ld      a,c                   ;       A = Key
    ld      b,1
    cp      'T'-64                ;       If ^2
    jr      z,.switch_screen      ;         Switch to Screen 1
    inc     b
    cp      'Y'-64                ;       If ^Y
    jr      z,.switch_screen      ;         Switch to Screen 2
    inc     b
    cp      'W'-64                ;       If ^W
    jr      z,.switch_screen      ;         Switch to Screen 3 
.dontscreen
    ld      a,c                   ;       A = Key
    sub     a,'K'-64              ;       A = 0 if ^K, 1 if ^L, etc...
    jr      c,.notrub             ;
    cp      'M'-'K'               ;
    jr      z,.notrub             ;
    jr      nc,.notrepeat         ;       If ^K or ^L
    dec     a                     ;         ^K = $FF, ^L = 0
    call    key_save_repeat       ;         Update BASYSCTL and ESP Keymode
    jr      .inlinc               ;       Else
.notrepeat:
    cp      'P'-'K'               ;         If not ^N OR ^O
    jr      c,.charset
.notrub
    ld      ix,NOTRUB             ;           Continue standard Ctrl-key check
    ret
.charset
    sub     a,'N'-'K'             ;         ^N = 0, ^O = 1
    xor     1                     ;         ^O = 1, ^O = 0
    push    hl                    ;
    call    select_chrset         ;         Select the character set
    pop     hl
.inlinc
    ld      ix,INLINC             ; Wait for next key
    ret
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
.string_loop
    ld      a,(hl)                ; Get character
    or      a                     ; If end of line
    jr      z,_crdone             ;   Finish it up
    cp      ' '                   ; If space
    jr      z,_kloop              ;   Stuff it and continue
    cp      ':'                   ; If colon
    jr      z,_kloop              ;   Stuff it and continue
    cp      '('                   ; If paren
    jr      z,_kloop              ;   Stuff it and continue
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
_crdone:
    ld      ix,CRDONE
    ret
_kloop:
    ld      ix,KLOOP
    ret

; If label at beginning of line: don't tokenize, just stuff it
_stuff_label:
    cp      '_'                   ; If not a undersoore
    jr      nz,_strngr            ;   Keep on truckin'
    call    _stuff_chr            ; Stuff character in KRUNCH buffer
.loop
    ld      a,(hl)                ; Get character from buf
    or      a                     ; If end of line
    jr      z,_crdone             ;   Finish it up
    cp      ' '                   ; If Space
    jr      z,.stuff_it           ;   Stuff it and keep going
    cp      ':'                   ; If colon
    jr      z,_strngr             ;   Stuff it and return
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
    jr      z,_strng              ; Special Handling for Double Quote
    cp      $27                   ;
    jp      z,_strng              ; Do the same for Single Quote
    cp      '~'                   ; If tilde
    jr      z,.stuff_suffix       ;   Stuff variable suffix
    cp      $5C                   ; If not backslash
    jp      nz,_stuff_label       ;   See if it's a label
; Stuff escaped string
    call    _stuff_chr            ; Copy backslash
    ld      a,(hl)                ; Get next character
    cp      '"'                   ; If not quotes`
    jr      nz,_strngr            ;   Carry on
.escape_char
    call    _stuff_chr            ; Stuff quotes
.escape_loop
    ld      a,(hl)                ; Get next character
    call    _stuff_chr            ; Else stuff it
    or      a                     ; If EOL
    jr      z,_strngr             ;   Finish CRUNCH
    cp      '"'                   ;
    jr      z,_kloop              ; If quotes
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
    ld      ix,STRNGR
    ret

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

;-----------------------------------------------------------------------------
; SOUNDS/BEEP extension
; Play slow if Carry Clear, resoect SOUNDSLOW if Carry Set
;-----------------------------------------------------------------------------
s3_sound_hook:
    jr      nc,.slow
    ld      a,(EXT_FLAGS)
    and     SOUNDSLOW             ; If SOUND FAST
    jp      z,SOUNDS              ;   Do sounds and return
.slow
    in      a,(IO_SYSCTRL)
    and     $7F                   ; Strip Reset bit
    push    af                    ; Stack = SysCtrl, RtnAdr
    and     ~SYSCTRL_TURBO
    out     (IO_SYSCTRL),a
    call    SOUNDS                
    pop     af
    out     (IO_SYSCTRL),a
    ret



;=====================================================================================
; Routines not routed through jump table
;=====================================================================================

; Eat digits and letters after tilde
; Any tokenized keywords are also eaten
aux_eat_suffix:
    inc     hl
    ld      a,(hl)                ; Get next character
    or      a                     ; If NUL
    ret     z                     ;   Get out
    cp      ' '                   ; If Space
    ret     z                     ;   Get out
    cp      '~'                   ; If tilde
    jr      z,aux_eat_suffix      ;   Skip it
    cp      '0'                   ; If < '0'
    ret     c                     ;   Get out
    cp      ':'                   ; If ':'
    ret     z                     ;   Get out
    jr      c,aux_eat_suffix      ; If <= '9', skip it
    cp      'A'                   ; If < 'A'
    ret     c                     ;   Get out
    cp      'Z'+1                 ; If <= 'Z'
    jr      c,aux_eat_suffix      ;   Skip it
    cp      $80                   ; If not token
    ret     c                     ;   Get out
    ;; Check for non-alphabetic tokens
    cp      PLUSTK                ; If < '+'
    jr      c,aux_eat_suffix      ;   Skip it
    cp      EXPTK+1               ; If <= '^'
    ret     c                     ;   Get out
    cp      ORTK+1                ; If AND or OR
    jr      c,aux_eat_suffix      ;   Skip it
    cp      LESSTK+1              ; If < '<'
    ret     c                     ;   Get out
    jr      aux_eat_suffix        ; Else skip it
