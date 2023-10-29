;=====================================================================================
; S3BASIC Enhancement Hooks 
; These routines are located in the 8K ROM in Bank 2 and neither call nor are 
; called by routines in Bank 1 ROM
; The routines are called through a jump table at $2000, then routed through 
; routine _call_bank2_routine which destroys AF'
;=====================================================================================

;-----------------------------------------------------------------------------
; Save MAIN Line Number Flag
;-----------------------------------------------------------------------------
s3_main_ext:
    pop     de                    ; Pop Return Address
    pop     bc                    ; C = Line Number Flag
    ld      (TEMP3),bc            ; Save it
    push    bc                    ; Flag back on stack
    push    de                      ; Return address back on stack
    jp      SCNLIN                ; Continue to SCNLIN

;-----------------------------------------------------------------------------
; Don't tokenize unquoted literal string after DOS command in direct mode
;-----------------------------------------------------------------------------
s3_stuffh_ext:
    cp      DATATK-':'            ; If DATA
    jp      z,COLIS               ;   Continue STUFFH
    ex      af,af'
    ld      a,(TEMP3)             ; Get Line# Flag
    and     $01                   ; If carry set
    jr      nz,.exaf_nodatt       ;   Continue  STUFFH
    ex      af,af'
    cp      DIRTK-':'             ; If Not DIRTK through CDTK

    jp      c,NODATT              ;
    cp      CDTK-':'+1            ;
    jp      nc,NODATT             ;  Continue STUFFH
.space_loop
    ld      a,(hl)                ; Eat Spaces
    cp      ' '
    jr      nz,.not_space
    call    STUFFS
    jr      .space_loop
.not_space
    ld      b,a                   ; Set up delimiter for STRNG
    cp      '"'                   ; If quotes
    jp      z,STRNG               ;   Go stuff quoted string
.string_loop
    ld      a,(hl)                ; Get character
    or      a                     ; If end of line
    jp      z,CRDONE              ;   Finish it up
    cp      ' '                   ; If space
    jp      z,KLOOP               ;   Stuff it and continue
    cp      ':'                   ; If colon
    jp      z,KLOOP               ;   Stuff it and continue
    call    STUFFS                ; Else Stiff it
    jr      .string_loop          ;   and check next character
.exaf_nodatt:
    ex      af,af'
    jp      NODATT

;-----------------------------------------------------------------------------
; Extended line editor function keys
; Jumped to from INLNC
; On entry: A,C = character typed, B = input buffer character count
;-----------------------------------------------------------------------------
s3_ctrl_keys:
    push    bc                    ; Save character count
    call    in_direct
    jr      c,.dontscreen         ; If Not in Direct Mode
    xor     a
    cp      b
    jr      nz,.dontscreen        ; and Input Buffer is empty
    ld      a,c
;ToDo: Add Ctrl-W = 80 columns, Crtl-T = 40 col screen 0, Ctrl-Y is 40 col screen 1
;Save cursor position and character under RAM in screen RAM hole

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
    cp      'Q'-'K'               ; If not ^N through ^P
    jr      c,.charset
.notrub
    pop     bc                    ;   Restore character count
    jp      NOTRUB                ;   Continue standard Ctrl-key check
.charset
    sub     a,'N'-'K'             ; ^N = 0, ^O = 1, ^P = 2
    xor     1                     ; ^O = 1, ^O = 0, ^P = 2
    push    hl                    ;
    call    select_chrset         ; Select the character set
    pop     hl
.inlinc
    pop     bc                    ;   Restore character count
    jp      INLINC                ;   Wait for next key

