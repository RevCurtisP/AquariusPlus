;====================================================================
; JOY Functions core routines
;====================================================================

;--------------------------------------------------------------------------------------------------------------------------------
; JOY(Idx/-ArgVal)
; JOYD() - Down (0/-1)
; JOYJ() - Joystick direction (1-8)
; JOYK() - Button mumber (1-6)
; JOYL() - Left (0/-1)
; JOYP() - Thumbpad direction (1-16)
; JOYR() - Right (0/-1)
; JOYU() - Up (0/-1)
; Extended Bluetooth Controller Functions
; JOYBA() - A button                  BIT(G$,48)
; JOYBB() - B button                  BIT(G$,49)
; JOYBG() - Guide button              BIT(G$,53)
; JOYBL() - Left Shoulder button      BIT(G$,57)
; JOYBM() - Menu button               BIT(G$,54)
; JOYBR() - Right Shoulder button     BIT(G$,58)
; JOYBS() - Share button              BIT(G$,63)
; JOYBT() - Left Stick button         BIT(G$,55)
; JOYBU() - Right Stick button        BIT(G$,56)
; JOYBV() - View button               BIT(G$,52)
; JOYBX() - X button                  BIT(G$,50)
; JOYBY() - Y button                  BIT(G$,51)
; JOYDD() - D-pad Down                BIT(G$,60)
; JOYDL() - D-pad Left                BIT(G$,61)
; JOYDR() - D-pad Right               BIT(G$,62)
; JOYDU() - D-pad Up                  BIT(G$,59)
; JOYLT() - Left Trigger              ASC(G$,5)
; JOYLX() - Left Stick X              BYTE(G$)
; JOYLY() - Left Stick Y              BYTE(G$,2)
; JOYRT() - Right Trigger             ASC(G$,6)
; JOYRX() - Right Stick X             BYTE(G$,3)
; JOYRY() - Right Stick Y             BYTE(G$,4)
;--------------------------------------------------------------------------------------------------------------------------------
; Called from FNJOY
; On entry, BC = JOY suffix
bas_joy:
    ld      a,b                   ; A = FnSfx1
    or      a                     ; If JOY()
    jr      nz,.extended
    call    CONINT
    call    read_gamepad          ;
    xor     a                     ;   Clear Carry and Sign
    ld      a,e                   ;   Return inverted port value
    ret
.extended
    cp      '$'                   ; If JOY$
    scf                           ;   Return carry set
    ret     z
    ld      a,c
    or      a
    jr      nz,_joy_gamectrl      ; JOYx()
    ld      a,b                   ;   A = FnSfx1
    push    af                    ;   Stack = FnSfxs, LABBCK, TxtPtr, RtnAdr
    call    FRCINT                ;   DE = FnArg
    ld      a,e
    inc     d                     ;
    jr      z,.negate             ;   If FnArg >= 0
    dec     d
    jp      nz,FCERR
    call    read_gamepad          ;     A = ! PrtVal
    byte    $16                   ;   Else (LD D, over NEG)
.negate
    neg                           ;     A = - FnArg
.decode
    ld      e,a                   ;   E = JoyVal
    pop     af                    ;   A = FnSfx2
    cp      'D'
    ld      d,1
    jr      z,.udlr
    cp      'J'                   ;   If JOYJ
    jp      z,decode_game_joystk  ;     Decode joystick direction
    cp      'K'                   ;   If JOYK
    jp      z,decode_game_button  ;     Decode button
    cp      'L'
    ld      d,8
    jr      z,.udlr
    cp      'P'                   ;   If JOYP
    jp      z,decode_game_dpad    ;     Decode dpad direction
    cp      'R'
    ld      d,2
    jr      z,.udlr
    cp      'U'
    ld      d,4
    jr      z,.udlr
    jp      SNERR                 ;   Else Syntax error

.udlr
    ld      a,e
    rla                           ; If Bit 7 set (Keys 2, 3, 5, or 6)
    jr      c,_zero               ;   Return 0
    rra                           ; Put bits back
    and     d
    ret     z
    ld      a,-1
    or      a
    ret
_zero:
    xor     a
    ret


; Game Controller String Offsets
_gc_lstkx  equ 0  ;( Signed ) Left stick X
_gc_lstky  equ 1  ;( Signed ) Left stick Y
_gc_rstkx  equ 2  ;( Signed ) Right stick X
_gc_rstky  equ 3  ;( Signed ) Right stick Y
_gc_ltrgr  equ 4  ;(Unsigned) Left trigger
_gc_rtrgr  equ 5  ;(Unsigned) Right trigger
_gc_butns  equ 6  ;(L-Endian) Button Status

_joy_gamectrl:
    push    bc                    ; Stack = FnSfxs, RtnAdr
    call    GETYPE
    jr      z,.stringarg          ; If FnArg is numeric
    call    CONINT                ;   Convert to byte
    ld      de,FBUFFR
    call    espx_get_gamectrl
    ld      a,c
    or      a
    jr      nz,.decode
    pop     bc
    ret
.stringarg
    call    faclo_addr_len        ; DE = StrAdr, BC = StrLen
.decode
    ld      a,c                   ; A = StrLen
    or      a                     ; If StrLen = 0
    jp      z,aux_eserr           ;   Empty string error
    cp      8                     ; If StrLen <> 8
    jp      nz,aux_slerr          ;   String Length Error
    pop     bc                    ; BC = FnSfx1; Stack = RtnAdr
    ex      de,hl                 ; HL = StrAdr
    ld      d,0                   ; Clear StrOfs MSB
    ld      a,b                   ; A = FnSfx1
    cp      'B'
    jr      z,_get_button
    cp      'D'
    jr      z,_get_dpad
    cp      'L'
    jr      z,.left
    cp      'R'
    jr      z,.right
    jp      SNERR

.left
    ld      a,c
    ld      e,_gc_ltrgr
    cp      'T'
    jr      z,.unsigned
    ld      e,_gc_lstkx
    cp      'X'
    jr      z,.signed
    ld      e,_gc_lstky
    cp      'Y'
    jr      z,.signed
    jp      SNERR

.right
    ld      a,c
    ld      e,_gc_rtrgr
    cp      'T'
    jr      z,.unsigned
    ld      e,_gc_rstkx
    cp      'X'
    jr      z,.signed
    ld      e,_gc_rstky
    cp      'Y'
    jr      z,.signed
    jp      SNERR

.signed
    add     hl,de
    ld      a,(hl)
    or      a
    ret

.unsigned
    add     hl,de
    xor     a
    ld      a,(hl)
    ret


; Button Bit Masks
_button_a       equ  1<<0  ; A
_button_b       equ  1<<1  ; B
_button_x       equ  1<<2  ; X
_button_y       equ  1<<3  ; Y
_button_view    equ  1<<4  ; View
_button_guide   equ  1<<5  ; Guide
_button_menu    equ  1<<6  ; Menu
_button_lstk    equ  1<<7  ; Left Stick
_button_rstk    equ  1<<8  ; Right Stick
_button_lsh     equ  1<<9  ; Left Shoulder
_button_rsh     equ  1<<10 ; Right Shoulder
_dpad_up        equ  1<<11 ; D-pad up
_dpad_down      equ  1<<12 ; D-pad down
_dpad_left      equ  1<<13 ; D-pad left
_dpad_right     equ  1<<14 ; D-pad right
_button_share   equ  1<<15 ; Share

; Check for D-pad Press
_get_dpad:
    ld      a,c
    call    _check_dpad           ; BC = BitMsk
    jr      _check_bit
; Check for Button Press
_get_button:
    ld      a,c
    call    _check_button         ; BC = BitMsk
_check_bit:
    ld      a,b
    and     d
    jr      nz,.notz
    ld      a,c
    and     e                     ; If BtnBit set
.notz
    ld      a,0                   ;  Return 0
    ret     z                     ; Else
    dec     a                     ;  Return -1
    ret

_check_button:
    call    _button_bytes
    ld      bc,_button_a
    cp      'A'
    ret     z
    ld      bc,_button_b
    cp      'B'
    ret     z
    ld      bc,_button_guide
    cp      'G'
    ret     z
    ld      bc,_button_lsh
    cp      'L'
    ret     z
    ld      bc,_button_menu
    cp      'M'
    ret     z
    ld      bc,_button_rsh
    cp      'R'
    ret     z
    ld      bc,_button_share
    cp      'S'
    ret     z
    ld      bc,_button_lstk
    cp      'T'
    ret     z
    ld      bc,_button_rstk
    cp      'U'
    ret     z
    ld      bc,_button_view
    cp      'V'
    ret     z
    ld      bc,_button_x
    cp      'X'
    ret     z
    ld      bc,_button_y
    cp      'Y'
    ret     z
    jp      SNERR

_check_dpad:
    call    _button_bytes
    ld      bc,_dpad_down
    cp      'D'
    ret     z
    ld      bc,_dpad_left
    cp      'L'
    ret     z
    ld      bc,_dpad_right
    cp      'R'
    ret     z
    ld      bc,_dpad_up
    cp      'U'
    ret     z
    cp      'X'
    jr      z,_joydx
    cp      'Y'
    jr      z,_joydy
    jp      SNERR

_button_bytes:
    ld      de,_gc_butns
    add     hl,de
    ld      e,(hl)
    inc     hl
    ld      d,(hl)
    ret

_joydx:
_joydy:
    jp      SNERR

; Called from _joy_string
bas_joy_string:
    call    CONINT                ; A = JoyArg
    push    af                    ; Stack = CtlIdx, LABBCK, TxtPtr, RtnAdr
    ld      a,8                   ; A = BufLen
    call    STRINI                ; HL = StrDsc, DE = StrAdr
    pop     af                    ; C = CtlIdx
    jp      espx_get_gamectrl     ; Read comtroller and return

