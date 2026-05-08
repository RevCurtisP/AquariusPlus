;=============================================================================
; Text Screen Control Assembly Routines
;=============================================================================

;-----------------------------------------------------------------------------
; Enable Color Print Mode and set Print Colors
; Input: A: Colors
;-----------------------------------------------------------------------------
set_color:
    ld      (SCOLOR),a
    ld      a,(SCREENCTL)
    or      SCRCOLOR
    jr      _write_screenctl

;-----------------------------------------------------------------------------
; Disable Color Print Mode
; Input: A: Colors
;-----------------------------------------------------------------------------
set_color_off:
    ld      a,(SCREENCTL)
    push    af
    and     $FF-SCRCOLOR
    call    _write_screenctl
    pop     af      
    and     CRSR_OFF
    ret     nz
    ret

_write_screenctl:
    ld      (SCREENCTL),a
    and     SCRCOLOR              ; Isolate Screen Control bit
    ld      e,a                   ; C = ScrColor
.direct
    call    in_direct             ; Clobbers B and C
    ret     c                     ; If Direct mode
    ld      a,(BASYSCTL)
    and     $FF-SCRCOLOR
    or      e
    ld      (BASYSCTL),a
    ret

;-----------------------------------------------------------------------------
; Reset Screen to Text only and default palette
; Clobbers: A,BC,D,HL
;-----------------------------------------------------------------------------
screen_reset:
    in      a,(IO_VCTRL)
    and     VCTRL_TEXT_BITS
    or      VCTRL_TEXT_EN
    out     (IO_VCTRL),a
    call    .linlen
    ld      (LINLEN),a
    call    bitmap__set_mode_nobuff
    ld      a,(SCREENCTL)
    and     SCRCHRSET+SCRCHRMOD
    ld      (SCREENCTL),a
    xor     a
    jp      palette_reset

.linlen
    and     VCRTL_80COL_EN
    ld      a,40
    ret     z
    add     a,a                   ; Set LinLen to 80
    ret

;-----------------------------------------------------------------------------
; Expand Convert IO_VCTRL to screen status
; Output: A,L: Bit 7-6: Text Mode 5: Border; 4: Priority; 3: Sprites; 2-1: Graphics
;           H: (IO_VCTRL)
;-----------------------------------------------------------------------------
screen_status:
    ld      bc,0
    ld      de,0
    in      a,(IO_VCTRL)
    ld      h,a                   ; H = (IO_VCTRL)
    ld      l,0
    bit     0,h                   ; Test VCTRL_TEXT_EN
    jr      z,.skip               ; If text enabled
    ld      l,192
    bit     6,h                   ;   Test VCRTL_80COL_EN
    jr      nz,.skip              ;   If not in 80 column mode
    ld      l,128
    bit     7,h                   ;     Test VCTRL_TEXT_PAGE
    jr      nz,.skip              ;     If in screen page 1
    ld      l,64
.skip
    ld      a,h                   ; A = IO_VCTRL
    and     VCTRL_TEXT_MASK       ; Strip text screen bits
    or      l                     ; Or in text mode
    ld      l,a                   ; L = Screen status
    ret
