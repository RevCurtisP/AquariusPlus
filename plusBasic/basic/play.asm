;====================================================================
; PLAY and related statements and functions
;====================================================================


;-----------------------------------------------------------------------------
; PLAY Statement
; TBD: PLAY notation_string$
;-----------------------------------------------------------------------------
ST_PLAY:
    rst     CHRGET                ; Skip PLAY
    cp      XTOKEN
    jr      nz,.not_extended      ; If extended token
    rst     CHRGET                ;   Skip XTOKEN
    sub     SAMPTK                ;   If SAMPLE
    jr      z,ST_PLAY_SAMPLE      ;     Play it
    dec     a                     ;   Else if PT3
    jr      z,ST_PLAY_PT3         ;     Not implemented error
.not_extended       
    jp      SNERR                 ; Else Syntax error
    
    
;-----------------------------------------------------------------------------
; Play digitized sample 
; Syntax: PLAY SAMPLE @page, address
; TBD: PLAY SAMPLE sampledef$
;-----------------------------------------------------------------------------
; CD "sounds"
; LOAD "bigben1.saq",@40,0
; PLAY SAMPLE @40,0
ST_PLAY_SAMPLE:
    rst     CHRGET                ; Skip SAMPLE
    cp      '@'
    jr      nz,.not_page          ; If followed by @
    call    req_page_arg          ;   A = Page
    cp      40                    ;   If reserved page
    jp      c,FCERR               ;     Illegal quantity error
    push    af                    ;     Stack = Page, RtnAdr
    ld      de,0                  ;   Default Address to 0
    call    CHRGT2                ;   Reget character
    jr      z,.no_addr            ;   If not end of statement
    SYNCHK  ','                   ;     Require comma
    call    get_int16k            ;     DE = Address
.no_addr
    pop     af                    ;   A = Page; Stack = RtnAdr
    push    hl                    ;   Stack = TxtPtr, RtnAdr
    ex      de,hl                 ;   HL = Address
    call    play_sample           ;   Play the sample
    pop     hl                    ;   HL = TxtPtr; Stack = RtnAdr
    ret
.not_page
    jp      SNERR                 ; Else Syntax error

;-----------------------------------------------------------------------------
; PLAY PT3 Statement
;-----------------------------------------------------------------------------
; LOOP pt3 "/music/songs1/dontstop.pt3"
; LOOP PT3
ST_LOOP_PT3:
    rst     CHRGET                ; Skip PT3
    ld      a,(EXT_FLAGS)
    or      PT3_LOOPS             ; Set repeat flag
    jr      _play_pt3
;-----------------------------------------------------------------------------
; PLAY PT3 Statement
;-----------------------------------------------------------------------------
; play pt3 "/music/songs1/dontstop.pt3"
; play pt3
ST_PLAY_PT3
    rst     CHRGET                ; Skip PT3
    ld      a,(EXT_FLAGS)
    and     $FF-PT3_LOOPS         ; Clear repeat flag
_play_pt3:
    ld      (EXT_FLAGS),a
    call    CHRGT2
    call    nz,load_pt3
.play
    push    hl
    call    pt3_start
    pop     hl
    ret

;-----------------------------------------------------------------------------
; PAUSE PT3 Statement
;-----------------------------------------------------------------------------
ST_PAUSE_PT3
    rst     CHRGET                ; Skip PT3
    push    hl
    call    pt3_disable
    ld      iy,pt3mute
    call    pt3call
    pop     hl
    ret

;-----------------------------------------------------------------------------
; RESUME PT3 Statement
;-----------------------------------------------------------------------------
ST_RESUME_PT3
    rst     CHRGET                ; Skip PT3
    push    hl
    call    pt3_enable
    pop     hl
    ret

;-----------------------------------------------------------------------------
; RESET Statement
;-----------------------------------------------------------------------------
ST_STOP_PT3:
    rst     CHRGET                ; Skip PT3
    push    hl
    call    pt3_reset
    pop     hl
    ret

