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
    cp      32                    ;  If not RAM page
    jp      c,FCERR               ;     Illegal quantity error
    cp      50                    ;   If reserved page
    jp      nc,FCERR              ;     Illegal quantity error
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
; LOOP PT3 "/music/songs1/dontstop.pt3"
; LOOP PT3
ST_LOOP_PT3:
    rst     CHRGET                ; Skip PT3
    ld      a,(EXT_FLAGS)
    or      PT3_LOOPS             ; Set repeat flag
    jr      _play_pt3
;-----------------------------------------------------------------------------
; PLAY PT3 Statement
;-----------------------------------------------------------------------------
; PLAY PT3 "/music/songs1/dontstop.pt3"
; PLAY PT3
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
    xor     a
    jp      pt3_loop
    ret

;-----------------------------------------------------------------------------
; PT3STATUS and PT3LOOP Functions
; Returns -1 if PT3 is playing/looped
;-----------------------------------------------------------------------------
FN_PT3:
    rst     CHRGET                ; Skip PT3
    call    pt3_status            ; B = Active, C = Looped
    rst     SYNCHR
    byte    XTOKEN
    cp      STATK                 
    jr      nz,.loop              ; If STATUS
    rst     CHRGET                ;   Skip it
    ld      a,b                   ;   A = Active
    jr      .retstat              ; Else
.loop
    rst     CHRGET                ;   Skip LOOP
    ld      a,c                   ;   A = Looped
.retstat
    push    hl
    ld      bc,LABBCK
    push    bc
    jp      float_signed_byte

;-----------------------------------------------------------------------------
; SET PT3 LOOP ON/OFF
;-----------------------------------------------------------------------------
; SET PT3 LOOP ON: PRINT PT3LOOP
; SET PT3 LOOP OFF: PRINT PT3LOOP
ST_SET_PT3:
    rst     CHRGET                ; Skip PT3
    rst     SYNCHR
    byte    XTOKEN
    rst     SYNCHR                ; Require LOOP
    byte    LOOPTK                
    call    check_on_off          ; A = $FF if ON, 0 if OFF
    jp      pt3_loop
    