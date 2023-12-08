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
ST_PLAY_PT3
    jp      GSERR

;-----------------------------------------------------------------------------
; RESET Statement
;-----------------------------------------------------------------------------
ST_RESET:
    rst     CHRGET                ; Skip RESET
    rst     SYNCHR
    byte    XTOKEN
    rst     SYNCHR
    byte    PT3TK                 ; Require PT3
    push    hl
reset_pt3:
    call    pt3reset
    pop     hl
    ret

