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
; PLAY SAMPLE !131072
ST_PLAY_SAMPLE:
    rst     CHRGET                ; Skip SAMPLE
    call    require_page_addr     ; AF = PgFlg, DE = Addr
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ex      de,hl                 ; HL = Address
    call    play_sample           ; Play the sample
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

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
; Returns -1 if PT3 is playing/looped/fast
;-----------------------------------------------------------------------------
FN_PT3:
    rst     CHRGET                ; Skip PT3
    call    pt3_status            ; B = Active, C = Looped, E = Fast
    rst     SYNCHR
    byte    XTOKEN
    call    push_hlinc_labbck
    cp      FASTK
    ld      l,e
    jr      z,.retstat
    cp      LOOPTK
    ld      l,c
    jr      z,.retstat
    cp      STATK
    ld      l,b
    jp      nz,SNERR
.retstat
    ld      a,l
    jp      float_signed_byte


;-----------------------------------------------------------------------------
; SET PT3 LOOP ON/OFF
;-----------------------------------------------------------------------------
; SET PT3 LOOP ON: PRINT PT3LOOP
; SET PT3 LOOP OFF: PRINT PT3LOOP
; SET PT3 FAST ON: PRINT PT3FAST
; SET PT3 FAST OFF: PRINT PT3FAST
ST_SET_PT3:
    rst     CHRGET                ; Skip PT3
    rst     SYNCHR
    byte    XTOKEN
    ld      ix,pt3_setmode
    cp      FASTK
    jr      z,.set
    ld      ix,pt3_loop
    cp      LOOPTK                
    jp      nz,SNERR
.set
    call    get_on_off          ; A = $FF if ON, 0 if OFF
    ld      e,a                 ; For pt3_setmode
    push    hl
    call    jump_ix
    pop     hl
    ret
    