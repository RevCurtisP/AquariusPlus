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
    dec     a                     ;   Else if TRACK
    jr      z,ST_PLAY_TRACK      ;     Not implemented error
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
    ld      iy,play_sample
    call    aux_call              ; Play the sample
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

;-----------------------------------------------------------------------------
; PLAY TRACK Statement
;-----------------------------------------------------------------------------
; LOOP TRACK "/music/songs1/dontstop.pt3"
; LOOP TRACK
ST_LOOP_TRACK:
    rst     CHRGET                ; Skip TRACK
    ld      a,(EXT_FLAGS)
    or      TRK_LOOPS             ; Set repeat flag
    jr      _play_track
;-----------------------------------------------------------------------------
; PLAY TRACK Statement
;-----------------------------------------------------------------------------
; PLAY TRACK "/music/songs1/dontstop.pt3"
; PLAY TRACK
ST_PLAY_TRACK:
    rst     CHRGET                ; Skip TRACK
    ld      a,(EXT_FLAGS)
    and     $FF-TRK_LOOPS         ; Clear repeat flag
_play_track:
    ld      (EXT_FLAGS),a
    call    CHRGT2
    call    nz,load_pt3
.play
    push    hl
    call    pt3_start
    pop     hl
    ret

;-----------------------------------------------------------------------------
; PAUSE TRACK Statement
;-----------------------------------------------------------------------------
ST_PAUSE_TRACK:
    rst     CHRGET                ; Skip TRACK
    push    hl
    call    pt3_disable
    ld      iy,pt3mute
    call    pt3call
    pop     hl
    ret

;-----------------------------------------------------------------------------
; RESUME TRACK Statement
;-----------------------------------------------------------------------------
ST_RESUME_TRACK:
    rst     CHRGET                ; Skip TRACK
    push    hl
    call    pt3_enable
    pop     hl
    ret

;-----------------------------------------------------------------------------
; RESET Statement
;-----------------------------------------------------------------------------
ST_STOP_TRACK:
    rst     CHRGET                ; Skip TRACK
    push    hl
    ld      iy,track_reset
    call    aux_call
    pop     hl
    xor     a
    ld      iy,track_loop
    jp      aux_call

;-----------------------------------------------------------------------------
; TRACKFAST, TRACKLOOP, TRACKSTATUS, and TRACKPEED Functions
; Returns -1 if TRACK is playing/looped/fast
;-----------------------------------------------------------------------------
FN_TRACK:
    rst     CHRGET                ; Skip TRACK
    ld      iy,track_status
    call    aux_call              ; B = Active, C = Looped, E = Fast
    SYNCHKT XTOKEN
    call    push_hlinc_labbck
    cp      FASTK
    ld      l,e
    jr      z,.retstat
    cp      LOOPTK
    ld      l,c
    jr      z,.retstat
    cp      STATK
    ld      l,b
    jr      z,.retstat
    cp      SPEEDTK
    jp      nz,SNERR
    ld      iy,track_speed
    call    aux_call
    jp      SNGFLT
.retstat
    ld      a,l
    jp      FLOAT

;-----------------------------------------------------------------------------
; SET TRACK LOOP ON/OFF
;-----------------------------------------------------------------------------
; SET TRACK LOOP ON: PRINT TRACkLOOP
; SET TRACK LOOP OFF: PRINT TRACkLOOP
; SET TRACK FAST ON: PRINT TRACkFAST
; SET TRACK FAST OFF: PRINT TRACkFAST
ST_SET_TRACK:
    rst     CHRGET                ; Skip TRACK
    SYNCHKT XTOKEN
    cp      SPEEDTK
    jr      z,_track_speed
    ld      iy,track_setmode
    cp      FASTK
    jr      z,.set
    ld      iy,track_loop
    cp      LOOPTK                
    jp      nz,SNERR
.set
    call    get_on_off          ; A = $FF if ON, 0 if OFF
    ld      e,a                 ; For track_setmode
    jp      aux_call_preserve_hl
_track_speed
    call    skip_get_byte       ; A = Speed
    ld      iy,track_setspeed
    call    aux_call
    jp      c,FCERR
    ret

