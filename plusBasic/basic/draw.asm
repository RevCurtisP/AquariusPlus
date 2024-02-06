;====================================================================
; Graphics Bitmap Drawing Statements and Functions
;====================================================================


;-----------------------------------------------------------------------------
; Bloxel PRESET and PRESET with the EX AF,AF' factored out 
;-----------------------------------------------------------------------------
ST_PSET:
; Note: These two lines broke PSET and PRESET - no idea why
;       this will need to be resolved when implementing PSETB snd PRESETB
;    cp      'B'
;    jr      z,ST_PSETB
    call    SCAND                 ; Parse (X,Y)
    call    scale_xy              ; Convert X,Y
    ld      a,1   
    jp      z,RSETCC              ; Semigraphics at screen location?
    ld      (hl),$A0              ; No, store base semigraphic
    jp      RSETCC    
    
ST_PRESET:    
;    cp      'B'
;    jr      z,ST_PRESETB
    call    SCAND                 ; Parse (X,Y)
    call    scale_xy              ; Convert X,Y
    ld      a,0   
    jp      z,RSETCC              ; Semigraphics at screen location?
    ld      (hl),$A0              ; No, store base semigraphic
    jp      RSETCC

; Convert PSET Coordinates to Screen Position and Character Mask
scale_xy: 
    ex      (sp),hl               ; HL = RtnAdr; Stack = TxtPtr     BC=X Coordinate
    push    hl                    ; Stack = RtnAdr, TxtPtr          DE=Y Coordinate
    push    bc                    ; Stack = Xpos, RtnAdr, TxtPtr
    push    de                    ; Stack = Ypos, Xpos, RtnAdr, TxtPtr
    ld      hl,71
    rst     COMPAR                ; If Y greater than 71
    jp      c,FCERR
    push    bc                    ; 
    pop     de    
    ld      a,(LINLEN)            ;  Get Line Length                          
    add     a,a                   ;  Multiply by 2                            
    dec     a                     ;  A = LinLen * 2 - 1                       
    ld      l,a                                                               
    ld      h, 0                  ;  HL = LinLen * 2 - 1                      
    rst     COMPAR                ; If X greater than screen width
    jp      c,FCERR
    pop     de    
    pop     bc    
    ld      a,e                   ; A=Y Coordinate   
    ld      de,(LINLEN)           
    ld      d,0
    ld      hl,SCREEN             
    add     hl,de                 ; Starting Offset
.yloop    
    cp      3                     ; Less than 3?
    jr      c,.ylooped            ; Convert X
    add     hl,de                 ; Add a line to offset
    dec     a                     ; Subtract 3
    dec     a                     ;
    dec     a                     ;
    jr      .yloop                ; Repeat
.ylooped    
    rlca                          ; Multiply remainder by 2
    srl     c                     ; Column = X-Coordinate / 2
    jr      nc,.even              ; Was it odd?
    inc     a                     ; Yes, add one to remainder
.even:    
    add     hl,bc                 ; Add column to screen offset
    ld      de,BITTAB             ;
.bloop:     
    or      a                     ; Check bit#
    jr      z,.blooped              ; If not 0
    inc     de                    ;   Bump table pointer
    dec     a                     ;   Decrement bit#
    jr      .bloop                ;   and repeat
.blooped:     
    ld      a,(hl)                ; Get character at screen offset
    or      $A0                   ; and return it with
    xor     (hl)                  ; bits 5 and 7 cleared
    ret                           ;

ST_PSETB:
    or      a,$FF                 
    push    af                    ; Stack = Mode, RtnAdr
    rst     CHRGET
    call    SCAND                 ; BC = X, DE = Y
    
    
ST_PRESETB:
    xor     a
    push    af                    ; Stack = Mode, RtnAdr
    rst     CHRGET
    call    SCAND                 ; BC = X, DE = Y
    
