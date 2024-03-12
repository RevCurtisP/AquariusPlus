;====================================================================
; Graphics Bitmap Drawing Statements and Functions
;====================================================================

;-----------------------------------------------------------------------------
; GETBITMAPBC, GETBITMAPBX, GETBITMAPBY
; GETBITMAPCC, GETBITMAPCX, GETBITMAPCY
;-----------------------------------------------------------------------------
; PRINT GETBITMAPBC;GETBITMAPBX;GETBITMAPBY
; PRINT GETBITMAPCC;GETBITMAPCX;GETBITMAPCY
FN_GETBITMAP:
    call    parse_bitmap
    ld      de,bitmap_read_sysvars
    jr      z,.skip
    ld      de,bitmapc_read_sysvars
.skip
    push    de                    ; Stack = GetRtn, RtnAdr
    call    parse_cxy             ; E = C/X/Y
    pop     iy                    ; IY = GetRtn; Stack = RtnAdr
    call    push_hl_labbck        ; Stack = LABBCK, TxtPtr, RtnAdr
    push    de                    ; Stack = C/X/Y, LABBCK, TxtPtr, RtnAdr
    call    aux_call              ; A = BmpClr, BC = BmpY, DE = BmpX
    pop     hl                    ; L = C/X/Y; Stack = LABBCK, TxtPtr, RtnAdr
    sla     l                     ; Set Carry if Y, NotZero if X
    jp      c,FLOAT_BC            ; If Y, return BmpY
    jp      nz,FLOAT_DE           ; If X, return BmpX
    jp      SNGFLT                ; Else return BmpClr

;-----------------------------------------------------------------------------
; SET BITMAPBC fgcolor,bgcolor|BITMAPBX xpos|BITMAPBY ypos...
;-----------------------------------------------------------------------------
; SET BITMAPBC 7,3 BITMAPBX 300 BITMAPBY 65
; SET BITMAPCC 15 BITMAPCX 300 BITMAPCY 65
ST_SET_BITMAP:
    call    parse_bitmap
.loop
    push    af                    ; Stack = B/C, RtnAdr
    call    parse_cxy             ; E = C/X/Y
    sla     e                     ; Set Carry if Y, NotZero if X
    jr      nc,.noty              ; If Y
    call    GETBYT                ;   DE = Y
    ld      b,d
    ld      c,e                   ;   BC = Y
    pop     af                    ; A = B/C; Stack = RtnAdr
    ld      iy,bitmap_write_ypos  
    jr      z,.next
    ld      iy,bitmapc_write_ypos  
    jr      .next
.noty
    jr      z,.notx               ; Else if X
    call    GETINT                ;   DE = X
    pop     af                    ; A = B/C; Stack = RtnAdr
    ld      iy,bitmap_write_xpos
    jr      z,.next
    ld      iy,bitmapc_write_xpos
    jr      .next                 ; Else
.notx
    pop     af                    ;   A = B/C; Stack = RtnAdr
    jr      nz,.notb
    call    get_screen_colors     ;   A = Colors
    ld      iy,bitmap_write_color 
    jr      .next
.notb
    call    get_byte16
    ld      iy,bitmapc_write_color 
.next
    call    aux_call
    call    CHRGT2
    ret     z
    jr      ST_SET_BITMAP

;-----------------------------------------------------------------------------
; POINT, POINTB, and POINTC
;-----------------------------------------------------------------------------
; PSETB(299,55):PRINT GETBITMAPBX;GETBITMAPBY;POINTB(299,55)
; PRESETB(299,55):? POINTB(299,55)
; PSETC(150,33),7:PRINT GETBITMAPCX;GETBITMAPCY;POINTC(150,33)
; PRESETC(150,33):? POINTC(150,33)
FN_POINT:
    rst     CHRGET                ; Skip POINT
    cp      'B'
    jr      z,_pointb
    cp      'C'
    jp      nz,POINTX
_pointc:
    ld      bc,bitmapc_getpixel
    jr      _point
_pointb:    
    ld      bc,bitmap_getpixel
_point:
    call    _psetb
    push    hl
    ld      bc,LABBCK
    push    bc
    jp      SNGFLT

;-----------------------------------------------------------------------------
; Bloxel PRESET and PRESET with the EX AF,AF' factored out 
;-----------------------------------------------------------------------------
ST_PSET:
; Note: These two lines broke PSET and PRESET - no idea why
;       this will need to be resolved when implementing PSETB snd PRESETB
    cp      'B'
    jr      z,ST_PSETB
    cp      'C'
    jp      z,ST_PSETC
    call    SCAND                 ; Parse (X,Y)
    call    scale_xy              ; Convert X,Y
    ld      a,1   
    jp      z,RSETCC              ; Semigraphics at screen location?
    ld      (hl),$A0              ; No, store base semigraphic
    jp      RSETCC    
    
ST_PRESET:    
    cp      'B'
    jr      z,ST_PRESETB
    cp      'C'
    jp      z,ST_PRESETC
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

_pset_code_size = $ - ST_PSET

; RUN /pbt/bitmap.bas
ST_PSETB:
    ld      bc,bitmap_setpixel
    jr      _psetb
ST_PRESETB:
    ld      bc,bitmap_resetpixel
_psetb:
    push    bc                    ; Stack = SubAdr, RtnAdr
    rst     CHRGET                ; Skip B
    call    SCANDYX               ; C = Y, DE = X
    pop     iy                    ; IY = SubAdr; Stack = RtnAdr
    push    hl                    ; Stack = TxtPtr, RtnAdr
    call    aux_call
    jp      c,FCERR               ; Error if illegal coordinate
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

; PRESETC (1,1)
; PRESETC (0,1)
ST_PRESETC:
    ld      bc,bitmapc_resetpixel
    push    bc                    ; Stack = SubAdr, RtnAdr
    rst     CHRGET                ; Skip C
    call    SCANDYX               ; C = Y, DE = X
    jr      _psetc

; CLEAR BITMAPC
; PSETC (1,1),12
; PSETC (0,1),5
ST_PSETC:
    ld      bc,bitmapc_setpixel
    push    bc                    ; Stack = SubAdr, RtnAdr
    rst     CHRGET                ; Skip C
    call    SCANDYX               ; C = Y, DE = X
    push    bc                    ; Stack = Y, SubAdr, RtnAdr
    push    de                    ; Stack = X, Y, SubAdr, RtnAdr
    ld      e,$FF                 ; -1 = Use default colot
    cp      ','                   ; If comma
    call    z,get_comma_byte16    ;   E = Color
    ld      a,e                   ; A = Color
    pop     de                    ; DE = X; Stack = Y, SubAdr, RtnAdr
    pop     bc                    ; C = Y ; Stack = SubAdr, RtnAdr
_psetc:
    pop     iy                    ; IY = SubAdr; Stack = RtnAdr
    push    hl                    ; Stack = TxtPtr, RtnAdr
    call    aux_call
    jp      c,FCERR               ; Error if illegal coordinate
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

; Scan (X,Y) into DE,C    
SCANDYX:
    SYNCHK  '('                   
    call    GETINT                ; DE = X
    push    de                    ; Stack = X, RtnAdr
    SYNCHK  ','
    call    GETBYT                ; A = Y
    ld      c,a                   ; C = Y
    pop     de                    ; DE = X; Stack = RtnAdr
    SYNCHK  ')'
    ret