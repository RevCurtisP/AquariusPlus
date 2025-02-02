;====================================================================
; Graphics Bitmap Drawing Statements and Functions
;====================================================================


;-----------------------------------------------------------------------------
; CLEAR BITMAP [foreground, background]
;-----------------------------------------------------------------------------
; SCREEN 1:CLEAR BITMAP
; SCREEN 1:CLEAR BITMAP 7,0
; SCREEN 3:CLEAR BITMAP 0,7
; SCREEN 0,2:CLEAR BITMAP:PAUSE
; SCREEN 0,2:CLEAR BITMAP 7,1:PAUSE
; SCREEN 0,3:CLEAR BITMAP:PAUSE
ST_CLEAR_BITMAP:              
    rst     CHRGET                ; Skip BIT    
    rst     SYNCHR
    byte    MAPTK
    ld      b,0
    call    nz,_color_args
    ld      iy,bitmap_clear_screen
    jp      aux_call_preserve_hl

;-----------------------------------------------------------------------------
; FILL BITMAP [BYTE byte] [COLOR foreground,background]
;-----------------------------------------------------------------------------
; To Do: FILL COLORMAP (x,y)-(x,y),fg,bg
; SCREEN 1,0:FILL BITMAP BYTES $EE:PAUSE
; SCREEN 1,0:FILL BITMAP BYTES $AA COLOR 7,0:PAUSE
; SCREEN 1,0:FILL BITMAP COLOR 2,8:PAUSE
; SCREEN 3,0:FILL BITMAP BYTES $AA COLOR 6,1:PAUSE
; SCREEN 0,2:FILL BITMAP BYTES $AB COLOR 6,1:PAUSE
; SCREEN 0,3:FILL BITMAP BYTES $AE:PAUSE
; SCREEN 0,3:FILL BITMAP BYTES $AA COLOR 6,1:PAUSE
ST_FILL_BITMAP:
    rst     CHRGET                ; Skip BIT    
    rst     SYNCHR
    byte    MAPTK
    cp      XTOKEN                  
    jr      nz,.fill_color        ; If Extended Token
    rst     CHRGET                ;   Skip it
    rst     SYNCHR                
    byte    BYTETK                ;   Require BYTES
    SYNCHK  'S'
    call    GETBYT                ;   A = FillByte
    ld      b,a                   ;   B = FillByte
    push    hl                    ;   Stack = TxtPtr, RtnAdr
    ld      iy,bitmap_fill_byte
    call    aux_call              ;   Do the fill
    pop     hl                    ;   HL = TxtPtr; Stack = RtnAdr
    call    CHRGT2                ;   Reget next character
    ret     z                     ;   Return if end of statement
.fill_color
    call    parse_colors          ; Require COLOR, A = Color Byte
_fill_color:
    ld      b,a   
    call    get_bitmap_mode
    push    hl                    ; Stack = TxtPtr, RtnAdr  
    ld      iy,bitmap_fill_color
    call    aux_call              ; Do the fill
    jp      c,TOERR
    pop     hl                    ; HL = TxtPtr; Stack = NxtChr, RtnAdr
    ret

;-----------------------------------------------------------------------------
; COLOR fgcolor, bgcolor
; COLOR color
; Set default oolor(s)
;-----------------------------------------------------------------------------
; SCREEN 1,0:COLOR 7,0:PRINT HEX$(COLOR)
; SCREEN 3,0:COLOR 0,7:PRINT HEX$(COLOR)
; SCREEN 0,2:COLOR 6,1:PRINT HEX$(COLOR)
; SCREEN 0,3:COLOR 5:PRINT HEX$(COLOR)
ST_COLOR:
    rst     SYNCHR
    byte    ORTK                  ; Require OR
    call    _color_args
    ld      iy,bitmap_write_color
    call    aux_call_preserve_hl
    jp      no_more               ; Error if any more operands

; Output: B: Color(s)
_color_args:
    ld      a,(EXT_FLAGS)
    and     GFXM_MASK             ; A = BmpMode
    cp      3
    jr      z,.color4bpp          ; If not 4bpp
    call    get_screen_colors     ;   A = fgcolor + bgcolor
    jr      .done                 ; Else 
.color4bpp            
    call    get_byte16            ;   A = color
.done
    ld      b,a
    ld      a,(hl)                
    cp      ','                   ; If NxtChr is comma
    ret     nz
    jp      TOERR                 ;   Too many arguments error

;-----------------------------------------------------------------------------
; COLOR
; Return default oolor(s)
;-----------------------------------------------------------------------------
; SCREEN 0,2:PRINT COLOR
FN_COLOR:    
    rst     CHRGET                ; Skip COL
    rst     SYNCHR
    byte    ORTK                  ; Require OR
    ld      iy,bitmap_read_color  
aux_call_sngflt:
    call    push_hl_labbck        ; Stack = LABBCK, TxtPtr, RtnAdr
    call    aux_call              ; A = Color
    jp      SNGFLT

;-----------------------------------------------------------------------------
; POSX, POSY
; Last X,Y position
;-----------------------------------------------------------------------------
; SCREEN 0,2:PSET (10,23):PRINT POSX;POSY
FN_POS:    
    inc     hl
    ld      a,(hl)
    cp      '('
    jp      z,ABORT_FN
    ex      af,af'
    rst     CHRGET                ; Skip SfxChr
    call    push_hl_labbck        ; Stack = LABBCK, TxtPtr, RtnAdr
    ex      af,af'
    push    af                    ; Stack = SfxChr, LABBCK, TxtPtr, RtnAdr
    ld      iy,bitmap_read_sysvars
    call    aux_call              ; B = BmpClr, C = BmpY, DE = BmpX
    pop     af                    ; A = SfxChr; Stack = LABBCK, TxtPtr, RtnAdr
    cp      'X'                   ; If POSX
    jp      z,FLOAT_DE            ;   Return X
    cp      'Y'
    jp      z,FLOAT_C
    jp      SNERR

;-----------------------------------------------------------------------------
; POINT(x,y)
;-----------------------------------------------------------------------------
; SCREEN 0,2:PSET(1,1):PRINT POINT(1,1):PAUSE
; SCREEN 0,2:PRESET(1,1):PRINT POINT(1,1):PAUSE

; SCREEN 0,2:PSET(299,55):PRINT POINT(299,55):PAUSE
; SCREEN 0,2:PRESET(299,55):PRINT POINT(299,55):PAUSE

; SCREEN 0,3:PSET(1,1),7:PRINT POINT(1,1):PAUSE
; SCREEN 0,3:PSET(1,1),2:PRINT POINT(1,1):PAUSE
; SCREEN 0,3:PRESET(1,1):PRINT POINT(1,1):PAUSE

; SCREEN 0,3:PSET(159,66),9:PRINT POINT(159,66):PAUSE
; SCREEN 0,3:PRESET(159,66):PRINT POINT(159,66):PAUSE


; SCREEN 0,2:? POINT (299,55)
FN_POINT:
    rst     CHRGET                ; Skip POINT
    ld      a,(EXT_FLAGS)
    and     GFXM_1BPP
    jp      z,POINTX
_point:
    ld      de,bitmap_getpixel
    call    _set
    push    hl
    ld      bc,LABBCK
    push    bc
    jp      SNGFLT

;-----------------------------------------------------------------------------
; Bloxel PRESET and PRESET with the EX AF,AF' factored out 
;-----------------------------------------------------------------------------
; SCREEN 0,2:CLEAR BITMAP 7,0:PSET (2,2):PAUSE
; SCREEN 0,2:PSET (4,4),2,7:PAUSE
; SCREEN 0,2:PSET (160,0):PAUSE
; SCREEN 0,2:PSET (160,0),0,3:PAUSE
; SCREEN 0,3:CLEAR BITMAP:COLOR 1:PSET (5,5):PAUSE
; SCREEN 0,3:PSET (159,0),2:PAUSE
; SCREEN 0,3:PSET (159,0),0:PAUSE
ST_PSET:
    ld      a,(EXT_FLAGS)
    and     GFXM_1BPP
    jr      nz,_pset
    call    SCAND                 ; Parse (X,Y)
    call    scale_xy              ; Convert X,Y
    ld      a,1   
    jp      z,RSETCC              ; Semigraphics at screen location?
    ld      (hl),$A0              ; No, store base semigraphic
    jp      RSETCC    
    
; SCREEN 0,2:PRESET (160,0)
ST_PRESET:    
    ld      a,(EXT_FLAGS)
    and     GFXM_1BPP
    jr      nz,_preset
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

; RUN /pbt/bitmap.bas
; A: Mode
_pset:
    ld      de,bitmap_setpixel
    call    _set
    ret
_preset:
    ld      de,bitmap_resetpixel
    jr      _set
_set:
    push    de                    ; Stack = SubAdr, RtnAdr
    call    SCANDYX               ; C = Y, DE = X
    call    _getcolor
    pop     iy                    ; IY = SubAdr; Stack = RtnAdr
    push    hl                    ; Stack = TxtPtr, RtnAdr
    call    aux_call
    jp      c,FCERR               ; Error if illegal coordinate
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret

_getcolor:
    ld      b,0
    ld      a,(hl)
    cp      ','                   ; If no comma
    ret     nz                    ;   Return 0
    push    bc                    ; Stack = Y, Mode, RtnAdr
    push    de                    ; Stack = X, Y, Mode, RtnAdr
    rst     CHRGET                ; Skip comma
    call    _color_args
    jp      z,FCERR               ; Error if color is 0
    ld      a,b
    pop     de                    ; DE = X; Stack = Y, RtnAdr
    pop     bc                    ; BC = Y; Stack = RtnAdr
    ld      b,a
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
    
;-----------------------------------------------------------------------------
; Return current bitmap mode
; Output: A: Mode (0: 40col, 1: 80col, 2: 1bpp, 3: 4bpp)
;  Flags: Carry set if text screen, clear if bitmap screen
; Clobbered: C
;-----------------------------------------------------------------------------
get_bitmap_mode:
    in      a,(IO_VCTRL)          
    ld      c,a
    rra                           ; A = 2: 1bpp, 3: 4bpp
    and     GFXM_MASK             ; Isolate bits
    cp      2                     ; If 1bpp or 2 bpp
    ret     nc                    ;   Return
    ld      a,c
    and     a,VCRTL_80COL_EN      ; A = 0: 40col, 1: 80col
    scf
    ret     z                     ; If 40 col return 0
    ld      a,1                   ; Else Return 1
    ret                     
    
;-----------------------------------------------------------------------------
; Move Bitmap Cursor
; MOVE{B|C) {ABS} (x,y)
;-----------------------------------------------------------------------------
ST_MOVE: