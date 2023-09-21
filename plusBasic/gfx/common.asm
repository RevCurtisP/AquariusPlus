This is getting split up

;=====================================================================================
; Graphics Common Routines
; Only supports 72x48 bloxel graphics for now.
;=====================================================================================



; Origin this code in bank 2 so that page 36 can be swapped into bank 2 to access
; the system variables? This would make the code usable by cartridges.
; If the routines are vectored through the jump table at $2100, they can be called
; by machine code in bank 2 as well. Maybe a single vector that uses A as the
; routine to call and passes through BC, HL, and DE.


; To Do:
; Check and Maybe Fix Aspect Ratio
; Change jumps to ERR routines to routines returning error code
; Call gfx_startup during Cold Boot
; Call gfx_init at beginning of RUN 

; For testing, load into RAM at $8000 and use CALLs
; To assemble:
;   zmac --zmac -o gfx.cim -o gfx.lst gfx.asm

include "../regs.inc"



_set_mode:
_get_mode:
    ret


_set_point:
    ld      bc,(bc_shadow)
    ld      (GRPACX),bc
    ld      de,(de_shadow)
    ld      (GRPACY),de
    ret

_line:
    push    hl
    ld      bc,(bc_shadow)
    ld      de,(de_shadow)
    call    gfx_draw_line  
    pop     hl
    ret

_pset:
    push    hl
    ld      bc,(bc_shadow)
    ld      de,(de_shadow)
    call    gfx_draw_pixel  
    pop     hl
    ret

_preset:
    push    hl
    ld      bc,(bc_shadow)
    ld      de,(de_shadow)
    call    gfx_erase_pixel  
    pop     hl
    ret

;Set Up Graphics System Variables
gfx_startup: 
    ld      de,$0704              ; Default = White, Current = Blue 
    ld      (FORCLR),de           ; Set Foreground Colors
    ld      a,$C3                 ; JP Instruction
    ld      (MAXUPD),a            ;{GWB} Major Axis Move Update
    ld      (MINUPD),a            ;{GWB} Minor Axis Move Update
    ld      (OPCJMP),a            ; Draw Operator Routine
;Initialize Graphics System Variables
gfx_init:     
    xor     a                     ; Store 0 in
    ld      (DRWSCL),a          
    ld      (DRWFLG),a          
    ld      (DRWANG),a    
    ld      (GRPACX),a            ; Set Last X Coordinate to 0
    ld      (GRPACX+1),a          
    ld      (GRPACY),a            ; Set Last Y Coordinate to 0
    ld      (GRPACY+1),a      
    ret

; Callers: gfx_draw_line
;          GLINE2

DOGRPH: call    scale_x_y          ; CHEATY SCALING - JUST TRUNCATE FOR NOW
        call    ex_bcde_gpos          
        call    scale_x_y          
DOGRP2: call    get_y_delta           ; GET COUNT DIFFERENCE IN [H,L]
        call    c,ex_bcde_gpos        ; IF CURRENT Y IS SMALLER NO EXCHANGE
        push    de              ; SAVE Y1 COORDINATE
        push    hl              ; SAVE DELTA Y
        call    get_x_delta           
        ex      de,hl           ; PUT DELTA X INTO [D,E]
        ld      hl,pixel_right       ; ASSUME X WILL GO RIGHT
        jr      nc,LINCN2        
        ld      hl,pixel_left        
LINCN2: ex      (sp),hl         ; XTHL
        rst     COMPAR          ; SEE WHICH DELTA IS BIGGER
        ; jr      nc,YDLTBG       ; get_y_deltaA IS BIGGER OR EQUAL 
        ld      (MINDEL),hl     ; SAVE MINOR AXIS DELTA (Y)
        pop     hl              ; GET X ACTION ROUTINE
        ld      (MAXUPD+1),hl   ; SAVE IN MAJOR ACTION Address
        ld      hl,pixel_down        ; ALWAYS INCREMENT 
        ld      (MINUPD+1),hl   ; WHICH IS THE MINOR AXIS
        ex      de,hl           ; [H,L]=DELTA X=MAJOR DELTA
        jp      LINCN3          ; MERGE WITH YDLTBG CASE AND DO DRAW

; Get Screen Address and and Pixel Index from Character X, Character Y
; Uses: [B,C] = Character X Position 
;       [D,E] = Pixel Y Position 
; Sets: PINDEX = [A] = 0, 1, or 2
;       CURLOC = [H,L] = Screen Address
MAPXYP: push    hl                ; Save [H,L] 
        push    de                ; Save [D,E] (YPOS)
        ld      hl,SCREEN+40      ; Address = Column 0, Line 1
        ld      a,e               ; Mask = YPOS
        ld      de,40             ; Screen Width
MAPPLP: sub     3                 ; Mask = Mask - 3
        jr      c,MAPPAD          ; If Positive
        add     hl,de             ;   Add Screen Width to Screen Address
        jr      MAPPLP            ;   and Loop
MAPPAD: add     a,3               ; Mask = Mask + 3
        add     a,a               ; 
        sra     c                 ; 
        jr      nc,MAPXYA         ; If Mask is Even
        inc     a                 ;   Mask = Mask + 1
MAPXYA: add     hl,bc             ; Add XPOS to Screen Address
        ld      (PINDEX),a        ; Store Mask
        ld      (CURLOC),hl       ; Store Address
        pop     de                ; Get YPOS back into DE
        pop     hl                ; Restore HL
        ret                       

; Register Exchange Routines
; ex_de_gypos EXCHANGES [D,E] WITH GYPOS
; ex_bcde_gpos PERFORMS BOTH OF THE ABOVE
; NONE OF THE OTHER REGISTERS IS AFFECTED
ex_de_gypos:  
    push    hl
    ld      hl,(GYPOS)
    ex      de,hl
    ld      (GYPOS),hl
    pop     hl
    ret
ex_bcde_gpos: 
    call    ex_de_gypos
ex_bc_gxpos:  
    push    hl
    push    bc
    ld      hl,(GXPOS)
    ex      (sp),hl
    ld      (GXPOS),hl
    pop     bc
    pop     hl
    ret

YDLTBG: ex      (sp),hl         ; XTHL
        ld      (MINUPD+1),hl   ; SAVE Address OF MINOR AXIS UPDATE
        ld      hl,pixel_down        ; Y IS ALWAYS INCREMENT MODE
        ld      (MINUPD+1),hl   ; Swapped
        ld      hl,pixel_down        ;
        ld      (MAXUPD+1),hl   ; SAVE AS MAJOR AXIS UPDATE
        ex      de,hl           ; [H,L]=DELTA X
        ld      (MINDEL),hl     ; SAVE MINOR DELTA
        pop     hl              ; [H,L]=DELTA Y=MAJOR DELTA

;-----------------------------------------------------------------------------
; Error if Point is Not on Screen; See if text coordinates are off screen
;-----------------------------------------------------------------------------
check_gmax: 
    ld       a,23
    ld       (GYMAX),a       ;;Mex Y = 23 Rows
    ld       a,39
    ld       (GXMAX),a       ;;Mac X = 39 Coumns
    jr       _compare_x_y

;-----------------------------------------------------------------------------
; Scale coordinates to fit screen
; Callers: gfx_draw_filled - Draw filled rectangle
;          DOGRPH - Base line drawing routine
;          PPSETC - Draw/erase pixel
;          PUT1   - Write graphics array
;          DRWABS - 
; Input: BC: Pixel X Coordinate
;        DE: Pixel Y Coordinate
; Clobbers: A
;-----------------------------------------------------------------------------

scale_x_y: 
    ld      a,71              
    ld      (GYMAX),a         ; Max Y = 71 Pixels
    ld      a,79              
    ld      (GXMAX),a         ; Max X = 79 Pixels
_compare_x_y: 
    push    hl                ;Save Registers
    push    bc                
    push    de                
    ld      a,(GYMAX)         
    ld      h,0               ; [H,L] = GYMAX
    ld      l,a               
    bit     7,d               ; If [D,E] is Negative
    jr      nz,_zero_ymax         ;   [H,L] = 0 and Set Carry
    rst     COMPAR            ; Else Compare [D,E] and [H,L]
_scale_bc: 
    ld      d,b               ; [D,E] = [B,C]
    ld      e,c               
    ld      b,0               
    jr      nc,_compare_gxmax         
    ex      (sp),hl           
    inc     b                 
_compare_gxmax: 
    ld      a,(GXMAX)         
    ld      h,0               ; [H,L] = GXMAX
    ld      l,a               
    bit     7,d               ; If [D,E] is Negative
    jr      nz,_xmax_zero         ;   [H,L] = 0 and Set Carry
    rst     COMPAR            ; Else Compare [D,E] and [H,L]
SCLXY3: 
    pop     de                
    jr      c,SCLXY4           
    rr      b                 
    db      $06               ; LD B, over EX
SCLXY4: ex      (sp),hl              
    pop     bc                
    ccf                       
    pop     hl                
    ret                       

_zero_ymax: 
    ld      l,0               ; [L] = 0
    scf                       ; Set Carry Flag
    jr      _scale_bc            ; and Continue
  
_xmax_zero: 
    ld      l,0                    ; [L] = 0
    scf                       ; Set Carry Flag
    jr      SCLXY3                 ; and Continue


;-----------------------------------------------------------------------------
; Renamed shared routines
;-----------------------------------------------------------------------------
 
fetch_current_loc: ld      a,(PINDEX)        ; Load Bit Index 
        ld      hl,(CURLOC)       ; Load Current Point Address
        ret                     

;;Convert FAC to Integer and Return in [H,L]
force_int_hl: 
    rst     FSIGN                 ;[M80] GET THE SIGN OF THE FAC IN A
    jp      m,.is_negative        ; If Positive
    ex      de,hl                 ;   Save DE in HL
    call    FRCINT                ;   Convert FAC into Integer in DE
    ex      de,hl                 ;   Put Result into HL, Restore DE
    ret                           ; Else
.is_negative: 
    push    de                    ;   Save DE
    call    NEG                   ;   Negate FAC
    call    FRCINT                ;   Convert FAC into Integer in DE
    call    NEG                   ;   Un-Negate FAC
    ld      hl,0    
    or      a   
    sbc     hl,de                 ;   Put 0 - Integer into HL
    pop     de                    ;   and Restore DE
    ret


;;!this aspect ratio is wrong
get_aspect_ratio: 
    ld      de,204          ; Aspect Ration = 318:204 (6.25:4)
    ld      hl,318
    ret

; get_x_delta SETS [H,L]=ABS(GXPOS-[B,C]) AND SETS CARRY IF [B,C].GT.GXPOS
; ALL REGISTERS EXCEPT [H,L] AND [A,PSW] ARE PRESERVED
; NOTE: [H,L] WILL BE A DELTA BETWEEN GXPOS AND [B,C] - ADD 1 FOR AN X "COUNT"
; Callers: gfx_draw_filled
;          DOGRPH
;          GPUTG 
get_x_delta:  
    ld      hl,(GXPOS)        ; GET ACCUMULATOR POSITION
    or      a
    sbc     hl,bc             ; DO SUBTRACT INTO [H,L]
negate_hl_c:
    ret     nc              

; get_y_delta SETS [H,L]=ABS(GYPOS-[D,E]) AND SETS CARRY IF [D,E].GT.GYPOS
; ALL REGISTERS EXCEPT [H,L] AND [A,PSW] ARE PRESERVED
; Callers: GPUTG
get_y_delta:  
    ld      hl,(GYPOS)
    or      a
    sbc     hl,de
    jr      negate_hl_c

;-----------------------------------------------------------------------------
; Negate DE
; Callers: CPLOT8
;          CPLFIN
;        _draw_up
;      _draw_left
;   _draw_up_left
;  _draw_up_right
; _draw_down_left
;          DOMOVR
;          GOSCAL
;          VALSCN
; Returns DE = -DE
; Clobbered: A
;-----------------------------------------------------------------------------
negate_de:
    xor     a                 ;;[DE] = -[DE]
    sub     e
    ld      e,a
    sbc     a,d
    sub     e
    ld      d,a
    ret

;-----------------------------------------------------------------------------
; Negate HL
; Callers: LINCN3
;          CPLOT8
;          ANGEVN
;          NEGDE  
; Returns HL = -HL
; Clobbered: A
;-----------------------------------------------------------------------------
negate_hl:  
        xor       a             ;
        sub       l
        ld        l,a
        sbc       a,h
        sub       l
        ld        h,a
        scf
        ret

; Get Bit Mask for Bit Position PINDEX
get_bit_mask:  ld      de,BITTAB       
         ld      a,(PINDEX)
         add     a,e
         ld      e,a
         jr      nc,GETMSS
         inc     d
GETMSS:  ld      a,(de)
         ret

; Move Pixel Cursor Down One Line   
pixel_down:  push    af                ; SAVE BIT MASK OF CURRENT "C" Address
        push    hl                ; SAVE Address    
        call    fetch_current_loc            ; GET CURRENT LOCATION
        call    calc_down             ; Calculate New Screen Address, Bit Index
        jr      store_pos_addr_pop            ; Store Current, Restore Saved and Return
;Down 1 Pixel: Calculate New Screen Address and Bit Index
calc_down:  inc     a               
        inc     a                 ; [A] = Bit Index * 2
        cp      6                 
        ccf                       
        ret     nc                
        sub     6                 ; [A] = [A] - 6 
        push    de                
        ld      de,40         
        add     hl,de             ; [HL] = curloc + 40
        pop     de                
        or      a                 ; Set Flags
        ret                       

pixel_left:  push    af                ; SAVE BIT MASK OF CURRENT "C" Address
        push    hl                ; SAVE Address    
        call    fetch_current_loc            ; GET CURRENT LOCATION
        call    pixel_left             ; Calculate New Screen Address, Bit Index
        jr      store_pos_addr_pop            ; Store Current, Restore Saved and Return

; Left 1 Pixel: Calculate New Screen Address and Bit Index
calc_left:  dec     a                 ; Decrement Bit Index
        bit     0,a               
        ret     z                 ; If Bit Index is Even
        inc     a                 ;   Add 2
        inc     a                 
        dec     hl                ;   Decremement Address
        or      a                 ;   Set Flags from Bit Index
        ret                     

pixel_right: 
    push    af                    ; SAVE BIT MASK OF CURRENT "C" Address
    push    hl                    ; SAVE Address    
    call    fetch_current_loc     ; GET CURRENT LOCATION
    call    calc_right
    jr      store_pos_addr_pop
; Calculate address
calc_right
    inc     a                     ; Increment Bit Index
    bit     0,a                   ; 
    ret     nz                    ; If Bit Index is Odd
    dec     a                     ;   Add 2
    dec     a                     ; 
    inc     hl                    ;   Incremement Address
    or      a                     ;   Set Flags from Bit Index
    ret                     


; Set Graphics Attribute (Foreground Color) 
; Callers: DRAW:DCOLR, 
set_attribute: 
    cp      16                    ; Is Color > 16
    ccf                           ; If Yes
    ret     c                     ;   Return Error
    ld      (ATRBYT),a            ; Store Color
    ret                     


; EE24       
; SET CURRENT POINT
; Sets Current Point to Foreground Color ATRBYT
; Background Color is not changed
; Current point is at Current Point Index PINDEX in 
; Semigraphics Character at Current Address CURLOC
set_current_point:   push    hl                ; Save [HL]
        push    de                ; and [DE]
        ld      hl,(CURLOC)       ; Get Current Screen Address
        ld      a,(hl)            ; Get Character at Address
        or      SGBASE            ; Verify it's in the range of              
        xor     (hl)              ; Semigraphics Characters
        jr      z,SETC2           ; If Not
        ld      (hl),SGBASE       ;   Store Base Semigraphic Character
SETC2:  call    get_bit_mask            ; Get Bit Mask for Pixel to set
        or      (hl)              ; Set it
        ld      (hl),a            ; Write Character back to Screen Matrix
SETCLR: ld      de,COLOR-SCREEN   ; Add Offset into Color Matrix to
        add     hl,de             ; Screen Address to Get Color Address
        ld      a,(ATRBYT)        ; Get Color Byte
        add     a,a               ; Now multiply by 16,
        add     a,a               ; moving it to the high nybble (Foreground Color)
        add     a,a               ; leaving 0 (Black) in thr low nybblr (Background Color)
        add     a,a                 
        ld      d,a               ; Save New Color Byte
        ld      e,(hl)            ; Get Old Color Byte
        ld      a,15              ; Override Old Background Color ($0F would keep it)
        and     e                 ; Clear Old Foreground Color
        or      d                 ; Put in the New Foreground Color
        ld      (hl),a            ; Writeback Attribute back to Color Matrix
        pop     de                
        pop     hl              
        ret



store_current_loc: 
        ld      (PINDEX),a        ; Store Point Position
        ld      (CURLOC),hl       ; and Semigraphics Character Address
        ret                     

store_pos_addr_pop: 
    ld      (PINDEX),a            ; Store Bit Position 
    ld      (CURLOC),hl           ; Store Current Point Address
    pop     hl                    ; Restore Saved Address
    pop     af                    ; Restore Saved Bit Index
    ret                           ; Return from Subroutine

    include "pset.asm"

    include "line.asm"

    end