DRWTAB: db      'U'+128           ; UP
        dw      draw_up  
        db      'D'+128           ; DOWN
        dw      draw_down  
        db      'L'+128           ; LEFT
        dw      draw_left  
        db      'R'+128           ; RIGHT
        dw      draw_right  
        db      'M'               ; MOVE
        dw      DMOVE 
        db      'E'+128           ; -,-
        dw      _draw_up_right  
        db      'F'+128           ; +,-
        dw      _draw_down_right  
        db      'G'+128           ; +,+
        dw      _draw_down_left  
        db      'H'+128           ; -,+
        dw      _draw_up_left  
        db      'A'+128           ; ANGLE COMMAND
        dw      DANGLE  
        db      'B'               ; MOVE WITHOUT PLOTTING
        dw      DNOPLT  
        db      'N'               ; DON'T CHANGE CURRENT COORDS
        dw      DNOMOV  
        db      'X'               ; EXECUTE STRING
        dw      MCLXEQ  
        db      'C'+128           ; COLOR
        dw      DCOLR 
        db      'S'+128           ; SCALE
        dw      DSCALE  
        db      0                 ; END OF TABLE
  

MCLOOP: pop     af                ; GET LENGTH OFF STACK
        ld      (MCLLEN),a        
        pop     hl                ; GET ADDR
        ld      a,h               
        or      l                 ; SEE IF LAST ENTRY
        jr      z,POPHRX          ; ALL FINISHED IF ZERO
        ld      (MCLPTR),hl       ; SET UP POINTER
MCLSCN: call    FETCHR            ; GET A CHAR FROM STRING
        jr      z,MCLOOP          ; END OF STRING - SEE IF MORE ON STK
        add     a,a               ; PUT CHAR * 2 INTO [C]
        ld      c,a               
        ld      hl,(MCLTAB)       ; POINT TO COMMAND TABLE
                                  
MSCNLP: ld      a,(hl)            ; GET CHAR FROM COMMAND TABLE
        add     a,a               ; CHAR = CHAR * 2 (CLR HI BIT FOR CMP)
                                  
GOFCER: call    z,FCERR           ; END OF TABLE.
        cp      c                 ; HAVE WE GOT IT?
        jr      z,MISCMD          ; YES.
        inc     hl                ; MOVE TO NEXT ENTRY
        inc     hl                
        inc     hl                
        jr      MSCNLP            
                                  
MISCMD: ld      bc,MCLSCN         ; RETURN TO TOP OF LOOP WHEN DONE
        push    bc                
        ld      a,(hl)            ; SEE IF A VALUE NEEDED
        ld      c,a               ; PASS GOTTEN CHAR IN [C]
        add     a,a               
        jr      nc,MNOARG         ; COMMAND DOESN'T REQUIRE ARGUMENT
        or      a                 ; CLEAR CARRY
        rra                       ; MAKE IT A CHAR AGAIN
        ld      c,a               ; PUT IN [C]
        push    bc                
        push    hl                ; SAVE PTR INTO CMD TABLE
        call    FETCHR            ; GET A CHAR
        ld      de,1              ; DEFAULT ARG=1
        jr      z,VSNAR0          
        call    ISLETC            
        jr      nc,VSNARG         ; NO ARG IF END OF STRING
        call    _is_letter            ; SEE IF POSSIBLE LETTER
        scf                       
        jr      ISCMD3            
                                  
VSNARG: call    _prev_char            ; PUT CHAR BACK INTO STRING
VSNAR0: or      a                 ; CLEAR CARRY
ISCMD3: pop     hl                
        pop     bc                ; GET BACK COMMAND CHAR
MNOARG: inc     hl                ; POINT TO DISPATCH ADDR
        ld      a,(hl)            ; GET Address INTO HL
        inc     hl                
        ld      h,(hl)            
        ld      l,a               
        jp      (hl)              ; DISPATCH
      
_prev_char: push    hl                
        ld      hl,MCLLEN         ; INCREMENT LENGTH
        inc     (hl)              
        ld      hl,(MCLPTR)       ; BACK UP POINTER
        dec     hl
        ld      (MCLPTR),hl
        pop     hl
        ret

      
FETCHZ: call    FETCHR            ; GET A CHAR FROM STRING
        jr      z,GOFCER          ; GIVE ERROR IF END OF LINE
        ret                       
                                  
FETCHR: push    hl                
FETCH2: ld      hl,MCLLEN         ; POINT TO STRING LENGTH
        ld      a,(hl)            
        or      a                 
        jr      z,POPHRX          ; RETURN Z=0 IF END OF STRING
        dec     (hl)              ; UPDATE COUNT FOR NEXT TIME
        ld      hl,(MCLPTR)       ; GET PTR TO STRING
        ld      a,(hl)            ; GET CHARACTER FROM STRING
        inc     hl                ; UPDATE PTR FOR NEXT TIME
        ld      (MCLPTR),hl       
        cp      ' '               ;  SKIP SPACES
        jr      z,FETCH2          
        cp      96                ; CONVERT LOWER CASE TO UPPER
        jr      c,POPHRX          
        sub     32                ; DO CONVERSION
POPHRX: pop     hl                
        ret                       

draw_up:   call    negate_de             ; MOVE +0,-Y
draw_down: ld      bc,0              ; MOVE +0,+Y, DX=0
        jp      DOMOVR            ; TREAT AS RELATIVE MOVE
;   
draw_left: call    negate_de             ; MOVE -X,+0
draw_right: ld      b,d               ; MOVE +X,+0
        ld      c,e               ; [BC]=VALUE
        ld      de,0              ; DY=0
        jp      DOMOVR            ; TREAT AS RELATIVE MOVE
  
_draw_up_left: call    negate_de             ; MOVE -X,-Y
_draw_down_right: ld      b,d               ; MOVE +X,+Y
        ld      c,e 
        jp      DOMOVR  
  
_draw_up_right: ld      b,d               ; MOVE +X,-Y
        ld      c,e 
draw_neg_de: call    negate_de 
        jp      DOMOVR  
  
_draw_down_left: call    negate_de             ; MOVE -X,+Y
        ld      b,d 
        ld      c,e 
        jp      draw_neg_de            ; MAKE DY POSITIVE & GO
  
DMOVE:  call    FETCHZ            ; GET NEXT CHAR AFTER COMMA
        ld      b,0               ; ASSUME RELATIVE
        cp      '+'               ; IF "+" OR "-" THEN RELATIVE
        jp      z,MOVREL  
        cp      '-' 
        jp      z,MOVREL  
        inc     b                 ; NON-Z TO FLAG ABSOLUTE
MOVREL: ld      a,b 
        push    af                ; SAVE ABS/REL FLAG ON STACK
        call    _prev_char            ; BACK UP SO _scan_argument WILL SEE "-"
        call    _scan_argument            ; GET X VALUE
        push    de                ; SAVE IT
        call    FETCHZ            ; NOW CHECK FOR COMMA
        cp      ','               ; COMMA?
        jp      nz,FCERR  
        call    _scan_argument            ; GET Y VALUE IN D
        pop     bc                ; GET BACK X VALUE
        pop     af                ; GET ABS/REL FLAG
        or      a 
        jp      nz,DRWABS         ; NZ - ABSOLUTE

_scan_argument: 
    call    FETCHZ            ; GET FIRST CHAR OF ARGUMENT
_is_letter: 
    cp      '='               ; NUMERIC?
        jr      z,VARGET          ; No, Evaluate Variable
        cp      '+'               ; PLUS SIGN?
        jr      z,_scan_argument          ; THEN SKIP IT
        cp      '-'               ; NEGATIVE VALUE?
        jr      nz,VALSC2         
        ld      de,negate_de           ; IF SO, NEGATE BEFORE RETURNING
        push    de                
        jr      _scan_argument            ; EAT THE "-"
VALSC2: ld      de,0              
        ld      b,4               


DOMOVR: call    DSCLDE            ; ADJUST Y OFFSET BY SCALE
        push    de                ; SAVE Y OFFSET
        ld      d,b               ; GET X INTO [DE]
        ld      e,c 
        call    DSCLDE            ; GO SCALE IT.
        ex      de,hl             ; GET ADJUSTED X INTO [HL]
        pop     de                ; GET ADJUSTED Y INTO [DE]
        xor     a 
        ld      (CSCLXY),a  
        ld      a,(DRWANG)        ; GET ANGLE BYTE
        rra                       ; LOW BIT TO CARRY
        jr      nc,ANGEVN         ; ANGLE IS EVEN - DON'T SWAP X AND Y
        push    af                ; SAVE THIS BYTE
        push    de                ; SAVE DY
        push    hl                ; SAVE DX
        call    get_aspect_ratio            ; GO GET SCREEN ASPECT RATIO
        ld      a,h 
        or      a                 ; IS ASPECT RATIO GREATER THAN ONE?
        jr      z,ASPLS0          ; BRIF GOOD ASPECT RATIO
        ld      a,1 
        ld      (CSCLXY),a  
ASPLS0: ex      de,hl             ; GET ASPECT RATIO INTO [C] FOR GOSCAL
        ld      c,l 
        pop     hl                ; GET BACK DX
        ld      a,(CSCLXY)  
        or      a 
        jr      z,ASPLS1          ;branch if aspect ratio less 1.0
        ex      (sp),hl           ; XTHL
ASPLS1: ex      de,hl             ; [HL]=DY, save DX
        push    hl                ; SAVE 1/ASPECT
        call    GOSCAL            ; SCALE DELTA X BY ASPECT RATIO
        pop     bc                ; GET BACK 1/ASPECT RATIO
        pop     hl                ; GET DY
        push    de                ; SAVE SCALED DX
        ex      de,hl             ; DY TO [DE] FOR GOSCAL
        ld      hl,0  
DMULP:  add     hl,de             ; MULTIPLY [DE] BY HI BYTE OF 1/ASPECT
        djnz    DMULP 
        push    hl                ; SAVE PARTIAL RESULT
        call    GOSCAL            ; MULTIPLY [DE] BY LOW BYTE
        pop     hl                ; GET BACK PARTIAL RESULT
        add     hl,de             ; [HL]=Y * 1/ASPECT
        pop     de                ; GET BACK SCALED Y
        ld      a,(CSCLXY)  
        or      a 
        jr      z,ASLSS1          ; branch if aspect ratio less than 1
        ex      de,hl 
ASLSS1: call    negate_de             ; ALWAYS NEGATE NEW DY
        pop     af                ; GET BACK SHIFTED ANGLE
ANGEVN: rra                       ; TEST SECOND BIT
        jp      nc,ANGPOS         ; DON'T NEGATE COORDS IF NOT SET
        call    negate_hl 
        call    negate_de             ; NEGATE BOTH DELTAS
ANGPOS: call    GTABSC            ; GO CALC TRUE COORDINATES

DRWABS: ld      a,(DRWFLG)        ; SEE WHETHER WE PLOT OR NOT
        add     a,a               ; CHECK HI BIT
        jp      c,DSTPOS          ; JUST SET POSITION.
        push    af                ; SAVE THIS FLAG
        push    bc                ; SAVE X,Y COORDS
        push    de                ; BEFORE SCALE SO REFLECT DISTANCE OFF
        call    scale_x_y            ; SCALE IN CASE COORDS OFF SCREEN
        call    GLINE2            
        pop     de                
        pop     bc                ; GET THEM BACK
        pop     af                ; GET BACK FLAG
DSTPOS: add     a,a               ; SEE WHETHER TO STORE COORDS
        jp      c,DNSTOR          ; DON'T UPDATE IF B6=1
        ex      de,hl             
        ld      (GRPACY),hl       ; UPDATE GRAPHICS AC
        ex      de,hl             
        ld      h,b               
        ld      l,c               
        ld      (GRPACX),hl       
DNSTOR: xor     a                 ; CLEAR SPECIAL FUNCTION FLAGS
        ld      (DRWFLG),a        
        ret                       
                                  
DNOMOV: ld      a,64              ; SET BIT SIX IN FLAG BYTE
        jp      DSTFLG            
                                  
DNOPLT: ld      a,128             ; SET BIT 7
DSTFLG: ld      hl,DRWFLG         
        or      (hl)              
        ld      (hl),a            ; STORE UPDATED BYTE
        ret                       
                                  
DANGLE: jp      nc,NCFCER         ; ERROR IF NO ARG
        ld      a,e               ; MAKE SURE LESS THAN 4
        cp      4                 
        jp      nc,NCFCER         ; ERROR IF NOT
        ld      (DRWANG),a        
        ret                       
NCFCER:                           
DSCALE: jp      nc,FCERR          ; FC ERROR IF NO ARG
        ld      a,d               ; MAKE SURE LESS THAN 256
        or      a                 
        jp      nz,FCERR          
        ld      a,e               
        ld      (DRWSCL),a        ; STORE SCALE FACTOR
        ret                       
                                  
DSCLDE: ld      a,(DRWSCL)        ; GET SCALE FACTOR
        or      a                 ; ZERO MEANS NO SCALING
        ret     z                 
        ld      hl,0              
                                  
DSCLP:  add     hl,de             ; ADD IN [DE] SCALE TIMES
        dec     a                 
        jp      nz,DSCLP          
        ex      de,hl             ; PUT IT BACK IN [DE]
        ld      a,d               ; SEE IF VALUE IS NEGATIVE
        add     a,a               
        push    af                ; SAVE RESULTS OF TEST
        jp      nc,DSCPOS         
        dec     de                ; MAKE IT TRUNCATE DOWN
DSCPOS: call    HLFDE             ; DIVIDE BY FOUR
        call    HLFDE             
        pop     af                ; SEE IF WAS NEGATIVE
        ret     nc                
        ld      a,d               
        or      0C0H              
        ld      d,a               
        inc     de                
        ret                       ; ALL DONE IF WAS POSITIVE
                                  
GOSCAL: ld      a,d               ; SEE IF NEGATIVE
        add     a,a               
        jp      nc,GOSC2          ; NO, MULTIPLY AS-IS
        ld      hl,NEGD           ; NEGATE BEFORE RETURNING
        push    hl                
        call    negate_de             ; MAKE POSITIVE FOR MULTIPLY
GOSC2:  ld      a,c               ; GET SCALE FACTOR
        jp      SCALE2            ; GET SCALE FACTOR
                                  
DCOLR:  jp      nc,NCFCER         ; FC ERROR IF NO ARG
        ld      a,e               ; GO SET ATTRIBUTE
        call    set_attribute            
        jp      c,FCERR           ; ERROR IF ILLEGAL ATTRIBUTE
        ret

