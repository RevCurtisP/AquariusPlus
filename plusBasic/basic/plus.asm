;-----------------------------------------------------------------------------
; plus.asm - plusBASIC specific statements and functions
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; DATE$ - Get Current Date
; DATETIME$ - Get Current Date & Time
;-----------------------------------------------------------------------------
FN_DATE:
    rst     CHRGET                ; Skip Token
    ld      c,0                   ; Return Just Date
    cp      TIMETK                ; If followed by TIME token
    jr      nz,.notime
    rst     CHRGET                ;   Skip TIME token
    dec     c                     ;   Return Date and Time
.notime
    SYNCHK  '$'                   ; Require Dollar Sign
    push    hl
    ld      de,LABBCK
    push    de
    ld      ix,esp_get_datetime   ; Read Date and Time into buffer
    call    bas_read_to_buff      ; Set buffer address and call routine
    inc     c                     ; 0 = DATETIME$, 1=DATE$
    jr      z,.done
    ld      de,8
    add     hl,de                 ; Start of Time substring
    xor     a
    ld      (hl),a                ; Terminate Date substring
    sbc     hl,de                 ; Set HL back to Buffer address
.done
    jp      TIMSTR                ; Create and return temporary string

;-----------------------------------------------------------------------------
; GET functions stub
;-----------------------------------------------------------------------------
FN_GET:
    rst     CHRGET                ; Skip GET Token
    rst     SYNCHR
    byte    XTOKEN                ; Check Extended Tokens
    cp      KEYTK
    jr      z,FN_GETKEY
    cp      SPRITK
    jp      z,FN_GETSPRITE
    cp      PALETK                
    jp      z,FN_GETPALETTE
    jp      SNERR

;-----------------------------------------------------------------------------
; GETKEY - Wait for key an return ASCII code
; GETKEY$  - Wait for key and return as string
;-----------------------------------------------------------------------------
FN_GETKEY:
    rst     CHRGET                ; Skip KEY
    push    hl
.loop
    call    CHARCG                ; Wait for keypress
    jr      z,.loop
    pop     hl
    ld      e,a                   ; Save ASCII Code
    ld      a,(hl)                ; Get character after KEY
    cp      '$'                   ; Check for GETKEY$
    push    af                    ; Save Flags
    call    z,CHRGTR              ; If GETKEY$, skip $
    pop     af                    ; Restore Flags
    push    hl                    ; Do the function stuff
    ld      bc,LABBCK
    push    bc
    ld      a,e                   ; Get ASCII back, Flags are from CP '$'
    jp      nz,SNGFLT             ; If not GETKEY$, Float it
    pop     bc                    ; Get rid of dummy return address
    jp      BUFCIN                ; Else Return String

;-----------------------------------------------------------------------------
; MOUSEB - Returns mouse buttons
; MOUSEX - Returns mouse x-position
; MOUSEY - Returns mouse y-position
;-----------------------------------------------------------------------------
FN_MOUSE:
    rst     CHRGET                ; Skip MOUSE token
    jr      z,.snerr              ;   Error
    ex      af,af'
    rst     CHRGET                ; Skip Character after MOUSE
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ld      bc,LABBCK
    push    bc                    ; Stack = LABBCK, TxtPtr, RtnAdr
    call    esp_get_mouse         ; BC = xpos, D = buttons, E = ypos
    jr      nz,.not_found
    ex      af,af'
    cp      'X'
    jr      z,.xpos
    cp      'Y'
    jr      z,.ypos
    cp      'B'
    jr      z,.buttons
    cp      'W'
    jr      .wheel
.snerr:
    jp      SNERR
.buttons
    ld      a,d
    byte    $06                   ; LD B, over next instruction
.ypos:
    ld      a,e
    jp      SNGFLT
.not_found:
    ld      a,-1                  ; Return Not found
    jr      .signed_byte
.xpos:
    jp      FLOAT_BC
.wheel
    ld      a,l
.signed_byte
    jp      float_signed_byte

;-----------------------------------------------------------------------------
; INKEY - Return ASCII code of currently pressed key
;-----------------------------------------------------------------------------
FN_INKEY:
    rst     CHRGET                ; Skip KEY Token
    push    hl                    ; Do the function stuff
    ld      bc,LABBCK
    push    bc
    call    CHARCG                ; Get Keypress
    jp      SNGFLT                ; and Float it

;-----------------------------------------------------------------------------
; TIME$ - Get Current Time
;-----------------------------------------------------------------------------
FN_TIME:
    rst     CHRGET                ; Skip Token
    cp      'R'                   ; If TIMER
    jr      z,FN_TIMER            ;   Read Countdown Timer
    SYNCHK  '$'                   ; Require dollar sign
    push    hl
    ld      bc,LABBCK
    push    bc
    ld      ix,esp_get_datetime   ; Read Date and Time into buffer
    call    bas_read_to_buff      ; Set buffer address and call routine
    ld      bc,8
    add     hl,bc                 ; Start of Date String
    jp      TIMSTR

;-----------------------------------------------------------------------------
; TIMER - Get Timer Count
;-----------------------------------------------------------------------------
FN_TIMER:
    rst     CHRGET                ; Skip 'R'
    push    hl                    ; Text Pointer on Stack
    ld      hl,LABBCK
    push    hl                    ; Return to LABBCK
    call    timer_read            ; Read timer
    jp      FLOAT_CDE             ; Return as 23 bit integer

;-----------------------------------------------------------------------------
; TIMER - Set Timer Count
; Syntax: TIMER = Expression
;-----------------------------------------------------------------------------
ST_TIMER:
    SYNCHK  'R'                   ; Require 'R'
    rst     SYNCHR
    byte    EQUATK                ; Require '='
    call    GET_LONG              ; Get count into C,D,E
    jp      timer_write           ; Set the timer

;----------------------------------------------------------------------------
; Compare blocks of memory
; Syntax: COMPARE( [@page,] address, [@page,] address, length)
;----------------------------------------------------------------------------
FN_COMPARE:
    rst     CHRGET                ; Skip COMPARE
    SYNCHK  '('
    cp      MULTK                 ; If *
    jp      z,.compare_arrays     ;   Compare Arrays
    call    get_page_addr         ; AF = PgFlg1, DE = Addr1
    push    de                    ; Stack = Addr1, RtnAdr
    push    af                    ; Stack = PgFlg1, Addr1, RtnAdr
    call    get_comma_page_addr   ; AF = PgFlg2, DE = Addr2
    push    af                    ; Stack = PgFlg2, PgFlg1, Addr1, RtnAdr
    push    de                    ; Stack = Addr2, PgFlg2, PgFlg1, Addr1, RtnAdr
    call    get_comma_int         ; DE = Length
    call    close_paren
    ld      b,d
    ld      c,e                   ; BC = Length
    pop     de                    ; DE = Addr2 ; Stack = PgFlg2, PgFlg1, Addr1, RtnAdr
    pop     af                    ; AF = PgFlg2; Stack = PgFlg1, Addr1, RtnAdr
    ex      af,af'                ; AF' = PgFlg2
    pop     af                    ; AF = PgFlg1; Stack = Addr1, RtnAdr
    ex      (sp),hl               ; HL = Addr1 ; Stack = TxtPtr, RtnAdr
    push    hl                    ; Stack = Addr1, TxtPtr, RtnAdr
    ld      hl,LABBCK             
    ex      (sp),HL               ; HL = Addr1; Stack = LABBCK, TxtPtr, RtnAdr
    jr      c,.page1              ; If Addr1 not paged
    ex      af,af'                ;   AF = PgFlg2, AF' = PgFlg1
    jr      c,.mem2page           ;   If Addr2 not paged
.mem2mem
    call    sys_mem_compare       ;     Do Memory to Memory Compare
    jr      .float_it             ;     Return Result
.mem2page                         ;   Else
    ex      de,hl                 ;     AF = PgFlg2, HL = Addr2, DE = Addr1
    jr      .page_mem             ;     Compare Paged to Memory and return result
.page1                            ; Else
    ex      af,af'                ;   AF = PgFlg2, AF' = PgFlg1
    jr      c,.page2page          ;   If Addr2 not paged
    ex      af,af'                ;     AF = PgFlg1, HL = Addr1, DE = Addr2
.page_mem
    call    page_mem_compare      ;      Compare Paged to Memory`
    jr      .float_it             ;      and return result
.page2page:                       ;   Else
    call    page_page_compare     ;     GS error for now
.float_it
    jp      float_signed_int

.compare_arrays
    call    skip_star_array       ; DE = Addr1, BC = Len1
    push    de                    ; Stack = Addr1, RtnAdr
    push    bc                    ; Stack = Len1, Addr1, RtnAdr
    call    get_comma             ; MO Error if no Comma
    call    get_star_array        ; DE = Addr2, BC = Len2
    call    close_paren           ; Require ')', TOERR if ','
    ex      (sp),hl               ; HL = Len1; Stack = TxtPtr, Addr1, RtnAdr
    sbc     hl,bc                 ; If Len1 - Len2 <> 0
    jr      nz,.badlen            ;   Return 0 (not equal)
    pop     hl                    ; HL = TxtPtr; Stack = Addr1, RtnAdr
    ex      (sp),hl               ; HL = Addr1, Stack = TxtPtr, Rtn Addr
    call    sys_mem_compare       ;      Compare Paged to Memory`
    ld      hl,LABBCK             
    push    hl                    ; Stack = LABBCK, TxtPtr, RtnAdr
    jp      float_signed_int      ; Return result
.badlen
    pop     hl                    ; HL = TxtPtr; Stack = Addr1, RtnAdr
    pop     de                    ; Stack = RtnAdr
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ld      hl,LABBCK             
    push    hl                    ; Stack = LABBCK, TxtPtr, RtnAdr
    xor     a
    jp      SNGFLT                ; Return 0
    
;----------------------------------------------------------------------------
; EVAL - Evaluate string
;----------------------------------------------------------------------------
FN_EVAL:
    call    ERRDIR            ; Issue Error if in Direct Mode
    rst     CHRGET            ; Skip Token
    call    PARCHK            ; Get Argument
    push    hl                ; Save Text Pointer
    call    FRESTR            ; Free up temporary and get descriptor address
    call    string_addr_len   ; Get Argument String Length in BC, Address in HL
    ld      a,ENDBUF-BUF      ;
    cp      c
    jr      c,LSERR           ; Error if longer than 127 bytes
    ex      de,hl             ; Text address into HL
    ld      de,BUF            ;
    ldir                      ; Copy String to Buffer
    xor     a
    ld      (de),a            ; Terminate String
    ld      hl,BUF            ; Reading Line Buffer
    ld      d,h               ; Writing Line Buffers
    ld      e,l
    xor     a                 ; Tokenize String
    ld      (DORES),a         ;
    ld      c,5               ;
    call    tokenize          ; Call KLOOP, restore Extended ROM
    ld      hl,BUF            ; Point to Line Buffer
    call    FRMEVL            ; Evaluate Formula
    pop     hl                ; Restore Text Pointer
    ret

LSERR:
    ld      e,ERRLS
    jp      ERROR


;-----------------------------------------------------------------------------
; FILL Statement stub
; FILL [@page], startaddr, oount, byte
; FILL! [@page], startaddr, count, word
;-----------------------------------------------------------------------------
 ST_FILL:
    rst     CHRGET                ; Skip FILL
    cp      SCRNTK
    jp      z,ST_FILL_SCREEN
    cp      TILETK
    jp      z,ST_FILL_TILE
    jp      SNERR

;----------------------------------------------------------------------------
; GET Statement stub
;----------------------------------------------------------------------------
ST_GET:
    cp      SCRNTK
    jp      z,ST_GET_SCREEN
    rst     SYNCHR
    byte    XTOKEN
    cp      TILETK                ; If GET TILEMAP
    jp      z,ST_GET_TILEMAP      ;   Go do it
    cp      ARGSTK
    jp      z,ST_GETARGS
    jp      SNERR

;----------------------------------------------------------------------------
; PUT Statement stub
; Syntax: GET (x1,y1)-(x2,y2),*arrayvar
;----------------------------------------------------------------------------
ST_PUT:
    cp      SCRNTK
    jp      z,ST_PUT_SCREEN
    rst     SYNCHR
    byte    XTOKEN
    cp      TILETK                ; If GET TILEMAP
    jp      z,ST_PUT_TILEMAP      ;   Go do it
    jp      SNERR

;-----------------------------------------------------------------------------
; SET Statement stub
;-----------------------------------------------------------------------------
ST_SET:
    cp      TILETK
    jp      z,ST_SET_TILE
    cp      FNTK      
    jr      z,ST_SETFNKEY
    cp      COLTK      
    jp      z,ST_SETCOLOR
    rst     SYNCHR                ; Must be extended Token
    byte    XTOKEN
    cp      SPRITK
    jp      z,ST_SET_SPRITE
    cp      PALETK
    jp      z,ST_SETPALETTE
    cp      FASTK      
    jr      z,ST_SETFAST
    cp      KEYTK      
    jr      z,ST_SETKEY
    jp      SNERR

;-----------------------------------------------------------------------------
; Set keybuffer mode
; Syntax: SET KEY mode
;-----------------------------------------------------------------------------
ST_SETKEY:
    rst     CHRGET                ; Skip KEY
    call    GETBYT                ; Get key mode
    push    hl
    call    key_set_keymode       ; Set the mode
    jp      m,FCERR
    pop     hl
    ret

;-----------------------------------------------------------------------------
; Set Fast Mode
; Syntax: SET FAST ON/OFF
;-----------------------------------------------------------------------------
ST_SETFAST:
    call    get_on_off            ; A = $FF if ON, $00 if OFF
    jp      sys_turbo_mode

;-----------------------------------------------------------------------------
; Write string to FNKEY buffer
; Syntax: SET FNKEY TO string$
;-----------------------------------------------------------------------------
; SET FNKEY 3 TO "CD"+CHR$(13)
ST_SETFNKEY:
    rst     CHRGET                ; Skip FN
    rst     SYNCHR
    byte    XTOKEN                ; Require KEY
    rst     SYNCHR
    byte    KEYTK
    call    GETBYT                ; Get Function Key Number
    dec     a                     ; Make it 0 to 15
    cp      16                    ; If > 15
    jp      nc,FCERR              ;   Illegal quantity
    push    af                    ; Stack = FnkNum, RtnAdr
    rst     SYNCHR                
    byte    TOTK                  ; Require TO
    call    get_string_arg        ; BC = StrLen, DE = StrAdr; Stack = TxtPtr, FnkNum, RtnAdr
    ld      a,c
    cp      32                    ; If longer than 31
    jp      nc,ERRLS              ;   String too long error
    pop     hl                    ; HL = TxtPtr; Stack = FnkNum, RtnAdr
    ex      (sp),hl               ; H = FnkNum; Stack = TxtPtr, RtnAdr
    ld      a,h                   ; A = FnkNum
    call    fnkey_write_buffer    ; Write to the buffer
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret


;-----------------------------------------------------------------------------
; PAUSE Statement 
;-----------------------------------------------------------------------------
ST_PAUSE:
    jp      z,TRYIN               ; If no argument, wait for key and return
    jp      SNERR                 ; Else Syntax error for now

;-----------------------------------------------------------------------------
; USE Statement stub
;-----------------------------------------------------------------------------
ST_USE:
    call    SYNCHR
    byte    XTOKEN
    cp      CHRTK
    jp      z,ST_USECHR           ; USE CHRSET
    jp      SNERR

;-----------------------------------------------------------------------------
; VER Function
; Syntax: VER$(x)
; x = 0 for System, 1 for plusBASIC
;-----------------------------------------------------------------------------
FN_VER:
    inc     hl                    ; Skip VER 
    ld      a,(hl)                ; Get following character
    cp      '$'                   ; 
    push    af                    ; Stack = StrFlg, RetAdr
    jr      nz,.notstring         ; If '$'
    inc     hl                    ;   Skip it
.notstring
    SYNCHK  '('
    call    FRMEVL                ; Evaluate argument
    SYNCHK  ')'
    call    GETYPE
    jr      nz,.getver            ; If it's a string
    ex      (sp),hl               ; HL = StrFlg, Stack = TxtPtr, RetAdr
    push    hl                    ; Stack = StrFlg, TxtPtr, RetAdr
    call    free_addr_len         
    ex      de,hl                 ; HL = TxtAdr
    jr      .return_ver
.getver    
    call    CONINT                ; Convert argument to byte
    ld      ix,esp_get_version
    or      a
    jr      z,.zero
    ld      ix,sys_ver_basic
.zero
    ex      (sp),hl               ; HL = StrFlg, Stack = TxtPtr, RetAdr
    push    hl                    ; Stack = StrFlg, TxtPtr, RetAdr
    ld      hl,FBUFFR
    call    jump_ix               ; Get version string
.return_ver
    pop     af                    ; F = StrFlg, 
    ld      bc,LABBCK
    push    bc                    ; Stack = LABBCK, TxtPtr, RetAdr
    jp      z,TIMSTR              ; If VER$(), return version string
    call    sys_num_ver           ; Else convert to integer
    jp      FLOAT_CDE             ; and return it
