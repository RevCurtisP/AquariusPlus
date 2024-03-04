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
; Convert hexadecimal string to integer
; DEC(hexstring$)
;-----------------------------------------------------------------------------
; ?DEC("FFFF")
FN_DEC:
    rst     CHRGET            ; Skip DEC
    call    PARCHK
    call    push_hl_labbck
    call    free_addr_len   ; Free string and get text 
    dec     de              ; Back up Text Pointer
    xor     a               ; Set A to 0
    inc     c               ; Bump Length for DEC C
    ex      de,hl
    jp      eval_hex_int    ; Convert the Text
    
;-----------------------------------------------------------------------------
; GET functions stub
;-----------------------------------------------------------------------------
FN_GET:
    rst     CHRGET                ; Skip GET Token
    cp      TILETK
    jp      z,FN_GETTILE
    rst     SYNCHR
    byte    XTOKEN                ; Check Extended Tokens
    cp      SPRITK
    jp      z,FN_GETSPRITE
    cp      PALETK                
    jp      z,FN_GETPALETTE
    cp      CHRTK
    jp      z,FN_GETCHR
    cp      ATTRTK
    jp      z,FN_GETATTR
    cp      KEYTK
    jr      z,FN_GETKEY
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

;----------------------------------------------------------------------------
; Search for string in string array
; INDEX(*array,string$)
; ToDo: INSTR(*array,string${,start})
;----------------------------------------------------------------------------
; A$(0)="foo":A$(9)="bar"
; ? INDEX(*A$,"foo")
; ? INDEX(*A$,"bar")
; ? INDEX(*A$,"baz")
FN_INDEX:
    rst     CHRGET                ; Skip DEX
    SYNCHK  '('                   ; Require (
    call    get_star_array        ; DE = AryAdr, BC = AryLen
    call    CHKSTR                ; Error if nor string
    srl     b
    rr      c
    srl     b
    rr      c                     ; ArySiz = AryLen / 4
    push    de                    ; Stack = AryAdr, RtnAdr
    push    bc                    ; Stack = ArySiz, AryAdr, RtnAdr
    SYNCHK  ','                   ; Require ,
    call    FRMEVL                ; Evaluate search arg
    SYNCHK  ')'                   ; Require (
    push    hl                    ; Stack = TxtPtr, ArySiz, AryAdr, RtnAdr
    call    free_addr_len         ; DE = StrAdr, A = StrLen
    pop     hl                    ; HL = TxtPtr; Stack = ArySiz, AryAdr, RtnAdr
    pop     bc                    ; BC = ArySiz; Stack = AryAdr, RtnAdr
    ex      (sp),hl               ; HL = AryAdr; Stack = TxtPtr, RtnAdr
    ex      de,hl                 ; HL = StrAdr, DE = AryAdr
    ld      iy,string_search_array
    call    aux_call
    ld      de,LABBCK
    push    de                    ; Stack = LABBCK, TxtPtr, RtnAdr 
    ld      a,b                   ; 
    jp      GIVINT                ; Float AB signed and return

;-----------------------------------------------------------------------------
; LOOP statements stub
;-----------------------------------------------------------------------------
ST_LOOP:
    rst     CHRGET                ; Skip LOOP token
    rst     SYNCHR
    byte    XTOKEN
    cp      PT3TK                 ;   If token is PT3
    jp      z,ST_LOOP_PT3         ;     Do PAUSE PT3
    jp      SNERR

;-----------------------------------------------------------------------------
; MOUSEB - Returns mouse buttons
; MOUSEX - Returns mouse x-position
; MOUSEY - Returns mouse y-position
;-----------------------------------------------------------------------------
FN_MOUSE:
    rst     CHRGET                ; Skip MOUSE token
    jr      z,.snerr              ;   Error
    push    af                    ; Stack = SfxChr, RetAdr
    rst     CHRGET                ; Skip Character after MOUSE
    ex      (sp),hl               ; HL = SfxChr, Stack = TxtPtr, RetAdr
    ld      bc,LABBCK
    push    bc                    ; Stack = LABBCK, TxtPtr, RetAdr
    push    hl                    ; Stack = SfxChr, LABBCK, TxtPtr, RetAdr
    call    esp_get_mouse         ; BC = xpos, D = buttons, E = ypos, L = wheel
    jr      nz,.not_found
    ld      a,(MOUSEWDLT)
    add     l                     ; Accumulate mouse wheel delta
    ld      (MOUSEWDLT),a
    pop     af                    ; AF = SfxChr; Stack = LABBCK, TxtPtr, RetAdr
    cp      'X'
    jr      z,.xpos
    cp      'Y'
    jr      z,.ypos
    cp      'B'
    jr      z,.buttons
    cp      'W'
    jr      z,.wheel
.snerr:
    jp      SNERR
.buttons
    ld      a,d
    byte    $06                   ; LD B, over next instruction
.ypos:
    ld      a,e
    jp      SNGFLT
.not_found:
    pop     af                    ; Stack = LABBCK, TxtPtr, RetAdr
    ld      a,-1                  ; Return Not found
    jr      .signed_byte
.xpos:
    jp      FLOAT_BC
.wheel
    ld      a,(MOUSEWDLT)
    ex      af,af'
    xor     a
    ld      (MOUSEWDLT),a
    ex      af,af'
.signed_byte
    jp      float_signed_byte

;-----------------------------------------------------------------------------
; INKEY - Return ASCII code of currently pressed key
;-----------------------------------------------------------------------------
FN_INKEY:
    rst     CHRGET                ; Skip KEY Token
    push    hl                    ; Do the function stuff
    ld  bc,LABBCK
    push    bc
    call    CHARCG                ; Get Keypress
    jp      SNGFLT                ; and Float it

;-----------------------------------------------------------------------------
; KEY - Check 
; KEY(keycode) - Check if key is pressed
;-----------------------------------------------------------------------------
; RUN progs/keymove.bas
FN_KEY:
    rst     CHRGET                ; Skip KEY Token
    call    PARCHK                ; Evaluate argument
    push    hl                    ; Do the function stuff
    ld      bc,LABBCK
    push    bc
    call    GETYPE                ; Get argument type
    jr      z,.string             ; If numeric
    call    CONINT                ;   A = Matrix code
    call    key_pressed           ;   A = -1 if pressed
    jp      c,FCERR               ;   Error if invalid keycode
    jp      float_signed_byte     ;   Return result
.string
    call    free_addr_len         ; DE = Address, BC - StrLen
    jp      z,FCERR               ; Error if NULL string
    ld      b,c
.loop
    ld      a,(de)
    push    bc
    push    de
    call    keycode
    ld      a,c
    call    key_pressed
    pop     de
    pop     bc
    jp      nz,float_signed_byte
    inc     de
    djnz    .loop
    xor     a
    jp      float_signed_byte

; Returns C = Keycode
keycode:
    ld      hl,keytable
    ld      b,keytablen
    ld      c,0
.loop
    cp      (hl)
    ret     z
    inc     hl
    inc     c
    djnz    .loop
    jp      FCERR

keytable:
    byte    '=',$08,':',$0D,';','.',$9D,$7F
    byte    '-','/','0','p','l',',',$8F,$8E
    byte    '9','o','k','m','n','j',$9E,$9F
    byte    '8','i','7','u','h','b',$9B,$9A
    byte    '6','y','g','v','c','f',$8A,$8B
    byte    '5','t','4','r','d','x',$89,$88
    byte    '3','e','s','z',' ','a',$9C,$8C
    byte    '2','w','1','q', 0 , 0 , 0 , 0 
keytablen   equ   $-keytable

;-----------------------------------------------------------------------------
; STASH Statement Stub
;-----------------------------------------------------------------------------
ST_STASH:
    rst     CHRGET                ; Skip STASH
    cp      SCRNTK                
    jp      z,ST_STASH_SCREEN
    jp      SNERR

;-----------------------------------------------------------------------------
; SWAP Statement Stub
;-----------------------------------------------------------------------------
ST_SWAP:
    cp      SCRNTK                
    jp      z,ST_SWAP_SCREEN
    rst     SYNCHR
    byte    XTOKEN
    cp      VARTK
    jp      z,ST_SWAP_VARS
    jp      SNERR

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
    push    de                    ; Stack = Addr1, RetAdr
    push    af                    ; Stack = PgFlg1, Addr1, RetAdr
    call    get_comma_page_addr   ; AF = PgFlg2, DE = Addr2
    push    af                    ; Stack = PgFlg2, PgFlg1, Addr1, RetAdr
    push    de                    ; Stack = Addr2, PgFlg2, PgFlg1, Addr1, RetAdr
    call    get_comma_int         ; DE = Length
    call    close_paren
    ld      b,d
    ld      c,e                   ; BC = Length
    pop     de                    ; DE = Addr2 ; Stack = PgFlg2, PgFlg1, Addr1, RetAdr
    pop     af                    ; AF = PgFlg2; Stack = PgFlg1, Addr1, RetAdr
    ex      af,af'                ; AF' = PgFlg2
    pop     af                    ; AF = PgFlg1; Stack = Addr1, RetAdr
    ex      (sp),hl               ; HL = Addr1 ; Stack = TxtPtr, RetAdr
    push    hl                    ; Stack = Addr1, TxtPtr, RetAdr
    ld      hl,LABBCK             
    ex      (sp),HL               ; HL = Addr1; Stack = LABBCK, TxtPtr, RetAdr
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
    push    de                    ; Stack = Addr1, RetAdr
    push    bc                    ; Stack = Len1, Addr1, RetAdr
    call    get_comma             ; MO Error if no Comma
    call    get_star_array        ; DE = Addr2, BC = Len2
    call    close_paren           ; Require ')', TOERR if ','
    ex      (sp),hl               ; HL = Len1; Stack = TxtPtr, Addr1, RetAdr
    sbc     hl,bc                 ; If Len1 - Len2 <> 0
    jr      nz,.badlen            ;   Return 0 (not equal)
    pop     hl                    ; HL = TxtPtr; Stack = Addr1, RetAdr
    ex      (sp),hl               ; HL = Addr1, Stack = TxtPtr, RetAdr
    call    sys_mem_compare       ;      Compare Paged to Memory`
    ld      hl,LABBCK             
    push    hl                    ; Stack = LABBCK, TxtPtr, RetAdr
    jp      float_signed_int      ; Return result
.badlen
    pop     hl                    ; HL = TxtPtr; Stack = Addr1, RetAdr
    pop     de                    ; Stack = RetAdr
    push    hl                    ; Stack = TxtPtr, RetAdr
    ld      hl,LABBCK             
    push    hl                    ; Stack = LABBCK, TxtPtr, RetAdr
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
    ld      bc,-1
    ld      (TEMP3),bc
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
; TRIM functions stub
;-----------------------------------------------------------------------------
; ? TRIML$("   123   ");"<"
; ? TRIMR$("   123   ");"<"
; ? TRIM$("   123   ");"<"
; ? TRIM$(\" \t1 2 3   \r\n")
; ? TRIML$("@@@123@@@","@")
; ? TRIMR$("@@@123@@@","@")
; ? TRIM$("@@@123@@@","@")
; ? TRIML$("*#*#123#*#","#*")
; ? TRIMR$("#*#123*#*","#*")
; ? TRIM$("#*#123#*#","*#")
; ? TRIML$("      ");"<"
; ? TRIMR$("      ");"<"
; ? TRIM$("      ");"<"
; ? TRIML$("@@@@@@","@");"<"
; ? TRIMR$("@@@@@@","@");"<"
; ? TRIM$("@@@@@@","@");"<"
FN_TRIM:
    rst     CHRGET            ; Skip TRIM
    jp      m,.token
    ld      iy,string_trim
    cp      '$'
    jr      z,trim_stringd
    ld      iy,string_trim_left
    cp      'L'
    jr      z,trim_string
    ld      iy,string_trim_right
    SYNCHK  'R'
    jr      trim_stringd
.token
    cp      XTOKEN            ; If XTOKEN
    jr      z,.trimext        ;   Parse extended token
    cp      DIRTK             ; If DIR
    jp      z,FN_TRIMDIR      ;   Do TRIMDIR
    jr      .snerr
.trimext:
    rst     CHRGET            ; Skip XTOKEN
    cp      EXTTK             ; If EXT
    jp      z,FN_TRIMEXT      ;   Do TRIMEXT$
.snerr
    jp      SNERR

trim_stringd:
    dec     hl                    ; Back up to before $
trim_string:
    push    iy                    ; Stack = TrmRtn, RetAdr
    rst     CHRGET                ; Skip L/R
    SYNCHK  '$'                   ; 
    call    FRMPRN                ; Evaluate argument after (
    call    CHKSTR                ; Error if not string
    ld      de,(FACLO)            ; DE = ArgDsc
    ld      bc,null_desc          ; BC = TrmDsc 
    ld      a,(hl)                ; A = Next character
    cp      ','                  
    jr      nz,.notcomma          ; If comma
    push    de                    ;   Stack = ArgDsc, TrmRtn, RetAdr
    rst     CHRGET                ;   Skip comma
    call    FRMEVL                ;   Evaluate argument
    call    CHKSTR                ;   Error if not string
    ld      bc,(FACLO)            ;   BC = TrmDsc
    pop     de                    ;   DE = ArgDsc; Stack = TrmRtn, RetAdr
.notcomma
    pop     iy                    ; IY = TrmRtn; Stack = RetAdr
    SYNCHK  ')'                   ; Require )
    push    hl                    ; Stack = TxtPtr, RetAdr
    push    de                    ; Stack = ArgDsc, TxtPtr, ret
    ld      d,b
    ld      e,c                   ; DE = TrmDsc
    call    FRETMP                ; HL = TrmDsc
    call    string_addr_len       ; DE = TrmAdr, A = TrmLen
    pop     hl                    ; HL = ArgDsc; Stack = TxtPtr, RetAdr
    push    af                    ; Stack = TrmLen, TxtPtr, RetAdr
    push    de                    ; Stack = TrmAdr, TrmLen, TxtPtr, RetAdr
    call    FRETM2                ; HL = ArgDsc
    call    string_addr_len       ; DE = ArgAdr, C = ArgLen
    ex      de,hl                 ; HL = ArgAdr
    pop     de                    ; DE = TrmAdr; Stack = TrmLen, TxtPtr, RetAdr
    pop     af                    ; A = TrmLen; Stack = TxtPtr, RetAdr
    ld      b,a                   ; B = TrmLen
    call    aux_call              ; DE = TrmAdr, C = TrmLen
    or      a                     ; Set flags
    jr      nz,.ret_str
    ld      hl,REDDY-1
.ret_str
    jp      STRNEX                ; Create temporary and return it

;-----------------------------------------------------------------------------
; FILL Statement stub
; FILL [@page], startaddr, oount, byte
; FILL [@page], startaddr, count, WORD word
; ToDo:
; FILL @page, byte
; FILL @page, WORD word
;-----------------------------------------------------------------------------
ST_FILL:
    rst     CHRGET                ; Skip FILL
    cp      SCRNTK
    jp      z,ST_FILL_SCREEN
    cp      TILETK
    jp      z,ST_FILL_TILE
    ;Fill memory
    call    get_page_addr         ; Check for Page Arg
    push    de                    ; Stack = BgnAdr, RetAdr
    push    af                    ; Stack = PgFlag, BgnAdr, RetAdr
    call    get_comma_int         ; DE = Count
    push    de                    ; Stack = Count, PgFlag, BgnAdr, RetAdr
    call    get_comma             ; MOERR if no comma
    cp      XTOKEN
    push    af                    ; Stack = WrdFlg, Count, PgFlag, BgnAdr, RetAdr
    jr      nz,.not_xt            ; If extended token
    rst     CHRGET
    rst     SYNCHR
    byte    WORDTK                 ; Require WORD 
.not_xt
    call    z,CHRGTR              ; Skip WORD
    call    GETINT                ; DE = FilVal
    pop     af                    ; A = WrdFlg; Stack = Count, PgFlag, BgnAdr, RetAdr
    jr      z,.isword             ; If not FILL...WORD
    push    af                    ;   Stack = WrdFlg, Count, PgFlag, BgnAdr, RetAdr
    ld      a,d
    or      a                     ;   If FilVal > 255
    jp      nz,FCERR              ;     Blow Up
    pop     af                    ;   A = WrdFlg; Stack = Count, PgFlag, BgnAdr, RetAdr
.isword
    ex      af,af'                ; AF' = WrdFlg
    pop     bc                    ; BC = Count; Stack = PgFlag, BgnAdr, RetAdr
    pop     af                    ; AF = PgFlag; Stack = BgnAdr, RetAdr
    ex      (sp),hl               ; HL = BgnAdr; Stack = TxtPtr, RetAdr
    push    hl                    ; Stack = BgnAdr, TxtPtr, RetAdr
    ld      hl,POPHRT             ; After fill, POP HL and RET
    ex      (sp),hl               ; Stack = POPHRT, TxtPtr, RetAdr
    jr      c,.fill_page          ; If not paged
    ex      af,af'                ;   AF = WrdFlg
    jp      z,sys_fill_word       ;   If WORD, fill memory with int
    ld      a,e                   ;   Else
    jp      sys_fill_mem          ;     Fill memory with byte
.fill_page
    ex      de,hl                 ; DE = BgnAdr, HL = FilVal
    ex      af,af'                ; AF = WrdFlg
    ld      ix,page_fill_word     ; If WORD
    jr      z,.go_fill            ;   Call page_fill_word
    ld      ix,page_fill_byte     ; Else call page_fill_byte
.go_fill
    ex      af,af'                ; AF = Page
    jp      (ix)                  ; Do page fill, pop HL, and return
    
;----------------------------------------------------------------------------
; GET Statement stub
;----------------------------------------------------------------------------
ST_GET:
    cp      SCRNTK                 
    jp      z,ST_GET_SCREEN
    cp      TILETK                
    jp      z,ST_GET_TILEMAP      
    rst     SYNCHR
    byte    XTOKEN
    cp      ARGSTK
    jp      z,ST_GETARGS
    jp      SNERR

;----------------------------------------------------------------------------
; LINE Statement stub
;----------------------------------------------------------------------------
ST_LINE:
    cp      INPUTK
    jp      z,ST_LINE_INPUT
    jp      SNERR

;----------------------------------------------------------------------------
; PUT Statement stub
;----------------------------------------------------------------------------
ST_PUT:
    cp      SCRNTK
    jp      z,ST_PUT_SCREEN
    cp      TILETK                ; If GET TILEMAP
    jp      z,ST_PUT_TILEMAP      ;   Go do it
    jp      SNERR

;-----------------------------------------------------------------------------
; SET Statement stub
;-----------------------------------------------------------------------------
ST_SET:
    cp      TILETK                ; $F0
    jp      z,ST_SET_TILE
    cp      COLTK                 ; $F5
    jp      z,ST_SETCOLOR
    cp      FNTK                  ; $A2
    jr      z,ST_SET_FNKEY
    cp      SAVETK                ; $DC
    jp      z,ST_SET_SAVE
    rst     SYNCHR                ; Must be extended Token
    byte    XTOKEN                ; $FE
    cp      SPRITK                ; $84
    jp      z,ST_SET_SPRITE
    cp      PALETK                ; $81
    jp      z,ST_SETPALETTE
    cp      FASTK                 ; $88
    jr      z,ST_SET_FAST
    cp      KEYTK                 ; $86
    jr      z,ST_SET_KEY
    cp      PT3TK                 ; $8C
    jp      z,ST_SET_PT3
    jp      SNERR

;-----------------------------------------------------------------------------
; Set keybuffer mode
; Syntax: SET KEY mode
;-----------------------------------------------------------------------------
ST_SET_KEY:
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
ST_SET_FAST:
    call    get_on_off            ; A = $FF if ON, $00 if OFF
    jp      sys_turbo_mode

;-----------------------------------------------------------------------------
; Write string to FNKEY buffer
; Syntax: SET FNKEY TO string$
;-----------------------------------------------------------------------------
; SET FNKEY 3 TO "CD"+CHR$(13)
ST_SET_FNKEY:
    rst     CHRGET                ; Skip FN
    rst     SYNCHR
    byte    XTOKEN                ; Require KEY
    rst     SYNCHR
    byte    KEYTK
    call    GETBYT                ; Get Function Key Number
    dec     a                     ; Make it 0 to 15
    cp      16                    ; If > 15
    jp      nc,FCERR              ;   Illegal quantity
    push    af                    ; Stack = FnkNum, RetAdr
    rst     SYNCHR                
    byte    TOTK                  ; Require TO
    call    get_string_arg        ; BC = StrLen, DE = StrAdr; Stack = TxtPtr, FnkNum, RetAdr
    ld      a,c
    cp      32                    ; If longer than 31
    jp      nc,ERRLS              ;   String too long error
    pop     hl                    ; HL = TxtPtr; Stack = FnkNum, RetAdr
    ex      (sp),hl               ; H = FnkNum; Stack = TxtPtr, RetAdr
    ld      a,h                   ; A = FnkNum
    call    fnkey_write_buffer    ; Write to the buffer
    pop     hl                    ; HL = TxtPtr; Stack = RetAdr
    ret


;-----------------------------------------------------------------------------
; PAUSE Statement 
;-----------------------------------------------------------------------------
ST_PAUSE:
    jp      z,.tryin              ; If no argument, wait for key and return
    jp      p,.nottoken           ; If followed by token
    rst     SYNCHR
    byte    XTOKEN
    cp      PT3TK                 ;   If token is PT3
    jp      z,ST_PAUSE_PT3        ;     Do PAUSE PT3
    jr      .snerr                ;   Else Syntax Error
.nottoken
    call    FRMEVL                ; Get Operand
    push    hl                    ; Stack = TxtPtr, RetAdr
    call    GETYPE                ; 
    jp      z,.string             ; If Numeric
.snerr
    jp      SNERR                 ;   Syntax error
.string
    call    STRPRT
    pop     hl                    ; HL = TxtPtr; Stack = RetAdr
.tryin
    jp      TRYIN                 ; Wait for key and return
    

;-----------------------------------------------------------------------------
; RESET Statement stub
;-----------------------------------------------------------------------------
ST_RESET:
    rst     CHRGET                ; Skip RESET
    cp      MULTK
    jr      z,_reset_array
    cp      SCRNTK                
    jp      z,ST_RESET_SCREEN
    rst     SYNCHR
    byte    XTOKEN
    cp      PALETK
    jp      z,ST_RESET_PALETTE
    jp      SNERR

;-----------------------------------------------------------------------------
; Set all array elements to 0 or empty string
; RESET *array
;-----------------------------------------------------------------------------
_reset_array
    call    get_array_argument    ; DE = AryAdr, BC = AryLen
    push    hl                    ; Stack = TxtPtr, RetAdr
    call    GETYPE                ; A = 0 and Z set if string
    push    af                    ; Stack = TypFlg, TxtPtr, RetAdr
    ex      de,hl                 ; HL = AryAdr
    call    sys_fill_zero         ; Fill array data with zeros
    pop     af                    ; A = TypFlg
    ld      bc,POPHRT
    push    bc                    ; Return to POPHRT
    ret     nz                    ; If numeric, pop TxtPtr and return
    jp      GARBA2                ; Else collect garbage, pop TxtPtr and return
    
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
; x = 0 for System, 1 for plusBASIC, 2 for S3 BASIC
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
    call    free_addr_len         ; BC = StrLen, DE = StrAdr, HL = StrDsc
    ex      de,hl                 ; HL = StrAdr
    jr      .return_ver
.getver    
    call    CONINT                ; Convert argument to byte
    ld      ix,esp_get_version
    or      a
    jr      z,.zero
    ld      ix,sys_ver_plusbasic
    cp      1
    jr      z,.zero
    ld      ix,sys_ver_s3basic
.zero
    ex      (sp),hl               ; HL = StrFlg, Stack = TxtPtr, RetAdr
    push    hl                    ; Stack = StrFlg, TxtPtr, RetAdr
    ld      hl,FBUFFR
    ld      bc,14
    call    jump_ix               ; Get version string
.return_ver
    pop     af                    ; F = StrFlg, 
    ld      bc,LABBCK
    push    bc                    ; Stack = LABBCK, TxtPtr, RetAdr
    jp      z,TIMSTR              ; If VER$(), return version string
    call    sys_num_ver           ; Else convert to integer
    jp      FLOAT_CDE             ; and return it
