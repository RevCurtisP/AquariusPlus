;=====================================================================================
; S3BASIC Enhancement Hooks 
; These routines are located in the 8K ROM in ROM Bank 2
; They called from S3BASIC and are routed through a jump table at $2000
;=====================================================================================

;-----------------------------------------------------------------------------
; Hook 2 - READY (Enter Direct Mode
;-----------------------------------------------------------------------------
s3direct_mode:
    call    s3text_screen

    ld      a,(BASYSCTL)
    and     KB_REPEAT
    or      KB_ENABLE | KB_ASCII
    call    key_set_keymode       ; Turn on keybuffer, ASCII mode, no repeat
    jp      page_restore_bank3
    call    close_bas_fdesc       ; Clean up after aborted file commnds
    jp      page_restore_bank3

close_bas_fdesc:
    ld      a,(BAS_FDESC)         ; Get File Descriptor in Use
    or      a                     ; If Valid Descriptor
    ret     m
    call    dos_close_file        ;   Close the File
init_bas_fdesc:
    ld      a,128
    ld      (BAS_FDESC),a         ;   Set to No File
    ret

s3text_screen:
    in      a,(IO_VCTRL)
    and     a,~VCTRL_MODE_MC
    or      a,VCTRL_TEXT_EN
    out     (IO_VCTRL),a
    ret

;-----------------------------------------------------------------------------
; Save MAIN Line Number Flag
;-----------------------------------------------------------------------------
s3_main_ext:
    pop     de                    ; Pop Return Address
    pop     bc                    ; C = Line Number Flag
    ld      (TEMP3),bc            ; Save it
    push    bc                    ; Flag back on stack
    push    de                      ; Return address back on stack
    jp      SCNLIN                ; Continue to SCNLIN

;-----------------------------------------------------------------------------
; Extended line editor function keys
; Jumped to from INLNC
; On entry: A,C = character typed, B = input buffer character count
;-----------------------------------------------------------------------------
s3_ctrl_keys:
    push    bc                    ; Save character count
    call    in_direct
    jr      c,.dontscreen         ; If Not in Direct Mode
    ld      a,c
    or      a
    jp      m,.extended
    pop     bc
    push    bc
    dec     b
    jr      nz,.dontscreen        ; and Input Buffer is empty
    ld      a,c
    ld      b,1
    cp      'T'-64
    jr      z,.switch_screen
    inc     b
    cp      'Y'-64
    jr      z,.switch_screen
    inc     b
    cp      'W'-64
    jr      z,.switch_screen
.dontscreen
    ld      a,c                   ; Get typed character
    sub     a,'K'-64              ;
    jr      c,.notrub             ;
    cp      'M'-'K'               ;
    jr      z,.notrub             ;
    jr      nc,.notrepeat         ; If ^K or ^L
    dec     a                     ;   ^K = $FF, ^L = 0
    and     KB_REPEAT             ;   ^K = Repeat on, ^L = off
    ld      b,a                   ;   Save it
    ld      a,(BASYSCTL)          ;   Get current Flags
    and     $FF-KB_REPEAT         ;   Mask out Repeat Bit
    or      b                     ;   OR new value back in
    ld      (BASYSCTL),a          ;   And write it back out
    ld      a,KB_ENABLE | KB_ASCII
    or      b                     ;
    call    key_set_keymode       ;   Now set new keybuffer mode
    jr      .inlinc               ;   Wait for next key
.notrepeat:
    cp      'Q'-'K'               ; If not ^N through ^P
    jr      c,.charset
.notrub
    pop     bc                    ;   Restore character count
    jp      NOTRUB                ;   Continue standard Ctrl-key check
.charset
    sub     a,'N'-'K'             ; ^N = 0, ^O = 1, ^P = 2
    xor     1                     ; ^O = 1, ^O = 0, ^P = 2
    push    hl                    ;
    call    select_chrset         ; Select the character set
    pop     hl
.inlinc
    pop     bc                    ;   Restore character count
    jp      INLINC                ;   Wait for next keyhl
.extended
    cp      $A0                   ; If international character
    jr      nc,.notrub            ;   use it
    call    fnkey_get_buff_addr   ; DE = Key Buffer Address
    jr      nz,.inlinc            ; If Function Key
    dec     de                    ;   Back up for autotype
    ld      (RESPTR),de           ;   Set pointer to buffer
    jr      .inlinc               ;   and return
.switch_screen
    ld      a,b                   ; Set screen number
    push    de
    push    hl
    call    screen_switch
    ld      hl,(CURRAM)
    ld      (hl),$7F
    pop     hl
    pop     de
    jr      .inlinc

;-----------------------------------------------------------------------------
; Don't tokenize unquoted literal string after DOS command in direct mode
;-----------------------------------------------------------------------------
s3_stuffh_ext:
    cp      DATATK-':'            ; If DATA
    jp      z,COLIS               ;   Continue STUFFH
    ex      af,af'
    ld      a,(TEMP3)             ; Get Line# Flag
    and     $01                   ; If carry set
    jr      nz,.exaf_nodatt       ;   Continue  STUFFH
    ex      af,af'
    cp      DIRTK-':'             ; If Not DIRTK through CDTK

    jp      c,NODATT              ;
    cp      CDTK-':'+1            ;
    jp      nc,NODATT             ;  Continue STUFFH
.space_loop
    ld      a,(hl)                ; Eat Spaces
    cp      ' '
    jr      nz,.not_space
    call    _stuff_chr
    jr      .space_loop
.not_space
    ld      b,a                   ; Set up delimiter for STRNG
    cp      '"'                   ; If quotes
    jp      z,STRNG               ;   Go stuff quoted string
.string_loop
    ld      a,(hl)                ; Get character
    or      a                     ; If end of line
    jp      z,CRDONE              ;   Finish it up
    cp      ' '                   ; If space
    jp      z,KLOOP               ;   Stuff it and continue
    cp      ':'                   ; If colon
    jp      z,KLOOP               ;   Stuff it and continue
    call    _stuff_chr                ; Else Stiff it
    jr      .string_loop          ;   and check next character
.exaf_nodatt:
    ex      af,af'
    jp      NODATT

; If label at beginning of line: don't tokenize, just stuff it                                  
stuff_label: 
    cp      '_'                   ; If not a undersoore                   
    jp      nz,STRNGR             ;   Keep on truckin'                    
    call    _stuff_chr            ; Stuff character in KRUNCH buffer      
.loop                                                                    
    ld      a,(hl)                ; Get character from buf                
    or      a                   ; If end of line                      
    jp      z,CRDONE            ;   Finish it up                      
    cp      ' '                 ; If Space                            
    jr      z,.stuff_it         ;   Stuff it and keep going         
    cp      ':'                 ; If colon                            
    jp      z,STRNGR            ;   Stuff it and return               
.stuff_it:                                                               
    call    _stuff_upper          ; Stuff character in KRUNCH buffer      
    jr      .loop                 ; Next character                         

; Uppercase and stuff character  
_stuff_upper: call    uppercase_char                                        
                                                                            
; Stuff char in KRUNCH buffer                                              
_stuff_chr:                                                                 
    inc     hl                    ; Bump BUF pointer                       
    ld      (de),a                ; Save byte in KRUNCH buffer             
    inc     de                    ; Bump KRUNCH pointer                    
    inc     c                     ; Increment buffer count                 
    ret                                                                   
                                                                            
; Skip label at begin of line                                           
skip_label: 
    ld      (CURLIN),hl           ; Save the Line #                        
    ex      de,hl                 ; DE = Line#, HL = Text Pointer          
    rst     CHRGET                ; Get first character                    
    cp      '_'                   ; If not underscore                      
    jr      nz,.gone              ;   Execute rest of line                 
.loop      
    rst     CHRGET                ; Get next charcter                      
    or      a                     ; If end of line                         
    jr      z,.gone               ;   done                                 
    cp      ':'                   ; If not a colon                         
    jr      nz,.loop              ;   Keep going                           
.gone    
    dec     hl                    ; Back up text pointer                   
    jp      GONE                  ; Execute rest of line                   

; Extended string processing during CRUNCH
s3_string_ext:
    jp      z,STRNG               ; Special Handling for Double Quote      
    cp      $27                   ;                                         
    jp      z,STRNG               ; Do the same for Single Quote           
    cp      $5C                   ; If not backslash
    jp      nz,stuff_label        ;   See if it's a label         

    call    _stuff_chr            ; Copy backslash 
    ld      a,(hl)                ; Get next character
    cp      '"'                   ; If not quotes`
    jp      nz,STRNGR             ;   Carry on
.escape_char
    call    _stuff_chr            ; Stuff quotes
.escape_loop
    ld      a,(hl)                ; Get next character
    call    _stuff_chr            ; Else stuff it
    or      a                     ; If EOL
    jp      z,STRNGR              ;   Finish CRUNCH
    cp      '"'                   ;
    jr      nz,.not_quotes        ; If quotes
    ld      a,(hl)                ;   Get next character
    jp      STRNGR                ;   and pass it on
.not_quotes
    cp      $5C                   ; If not escape
    jr      nz,.escape_loop       ;   check next character
    ld      a,(hl)                ; Else get next character
    jr      .escape_char          ;   escape it, and loop
.done
    push    hl                    ; Push text pointer
    call    get_strbuf_addr       ; HL = StrBuf
    push    hl                    ; Push Dummy Return Address
    dec     hl                    ; Back up to before string
    ld      b,0                   ; NUL is the only teminator
    call    STRLT3            
    call    FREFAC            
    ld      bc,FINBCK             
    push    bc                
    jp      STRCPY




;-----------------------------------------------------------------------------
; Get Function Key Buffer Address
; Input: A: Key ASCII Code
; Output: DE: Buffer Address
; Flags Set: Z if A = Function key
; Clobbered: A
;-----------------------------------------------------------------------------
fnkey_get_buff_addr:
    ld      e,a                                             
    and     $97                   ;            100X0XXX   
    cp      e
    ret     nz
    ld      d,FKEYBASE/512        ; 011??000                
    rla                           ; 011??000 1 00X0XXX0     
    ccf                           ; 011??000 0 00X0XXX0   
    rla                           ; 011??000 0 0X0XXX00   
    rla                           ; 011??000 0 X0XXX000   
    rla                           ; 011??000 X 0XXX0000   
    rl      d                     ; 11??000X 0 0XXX0000   
    rla                           ; 11??000X 0 XXX00000   
    ld      e,a
    xor     a
    ret

