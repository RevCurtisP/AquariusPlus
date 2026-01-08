;=====================================================================================
; basbuf.asm - Routines that write to and read from page BAS_BUFFR
;=====================================================================================


;-----------------------------------------------------------------------------
; Initialize variables in page BAS_BUFFR
;-----------------------------------------------------------------------------
init_basbuf_vars:
    call    clear_mousedlt
    ;ret
;;; ToDo: Debug this routine, then add reading of history file
    call    get_history_len
    ret

;-----------------------------------------------------------------------------
; Write string to Autokey buffer
; Input: BC: String length
;        DE: String Address
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
autokey_write_buffer:
    ex      de,hl                 ; HL = StrAdr
    ld      de,BASBUF_BASE+WRTKEYBUF-1
    ld      (RESPTR),de           ; Set autokey address pointer to buffer.
    inc     de                    ; DE = BufAdr
    call    basbuf_write_bytes    ; C = 0 when done
    jp      basbuf_write_byte

;-----------------------------------------------------------------------------
; Write string to Function Key buffer
; Input: A: Function Key (0-15)
;       BC: String length
;       DE: String Address
; Clobbered: A,BC,DE,HL
;-----------------------------------------------------------------------------
fnkey_write_buffer:
    ex      de,hl                 ; HL = String Address
    ld      d,FKEYBASE/512        ; 011??000
    and     $0F                   ; 011??000 0 0000XXXX
    rla                           ; 011??000 0 000XXXX0
    rla                           ; 011??000 0 00XXXX00
    rla                           ; 011??000 0 0XXXX000
    rla                           ; 011??000 0 XXXX0000
    rla                           ; 011??000 X XXX00000
    rl      d                     ; 11??000X 0 XXX00000
    ld      e,a                   ; DE = Buffer Address
    call    basbuf_write_bytes
    inc     de
    jp      basbuf_write_byte


read_mousewdlt:
    ld      de,MOUSEWDLT
    call    basbuf_read_byte
    ld      a,c                   ; C = WheelDelt
    ret  

clear_mousedlt:
    xor     a
write_mousewdlt:
    ld      de,MOUSEWDLT
    ld      c,a
    jp      basbuf_write_byte

;----------------------------------------------------------------------------
; Get RUN arguments count
; Output: A, BC = # of Arguments (excluding filespec)
;         DE = Byte after ArgCount in ArgsBuffer
; Flags: Z if no arguments
; Clobbered: A
;----------------------------------------------------------------------------
runarg_count:
    ld      de,RUNARGS
    call    basbuf_read_word
    ld      a,c
    ret

;----------------------------------------------------------------------------
; Get RUN argument
; Input: BC = Argument# (0 = filespec)
;        HL = Buffer Address
; Output: DE = Address of string terminator
; Flags: Carry set if illegal argument number
; Clobbered: AF,AF',BC,HL
;----------------------------------------------------------------------------
; ToDo: Parse args after filespec
runarg_get:
    push    bc                    ; Stack = ArgNum, RtnAdr
    call    runarg_count          ; A = ArgCnt, DE = ArgPtr
    pop     bc                    ; C = ArgNum; Stack = RtnAdr
    inc     bc                    ; Bump ArgNum for compare
    cp      c                     ; If ArgCnt < ArgNum
    ret     c                     ;   Return Carry Set
.get_arg
    call    basbuf_read_word      ; BC = ArgLen, DE = ArgTxt
    push    bc                    ; Stack = ArgLen, TxtPtr, RtnAdr
    ex      de,hl                 ; HL = ArgTxt, DE = BufAdt
    pop     bc                    ; BC = ArgLen; Stack = TxtPtr, RtnAdr
    call    basbuf_read_bytes     ; Copy ARG to Buffer
    xor     a
    ld      (de),a                ; Terminate string
    ret

; On entry: B = LinLen, HL = BufAdr
;           Carry Set if first character is digit
write_prevbuf:
    ret     c                     ; Return if line number
    push    hl                    ; Stack = BufPtr, RtnAdr
    ld      c,b
    ld      b,0                   ; BC = LinLen
    inc     bc                    ; Include NUL terminator
    ld      de,PRVDIRLIN          ; DE = PrvBuf
    call    basbuf_write_bytes    ; Copy Input Buffer to PrvLineBuf
    pop     hl                    ; HL = BufPtr; Stack = RtnAdr
    ret

; On entry: HL = BufAdr
write_history:
    call    CHRGT2                ; A = CurChr
    ret     z                     ; Return if terminator (blank for first character is colon)
    ret     c                     ; Return if first char is a digit (program line)
    ld      a,(BASYSCTL)
    and     BASWRTHST             ; If Write history flag not set
    ret     z                     ;   Return
    push    hl
    ex      de,hl                 ; DE = BufAdr
    ld      bc,256                ; Write entire buffer
    ld      hl,_histdesc
    call    file_append_binary
    call    p,get_history_len     ; Update history length if successful
    pop     hl
    ret

get_history_len:
    ld      hl,_histdesc
    call    file_size
    jp      p,.saveit
    ld      bc,0
    ld      de,0
.saveit
    ld      hl,HSTFILLEN
    call    _basbuff_write_long   ; Write History File Length
    ld      hl,HSTFILPOS          ; Write History File Position
_basbuff_write_long:
    ld      a,BAS_BUFFR
_write_long:
    push    bc                    ; Stack = LSW, RtnAdr
    push    de                    ; Stack = MSW, LSW, RtnAdr
    ex      de,hl                 ; DE = Address; HL = MSW
    call    _write_word           ; DE = NxtAdr
    pop     bc                    ; BC = MSW; Stack = LSW, RtnAdr
    call    _write_word           ; DE = NxtAdr
    ex      de,hl                 ; HL = NxtAdr, DE = MSW
    pop     bc                    ; BC = LSW; Stack = RtnAdr
    ret

_write_word:
    push    af                    ; Stack = Page, RtnAdr
    call    page_write_word_sys
    inc     de                    ; Bump Address
    inc     de
    pop     af                    ; A = Page; Stack = RtnAdr
    ret

;  Input: HL: BufAdr
; Output: HL: BufPtr, D = BufLen
;  Flags: M set if error
read_prev_hist_line:
    ld      (BUFADR),hl           ; Save BufAdr
    ld      hl,_histdesc          ; HL = FilDsc
    call    dos_open_read         ; A = FilChn
    ret     m                     ;   Return if Error
    push    af                    ; Stack = FilChn, RtnAdr
    ld      hl,HSTFILPOS          ; HL = History File Position
    call    _basbuff_read_long    ; BCDE = Position
    ld      a,b
    or      a
    jr      nz,.notzero           ; If D = 0
    dec     de                    ;   Decrement MSW
.notzero
    dec     b                     ; Back up 256 bytes
    call    _basbuff_write_long   ; Save new position
    pop     af                    ; A = FilChn; Stack = RtnAdr
    call    dos_seek              ; Move to Previous Line
    ret     m                     ;   Return if error
    ld      de,(BUFADR)           ; DE = BufAdr
    ld      bc,255                ; Read 256 characters
    call    esp_read_bytes        ; C = Bytes Read
    ret     m                     ;   Return if error
    ld      hl,(BUFADR)           ; HL = BufAdr
    xor     a                     ; A = 0
    ld      d,a                   ; D = 0
    ld      b,255                 ; Checking 255 characters
.loop
    or      (hl)                  ; If Terminator
    ret     z                     ;   Return Len in D
    inc     hl
    djnz    .loop                 ; If no terminator
    dec     b                     ;   Return error
    ret

_basbuff_read_long:
    ld      a,BAS_BUFFR
_read_long:
    push    hl                    ; Stack = RdAddr
    ex      de,hl                 ; DE = RdAddr; HL = MSW
    call    _read_word            ; DE = NxtAdr
    push    bc                    ; BC = MSW; Stack = LSW, RdAddr, RtnAdr
    call    _read_word            ; DE = NxtAdr
    ld      d,b
    ld      e,c                   ; DE = MSW
    pop     bc                    ; BC = LSW; Stack = RdAddr, RtnAdr
    pop     hl                    ; HL = RdAddr; Stack = RtnAdr
    ret

_read_word:
    push    af                    ; Stack = Page, RtnAdr
    call    page_read_word
    inc     de                    ; Bump Address
    inc     de
    pop     af                    ; A = Page; Stack = RtnAdr
    ret


_histname:
    db      "/_history"
_histlen = $ - _histname
_histdesc
    dw      _histlen,_histname

; On entry: HL = BufAdr
read_prevbuf:
    call    get_linbuf_hl         ; HL = BufAdr
    ld      de,BANK1_BASE+PRVDIRLIN
    ld      b,0                   ; LinLen = 0
    ld      a,BAS_BUFFR
    call    page_map_bank1        
.loop
    ld      a,(de)                ; Get PrvChr
    inc     de                    ; Bump PrvPtr
    ld      (hl),a                ; Write to Buffer
    inc     b                     ; Bump LinLen
    or      a
    ld      a,b                   ; A = LinLen
    jr      z,.done               ; If not EOL
    inc     hl                    ;   Bump BufPtr
    cp      BUFSIZ                ;   If LinLen < BufLen
    jr      c,.loop               ;      Copy next character
.done
    cp      1                     ; Set Carry if LinLen = 1
    ld      a,7                   ; Set A for OUTBEL
    jp      page_restore_bank1
