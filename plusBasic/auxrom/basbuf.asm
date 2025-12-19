;=====================================================================================
; basbuf.asm - Routines that write to and read from page BAS_BUFFR
;=====================================================================================


;-----------------------------------------------------------------------------
; Initialize variables in page BAS_BUFFR
;-----------------------------------------------------------------------------
init_basbuf_vars:
    call    clear_mousedlt
    ret
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

write_history:
    ret     c                     ; Return if first char is digit
    ld      a,(hl)
    cp      ':'
    ret     z                     ; Return if first char is colon
    push    hl
    ex      de,hl                 ; DE = BufAdr
    ld      bc,256                ; Write entire buffer
    ld      hl,_histdesc
    call    file_append_binary
    pop     hl
    ret

get_history_len:
    ld      hl,_histdesc
    call    file_size
    jp      p,.saveit
    ld      bc,0
    ld      de,0
.saveit
    push    de
    ld      de,HSTFILLEN
    ld      a,BAS_BUFFR
    call    page_write_word_sys
    inc     de
    inc     de
    pop     bc
    ld      a,BAS_BUFFR
    jp      page_write_word_sys

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
