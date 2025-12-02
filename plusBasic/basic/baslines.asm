;=====================================================================================
; Routines to tokenize, detokenize, add, insert, and delete basic lines
;=====================================================================================

; PRINT HEX$(DEEK($384B))
; POKE DEEK($384B),"FOR I=1 TO 10"
; CALL $C2DB ARGS DEEK($384B),DEEK($384B)

;-----------------------------------------------------------------------------
; Tokenize zero terminated string
; Input: DE = Output buffer address
;        HL = Input buffer address
; Clobbers: A, BC, IX
;-----------------------------------------------------------------------------
tokenize_string:
    push    de                    ; Stack = OutAdr, RtnAdr
    push    hl                    ; Stack = InpAdr, OutAdr, RtnAdr
    xor     a                     ; Tokenize String
    ld      (DORES),a             ;
    ld      c,5                   ;
    call    tokenize              ; Call KLOOP and restore Ext ROM
    pop     hl                    ; HL = InpAdr; Stack = OutAdr, RtnAdr
    pop     de                    ; DE = OutAdr; Stack = RtnAdr
    ret

; PRINT HEX$(DEEK($384B))
; POKE DEEK($384B),"10 FOR I=1 TO 10"
; CALL $C2E9 ARGS DEEK($384B),DEEK($384B)

;-----------------------------------------------------------------------------
; Tokenize zero terminated string with line number
; Input: DE: Output buffer address
;        HL: Input buffer address
; Output: BC: Line# ($FFFF if no line number and line is not tokenized)
; Sets Flags: Zero if line is blank
;             No Carry if no line number
; Clobbers: A, IX
;-----------------------------------------------------------------------------
tokenize_line:
    ld      bc,-1                 ; Default line number to 65535
    ld      (TEMP3),bc            ; Set line number flag for s3_stuffh_ext
    dec     hl                    ; Back up for CHRGET
    rst     CHRGET                ; Get first non-space character
    ret     nc                    ; If not a digit, return no line number
    cp      0                     ; If NULL
    ret     z                     ;   Return empty, no line number
    push    de                    ; Stack = OutBuf, RtnAdr
    call    SCNLIN                ; DE = Line#, HL = InpPtr
    ex      de,hl                 ; HL = Line#, DE = InpPtr
    ex      (sp),hl               ; HL = OutBuf, Stack = Line#, RtnAdr
    ex      de,hl                 ; DE = OutBuf, HL = InpPtr
    call    tokenize_string       ; Tokenize rest of line
    pop     bc                    ; BC = Line#; Stack = RtnAdr
    sub     a,1                   ; Return Not Zero, Carry Set
    ret

; PRINT HEX$(DEEK($384B))
; POKE DEEK($384B),"10 FOR I=1 TO 10"
; CALL $C2FB ARGS DEEK($384B)


;-----------------------------------------------------------------------------
; Tokenize and add basic line
; Input: HL: Buffer address
; Output: BC: Line#
;         HL: New VARTAB
; Clobbered: A, DE
;-----------------------------------------------------------------------------
basic_add_line:
    ld      d,h                   ; OutBuf = Inbuf
    ld      e,l
    call    tokenize_line         ; BC = Line#
    ld      a,b                   
    and     c                     ; A = $FF iff BC = $FFFF          
    inc     a                     ; If No line number
    ret     z                     ;   Don't add it

;-----------------------------------------------------------------------------
; Append Tokenized Line from Program
; Input: BC: Line#
;        DE: Line Buffer
; Output: HL: New VARTAB
; Clobbered: A, DE
;-----------------------------------------------------------------------------
basic_append_line:
    ld      hl,(VARTAB)             
    dec     hl
    dec     hl
    ld      (hl),$FF                ; Fake line link
    inc     hl
    inc     hl
    ld      (hl),c                  
    inc     hl
    ld      (hl),b                  ; Write the Line Number                  
    inc     hl
.loop       
    ld      a,(de)                  ; Get byte from buffer
    inc     de
    ld      (hl),a                  ; Write to line
    inc     hl
    or      a                       ; If not terminator
    jr      nz,.loop                ;   Do next character
    ld      (hl),a
    inc     hl
    ld      (hl),a                  ; Terminate program
    inc     hl
    ld      (VARTAB),hl
    ret

; POKE DEEK($384B),"10 FOR I=1 TO 10"+CHR$(0)      
; POKE DEEK($384B),"20 PRINT I;"+CHR$(0)      
; POKE DEEK($384B),"40 NEXT"+CHR$(0)      
; CALL $C320 ARGS DEEK($384B)

_test:
    call    basic_add_line
        
;-----------------------------------------------------------------------------
; Relink BASIC program lines, starting from beginning of program
;---------------------------------  --------------------------------------------
basic_link_lines:
    ld      de,(TXTTAB)           ; Star at beginning of program
    jp      CHEAD                 ; Link lines and return

;; LOAD "/t/test.txt",ASC

skip_unpack_line:
    inc     hl
    inc     hl
    inc     hl
    inc     hl
;-----------------------------------------------------------------------------
; Unpack BASIC line into buffer
; Input: DE: Buffer address
;        HL: Pointer into Line
; Output: HL: Pointer to beginning of next line
; Clobbers: A, BC, DE, IX
;-----------------------------------------------------------------------------
unpack_line: 
    push    hl                    ; Stack = LinPtr, RtnAdr
    ld      hl,2                  ; 
    add     hl,sp                 ; 
    ld      (BUFRET),hl           ; BUFRET = RtnPtr
    call    set_outdo_buffer      ; BUFPTR = StrBuf
    pop     hl                    ; HL = LinPtr; Stack = RtnAdr
    push    hl                    ; Stack = Dummy, RtnAdr
    push    hl                    ; Stack = Dummy, RtnAdr
    jp      LISPRT                ; Detokenize Line into Buffer

; Output Character to BUF
output_to_buffer: 
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ld      hl,(BUFPTR)
    cp      13                    ; 
    jr      z,.got_cr             ; If Not Carriage Return
    ld      (hl),a                ;   Store Character at Puffer Pointer
    inc     hl                    ;   Increment Buffer Pointer
    ld      (BUFPTR),hl           
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ret                           ; Return Out of OUTDO
.got_cr
    xor     a                     ; Else
    ld      (hl),a                ;   Terminate Buffer Text
    pop     de                    ; DE = TxtPtr; Stack = RtnAdr
    ld      hl,(BUFRET)           ; HL = RtnPtr
    ld      sp,hl                 ; StkPtr = RtnPtr
    ex      de,hl                 ; HL = LinPtr
disable_buffer_output:
    ld      a,(BASYSCTL)          ; Get system control bits
    and     ~BASOUTBUF            ; Reset Output to Buffer
    ld      (BASYSCTL),a          ; and write back out
    ret                           ; Return from un

set_outdo_buffer:
    ld      (BUFPTR),de           ; 
enable_buffer_output:
    ld      a,(BASYSCTL)          ; Get system control bits
    or      BASOUTBUF             ; Set Output to Buffer
    ld      (BASYSCTL),a          ; and write back out
    ret
