;====================================================================
; File I/O Machine Language Routines
;====================================================================

;-----------------------------------------------------------------------------
; file_copy - Copy file
; Input: DE: Destination filename string descriptor
;        HL: Source filename string descriptor
; Output:  A: Result
; Clobbered: AF' BC, DE
;-----------------------------------------------------------------------------
file_copy:
    call    dos_open_read         ; A = SrcFil
    ret     m                     
    push    af                    ; Stack = SrcFil, RtnAdr
    ex      de,hl                 ; HL = DstSpc
    call    dos_open_write        ; A = DstFil
    pop     de                    ; D = SrcFil; Stack = RtnAdr
    ret     m
    ld      e,a                   ; E = DstFil
    in      a,(IO_BANK1)           
    push    af                    ; Stack = OldPg, RtnAdr
    ld      a,TMP_BUFFR
    out     (IO_BANK1),a          ; Map TMP_BUFFR into Bank 1
    call    _do_copy              ;
    ex      af,af'                ; AF' = Result
    pop     af                    ; A = OldPg; Stack = RtnAdr
    out     (IO_BANK1),a          ; Map original page into Bank 1
    ex      af,af'                ; AF = Result
    push    af                    ; Stack = Result, RtnAdr
    ld      a,d                   ; A = SrcFil
    call    dos_close             ; Close Source FIle
    ld      a,e                   ; A = DstFil
    call    dos_close             ; Close Destination FIle
    pop     af                    ; AF = Result; Stack = RtnAdr
    ret
    
_do_copy:
    push    de                    ; Stack = SrcDst, RtnAdr
    ld      a,d                   ; A = SrcFil
    ld      bc,$4000              ; BC = LoadLen
    ld      de,BANK1_BASE         ; DE = LoadAdr
    call    esp_readc_bytes       ; A = Result, BC = Bytes Read
    pop     de                    ; DE = SrcDst
    ret     m                     ; Return if error
    ld      a,b
    or      c                     ; If zero bytes read
    ret     z                     ;   Return
    ld      a,e                   ; A = DstFil
    push    de                    ; Stack = SrcDst, RtnAdr
    ld      de,BANK1_BASE         ; DE = LoadAdr
    call    esp_writec_bytes      ; A = Result
    pop     de                    ; DE = SrcDst; Stack = RtnAdr
    ret     m                     ; Return if error
    jr      _do_copy
    
;====================================================================
; Directory Related File I/O Routines
;====================================================================


;-----------------------------------------------------------------------------
; Read directory - filenames only
; Input: A: File descriptor
;       HL: Buffer address
; Output: A: Result
;        BC: Entry Length
;        HL: Entry Address
; Clobbered: BC
;-----------------------------------------------------------------------------
file_read_dir:
    call    dos_read_dir          ; A = Result, B=NamLen, C=EntLen, DE=NamAdr, HL=BufAdr
    ret     m
    push    de                    ; Stack = NamAdr, RtnAdr
    push    bc                    ; Stack = EntLen, NamAdr, RtnAdr
    push    hl                    ; Stack = BufAdr, EntLen, NamAdr, RtnAdr
    ld      de,4
    add     hl,de                 ; HL = AttrAdr
    ld      a,(hl)                ; A = Attrs
    pop     hl                    ; HL = BufAdr; Stack = EntLen, NamAdr, RtnAdr
    pop     de                    ; DE = EntLen; Stack = NamAdr, RtnAdr
    ld      d,0
    add     hl,de                 ; HL = TrmAdr
    and     1                     ; A = AttrDir
    jr      z,.notdir             ; If directory
    ld      (hl),"/"              ;   Append /
    inc     hl
    ld      (hl),0                ;   Terminate FilName
    inc     b                     ;   Bump NamLen
.notdir
    ld      c,b                   ; EntLen = NamLen
    ld      b,0
    pop     hl                    ; HL = NamAdr; Stack = RtnAdr
    xor     a                     ; Return success
    ret

;-----------------------------------------------------------------------------
; Read directory entry into formatted string
; YY-MM-DD HH:MM fsize filespec
; Input: A: File descriptor
;       HL: Buffer address
; Output: A: Result
;         B: Filename length
;         C: Entry length
;        DE: Filename address
;        HL: Buffer address
;-----------------------------------------------------------------------------
file_read_dir_asc:
    push    af                    ; Stack = FilDsc, RtnAdr
    ld      a,ESPCMD_READDIR      ; Read entry
    call    esp_cmd
    pop     af                    ; A = FilDsc; Stack = RtnAdr
    call    esp_send_byte
    call    esp_get_byte          ; A = Result
    or      a                     ; Set flags
    ret     m                     ; Return if error

    push    hl                    ; Stack = BufAdr, RtnAdr
    push    hl                    ; Stack = BufAdr, BufAdr, RtnAdr
    call    esp_get_de            ; DE = Date, A = D
    srl     a                     ; A = Year - 1980
    add     80                    
    call    buffer_a_2digits      ; Write to buffer
    call    buffer_dash           ; Write '-' to buffer
    ld      a, d                  ; Extract Month
    rra                           ; Lowest bit in carry
    ld      a, e
    rra
    call    srl4buffer            ; Shift right and writte to buffer
    call    buffer_dash           ; Write '-' to buffer
    ld      a, e
    and     $1F                   ; A = Day
    call    buffer_a_2digits
    call    buffer_space

    call    esp_get_de            ; DE = Time (hhhhhmmm mmmsssss), A =  D
    call    srl3buffer            ; Write hours to buffer
    call    buffer_colon          ; Write ':' to buffer
    ld      a,d
    and     $07
    ld      c,a
    ld      a,e
    ld      b,5
.srlrra
    srl     c
    rra
    djnz    .srlrra               ; A = Minutes
    call    buffer_a_2digits
    call    buffer_space

    call    esp_get_byte          ; A = Attributes
    and     1                     ; If directory
    jr      z,.filesize
    call    esp_get_long          ;   Skip file length 
    ld      de,_dirstr            ;   Print " <DIR>"
.dirloop    
    ld      a,(de)
    or      a
    jr      z,.do_filename
    inc     de
    ld      (hl),a
    inc     hl
    jr      .dirloop              
.filesize
    call    esp_get_long          ;   BCDE = File Dize
    ld      a,d                   ;   Megabytes range?
    or      a
    jr      nz,.mb
    ld      a,e
    cp      $9C                   ; If >=10,240,000 show Megabytes
    jr      c,.notmb
    ld      a,b
    cp      $40
    jr      nc, .mb
.notmb
    ld      a,e                   ; Kilobytes range?
    or      a
    jr      nz,.kb
    ld      a,b
    cp      $27                   ; If >=10,000 show Kilobytes
    jr      c,.notkb
    cp      $10
    jr      nc,.kb
.notkb
    call    buffer_bc_4digits
    ld      a,'B'
    jr      .suffix
.kb:
    ld      c,b
    ld      b,e
    call    srlb_rrl_buf2
    ld      a,'K'
    jr      .suffix
.mb:
    ld      b,d
    ld      c,e
    call    srlb_rrl_buf4
    ld      a,'M'
    ld      (hl),a
    inc     hl
.suffix
    ld      (hl),a
    inc     hl
.do_filename
    call    buffer_space
.filename:
    call    esp_get_byte
    or      a
    jr      z,.done
    ld      (hl),a
    inc     hl
    jr      .filename
.done:
    xor     a                     ; Write terminating NUL
    ld      (hl),a
    pop     bc                    ; BC = BufAdr; Stack = BufAdr, RtnAdr
    sbc     hl,bc                 ; HL = StrLen
    ld      c,l                   ; C = StrLen
    ld      a,c
    sub     21                    ; 
    ld      b,a                   ; B = NamLen
    pop     hl                    ; HL = BufAdr
    ld      a,l
    add     21
    ld      e,a
    ld      a,h
    add     0
    ld      d,a                   ; DE = NamPtr
    xor     a
    ret

;-----------------------------------------------------------------------------
; Write colon or dash to buffer
; Input: HL: Buffer pointer
; Output: HL: Updated buffer pointer
; Clobbered: A, BC
;-----------------------------------------------------------------------------
buffer_colon:
    ld      a,':'
    byte    $01                   ; LD BC, over LD A
buffer_dash:
    ld      a,'-'
    byte    $01                   ; LD BC, over LD A
;-----------------------------------------------------------------------------
; Write space to buffer
; Input: HL: Buffer pointer
; Output: HL: Updated buffer pointer
; Clobbered: A
;-----------------------------------------------------------------------------
buffer_space:
    ld      a,' '
    ld      (hl),a
    inc     hl
    ret

;-----------------------------------------------------------------------------
; Shift A right and right to buffer
;-----------------------------------------------------------------------------
srl4buffer:
    srl     a
srl3buffer:
    srl     a
    srl     a
    srl     a
;-----------------------------------------------------------------------------
; Write 2 number digit in A to buffer
; Input: A: Number
;       HL: Buffer pointer
; Output: HL: Updated buffer pointer
; Clobbered: A, C
;-----------------------------------------------------------------------------
buffer_a_2digits:
    cp      100
    jr      c, .l0
    sub     a, 100
    jr      buffer_a_2digits
.l0:
    ld      c, 0
.l1:
    inc     c
    sub     a, 10
    jr      nc, .l1
    add     a, 10
    push    a

    ld      a, c
    add     '0'-1
    ld      (hl),a
    inc     hl
    pop     a
    add     '0'
    ld      (hl),a
    inc     hl
    ret

srlb_rrl_buf4:
    srl     b
    rr      c
    srl     b
    rr      c
srlb_rrl_buf2:
    srl     c
    rr      b
    srl     c
    rr      b
;-----------------------------------------------------------------------------
; Output 4 number digit in BC
;-----------------------------------------------------------------------------
buffer_bc_4digits:
    push    hl                    ; Stack = BufPtr, RtnAdr
    ld      h,b
    ld      l,c
    ld      e,'0'
    ld	    bc,-10000
    call	  .num1
    ld	    bc,-1000
    call	  .num1
    ld	    bc,-100
    call	  .num1
    ld	    c,-10
    call	  .num1
    ld      e,0
    ld	    c,-1
    call    .num1
    pop     hl                    ; HL = BufPtr; Stack = RtnAdr
    ret
.num1:
    ld	    a,-1
.num2:
    inc	    a
    add	    hl,bc
    jr	    c,.num2
    sbc	    hl,bc
    add     a,'0'
    cp      e
    jr      nz,.num3
    add     a,' '-'0'
    byte    $16                   ; LD D over DEC E
.num3
    dec     e
    pop     ix                    ; IX = RtnAdr
    ex      (sp),hl               ; HL = BufPtr
    ld      (hl),a
    inc     hl
    ex      (sp),hl               ; Stack = BufPtr
    jp      (ix)                  ; Return

_dirstr:   
    byte    " <DIR>",0

