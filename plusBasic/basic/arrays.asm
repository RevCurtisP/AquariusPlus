marrays:

;-----------------------------------------------------------------------------
; DIM(x...)=literal,...
;-----------------------------------------------------------------------------
; DIM A(9)=1,2,3,4,5,6,7,8,9,10
; FOR I=0 TO 9:PRINT I;A(I):NEXT
; DIM A$(9)=A,B,C,D,E,F,G,H,I,J
; FOR I=0 TO 9:PRINT I,A$(I):NEXT
; CLEAR
; DIM A(2,2)=1,2,3,4,5,6,7,8,9
; FOR I=0 TO 2:FOR J=0 TO 2:PRINT I;J,A(J,I):NEXT J,I
; CLEAR
; DIM A$(2,2)=A1,B1,C1,A2,B2,C2,A3,B3,C3
; FOR I=0 TO 2:FOR J=0 TO 2:PRINT I;J,A$(J,I):NEXT J,I
dim_extension:
    call    CHRGT2                ; Reget character
    ret     z                     ; Return if terminator
    cp      EQUATK                ; If not = 
    jp      nz,DIMNXT             ;   Do next DIM
    rst     CHRGET                ; Skip =
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ld      hl,(TEMP3)            ; HL = DimsPtr
    dec     hl
    dec     hl
    ld      b,(hl)
    inc     hl
    ld      c,(hl)                ; BC = AryLen;
    inc     hl
    ld      (ARRAYLEN),bc         ; BC = AryLen
    ld      a,(hl)                ; A = NumDims
    or      a                     ; If no dimensions
    jp      z,UDERR               ;   Undimensioned array error
    inc     hl
    add     a,a                   ; A = NumDims * 2
    ld      b,0
    ld      c,a                   ; BC = NumDims * 2
    add     hl,bc                 ; HL = AryAdr    
    ld      (ARRAYPTR),hl         ; ARRAYPTR = AryPtr
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    call    CHRGT2                ; Reget current character
    ret     z                     ; If terminator, return
    ld      a,$FF
    ld      (ARRAYREQ),a          ; Strings must be quoted
    call    array_read            ; Read literals into array
    ret     c                     ; Return if end of statement
    call    CHRGT2                ; If terminator
    ret     z
    call    no_more               ; If comma, Too many operands error
    jp      SNERR                 ; Else syntax error

;-----------------------------------------------------------------------------
; READ *var
;-----------------------------------------------------------------------------
; 10 INPUT I:PRINT I
; 20 READ A,B,C,D:PRINT A,B,C,D
; 30 DATA $AA,$BB,$CC,$DD
; 40 DIM A(9)
; 50 READ *A
; 60 FOR I=0 TO 9:PRINT A(I);:NEXT
; 70 DATA 1,$2,3,$4,5
; 80 DATA $6,7,$8,9,$A

;
; 10 DIM A$(9)
; 20 READ *A$
; 30 FOR I=0 TO 9:PRINT I,A$(I):NEXT
; 40 DATA A,B,C,D,E
; 50 DATA F,G,H,I,J
read_extension:
    pop     hl                    ; HL = TxtPtr, Stack = RtnAdr
    cp      MULTK                 
    jr      z,.read_array         ; If not *
    push    hl                    ;   Stack = TxtPtr, RtnAdr
    jp      read_cont             ;   Continue normal read
.read_array
    xor     a
    ld      (ARRAYREQ),a          ; Unquoted strings allowed
    call    get_star_array        ; DE = AryAdr, BC = AryLen
    push    hl                    ; Stack = TxtPtr, RtnAdr
    ld      (ARRAYPTR),de
    ld      (ARRAYLEN),bc
    call    GETYPE                ; A = 0: String, 1: Numeric
    ld      (ARRAYTYP),a          ; Save arraytype
    ld      hl,(DATPTR)           ;
    dec     hl                    ; Back up for CHRGET
.read_loop
    rst     CHRGET                ; Skip DATA
    jr      z,.more_data          ; If not terminator
    call    _array_read           ;   If array filled
    ld      (DATPTR),hl
    jp      nc,POPHRT             ;     Pop text pointer and return
.more_data
    call    DATA                  ;; Skip next statement
    or      a                     ;
    jr      nz,.next_st           ;; If End of Line
    inc     hl                    ;
    ld      a,(hl)                ;
    inc     hl                    ;
    or      (hl)                  ;;   A = 0 if end of pr
    ld      e,ERROD               ;[M80] NO DATA IS ERROR
    jp      z,ERROR               ;[M80] IF SO COMPLAIN
    inc     hl                    ;[M80] SKIP PAST LINE #
    ld      e,(hl)                ;[M80] GET DATA LINE #
    inc     hl                    ;
    ld      d,(hl)                ;
    ld      (DATLIN),de           ;
.next_st
    rst     CHRGET                ;[M80] GET THE STATEMEN
    cp      DATATK                ;[M80] IS IS "DATA"?
    jr      nz,.more_data         ;[M80] NOT DATA SO LOOK
    jr      .read_loop                 ;[M80] CONTINUE READING

; Populate array from comma separated text
; (ARRAYPTR) = Pointer into array data
; (ARRAYLEN) = Bytes left in array data
; HL = TxtPtr (next literal to read)
array_read:
    call    GETYPE                ; A = 0: String, 1: Numeric
    ld      (ARRAYTYP),a          ; Save arraytype
_array_read:
    ld      a,(ARRAYTYP)
    or      a
    jr      z,.string             ; If numeric array
    call    fin_extokens          ;   Read number into FACC
    push    hl                    ;   Stack = TxtPtr, RtnAdr
    ld      hl,FACLO              ;   HL = FACC
    jr      .next                 ; Else
.string
    call    CHRGT2                ;   A = First character
    cp      ','
    jr      z,.comma
    ld      d,a                   ;   If it's a quote, terminators will be quote
    ld      e,a                   ;   
    cp      '"'
    jr      z,.parse              ;   If not quoted
    ld      a,(ARRAYREQ)
    or      a                     ;     If quotes required
    jp      nz,TMERR              ;       Type mismatch error
.comma
    ld      d,':'                 ;     Terminators are colon and comms
    ld      b,','                 ;    
    dec     hl                    ;     Back up TxtPtr
.parse
    call    STRLT2                ;   Parse string literal
    push    hl                    ;   Stack = TxtPtr, RtnAdr
    call    FREFAC                ;   HL = StrDsc 
    ex      de,hl                 ;   DE = StrDsc
    call    copy_literal_string
    ex      de,hl                 ;   HL = StrDsc
.next
    ld      de,(ARRAYPTR)         ; HL = AryPtr
    ld      bc,4
    ldir                          ; Copy to Array
    ld      (ARRAYPTR),de         ; ARRAYPTR = AryPtr
    pop     hl                    ; HL = TxtPtr; Stack = RtnAdr
    ld      bc,(ARRAYLEN)         ; BC = AryLen
    dec     bc                    ; Count down
    dec     bc
    dec     bc
    dec     bc
    ld      (ARRAYLEN),bc         ; BC = AryLen
    ld      a,b
    or      c                     ; If end of array
    ret     z                     ;   Return
    call    CHRGT2                ; If terminator
    scf                           ;   Return Carry Set
    ret     z
.noteol
    SYNCHK  ','                   ; Else require comma
    jr      _array_read           ;   and get next item

    msize_arrays = $ - marrays