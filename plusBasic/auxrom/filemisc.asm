;====================================================================
; File I/O Statements and Functions Auxillary ROM routines
;====================================================================


;-----------------------------------------------------------------------------
; file_checkdir - return true if filespec exists and is directory
; Flags are NOT set
;-----------------------------------------------------------------------------
file_checkdir:
    call    send_stat_cmd         ; Send stat command
    jp      m,aux_ret_zero        ; If no error
    call    esp_get_long          ;   Skip DateTime
    call    esp_get_byte          ;   A = FileAttr
    and     1                     ;   If not directory
    ret     z                     ;     Return 0
    or      $FF                   ;   Else
    ret                           ;     Return -1
aux_ret_zero:
    xor     a                     ; Else
    ret                           ;   Return 0

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; file_datetime - Get file date
; - Returns the specified file's date stamp as a 14 byte ASCII string
; | in the format YYYYMMDDHHMMSS
; Input: DE: Buffer address
;        HL: Filespec string descriptor
; Output:  A: Result
;         BC: Data Length
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
file_datetime:
    call    send_stat_cmd         ; Send stat command
    ret     m                     ;   Return if error
    push    de                    ; Stack = BufAdr
    ex      de,hl                 ; HL = BufAdr. RtnAdr
    call    esp_get_de            ; DE = Date, A = D
    push    de                    ; Stack = Date, RtnAdr
    srl     a                     ; A = Year - 1980
    add     80                    
    ld      de,$3139
    jr      c,.c1900
    ld      de,$3230
.c1900
    call    buffer_de             ; Write century to buffer
    call    buffer_a_2digits      ; Write to buffer
    pop     de                    ; DE = Date; Stack = RtnAdr
    ld      a, d                  ; Extract Month
    rra                           ; Lowest bit in carry
    ld      a, e
    rra
    call    srl4buffer            ; Shift right and writte to buffer
    ld      a, e
    and     $1F                   ; A = Day
    call    buffer_a_2digits
    call    esp_get_de            ; DE = Time (hhhhhmmm mmmsssss), A =  D
    call    srl3buffer            ; Write hours to buffer
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
    ld      de,$3030              ; DE = Seconds
    call    buffer_de             ; Write seoonds to buffer
    pop     de                    ; DE = BufAdr; Stack = RtnAdr
    ld      bc,14                 ; BC = StrLen
    ret

buffer_de:
    ld      (hl),d                
    inc     hl
    ld      (hl),e
    inc     hl
    ret
    
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; file_attrs:
; - Returns the specified file's attributes byte
; | (see dos_read_dir for structure)
; Input: HL: Filespec string descriptor
; Output:  A: Result
;          B: Attributes byte
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
file_attrs:
    call    send_stat_cmd         ; Send stat command
    ret     m                     ;   Return if error
    call    esp_get_long          ; Skip DateTime
    call    esp_get_byte          
    ld      b,a                   ; B = Attribute byte
    xor     a                     ; Return no errors
    ret       

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; file_size:
; - Returns the specified file's size as a 32-bit integer
; Input: HL: Filespec string descriptor
; Output:  A: Result
;          DEBC: File size
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
file_size:
    call    file_attrs            ; Skip DateTime and attrs 
    ret     m                     ;   Return if error
    call    esp_get_long          ; BCDE = FilZiz
    xor     a                     ; Return no errors
    ret       
