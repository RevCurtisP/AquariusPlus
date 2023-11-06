;====================================================================
; File I/O Machine Language Routines
;====================================================================

;-----------------------------------------------------------------------------
; Load binary file into main memory
; Input: BC: Maximum Length
;        DE: Start Address
;        HL: String descriptor address
; Output: A: result code
;        BC: number of bytes actually read
;     DE,HL: next address (start address if no bytes read)
; Flags Set: S if I/O error
;-----------------------------------------------------------------------------
file_load_binary:
    call    esp_open_read
    ret     m
    call    esp_read_bytes
    ret     m
    push    af
    call    esp_close
    pop     af
    or      a
    ret

;-----------------------------------------------------------------------------
; Load file into character RAM buffer
; Input: HL: File name string descriptor address
; Output: A: Result
; Clobbered registers: BC,DE,HL
;-----------------------------------------------------------------------------
file_load_chrset:
    ld      a,BAS_BUFFR
    ld      bc,CHRSETLEN
    ld      de,CHRSETBUF
    call    file_load_paged
    ret     z                     ; Illegal page
    ret     c                     ; Page overflow
    ret     m                     ; I/O error
    jp      custom_chrset    

;-----------------------------------------------------------------------------
; Load binary file into paged memory`
; Input: A: Page
;        BC: Maximum Length
;        DE: Start Address
;        HL: String descriptor address
; Output: A: result code
;        BC: number of bytes actually read
;     DE,HL: next address (start address if no bytes read)
; Flags Set: Z if llegal page
;            C if page overflow
;            S if I/O error
;-----------------------------------------------------------------------------
file_load_paged:
    push    af                    ; Stack = Page
    call    esp_open_read         ;
    jp      m,_discard
    pop     af                    ; AF = Page
    call    esp_read_paged        ;
    ret     z                     ; Illegal page
    ret     c                     ; Page overflow
    push    af
    call    esp_close
    pop     af
    ret
_discard:
    inc     sp                    ; Discard top entry on stack
    inc     sp
    ret
