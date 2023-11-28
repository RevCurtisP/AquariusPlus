;====================================================================
; File I/O Machine Language Routines
;====================================================================

;-----------------------------------------------------------------------------
; Load binary file into main memory
; Input: BC: maximum length
;        DE: destination address
;        HL: string descriptor address
; Output: A: result code
;        BC: number of bytes actually read
;        DE: next destination address 
; Flags Set: S if I/O error
; Clobbered: HL
;-----------------------------------------------------------------------------
file_load_binary:
    call    dos_open_read
    ret     m
    call    esp_read_bytes
    ret     m
    push    af
    call    esp_close_all
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
;        BC: maximum length
;        DE: destination address
;        HL: string descriptor address
; Output: A: result code
;        BC: number of bytes actually read
;        DE: next destination address 
;         H: destination page
; Flags Set: Z if llegal page
;            C if page overflow
;            S if I/O error
; Clobbered: AF',L
;-----------------------------------------------------------------------------
file_load_paged:
    push    af                    ; Stack = Page, RtnAdr
    call    dos_open_read
    pop     hl                    ; H = Page, Stack = RtnAdr
    ret     m
    call    esp_read_paged        ;
    push    af                    ; Stack = Result, RtnAdr
    ld      a,l
    call    dos_close_file
    pop     af                    ; AF = Result; Stack = RtnAdr
    ret


;-----------------------------------------------------------------------------
; Load ROM file into page 35
;        HL: string descriptor address
;-----------------------------------------------------------------------------
file_load_rom:
    ld      a,35
    ld      de, $C000
    ld      bc, $4000
    call    file_load_paged
    ret     m

    ; Check length
    ld      a, b
    cp      $20         ; 8KB ROM?
    jr      nz,.done
    
    ld      hl, $0000
    ld      de, $2000
    ld      bc, $2000
    call    page_fast_copy
.done
    xor     a
    ret

;-----------------------------------------------------------------------------
; Load screen image
; Input: HL: String descriptor address
; Output: A: result code
; Flags Set: S if I/O error
; Clobbered: CD, DE, EF
;-----------------------------------------------------------------------------
file_load_screen:
    call    dos_open_read
    ret     m
; Read first 2k into Screen RAM
    ld      bc,2048
    ld      de,SCREEN
    call    esp_read_bytes
    ret     m
; Read second 2k into scratch RAM
    ld      a,RAM_BAS_3
    ld      bc,2048
    ld      de,$3000
    call    esp_read_paged

    
    
    push    af
    call    esp_close_all
    pop     af
    or      a
    ret
