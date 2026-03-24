;====================================================================
; Graphics Bitmap Read/Write TMP_BUFFR Routines
;====================================================================

;-----------------------------------------------------------------------------
; Copy Bitmap data in TMP_BUFFR to Video RAM and palette
; Input: BC = Length of data to copy
; Sets flags: Carry if invalid data length
; Clobbers: A, AF', BC, DE, HL
; 
;  Size  Hex  Format
;  8000 $1F40 1bpp pixels only
;  9000 $2328 1bpp pixel + color
;  9032 $2348 1bpp pixel + color + palette
;  9192 $23E8 1bpp pixel + palette + 160 byte gap + color
; 16000 $3E80 4bpp pixels only
; 16032 $3EA0 4bpp pixels + palette
;-----------------------------------------------------------------------------
bitmap_read_tmpbfr:
    ld      h,b
    ld      l,c                   ; HL = DatLen
    ld      de,$1F40              ; 
    rst     COMPAR                ; If DatLen = 8000 
    jr      z,.bitmap1bpp         ;   Copy 1bpp pixels only
    ld      de,$2328              ; 
    rst     COMPAR                ; Else If DatLen = 9000 
    jr      z,.color1bpp          ;   Copy 1bpp pixels + color
    ld      e,$48                 ; 
    rst     COMPAR                ; Else If DatLen = 9032 
    jr      z,.palette1bpp        ;   Copy 1bpp pixels + color + palette
    ld      e,$E8                 ; 
    rst     COMPAR                ; Else If DatLen = 9192 
    jr      z,.bitgap1bpp         ;   Legacy 1bpp pixels
    ld      de,$3E80
    rst     COMPAR                ; Else If DatLen = 16000
    jr      z,.bitmap4bpp         ;   Copy 4bpp pixels
    ld      e,$A0
    rst     COMPAR                ; Else If DatLen <> 16032
    scf                           ;   Return Carry Set
    ret     nz
.palette4bpp    
    ld      de,BANK1_BASE+16000
    call    .palette
.bitmap4bpp
    ld      bc,16000
    jr      .bitmap
.color
    ld      bc,1000
    ld      de,8192
    jr      .zero
.bitgap1bpp
    ld      bc,8000               
    call    .bitmap
    ld      de,BANK1_BASE+8000
    call    .palette
    ld      bc,1000
    ld      de,8192
    ld      hl,8192
    jr      .copy
.palette1bpp
    ld      de,BANK1_BASE+9000
    call    .palette 
.color1bpp
    ld      hl,8000
    ld      bc,1000
    ld      de,8192
    call    .copy
.bitmap1bpp
    ld      bc,8000
.bitmap
    ld      de,0
.zero
    ld      hl,0
.copy
    jp      _copy_tmpbfr_vidram
; Enter with DE pointing to bitmap data
.palette
    call    page_map_tmpbfr_af
    xor     a
    ld      bc,32
    ld      l,1
    call    palette_set
    jp      page_restore_bank1_af
 
;-----------------------------------------------------------------------------
; Copy Bitmap date in TMP_BUFFR to Video RAM and palette
; Input: A = 0 = 1bpp, 1 = 4bpp
; Output: BC = Length of copied data
; Sets flags: Carry if invalid data length
; Clobbers: A, AF', BC, DE, HL
;-----------------------------------------------------------------------------
bitmap_write_tmpbfr:
    or      a
    jr      nz,.do4bpp            ; If A = 0
    ld      bc,8000
    call    .bitmap               ;   Write pixel data to 0-7999          
    ld      hl,8192
    ld      de,8000
    ld      bc,1000
    call    .copy                 ;   Write color data to 8000-8999             
    ld      de,BANK1_BASE+9000
    call    .palette              ;   Write palette to 9000-9031
    ld      bc,9032               ;   Return DatLen = 9032
    ret                           ; Else
.do4bpp
    ld      bc,16000              
    call    .bitmap               ;   Write pixel data to 0-15999
    ld      de,BANK1_BASE+16000
    call    .palette              ;   Write palette to 16000-16031
    ld      bc,16032              ;   Return DatLen = 9032
    ret
.bitmap
    ld      hl,0
    ld      de,0
.copy
    ld      a,VIDEO_RAM           ; Copying from Video RAM
    ex      af,af'
    ld      a,TMP_BUFFR           ; to buffer
    jp      page_fast_copy
; Enter with DE pointing to bitmap data
.palette
    call    page_map_tmpbfr_af
    ld      a,1
    ld      bc,32
    call    palette_get
    jp      page_restore_bank1_af


; Input: BC: BytCnt, DE: DstAdr, HL: SrcAdr
; Output: DE: NewDstAdr, HL: NewSrcAdr
_copy_tmpbfr_vidram:
    ld      a,TMP_BUFFR           ; Copying from buffer
    ex      af,af'
    ld      a,VIDEO_RAM           ; to Video RAM
    jp      page_fast_copy
