;====================================================================
; Miscellaneous Assembly Calls used by BASIC
;====================================================================

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
    ld      a,BAS_BUFFR
    call    page_write_bytes
    ld      a,BAS_BUFFR
    inc     de
    jp      page_write_byte
    
;-----------------------------------------------------------------------------
; S3 BASIC patch
; Skip Label in ON GOSUB
;-----------------------------------------------------------------------------
skip_on_label:
    rst     CHRGET            ;;Get first character of LineRef            
    cp      '_'               ;;If not a label                            
    jp      nz,SCNLIN         ;;  Scan Line Number                        
.loop                                                                              
    rst     CHRGET            ;;Get next character                        
    ret     z                 ;;Return if end of line                     
    cp      ','                                                           
    ret     z                 ;;Return if comma                           
    cp      ':'                                                           
    ret     z                 ;;Return if colon                           
    jr      .loop             ;;Check next character                      
