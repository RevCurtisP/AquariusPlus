;===========================================================================
; Statement and Function Dispatch Tables
; Aligned to 256 byte boundaries
;===========================================================================

;Put the lookup table at 256 byte boundary
if $ & $FF
    dc ($FF00&$)+256-$,$FF
endif

;----------------------------------------------------------------------------
; Combined Statement Jump Table 
; Total Length: 256 Bytes
; Standard BASIC Routines left as HEX
;----------------------------------------------------------------------------
STJUMPS:
    dw      ST_END                ;$80 END              
    dw      $05BC                 ;$81 FOR               
    dw      $0D13                 ;$82 NEXT              
    dw      $071C                 ;$83 DATA              
    dw      ST_INPUT              ;$84 INPUT             
    dw      $10CC                 ;$85 DIM               
    dw      ST_READ               ;$86 READ
    dw      $0731                 ;$87 LET               
    dw      $06DC                 ;$88 GOTO              
    dw      $06BE                 ;$89 RUN               
    dw      $079C                 ;$8A IF               
    dw      $ST_RESTORE           ;$8B RESTOR            
    dw      $06CB                 ;$8C GOSUB             
    dw      ST_RETURN             ;$8D RETURN            
    dw      $071E                 ;$8E REM               
    dw      ST_STOP               ;$8F STOP              
    dw      ST_ON                 ;$90 ON
    dw      $07B5                 ;$91 LPRINT            
    dw      ST_COPY               ;$92 COPY              
    dw      ST_DEF                ;$93 DEF               
    dw      ST_POKE               ;$94 POKE              
    dw      ST_PRINT              ;$95 PRINT             
    dw      $0C4B                 ;$96 CONT              
    dw      ST_LIST               ;$97 LIST              
    dw      ST_LLIST              ;$98 LLIST             
    dw      ST_CLEAR              ;$99 CLEAR             
    dw      $1C2C                 ;$9A CLOAD             
    dw      $1C08                 ;$9B CSAVE             
    dw      ST_PSET               ;$9C PSET              
    dw      ST_PRESET             ;$9D PRESET            
    dw      $1AD6                 ;$9E SOUND             
    dw      $0BBD                 ;$9F NEW
;Miscellaneous Functions
    dw      SNERR                 ;$A0 TAB(   
    dw      SNERR                 ;$A1 TO     
    dw      FN_FN                 ;$A2 FN     
    dw      SNERR                 ;$A3 SPC(   
    dw      SNERR                 ;$A4 INKEY$ 
    dw      SNERR                 ;$A5 THEN   
    dw      SNERR                 ;$A6 NOT    
    dw      SNERR                 ;$A7 STEP   
;Operators
    dw      SNERR                 ;$A8 +      
    dw      SNERR                 ;$A9 -      
    dw      SNERR                 ;$AA *      
    dw      SNERR                 ;$AB /      
    dw      SNERR                 ;$AC ^      
    dw      SNERR                 ;$AD AND    
    dw      SNERR                 ;$AE OR     
    dw      SNERR                 ;$AF >      
    dw      SNERR                 ;$B0 =      
    dw      SNERR                 ;$B1 <      
;Standard BASIC Functions
    dw      SNERR                 ;$B2 SGN     
    dw      SNERR                 ;$B3 INT     
    dw      SNERR                 ;$B4 ABS     
    dw      SNERR                 ;$B5 USR  
    dw      SNERR                 ;$B6 FRE     
    dw      SNERR                 ;$B7 LPOS    
    dw      SNERR                 ;$B8 POS     
    dw      SNERR                 ;$B9 SQR     
    dw      SNERR                 ;$BA RND     
    dw      SNERR                 ;$BB LOG     
    dw      SNERR                 ;$BC EXP     
    dw      SNERR                 ;$BD COS     
    dw      SNERR                 ;$BE SIN     
    dw      SNERR                 ;$BF TAN     
    dw      SNERR                 ;$C0 ATN     
    dw      SNERR                 ;$C1 PEEK    
    dw      SNERR                 ;$C2 LEN     
    dw      SNERR                 ;$C3 STR$     
    dw      SNERR                 ;$C4 VAL     
    dw      SNERR                 ;$C5 ASC     
    dw      SNERR                 ;$C6 CHR$     
    dw      SNERR                 ;$C7 LEFT$    
    dw      SNERR                 ;$C8 RIGHT$   
    dw      ST_MID                ;$C9 MID$     
    dw      SNERR                 ;$CA POINT
;PlusBASIC Statements and Functions
    dw      SNERR                 ;$CB XOR
    dw      ST_PUT                ;$CC PUT
    dw      ST_GET                ;$CD GET
    dw      ST_DRAW               ;$CE DRAW
    dw      SNERR                 ;$CF unused
    dw      ST_LINE               ;$D0 LINE
    dw      ST_SWAP               ;$D1 SWAP 
    dw      ST_DOKE               ;$D2 DOKE
    dw      ST_TIMER              ;$D3 TIME 
    dw      ST_EDIT               ;$D4 EDIT   
    dw      ST_CLS                ;$D5 CLS    
    dw      ST_LOCATE             ;$D6 LOCATE 
    dw      ST_OUT                ;$D7 OUT    
    dw      ST_PSG                ;$D8 PSG    
    dw      SNERR                 ;$D9 MOUSE
    dw      ST_CALL               ;$DA CALL   
    dw      ST_LOAD               ;$DB LOAD   
    dw      ST_SAVE               ;$DC SAVE   
    dw      ST_DIR                ;$DD DIR    
    dw      ST_MKDIR              ;$DE MKDIR
    dw      ST_DEL                ;$DF DEL    
    dw      ST_CD                 ;$E0 CD     
    dw      ST_INC                ;$E1 IN
    dw      SNERR                 ;$E2 JOY
    dw      SNERR                 ;$E3 HEX
    dw      ST_RENAME             ;$E4 RENAME
    dw      SNERR                 ;$E5 DATE
    dw      ST_DEC                ;$E6 DEC
    dw      SNERR                 ;$E7 MOD
    dw      SNERR                 ;$E8 DEEK
    dw      SNERR                 ;$E9 ERR
    dw      SNERR                 ;$EA STRING$
    dw      SNERR                 ;$EB BIT
    dw      SNERR                 ;$EC unused
    dw      SNERR                 ;$ED EVAL
    dw      ST_PAUSE              ;$EE PAUSE
    dw      SNERR                 ;$EF ELSE
    dw      SNERR                 ;$F0 TILE
    dw      SNERR                 ;$F1 RGB
    dw      SNERR                 ;$F2 MAP
    dw      SNERR                 ;$F3 FILE
    dw      ST_RESUME             ;$F4 RESUME
    dw      ST_COLOR              ;$F5 COL
    dw      ST_SCREEN             ;$F6 SCREEN 
    dw      ST_SET                ;$F7 SET
    dw      ST_WRITE              ;$F8 WRITE
    dw      ST_USE                ;$F9 USE
    dw      ST_OPEN               ;$FA OPEN
    dw      ST_CLOSE              ;$FB CLOSE
    dw      SNERR                 ;$FC unused
    dw      SNERR                 ;$FD unused
    dw      extended_statement    ;$FE XTOKEN
    dw      SNERR                 ;$FF unused

;----------------------------------------------------------------------------
; Combined Function Jump Table 
; Total Length: 106 Bytes
; Immediate follows STJUMPS, aligning it to a 256 byte boundary
;----------------------------------------------------------------------------
FNJUMPS:
; Standard BASIC Functions
    dw      HOOK27+1              ;$B2 SGN     
    dw      FN_INT                ;$B3 INT     
    dw      HOOK27+1              ;$B4 ABS     
    dw      HOOK27+1              ;$B5 USR  
    dw      FN_FRE                ;$B6 FRE     
    dw      HOOK27+1              ;$B7 LPOS    
    dw      FN_POS                ;$B8 POS     
    dw      HOOK27+1              ;$B9 SQR     
    dw      HOOK27+1              ;$BA RND     
    dw      HOOK27+1              ;$BB LOG     
    dw      HOOK27+1              ;$BC EXP     
    dw      HOOK27+1              ;$BD COS     
    dw      HOOK27+1              ;$BE SIN     
    dw      HOOK27+1              ;$BF TAN     
    dw      HOOK27+1              ;$C0 ATN     
    dw      FN_PEEK               ;$C1 PEEK    
    dw      FN_LEN                ;$C2 LEN     
    dw      HOOK27+1              ;$C3 STR$     
    dw      HOOK27+1              ;$C4 VAL     
    dw      FN_ASC                ;$C5 ASC     
    dw      HOOK27+1              ;$C6 CHR$     
    dw      HOOK27+1              ;$C7 LEFT$    
    dw      HOOK27+1              ;$C8 RIGHT$   
    dw      HOOK27+1              ;$C9 MID$     
    dw      FN_POINT              ;$CA POINT
; PlusBASIC Statements and Functions
    dw      SNERR                 ;$CB XOR
    dw      SNERR                 ;$CC PUT  
    dw      FN_GET                ;$CD GET   
    dw      SNERR                 ;$CE   
    dw      SNERR                 ;$CF   
    dw      SNERR                 ;$D0 LINE
    dw      SNERR                 ;$D1 SWAP  
    dw      SNERR                 ;$D2 DOKE
    dw      FN_TIME               ;$D3 TIME$
    dw      SNERR                 ;$D4 EDIT   
    dw      SNERR                 ;$D5 CLS    
    dw      SNERR                 ;$D6 LOCATE 
    dw      SNERR                 ;$D7 OUT    
    dw      SNERR                 ;$D8 PSG    
    dw      FN_MOUSE              ;$D9 MOUSE  
    dw      SNERR                 ;$DA CALL   
    dw      SNERR                 ;$DB LOAD   
    dw      SNERR                 ;$DC SAVE   
    dw      SNERR                 ;$DD DIR    
    dw      SNERR                 ;$DE MKDIR    
    dw      SNERR                 ;$DF DEL    
    dw      FN_CD                 ;$E0 CD$()     
    dw      FN_IN                 ;$E1 IN()
    dw      FN_JOY                ;$E2 JOY()
    dw      FN_HEX                ;$E3 HEX()
    dw      SNERR                 ;$E4 RENAME
    dw      FN_DATE               ;$E5 DATE$
    dw      FN_DEC                ;$E6 DEC
    dw      SNERR                 ;$E7 MOD
    dw      FN_DEEK               ;$E8 DEEK
    dw      FN_ERR                ;$E9 ERR
    dw      FN_STRING             ;$EA STRING$
    dw      FN_BIT                ;$EB BIT
    dw      SNERR                 ;$EC 
    dw      FN_EVAL               ;$ED EVAL
    dw      SNERR                 ;$EE PAUSE
    dw      SNERR                 ;$EF ELSE
    dw      FN_TILE               ;$F0 TILE
    dw      FN_RGB                ;$F1 RGB
    dw      SNERR                 ;$F2 MAP
    dw      FN_FILE               ;$F3 FILE
    dw      SNERR                 ;$F4 RESUME
    dw      FN_COLOR              ;$F5 COL
    dw      FN_SCREEN             ;$F6 SCREEN 
    dw      SNERR                 ;$F7 SET
    dw      SNERR                 ;$F8 WRITE
    dw      SNERR                 ;$F9 USE
    dw      SNERR                 ;$FA OPEN
    dw      SNERR                 ;$FB CLOSE
    dw      SNERR                 ;$FC
    dw      SNERR                 ;$FD
    dw      extended_function     ;$FE
    dw      SNERR                 ;$FF

;===========================================================================
; Extended BASIC Utility calls
; Aligned to next 256 byte boundary
;===========================================================================

    dc $C200-$,$76

_jump_table:
; .
; ******************************************************
; * Extended ROM                                       *
; * Running from paged memoty in bank 3 ($C000-$FFFF): *
; *   LD   IY,label                                    *
; *   CALL ext_call                                    *
; ******************************************************
; <<Extended ROM>>
    jp      just_ret              ; C200 
    jp      just_ret              ; C203 
    jp      FLOAT_BC              ; C206 Convert BC to unsigned float in FACC
    jp      FLOAT_DE              ; C209 Convert DE to unsigned float in FACC
    jp      FLOAT_CDE             ; C20C Convert CDE to unsigned float in FACC
_end_jump_table:

;===========================================================================
; Escaped String Escape Sequence lookup tables
; Filling in unused space before next table
;===========================================================================

esc_sequences:
    byte    7               ; \a  ^G  BEL 
    byte    8               ; \b  ^H  BS
    byte    'c'             ; {Ctl-char}
    byte    127             ; \d      DEL
    byte    27              ; \e  ^[  ESC
    byte    12              ; \f  ^L  FF
    byte    'g','h','i','j'
    byte    'k'             ; {Kbd-char}
    byte    'l','m'
    byte    255             ; \n  newline
    byte    'o','p','q'
    byte    13              ; \r  ^M  CR
    byte    's'
    byte    9               ; \t  ^I  TAB
    byte    'u'
    byte    11              ; \v  ^K  VT  (clear screen)


;===========================================================================
; Error message lookup tables
; Aligned to next 256 byte boundary
;===========================================================================

;Put the lookup table at 256 byte boundary
if $ & $FF
    dc ($FF00&$)+256-$,$76
endif

;----------------------------------------------------------------------------
; Combined Statement Jump Table 
; Immediate follows STJUMPS, aligning it to a 256 byte boundary
;----------------------------------------------------------------------------

