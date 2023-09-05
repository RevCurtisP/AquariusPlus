;=====================================================================================
; Keyword Table and Routines to parse and expand tokens
; plusBASIC and MX-BASIC cross-refernce at bottom
;=====================================================================================

;-----------------------------------------------------------------------------
; plusBASIC keyword list
;-----------------------------------------------------------------------------
BTOKEN:     equ $CB               ; Our first token number

TBLCMDS:

    db $80 + ' '                  ; $CB            
    db $80 + ' '                  ; $CC            
    db $80 + 'G',"ET"             ; $CD             
    db $80 + ' '                  ; $CE            
    db $80 + ' '                  ; $CF             
    db $80 + 'L',"INE"            ; $D0             
    db $80 + ' '                  ; $D1            
    db $80 + ' '                  ; $D2   paint/fill
    db $80 + 'T',"IME"            ; $D3   Replaces MX-BASIC keyword SDTM
    db $80 + 'E',"DIT"            ; $D4   Not Implemented
    db $80 + 'C',"LS"             ; $D5
    db $80 + 'L',"OCATE"          ; $D6
    db $80 + 'O',"UT"             ; $D7
    db $80 + 'P',"SG"             ; $D8
    db $80 + 'D',"EBUG"           ; $D9   Not Implemented
    db $80 + 'C',"ALL"            ; $DA
    db $80 + 'L',"OAD"            ; $DB
    db $80 + 'S',"AVE"            ; $DC
    db $80 + 'D',"IR"             ; $DD
    db $80 + 'M',"KDIR"           ; $DE   Replaces USB-BASIC/MX-BASIC keyword CAT
    db $80 + 'D',"EL"             ; $DF
    db $80 + 'C',"D"              ; $E0
    db $80 + 'I',"N"              ; $E1
    db $80 + 'J',"OY"             ; $E2
    db $80 + 'H',"EX$"            ; $E3
    db $80 + 'R',"ENAME"          ; $E4
    db $80 + 'D',"ATE"            ; $E5   Replaces MX-BASIC keyword DTM$
    db $80 + ' '                  ; $E6             
    db $80 + 'K',"EY"             ; $E7             
    db $80 + 'A',"RGS"            ; $E8   Replaces MX-BASIC keyword DEEK
    db $80 + 'E',"RR"             ; $E9             
    db $80 + ' '                  ; $EA             
    db $80 + ' '                  ; $EB             
    db $80 + ' '                  ; $EC             
    db $80 + 'E',"VAL"            ; $ED             
    db $80 + ' '                  ; $EE             
    db $80 + 'S',"RITE"           ; $EF             
    db $80 + 'T',"ILE"            ; $F0             
    db $80 + 'O',"FF"             ; $F1             
    db $80 + 'M',"AP"             ; $F2             
    db $80 + ' '                  ; $F3             
    db $80 + 'R',"ESUME"          ; $F4             
    db $80 + 'C',"OL"             ; $F5             
    db $80 + 'S',"CREEN"          ; $F6             
    db $80 + 'S',"ET"             ; $F7             

    db $80             ; End of table marker
    
;-----------------------------------------------------------------------------
; plusBASIC tokens
;-----------------------------------------------------------------------------
TIMETK    equ     $D3    
CDTK      equ     $E0
KEYTK     equ     $E7
ARGSTK    equ     $E8
ERRTK     equ     $E9
LINETK    equ     $D0

;;; Extended Error Message Table can go here

;-----------------------------------------------------------------------------
; Convert keyword to token - hook 10
;-----------------------------------------------------------------------------
keyword_to_token:
    ld      a, b               ; A = current index

    cp      $CB                ; If < $CB then keyword was found in BASIC table
    ld      IX,HOOK10+1        ;   CRUNCX will also return here when done
    push    IX
    ret     nz                 ;   so return
    ; Set our own keyword table and let BASIC code use that instead
    ex      de, hl             ; HL = Line buffer
    ld      de, TBLCMDS - 1    ; DE = our keyword table
    ld      b, BTOKEN - 1      ; B = our first token
    
    jp      CRUNCX             ; Continue searching using our keyword table

;-----------------------------------------------------------------------------
; Convert token to keyword - hook 22
;
; This function will check if the passed token is one of the stock BASIC or
; our extra commands. If it one of our commands, we pass our command table
; to the ROM code.
;-----------------------------------------------------------------------------
token_to_keyword:
    cp      BTOKEN              ; Is it one of our tokens?
    jr      nc, .expand_token   ; Yes, expand it
    jp      HOOK22+1            ; No, return to system for expansion

.expand_token:
    sub     BTOKEN - 1
    ld      c, a                ; C = offset to AquBASIC command
    ld      de, TBLCMDS         ; DE = table of AquBASIC command names
    jp      RESSRC              ; Print keyword indexed by C

; * = Likely to be replaced
;
;     plusBASIC   MX-BASIC             Conversion
; $CB             INSTR            IN STRING <--> INSTR  
; $CC             PUT    
; $CD GET         GET    
; $CE             DRAW   
; $CF             CIRCLE  
; $D0             LINE                compatible
; $D1             SWAP   
; $D2 paint       DOKE              POKE! <--> DOKE
; $D3 TIME        SDTM   
; $D4             EDIT   
; $D5 CLS         CLS    
; $D6 LOCATE      LOCATE 
; $D7 OUT         OUT    
; $D8 PSG         PSG    
; $D9 DEBUG*      DEBUG  
; $DA CALL        CALL   
; $DB LOAD        LOAD   
; $DC SAVE        SAVE   
; $DD DIR         DIR    
; $DE CAT         MKDIR    
; $DF DEL         DEL                  compatible
; $E0 CD          CD                   compatible
; $E1 IN          IN                   compatible
; $E2 JOY         JOY                  compatible
; $E3 HEX         HEX                  compatible
; $E4 RENAME      VER
; $E5 DATE        DTM           DATETIME$ <--> DTM$(0)
; $E6             DEC
; $E7 KEY         KEY             
; $E8 ARGS        DEEK              PEEK! <--> DEEK
; $E9 ERR         ERR [OR]            ERR <--> ERR(0), ERRLINE <--> ERR(1), ERR$ <--> ERR$(1)
; $EA             STRING
; $EB             XOR           (a XOR b) <--> XOR(a,b)
; $EC             MENU
; $ED EVAL        EVAL                 compatible
; $EE             SLEEP
; $EF SPRITE      MKDIR
; $F0 TILE        RMDIR               DEL <--> RMDIR
; $F1 OFF         OFF
; $F2 MAP         WAIT
; $F3             FILE
; $F4 RESUME      RESUME
; $F5 COL         COL [OR]
; $F6 SCREEN
; $F7 SET
; $F8
; $F9
; $FA
; $FB
; $FC
; $FD
; $FE
; $FF
