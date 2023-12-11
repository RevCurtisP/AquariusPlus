;----------------------------------------------------
; Project: pt3standalone.zdsp
; Modified to be interrupt driven by Mack Wharton
; Main File: main.asm
;----------------------------------------------------

; Generates a binary file to be included in the plusBASIC ROM

SongData  = $4400               ; (For now)

 org     0xD800                 ; Last 2k of Auxilliary ROM

;Jump table
      jp      StartPlayer       ; Initialize Player
      jp      PlayQuark         ;
      jp      CHECKLP
      jp      MUTE

StartPlayer:
      LD      HL,SongData
      CALL    PT3_PLAY
      or      a                 ; Return Zero set
      RET

; Interrupt call
; Sets Non-Zero when song has finished.
PlayQuark:
      LD      HL,SongData
      CALL    PLAY        
      ld      hl,SETUP
      bit     7,(hl)            ; quit if end of song
      ret
      jr      nz,StartPlayer
      RET




include  "macros.i"   ; structure macros
include   PTPlayer.asm
 
      dc $E000-$,$76