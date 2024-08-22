;----------------------------------------------------
; Project: pt3standalone.zdsp
; Modified to be interrupt driven by Mack Wharton
; Main File: main.asm
;----------------------------------------------------

; Generates a binary file to be loaded into a RAM page

BaseAddr  = $C000
VarsAddr  = BaseAddr+$700
SongData  = BaseAddr+$A00

 org     BaseAddr               ; PT3 page switched into Bank 3

;Jump table
      jp      StartPlayer       ; Initialize Player
      jp      PlayQuark         ;
      jp      CHECKLP
      jp      MUTE
      jp      SetMode           
      jp      GetMode

StartPlayer:
      LD      HL,SongData
      CALL    PT3_PLAY
      or      a                 ; Return Zero set
      RET

; Interrupt call
; Sets Non-Zero when song has finished.
PlayQuark:
      ld      a,(CountDn)
      or      a
      jp      m,_play
      jr      nz,_decrement
      ld      a,6
      ld      (CountDn),a
      ret
_decrement
      dec     a
      ld      (CountDn),a
_play
      LD      HL,SongData
      CALL    PLAY        
      ld      hl,SETUP
      bit     7,(hl)            ; quit if end of song
      ret

; Input: E: 0 - 50 Hz, $FF, 60 Hz
; Clobbers: A
SetMode:
      ld      a,e 
      ld      (CountDn),a
      ret

GetMode:
      ld      e,0
      ld      a,(CountDn)
      or      a
      ret     p
      dec     e
      ret


include  "macros.i"   ; structure macros
include   PTPlayer.asm
 
      dc $VarsAddr-$,$76
