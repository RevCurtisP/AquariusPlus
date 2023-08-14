;-----------------------------------------------------------------
;               Print Null-terminated String
;-----------------------------------------------------------------
;  in: HL = text ending with NULL
;
print_string:
   ld   a,(hl)
   inc  hl
   or   a
   ret  z
   call TTYOUT
   jr   print_string

;--------------------------
;   print hex word
;--------------------------
; in: DE = word
;
print_word:
    ld      a,d
    call    print_hex
    ld      a,a

;--------------------------
;   print hex byte
;--------------------------
; in: A = byte

print_hex:
    push    bc
    ld      b,a
    and     $f0
    rra
    rra
    rra
    rra
    cp      10
    jr      c,.hi_nib
    add     7
.hi_nib:
    add     '0'
    call    TTYOUT
    ld      a,b
    and     $0f
    cp      10
    jr      c,.low_nib
    add     7
.low_nib:
    add     '0'
    pop     bc
    jp      TTYOUT

