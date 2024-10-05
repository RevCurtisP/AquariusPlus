; Hello World Aquarius+ Executable

print_string_immd   equ  $210F

load_addr equ $4000
exec_addr equ $4000

    org   load_addr - 16
; Header
   byte   "AQPLUSEXEC"
   word   load_addr
   word   prog_end - load_addr
   word   exec_addr
; Program   
   call   print_string_immd
   byte   "Hello world",0
   ret
prog_end:   
