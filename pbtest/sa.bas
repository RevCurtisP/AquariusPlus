100 REM Save String Array
110 DIM A$(9),B$(9)
120 A$(0)="Lorem ipsum dolor sit amet, consectetur adipiscing elit, "
121 A$(1)="sed do eiusmod tempor incididunt ut labore et dolore magna "
122 A$(2)="aliqua. Ut enim ad minim veniam, quis nostrud exercitation "
123 A$(3)="ullamco laboris nisi ut aliquip ex ea commodo consequat. "
124 A$(4)="Duis aute irure dolor in reprehenderit in voluptate velit " 
125 A$(5)="esse cillum dolore eu fugiat nulla pariatur. Excepteur sint "
126 A$(6)="occaecat cupidatat non proident, sunt in culpa qui officia "
127 A$(7)=""
128 A$(8)="deserunt mollit anim id est laborum."
130 PRINT "Saving":SAVE "sa.sta",*A$
150 PRINT "Loading":LOAD "sa.sta",*B$
160 FOR I=0 TO 9:PRINT B$(I):NEXT
