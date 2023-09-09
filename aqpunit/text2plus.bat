copy /B %1.bas.txt+aqpunit.bas.txt _prog.txt
python ..\util\text2plus.py _prog.txt %1.bas
del _prog.txt
