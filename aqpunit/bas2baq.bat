copy /B %1.bas+aqpunit.bas _prog.bas
python ..\util\bas2baq.py _prog.bas %1.baq
del _prog.bas
