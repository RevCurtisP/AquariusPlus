@echo %~1 > sendkeys.out
@curl --data-binary @sendkeys.out http://aqplus/keyboard
@del sendkeys.out
