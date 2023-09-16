@echo RUN "/plusdemo/%1.baq" > runbaq.out
@curl --data-binary @runbaq.out http://aqplus/keyboard
@del runbaq.out
