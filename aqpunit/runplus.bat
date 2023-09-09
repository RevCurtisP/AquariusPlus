@echo RUN "/aqpunit/%1.BAS" > runplus.out
@curl --data-binary @runplus.out http://aqplus/keyboard
@del runplus.out
