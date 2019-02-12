@echo off
call win64\m4as.bat pad.s

call win64\m4as.bat x86-spentry64.s

call win64\m4as.bat x86-spjump64.s

call win64\m4as.bat x86-subprims64.s

call win64\m4as.bat imports.s

call win64\m4as.bat x86-asmutils64.s
