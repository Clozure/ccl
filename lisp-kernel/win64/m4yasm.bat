@echo OFF
rem M4 then YASM compile file, used together as external build tool in Visual Studio
set m4=m4 rem C:\msys64\usr\bin\m4
set yasm=yasm rem C:\msys64\usr\bin\yasm

if "%~1" neq "" (
    %m4% -DWIN_64 -DWINDOWS -DX86 -DX8664 -DHAVE_TLS -DEMUTLS -DTCR_IN_GPR -I . %~n1.s m4macros.m4 >  %~n1.asm
    %yasm% %~n1.asm -f win64 -g cv8 -p gas -o %~n1.obj
	del %n1.asm
) else (
    echo Usage: m4yasm.bat filename
)