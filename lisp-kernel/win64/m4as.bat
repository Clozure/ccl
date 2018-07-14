@echo OFF
rem M4 then YASM compile file, used together as external build tool in Visual Studio
set m4=C:\msys64\usr\bin\m4
set as=C:\msys64\mingw64\bin\as

if "%~1" neq "" (
    %m4% -DWIN_64 -DWINDOWS -DX86 -DX8664 -DHAVE_TLS -DEMUTLS -DTCR_IN_GPR -I .. %~1 m4macros.m4 >  %~1.asm
    %as% %~1.asm -g --64 -o %~n1.obj
) else (
    echo Usage: m4yasm.bat filename
)