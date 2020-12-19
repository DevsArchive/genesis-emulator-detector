@echo off

set OUTPUT=emudetect.md

..\bin\asm68k /m /p /o ae- ..\md\main.asm,%OUTPUT%,,list.lst
echo.
..\bin\rompad %OUTPUT% 255 0
..\bin\fixheadr %OUTPUT%

pause