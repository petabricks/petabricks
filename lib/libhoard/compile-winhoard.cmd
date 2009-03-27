@echo off
echo Building winhoard.dll.
cl /MD /I../../heaplayers /I../../heaplayers/util /nologo /Ox /Oy /DNDEBUG /D_MT /c winhoard.cpp
cl /MD /I../../heaplayers /I../../heaplayers/util /nologo /Ox /Oy /DNDEBUG /D_MT /c usewinhoard.cpp
cl /LD winhoard.obj /link /base:0x63000000 kernel32.lib msvcrt.lib /subsystem:console /dll /incremental:no /entry:HoardDllMain
echo *****
echo Build complete.

