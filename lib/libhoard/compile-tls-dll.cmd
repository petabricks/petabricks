@echo off
echo Building libhoard.dll and libhoard.lib (ignore linker warnings).
cl /I../../heaplayers /c /MD /DNDEBUG /Ox /Zp8 /Oa /G7 /Oy libhoard-tls.cpp
cl /LD libhoard-tls.obj /o libhoard-tls.dll /Ox /link /def:libhoard-tls.def /force:multiple /subsystem:console /entry:_DllMainCRTStartup@12
 
