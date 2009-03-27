@echo off
echo Building hoarddetours.dll.
cl /I../../heaplayers /LD /MD /DNDEBUG /Ox /G6 /I./detours hoarddetours.cpp  detours\detours.lib /link /subsystem:console /entry:_DllMainCRTStartup@12 /force:multiple
 
