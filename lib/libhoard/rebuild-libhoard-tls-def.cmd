: We need to execute this if we change libhoard-tls.cpp.
@echo off
cl /I../../heaplayers /c /MD /DNDEBUG /Ox /Zp8 /Oa /G7 /Oy libhoard-tls.cpp
nm -g libhoard-tls.obj > @@@.@@@ 2>NUL
grep ' T ?' @@@.@@@ > @@@.@@1 2>NUL
grep ' T \_' @@@.@@@ > @@@.@@2 2>NUL
echo EXPORTS > libhoard-tls.def
sed 's/.* T //' @@@.@@1 | grep -v DllMain >> libhoard-tls.def 2>NUL
sed 's/.* T \_//' @@@.@@2 | grep -v DllMain >> libhoard-tls.def 2>NUL
erase @@@.@@@
erase @@@.@@1
erase @@@.@@2
 
