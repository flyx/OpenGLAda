gcc -S -gnatpn -O2 -I.. to_bmp.adb
ren to_bmp.s to_bmp_O2.s

gcc -S -gnatpn -O0 -I.. to_bmp.adb
ren to_bmp.s to_bmp_O0.s

del to_bmp.ali
