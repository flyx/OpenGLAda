echo Convert large high-res images from the NASA  

copy nasa4.jpg r:
copy nasa8.jpg r:
copy nj*.exe r:
copy to_bmp.exe r:

r:
del *.dib
del *.ppm

to_bmp - nasa*.jpg

for /l %%i in (1,1,7) do timeit -s -f c:gid.dat -k jpg_rev91 to_bmp.exe nasa*.jpg


rem for /l %%i in (1,1,7) do timeit -f gid.dat -k png001 to_bmp.exe - nasa*.png
rem for /l %%i in (1,1,7) do timeit -f c:gid.dat -k nj_0_1.1     nj0.exe    nasa6.jpg
rem for /l %%i in (1,1,7) do timeit -f c:gid.dat -k nj_bicub_1.1 nj.exe     nasa6.jpg
rem for /l %%i in (1,1,8) do timeit -s -f c:gid.dat -k nj0.no_out     nj0.no_out.exe    nasa7.jpg

c:

timeit -f gid.dat

pause
