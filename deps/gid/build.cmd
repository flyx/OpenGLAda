gprbuild -p -P gid -XBuild_Mode=Debug to_bmp
copy test\to_bmp.exe test\tb.exe
gprbuild -p -P gid -XBuild_Mode=Fast
gprbuild -p -P gid -XBuild_Mode=Smallest mini
