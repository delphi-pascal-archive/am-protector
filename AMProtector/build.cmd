del protector.dproj
del protector.identcache
del *.dcu
del lzma\*.dcu
del rnd.inc
del protector.exe
del catcher.exe
del makernd.exe
dcc32 -B -$C- -$D- -$L- -$M- -$O+ -$Q- -$R- -$U- -$W- -$Z1 -$Y- makernd.dpr > makernd.txt
makernd
del upx.res
brcc32 -32 upx.rc
dcc32 -B -DFACTORY -DDBG -$C- -$D- -$L- -$M- -$O+ -$Q- -$R- -$U- -$W- -$Z1 -$Y- protector.dpr > protector.txt


del protector.dproj
del protector.identcache
copy protector.dpr catcher.dpr
del *.dcu
del lzma\*.dcu
dcc32 -B -DCATCHER -$C- -$D- -$L- -$M- -$O+ -$Q- -$R- -$U- -$W- -$Z1 -$Y- catcher.dpr > catcher.txt


del *.dcu
del lzma\*.dcu