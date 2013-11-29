:: This is a batch file because I happen to be on Windows
:: but it could easily be ported to a Linux shell script
:: or build task

@echo off


mkdir "slim" 2>nul

echo Building FAST slim version...
haxe PNGEncoder2.hxml -D FAST_ONLY
copy /Y PNGEncoder2.swc slim\PNGEncoder2_fast.swc >nul
haxe PNGEncoder2.hxml -D FAST_ONLY -D DECODER
copy /Y PNGEncoder2.swc slim\PNGEncoder2_fast_with_decoder.swc >nul

echo Building NORMAL slim version...
haxe PNGEncoder2.hxml -D NORMAL_ONLY
copy /Y PNGEncoder2.swc slim\PNGEncoder2_normal.swc >nul
haxe PNGEncoder2.hxml -D NORMAL_ONLY -D DECODER
copy /Y PNGEncoder2.swc slim\PNGEncoder2_normal_with_decoder.swc >nul

echo Building GOOD slim version...
haxe PNGEncoder2.hxml -D GOOD_ONLY
copy /Y PNGEncoder2.swc slim\PNGEncoder2_good.swc >nul
haxe PNGEncoder2.hxml -D GOOD_ONLY -D DECODER
copy /Y PNGEncoder2.swc slim\PNGEncoder2_good_with_decoder.swc >nul

echo Building full version...
haxe PNGEncoder2.hxml -D DECODER

rem pause
