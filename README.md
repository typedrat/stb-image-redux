# stb-image-redux
[![Build Status](https://travis-ci.org/SASinestro/stb-image-redux.svg?branch=master)](https://travis-ci.org/SASinestro/stb-image-redux) [![Hackage Version](http://img.shields.io/hackage/v/stb-image-redux.svg)](http://hackage.haskell.org/package/stb-image-redux)

A Haskell library for loading and storing images, based on the simple [stb](https://github.com/nothings/stb) single-file C libraries.

The same caveats given in `stb_image.h` obviously still apply to this wrapper, namely:
- no 16-bit-per-channel PNG
- no 12-bit-per-channel JPEG
- no JPEGs with arithmetic coding
- no 1-bit BMP

Formats that can be read:
- JPEG baseline & progressive (12 bpc/arithmetic not supported, same as stock IJG lib)
- PNG 1/2/4/8-bit-per-channel (16 bpc not supported)
- TGA (not sure what subset, if a subset)
- BMP non-1bpp, non-RLE
- PSD (composited view only, no extra channels, 8/16 bit-per-channel)
- GIF
- HDR (radiance rgbE format)
- PIC (Softimage PIC)
- PNM (PPM and PGM binary only)

Formats that can be written:
- PNG (suboptimal, 20-50% larger than a 'reasonably optimised' implementation)
- BMP
- TGA

`stb_image.h` and `stb_image_write.h` are written by Sean T. Barrett and are used under the following license:
>This software is dual-licensed to the public domain and under the following license: you are granted a perpetual, irrevocable license to copy, modify, publish, and distribute this file as you see fit.
