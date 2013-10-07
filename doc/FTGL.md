---
layout : default
title : API - FTGL
packages : FTGL
weight: 14
api : true
---

# The FTGL binding

FTGL is a small library for rendering text in OpenGL. It uses FreeType2 and
is able to use TrueType fonts. This binding is not complete, because a
complete binding would also need to provide parts of the FreeType2 API.

Currently, there is only the package `FTGL.Fonts` available, which enables
you to load fonts and render text with them. Handling of single glyphs has
not been wrapped, nor have the layouting features. While writing this binding,
I learned that a more complete port of FTGL will be available soon as part
of the [Lumen][1] library, so I stopped the work on this binding to avoid
doing things twice.

This binding does need to have the FTGL library available in order to
compile.

 [1]: http://www.niestu.com/software/lumen/