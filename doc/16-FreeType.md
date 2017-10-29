---
layout : default
title : API - FreeType
packages : FT
api : true
permalink: ft.html
---

# The FreeType binding

The package `FT` and its subpackages provide a wrapper for the [FreeType][1]
library. You can use it to transform characters from a TrueType / OpenType font
file into textures, which can then be used for rendering text.

Like OpenGLAda's object system, the types in `FT` are reference-counted so you
do not have to worry about manual deallocation. An example of the usage of `FT`
is the package `GL.Text` which automatically generates textures that represent
longer Strings.

This binding requires the FreeType library on your system.

 [1]: https://freetype.org/