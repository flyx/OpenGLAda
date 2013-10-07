---
layout : default
title : API - Simple OpenGL Image Loader
packages : SOIL
weight: 13
api: true
---

# The Simple OpenGL Image Loader (SOIL)

This wrapper enables you read image files in various formats and load them
into OpenGL textures. See [the SOIL website][1] for a list of supported
image formats.

You can either load the file directly into a texture with the loader
functions in `SOIL` or load them into an opaque image object and later create
a texture from that, using `SOIL.Images`.

The SOIL library is directly linked into OpenGLAda if you're using this
optional extension, so it won't require an external installation of the
library.

 [1]: http://www.lonesock.net/soil.html