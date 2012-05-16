---
layout : default
title : API - The Package GL
packages :
  - GL
weight: 2
---

# The package `GL`

This is the root package of the core OpenGL wrapper.

The index types for vector and matrix types are defined here because they would cause
problems with cyclic dependencies when they were defined in `GL.Types`.

With `Toggle_Error_Checking`, you can enable or disable the checking for OpenGL errors.
When enabled, every call to OpenGL will be succeeded by a check whether an error has
occured. If that's the case, the corresponding OpenGLAda exception will be raised.
Disabling it may increase the performance when you do a lot of API calls.

`Flush` and `Finish` act like their conterparts in the OpenGL API.