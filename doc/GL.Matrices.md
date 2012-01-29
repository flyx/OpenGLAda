---
layout : default
title : API - The package GL.Matrices
packages :
  - GL.Matrices
versions :
  - 2.1
weight: 3
---

# The package `GL.Matrices`

This package provides the type `Matrix` which represents a 4x4 Matrix. The usual
mathematical operators on `Matrix` and `Vector` types are also defined. These operators
are implemented in OpenGLAda and will not call the OpenGL API.

Additionally, access to OpenGL's `Matrix_Stack`s is provided. These are deprecated as
remains of the deprecated fixed pixel pipeline. They may be useful for beginners. The four
available matrix stacks are provided as constants. There is no possibility to create
custom matrix stacks, as they are hardcoded into OpenGL.