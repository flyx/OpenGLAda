---
layout : default
title : API - The package GL.Types
packages : GL.Types
weight: 3
---

# The package `GL.Types`

This package defines most of the basic types used in OpenGL. Note that the
single-precision floating point type is named `Single` to avoid collission with Ada's
`Float` type.

For each type, an instance of `GL.Algebra` is available. These instances provide
vector and matrix types for that base type. The usual mathematical operations on
vectors and matrices are also available. The vectors and matrices use the indexes
`X`, `Y`, `Z` and `W`. The `W` value is used in the homogeneous coordinate system.