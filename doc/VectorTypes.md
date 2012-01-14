---
layout : default
title : API - The package GL.Vectors
---

# Vector types

## The package `GL.Vectors`

This package provides the `Vector` type, which is used for specifying points in the 
4-dimensional space of homogeneous coordinates. The usual operations one would expect for
a mathematical vector are provided here.

The function `Normalize` normalizes a vector so that its `W` component gets `1.0`. In the
case that the `W` component equals `0.0`, it will raise a `Constraint_Error`.

## The package `GL.Normals`

Similar to `GL.Vectors`. Normals do not include the homogeneous component `W`.

## The package `GL.Colors`

Similar to `GL.Vectors`. Color values have a range from 0.0 to 1.0. As mathematical
operations o a whole color vector do not really make sense, they are not provided here.