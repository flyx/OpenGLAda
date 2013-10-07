---
layout : default
title : API - Setting Shader Input Values
packages :
  - GL.Attributes
  - GL.Uniforms
weight : 10
---

# Setting Shader Input Values

Shaders have two kinds of input parameters: Uniform values and attribute values. Attribute
values are the same as vertices, normals or colors in the fixed pixel pipeline, i.e.
their value varies with every call. They are only available to the vertex shader.
Uniform values are available to both the vertex and the fragment shader. They may be
stuff like transformation matrices or lighting configuration.

OpenGLAda provides setters for these parameters with the packages `GL.Attributes` and
`GL.Uniforms`. Uniform values are always set directly. Attribute values can be set
directly too, but they can also be read from a vertex buffer object by means of a
vertex array. This method is not described in detail here - you can look it up in the
OpenGL documentation or have a look at the example code in OpenGLAda.

The setter procedures are overloaded by input type. For each input type, you can set
a single value, a vector, and for uniform values also a vector array, a matrix or a
matrix array. Note that when you have a matrix as attribute parameter, you need to fill
its rows one by one with vectors. Consult the OpenGL documentation for further
information.