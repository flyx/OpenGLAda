---
layout: default
title: API Documentation
permalink: api.html
---

# API Documentation

## Introduction

OpenGLAda provides an API that aims to be similar to the standard OpenGL API you
might be familiar with. As a thick binding, the library introduces some additional
concepts and features. On this page, you find some general information on the structure
and concepts of the OpenGLAda API. The documentation of the packages
provided by OpenGLAda can be accessed from the navigation menu to the left.

This is not a standalone documentation. For a detailed description of the behaviour of
OpenGL, you have to consult the
[OpenGL Reference Pages](http://www.opengl.org/sdk/docs/man/). This documentation only
describes how OpenGL features are wrapped by OpenGLAda.

You can discover where a specific OpenGL function is available in OpenGLAda by searching it in the [mapping index][2].

## OpenGL Versions

OpenGLAda aims to provide the feature set of OpenGL 4.5, but is not complete
yet as the main developer has few time for maintaining this library. Features
are being added when requested. Refer to the mapping table to check whether a
feature you need is wrapped by OpenGLAda.

OpenGL 3.0 has deprecated a lot of functionality that was part of earlier versions.
OpenGLAda provides some of this older functionality that seems to be important enough
to be inlcuded, but not all of it is available. If you discover that some
OpenGL 2.1 functionality isn't available in OpenGLAda, you should look for the
alternatives OpenGL 3.0 offers.

## Types

Some API entry points in OpenGL are provided for a number of different numeric types, like
`GLbyte`, `GLshort`, `GLint`, `GLuint`, `GLfloat`, `GLdouble` and so on. The incoming
types often make no difference to OpenGL. The values will be converted to whatever
format OpenGL uses internally (this may depend on the implementation). These entry
points may not be wrapped for all available types in OpenGLAda. For example, color
values are always single-precision floating point values. Other types, like vectors, are
provided for multiple base types.

Keep in mind that the size and type of values passed to OpenGL only matters on the
OpenGL side if you directly pass them to shaders or write them into buffers. As mentioned
above, other calls will convert the values to the internally used type.

## Exception Handling

OpenGL provides a polling mechanism for discovering errors, i.e. you have to ask OpenGL
whether an error has occured. OpenGLAda can do this automatically in most calls that are
relayed to OpenGL. If this feature is enabled, an appropriate exception is raised any time
OpenGL sets an error flag. You can disable this behavior by setting the scenario variable
`Auto_Exceptions` to `disabled` when compiling OpenGLAda.

## Runtime Binding

All features that were introduced in OpenGL 1.2 or later will not be expected to
be available. OpenGLAda queries the neccessary function pointers at runtime for
all wrapped OpenGL functions. You should make sure the functionality you want to
use is available before calling it, because if it is not, you will get a
`Constraint_Error`. The querying will be done once by calling `GL.Init`.

The call to `GL.Init` should be made after an OpenGL context has been created
and has been made current. Note that the `Glfw` package does this automatically
when you create a Window so typically, you do not have to worry about it.

## Getting Started

If you want to see OpenGLAda in action, check out the [OpenGLAda examples][1].
They give you an idea of how to use the API.
In particular, the examples under `superbible` may be interesting since they
are taken from http://www.openglsuperbible.com which teaches the reader to
use modern OpenGL 4.5 features.

 [1]: https://github.com/flyx/OpenGLAda-examples
 [2]: /mapping.html