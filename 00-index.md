---
layout : default
title : Home
permalink: index.html
---

## About

**OpenGLAda** is a thick Ada binding for [OpenGL][5] and [GLFW][4]. It enables
you to create complex graphical applications in Ada, using the OpenGL industry
standard.

OpenGLAda is not the first attempt to create an OpenGL binding for Ada. Other
bindings are:

 * [AdaOpenGL](http://adaopengl.sourceforge.net/): Afaik the first binding that
   has been developed. It's a rather thin binding and is not quite up-to-date,
   but like OpenGLAda, it also includes GLFW bindings.
 * [Globe3D](http://globe3d.sourceforge.net/): This is not only an OpenGL
   binding, but also provides methods for rendering of 3D objects. The included
   OpenGL binding is decent and up-to-date, but also rather thin.
 * [Lumen](http://www.niestu.com/software/lumen/): Lumen aims to be an interface
   library for Ada and is currently under development. It includes an
   auto-generated, very thin OpenGL binding.

## Highlights

 * Strong typing: Thanks to extensive usage of Ada's type system, using the API
   is much less error-prone than it is in C.
 * Dynamic run-time loading of OpenGL functions: OpenGLAda will link properly
   against OpenGL 1.1 because all functions that were introduced in OpenGL 1.2
   or later are loaded at run-time. Functions will also automatically be loaded
   as ARB or EXT function if available.
 * Object-oriented design: All OpenGL objects are wrapped as tagged types. They
   are also reference-counted, so you do not need to worry about freeing memory
   yourself.
 * OS-agnostic: OpenGLAda builds on MacOSX, Windows and Linux without further
   configuration.
 * Structured: In contrast to the raw OpenGL library, OpenGLAda's functionality
   is structured into packages, so you have a better overview over the
   functionality of the API.
 * Automatic exception handling: By default, every OpenGLAda checks after every
   OpenGL call whether an error flag has been set, and raises the corresponding
   exception if that's the case. This makes debugging easier. (You can turn off
   this feature completely at compile-time.)
 * Batteries included: Besides raw OpenGL, OpenGLAda also wraps [GLFW][4]
   (with separate bindings for version 2 and 3) for window creation and user
   input, [SOIL][6] for loading images into textures, and [FTGL][7] for using
   TrueType fonts with OpenGL. These additional bindings are optional; you do
   not have to use them.

## Getting the Code

You can get the code from the [public GitHub repository][1]. Releases are
available as [tags][2] of the git repository.

## License

OpenGLAda is distributed under the terms of the [ISC License][3].

 [1]: https://github.com/flyx/OpenGLAda
 [2]: https://github.com/flyx/OpenGLAda/tags
 [3]: http://www.opensource.org/licenses/ISC
 [4]: http://www.glfw.org
 [5]: http://www.opengl.org/
 [6]: http://www.lonesock.net/soil.html
 [7]: http://ftgl.sourceforge.net/docs/html/