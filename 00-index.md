---
layout : default
title : Home
permalink: index.html
---

## About

**OpenGLAda** is a thick Ada binding for [OpenGL][5] and related C libraries
([GLFW][4], [FreeType][7]). It enables you to create complex
graphical applications in Ada using the OpenGL industry standard.

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
 * Cross-Platform: OpenGLAda supports macOS, Windows and X11-based operating
   systems like Linux.
 * Structured: In contrast to the raw OpenGL library, OpenGLAda's functionality
   is structured into packages, so you have a better overview over the
   functionality of the API.
 * Automatic exception handling: By default, every OpenGLAda checks after every
   OpenGL call whether an error flag has been set, and raises the corresponding
   exception if that's the case. This makes debugging easier. (You can turn off
   this feature completely at compile-time.)
 * Batteries included: Besides raw OpenGL, OpenGLAda also wraps [GLFW 3][4]
   for window creation and user input and [FreeType][7] for text rendering with
   OpenGL. It includes the [Generic Image Decoder][6] for loading image files
   into textures. These additional libraries are optional; you do not have to
   use them.

## Setting it up

The preferred way to use OpenGLAda in your project is to use [Alire][8].
OpenGLAda provides the following Alire crates:

 * `openglada`: Core
 * `openglada_glfw`: GLFW binding
 * `openglada_images`: Image loading
 * `openglada_text`: Text rendering

You can also fetch releases of these crates from the [GitHub repository][2].

## License

OpenGLAda is distributed under the terms of the [MIT License][3].

## Related Projects

OpenGLAda is not the first attempt to create an OpenGL binding for Ada. Other
bindings are:

 * [AdaOpenGL](http://adaopengl.sourceforge.net/): Probably the first
   open-source binding that has been developed. It's rather thin and has seen
   its last update in 2003. Like OpenGLAda, it includes GLFW bindings.
 * [Globe3D](http://globe3d.sourceforge.net/): A 3D engine in Ada based on
   OpenGL. The included OpenGL binding is decent and up-to-date, but also rather
   thin.
 * [Orka](https://orka-engine.netlify.app/): This project uses parts of
   OpenGLAda.

 [1]: https://github.com/flyx/OpenGLAda
 [2]: https://github.com/flyx/OpenGLAda/releases
 [3]: http://www.opensource.org/licenses/MIT
 [4]: http://www.glfw.org
 [5]: http://www.opengl.org/
 [6]: https://gen-img-dec.sourceforge.io/
 [7]: https://www.freetype.org/
 [8]: https://alire.ada.dev