# OpenGLAda - Yet Another OpenGL binding for Ada

## Overview

**OpenGLAda** is a fat OpenGL binding for the Ada programming language.
Unlike other bindings (see the [project's homepage][4] for a list),
OpenGLAda enriches the original API with concepts and features provided by
Ada, like object orientation, type safety and generics.

In addition to the core OpenGL API, a wrapper for GLFW is also
included. GLFW is a third-party library that provides functions to create a window
or a fullscreen display with an OpenGL context on it. GLFW also includes functions
for basic input handling.

OpenGLAda supports MacOSX, Windows and Linux with X11. It may also work
on other X11-based operating systems.

## Prerequisites

In order to build OpenGLAda, you need to have:

 * A GNAT compiler (a GPL'd version is available on [AdaCore's Libre Site][1])
 * [GPRBuild][2] (is bundled with AdaCore's GNAT distribution). Minimum supported
   version is the one that comes with GNAT GPL 2012 (20120509). Do not use
   `gnatmake` to build the project files, it won't work.
 * An OpenGL implementation (usually comes bundled with your graphics driver)
 * Optionally [GLFW][3]

_Note_: You may also be able to build OpenGLAda with another Ada compiler and/or
without using the *.gpr files. You just have to import the sources to your project
and whichever build system you are using.

## Compilation

A Makefile is provided mainly for building the tests:

    $ make tests

If you're on Windows and do not have the `make` utility available, you can build
the test by executing

    $ gprbuild -P glfw_test.gpr   -XGL_Backend=Windows
    $ gprbuild -P opengl_test.gpr -XGL_Backend=Windows

## Using OpenGLAda in your project

### With GPRBuild

The easiest way to use OpenGLAda in your project is to just copy the sources
in some dependency folder within your project folder, e.g.:

    YourProject
     |
     |-dependencies
     |  |
     |  +-OpenGLAda
     |
     |-your
     |-project
     |-files
     +-...

If you're using the GPRBuild system, you can then just declare using OpenGLAda in
your *.gpr file:

    with "dependencies/OpenGLAda/opengl";

Alternatively, you can specify the path to the OpenGL project file in as environment
variable:

    export ADA_PROJECT_PATH=dependencies/OpenGLAda

... and then specify the dependency without the path:

    with "opengl";

If you want to use GLFW, you also need to refer to the project  `glfw.gpr` in your project file
the same way as you referred to `opengl.gpr`.

The project files `opengl.gpr` and `glfw.gpr` take the following scenario parameters:

 * `GL_Backend`: May take one of the following values: `linux`, `windows`, `mac`.
   Default is `linux`.
 * `mode`: May take one of the following values: `debug`, `release`. Default is `debug`.

### With other build systems

You can add the OpenGLAda sources to your code, then use whatever build system
you want. Just make sure that you link properly to your OpenGL implementation:

 * OS X: `-framework OpenGL -framework CoreFoundation`
 * Windows: `-lOpenGL32 -lGdi32`
 * Linux / X11-based: `-lGL -lX11`

If you're using GLFW, add `-lglfw`. If you're on Windows and link against GLFW
as dynamic library, you also need to add `-lwinmm`.

## Installation

OpenGLAda is not designed to be installed as a standalone library. The reasoning
behind this is that OpenGLAda is a wrapper library that doesn't provide much
functionality on its own. Therefore, maintaining a library installing routine
does not seem worth the effort - even less as it would be expected to support
multiple platforms.

## Tests

As mentioned, OpenGLAda contains some tests. You can also see them as examples
that demonstrate the basic usage of the API. After building them as described
above, you can execute them in the `bin` directory. Some tests load shader
files from the source directory by using relative paths, so they only work with
`bin` as working directory.



For additional information and documentation, see the
[project's homepage][4].



 [1]: http://libre.adacore.com/
 [2]: http://www.adacore.com/gnatpro/toolsuite/gprbuild/
 [3]: http://www.glfw.org/
 [4]: http://flyx.github.io/OpenGLAda/
