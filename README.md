# OpenGLAda - Yet Another OpenGL binding for Ada

## Overview

**OpenGLAda** is a thick OpenGL binding for the Ada 2005 programming language.
Unlike other, thin, bindings (see the [project's homepage][4] for a list),
OpenGLAda enriches the original API with concepts and features provided by
Ada, like object orientation, type safety and generics.

Besides standard OpenGL functionality, OpenGLAda optionally provides
bindings to the following OpenGL-related libraries:

 * [GLFW][3]: This is a library for creating windows with an OpenGL context
   on them. It also provides functionality for capturing user input on
   keyboard, mouse and joystick. Having a window with an OpenGL context
   is the prerequisite for using any OpenGL functionality. The GLFW binding
   comes in two flavors: One for GLFW 2.x and one for GLFW 3+. There are
   significant differences between these two, the most prominent being that
   GLFW 3 can handle multiple windows. You can set the desired GLFW version
   for the binding at compile time.
 * [SOIL][10]: The *Simple OpenGL Image Library*. This is a very tiny library
   for loading image files into OpenGL textures. It is public domain. Because
   it's so tiny, it is linked directly into OpenGLAda. Its source is included
   in the OpenGLAda sources.
 * [FTGL][11]: A library built on top of FreeType that provides an API to
   load TrueType fonts and render text with OpenGL. The Ada wrapper only
   provides basic functionality to load fonts and render text. As it does not
   include a wrapper to FreeType, the more low-level functionality has been
   excluded, it may be available as part of a more complete wrapper included
   in [Lumen][12] in the future.
   
OpenGLAda supports MacOSX, Windows and X11-based systems.

## Prerequisites

In order to build OpenGLAda, you need to have:

 * A GNAT compiler¹. The GNAT/GCC binary release from [GnuAda][12] is known to
   work as of November 2016 and is preferable if you do not fancy a GPL'd
   runtime. A GPL'd version is available on [AdaCore's Libre Site][1],
   though it is known to have issues at least on OSX 10.11². More information
   is available on the [GCC website][5].
 * [GPRBuild][2] (is bundled with AdaCore's GNAT distribution). Minimum
   supported version is the one that comes with GNAT GPL 2012 (20120509). Do
   not use `gnatmake` to build the project files, it won't work.
 * An OpenGL implementation (usually comes bundled with your graphics driver)
 * Optionally [GLFW][3]
 * Optionally [FTGL][11]

¹: You may also be able to build OpenGLAda with another Ada compiler and/or
without using the `*.gpr` files. You just have to import the sources to your
project and whichever build system you are using. I never used other Ada
compilers apart from GNAT, so if I accidentally used some GNAT-specific features
in the code, please drop me a message.

²: With GNAT GPL 2016, runtime loading of OpenGL function pointers does not
seem to work on OSX 10.11, see [this bug][13]. It is unknown whether this
affects other OSX versions or operation systems. The exact cause for this error
is unknown. Therefore, using GNAT/GCC is currently strongly recommended.

## Compilation

A Makefile is provided mainly for building the tests:

    $ make tests

If you're on Windows and do not have the `make` utility available, you can build
the test by executing

    $ gprbuild -P glfw_test.gpr   -XWindowing_System=windows
    $ gprbuild -P opengl_test.gpr -XWindowing_System=windows

The tests require GLFW, because they need to create windows. By default, they
try to link against GLFW 3+. You can instead build the tests against GLFW 2.x
by executing:

    $ gprbuild -P opengl_test.gpr -XWindowing_System=windows -XGLFW_Version=2
    $ gprbuild -P glfw_test.gpr   -XWindowing_System=windows -XGLFW_Version=2

(Substitute `windows` with `x11` or `quartz` if needed.)

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

If you're using the GPRBuild system, you can then just declare using OpenGLAda
in your *.gpr file:

    with "dependencies/OpenGLAda/opengl";

Alternatively, you can specify the path to the OpenGL project file in as
environment variable:

    export ADA_PROJECT_PATH=dependencies/OpenGLAda

... and then specify the dependency without the path:

    with "opengl";

If you want to use GLFW, you also need to refer to the project `opengl-glfw.gpr`
instead (it automatically adds a dependency to the `opengl` project).

The project files `opengl.gpr` and `opengl-glfw.gpr` take the following
scenario parameters:

 * `Windowing_System`: Sets the backend windowing system. Used for GLFW and also
                       for system-dependent parts of the API (GLX, WGL, CGL):
    
    - `x11`: X Windowing System (Linux, BSD, etc)
    - `windows`: Microsoft Windows
    - `quartz`: Quartz Compositor (OS X)
    
 * `mode`: May take one of the following values:
 
    - `debug` (default): Compile the project with debugging symbols and without
               optimization.
    - `release`: Compile the project for a release environment.
    
 * `GLFW_Version` (GLFW only): Sets the version of the GLFW library to link
                               against. See [here][6] for a detailed comparison
                               of the two API versions.
    
    - `2`: GLFW 2.x. Only one window.
    - `3`: GLFW 3+. Multiple windows, multiple monitor support, etc.

 * `Auto_Exceptions`: Configures exception handling:

    - `enabled` (default): After each call to OpenGL, OpenGLAda checks whether
      OpenGL has set an error flag and if it had, raises the corresponding
      exception.
    - `disabled`: The user has to query the error flag on his own.

### With other build systems

You can add the OpenGLAda sources to your code, then use whatever build system
you want. Just make sure that you link properly against your OpenGL
implementation:

 * OS X: `-framework OpenGL -framework CoreFoundation`
 * Windows: `-lOpenGL32 -lGdi32`
 * X11-based (Linux, BSD, etc): `-lGL -lX11`

If you're using GLFW, add `-lglfw` for GLFW 2 or `-lglfw3` for GLFW 3. If you're
on Windows and link against GLFW as dynamic library, you also need to add
`-lwinmm`. If you're using FTGL, add `-lftgl`.

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

## License

OpenGLAda is distributed under the terms of the [ISC License][7]. The Ada 2012
logo that is used in the SOIL tests is distributed under the terms of the
[CC BY-ND 3.0][8] license, the original author is [AdaCore][9].

 [1]: http://libre.adacore.com/
 [2]: http://www.adacore.com/gnatpro/toolsuite/gprbuild/
 [3]: http://www.glfw.org/
 [4]: http://flyx.github.io/OpenGLAda/
 [5]: http://gcc.gnu.org/wiki/GNAT
 [6]: http://www.glfw.org/docs/3.0/moving.html
 [7]: http://opensource.org/licenses/ISC
 [8]: http://creativecommons.org/licenses/by-nd/3.0/deed.en_GB
 [9]: http://www.ada2012.org/#the_logo
 [10]: http://www.lonesock.net/soil.html
 [11]: https://sourceforge.net/projects/ftgl/
 [12]: https://sourceforge.net/projects/gnuada/files/
 [13]: https://github.com/flyx/OpenGLAda/issues/15
