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
   excluded.

OpenGLAda supports MacOSX, Windows and X11-based systems. API documentation can
be found on the [project's homepage][4].

## Prerequisites

In order to build OpenGLAda, you need to have:

 * A GNAT compiler¹. Compilers known to work well with OpenGLAda are
   [GnuAda][12], [AdaCore GNAT GPL 2017][1], and [TDM-GCC][17]. More information
   is available on the [GCC website][5].
 * [GPRBuild][2] (is bundled with AdaCore's GNAT distribution). TDM-GCC users
   can get it from [here][[16] (**NOTE: The gprbuild bundled in this zip is
   known not to work. For the time being, if you're using TDM-GCC, replace
   gprbuild with gnatmake in all commands even though that will tell you that
   project support in gnatmake is deprecated and will soon be removed.** The
   reason for this incompatibility has not been found yet).
 * An OpenGL implementation (usually comes bundled with your graphics driver)
 * Optionally [GLFW][3] (OpenGLAda is pretty useless without the ability to
   create an OpenGL context.)
 * Optionally [FTGL][11]

¹: You may also be able to build OpenGLAda with another Ada compiler and/or
without using the `*.gpr` files. You just have to import the sources to your
project and whichever build system you are using. I never used other Ada
compilers apart from GNAT, so if I accidentally used some GNAT-specific features
in the code, please drop me a message.

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

## Detailed Installation & Compilation Instructions

Each section has in its title the major version numbers of the environment in
which it has been tested. Chances are that the instructions also work with other
versions (e.g. on Windows 7 instead of Windows 10).

### Windows 10 / GNAT GPL 2017 / GLFW 3

 * Download and install GNAT GPL 2017 Windows x86 from
   [AdaCore's Libre Site][1]. We assume you install it to `C:\GNAT\2017`. Make
   sure the `bin` folder is in the `PATH` of your shell.
 * Download *32-bit Windows binaries* from the [GLFW download section][14]
   (tested with GLFW 3.2.1).
 * Open the zip folder and copy the file `glfw3.dll` from the `lib-mingw`
   folder into `C:\GNAT\2017\lib`.
 * Open your shell and navigate to the root folder of OpenGLAda. Execute:

        gprbuild -p -P opengl_test.gpr -XWindowing_System=windows -XGLFW_Version=3

   This should produce executables in the `bin` folder inside OpenGLAda.
 * You can now either add `C:\TDM-GCC-64\lib` to your `PATH` or copy the
   `glfw3.dll` from there to the `bin` folder. One of those two options is
   necessary so that the binaries can find the dll.
 * Some of the test binaries require to be launched from the `bin` folder as
   working directory because they are loading shader files from hardcoded paths.
 * Keep in mind that you need to spread the `glfw3.dll` alongside your binaries
   for them to work.

### Windows 10 / TDM-GCC 64bit / GLFW 3

These instructions are for building 64bit binaries on Windows.

 * Download and install the 64bit TDM-GCC compiler from [here][15]. In the
   installation wizard, make sure to check the (by default unchecked) *ada*
   option. Make sure the `bin` folder is in the `PATH` of your shell.

<!-- These currently do not work – uncomment when fixed

 * Download `gpr-tools.zip` from [here][16] and extract its contents to the
   folder where you installed TDM-GCC (e.g. `C:\TDM-GCC-64\`).

-->

 * Download the *64-bit Windows binaries* from the [GLFW download section][14]
   (tested with GLFW 3.2.1).
 * Open the zip folder and copy the file `glfw3.dll` from the `lib-mingw-w64`
   folder into `C:\TDM-GCC-64\lib`.
 * Open your shell and navigate to the root folder of OpenGLAda. Execute:

        gnatmake -p -P opengl_test.gpr -XWindowing_System=windows =XGLFW_Version=3

   This should produce executables in the `bin` folder inside OpenGLAda.
 * You can now either add `C:\TDM-GCC-64\lib` to your `PATH` or copy the
   `glfw3.dll` from there to the `bin` folder. One of those two options is
   necessary so that the binary can find the dll.
 * Some of the test binaries require to be launched from the `bin` folder as
   working directory because they are loading shader files from hardcoded paths.
 * Keep in mind that you need to spread the `glfw3.dll` alongside your binaries
   for them to work.

### macOS High Sierra / GNAT GPL 2017 / GLFW 3

 * Download and install GNAT GPL 2017 x86_64 Mac OS X from
   [AdaCore's Libre Site][1]. We assume you install it to `/usr/local/gnat`. Make
   sure the `bin` folder is in the `PATH` of your shell.
 * There are multiple ways to install `glfw`; the most popular one is probably
   using [Homebrew][18] and executing `brew install glfw`.
 * Open your shell and navigate to the root folder of OpenGLAda. Execute:

        gprbuild -p -P opengl_test.gpr -XWindowing_System=quartz -XGLFW_Version=3

   This should produce executables in the `bin` folder inside OpenGLAda.
 * Some of the test binaries require to be launched from the `bin` folder as
   working directory because they are loading shader files from hardcoded paths.
 * Keep in mind that you need to spread the `glfw3.dll` alongside your binaries
   for them to work.

### Linux

Please refer to your package manager documentation. Most Linux distributions
provide packages for GNAT and GLFW. On Debian, you need to install `gprbuild` as
separate package.

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

    export GPR_PROJECT_PATH=dependencies/OpenGLAda

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

If you want to use another build system, take a look at the `.gpr` files bundled
with OpenGLAda, they tell you which compiler and linker options you need to give
in order to compile it.

## Installation

OpenGLAda is not designed to be installed as a standalone library. The reasoning
behind this is that OpenGLAda is a wrapper library that doesn't provide much
functionality on its own. Therefore, maintaining a library installing routine
does not seem worth the effort - even less as it would be expected to support
multiple platforms.

## Examples

OpenGLAda comes bundled with a lot of examples. They have mostly been translated
from C examples from OpenGL textbooks. They are located inside the `examples`
folder. Each example has a separate `.gpr` file with which it can be built.

Contributions of examples are always welcome.

## Developer Documentation

OpenGLAda autogenerates its API binding to OpenGL. The autogenerated source
files are those in `src/gl/generated` and they are generated from the `*.spec`
files in `src/gl/specs`. The syntax of the spec files is similar to Ada.

The reason behind this is that all functionality newer than OpenGL 1.1 is not
expected to be provided by the OpenGL implementation. Instead, function pointers
to the implementations should be queried at runtime. This makes it possible for
the user to provide a fallback in case some OpenGL functionality is not
available on the target system.

The `generate` tool compiled from `src/generator` will take care of creating
both the API imports from OpenGL and the code for loading the function pointers
at runtime. The tool is only necessary when adding OpenGL API functions to
OpenGLAda and thus not of interest to the general user, since the autogenerated
files are checked in to version control.

If you change the `*.spec` files, running `make generate` afterwards will
update the autogenerated files. Be sure to check them in along with the spec
changes.

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
 [14]: http://www.glfw.org/download.html
 [15]: http://tdm-gcc.tdragon.net/download
 [16]: http://getadanow.com/#get_windows
 [17]: http://tdm-gcc.tdragon.net/
 [18]: https://brew.sh