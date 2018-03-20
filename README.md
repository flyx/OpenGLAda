# OpenGLAda – OpenGL binding for Ada

## Overview

**OpenGLAda** is a thick OpenGL binding for the Ada 2005 programming language.
Unlike other, thin, bindings (see the [project's homepage][4] for a list),
OpenGLAda enriches the original API with concepts and features provided by
Ada, like object orientation, type safety and generics.

Besides standard OpenGL functionality, OpenGLAda optionally provides
bindings to the following OpenGL-related libraries:

 * [GLFW][3] (`opengl-glfw.gpr`): This is a library for creating windows with an
   OpenGL context on them. It also provides functionality for capturing user
   input on keyboard, mouse and joystick. Having a window with an OpenGL context
   is the prerequisite for using any OpenGL functionality. The GLFW binding
   comes in two flavors: One for GLFW 2.x and one for GLFW 3+. There are
   significant differences between these two, the most prominent being that
   GLFW 3 can handle multiple windows. You can set the desired GLFW version
   for the binding at compile time.
 * [SOIL][10] (`opengl-soil.gpr`): The *Simple OpenGL Image Library*. This is a
   very tiny library for loading image files into OpenGL textures. It is public
   domain. Because it's so tiny, it is linked directly into OpenGLAda. Its
   source is included in the OpenGLAda sources.
 * [FTGL][11] (`opengl-ftgl.gpr`, **deprecated**): A library built on top of
   FreeType that provides an API to load TrueType fonts and render text with
   OpenGL. The Ada wrapper only provides basic functionality to load fonts and
   render text. As it does not include a wrapper to FreeType, the more low-level
   functionality has been excluded. **This wrapper is not maintained anymore in
   favor of the new FreeType wrapper.**
 * [FreeType][19] (`FreeTypeAda/freetype.gpr`): A library for loading TrueType
   and OpenType fonts. OpenGLAda includes [FreeTypeAda][20], a wrapper for the
   FreeType library. The project `opengl-text.gpr` provides an original
   higher-level API for rendering text based on FreeTypeAda.

OpenGLAda supports MacOSX, Windows and X11-based systems. API documentation can
be found on the [project's homepage][4].

## Prerequisites

In order to build OpenGLAda, you need to have:

 * A GNAT compiler¹. Compilers known to work well with OpenGLAda are
   [GnuAda][12], [AdaCore GNAT GPL 2017][1], and [TDM-GCC][17]. More information
   is available on the [GCC website][5].
 * [GPRBuild][2] (is bundled with AdaCore's GNAT distribution). TDM-GCC users
   can get it from [here][16] (**NOTE: The gprbuild bundled in this zip is
   known not to work. For the time being, if you're using TDM-GCC, replace
   gprbuild with gnatmake in all commands even though that will tell you that
   project support in gnatmake is deprecated and will soon be removed.** The
   reason for this incompatibility has not been found yet).
 * An OpenGL implementation (usually comes bundled with your graphics driver)
 * Optionally [GLFW][3] (OpenGLAda is pretty useless without the ability to
   create an OpenGL context.)
 * Optionally [FTGL][11] (**deprecated**)
 * Optionally [FreeType][19]

¹: You may also be able to build OpenGLAda with another Ada compiler and/or
without using the `*.gpr` files. You just have to import the sources to your
project and whichever build system you are using. I never used other Ada
compilers apart from GNAT, so if I accidentally used some GNAT-specific features
in the code, please drop me a message.

## Installation

To install OpenGLAda with all optional libraries, execute

    $ gprbuild [options] openglada.gpr
    $ gprinstall [options] openglada.gpr

Where *[options]* is the set of scenario variables you want to use. The
available variables are:

 * `Windowing_System`: Sets the backend windowing system. Used for GLFW and also
                       for system-dependent parts of the API (GLX, WGL, CGL):

    - `x11`: X Windowing System (Linux, BSD, etc)
    - `windows`: Microsoft Windows
    - `quartz`: Quartz Compositor (OS X)

 * `mode`: May take one of the following values:

    - `debug` (default): Compile the project with debugging symbols and without
               optimization.
    - `release`: Compile the project for a release environment.

 * `Auto_Exceptions`: Configures exception handling:

    - `enabled` (default): After each call to OpenGL, OpenGLAda checks whether
      OpenGL has set an error flag and if it had, raises the corresponding
      exception.
    - `disabled`: The user has to query the error flag on their own.

 * `GLFW_Version`: Sets the version of the GLFW library to link against. See
                   [here][6] for a detailed comparison of the two API versions.

    - `2`: GLFW 2.x. Only one window.
    - `3` (default): GLFW 3+. Multiple windows, multiple monitor support, etc.

 * `GLFW_Linker_Param`: Define how you will link to GLFW. Default is `-lglfw`
   everywhere but on Windows with GLFW 3, in which case it is `-lglfw3`.

 * `FreeType_Linker_Param`: Define how you will link to FreeType.
   Default is `-lfreetype`.

For example, a typical Windows installation would be

    $ gprbuild -XWindowing_System=windows -Xmode=release openglada.gpr
    $ gprinstall -XWindowing_System=windows -Xmode=release openglada.gpr

Installing OpenGLAda makes its projects available to `gprbuild` and also to the
GPS GUI. You can now import it like this:

    with "opengl";
    with "opengl-glfw";
    with "opengl-text";
    -- and so on

**Note:** The projects file `openglada.gpr` is just an aggregate project used
for installation. In your projects, depend on `opengl.gpr` and its child
projects.

If you are having trouble getting OpenGLAda to compile in your environment,
please refer to the [detailed guides on the website][13].

## Tests

The tests in this repository are small programs that are mainly used to check
if the basic system is working. You can build them with

    $ make tests

If you're on Windows and do not have the `make` utility available, do this
instead:

    $ gprbuild -P opengl-glfw-test.gpr -XWindowing_System=windows
    $ gprbuild -P opengl-test.gpr -XWindowing_System=windows

The tests require GLFW, because they need to create windows. By default, they
try to link against GLFW 3+. You can instead build the tests against GLFW 2.x
by executing:

    $ gprbuild -P opengl-test.gpr -XWindowing_System=windows -XGLFW_Version=2
    $ gprbuild -P opengl-glfw-test.gpr -XWindowing_System=windows -XGLFW_Version=2

(Substitute `windows` with `x11` or `quartz` if needed.)

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

OpenGLAda is distributed under the terms of the [MIT License][7]. The Ada 2012
logo that is used in the SOIL tests is distributed under the terms of the
[CC BY-ND 3.0][8] license, the original author is [AdaCore][9].

 [1]: http://libre.adacore.com/
 [2]: http://www.adacore.com/gnatpro/toolsuite/gprbuild/
 [3]: http://www.glfw.org/
 [4]: http://flyx.github.io/OpenGLAda/
 [5]: http://gcc.gnu.org/wiki/GNAT
 [6]: http://www.glfw.org/docs/3.0/moving.html
 [7]: COPYING
 [8]: http://creativecommons.org/licenses/by-nd/3.0/deed.en_GB
 [9]: http://www.ada2012.org/#the_logo
 [10]: http://www.lonesock.net/soil.html
 [11]: https://sourceforge.net/projects/ftgl/
 [12]: https://sourceforge.net/projects/gnuada/files/
 [13]: http://flyx.github.io/OpenGLAda/setup.html
 [16]: http://getadanow.com/#get_windows
 [17]: http://tdm-gcc.tdragon.net/
 [19]: https://freetype.org/
 [20]: https://github.com/flyx/FreeTypeAda