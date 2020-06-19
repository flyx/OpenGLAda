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
 * [FreeType][19] (`deps/FreeTypeAda/freetype.gpr`): A library for loading
   TrueType and OpenType fonts. OpenGLAda includes [FreeTypeAda][20], a wrapper
   for the FreeType library. The project `opengl-text.gpr` provides an original
   higher-level API for rendering text based on FreeTypeAda.
 * [GID][10] (`deps/gid/gid.gpr`): The *Generic Image Decoder*. This is an
   original Ada library for loading common image formats, which is included in
   OpenGLAda. The project `opengl-images.gpr` provides an original higher-level
   API for generating OpenGL textures with GID.

OpenGLAda supports macOS, Windows and X11-based systems. API documentation can
be found on the [project's homepage][4].

### Migrating from C

Compared to C, OpenGLAda provides the features of the following C libraries:

 * OpenGL
 * [GLEW][22]: OpenGLAda loads all post-OpenGL 1.1 subroutines dynamically via
   pointers, so that available features may be queried at runtime.
 * [GLUT][23]: Long deprecated, yet still referenced from articles about OpenGL.
   OpenGLAda provides an optional wrapper for the GLFW library that provides
   functionality similar to GLUT.
   
   Text rendering functionality superior to what GLUT provides are supplied by
   `opengl-text.gpr` with the help of the FreeType library.
 * [Image loading][24]: OpenGLAda includes the [GID][10] library for image
   loading.

## Building & Installation

### Windows

There is an installer available for 64bit Windows + GNAT Community in the
[repository's *releases* section][21] which includes all optional dependencies.

### Prerequisites

In order to build OpenGLAda, you need to have:

 * A GNAT compiler¹. Compilers known to work well with OpenGLAda are
   [GnuAda][12], [GNAT Community 2020][1], and [TDM-GCC][17]. More information
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
 * Optionally [FreeType][19]

¹: You may also be able to build OpenGLAda with another Ada compiler and/or
without using the `*.gpr` files. You just have to import the sources to your
project and whichever build system you are using. I never used other Ada
compilers apart from GNAT, so if I accidentally used some GNAT-specific features
in the code, open an issue.

### Installation

To install OpenGLAda with all optional libraries, execute

    $ gprbuild -p [options] openglada.gpr
    $ gprinstall [options] openglada.gpr

Where *[options]* is the set of scenario variables you want to use (generally
in the form of `-X`*name*`=`*value*`). The available variables are:

 * `Windowing_System`: Sets the backend windowing system. Used for GLFW and also
                       for system-dependent parts of the API (GLX, WGL, CGL):

    - `x11`: X Windowing System (Linux, BSD, etc)
    - `windows`: Microsoft Windows
    - `quartz`: Quartz Compositor (macOS)

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

    $ gprbuild -p -XWindowing_System=windows -Xmode=release openglada.gpr
    $ gprinstall -XWindowing_System=windows -Xmode=release openglada.gpr

Installing OpenGLAda makes its projects available to `gprbuild` and also to
GNAT Studio. You can now import it like this:

    with "opengl";
    with "opengl-glfw";
    with "opengl-text";
    -- and so on

**Note:** The project file `openglada.gpr` is just an aggregate project used
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
    $ gprbuild -P opengl-text-test.gpr -XWindowing_System=windows
    $ gprbuild -P opengl-images-test.gpr -XWindowing_System=windows

The tests require GLFW, because they need to create windows. By default, they
try to link against GLFW 3+. You can instead build the tests against GLFW 2.x
by adding the parameter `-XGLFW_Version=2`.

## Examples

Examples are available in [this repository][11].

## Developer Documentation

I have written an article about the development of OpenGLAda on AdaCore's blog:

 * [Part 1][25]
 * [Part 2][26]

### Binding Generation

OpenGL implementations do not necessarily provide the newest OpenGL version,
but possibly some older one with some functionality of the newer versions
provided as extensions. For OpenGLAda, this means that most OpenGL functionality
cannot be linked against via the library loader, since loading the library
would immediately fail if any function is not available, even if the user never
calls it. Most notoriously, Windows does not provide any OpenGL functionality
newer than 1.1 via library loading.

The remedy for this is that function pointers of newer OpenGL functions must
be queried at runtime. This is a tedious process and similar for each function.
For this reason, OpenGLAda uses a code generator to autogenerate the function
pointer types and the code loading each function, as well as the code importing
OpenGL 1.1 functions via library loading.

The code generator can be found in `src/generator`. It processes the files found
in `src/gl/specs` and creates the files found in `src/gl/generated`. The
generator is a tool used at compile-time for building OpenGLAda and of no
interest to the general user. The generated files are checked in to version
control. The generator also generates the markdown file which is the base for
the [function mapping list][27] on the website.

The process of wrapping a new OpenGL function is:

 * Add the function specification to one of the `*.spec` files in `src/gl/specs`.
 * Run `make generate`, which builds the generator and generates the Ada code.
 * Check in the newly generated code along with the changed spec.

The `*.spec` files use a syntax similar to Ada.

### Building the Windows Installer

The Windows installer is generated with the [WiX Toolset][28]. You'll need its
executables `heat.exe`, `candle.exe` and `light.exe` in your `PATH`. Executing
the script `create-msi.ps1` in the `install` directory containing it will

 * download binaries of the GLFW and FreeType libraries
 * build OpenGLAda with GNAT
 * install OpenGLAda in a temporary directory
 * Build an MSI installer that will copy OpenGLAda's binaries as well as the
   GLFW and FreeType libraries into the installation directory of a
   GNAT Community installation.

## License

OpenGLAda, as well as the included libraries FreeTypeAda and GID, are
distributed under the terms of the [MIT License][7].

The Ada 2012 logo that is used in the images tests is distributed under the
terms of the [CC BY-ND 3.0][8] license, the original author is [AdaCore][9].

 [1]: http://libre.adacore.com/
 [2]: http://www.adacore.com/gnatpro/toolsuite/gprbuild/
 [3]: http://www.glfw.org/
 [4]: http://flyx.github.io/OpenGLAda/
 [5]: http://gcc.gnu.org/wiki/GNAT
 [6]: http://www.glfw.org/docs/3.0/moving.html
 [7]: COPYING
 [8]: http://creativecommons.org/licenses/by-nd/3.0/deed.en_GB
 [9]: http://www.ada2012.org/#the_logo
 [10]: https://gen-img-dec.sourceforge.io/
 [11]: https://github.com/flyx/OpenGLAda-examples
 [12]: https://sourceforge.net/projects/gnuada/files/
 [13]: http://flyx.github.io/OpenGLAda/setup.html
 [16]: http://getadanow.com/#get_windows
 [17]: http://tdm-gcc.tdragon.net/
 [19]: https://freetype.org/
 [20]: https://github.com/flyx/FreeTypeAda
 [21]: https://github.com/flyx/OpenGLAda/releases
 [22]: http://glew.sourceforge.net/
 [23]: https://www.opengl.org/resources/libraries/glut/
 [24]: https://www.khronos.org/opengl/wiki/Image_Libraries
 [25]: https://blog.adacore.com/the-road-to-a-thick-opengl-binding-for-ada
 [26]: https://blog.adacore.com/the-road-to-a-thick-opengl-binding-for-ada-part-2
 [27]: https://flyx.github.io/OpenGLAda/mapping.html
 [28]: https://wixtoolset.org/