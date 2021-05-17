# OpenGLAda – OpenGL binding for Ada

## Overview

**OpenGLAda** is a thick OpenGL binding for the Ada 2005 programming language.
Unlike other, thin, bindings (see the [project's homepage][4] for a list),
OpenGLAda enriches the original API with concepts and features provided by
Ada, like object orientation, type safety and generics.

OpenGLAda provides the following libraries:

| Library name   | [Alire][29] crate name | Description                  |
| ---            | ---                    | ---                          |
| `opengl`       | `openglada`            | The OpenGL binding itself    |
| `opengl-glfw`  | `openglada_glfw`       | [GLFW][3] binding            |
| `opengl-text`  | `openglada_text`       | Text rendering library       |
| `opengl-images`| `openglada_images`     | Image loading library        |

The library name is what you want to **`with`** in your `.gpr` file, e.g. `with "opengl-glfw";`.
The Alire crate is how you refer to the library if you're using Alire, e.g. `alr with openglada`.

The **GLFW binding** requires the [GLFW][3] library.
The **Text rendering library** requires the [FreeType][19] library.
The **Image loading library** uses [GID][10].

The GLFW and FreeType dependencies will be fetched automatically by Alire on Windows, Debian, Ubuntu and Arch Linux.
On other systems, you need to make them available to the linker yourself.

OpenGLAda supports macOS, Windows and X11-based systems.
API documentation can be found on the [project's homepage][4].

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

## Installation

OpenGLAda is designed to be used with the [Alire][29] package manager.
You can instead install the projects via `gprbuild` + `gprinstall`.
This needs to be done for each project you want to use.

## Scenario Variables

OpenGLAda defines a number of scenario variables.
Some are set automatically by Alire, others need to be changed manually with the `-X` command line parameter (either through `alr` or `grpbuild`).

The available variables are:

 * `Auto_Exceptions`: Configures exception handling:

    - `enabled` (default): After each call to OpenGL, OpenGLAda checks whether
      OpenGL has set an error flag and if it had, raises the corresponding
      exception.
    - `disabled`: The user has to query the error flag on their own.

 * `mode`: May take one of the following values:

    - `debug` (default): Compile the project with debugging symbols and without
               optimization.
    - `release`: Compile the project for a release environment.


 * `Windowing_System`: *Automatically set by Alire*.
    Sets the backend windowing system.
    Used for GLFW and also for system-dependent parts of the API (GLX, WGL, CGL):

    - `x11`: X Windowing System (Linux, BSD, etc)
    - `windows`: Microsoft Windows
    - `quartz`: Quartz Compositor (macOS)

 * `GLFW_Linker_Param`:
   Relevant for the GFLW binding.
   Define how you will link to GLFW. Default is `-lglfw`
   everywhere but on Windows with GLFW 3, in which case it is `-lglfw3`.
   No need to change this if the GLFW library is loaded via Alire.

 * `FreeType_Linker_Param`: *Automatically set by Alire*.
   Relevant for the text rendering library.
   Define how you will link to FreeType.
   Default is `-lfreetype`.

A typical Windows installation would be

    $ cd opengl
    $ gprbuild -p -XWindowing_System=windows -Xmode=release opengl.gpr
    $ gprinstall -XWindowing_System=windows -Xmode=release opengl.gpr

## Tests

The tests in this repository are small programs that are mainly used to check
if the basic system is working. You can build them with

    $ env GPR_PROJECT_PATH=opengl:opengl-glfw:opengl-images:opengl-text \
          alr build

The tests use Alire for fetching the dependencies, but do not use the Alire projects of the libraries.
The Alire workspace for the tests is not meant to be published.

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

The code generator can be found in `opengl/src/generator`. It processes the files found
in `opengl/src/specs` and creates the files found in `opengl/src/generated`. The
generator is a tool used at compile-time for building OpenGLAda and of no
interest to the general user. The generated files are checked in to version
control. The generator also generates the markdown file which is the base for
the [function mapping list][27] on the website.

The process of wrapping a new OpenGL function is:

 * Build the generator using `gprbuild generator.gpr` in the `opengl` directory.
 * Add the function specification to one of the `*.spec` files in `opengl/src/specs`.
 * Run `./generator` in the `opengl` directory (or `generator.exe` on Windows), which runs the generator on the specs and generates the Ada code.
 * Check in the newly generated code along with the changed spec.

The `*.spec` files use a syntax similar to Ada.

## License

OpenGLAda, as well as the Ada dependencies FreeTypeAda and GID, are
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
 [29]: https://alire.ada.dev