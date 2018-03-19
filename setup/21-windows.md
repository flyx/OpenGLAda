---
layout : default
title : Windows Instructions
permalink: setup.html
navclass: setup
---

# Windows Setup

**General advice:** Keep in mind that you need to spread the DLLs you link to
along with your binaries for them to work on other systems.

## GNAT GPL 2017

 * Download and install GNAT GPL 2017 Windows x86 from
   [AdaCore's Libre Site][1]. We assume you install it to `C:\GNAT\2017`. Make
   sure the `bin` folder is in the `PATH` of your shell.
 * Download *32-bit Windows binaries* from the [GLFW download section][2]
   (tested with GLFW 3.2.1).
 * Open the zip folder and copy the file `glfw3.dll` from the `lib-mingw`
   folder into `C:\GNAT\2017\lib`.
 * If you want to use FreeType, you need at least FreeType 2.7.1. There is an
   [semi-official GitHub repository][3] which hosts current Windows binaries
   known to work; get the `freetype.dll` file from the `win32` folder and copy
   it into `C:\GNAT\2017\lib`.
 * The FreeType library requires the Visual C++ Redistributable libraries for
   Visual Studio 2017. Download and install the `x86` version from [here][4] and
   install it.
 * Open your shell and navigate to the root folder of OpenGLAda. Execute:

        $ gprbuild -p -P opengl-test.gpr -XWindowing_System=windows

   This should produce executables in the `bin` folder inside OpenGLAda.
 * You can now either add `C:\GNAT\2017\lib` to your `PATH` or copy the
   DLLs from there to the `bin` folder. One of those two options is
   necessary so that the binaries can find the dll.

## TDM-GCC 64bit

These instructions are for building 64bit binaries on Windows.

 * Download and install the 64bit TDM-GCC compiler from [here][6]. In the
   installation wizard, make sure to check the (by default unchecked) *ada*
   option. Make sure the `bin` folder is in the `PATH` of your shell.

<!-- These currently do not work – uncomment when fixed

 * Download `gpr-tools.zip` from [here][16] and extract its contents to the
   folder where you installed TDM-GCC (e.g. `C:\TDM-GCC-64\`).

-->

 * Download the *64-bit Windows binaries* from the [GLFW download section][2]
   (tested with GLFW 3.2.1).
 * Open the zip folder and copy the file `glfw3.dll` from the `lib-mingw-w64`
   folder into `C:\TDM-GCC-64\lib`.
 * If you want to use FreeType, you need at least FreeType 2.7.1. There is an
   [semi-official GitHub repository][3] which hosts current Windows binaries
   known to work; get the `freetype.dll` file from the `win64` folder and copy
   it into `C:\TDM-GCC-64\lib`.
 * The FreeType library requires the Visual C++ Redistributable libraries for
   Visual Studio 2017. Download and install the `x64` version from [here][4] and
   install it.
 * Open your shell and navigate to the root folder of OpenGLAda. Execute:

        gnatmake -p -P opengl-test.gpr -XWindowing_System=windows
        gnatmake -p -P opengl-text-test.gpr -XWindowing_System=quartz

   This should produce executables in the `bin` folder inside OpenGLAda.
 * You can now either add `C:\TDM-GCC-64\lib` to your `PATH` or copy the
   DLLs from there to the `bin` folder. One of those two options is
   necessary so that the binary can find the dll.

 [1]: http://libre.adacore.com/
 [2]: http://www.glfw.org/download.html
 [3]: https://github.com/ubawurinna/freetype-windows-binaries/
 [4]: https://support.microsoft.com/en-us/help/2977003/the-latest-supported-visual-c-downloads
 [6]: http://tdm-gcc.tdragon.net/download