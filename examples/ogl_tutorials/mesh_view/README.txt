-- 06 Sep 2017 ----------------------------------------------------------------
1) Unpack the .tar.gz archive so that the meshview directory is in

   OpenGLAda-0.4/examples/ogl_tutorials/meshview

2) Compile and run

   cd OpenGLAda-0.4/examples/ogl_tutorials/meshview
   make
   meshview

3) Other examples

   meshview meshes/lattice.obj
   meshview meshes/suzanne.obj

This code has been tested using GNAT GPL 2017 on MacOS Sierra 10.12.6 with
Xcode 8.3.3 and the Xcode command line tools.

The code requires OpenGLAda-0.4.

The glfw-3.1.1 and glew-2.1.0 libraries were installed using "brew".
