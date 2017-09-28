-- 07 Sep 2017 ----------------------------------------------------------------

This is simple example of triangle mesh viewer using OpenGLAda.

The code borrows extensively from other examples in this folder.

The code reads only very basic Wavefront .obj file (allowing for only the simplest
of .obj files, see the examples in the meshes directory).

Controls are simple:

1) left mouse down and drag to rotate the view
2) scroll wheel to move in or out
3) arrow keys to shift the view left, right, up and down
4) press "r" to reset to the initial view
5) press "esc" to terminate

When dragging with the left mouse down you can get the view to spin if you
drag to an abrupt halt and continue to hold the button down. The spinning will
stop when you release the left mouse button.

-- 06 Sep 2017 ----------------------------------------------------------------

1) Compile and run

   cd OpenGLAda-0.4/examples/ogl_tutorials/mesh_view
   make
   meshview obj-file colour-map

2) Other examples

   meshview meshes/lattice.obj
   meshview meshes/suzanne.obj

This code has been tested using GNAT GPL 2017 on MacOS Sierra 10.12.6 with
Xcode 8.3.3 and the Xcode command line tools.

The code requires OpenGLAda-0.4.

The glfw-3.1.1 and glew-2.1.0 libraries were installed using "brew".
