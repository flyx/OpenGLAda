# macOS

### GNAT GPL 2017

 * Download and install GNAT GPL 2017 x86_64 Mac OS X from
   [AdaCore's Libre Site][1]. We assume you install it to `/usr/local/gnat`. Make
   sure the `bin` folder is in the `PATH` of your shell.
 * There are multiple ways to install `glfw`; the most popular one is probably
   using [Homebrew][2] and executing `brew install glfw`.
 * If you want to use FreeType, you can install it with `brew install freetype`.
 * Open your shell and navigate to the root folder of OpenGLAda. Execute:

        $ gprbuild -p -P opengl-test.gpr -XWindowing_System=quartz
        $ gprbuild -p -P opengl-text-test.gpr -XWindowing_System=quartz

   This should produce executables in the `bin` folder inside OpenGLAda.
 * Keep in mind that you need to spread the `glfw.dylib` and `freetype.dylib` 
   alongside your binaries for them to work.

 [1]: http://libre.adacore.com/
 [2]: https://brew.sh