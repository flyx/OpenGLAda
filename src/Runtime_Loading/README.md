# Runtime_Loading

This is shared code between [OpenGLAda][1] and [OpenCLAda][2]. The OpenGL and OpenCL APIs provide functions
that return function pointers to other library functions. This is necessary because these other functions
were introduced in newer revisions of the APIs and might not be available in implementations that are based
on older API versions.

**Runtime_Loading** provides generic functions and procedures that wrap such API functions. Querying and
caching of the function pointers is done automatically when the subprogram is called for the first time.
If the current implementation does not provide the requested functionality, an exception is thrown.

This repo exists to share this code between those two wrappers. No build script is provided to compile
*Runtime_Loading* as standalone library, because it is too small and too specialized. If you happen to
find this code useful, just drop it in your codebase. It's available under the terms of the 
[ISC License][3].

 [1]: http://flyx86.github.com/OpenGLAda/
 [2]: http://flyx86.github.com/OpenCLAda/
 [3]: http://www.opensource.org/licenses/ISC