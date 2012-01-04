#include <stdio.h>
#include <OpenGL/OpenGL.h>

int main () {
	printf ("Sizes of CGL types\n");
	printf ("------------------\n");
	printf ("CGLPixelFormatAttribute: %d\n", sizeof (CGLPixelFormatAttribute));
	printf ("CGLRendererProperty: %d\n", sizeof (CGLRendererProperty));
	printf ("CGLContextEnable: %d\n", sizeof (CGLContextEnable));
	printf ("CGLContextParameter: %d\n", sizeof (CGLContextParameter));
	printf ("CGLGlobalOption: %d\n", sizeof (CGLGlobalOption));
	printf ("CGLOpenGLProfile: %d\n", sizeof (CGLOpenGLProfile));
	printf ("CGLError: %d\n", sizeof (CGLError));
	return 0;
}