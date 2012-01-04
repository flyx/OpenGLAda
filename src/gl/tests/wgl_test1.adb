with GL;
with GL.Binding;
with GL.WGL;

with System;

procedure WGL_Test1 is

   type Dummy_Refs_Array is array (Positive range <>) of System.Address;

   --  just to test if the wgl functions are correctly imported
   Dummy_Refs : constant Dummy_Refs_Array :=
     (GL.WGL.wglDeleteContext'Address, GL.WGL.wglMakeCurrent'Address,
      GL.WGL.wglSetPixelFormat'Address, GL.WGL.wglSwapBuffers'Address,
      GL.WGL.wglGetCurrentDC'Address, GL.WGL.wglCreateContext'Address,
      GL.WGL.wglCreateLayerContext'Address, GL.WGL.wglGetCurrentContext'Address,
      GL.WGL.wglGetProcAddress'Address, GL.WGL.wglChoosePixelFormat'Address);
begin
   null;
end WGL_Test1;
