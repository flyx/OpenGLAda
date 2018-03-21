--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

package body GL.API.Mac_OS_X is
   OpenGLFramework_Cached : CFBundleRef;

   function OpenGLFramework return CFBundleRef is
      use type System.Address;
   begin
      if OpenGLFramework_Cached = System.Null_Address then
         declare
            OpenGLFramework_ID : constant CFStringRef
              := CFStringCreateWithCString (System.Null_Address,
                                            IFC.To_C ("com.apple.opengl"),
                                            kCFStringEncodingASCII);
         begin
            OpenGLFramework_Cached
              := CFBundleGetBundleWithIdentifier (OpenGLFramework_ID);
         end;
      end if;
      return OpenGLFramework_Cached;
   end OpenGLFramework;

end GL.API.Mac_OS_X;
