--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.API;

-- Fixed function pipeline. Deprecated in OpenGL 3.0.
package body GL.Fixed is
   procedure Set_Vertex_Pointer (Length : Vertex_Length;
                                 Stride, Offset : Size) is

   begin
      API.Vertex_Pointer (Int (Length), Double_Type,
                          Stride, Offset);
      Raise_Exception_On_OpenGL_Error;
   end Set_Vertex_Pointer;

   procedure Set_Color_Pointer  (Stride, Offset : Size) is
   begin
      API.Color_Pointer (4, Single_Type, Stride,
                         Int (Offset));
      Raise_Exception_On_OpenGL_Error;
   end Set_Color_Pointer;

   procedure Enable (Capability : Client_Side_Capability) is
   begin
      API.Enable_Client_State (Capability);
      Raise_Exception_On_OpenGL_Error;
   end Enable;

   procedure Disable (Capability : Client_Side_Capability) is
   begin
      API.Disable_Client_State (Capability);
      Raise_Exception_On_OpenGL_Error;
   end Disable;
end GL.Fixed;
