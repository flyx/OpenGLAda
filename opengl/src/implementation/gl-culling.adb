--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.API;
with GL.Enums.Getter;

package body GL.Culling is

   procedure Set_Front_Face (Face : Orientation) renames API.Front_Face;

   function Front_Face return Orientation is
      Ret : aliased Orientation;
   begin
      API.Get_Orientation (Enums.Getter.Front_Face, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Front_Face;

   procedure Set_Cull_Face (Selector : Face_Selector) renames API.Cull_Face;

   function Cull_Face return Face_Selector is
      Ret : aliased Face_Selector;
   begin
      API.Get_Face_Selector (Enums.Getter.Cull_Face_Mode, Ret'Access);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Cull_Face;

end GL.Culling;
