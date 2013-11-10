--------------------------------------------------------------------------------
-- Copyright (c) 2013, Felix Krause <contact@flyx.org>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

with GL.API;
with GL.Enums.Textures;

package body GL.Objects.Textures.Targets is

   function Buffer_Offset (Object : Texture_Buffer_Target;
                           Level : Mipmap_Level) return Size is
      Ret : Size;
   begin
      API.Get_Tex_Level_Parameter_Size (Object.Kind, Level,
                                        Enums.Textures.Buffer_Offset, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Buffer_Offset;

   function Buffer_Size (Object : Texture_Buffer_Target;
                         Level : Mipmap_Level) return Size is
      Ret : Size;
   begin
      API.Get_Tex_Level_Parameter_Size (Object.Kind, Level,
                                        Enums.Textures.Buffer_Size, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Buffer_Size;

   function Target_From_Kind (Kind : Low_Level.Enums.Texture_Kind)
                              return not null access constant Texture_Proxy'Class is
   begin
      case Kind is
         when GL.Low_Level.Enums.Texture_1D => return Texture_1D'Access;
         when GL.Low_Level.Enums.Texture_2D => return Texture_2D'Access;
         when GL.Low_Level.Enums.Texture_3D => return Texture_3D'Access;
         when GL.Low_Level.Enums.Proxy_Texture_1D =>
            return Texture_1D_Proxy'Access;
         when GL.Low_Level.Enums.Proxy_Texture_2D =>
            return Texture_2D_Proxy'Access;
         when GL.Low_Level.Enums.Proxy_Texture_3D =>
            return Texture_3D_Proxy'Access;
         when GL.Low_Level.Enums.Proxy_Texture_Cube_Map =>
            return Texture_Cube_Map_Proxy'Access;
         when GL.Low_Level.Enums.Texture_Cube_Map =>
            return Texture_Cube_Map'Access;
         when GL.Low_Level.Enums.Texture_Cube_Map_Positive_X =>
            return Texture_Cube_Map_Positive_X'Access;
         when GL.Low_Level.Enums.Texture_Cube_Map_Negative_X =>
            return Texture_Cube_Map_Negative_X'Access;
         when GL.Low_Level.Enums.Texture_Cube_Map_Positive_Y =>
            return Texture_Cube_Map_Positive_Y'Access;
         when GL.Low_Level.Enums.Texture_Cube_Map_Negative_Y =>
            return Texture_Cube_Map_Negative_Y'Access;
         when GL.Low_Level.Enums.Texture_Cube_Map_Positive_Z =>
            return Texture_Cube_Map_Positive_Z'Access;
         when GL.Low_Level.Enums.Texture_Cube_Map_Negative_Z =>
            return Texture_Cube_Map_Negative_Z'Access;
         when GL.Low_Level.Enums.Texture_1D_Array |
              GL.Low_Level.Enums.Texture_2D_Array |
              GL.Low_Level.Enums.Proxy_Texture_1D_Array |
              GL.Low_Level.Enums.Proxy_Texture_2D_Array =>
            raise Not_Implemented_Exception with Kind'Img;
         when GL.Low_Level.Enums.Texture_Buffer =>
            return Texture_Buffer'Access;
      end case;
   end Target_From_Kind;

end GL.Objects.Textures.Targets;
