--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <contact@flyx.org>
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

with GL.Enums.Textures;
with GL.Helpers;
with GL.API;

package body GL.Fixed.Textures is

   procedure Set_Tex_Function (Func : Texture_Function) is
   begin
      API.Tex_Env_Tex_Func (Enums.Textures.Texture_Env, Enums.Textures.Env_Mode,
                                Func);
      Raise_Exception_On_OpenGL_Error;
   end Set_Tex_Function;

   function Tex_Function return Texture_Function is
      Ret : Texture_Function := Texture_Function'Val(0);
   begin
      API.Get_Tex_Env_Tex_Func (Enums.Textures.Texture_Env, Enums.Textures.Env_Mode,
                                Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Tex_Function;

   procedure Set_RGB_Combine (Func : Combine_Function) is
   begin
      API.Tex_Env_Combine_Func (Enums.Textures.Texture_Env, Enums.Textures.Combine_RGB,
                                Func);
      Raise_Exception_On_OpenGL_Error;
   end Set_RGB_Combine;

   function RGB_Combine return Combine_Function is
      Ret : Combine_Function := Combine_Function'Val(0);
   begin
      API.Get_Tex_Env_Combine_Func (Enums.Textures.Texture_Env,
                                    Enums.Textures.Combine_RGB, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end RGB_Combine;

   procedure Set_Alpha_Combine (Func : Alpha_Combine_Function) is
   begin
      API.Tex_Env_Combine_Func (Enums.Textures.Texture_Env,
                                Enums.Textures.Combine_Alpha, Func);
      Raise_Exception_On_OpenGL_Error;
   end Set_Alpha_Combine;

   function Alpha_Combine return Alpha_Combine_Function is
      Ret : Combine_Function := Combine_Function'Val(0);
   begin
      API.Get_Tex_Env_Combine_Func (Enums.Textures.Texture_Env,
                                    Enums.Textures.Combine_Alpha, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Alpha_Combine;

   procedure Set_RGB_Source (Source : Source_Kind; Index : Source_Index) is
      Param : Enums.Textures.Env_Parameter;
   begin
      case Index is
         when 0 => Param := Enums.Textures.Src0_RGB;
         when 1 => Param := Enums.Textures.Src1_RGB;
         when 2 => Param := Enums.Textures.Src2_RGB;
      end case;
      API.Tex_Env_Source (Enums.Textures.Texture_Env, Param, Source);
      Raise_Exception_On_OpenGL_Error;
   end Set_RGB_Source;

   function RGB_Source (Index : Source_Index) return Source_Kind is
      Param : Enums.Textures.Env_Parameter;
      Ret   : Source_Kind := Source_Kind'Val(0);
   begin
      case Index is
         when 0 => Param := Enums.Textures.Src0_RGB;
         when 1 => Param := Enums.Textures.Src1_RGB;
         when 2 => Param := Enums.Textures.Src2_RGB;
      end case;
      API.Get_Tex_Env_Source (Enums.Textures.Texture_Env, Param, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end RGB_Source;

   procedure Set_Alpha_Source (Source : Source_Kind; Index : Source_Index) is
      Param : Enums.Textures.Env_Parameter;
   begin
      case Index is
         when 0 => Param := Enums.Textures.Src0_Alpha;
         when 1 => Param := Enums.Textures.Src1_Alpha;
         when 2 => Param := Enums.Textures.Src2_Alpha;
      end case;
      API.Tex_Env_Source (Enums.Textures.Texture_Env, Param, Source);
      Raise_Exception_On_OpenGL_Error;
   end Set_Alpha_Source;

   function Alpha_Source (Index : Source_Index) return Source_Kind is
      Param : Enums.Textures.Env_Parameter;
      Ret   : Source_Kind := Source_Kind'Val(0);
   begin
      case Index is
         when 0 => Param := Enums.Textures.Src0_Alpha;
         when 1 => Param := Enums.Textures.Src1_Alpha;
         when 2 => Param := Enums.Textures.Src2_Alpha;
      end case;
      API.Get_Tex_Env_Source (Enums.Textures.Texture_Env, Param, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Alpha_Source;

   procedure Set_RGB_Scale (Value : Scaling_Factor) is
   begin
      API.Tex_Env_Float (Enums.Textures.Texture_Env,
                         Enums.Textures.RGB_Scale, Single (Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_RGB_Scale;

   function RGB_Scale return Scaling_Factor is
      Ret : Single := 0.0;
   begin
      API.Get_Tex_Env_Float (Enums.Textures.Texture_Env,
                             Enums.Textures.RGB_Scale, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Double (Ret);
   end RGB_Scale;

   procedure Set_Alpha_Scale (Value : Scaling_Factor) is
   begin
      API.Tex_Env_Float (Enums.Textures.Texture_Env,
                         Enums.Textures.Alpha_Scale, Single (Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_Alpha_Scale;

   function Alpha_Scale return Scaling_Factor is
      Ret : Single := 0.0;
   begin
      API.Get_Tex_Env_Float (Enums.Textures.Texture_Env,
                             Enums.Textures.Alpha_Scale, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Double (Ret);
   end Alpha_Scale;

   procedure Set_LoD_Bias (Value : Double) is
   begin
      API.Tex_Env_Float (Enums.Textures.Filter_Control,
                         Enums.Textures.LoD_Bias, Single (Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_LoD_Bias;

   function LoD_Bias return Double is
      Ret : Single := 0.0;
   begin
      API.Get_Tex_Env_Float (Enums.Textures.Filter_Control,
                             Enums.Textures.LoD_Bias, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Double (Ret);
   end LoD_Bias;

   procedure Set_Env_Color (Value : Colors.Color) is
      Float_Colors : constant Low_Level.Single_Array
        := Helpers.Float_Array (Value);
   begin
      API.Tex_Env_Arr (Enums.Textures.Texture_Env,
                       Enums.Textures.Env_Color, Float_Colors);
      Raise_Exception_On_OpenGL_Error;
   end Set_Env_Color;

   function Env_Color return Colors.Color is
      Ret : Low_Level.Single_Array (1 .. 4);
   begin
      API.Get_Tex_Env_Arr (Enums.Textures.Texture_Env,
                           Enums.Textures.Env_Color, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Helpers.Color (Ret);
   end Env_Color;

   procedure Toggle_Point_Sprite_Coord_Replace (Enabled : Boolean) is
   begin
      API.Tex_Env_Bool (Enums.Textures.Point_Sprite,
                        Enums.Textures.Coord_Replace, Low_Level.Bool (Enabled));
      Raise_Exception_On_OpenGL_Error;
   end Toggle_Point_Sprite_Coord_Replace;

   function Point_Sprite_Coord_Replace return Boolean is
      Ret : Low_Level.Bool := Low_Level.False;
   begin
      API.Get_Tex_Env_Bool (Enums.Textures.Point_Sprite,
                            Enums.Textures.Coord_Replace, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Boolean (Ret);
   end Point_Sprite_Coord_Replace;

end GL.Fixed.Textures;
