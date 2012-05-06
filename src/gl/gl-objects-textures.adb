--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <flyx@isobeef.org>
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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with GL.API;
with GL.Helpers;

package body GL.Objects.Textures is
   Cur_Texture_1D : aliased Texture := Texture'(
     Ada.Finalization.Controlled with Kind => Texture_1D, Reference => null);
   Cur_Texture_2D : aliased Texture := Texture'(
     Ada.Finalization.Controlled with Kind => Texture_2D, Reference => null);
   Cur_Texture_3D : aliased Texture := Texture'(
     Ada.Finalization.Controlled with Kind => Texture_3D, Reference => null);
   Cur_Texture_CM : aliased Texture := Texture'(
     Ada.Finalization.Controlled with Kind      => Texture_Cube_Map,
                                      Reference => null);
   
   Current_Textures : constant array (Texture_Kind) of access Texture := (
     Texture_1D       => Cur_Texture_1D'Access,
     Texture_2D       => Cur_Texture_2D'Access,
     Texture_3D       => Cur_Texture_3D'Access,
     Texture_Cube_Map => Cur_Texture_CM'Access);
   
   overriding procedure Bind (Object : Texture) is
   begin
      if Current_Textures (Object.Kind).all /= Object then
         API.Bind_Texture (Object.Kind, Object.Reference.GL_Id);
         Check_OpenGL_Error;
         Current_Textures (Object.Kind).all := Object;
      end if;
   end Bind;
   
   procedure Create_Id (Object : in out Texture) is
      New_Id : Low_Level.UInt_Array (1..2) := (1 => 0, 2 => 0);
   begin
      API.Gen_Textures (1, New_Id (1)'Access);
      Check_OpenGL_Error;
      Object.Reference.GL_Id := New_Id (1);
   end Create_Id;
   
   procedure Delete_Id (Object : in out Texture) is
      Arr : Low_Level.UInt_Array := (1 => Object.Reference.GL_Id);
   begin
      API.Delete_Textures (1, Arr);
      Check_OpenGL_Error;
   end Delete_Id;

   procedure Set_Minifying_Filter (Target : Texture;
                                   Filter : Minifying_Function) is
   begin
      Target.Bind;
      API.Tex_Parameter_Min_Filter (Target.Kind, Enums.Textures.Min_Filter,
                                    Filter);
      Check_OpenGL_Error;
   end Set_Minifying_Filter;

   function Minifying_Filter (Target : Texture) return Minifying_Function is
      function Convert is new Ada.Unchecked_Conversion (Source => Low_Level.Int,
                                                        Target => Minifying_Function);

      Arr : Low_Level.Int_Array (1 .. 1);
   begin
      Target.Bind;
      API.Get_Tex_Parameter_Int (Target.Kind, Enums.Textures.Min_Filter,
                                 Arr);
      Check_OpenGL_Error;
      return Convert (Arr (1));
   end Minifying_Filter;

   procedure Set_Magnifying_Filter (Target : Texture;
                                    Filter : Magnifying_Function) is
   begin
      Target.Bind;
      API.Tex_Parameter_Mag_Filter (Target.Kind, Enums.Textures.Mag_Filter,
                                    Filter);
      Check_OpenGL_Error;
   end Set_Magnifying_Filter;

   function Magnifying_Filter (Target : Texture)
                               return Magnifying_Function is
      function Convert is new Ada.Unchecked_Conversion (Source => Low_Level.Int,
                                                        Target => Minifying_Function);

      Arr : Low_Level.Int_Array (1 .. 1);
   begin
      Target.Bind;
      API.Get_Tex_Parameter_Int (Target.Kind, Enums.Textures.Mag_Filter,
                                 Arr);
      Check_OpenGL_Error;
      return Convert (Arr (1));
   end Magnifying_Filter;

   procedure Set_Minimum_LoD (Target : Texture; Level : Real) is
   begin
      Target.Bind;
      API.Tex_Parameter_Float (Target.Kind, Enums.Textures.Min_LoD,
                               Low_Level.Single (Level));
      Check_OpenGL_Error;
   end Set_Minimum_LoD;

   function Minimum_LoD (Target : Texture) return Real is
      Arr : Low_Level.Single_Array (1 .. 1);
   begin
      Target.Bind;
      API.Get_Tex_Parameter_Float (Target.Kind, Enums.Textures.Min_LoD,
                                   Arr);
      Check_OpenGL_Error;
      return Real (Arr (1));
   end Minimum_LoD;

   procedure Set_Maximum_LoD (Target : Texture; Level : Real) is
   begin
      Target.Bind;
      API.Tex_Parameter_Float (Target.Kind, Enums.Textures.Max_LoD,
                               Low_Level.Single (Level));
      Check_OpenGL_Error;
   end Set_Maximum_LoD;

   function Maximum_LoD (Target : Texture) return Real is
      Arr : Low_Level.Single_Array (1 .. 1);
   begin
      Target.Bind;
      API.Get_Tex_Parameter_Float (Target.Kind, Enums.Textures.Max_LoD,
                                   Arr);
      Check_OpenGL_Error;
      return Real (Arr (1));
   end Maximum_LoD;

   procedure Set_Lowest_Mipmap_Level (Target : Texture; Level : Integer) is
   begin
      Target.Bind;
      API.Tex_Parameter_Int (Target.Kind, Enums.Textures.Base_Level,
                             Low_Level.Int (Level));
      Check_OpenGL_Error;
   end Set_Lowest_Mipmap_Level;

   function Lowest_Mipmap_Level (Target : Texture) return Integer is
      Arr : Low_Level.Int_Array (1 .. 1);
   begin
      Target.Bind;
      API.Get_Tex_Parameter_Int (Target.Kind, Enums.Textures.Base_Level,
                                 Arr);
      Check_OpenGL_Error;
      return Integer (Arr (1));
   end Lowest_Mipmap_Level;

   procedure Set_Highest_Mipmap_Level (Target : Texture; Level : Integer) is
   begin
      Target.Bind;
      API.Tex_Parameter_Int (Target.Kind, Enums.Textures.Max_Level,
                             Low_Level.Int (Level));
      Check_OpenGL_Error;
   end Set_Highest_Mipmap_Level;

   function Highest_Mipmap_Level (Target : Texture) return Integer is
      Arr : Low_Level.Int_Array (1 .. 1);
   begin
      Target.Bind;
      API.Get_Tex_Parameter_Int (Target.Kind, Enums.Textures.Max_Level,
                                 Arr);
      Check_OpenGL_Error;
      return Integer (Arr (1));
   end Highest_Mipmap_Level;

   procedure Set_X_Wrapping (Target : Texture; Mode : Wrapping_Mode) is
   begin
      Target.Bind;
      API.Tex_Parameter_Wrap_Mode (Target.Kind, Enums.Textures.Wrap_S,
                                   Mode);
      Check_OpenGL_Error;
   end Set_X_Wrapping;

   function X_Wrapping (Target : Texture) return Wrapping_Mode is
      Ret : Wrapping_Mode;
   begin
      Target.Bind;
      API.Get_Tex_Parameter_Wrap_Mode (Target.Kind, Enums.Textures.Wrap_S,
                                       Ret);
      Check_OpenGL_Error;
      return Ret;
   end X_Wrapping;

   procedure Set_Y_Wrapping (Target : Texture; Mode : Wrapping_Mode) is
   begin
      Target.Bind;
      API.Tex_Parameter_Wrap_Mode (Target.Kind, Enums.Textures.Wrap_T,
                                   Mode);
      Check_OpenGL_Error;
   end Set_Y_Wrapping;

   function Y_Wrapping (Target : Texture) return Wrapping_Mode is
      Ret : Wrapping_Mode;
   begin
      Target.Bind;
      API.Get_Tex_Parameter_Wrap_Mode (Target.Kind, Enums.Textures.Wrap_T,
                                       Ret);
      Check_OpenGL_Error;
      return Ret;
   end Y_Wrapping;

   procedure Set_Z_Wrapping (Target : Texture; Mode : Wrapping_Mode) is
   begin
      Target.Bind;
      API.Tex_Parameter_Wrap_Mode (Target.Kind, Enums.Textures.Wrap_R,
                                   Mode);
      Check_OpenGL_Error;
   end Set_Z_Wrapping;

   function Z_Wrapping (Target : Texture) return Wrapping_Mode is
      Ret : Wrapping_Mode;
   begin
      Target.Bind;
      API.Get_Tex_Parameter_Wrap_Mode (Target.Kind, Enums.Textures.Wrap_R,
                                       Ret);
      Check_OpenGL_Error;
      return Ret;
   end Z_Wrapping;

   procedure Set_Border_Color (Target : Texture; Color : Colors.Color) is
      use GL.Colors;

      Raw : Low_Level.Single_Array := Helpers.Float_Array (Color);
   begin
      Target.Bind;
      API.Tex_Parameter_Floats (Target.Kind, Enums.Textures.Border_Color,
                                Raw);
      Check_OpenGL_Error;
   end Set_Border_Color;

   function Border_Color (Target : Texture) return Colors.Color is
      use GL.Colors;

      Raw : Low_Level.Single_Array (1 .. 4);
   begin
      Target.Bind;
      API.Get_Tex_Parameter_Float (Target.Kind, Enums.Textures.Border_Color,
                                   Raw);
      Check_OpenGL_Error;
      return Helpers.Color (Raw);
   end Border_Color;

   procedure Set_Texture_Priority (Target : Texture;
                                   Value : Priority) is
   begin
      Target.Bind;
      API.Tex_Parameter_Float (Target.Kind, Enums.Textures.Priority,
                               Low_Level.Single (Value));
      Check_OpenGL_Error;
   end Set_Texture_Priority;

   function Texture_Priority (Target : Texture) return Priority is
      Arr : Low_Level.Single_Array (1 .. 1);
   begin
      Target.Bind;
      API.Get_Tex_Parameter_Float (Target.Kind, Enums.Textures.Priority, Arr);
      Check_OpenGL_Error;
      return Priority (Arr (1));
   end Texture_Priority;

   procedure Toggle_Compare_X_To_Texture (Target  : Texture;
                                          Enabled : Boolean) is
      Value : Enums.Textures.Compare_Kind;
   begin
      Target.Bind;
      if Enabled then
         Value := Enums.Textures.Compare_R_To_Texture;
      else
         Value := Enums.Textures.None;
      end if;
      API.Tex_Parameter_Comp_Mode (Target.Kind, Enums.Textures.Compare_Mode,
                                   Value);
      Check_OpenGL_Error;
   end Toggle_Compare_X_To_Texture;

   function Compare_X_To_Texture_Enabled (Target : Texture) return Boolean is
      use type Enums.Textures.Compare_Kind;

      Value : Enums.Textures.Compare_Kind;
   begin
      Target.Bind;
      API.Get_Tex_Parameter_Comp_Mode (Target.Kind, Enums.Textures.Compare_Mode,
                                       Value);
      Check_OpenGL_Error;
      return Value = Enums.Textures.Compare_R_To_Texture;
   end Compare_X_To_Texture_Enabled;

   procedure Set_Compare_Function (Target : Texture;
                                   Func : Common.Compare_Function) is
   begin
      Target.Bind;
      API.Tex_Parameter_Comp_Func (Target.Kind, Enums.Textures.Compare_Func,
                                   Func);
      Check_OpenGL_Error;
   end Set_Compare_Function;

   function Compare_Function (Target : Texture)
                              return Common.Compare_Function is
      Value : Common.Compare_Function;
   begin
      Target.Bind;
      API.Get_Tex_Parameter_Comp_Func (Target.Kind, Enums.Textures.Compare_Func,
                                       Value);
      Check_OpenGL_Error;
      return Value;
   end Compare_Function;

   procedure Set_Depth_Texture_Mode (Target : Texture; Mode : Depth_Mode) is
   begin
      Target.Bind;
      API.Tex_Parameter_Depth_Mode (Target.Kind, Enums.Textures.Depth, Mode);
      Check_OpenGL_Error;
   end Set_Depth_Texture_Mode;

   function Depth_Texture_Mode (Target : Texture) return Depth_Mode is
      Value : Depth_Mode;
   begin
      Target.Bind;
      API.Get_Tex_Parameter_Depth_Mode (Target.Kind, Enums.Textures.Depth, Value);
      Check_OpenGL_Error;
      return Value;
   end Depth_Texture_Mode;

   procedure Toggle_Mipmap_Autoupdate (Target : Texture; Enabled : Boolean) is
   begin
      Target.Bind;
      API.Tex_Parameter_Bool (Target.Kind, Enums.Textures.Generate_Mipmap,
                              Low_Level.Bool (Enabled));
      Check_OpenGL_Error;
   end Toggle_Mipmap_Autoupdate;

   function Mipmap_Autoupdate_Enabled (Target : Texture) return Boolean is
      Value : Low_Level.Bool;
   begin
      Target.Bind;
      API.Get_Tex_Parameter_Bool (Target.Kind, Enums.Textures.Generate_Mipmap,
                                  Value);
      Check_OpenGL_Error;
      return Boolean (Value);
   end Mipmap_Autoupdate_Enabled;

   
end GL.Objects.Textures;
