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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Indefinite_Hashed_Maps;

with GL.API;
with GL.Helpers;
with GL.Enums.Getter;
with GL.Enums.Indexes;

package body GL.Objects.Textures is
   use type UInt;
   use type Low_Level.Enums.Texture_Kind;

   function Hash (Key : Low_Level.Enums.Texture_Kind)
     return Ada.Containers.Hash_Type is
      function Value is new Ada.Unchecked_Conversion
        (Source => Low_Level.Enums.Texture_Kind, Target => Low_Level.Enum);
   begin
      return Ada.Containers.Hash_Type (Value (Key));
   end Hash;

   package Texture_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type     => Low_Level.Enums.Texture_Kind,
       Element_Type => Texture,
       Hash         => Hash,
       Equivalent_Keys => Low_Level.Enums."=");
   use type Texture_Maps.Cursor;

   Current_Textures : Texture_Maps.Map;

   procedure Bind (Target : Texture_Target; Object : Texture'Class) is
      Cursor : Texture_Maps.Cursor := Current_Textures.Find (Target.Kind);
   begin
      if Cursor = Texture_Maps.No_Element or else
        Texture_Maps.Element (Cursor).Reference.GL_Id /= Object.Reference.GL_Id
        then
         API.Bind_Texture (Target.Kind, Object.Reference.GL_Id);
         Check_OpenGL_Error;
         if Cursor = Texture_Maps.No_Element then
            Current_Textures.Insert (Target.Kind, Texture (Object));
         else
            Current_Textures.Replace_Element (Cursor, Texture (Object));
         end if;
      end if;
   end Bind;

   function Current_Texture (Target : Texture_Target) return Texture'Class is
      Cursor : Texture_Maps.Cursor := Current_Textures.Find (Target.Kind);
   begin
      if Cursor /= Texture_Maps.No_Element then
         return Texture_Maps.Element (Cursor);
      else
         raise No_Object_Bound_Exception with Target.Kind'Img;
      end if;
   end Current_Texture;

   procedure Initialize_Id (Object : in out Texture) is
      New_Id : Low_Level.UInt_Array (1..2) := (1 => 0, 2 => 0);
   begin
      API.Gen_Textures (1, New_Id (1)'Access);
      Check_OpenGL_Error;
      Object.Reference.GL_Id := New_Id (1);
      Object.Reference.Initialized := True;
   end Initialize_Id;

   procedure Delete_Id (Object : in out Texture) is
      Arr : Low_Level.UInt_Array := (1 => Object.Reference.GL_Id);
   begin
      API.Delete_Textures (1, Arr);
      Check_OpenGL_Error;
      Object.Reference.Initialized := False;
   end Delete_Id;

   procedure Set_Minifying_Filter (Target : Texture_Target;
                                   Filter : Minifying_Function) is
   begin
      API.Tex_Parameter_Min_Filter (Target.Kind, Enums.Textures.Min_Filter,
                                    Filter);
      Check_OpenGL_Error;
   end Set_Minifying_Filter;

   function Minifying_Filter (Target : Texture_Target) return Minifying_Function is
      function Convert is new Ada.Unchecked_Conversion (Source => Int,
                                                        Target => Minifying_Function);

      Arr : Low_Level.Int_Array (1 .. 1);
   begin
      API.Get_Tex_Parameter_Int (Target.Kind, Enums.Textures.Min_Filter,
                                 Arr);
      Check_OpenGL_Error;
      return Convert (Arr (1));
   end Minifying_Filter;

   procedure Set_Magnifying_Filter (Target : Texture_Target;
                                    Filter : Magnifying_Function) is
   begin
      API.Tex_Parameter_Mag_Filter (Target.Kind, Enums.Textures.Mag_Filter,
                                    Filter);
      Check_OpenGL_Error;
   end Set_Magnifying_Filter;

   function Magnifying_Filter (Target : Texture_Target)
                               return Magnifying_Function is
      function Convert is new Ada.Unchecked_Conversion (Source => Int,
                                                        Target => Minifying_Function);

      Arr : Low_Level.Int_Array (1 .. 1);
   begin
      API.Get_Tex_Parameter_Int (Target.Kind, Enums.Textures.Mag_Filter,
                                 Arr);
      Check_OpenGL_Error;
      return Convert (Arr (1));
   end Magnifying_Filter;

   procedure Set_Minimum_LoD (Target : Texture_Target; Level : Double) is
   begin
      API.Tex_Parameter_Float (Target.Kind, Enums.Textures.Min_LoD,
                               Single (Level));
      Check_OpenGL_Error;
   end Set_Minimum_LoD;

   function Minimum_LoD (Target : Texture_Target) return Double is
      Arr : Low_Level.Single_Array (1 .. 1);
   begin
      API.Get_Tex_Parameter_Float (Target.Kind, Enums.Textures.Min_LoD,
                                   Arr);
      Check_OpenGL_Error;
      return Double (Arr (1));
   end Minimum_LoD;

   procedure Set_Maximum_LoD (Target : Texture_Target; Level : Double) is
   begin
      API.Tex_Parameter_Float (Target.Kind, Enums.Textures.Max_LoD,
                               Single (Level));
      Check_OpenGL_Error;
   end Set_Maximum_LoD;

   function Maximum_LoD (Target : Texture_Target) return Double is
      Arr : Low_Level.Single_Array (1 .. 1);
   begin
      API.Get_Tex_Parameter_Float (Target.Kind, Enums.Textures.Max_LoD,
                                   Arr);
      Check_OpenGL_Error;
      return Double (Arr (1));
   end Maximum_LoD;

   procedure Set_Lowest_Mipmap_Level (Target : Texture_Target; Level : Integer) is
   begin
      API.Tex_Parameter_Int (Target.Kind, Enums.Textures.Base_Level,
                             Int (Level));
      Check_OpenGL_Error;
   end Set_Lowest_Mipmap_Level;

   function Lowest_Mipmap_Level (Target : Texture_Target) return Integer is
      Arr : Low_Level.Int_Array (1 .. 1);
   begin
      API.Get_Tex_Parameter_Int (Target.Kind, Enums.Textures.Base_Level,
                                 Arr);
      Check_OpenGL_Error;
      return Integer (Arr (1));
   end Lowest_Mipmap_Level;

   procedure Set_Highest_Mipmap_Level (Target : Texture_Target; Level : Integer) is
   begin
      API.Tex_Parameter_Int (Target.Kind, Enums.Textures.Max_Level,
                             Int (Level));
      Check_OpenGL_Error;
   end Set_Highest_Mipmap_Level;

   function Highest_Mipmap_Level (Target : Texture_Target) return Integer is
      Arr : Low_Level.Int_Array (1 .. 1);
   begin
      API.Get_Tex_Parameter_Int (Target.Kind, Enums.Textures.Max_Level,
                                 Arr);
      Check_OpenGL_Error;
      return Integer (Arr (1));
   end Highest_Mipmap_Level;

   procedure Set_X_Wrapping (Target : Texture_Target; Mode : Wrapping_Mode) is
   begin
      API.Tex_Parameter_Wrap_Mode (Target.Kind, Enums.Textures.Wrap_S,
                                   Mode);
      Check_OpenGL_Error;
   end Set_X_Wrapping;

   function X_Wrapping (Target : Texture_Target) return Wrapping_Mode is
      Ret : Wrapping_Mode;
   begin
      API.Get_Tex_Parameter_Wrap_Mode (Target.Kind, Enums.Textures.Wrap_S,
                                       Ret);
      Check_OpenGL_Error;
      return Ret;
   end X_Wrapping;

   procedure Set_Y_Wrapping (Target : Texture_Target; Mode : Wrapping_Mode) is
   begin
      API.Tex_Parameter_Wrap_Mode (Target.Kind, Enums.Textures.Wrap_T,
                                   Mode);
      Check_OpenGL_Error;
   end Set_Y_Wrapping;

   function Y_Wrapping (Target : Texture_Target) return Wrapping_Mode is
      Ret : Wrapping_Mode;
   begin
      API.Get_Tex_Parameter_Wrap_Mode (Target.Kind, Enums.Textures.Wrap_T,
                                       Ret);
      Check_OpenGL_Error;
      return Ret;
   end Y_Wrapping;

   procedure Set_Z_Wrapping (Target : Texture_Target; Mode : Wrapping_Mode) is
   begin
      API.Tex_Parameter_Wrap_Mode (Target.Kind, Enums.Textures.Wrap_R,
                                   Mode);
      Check_OpenGL_Error;
   end Set_Z_Wrapping;

   function Z_Wrapping (Target : Texture_Target) return Wrapping_Mode is
      Ret : Wrapping_Mode;
   begin
      API.Get_Tex_Parameter_Wrap_Mode (Target.Kind, Enums.Textures.Wrap_R,
                                       Ret);
      Check_OpenGL_Error;
      return Ret;
   end Z_Wrapping;

   procedure Set_Border_Color (Target : Texture_Target; Color : Colors.Color) is

      Raw : Low_Level.Single_Array := Helpers.Float_Array (Color);
   begin
      API.Tex_Parameter_Floats (Target.Kind, Enums.Textures.Border_Color,
                                Raw);
      Check_OpenGL_Error;
   end Set_Border_Color;

   function Border_Color (Target : Texture_Target) return Colors.Color is
      Raw : Low_Level.Single_Array (1 .. 4);
   begin
      API.Get_Tex_Parameter_Float (Target.Kind, Enums.Textures.Border_Color,
                                   Raw);
      Check_OpenGL_Error;
      return Helpers.Color (Raw);
   end Border_Color;

   procedure Set_Texture_Priority (Target : Texture_Target;
                                   Value : Priority) is
   begin
      API.Tex_Parameter_Float (Target.Kind, Enums.Textures.Priority,
                               Single (Value));
      Check_OpenGL_Error;
   end Set_Texture_Priority;

   function Texture_Priority (Target : Texture_Target) return Priority is
      Arr : Low_Level.Single_Array (1 .. 1);
   begin
      API.Get_Tex_Parameter_Float (Target.Kind, Enums.Textures.Priority, Arr);
      Check_OpenGL_Error;
      return Priority (Arr (1));
   end Texture_Priority;

   procedure Toggle_Compare_X_To_Texture (Target  : Texture_Target;
                                          Enabled : Boolean) is
      Value : Enums.Textures.Compare_Kind;
   begin
      if Enabled then
         Value := Enums.Textures.Compare_R_To_Texture;
      else
         Value := Enums.Textures.None;
      end if;
      API.Tex_Parameter_Comp_Mode (Target.Kind, Enums.Textures.Compare_Mode,
                                   Value);
      Check_OpenGL_Error;
   end Toggle_Compare_X_To_Texture;

   function Compare_X_To_Texture_Enabled (Target : Texture_Target) return Boolean is
      use type Enums.Textures.Compare_Kind;

      Value : Enums.Textures.Compare_Kind;
   begin
      API.Get_Tex_Parameter_Comp_Mode (Target.Kind, Enums.Textures.Compare_Mode,
                                       Value);
      Check_OpenGL_Error;
      return Value = Enums.Textures.Compare_R_To_Texture;
   end Compare_X_To_Texture_Enabled;

   procedure Set_Compare_Function (Target : Texture_Target;
                                   Func   : Compare_Function) is
   begin
      API.Tex_Parameter_Comp_Func (Target.Kind, Enums.Textures.Compare_Func,
                                   Func);
      Check_OpenGL_Error;
   end Set_Compare_Function;

   function Current_Compare_Function (Target : Texture_Target)
                                     return Compare_Function is
      Value : Compare_Function;
   begin
      API.Get_Tex_Parameter_Comp_Func (Target.Kind, Enums.Textures.Compare_Func,
                                       Value);
      Check_OpenGL_Error;
      return Value;
   end Current_Compare_Function;

   procedure Set_Depth_Texture_Mode (Target : Texture_Target; Mode : Depth_Mode) is
   begin
      API.Tex_Parameter_Depth_Mode (Target.Kind, Enums.Textures.Depth, Mode);
      Check_OpenGL_Error;
   end Set_Depth_Texture_Mode;

   function Depth_Texture_Mode (Target : Texture_Target) return Depth_Mode is
      Value : Depth_Mode;
   begin
      API.Get_Tex_Parameter_Depth_Mode (Target.Kind, Enums.Textures.Depth, Value);
      Check_OpenGL_Error;
      return Value;
   end Depth_Texture_Mode;

   procedure Toggle_Mipmap_Autoupdate (Target : Texture_Target; Enabled : Boolean) is
   begin
      API.Tex_Parameter_Bool (Target.Kind, Enums.Textures.Generate_Mipmap,
                              Low_Level.Bool (Enabled));
      Check_OpenGL_Error;
   end Toggle_Mipmap_Autoupdate;

   function Mipmap_Autoupdate_Enabled (Target : Texture_Target) return Boolean is
      Value : Low_Level.Bool;
   begin
      API.Get_Tex_Parameter_Bool (Target.Kind, Enums.Textures.Generate_Mipmap,
                                  Value);
      Check_OpenGL_Error;
      return Boolean (Value);
   end Mipmap_Autoupdate_Enabled;
   
   procedure Set_Active_Unit (Unit : Texture_Unit) is
      package Texture_Indexing is new Enums.Indexes
         (Enums.Textures.Texture_Unit_Start_Rep,
          Enums.Getter.Max_Combined_Texture_Image_Units);
   begin
      API.Active_Texture (Texture_Indexing.Representation (Unit));
      Check_OpenGL_Error;
   end Set_Active_Unit;
   
   function Active_Unit return Texture_Unit is
      package Texture_Indexing is new Enums.Indexes
         (Enums.Textures.Texture_Unit_Start_Rep,
          Enums.Getter.Max_Combined_Texture_Image_Units);
      
      Raw_Unit : aliased Int := Enums.Textures.Texture_Unit_Start_Rep;
   begin
      API.Get_Integer (Enums.Getter.Active_Texture, Raw_Unit'Access);
      return Texture_Indexing.Value (Raw_Unit);
   end Active_Unit;

   function Texture_Unit_Count return Natural is
      Count : aliased Int;
   begin
      API.Get_Integer (Enums.Getter.Max_Combined_Texture_Image_Units,
                       Count'Access);
      return Natural (Count);
   end Texture_Unit_Count;

end GL.Objects.Textures;
