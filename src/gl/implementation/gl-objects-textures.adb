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
with Ada.Containers.Indefinite_Hashed_Maps;

with GL.API;
with GL.Helpers;
with GL.Enums.Getter;
with GL.Enums.Indexes;
with GL.Enums.Textures;

package body GL.Objects.Textures is
   use type Low_Level.Enums.Texture_Kind;

   function Width (Object : Texture_Proxy; Level : Mipmap_Level) return Size is
      Ret : Size;
   begin
      API.Get_Tex_Level_Parameter_Size (Object.Kind, Level,
                                        Enums.Textures.Width, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Width;
   
   function Height (Object : Texture_Proxy; Level : Mipmap_Level) return Size is
      Ret : Size;
   begin
      API.Get_Tex_Level_Parameter_Size (Object.Kind, Level,
                                        Enums.Textures.Height, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Height;
   
   function Depth (Object : Texture_Proxy; Level : Mipmap_Level) return Size is
      Ret : Size;
   begin
      API.Get_Tex_Level_Parameter_Size (Object.Kind, Level,
                                        Enums.Textures.Depth_Size, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Depth;
   
   function Format (Object : Texture_Proxy; Level : Mipmap_Level)
                    return Pixels.Internal_Format is
      Ret : Pixels.Internal_Format;
   begin
      API.Get_Tex_Level_Parameter_Format (Object.Kind, Level,
                                          Enums.Textures.Internal_Format, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Format;
   
   function Red_Type (Object : Texture_Proxy; Level : Mipmap_Level)
                      return Pixels.Channel_Data_Type is
      Ret : Pixels.Channel_Data_Type;
   begin
      API.Get_Tex_Level_Parameter_Type (Object.Kind, Level,
                                        Enums.Textures.Red_Type, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Red_Type;
   
   function Green_Type (Object : Texture_Proxy; Level : Mipmap_Level)
                        return Pixels.Channel_Data_Type is
      Ret : Pixels.Channel_Data_Type;
   begin
      API.Get_Tex_Level_Parameter_Type (Object.Kind, Level,
                                        Enums.Textures.Green_Type, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Green_Type;
   
   function Blue_Type (Object : Texture_Proxy; Level : Mipmap_Level)
                       return Pixels.Channel_Data_Type is
      Ret : Pixels.Channel_Data_Type;
   begin
        API.Get_Tex_Level_Parameter_Type (Object.Kind, Level,
                                          Enums.Textures.Blue_Type, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Blue_Type;
   
   function Alpha_Type (Object : Texture_Proxy; Level : Mipmap_Level)
                        return Pixels.Channel_Data_Type is
      Ret : Pixels.Channel_Data_Type;
   begin
      API.Get_Tex_Level_Parameter_Type (Object.Kind, Level,
                                        Enums.Textures.Alpha_Type, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Alpha_Type;
   
   function Depth_Type (Object : Texture_Proxy; Level : Mipmap_Level)
                        return Pixels.Channel_Data_Type is
      Ret : Pixels.Channel_Data_Type;
   begin
      API.Get_Tex_Level_Parameter_Type (Object.Kind, Level,
                                        Enums.Textures.Depth_Type, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Depth_Type;
   
   function Red_Size (Object : Texture_Proxy; Level : Mipmap_Level)
                      return Size is
      Ret : Size;
   begin
      API.Get_Tex_Level_Parameter_Size (Object.Kind, Level,
                                        Enums.Textures.Red_Size, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Red_Size;
   
   function Green_Size (Object : Texture_Proxy; Level : Mipmap_Level)
                        return Size is
      Ret : Size;
   begin
      API.Get_Tex_Level_Parameter_Size (Object.Kind, Level,
                                        Enums.Textures.Green_Size, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Green_Size;
   
   function Blue_Size (Object : Texture_Proxy; Level : Mipmap_Level)
                       return Size is
      Ret : Size;
   begin
      API.Get_Tex_Level_Parameter_Size (Object.Kind, Level,
                                        Enums.Textures.Blue_Size, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Blue_Size;
   
   function Alpha_Size (Object : Texture_Proxy; Level : Mipmap_Level)
                        return Size is
      Ret : Size;
   begin
      API.Get_Tex_Level_Parameter_Size (Object.Kind, Level,
                                        Enums.Textures.Alpha_Size, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Alpha_Size;
   
   function Depth_Size (Object : Texture_Proxy; Level : Mipmap_Level)
                        return Size is
      Ret : Size;
   begin
      API.Get_Tex_Level_Parameter_Size (Object.Kind, Level,
                                        Enums.Textures.Depth_Size, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Depth_Size;
   
   function Compressed (Object : Texture_Proxy; Level : Mipmap_Level)
                        return Boolean is
      Ret : Low_Level.Bool;
   begin
      API.Get_Tex_Level_Parameter_Bool (Object.Kind, Level,
                                        Enums.Textures.Compressed, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Boolean (Ret);
   end Compressed;
   
   function Compressed_Image_Size (Object : Texture_Proxy; Level : Mipmap_Level)
                                   return Size is
      Ret : Size;
   begin
      API.Get_Tex_Level_Parameter_Size (Object.Kind, Level,
                                        Enums.Textures.Compressed_Image_Size,
                                        Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Compressed_Image_Size;
   
   function Raw_Kind (Object : Texture_Proxy)
                      return Low_Level.Enums.Texture_Kind is
   begin
      return Object.Kind;
   end Raw_Kind;
   
   function Hash (Key : Low_Level.Enums.Texture_Kind)
     return Ada.Containers.Hash_Type is
      function Value is new Ada.Unchecked_Conversion
        (Source => Low_Level.Enums.Texture_Kind, Target => Low_Level.Enum);
   begin
      return Ada.Containers.Hash_Type (Value (Key));
   end Hash;

   package Texture_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type     => Low_Level.Enums.Texture_Kind,
       Element_Type => Texture'Class,
       Hash         => Hash,
       Equivalent_Keys => Low_Level.Enums."=");
   use type Texture_Maps.Cursor;

   Current_Textures : Texture_Maps.Map;
   
   procedure Bind (Target : Texture_Target; Object : Texture'Class) is
      Cursor : constant Texture_Maps.Cursor
        := Current_Textures.Find (Target.Kind);
   begin
      if Cursor = Texture_Maps.No_Element or else
        Texture_Maps.Element (Cursor).Reference.GL_Id /= Object.Reference.GL_Id
        then
         API.Bind_Texture (Target.Kind, Object.Reference.GL_Id);
         Raise_Exception_On_OpenGL_Error;
         if Cursor = Texture_Maps.No_Element then
            Current_Textures.Insert (Target.Kind, Object);
         else
            Current_Textures.Replace_Element (Cursor, Object);
         end if;
      end if;
   end Bind;

   function Current_Texture (Target : Texture_Target) return Texture'Class is
      Cursor : constant Texture_Maps.Cursor
        := Current_Textures.Find (Target.Kind);
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
      Raise_Exception_On_OpenGL_Error;
      Object.Reference.GL_Id := New_Id (1);
      Object.Reference.Initialized := True;
   end Initialize_Id;

   procedure Delete_Id (Object : in out Texture) is
      Arr : constant Low_Level.UInt_Array := (1 => Object.Reference.GL_Id);
   begin
      API.Delete_Textures (1, Arr);
      Raise_Exception_On_OpenGL_Error;
      Object.Reference.GL_Id := 0;
      Object.Reference.Initialized := False;
   end Delete_Id;
   
   procedure Invalidate_Image (Object : Texture; Level : Mipmap_Level) is
   begin
      API.Invalidate_Tex_Image (Object.Reference.GL_Id, Level);
      Raise_Exception_On_OpenGL_Error;
   end Invalidate_Image;
   
   procedure Invalidate_Sub_Image (Object : Texture; Level : Mipmap_Level;
                                   X, Y, Z : Int; Width, Height, Depth : Size)
   is
   begin
      API.Invalidate_Tex_Sub_Image (Object.Reference.GL_Id, Level, X, Y, Z,
                                    Width, Height, Depth);
      Raise_Exception_On_OpenGL_Error;
   end Invalidate_Sub_Image;

   procedure Set_Minifying_Filter (Target : Texture_Target;
                                   Filter : Minifying_Function) is
   begin
      API.Tex_Parameter_Min_Filter (Target.Kind, Enums.Textures.Min_Filter,
                                    Filter);
      Raise_Exception_On_OpenGL_Error;
   end Set_Minifying_Filter;

   function Minifying_Filter (Target : Texture_Target)
                              return Minifying_Function is
      function Convert is new Ada.Unchecked_Conversion
        (Source => Int, Target => Minifying_Function);

      Ret : Int;
   begin
      API.Get_Tex_Parameter_Int (Target.Kind, Enums.Textures.Min_Filter,
                                 Ret);
      Raise_Exception_On_OpenGL_Error;
      return Convert (Ret);
   end Minifying_Filter;

   procedure Set_Magnifying_Filter (Target : Texture_Target;
                                    Filter : Magnifying_Function) is
   begin
      API.Tex_Parameter_Mag_Filter (Target.Kind, Enums.Textures.Mag_Filter,
                                    Filter);
      Raise_Exception_On_OpenGL_Error;
   end Set_Magnifying_Filter;

   function Magnifying_Filter (Target : Texture_Target)
                               return Magnifying_Function is
      function Convert is new Ada.Unchecked_Conversion
        (Source => Int, Target => Minifying_Function);

      Ret : Int;
   begin
      API.Get_Tex_Parameter_Int (Target.Kind, Enums.Textures.Mag_Filter,
                                 Ret);
      Raise_Exception_On_OpenGL_Error;
      return Convert (Ret);
   end Magnifying_Filter;

   procedure Set_Minimum_LoD (Target : Texture_Target; Level : Double) is
   begin
      API.Tex_Parameter_Float (Target.Kind, Enums.Textures.Min_LoD,
                               Single (Level));
      Raise_Exception_On_OpenGL_Error;
   end Set_Minimum_LoD;

   function Minimum_LoD (Target : Texture_Target) return Double is
      Ret : Single;
   begin
      API.Get_Tex_Parameter_Float (Target.Kind, Enums.Textures.Min_LoD,
                                   Ret);
      Raise_Exception_On_OpenGL_Error;
      return Double (Ret);
   end Minimum_LoD;

   procedure Set_Maximum_LoD (Target : Texture_Target; Level : Double) is
   begin
      API.Tex_Parameter_Float (Target.Kind, Enums.Textures.Max_LoD,
                               Single (Level));
      Raise_Exception_On_OpenGL_Error;
   end Set_Maximum_LoD;

   function Maximum_LoD (Target : Texture_Target) return Double is
      Ret : Single;
   begin
      API.Get_Tex_Parameter_Float (Target.Kind, Enums.Textures.Max_LoD,
                                   Ret);
      Raise_Exception_On_OpenGL_Error;
      return Double (Ret);
   end Maximum_LoD;

   procedure Set_Lowest_Mipmap_Level (Target : Texture_Target;
                                      Level : Mipmap_Level) is
   begin
      API.Tex_Parameter_Int (Target.Kind, Enums.Textures.Base_Level, Level);
      Raise_Exception_On_OpenGL_Error;
   end Set_Lowest_Mipmap_Level;

   function Lowest_Mipmap_Level (Target : Texture_Target) return Mipmap_Level is
      Ret : Int;
   begin
      API.Get_Tex_Parameter_Int (Target.Kind, Enums.Textures.Base_Level,
                                 Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Lowest_Mipmap_Level;

   procedure Set_Highest_Mipmap_Level (Target : Texture_Target;
                                       Level : Mipmap_Level) is
   begin
      API.Tex_Parameter_Int (Target.Kind, Enums.Textures.Max_Level, Level);
      Raise_Exception_On_OpenGL_Error;
   end Set_Highest_Mipmap_Level;

   function Highest_Mipmap_Level (Target : Texture_Target)
                                  return Mipmap_Level is
      Ret : Int;
   begin
      API.Get_Tex_Parameter_Int (Target.Kind, Enums.Textures.Max_Level,
                                 Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Highest_Mipmap_Level;

   procedure Set_X_Wrapping (Target : Texture_Target; Mode : Wrapping_Mode) is
   begin
      API.Tex_Parameter_Wrap_Mode (Target.Kind, Enums.Textures.Wrap_S,
                                   Mode);
      Raise_Exception_On_OpenGL_Error;
   end Set_X_Wrapping;

   function X_Wrapping (Target : Texture_Target) return Wrapping_Mode is
      Ret : Wrapping_Mode;
   begin
      API.Get_Tex_Parameter_Wrap_Mode (Target.Kind, Enums.Textures.Wrap_S,
                                       Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end X_Wrapping;

   procedure Set_Y_Wrapping (Target : Texture_Target; Mode : Wrapping_Mode) is
   begin
      API.Tex_Parameter_Wrap_Mode (Target.Kind, Enums.Textures.Wrap_T,
                                   Mode);
      Raise_Exception_On_OpenGL_Error;
   end Set_Y_Wrapping;

   function Y_Wrapping (Target : Texture_Target) return Wrapping_Mode is
      Ret : Wrapping_Mode;
   begin
      API.Get_Tex_Parameter_Wrap_Mode (Target.Kind, Enums.Textures.Wrap_T,
                                       Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Y_Wrapping;

   procedure Set_Z_Wrapping (Target : Texture_Target; Mode : Wrapping_Mode) is
   begin
      API.Tex_Parameter_Wrap_Mode (Target.Kind, Enums.Textures.Wrap_R,
                                   Mode);
      Raise_Exception_On_OpenGL_Error;
   end Set_Z_Wrapping;

   function Z_Wrapping (Target : Texture_Target) return Wrapping_Mode is
      Ret : Wrapping_Mode;
   begin
      API.Get_Tex_Parameter_Wrap_Mode (Target.Kind, Enums.Textures.Wrap_R,
                                       Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Z_Wrapping;

   procedure Set_Border_Color (Target : Texture_Target; Color : Colors.Color) is

      Raw : constant Low_Level.Single_Array := Helpers.Float_Array (Color);
   begin
      API.Tex_Parameter_Floats (Target.Kind, Enums.Textures.Border_Color,
                                Raw);
      Raise_Exception_On_OpenGL_Error;
   end Set_Border_Color;

   function Border_Color (Target : Texture_Target) return Colors.Color is
      Raw : Low_Level.Single_Array (1 .. 4);
   begin
      API.Get_Tex_Parameter_Floats (Target.Kind, Enums.Textures.Border_Color,
                                    Raw);
      Raise_Exception_On_OpenGL_Error;
      return Helpers.Color (Raw);
   end Border_Color;

   procedure Set_Texture_Priority (Target : Texture_Target;
                                   Value : Priority) is
   begin
      API.Tex_Parameter_Float (Target.Kind, Enums.Textures.Priority,
                               Single (Value));
      Raise_Exception_On_OpenGL_Error;
   end Set_Texture_Priority;

   function Texture_Priority (Target : Texture_Target) return Priority is
      Ret : Single;
   begin
      API.Get_Tex_Parameter_Float (Target.Kind, Enums.Textures.Priority, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Priority (Ret);
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
      Raise_Exception_On_OpenGL_Error;
   end Toggle_Compare_X_To_Texture;

   function Compare_X_To_Texture_Enabled (Target : Texture_Target)
                                          return Boolean is
      use type Enums.Textures.Compare_Kind;

      Value : Enums.Textures.Compare_Kind;
   begin
      API.Get_Tex_Parameter_Comp_Mode (Target.Kind, Enums.Textures.Compare_Mode,
                                       Value);
      Raise_Exception_On_OpenGL_Error;
      return Value = Enums.Textures.Compare_R_To_Texture;
   end Compare_X_To_Texture_Enabled;

   procedure Set_Compare_Function (Target : Texture_Target;
                                   Func   : Compare_Function) is
   begin
      API.Tex_Parameter_Comp_Func (Target.Kind, Enums.Textures.Compare_Func,
                                   Func);
      Raise_Exception_On_OpenGL_Error;
   end Set_Compare_Function;

   function Current_Compare_Function (Target : Texture_Target)
                                     return Compare_Function is
      Value : Compare_Function;
   begin
      API.Get_Tex_Parameter_Comp_Func (Target.Kind, Enums.Textures.Compare_Func,
                                       Value);
      Raise_Exception_On_OpenGL_Error;
      return Value;
   end Current_Compare_Function;

   procedure Set_Depth_Texture_Mode (Target : Texture_Target;
                                     Mode : Depth_Mode) is
   begin
      API.Tex_Parameter_Depth_Mode (Target.Kind, Enums.Textures.Depth, Mode);
      Raise_Exception_On_OpenGL_Error;
   end Set_Depth_Texture_Mode;

   function Depth_Texture_Mode (Target : Texture_Target) return Depth_Mode is
      Value : Depth_Mode;
   begin
      API.Get_Tex_Parameter_Depth_Mode (Target.Kind, Enums.Textures.Depth,
                                        Value);
      Raise_Exception_On_OpenGL_Error;
      return Value;
   end Depth_Texture_Mode;

   procedure Toggle_Mipmap_Autoupdate (Target : Texture_Target;
                                       Enabled : Boolean) is
   begin
      API.Tex_Parameter_Bool (Target.Kind, Enums.Textures.Generate_Mipmap,
                              Low_Level.Bool (Enabled));
      Raise_Exception_On_OpenGL_Error;
   end Toggle_Mipmap_Autoupdate;

   function Mipmap_Autoupdate_Enabled (Target : Texture_Target)
                                       return Boolean is
      Value : Low_Level.Bool;
   begin
      API.Get_Tex_Parameter_Bool (Target.Kind, Enums.Textures.Generate_Mipmap,
                                  Value);
      Raise_Exception_On_OpenGL_Error;
      return Boolean (Value);
   end Mipmap_Autoupdate_Enabled;
   
   procedure Generate_Mipmap (Target : Texture_Target) is
   begin
      API.Generate_Mipmap (Target.Kind);
      Raise_Exception_On_OpenGL_Error;
   end Generate_Mipmap;
   
   function Raw_Type (Target : Texture_Target)
                      return Low_Level.Enums.Texture_Kind is
   begin
      return Target.Kind;
   end Raw_Type;
   
   procedure Set_Active_Unit (Unit : Texture_Unit) is
      package Texture_Indexing is new Enums.Indexes
         (Enums.Textures.Texture_Unit_Start_Rep,
          Enums.Getter.Max_Combined_Texture_Image_Units);
   begin
      API.Active_Texture (Texture_Indexing.Representation (Unit));
      Raise_Exception_On_OpenGL_Error;
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
