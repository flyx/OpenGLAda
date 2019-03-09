--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with System;

with SOIL.API;

package body SOIL.Images is
   use type GL.Types.UInt;
   use type GL.Objects.Textures.Image_Source;

   function Null_Image return GL.Objects.Textures.Image_Source is
   begin
      return GL.Objects.Textures.Image_Source (System.Null_Address);
   end Null_Image;
   pragma Inline (Null_Image);

   overriding procedure Initialize (Object : in out Image) is
   begin
      Object.Reference := new Image_Data'(Pointer         => Null_Image,
                                          Width           => 0,
                                          Height          => 0,
                                          Channels        => L,
                                          Reference_Count => 1);
   end Initialize;

   overriding procedure Adjust (Object : in out Image) is
   begin
      Object.Reference.Reference_Count := Object.Reference.Reference_Count + 1;
   end Adjust;

   overriding procedure Finalize (Object : in out Image) is
   begin
      if Object.Reference.Reference_Count = 1 then
         if Object.Reference.Pointer /= Null_Image then
            API.Free_Image_Data (Object.Reference.Pointer);
            Object.Reference.Pointer := Null_Image;
         end if;
      else
         Object.Reference.Reference_Count
           := Object.Reference.Reference_Count - 1;
      end if;
   end Finalize;

   procedure Load (Object : in out Image; File_Name : String) is
   begin
      if Object.Reference.Pointer /= Null_Image then
         API.Free_Image_Data (Object.Reference.Pointer);
      end if;
      Object.Reference.Pointer := API.Load_Image
        (Interfaces.C.To_C (File_Name), Object.Reference.Width'Access,
         Object.Reference.Height'Access,
         Object.Reference.Channels'Access, Auto);
      if Object.Reference.Pointer = Null_Image then
         raise SOIL_Error with Last_Error;
      end if;
   end Load;

   procedure Load (Object          : in out Image;
                   File_Name       : String;
                   Force_Format    : Explicit_Image_Format;
                   Original_Format : out Explicit_Image_Format) is

      Tmp_Original_Format : aliased Explicit_Image_Format;
   begin
      if Object.Reference.Pointer /= Null_Image then
         API.Free_Image_Data (Object.Reference.Pointer);
      end if;
      Object.Reference.Pointer := API.Load_Image
        (Interfaces.C.To_C (File_Name), Object.Reference.Width'Access,
         Object.Reference.Height'Access,
         Tmp_Original_Format'Access, Auto);
      Original_Format := Tmp_Original_Format;
      if Object.Reference.Pointer = Null_Image then
         raise SOIL_Error with Last_Error;
      end if;
      Object.Reference.Channels := Force_Format;
   end Load;

   procedure Save (Object : in out Image; File_Name : String;
                   Image_Type : Image_Save_Type) is

      Result : Bool;
   begin
      if Object.Reference.Pointer = Null_Image then
         raise SOIL_Error with "No image loaded!";
      end if;
      Result := API.Save_Image
        (Interfaces.C.To_C (File_Name), Image_Type, Object.Reference.Width,
         Object.Reference.Height, Object.Reference.Channels,
         Object.Reference.Pointer);
      if not Result then
         raise SOIL_Error with Last_Error;
      end if;
   end Save;

   function Width (Object : Image) return GL.Types.Int is
   begin
      return Object.Reference.Width;
   end Width;

   function Height (Object : Image) return GL.Types.Int is
   begin
      return Object.Reference.Height;
   end Height;

   function Channels (Object : Image) return Explicit_Image_Format is
   begin
      return Object.Reference.Channels;
   end Channels;

   function Loaded   (Object : Image) return Boolean is
   begin
      return Object.Reference.Pointer /= Null_Image;
   end Loaded;

   function Data     (Object : Image) return GL.Objects.Textures.Image_Source is
   begin
      return Object.Reference.Pointer;
   end Data;

   procedure Clear (Object : in out Image) is
   begin
      if Object.Reference.Pointer /= Null_Image then
         API.Free_Image_Data (Object.Reference.Pointer);
         Object.Reference.Pointer := Null_Image;
      end if;
   end Clear;

   procedure To_Texture (Object  : Image;
                         Texture : in out GL.Objects.Textures.Texture'Class;
                         Flags   : Texture_Flags) is
      Raw_Id : GL.Types.UInt;
   begin
      if Object.Reference.Pointer = Null_Image then
         raise SOIL_Error with "No image loaded!";
      end if;
      Raw_Id := API.Create_OGL_Texture (Object.Reference.Pointer,
                                        Object.Reference.Width,
                                        Object.Reference.Height,
                                        Object.Reference.Channels,
                                        Input_Id (Texture), Flags);
      if Raw_Id = 0 then
         raise SOIL_Error with Last_Error;
      end if;
      Texture.Set_Raw_Id (Raw_Id);
   end To_Texture;

   procedure To_Cubemap (Object     : Image;
                         Texture    : in out GL.Objects.Textures.Texture'Class;
                         Face_Order : Cubemap_Layout;
                         Flags      : Texture_Flags) is
      Raw_Id : GL.Types.UInt;
   begin
      if Object.Reference.Pointer = Null_Image then
         raise SOIL_Error with "No image loaded!";
      end if;
      Raw_Id := API.Create_OGL_Single_Cubemap (Object.Reference.Pointer,
                                               Object.Reference.Width,
                                               Object.Reference.Height,
                                               Object.Reference.Channels,
                                               Face_Order, Input_Id (Texture),
                                               Flags);
      if Raw_Id = 0 then
         raise SOIL_Error with Last_Error;
      end if;
      Texture.Set_Raw_Id (Raw_Id);
   end To_Cubemap;

end SOIL.Images;
