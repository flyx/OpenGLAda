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

with Interfaces.C.Strings;

with SOIL.API;

package body SOIL.Images is
   use type GL.Types.UInt;

   overriding procedure Initialize (Object : in out Image) is
   begin
      Object.Reference := new Image_Data'(Pointer         => System.Null_Address,
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
      use type System.Address;
   begin
      if Object.Reference.Reference_Count = 1 then
         if Object.Reference.Pointer /= System.Null_Address then
            API.Free_Image_Data (Object.Reference.Pointer);
            Object.Reference.Pointer := System.Null_Address;
         end if;
      else
         Object.Reference.Reference_Count
           := Object.Reference.Reference_Count - 1;
      end if;
   end Finalize;

   procedure Load (Object : in out Image; File_Name : String) is
      use type System.Address;
   begin
      if Object.Reference.Pointer /= System.Null_Address then
         API.Free_Image_Data (Object.Reference.Pointer);
      end if;
      Object.Reference.Pointer := API.Load_Image
        (Interfaces.C.To_C (File_Name), Object.Reference.Width'Access,
         Object.Reference.Height'Access,
         Object.Reference.Channels'Access, Auto);
      if Object.Reference.Pointer = System.Null_Address then
         raise SOIL_Error with Last_Error;
      end if;
   end Load;

   procedure Load (Object          : in out Image;
                   File_Name       : String;
                   Force_Format    : Explicit_Image_Format;
                   Original_Format : out Explicit_Image_Format) is
      use type System.Address;

      Tmp_Original_Format : aliased Explicit_Image_Format;
   begin
      if Object.Reference.Pointer /= System.Null_Address then
         API.Free_Image_Data (Object.Reference.Pointer);
      end if;
      Object.Reference.Pointer := API.Load_Image
        (Interfaces.C.To_C (File_Name), Object.Reference.Width'Access,
         Object.Reference.Height'Access,
         Tmp_Original_Format'Access, Auto);
      Original_Format := Tmp_Original_Format;
      if Object.Reference.Pointer = System.Null_Address then
         raise SOIL_Error with Last_Error;
      end if;
      Object.Reference.Channels := Force_Format;
   end Load;

   procedure Save (Object : in out Image; File_Name : String;
                   Image_Type : Image_Save_Type) is
      use type System.Address;

      Result : Bool;
   begin
      if Object.Reference.Pointer = System.Null_Address then
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
      use type System.Address;
   begin
      return Object.Reference.Pointer /= System.Null_Address;
   end Loaded;

   procedure Clear (Object : in out Image) is
      use type System.Address;
   begin
      if Object.Reference.Pointer /= System.Null_Address then
         API.Free_Image_Data (Object.Reference.Pointer);
         Object.Reference.Pointer := System.Null_Address;
      end if;
   end Clear;

   procedure To_Texture (Object  : Image;
                         Texture : in out GL.Objects.Textures.Texture'Class;
                         Flags   : Texture_Flags) is
      use type System.Address;
      Raw_Id : GL.Types.UInt;
   begin
      if Object.Reference.Pointer = System.Null_Address then
         raise SOIL_Error with "No image loaded!";
      end if;
      Raw_Id := API.Create_OGL_Texture (Object.Reference.Pointer,
                                        Object.Reference.Width,
                                        Object.Reference.Height,
                                        Object.Reference.Channels,
                                        Texture.Raw_Id, Flags);
      if Raw_Id = 0 then
         raise SOIL_Error with Last_Error;
      end if;
      Texture.Set_Raw_Id (Raw_Id);
   end To_Texture;

   procedure To_Cubemap (Object     : Image;
                         Texture    : in out GL.Objects.Textures.Texture'Class;
                         Face_Order : Cubemap_Layout;
                         Flags      : Texture_Flags) is
      use type System.Address;
      Raw_Id : GL.Types.UInt;
   begin
      if Object.Reference.Pointer = System.Null_Address then
         raise SOIL_Error with "No image loaded!";
      end if;
      Raw_Id := API.Create_OGL_Single_Cubemap (Object.Reference.Pointer,
                                               Object.Reference.Width,
                                               Object.Reference.Height,
                                               Object.Reference.Channels,
                                               Face_Order, Texture.Raw_Id,
                                               Flags);
      if Raw_Id = 0 then
         raise SOIL_Error with Last_Error;
      end if;
      Texture.Set_Raw_Id (Raw_Id);
   end To_Cubemap;

end SOIL.Images;
