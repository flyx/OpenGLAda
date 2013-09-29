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

package body SOIL is
   use type GL.Types.UInt;

   procedure Load_File_To_Texture
     (File_Name : String;
      Texture   : in out GL.Objects.Textures.Texture;
      Format    : Image_Format  := Auto;
      Flags     : Texture_Flags := (others => False)) is

      C_File_Name : Interfaces.C.Strings.chars_ptr
        := Interfaces.C.Strings.New_String (File_Name);
      Raw_Id : GL.Types.UInt
        := API.Load_OGL_Texture (C_File_Name, Format, Texture.Raw_Id, Flags);
   begin
      Interfaces.C.Strings.Free (C_File_Name);
      if Raw_Id = 0 then
         raise SOIL_Error with Last_Error;
      end if;
      Texture.Set_Raw_Id (Raw_Id);
   end Load_File_To_Texture;

   procedure Load_Files_To_Cubemap
     (Pos_X_File, Neg_X_File, Pos_Y_File, Neg_Y_File,
      Pos_Z_File, Neg_Z_File : String;
      Texture : in out GL.Objects.Textures.Texture;
      Format  : Image_Format  := Auto;
      Flags   : Texture_Flags := (others => False)) is

      use Interfaces.C.Strings;

      C_Pos_X : chars_ptr := New_String (Pos_X_File);
      C_Neg_X : chars_ptr := New_String (Neg_X_File);
      C_Pos_Y : chars_ptr := New_String (Pos_Y_File);
      C_Neg_Y : chars_ptr := New_String (Neg_Y_File);
      C_Pos_Z : chars_ptr := New_String (Pos_Z_File);
      C_Neg_Z : chars_ptr := New_String (Neg_Z_File);

      Raw_Id : GL.Types.UInt := API.Load_OGL_Cubemap
        (C_Pos_X, C_Neg_X, C_Pos_Y, C_Neg_Y, C_Pos_Z, C_Neg_Z, Format,
         Texture.Raw_Id, Flags);
   begin
      Free (C_Pos_X);
      Free (C_Neg_X);
      Free (C_Pos_Y);
      Free (C_Neg_Y);
      Free (C_Pos_Z);
      Free (C_Neg_Z);
      if Raw_Id = 0 then
         raise SOIL_Error with Last_Error;
      end if;
      Texture.Set_Raw_Id (Raw_Id);
   end Load_Files_To_Cubemap;

   procedure Load_File_To_Cubemap
     (File_Name  : String;
      Texture    : in out GL.Objects.Textures.Texture;
      Face_Order : Cubemap_Layout := "EWUDNS";
      Format     : Image_Format   := Auto;
      Flags      : Texture_Flags  := (others => False)) is

      C_File_Name : Interfaces.C.Strings.chars_ptr
        := Interfaces.C.Strings.New_String (File_Name);
      Raw_Id : GL.Types.UInt
        := API.Load_OGL_Single_Cubemap (C_File_Name, Face_Order, Format,
                                        Texture.Raw_Id, Flags);
   begin
      Interfaces.C.Strings.Free (C_File_Name);
      if Raw_Id = 0 then
         raise SOIL_Error with Last_Error;
      end if;
      Texture.Set_Raw_Id (Raw_Id);
   end Load_File_To_Cubemap;

   procedure Load_HDR_Texture
     (File_Name      : String;
      Texture        : in out GL.Objects.Textures.Texture;
      Format         : Fake_HDR_Representation;
      Rescale_To_Max : Boolean;
      Flags          : Texture_Flags := (others => False)) is

      C_File_Name : Interfaces.C.Strings.chars_ptr
        := Interfaces.C.Strings.New_String (File_Name);
      Raw_Id : GL.Types.UInt := API.Load_OGL_HDR_Texture
        (C_File_Name, Format, Bool (Rescale_To_Max), Texture.Raw_Id, Flags);
   begin
      Interfaces.C.Strings.Free (C_File_Name);
      if Raw_Id = 0 then
         raise SOIL_Error with Last_Error;
      end if;
      Texture.Set_Raw_Id (Raw_Id);
   end Load_HDR_Texture;

   procedure Save_Screenshot (File_Name  : String;
                              Image_Type : Image_Save_Type;
                              X, Y, Width, Height : GL.Types.Int) is
      C_File_Name : Interfaces.C.Strings.chars_ptr
        := Interfaces.C.Strings.New_String (File_Name);
      Result : Bool := API.Save_Screenshot
        (C_File_Name, Image_Type, X, Y, Width, Height);
   begin
      Interfaces.C.Strings.Free (C_File_Name);
      if not Result then
         raise SOIL_Error with Last_Error;
      end if;
   end Save_Screenshot;

   function Last_Error return String is
   begin
      return Interfaces.C.Strings.Value (API.Last_Result);
   end Last_Error;
end SOIL;
