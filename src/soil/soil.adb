--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Interfaces.C.Strings;

with SOIL.API;

package body SOIL is
   use type GL.Types.UInt;

   function Input_Id (Texture : GL.Objects.Textures.Texture'Class)
                      return GL.Types.UInt is
   begin
      if Texture.Initialized then
         return Texture.Raw_Id;
      else
         return 0;
      end if;
   end Input_Id;

   procedure Load_File_To_Texture
     (File_Name : String;
      Texture   : in out GL.Objects.Textures.Texture'Class;
      Channels  : Image_Format  := Auto;
      Flags     : Texture_Flags := (others => False)) is

      Raw_Id : constant GL.Types.UInt
        := API.Load_OGL_Texture (Interfaces.C.To_C (File_Name),
                                 Channels, Input_Id (Texture), Flags);
   begin
      if Raw_Id = 0 then
         raise SOIL_Error with Last_Error;
      end if;
      Texture.Set_Raw_Id (Raw_Id);
   end Load_File_To_Texture;

   procedure Load_Files_To_Cubemap
     (Pos_X_File, Neg_X_File, Pos_Y_File, Neg_Y_File,
      Pos_Z_File, Neg_Z_File : String;
      Texture  : in out GL.Objects.Textures.Texture'Class;
      Channels : Image_Format  := Auto;
      Flags    : Texture_Flags := (others => False)) is

      Raw_Id : constant GL.Types.UInt := API.Load_OGL_Cubemap
        (Interfaces.C.To_C (Pos_X_File),
         Interfaces.C.To_C (Neg_X_File),
         Interfaces.C.To_C (Pos_Y_File),
         Interfaces.C.To_C (Neg_Y_File),
         Interfaces.C.To_C (Pos_Z_File),
         Interfaces.C.To_C (Neg_Z_File), Channels,
         Input_Id (Texture), Flags);
   begin
      if Raw_Id = 0 then
         raise SOIL_Error with Last_Error;
      end if;
      Texture.Set_Raw_Id (Raw_Id);
   end Load_Files_To_Cubemap;

   procedure Load_File_To_Cubemap
     (File_Name  : String;
      Texture    : in out GL.Objects.Textures.Texture'Class;
      Face_Order : Cubemap_Layout := "EWUDNS";
      Channels   : Image_Format   := Auto;
      Flags      : Texture_Flags  := (others => False)) is

      Raw_Id : constant GL.Types.UInt
        := API.Load_OGL_Single_Cubemap (Interfaces.C.To_C (File_Name),
                                        Face_Order, Channels,
                                        Input_Id (Texture), Flags);
   begin
      if Raw_Id = 0 then
         raise SOIL_Error with Last_Error;
      end if;
      Texture.Set_Raw_Id (Raw_Id);
   end Load_File_To_Cubemap;

   procedure Load_HDR_Texture
     (File_Name      : String;
      Texture        : in out GL.Objects.Textures.Texture'Class;
      Format         : Fake_HDR_Representation;
      Rescale_To_Max : Boolean;
      Flags          : Texture_Flags := (others => False)) is

      Raw_Id : constant GL.Types.UInt := API.Load_OGL_HDR_Texture
        (Interfaces.C.To_C (File_Name), Format, Bool (Rescale_To_Max),
         Input_Id (Texture), Flags);
   begin
      if Raw_Id = 0 then
         raise SOIL_Error with Last_Error;
      end if;
      Texture.Set_Raw_Id (Raw_Id);
   end Load_HDR_Texture;

   procedure Save_Screenshot (File_Name  : String;
                              Image_Type : Image_Save_Type;
                              X, Y, Width, Height : GL.Types.Int) is
      Result : constant Bool := API.Save_Screenshot
        (Interfaces.C.To_C (File_Name), Image_Type, X, Y, Width, Height);
   begin
      if not Result then
         raise SOIL_Error with Last_Error;
      end if;
   end Save_Screenshot;

   function Last_Error return String is
   begin
      return Interfaces.C.Strings.Value (API.Last_Result);
   end Last_Error;
end SOIL;
