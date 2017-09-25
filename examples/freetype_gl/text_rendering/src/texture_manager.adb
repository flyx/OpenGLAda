
with System;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use  Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Types.Colors;

with FT;
with FT.API;
with FT.Glyphs;
with FT.Image;
with FT.Utilities;

with Utilities;

package body Texture_Manager is
   theLibrary     : FT.API.Library_Ptr;
   Face_Ptr       : FT.API.Face_Ptr;

   --  ------------------------------------------------------------------------

   procedure Setup_Font is
      use GL.Types;
      --        Font_File       : String := "/Library/Fonts/Arial.ttf";
      Font_File       : String := "/System/Library/Fonts/Helvetica.dfont";
   begin
      if FT.Interfac.New_Face (theLibrary, Font_File, 0, Face_Ptr) /= 0 then
         Put_Line ("A face failed to load.");
         raise FT.FT_Exception;
      end if;
      --  Set pixel size to 48 x 48
      if FT.Interfac.Set_Pixel_Sizes (Face_Ptr, 0, 48) /= 0 then
         Put_Line ("Unable to set pixel sizes.");
         raise FT.FT_Exception;
      end if;

      GL.Pixels.Set_Unpack_Alignment (GL.Pixels.Bytes);  --  Disable byte-alignment restriction
   exception
      when others =>
         Put_Line ("An exception occurred in Setup_Font.");
         raise;
   end Setup_Font;

   --  ------------------------------------------------------------------------

   procedure Setup_Graphic (Vertex_Buffer : in out V_Buffer;
                            Character_Data : in out FT.Interfac.Character_Data_Vector;
                            X, Y: GL.Types.Single; Scale : GL.Types.Single := 1.0) is
      use GL.Types;
      aTexture      : GL.Objects.Textures.Texture;
   begin
      if FT.Interfac.Init_FreeType (theLibrary) /= 0 then
         Put_Line ("The Freetype Library failed to load.");
         raise FT.FT_Exception;
      end if;

      Setup_Font;
      FT.Utilities.Setup_Character_Textures (Face_Ptr, Character_Data);

      FT.Interfac.Done_Face (Face_Ptr);
      FT.Interfac.Done_Library (theLibrary);
   end Setup_Graphic;

   --  ------------------------------------------------------------------------

end Texture_Manager;
