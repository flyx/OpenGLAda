
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Pixels;
with GL.Objects.Textures.Targets;

with FTGL;

package body FTGL_Interface is

   --  ------------------------------------------------------------------------

   function Glyph_Advance (Data : Glyph_Data) return GL.Types.Single is
   begin
      return Data.Advance;
   end Glyph_Advance;

   --  ------------------------------------------------------------------------

   function Glyph_Ascend (Data : Glyph_Data)  return GL.Types.Single is
   begin
      return Data.Ascend;
   end Glyph_Ascend;

   --  ------------------------------------------------------------------------

   function Glyph_Bearing_X (Data : Glyph_Data) return GL.Types.Single is
   begin
      return Data.Bearing_X;
   end Glyph_Bearing_X;

   --  ------------------------------------------------------------------------

   function Glyph_Bearing_Y (Data : Glyph_Data) return GL.Types.Single is
   begin
      return Data.Bearing_Y;
   end Glyph_Bearing_Y;

   --  ------------------------------------------------------------------------

   function Glyph_Descend (Data : Glyph_Data) return  GL.Types.Single is
   begin
      return Data.Descend;
   end Glyph_Descend;

   --  ------------------------------------------------------------------------

   function Glyph_Height (Data : Glyph_Data) return GL.Types.Single is
   begin
      return Data.Height;
   end Glyph_Height;

   --  ------------------------------------------------------------------------

   function Glyph_Kind (Data : Glyph_Data) return Font_Kind is
   begin
      return Data.Kind;
   end Glyph_Kind;

   --  ------------------------------------------------------------------------

   function Glyph_Pitch (Data : Glyph_Data) return GL.Types.UInt is
   begin
      return Data.Pitch;
   end Glyph_Pitch;

   --  ------------------------------------------------------------------------

   function Glyph_Texture (Data : Glyph_Data) return GL.Objects.Textures.Texture is
   begin
      return Data.Texture;
   end Glyph_Texture;

   --  ------------------------------------------------------------------------

   function Glyph_Top_Left (Data : Glyph_Data) return FT_Point is
   begin
      return Data.Top_Left;
   end Glyph_Top_Left;

   --  ------------------------------------------------------------------------

   function Glyph_Valid (Data : Glyph_Data) return Boolean is
   begin
      return Data.Valid;
   end Glyph_Valid;

   --  ------------------------------------------------------------------------

   function Glyph_Width (Data : Glyph_Data) return GL.Types.Single is
   begin
      return Data.Width;
   end Glyph_Width;

   --  ------------------------------------------------------------------------

   procedure Generate_Texture (Font_Texture  : in out GL.Objects.Textures.Texture;
                               Image_Address : GL.Objects.Textures.Image_Source;
                               Data          : Glyph_Data) is
      use GL.Objects.Textures;
      use GL.Objects.Textures.Targets;
      use GL.Pixels;

   begin
      Font_Texture.Initialize_Id;
      if not Font_Texture.Initialized then
         Put_Line ("Generate_Texture, Font_Texture initialization failed.");
         raise FTGL.FTGL_Error;
      end if;
      Texture_2D.Bind (Font_Texture);    -- Complete initialization

      Texture_2D.Load_From_Data  (0, RGB, GL.Types.Int (Data.Width),
                                  GL.Types.Int (Data.Height), Red,
                                  Unsigned_Byte, Image_Address);
      Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge);
      Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge);
      Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);

   exception
      when anError : FTGL.FTGL_Error =>
         Put_Line ("Generate_Texture returned an FTGL error: ");
         raise;
      when  others =>
         Put_Line ("An exception occurred in Generate_Texture.");
         raise;
   end Generate_Texture;

   --  ------------------------------------------------------------------------

   procedure Load_Characters (theFont           : in out FTGL.Fonts.Bitmap_Font;
                              Kind              : Font_Kind;
                              Glyph_Texture_Map : in out Chars_Map_Type;
                              Face_Size         : GL.Types.Int) is
      use FTGL.Fonts;
      use GL.Objects.Textures;
      use GL.Objects.Textures.Targets;
      use GL.Types;

      Font_Texture      : GL.Objects.Textures.Texture;
      Glyph_Map_List    : FTGL.Fonts.Charset_List := theFont.Get_Char_Map_List;
      Font_Glyph_Map    : FTGL.Charset := Glyph_Map_List (5);
      aChar             : Character;
      Glyph_S           : String := " ";
      Data              : Glyph_Data (Bitmap);
      Image_Address     : GL.Objects.Textures.Image_Source;

      --  BBox array elements:
      --  1 lower  2 left  3 near
      --  4 upper  5 right 6 far
      BBox            : FTGL.Bounding_Box;
   begin
      Put_Line ("Load_Characters Font_Glyph_Map: " & FTGL.Charset'Image (Font_Glyph_Map));
      for Index in 0 .. 128 loop
         aChar := Character'Val (Index);
         Glyph_S (1) := aChar;
         BBox := theFont.Bounds (Glyph_S);
--           Data.Ascend := theFont.Ascender;
--           Data.Descend := theFont.Descender;
--           Data.Advance := theFont.Advance_Width (Glyph_S);
         Data.Height := (BBox (4) - BBox (1));
         Data.Width  := (BBox (5) - BBox (2));
--           Data.Bearing_X := 0.5 * Data.Width;
--           Data.Bearing_Y := Data.Height - Data.Descend;
--           if Kind = Buffer or else Kind = Extrude then
--              Data.Depth := theFont.Depth;
--           end if;
         Image_Address := Image_Source (Get_Glyph (aChar));
--           Put_Line ("Load_Characters Data.Bearing_Y " & Single'Image (Data.Bearing_Y));
--           Put_Line ("Load_Characters Index " & Integer'Image (Index));
         Generate_Texture (Font_Texture, Image_Address, Data);
         Data.Texture := Font_Texture;
         Glyph_Texture_Map.Insert (aChar, Data);
         Data.Valid := True;
      end loop;
   exception
      when anError : FTGL.FTGL_Error =>
         Put_Line ("Load_Characters returned an FTGL error: ");
         raise;
      when  others =>
         Put_Line ("An exception occurred in Load_Characters.");
         raise;
   end Load_Characters;

   --  ------------------------------------------------------------------------

   procedure Setup_Font (theFont               : in out FTGL.Fonts.Bitmap_Font;
                         Texture_Map           : in out Chars_Map_Type;
                         Font_File             : String;
                         Face_Size, Resolution : GL.Types.UInt := 72) is
   begin
--        theFont.Initialize;
--        theFont.Load (Font_File);
      theFont := new FTBitmapFont (Font_File);
      if not theFont.Loaded then
         Put_Line (Font_File & " failed to load.");
         raise FTGL.FTGL_Error;
      end if;
      theFont.Set_Font_Face_Size (Face_Size, Resolution);
      Load_Characters (theFont, Bitmap, Texture_Map, GL.Types.Int (Face_Size));
   exception
      when anError : FTGL.FTGL_Error =>
         Put_Line ("Setup_Bitmap_Font_Font returned an FTGL error: ");
         raise;
      when  others =>
         Put_Line ("An exception occurred in Setup_Bitmap_Font_Font.");
         raise;
   end Setup_Font;

   --  ------------------------------------------------------------------------

end FTGL_Interface;
