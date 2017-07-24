
with Ada.Containers.Ordered_Maps;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Pixels;
with GL.Objects.Textures.Targets;

with FTGL;

package body FTGL_Interface is

   subtype Bitmap_Glyph_Data is Glyph_Data (Bitmap);
   package Characters_Package is new Ada.Containers.Ordered_Maps (Character, Bitmap_Glyph_Data);
   type Chars_Map_Type is new Characters_Package.Map with null record;
   type Chars_Cursor is new Characters_Package.Cursor;

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
   function Glyph_Texture (Data : Glyph_Data) return GL.Objects.Textures.Texture is
   begin
      return Data.Texture;
   end Glyph_Texture;

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
                               Image_Address : in out GL.Objects.Textures.Image_Source;
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
      Texture_2D.Bind (Font_Texture);

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

   procedure Load_Characters (theFont   : in out FTGL.Fonts.Bitmap_Font;
                              Kind      : Font_Kind;
                              Data      : out Glyph_Data;
                              Face_Size : GL.Types.Int) is
      use FTGL.Fonts;
      use GL.Objects.Textures;
      use GL.Types;
      Font_Texture     : GL.Objects.Textures.Texture;
      Glyph_Map_List   : FTGL.Fonts.Charset_List := theFont.Get_Char_Map_List;
      Font_Glyph_Map   : FTGL.Charset := Glyph_Map_List (5);
      aChar            : Character;
      Glyph_S          : String := " ";
      Glyph_Image_Map  : Chars_Map_Type;
      Image_Address    : GL.Objects.Textures.Image_Source;

      --  BBox array elements:
      --  1 lower  2 left  3 near
      --  4 upper  5 right 6 far
      BBox            : FTGL.Bounding_Box;
   begin
      for Index in 0 .. 128 loop
         aChar := Character'Val (Index);
         Glyph_S (1) := aChar;
         BBox := theFont.Bounds (Glyph_S);
         Data.Ascend := theFont.Ascender;
         Data.Descend := theFont.Descender;
         Data.Advance := theFont.Advance_Width (Glyph_S);
         Data.Height := (BBox (4) - BBox (1));
         Data.Width  := (BBox (5) - BBox (2));
         Data.Bearing_X := 0.5 * Data.Width;
         Data.Bearing_Y := Data.Height - Data.Descend;
--           if Kind = Buffer then
--              Data.Depth := Depth;
--           end if;
         Image_Address := Image_Source (Font_Glyph_Map'Address);
--           Put_Line ("Load_Characters Data.Bearing_Y " & Single'Image (Data.Bearing_Y));
--           Put_Line ("Load_Characters Index " & Integer'Image (Index));
         Generate_Texture (Font_Texture, Image_Address, Data);
         Glyph_Image_Map.Insert (aChar, Data);
         Data.Texture := Font_Texture;
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
                         Data                  : out Glyph_Data;
                         Font_File             : String;
                         Face_Size, Resolution : GL.Types.UInt := 72) is
   begin
      theFont.Initialize;
      theFont.Load (Font_File);
      if not theFont.Loaded then
         Put_Line (Font_File & " failed to load.");
         raise FTGL.FTGL_Error;
      end if;
      theFont.Set_Font_Face_Size (Face_Size, Resolution);
      Load_Characters (theFont, Bitmap, Data, GL.Types.Int (Face_Size));
   exception
      when anError : FTGL.FTGL_Error =>
         Put_Line ("Setup_Bitmap_Font_Font returned an FTGL error: ");
         raise;
      when  others =>
         Put_Line ("An exception occurred in Setup_Bitmap_Font_Font.");
         raise;
   end Setup_Font;

   --  ------------------------------------------------------------------------

--     procedure Setup_Font (theFont               : in out FTGL.Fonts.Buffer_Font;
--                           Data                  : out Glyph_Data;
--                           Font_File             : String;
--                           Face_Size, Resolution : GL.Types.UInt := 72;
--                           Depth                 : GL.Types.Single := 0.05) is
--     begin
--        theFont.Initialize;
--        theFont.Load (Font_File);
--        if not theFont.Loaded then
--           Put_Line (Font_File & " failed to load.");
--           raise FTGL.FTGL_Error;
--        end if;
--        theFont.Set_Font_Face_Size (Face_Size, Resolution);
--        theFont.Set_Font_Depth (Depth);
--        Load_Characters (theFont, Buffer, Data, GL.Types.Int (Face_Size));
--     exception
--        when anError : FTGL.FTGL_Error =>
--           Put_Line ("Setup_Buffer_Font returned an FTGL error: ");
--           raise;
--        when  others =>
--           Put_Line ("An exception occurred in Setup_Buffer_Font.");
--           raise;
--     end Setup_Font;

   --  ------------------------------------------------------------------------
end FTGL_Interface;
