
with Ada.Containers.Ordered_Maps;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Pixels;
with GL.Objects.Textures.Targets;

with FTGL;

package body FTGL_Interface is

   package Characters_Package is new Ada.Containers.Ordered_Maps (Character, Character_Data);
   type Chars_Map_Type is new Characters_Package.Map with null record;
   type Chars_Cursor is new Characters_Package.Cursor;

   --  ------------------------------------------------------------------------

   function Char_Advance (Data : Character_Data) return GL.Types.Single is
   begin
         return Data.Advance;
   end Char_Advance;

   --  ------------------------------------------------------------------------

   function Char_Ascend (Data : Character_Data)  return GL.Types.Single is
   begin
         return Data.Ascend;
   end Char_Ascend;

   --  ------------------------------------------------------------------------

   function Char_Bearing_X (Data : Character_Data) return GL.Types.Single is
   begin
         return Data.Bearing_X;
   end Char_Bearing_X;

   --  ------------------------------------------------------------------------

   function Char_Bearing_Y (Data : Character_Data) return GL.Types.Single is
   begin
         return Data.Bearing_Y;
   end Char_Bearing_Y;

   --  ------------------------------------------------------------------------

   function Char_Descend (Data : Character_Data) return  GL.Types.Single is
   begin
         return Data.Descend;
   end Char_Descend;

   --  ------------------------------------------------------------------------

   function Char_Height (Data : Character_Data) return GL.Types.Single is
   begin
         return Data.Height;
   end Char_Height;

   --  ------------------------------------------------------------------------

   function Char_Texture (Data : Character_Data) return GL.Objects.Textures.Texture is
   begin
         return Data.Texture;
   end Char_Texture;

   --  ------------------------------------------------------------------------

   function Char_Valid (Data : Character_Data) return Boolean is
   begin
         return Data.Valid;
   end Char_Valid;

   --  ------------------------------------------------------------------------

   function Char_Width (Data : Character_Data) return GL.Types.Single is
   begin
         return Data.Width;
   end Char_Width;

   --  ------------------------------------------------------------------------

   package body Setup is

      procedure Generate_Texture (Font_Texture  : in out GL.Objects.Textures.Texture;
                                  Image_Address : GL.Objects.Textures.Image_Source;
                                  Data          : Character_Data) is
         use GL.Objects.Textures;
         use GL.Objects.Textures.Targets;
         use GL.Pixels;

      begin
         Font_Texture.Initialize;
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

      procedure Load_Characters (theFont   : in out Font_Type;
                                 Data      : out Character_Data;
                                 Face_Size : GL.Types.Int) is
         use FTGL.Fonts;
         use GL.Objects.Textures;
         use GL.Types;
         Char_Map_List   : FTGL.Fonts.Charset_List := theFont.Get_Char_Map_List;
         Font_Char_Map   : FTGL.Charset := Char_Map_List (5);
         aChar           : Character;
         Char_S          : String := " ";
         Char_Image_Map  : Chars_Map_Type;
         Font_Texture    : GL.Objects.Textures.Texture;
         Image_Address   : GL.Objects.Textures.Image_Source;

         --  BBox array elements:
         --  1 lower  2 left  3 near
         --  4 upper  5 right 6 far
         BBox            : FTGL.Bounding_Box;
      begin
         for Index in 0 .. 128 loop
            aChar := Character'Val (Index);
            Char_S (1) := aChar;
            BBox := theFont.Bounds (Char_S);
            Data.Ascend := theFont.Ascender;
            Data.Descend := theFont.Descender;
            Data.Advance := theFont.Advance_Width (Char_S);
            Data.Height := (BBox (4) - BBox (1));
            Data.Width  := (BBox (5) - BBox (2));
            Data.Bearing_X := 0.5 * Data.Width;
            Data.Bearing_Y := Data.Height - Data.Descend;
            Image_Address := Image_Source (Font_Char_Map'Address);
            Generate_Texture (Font_Texture, Image_Address, Data);
            Char_Image_Map.Insert (aChar, Data);
            Data.Texture := Font_Texture;
            Data.Valid := True;
         end loop;

      exception
         when anError : FTGL.FTGL_Error =>
            Put_Line ("Generate_Texture returned an FTGL error: ");
            raise;
         when  others =>
            Put_Line ("An exception occurred in Load_Char_Vector.");
            raise;
      end Load_Characters;

      --  ------------------------------------------------------------------------

      procedure Setup_Font (theFont    : in out Font_Type;
                            Data       : out Character_Data;
                            Font_File  : String;
                            Face_Size, Resolution : GL.Types.UInt := 72) is
      begin
         theFont.Initialize;
         theFont.Load (Font_File);
         if not theFont.Loaded then
            Put_Line (Font_File & " failed to load.");
            raise FTGL.FTGL_Error;
         end if;
         theFont.Set_Font_Face_Size (Face_Size, Resolution);
         Load_Characters (theFont, Data, GL.Types.Int (Face_Size));
      exception
         when anError : FTGL.FTGL_Error =>
            Put_Line ("Setup_Font returned an FTGL error: ");
            raise;
         when  others =>
            Put_Line ("An exception occurred in Setup_Font.");
            raise;
      end Setup_Font;

      --  ------------------------------------------------------------------------

   end Setup;

end FTGL_Interface;
