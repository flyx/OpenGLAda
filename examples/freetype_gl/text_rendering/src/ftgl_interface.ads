
with Ada.Containers.Ordered_Maps;
with GL.Objects.Textures;
with GL.Types;

with FTGL.Fonts;

package FTGL_Interface is

   type Font_Kind is (Bitmap, Pixmap, Polygon, Outlne, Extrude, Texture, Buffer);

   type FT_Point is record
      X  : GL.Types.Double;
      Y  : GL.Types.Double;
      Z  : GL.Types.Double := 0.0;
   end record;

   type Glyph_Data (Kind : Font_Kind) is record
      Valid       : Boolean := False;
      Texture     : GL.Objects.Textures.Texture;
      Width       : GL.Types.Single;
      Height      : GL.Types.Single;
      case Kind is
         when Bitmap =>
            Pitch    : GL.Types.UInt;
            Top_Left : FT_Point;
         when Buffer | Extrude =>
            Depth : GL.Types.Single := 0.05;
         when others =>
            Advance     : GL.Types.Single;
            Bearing_X   : GL.Types.Single;
            Bearing_Y   : GL.Types.Single;
            Ascend      : GL.Types.Single;
            Descend     : GL.Types.Single;
      end case;
   end record;

   subtype Bitmap_Glyph_Data is Glyph_Data (Bitmap);

   package Characters_Package is new Ada.Containers.Ordered_Maps (Character, Bitmap_Glyph_Data);
   type Chars_Map_Type is new Characters_Package.Map with null record;
   type Chars_Cursor is new Characters_Package.Cursor;

   procedure Setup_Font (theFont               : in out FTGL.Fonts.Bitmap_Font;
                         Texture_Map           : in out Chars_Map_Type;
                         Font_File             : String;
                         Face_Size, Resolution : GL.Types.UInt := 72);
--     procedure Setup_Font (theFont               : in out FTGL.Fonts.Buffer_Font;
--                           Data                  : out Glyph_Data;
--                           Font_File             : String;
--                           Face_Size, Resolution : GL.Types.UInt := 72;
--                           Depth                 : GL.Types.Single := 0.05);

   function Glyph_Advance (Data : Glyph_Data) return GL.Types.Single;
   function Glyph_Ascend (Data : Glyph_Data)  return GL.Types.Single;
   function Glyph_Bearing_X (Data : Glyph_Data) return GL.Types.Single;
   function Glyph_Bearing_Y (Data : Glyph_Data) return GL.Types.Single;
   function Glyph_Descend (Data : Glyph_Data) return  GL.Types.Single;
   function Glyph_Height (Data : Glyph_Data) return GL.Types.Single;
   function Glyph_Kind (Data : Glyph_Data) return Font_Kind;
   function Glyph_Pitch (Data : Glyph_Data) return GL.Types.UInt;
   function Glyph_Texture (Data : Glyph_Data) return GL.Objects.Textures.Texture;
   function Glyph_Top_Left (Data : Glyph_Data) return FT_Point;
   function Glyph_Valid (Data : Glyph_Data) return Boolean;
   function Glyph_Width (Data : Glyph_Data) return GL.Types.Single;

end FTGL_Interface;
