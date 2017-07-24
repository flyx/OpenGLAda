
with GL.Objects.Textures;
with GL.Types;

with FTGL.Fonts;

package FTGL_Interface is

   type Font_Kind is (Bitmap, Pixmap, Polygon, Outlne, Extrude, Texture, Buffer);
   type Glyph_Data (Kind : Font_Kind) is private;

   procedure Setup_Font (theFont               : in out FTGL.Fonts.Bitmap_Font;
                         Data                  : out Glyph_Data;
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
   function Glyph_Texture (Data : Glyph_Data) return GL.Objects.Textures.Texture;
   function Glyph_Valid (Data : Glyph_Data) return Boolean;
   function Glyph_Width (Data : Glyph_Data) return GL.Types.Single;

private

   type Glyph_Data (Kind : Font_Kind) is record
      Valid             : Boolean := False;
      Texture           : GL.Objects.Textures.Texture;
      Width             : GL.Types.Single;
      Height            : GL.Types.Single;
      Advance           : GL.Types.Single;
      case Kind is
         when Bitmap =>
            Ascend            : GL.Types.Single;
            Descend           : GL.Types.Single;
            Bearing_X         : GL.Types.Single;
            Bearing_Y         : GL.Types.Single;
         when Buffer =>
            Depth : GL.Types.Single := 0.05;
         when others =>
            null;
      end case;
   end record;

end FTGL_Interface;
