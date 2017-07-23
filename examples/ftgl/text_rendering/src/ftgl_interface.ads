
with GL.Objects.Textures;
with GL.Types;

with FTGL.Fonts;

package FTGL_Interface is

   type Character_Data is private;

   generic
      type Font_Type is new FTGL.Fonts.Font with private;
   package Setup is
      procedure Setup_Font (theFont    : in out Font_Type;
                            Data       : out Character_Data;
                            Font_File  : String;
                            Face_Size, Resolution : GL.Types.UInt := 72;
                            Depth      : GL.Types.Single := 0.05);
   end Setup;

   function Char_Advance (Data : Character_Data) return GL.Types.Single;
   function Char_Ascend (Data : Character_Data)  return GL.Types.Single;
   function Char_Bearing_X (Data : Character_Data) return GL.Types.Single;
   function Char_Bearing_Y (Data : Character_Data) return GL.Types.Single;
   function Char_Descend (Data : Character_Data) return  GL.Types.Single;
   function Char_Height (Data : Character_Data) return GL.Types.Single;
   function Char_Texture (Data : Character_Data) return GL.Objects.Textures.Texture;
   function Char_Valid (Data : Character_Data) return Boolean;
   function Char_Width (Data : Character_Data) return GL.Types.Single;

private
   type Character_Data is record
      Texture           : GL.Objects.Textures.Texture;
      Width             : GL.Types.Single;
      Height            : GL.Types.Single;
      Advance           : GL.Types.Single;
      Ascend            : GL.Types.Single;
      Descend           : GL.Types.Single;
      Bearing_X         : GL.Types.Single;
      Bearing_Y         : GL.Types.Single;
      Depth             : GL.Types.Single := 0.05;
      Valid             : Boolean := False;
   end record;

end FTGL_Interface;
