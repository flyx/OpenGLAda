--------------------------------------------------------------------------------

with Interfaces.C;

with GL.Attributes;
with GL.Blending;
with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Toggles;

with FT;
with FT.Faces;
with FT.Glyphs;

package body Texture_Management is
   use Interfaces.C;

   type Character_Record is record
      Texture   : GL.Objects.Textures.Texture;
      Width     : GL.Types.Int := 0;
      Rows      : GL.Types.Int := 0;
      Left      : GL.Types.Int := 0;
      Top       : GL.Types.Int := 0;
      Advance_X : GL.Types.Int := 0;
   end record;

   OGL_Exception : Exception;

    procedure Load_Vertex_Buffer is new
     GL.Objects.Buffers.Load_To_Buffer (GL.Types.Singles.Vector4_Pointers);

   type Character_Data_Vector is array (Natural range <>) of Character_Record;

   Vertex_Array         : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer        : GL.Objects.Buffers.Buffer;
   Extended_Ascii_Data  : Character_Data_Vector (0 .. 255);

   procedure Setup_Character_Textures (Face_Ptr : FT.Faces.Face_Reference);
   procedure Setup_Font (theLibrary : FT.Library_Reference;
                         Face_Ptr   : out FT.Faces.Face_Reference;
                         Font_File  : String);

   --  ------------------------------------------------------------------------

   function Advance_X (Data : Character_Record) return GL.Types.Int is
   begin
      return Data.Advance_X;
   end Advance_X;

   --  ------------------------------------------------------------------------

   procedure Initialize_Font_Data (Font_File : String) is
      use GL.Types;
      theLibrary : FT.Library_Reference;
      Face_Ptr   : FT.Faces.Face_Reference;
   begin
      theLibrary.Init;
      Setup_Font (theLibrary, Face_Ptr, Font_File);
      Setup_Character_Textures (Face_Ptr);
   end Initialize_Font_Data;

   --  ------------------------------------------------------------------------

   procedure Load_Texture (Face_Ptr  : FT.Faces.Face_Reference;
                           Char_Data : in out Character_Record;
                           Width, Height : GL.Types.Size;
                           X_Offset, Y_Offset : GL.Types.Int) is
      use GL.Objects.Textures.Targets;
      use GL.Pixels;
      use GL.Types;
      aTexture          : GL.Objects.Textures.Texture;
      Bitmap_Image_Ptr  : GL.Objects.Textures.Image_Source;
      Bitmap            : constant FT.Bitmap_Record :=
                                   FT.Glyphs.Bitmap (Face_Ptr.Glyph_Slot);
      Num_Levels        : constant GL.Types.Size := 1;
      Mip_Level_0       : constant GL.Objects.Textures.Mipmap_Level := 0;
   begin
      aTexture.Initialize_Id;
      Texture_2D.Bind (aTexture);
      Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_S
      Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_T

      if Width > 0 and then Height > 0 then
         Texture_2D.Storage (Num_Levels, RGBA8, Width, Height);
      else
         Texture_2D.Storage (Num_Levels, RGBA8, 1, 1);
      end if;

      Bitmap_Image_Ptr := GL.Objects.Textures.Image_Source (Bitmap.Buffer);
      if Width > 0 and Height > 0 then
         Texture_2D.Load_Sub_Image_From_Data
           (Mip_Level_0, X_Offset, Y_Offset, Width, Height, Red, Unsigned_Byte,
            Bitmap_Image_Ptr);
      end if;
      Char_Data.Texture := aTexture;
   end Load_Texture;

   -- --------------------------------------------------------------------------

   procedure Render_Text (Render_Program : GL.Objects.Programs.Program;
                          Text   : String; X, Y, Scale : GL.Types.Single;
                          Colour : GL.Types.Colors.Basic_Color;
                          Texture_ID, Projection_Matrix_ID, Colour_ID : GL.Uniforms.Uniform;
                          Projection_Matrix : GL.Types.Singles.Matrix4) is
      use GL.Objects.Buffers;
      use GL.Objects.Textures.Targets;
      use GL.Types.Colors;
      use GL.Types;
      use FT.Faces;

      Num_Triangles  : constant GL.Types.Int := 2;
      Num_Vertices   : constant GL.Types.Int := Num_Triangles * 3; -- Two triangles
      Num_Components : constant GL.Types.Int := 4;                 -- Coords vector size;
      Stride         : constant GL.Types.Int := 0;
      Blend_State    : constant GL.Toggles.Toggle_State :=
        GL.Toggles.State (GL.Toggles.Blend);
      Src_Alpha_Blend : constant  GL.Blending.Blend_Factor :=
                                  GL.Blending.Blend_Func_Src_Alpha;
      One_Minus_Src_Alpha_Blend : constant  GL.Blending.Blend_Factor :=
                                  GL.Blending.One_Minus_Src_Alpha;
      Char           : Character;
      Char_Data      : Character_Record;
      Char_Texture   : GL.Objects.Textures.Texture;
      X_Orig         : Single := X;
      Y_Orig         : constant Single := Y;
      X_Pos          : Single;
      Y_Pos          : Single;
      Char_Width     : Single;
      Height         : Single;
      --  2D quad as two triangles requires 2 * 3 vertices of 4 floats
      Vertex_Data    : Singles.Vector4_Array (1 .. Num_Vertices);
   begin
      --  Blending allows a fragment colour's alpha value to control the resulting
      --  colour which will be transparent for all the glyph's background colours and
      --  non-transparent for the actual character pixels.
      GL.Toggles.Enable (GL.Toggles.Blend);
      GL.Blending.Set_Blend_Func (GL.Blending.Src_Alpha,
                                  GL.Blending.One_Minus_Src_Alpha);
      GL.Objects.Programs.Use_Program (Render_Program);

      for index in Text'Range loop
         Char := Text (index);
         Char_Data := Extended_Ascii_Data (Character'Pos (Char));
         X_Pos := X_Orig + Single (Char_Data.Left) * Scale;
         Y_Pos := Y_Orig - Single (Char_Data.Rows - Char_Data.Top) * Scale;
         Char_Width := Single (Char_Data.Width) * Scale;
         Height := Single (Char_Data.Rows) * Scale;

         Vertex_Data := ((X_Pos, Y_Pos + Height,             0.0, 0.0),
                         (X_Pos, Y_Pos,                      0.0, 1.0),
                         (X_Pos + Char_Width, Y_Pos,         1.0, 1.0),

                         (X_Pos, Y_Pos + Height,              0.0, 0.0),
                         (X_Pos + Char_Width, Y_Pos,          1.0, 1.0),
                         (X_Pos + Char_Width, Y_Pos + Height, 1.0, 0.0));

         Vertex_Array.Bind;
         Array_Buffer.Bind (Vertex_Buffer);
         Load_Vertex_Buffer (Array_Buffer, Vertex_Data, Dynamic_Draw);

         Char_Texture :=  Char_Data.Texture;
         if not GL.Objects.Textures.Is_Texture  (Char_Texture.Raw_Id) then
            raise OGL_Exception with "FT.OGL.Render_Text, aTexture is invalid for character " & Char'Img & ".";
         end if;

         GL.Objects.Textures.Set_Active_Unit (0);
         Texture_2D.Bind (Char_Texture);
         GL.Uniforms.Set_Int (Texture_ID, 0);
         GL.Uniforms.Set_Single (Colour_ID, Colour (R), Colour (G), Colour (B));
         GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

         GL.Attributes.Enable_Vertex_Attrib_Array (0);
         Array_Buffer.Bind (Vertex_Buffer);
         GL.Attributes.Set_Vertex_Attrib_Pointer (Index  => 0, Count  => Num_Components,
                                                  Kind   => GL.Types.Single_Type,
                                                  Stride => Stride, Offset => 0);

         GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, Num_Vertices);
         GL.Attributes.Disable_Vertex_Attrib_Array (0);
         --  Bitshift by 6 to get value in pixels (2^6 = 64
         --  (divide amount of 1/64th pixels by 64 to get amount of pixels))
         X_Orig := X_Orig + Single (Advance_X (Char_Data)) / 64.0 * Scale;
      end loop;
      GL.Toggles.Set (GL.Toggles.Blend, Blend_State);
      GL.Blending.Set_Blend_Func (Src_Alpha_Blend, One_Minus_Src_Alpha_Blend);
   end Render_Text;

   --  ------------------------------------------------------------------------

   procedure Set_Char_Data (Char_Data : in out Character_Record;
                            Width     : GL.Types.Int; Height : GL.Types.Int;
                            Left      : GL.Types.Int; Top    : GL.Types.Int;
                            Advance_X : GL.Types.Int) is
   begin
      Char_Data.Width := Width;
      Char_Data.Rows := Height;
      Char_Data.Left := Left;
      Char_Data.Top := Top;
      Char_Data.Advance_X := Advance_X;
   end Set_Char_Data;

   --  -------------------------------------------------------------------------

   procedure Setup_Character_Textures (Face_Ptr : FT.Faces.Face_Reference) is
      use GL.Objects.Buffers;
      use GL.Types;
      Glyph_Slot     : constant FT.Glyph_Slot_Reference := Face_Ptr.Glyph_Slot;
      Width          : GL.Types.Size;
      Height         : GL.Types.Size;
      X_Offset       : constant GL.Types.Int := 0;
      Y_Offset       : constant GL.Types.Int := 0;
      Char_Data      : Character_Record;
   begin
      --  Blending allows a fragment colour's alpha value to control the resulting
      --  colour which will be transparent for all the glyph's background colours and
      --  non-transparent for the actual character pixels.
      GL.Toggles.Enable (GL.Toggles.Blend);
      GL.Blending.Set_Blend_Func (GL.Blending.Src_Alpha,
                                  GL.Blending.One_Minus_Src_Alpha);

      Vertex_Array.Initialize_Id;
      Vertex_Array.Bind;

      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (Vertex_Buffer);
      for index in Extended_Ascii_Data'Range loop
         --  Load_Render asks FreeType to create an 8-bit grayscale bitmap image
         --  that can be accessed via face->glyph->bitmap.
         FT.Faces.Load_Character (Face_Ptr, unsigned_long (index),
                                  FT.Faces.Load_Render);
         --  Ensure that the glyph image is an anti-aliased bitmap
         FT.Glyphs.Render_Glyph (Glyph_Slot, FT.Faces.Render_Mode_Mono);

         Width := GL.Types.Int (FT.Glyphs.Bitmap (Glyph_Slot).Width);
         Height := GL.Types.Int (FT.Glyphs.Bitmap (Glyph_Slot).Rows);

         Set_Char_Data (Char_Data, Width, Height,
                        GL.Types.Int (FT.Glyphs.Bitmap_Left (Glyph_Slot)),
                        GL.Types.Int (FT.Glyphs.Bitmap_Top (Glyph_Slot)),
                        GL.Types.Int (FT.Glyphs.Advance (Glyph_Slot).X));

         Load_Texture (Face_Ptr, Char_Data, Width, Height, X_Offset, Y_Offset);
         Extended_Ascii_Data (index) := Char_Data;
      end loop;
   end Setup_Character_Textures;

      --  ------------------------------------------------------------------------

   procedure Setup_Font (theLibrary : FT.Library_Reference;
                         Face_Ptr   : out FT.Faces.Face_Reference; Font_File : String) is
      use GL.Types;
   begin
      FT.Faces.New_Face (theLibrary, Font_File, 0, Face_Ptr);
      --  Set pixel size to 48 x 48
      FT.Faces.Set_Pixel_Sizes (Face_Ptr, 0, 48);
      --  Disable byte-alignment restriction
      GL.Pixels.Set_Unpack_Alignment (GL.Pixels.Bytes);
   end Setup_Font;

   --  ------------------------------------------------------------------------

end Texture_Management;
