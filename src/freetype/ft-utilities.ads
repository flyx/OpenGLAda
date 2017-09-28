
with GL.Objects.Programs;
with GL.Types;
with GL.Types.Colors;
with GL.Uniforms;

with FT.API;
with FT.Image;
with FT.FreeType;

package FT.Utilities is
   procedure Initialize_Font_Data (Font_File : String);

   procedure Print_Bitmap_Metadata (Bitmap : FT.Image.Bitmap_Record);
   procedure Print_Character_Metadata (aFace : FT.API.Face_Ptr;
                                       aChar : Character);
   procedure Print_Character_Metadata (Data : FT.FreeType.Character_Record);
   procedure Render_Text (Render_Program : GL.Objects.Programs.Program;
                          Text   : String; X, Y, Scale : GL.Types.Single;
                          Colour : GL.Types.Colors.Basic_Color;
                          Texture_ID, Projection_Matrix_ID, Colour_ID : GL.Uniforms.Uniform;
                          Projection_Matrix : GL.Types.Singles.Matrix4);
end FT.Utilities;
