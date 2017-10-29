
with GL.Objects.Programs;
with GL.Types;
with GL.Types.Colors;
with GL.Uniforms;

package Text_Management is
   procedure Setup (Font_File : String);
   procedure Render_Text (Render_Program : GL.Objects.Programs.Program;
                          Text   : String; X, Y, Scale : GL.Types.Single;
                          Colour : GL.Types.Colors.Color;
                          Texture_ID, Projection_Matrix_ID, Dimensions_ID,
                          Colour_ID : GL.Uniforms.Uniform;
                          Projection_Matrix : GL.Types.Singles.Matrix4);
end Text_Management;
