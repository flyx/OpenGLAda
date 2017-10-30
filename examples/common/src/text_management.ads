
with Ada.Strings.Unbounded;

with GL.Objects.Programs;
with GL.Types;
with GL.Types.Colors;
with GL.Uniforms;

package Text_Management is

   type Text_Record is record
      Text    : Ada.Strings.Unbounded.Unbounded_String;
      Pos_X   : GL.Types.Single;
      Pos_Y   : GL.Types.Single;
      Scale   : GL.Types.Single;
      Colour : GL.Types.Colors.Color;
   end record;
   type Text_Array is array (Positive range <>) of Text_Record;

   procedure Setup (Font_File : String);
   procedure Render_Text (Render_Program : GL.Objects.Programs.Program;
                          Text   : String; X, Y, Scale : GL.Types.Single;
                          Colour : GL.Types.Colors.Color;
                          Texture_ID, MVP_Matrix_ID, Dimensions_ID,
                          Colour_ID : GL.Uniforms.Uniform;
                          MVP_Matrix : GL.Types.Singles.Matrix4);
   procedure Render_Text (Render_Program : GL.Objects.Programs.Program;
                          Text_Data   : Text_Array;
                          Texture_ID, MVP_Matrix_ID, Dimensions_ID,
                          Colour_ID : GL.Uniforms.Uniform;
                          MVP_Matrix : GL.Types.Singles.Matrix4);
end Text_Management;
