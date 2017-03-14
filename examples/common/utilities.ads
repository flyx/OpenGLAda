
with GL.Objects.Programs;
with GL.Types;
with GL.Types.Colors;

package Utilities is

    procedure Clear_Background_Colour (Colour : GL.Types.Colors.Color);
    procedure Clear_Background_Colour_And_Depth (Colour : GL.Types.Colors.Color);
    procedure Print_Vector (Name : String; aVector : GL.Types.Singles.Vector3);
    procedure Show_Shader_Program_Data (aProgram : gl.Objects.Programs.Program);
    procedure Show_GL_Data;

end Utilities;
