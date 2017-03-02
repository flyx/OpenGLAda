
with GL.Objects.Programs;
with GL.Types.Colors;

package Utilities is

    procedure Clear_Background_Colour (Colour : GL.Types.Colors.Color);
    procedure Clear_Background_Colour_And_Depth (Colour : GL.Types.Colors.Color);
    procedure Show_Shader_Program_Data (aProgram : gl.Objects.Programs.Program);
    procedure Show_GL_Data;

end Utilities;
