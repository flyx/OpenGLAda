
with GL.Objects.Programs;
with GL.Types;
with GL.Types.Colors;

package Utilities is

    procedure Clear_All (Colour : GL.Types.Colors.Color);
    procedure Clear_Background_Colour (Colour : GL.Types.Colors.Color);
    procedure Clear_Background_Colour_And_Depth (Colour : GL.Types.Colors.Color);
    procedure Print_GL_Array (Name : String; anArray : GL.Types.Single_Array);
    procedure Print_Matrix (Name : String; aMatrix : GL.Types.Singles.Matrix3);
    procedure Print_Matrix (Name : String; aMatrix : GL.Types.Singles.Matrix4);
    procedure Print_Vector (Name : String; aVector : GL.Types.Singles.Vector3);
    procedure Print_Vector (Name : String; aVector : GL.Types.Singles.Vector4);
    procedure Show_Shader_Program_Data (aProgram : gl.Objects.Programs.Program);
    procedure Show_GL_Data;

end Utilities;
