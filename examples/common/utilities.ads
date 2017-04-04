
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Types;
with GL.Types.Colors;

package Utilities is

    procedure Clear_All (Colour : GL.Types.Colors.Color);
    procedure Clear_Background_Colour (Colour : GL.Types.Colors.Color);
    procedure Clear_Background_Colour_And_Depth (Colour : GL.Types.Colors.Color);
    procedure Load_Vertex_Buffer is new
      GL.Objects.Buffers.Load_To_Buffer (GL.Types.Singles.Vector3_Pointers);
    procedure Print_GL_Array3 (Name : String; anArray : GL.Types.Singles.Vector3_Array);
    procedure Print_GL_Array4 (Name : String; anArray : GL.Types.Singles.Vector4_Array);
    procedure Print_Matrix (Name : String; aMatrix : GL.Types.Singles.Matrix3);
    procedure Print_Matrix (Name : String; aMatrix : GL.Types.Singles.Matrix4);
    procedure Print_Vector (Name : String; aVector : GL.Types.Singles.Vector3);
    procedure Print_Vector (Name : String; aVector : GL.Types.Singles.Vector4);
    procedure Show_Shader_Program_Data (aProgram : gl.Objects.Programs.Program);
    procedure Show_GL_Data;

end Utilities;
