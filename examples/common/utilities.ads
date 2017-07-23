
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Types;
with GL.Types.Colors;

with Glfw.Windows;

with Maths;

package Utilities is

    procedure Clear_All (Colour : GL.Types.Colors.Color);
    procedure Clear_Background_Colour (Colour : GL.Types.Colors.Color);
    procedure Clear_Background_Colour_And_Depth (Colour : GL.Types.Colors.Color);
    procedure Enable_Mouse_Callbacks (Window : in out Glfw.Windows.Window; Enable : Boolean);
    procedure Load_Element_Buffer is new
      GL.Objects.Buffers.Load_To_Buffer (GL.Types.Int_Pointers);
    procedure Load_Vertex_Buffer is new
      GL.Objects.Buffers.Load_To_Buffer (GL.Types.Singles.Vector2_Pointers);
    procedure Load_Vertex_Buffer is new
      GL.Objects.Buffers.Load_To_Buffer (GL.Types.Singles.Vector3_Pointers);
    procedure Load_Vertex_Buffer is new
      GL.Objects.Buffers.Load_To_Buffer (GL.Types.Singles.Vector4_Pointers);
    procedure Load_Vector5_Buffer is new GL.Objects.Buffers.Load_To_Buffer
      (Maths.Vector5_Pointers);
    procedure Load_Vector6_Buffer is new GL.Objects.Buffers.Load_To_Buffer
      (Maths.Vector6_Pointers);
    procedure Print_GL_Int_Array (Name : String; anArray : GL.Types.Int_Array);
    procedure Print_GL_Array3 (Name : String; anArray : GL.Types.Singles.Vector3_Array);
    procedure Print_GL_Array4 (Name : String; anArray : GL.Types.Singles.Vector4_Array);
    procedure Print_Array6 (Name : String; anArray : Maths.Vector6_Array);
    procedure Print_Matrix (Name : String; aMatrix : GL.Types.Singles.Matrix3);
    procedure Print_Matrix (Name : String; aMatrix : GL.Types.Singles.Matrix4);
    procedure Print_Vector (Name : String; aVector : GL.Types.Singles.Vector2);
    procedure Print_Vector (Name : String; aVector : GL.Types.Singles.Vector3);
    procedure Print_Vector (Name : String; aVector : GL.Types.Singles.Vector4);
    procedure Show_Shader_Info_Log (aProgram : gl.Objects.Programs.Program);
    procedure Show_Shader_Program_Data (aProgram : gl.Objects.Programs.Program);
    procedure Show_GL_Data;

end Utilities;
