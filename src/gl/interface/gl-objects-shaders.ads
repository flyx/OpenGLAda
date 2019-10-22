--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

private with GL.Low_Level;

package GL.Objects.Shaders is
   pragma Preelaborate;

   type Shader_Type is (Fragment_Shader, Vertex_Shader, Geometry_Shader,
                        Tess_Evaluation_Shader, Tess_Control_Shader);

   type Shader (Kind : Shader_Type) is new GL_Object with private;

   procedure Set_Source (Subject : Shader; Source : String);
   function Source (Subject : Shader) return String;

   procedure Compile (Subject : Shader);

   procedure Release_Shader_Compiler;

   function Compile_Status (Subject : Shader) return Boolean;

   function Info_Log (Subject : Shader) return String;

   -- low-level
   function Create_From_Id (Id : UInt) return Shader;
private
   type Shader (Kind : Shader_Type) is new GL_Object with null record;

   overriding
   procedure Internal_Create_Id (Object : Shader; Id : out UInt);

   overriding
   procedure Internal_Release_Id (Object : Shader; Id : UInt);

   for Shader_Type use (Fragment_Shader        => 16#8B30#,
                        Vertex_Shader          => 16#8B31#,
                        Geometry_Shader        => 16#8DD9#,
                        Tess_Evaluation_Shader => 16#8E87#,
                        Tess_Control_Shader    => 16#8E88#);
   for Shader_Type'Size use Low_Level.Enum'Size;

end GL.Objects.Shaders;
