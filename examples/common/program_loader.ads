with Ada.Strings.Unbounded;

with GL.Objects.Shaders;
with GL.Objects.Programs;

package Program_Loader is
    type Shader_Source is private;

    type Shader_Sources is array (Positive range <>) of Shader_Source;

    Shader_Loading_Error : exception;

    function Src (Path : String; Kind : GL.Objects.Shaders.Shader_Type)
      return Shader_Source;

    function Program_From (List : Shader_Sources)
      return GL.Objects.Programs.Program;
private
    type Shader_Source is record
        Path : Ada.Strings.Unbounded.Unbounded_String;
        Kind : GL.Objects.Shaders.Shader_Type;
    end record;
end Program_Loader;
