
with GL.Objects.Programs;

with Window_Types;

package Shaders_Program is

    function Make_Shader_Program
      (Shader_Program : out GL.Objects.Programs.Program) return Boolean;

end Shaders_Program;
