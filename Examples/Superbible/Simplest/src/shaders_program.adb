
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use  Ada.Text_IO;

with GL.Errors;
with GL.Files;
with GL.Objects.Shaders;

package body Shaders_Program is

     --  -----------------------------------------------------------------------------------------------------------------------

    function Compile_Shader(Shader : GL.Objects.Shaders.Shader) return Boolean is
        result : Boolean := False;
    begin
        Shader.Compile;
        result := Shader.Compile_Status;
        GL.Objects.Shaders.Release_Shader_Compiler;
        if not result then
           Put_Line("Shader compilation failed.");
           Put_Line("Log:");
           Put_Line(Shader.Info_Log);
        end if;
        return result;

    end Compile_Shader;

    --  -----------------------------------------------------------------------------------------------------------------------

    function Make_Shader_Program(Window         : Window_Types.tWindow;
                                 Shader_Program : out GL.Objects.Programs.Program) return Boolean is

        Vertex_Shader         : GL.Objects.Shaders.Shader(GL.Objects.Shaders.Vertex_Shader);
        Fragment_Shader    : GL.Objects.Shaders.Shader(GL.Objects.Shaders.Fragment_Shader);
        Linked                       : Boolean := False;

    begin
        Vertex_Shader.Initialize_Id;
        Fragment_Shader.Initialize_Id;

        GL.Files.Load_Shader_Source_From_File (Vertex_Shader, "src/shaders/vertex_shader.glsl");
        GL.Files.Load_Shader_Source_From_File (Fragment_Shader, "src/shaders/fragment_shader.glsl");

        if Compile_Shader(Vertex_Shader) then
                    if Compile_Shader(Fragment_Shader) then
                        Shader_Program.Initialize_Id;
                        Shader_Program.Attach(Vertex_Shader);
                        Shader_Program.Attach(Fragment_Shader);

                        Shader_Program.Link;
                        Linked := Shader_Program.Link_Status;
                    end if;
        end if;

        Vertex_Shader.Delete_Id;
        Fragment_Shader.Delete_Id;
        return Linked;

exception
      when anError : Constraint_Error =>
         Put("Make_Shader_Program returned constraint error: ");
         Put_Line(Exception_Information(anError));
         return Linked;

      when anError : GL.Errors.Invalid_Operation_Error =>
         Put_Line("Make_Shader_Program returned an invalid operation error: ");
         Put_Line(Exception_Information(anError));
         return Linked;

      when anError :  others =>
         Put_Line("An exceptiom occurred in Make_Shader_Program.");
        Put_Line(Exception_Information(anError));
         return Linked;
    end Make_Shader_Program;

end Shaders_Program;
