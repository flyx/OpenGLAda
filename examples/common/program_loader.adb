
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use  Ada.Text_IO;
with GL.Files;
with GL.Objects.Programs;
with GL.Objects.Shaders;

package body Program_Loader is

    procedure Compile_Shader (Shader : GL.Objects.Shaders.Shader) is
    begin
        Shader.Compile;
        if not Shader.Compile_Status then
            Put_Line ("Shader compilation failed.");
            Put_Line ("Log:");
            Put_Line (Shader.Info_Log);
            raise Shader_Loading_Error;
        end if;
    end Compile_Shader;

    --  ------------------------------------------------------------------------

    function Program_From (List : Shader_Sources)
                           return GL.Objects.Programs.Program is
        Shader_Program : GL.Objects.Programs.Program;
    begin
        Shader_Program.Initialize_Id;

        for I in List'Range loop
            declare
                My_Shader : GL.Objects.Shaders.Shader (List (I).Kind);
            begin
                My_Shader.Initialize_Id;
                GL.Files.Load_Shader_Source_From_File (My_Shader,
                                                       Ada.Strings.Unbounded.To_String (List (I).Path));
                Compile_Shader (My_Shader);
                Shader_Program.Attach (My_Shader);
            end;
        end loop;

        Shader_Program.Link;
        if not Shader_Program.Link_Status then
            Put_Line ("Shader linking failed.");
            Put_Line ("Log:");
            Put_Line (GL.Objects.Programs.Info_Log (Shader_Program));
            raise Shader_Loading_Error;
        end if;

        GL.Objects.Shaders.Release_Shader_Compiler;
        return Shader_Program;

    exception
        when others =>
            Put_Line ("An exception occurred in Program_From.");
            raise Shader_Loading_Error;
    end Program_From;

    --  ------------------------------------------------------------------------

    function Src (Path : String; Kind : GL.Objects.Shaders.Shader_Type)
                  return Shader_Source is
    begin
        return Shader_Source'(
                              Path => Ada.Strings.Unbounded.To_Unbounded_String (Path),
                              Kind => Kind);
    exception
        when others =>
            Put_Line ("An exception occurred in Program_Loader.Src.");
            raise;
    end Src;

    --  ------------------------------------------------------------------------

end Program_Loader;
