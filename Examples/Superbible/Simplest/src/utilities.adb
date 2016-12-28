
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;

with gl.Context;
with GL.Errors;
with GL.Objects.Shaders.Lists;
with GL.Types; use  GL.Types;

package body Utilities is

    procedure Show_GL_Data is
        GL_Version                                 : Unbounded_String;
        Renderer                                   : Unbounded_String;
        Shading_Language_Version                   : Unbounded_String;
    begin
        GL_Version := To_Unbounded_String (GL.Types.Int'image (gl.Context.Major_Version) & "." &
                                             GL.Types.Int'image (gl.Context.Minor_Version));
        Renderer := To_Unbounded_String (gl.Context.Renderer);
        Shading_Language_Version :=
          To_Unbounded_String (gl.Context.Primary_Shading_Language_Version);
        New_Line;
        Put_Line ("OpenGL version supported: " & To_String (GL_Version));
        Put_Line ("Renderer: " & To_String (Renderer));
        Put_Line ("Primary_Shading_Language_Version: " & To_String (Shading_Language_Version));
        New_Line;
    end Show_GL_Data;

    --  ------------------------------------------------------------------------------------------------------------------------

    procedure Show_Shader_Program_Data (aProgram : gl.Objects.Programs.Program) is
        use GL.Objects;
        Shaders_List        : Shaders.Lists.List := Programs.Attached_Shaders (aProgram);
        List_Cursor         : Shaders.Lists.Cursor := Shaders_List.First;
        Shader1             : Shaders.Shader := Shaders.Lists.Element (List_Cursor);
        Shader_Count        : Positive := 1;
    begin
        Put_Line ("Shader: " & Positive'Image (Shader_Count));
        Put_Line (Shaders.Source (Shader1));

        while Shaders.Lists.Has_Next (List_Cursor)  loop
            List_Cursor := Shaders.Lists.Next (List_Cursor);
            declare
                ShaderN  : Shaders.Shader := Shaders.Lists.Element (List_Cursor);
            begin
                Shader_Count := Shader_Count + 1;
                Put_Line ("Shader: " & Positive'Image (Shader_Count));
                Put_Line (Shaders.Source (ShaderN));
            end;
        end loop;
        New_Line;

    exception
        when anError : Constraint_Error =>
            Put ("Show_Shader_Program_Data returned constraint error: ");
            Put_Line (Exception_Information (anError));

        when anError : GL.Errors.Invalid_Operation_Error =>
            Put_Line ("Show_Shader_Program_Data returned an invalid operation error: ");
            Put_Line (Exception_Information (anError));

        when anError :  others =>
            Put_Line ("An exceptiom occurred in Show_Shader_Program_Data.");
            Put_Line (Exception_Information (anError));

    end Show_Shader_Program_Data;

    --  ------------------------------------------------------------------------------------------------------------------------

end Utilities;
