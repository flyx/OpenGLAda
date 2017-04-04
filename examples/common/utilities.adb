
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;

with GL.Buffers;
with GL.Context;
with GL.Errors;
with GL.Objects.Shaders.Lists;
with GL.Types.Colors;

package body Utilities is
   --  Set_Color_Clear_Value sets the value to which a buffer should be set
   --  when cleared.
   --  Clear "clears" selected values to the previously selected value set by
   --  Set_Color_Clear_Value .
   procedure Clear_All (Colour : GL.Types.Colors.Color) is
   begin
        GL.Buffers.Set_Color_Clear_Value (Colour);
        GL.Buffers.Clear ((True, True, True, True));   end Clear_All;

    --  ------------------------------------------------------------------------
   procedure Clear_Background_Colour (Colour : GL.Types.Colors.Color) is
   begin
        GL.Buffers.Set_Color_Clear_Value (Colour);
        GL.Buffers.Clear ((False, False, False, True));
   end Clear_Background_Colour;

    --  ------------------------------------------------------------------------

   procedure Clear_Background_Colour_And_Depth (Colour : GL.Types.Colors.Color) is
   begin
        GL.Buffers.Set_Color_Clear_Value (Colour);
        GL.Buffers.Clear ((True, False, False, True));
   end Clear_Background_Colour_And_Depth;

    --  ------------------------------------------------------------------------

    procedure Print_GL_Array3 (Name : String; anArray : GL.Types.Singles.Vector3_Array) is
        use GL.Types;
    begin
        Put_Line (Name & ": ");
        for Index in anArray'First .. anArray'Last loop
            Print_Vector (Int'Image (Index) & ":  ", anArray (Index));
        end loop;
        New_Line;
    end Print_GL_Array3;

    --  ------------------------------------------------------------------------

    procedure Print_GL_Array4 (Name : String; anArray : GL.Types.Singles.Vector4_Array) is
        use GL.Types;
    begin
        Put_Line (Name & ": ");
        for Index in anArray'First .. anArray'Last loop
            Print_Vector (Int'Image (Index) & ":  ", anArray (Index));
        end loop;
        New_Line;
    end Print_GL_Array4;

    --  ------------------------------------------------------------------------


    procedure Print_Matrix (Name : String;
                            aMatrix : GL.Types.Singles.Matrix3) is
        use GL.Types.Singles;
    begin
        Put_Line (Name & ":");
        for Row in GL.Index_3D'Range loop
            for Column in GL.Index_3D'Range loop
                Put (GL.Types.Single'Image (aMatrix (Row, Column)) & "   ");
            end loop;
            New_Line;
        end loop;
        New_Line;
    end Print_Matrix;

    --  ------------------------------------------------------------------------

    procedure Print_Matrix (Name : String;
                            aMatrix : GL.Types.Singles.Matrix4) is
        use GL.Types.Singles;
    begin
        Put_Line (Name & ":");
        for Row in GL.Index_Homogeneous'Range loop
            for Column in GL.Index_Homogeneous'Range loop
                Put (GL.Types.Single'Image (aMatrix (Row, Column)) & "   ");
            end loop;
            New_Line;
        end loop;
        New_Line;
    end Print_Matrix;

    --  ------------------------------------------------------------------------

    procedure Print_Vector (Name : String; aVector : GL.Types.Singles.Vector3) is
    begin
        Put (Name & ":  ");
        for Index in aVector'Range loop
            Put (GL.Types.Single'Image (aVector (Index)) & "   ");
        end loop;
        New_Line;
    end Print_Vector;

    --  ------------------------------------------------------------------------

    procedure Print_Vector (Name : String; aVector : GL.Types.Singles.Vector4) is
    begin
        Put (Name & ":  ");
        for Index in aVector'Range loop
            Put (GL.Types.Single'Image (aVector (Index)) & "   ");
        end loop;
        New_Line;
    end Print_Vector;

    --  ------------------------------------------------------------------------

    procedure Show_GL_Data is
        GL_Version                : Unbounded_String;
        Renderer                  : Unbounded_String;
        Shading_Language_Version  : Unbounded_String;
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

    --  ------------------------------------------------------------------------

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

    --  ------------------------------------------------------------------------

end Utilities;
