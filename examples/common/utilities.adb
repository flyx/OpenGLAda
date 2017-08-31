
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;

with GL.Buffers;
with GL.Context;
with GL.Objects.Shaders.Lists;
with GL.Types.Colors;

package body Utilities is

    generic
        type Index_Type is (<>);
        type Vector_Type is  array (Index_Type) of aliased GL.Types.Single;
    procedure Print_Vector1 (Name : String; aVector : Vector_Type);

    procedure Print_Vector1 (Name : String; aVector : Vector_Type) is
    begin
        Put (Name & ":  ");
        for Index in aVector'Range loop
            Put (GL.Types.Single'Image (aVector (Index)) & "   ");
        end loop;
        New_Line;
    end Print_Vector1;

    --  -------------------------------------------------------------------

    procedure Print_Vector2 is
        new Print_Vector1 (GL.Index_2D, GL.Types.Singles.Vector2);
    procedure Print_Vector3 is
        new Print_Vector1 (GL.Index_3D, GL.Types.Singles.Vector3);
    procedure Print_Vector4 is
        new Print_Vector1 (GL.Index_Homogeneous, GL.Types.Singles.Vector4);
    procedure Print_Vector5 is
        new Print_Vector1 (Maths.Index_5, Maths.Vector5);
    procedure Print_Vector6 is
        new Print_Vector1 (Maths.Index_6, Maths.Vector6);

    --  ---------------------------------------------------------------

--  Set_Color_Clear_Value sets the value to which a buffer should be set
--  when cleared.
--  Clear "clears" selected values to the previously selected value set by
--  Set_Color_Clear_Value .
    procedure Clear_All (Colour : GL.Types.Colors.Color) is
    begin
        GL.Buffers.Set_Color_Clear_Value (Colour);
        GL.Buffers.Clear ((True, True, True, True));
    end Clear_All;

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

    procedure Enable_Mouse_Callbacks (Window : in out Glfw.Windows.Window; Enable : Boolean) is
    begin
        if Enable then
            Window.Enable_Callback (Glfw.Windows.Callbacks.Mouse_Position);
            Window.Enable_Callback (Glfw.Windows.Callbacks.Mouse_Enter);
            Window.Enable_Callback (Glfw.Windows.Callbacks.Mouse_Button);
            Window.Enable_Callback (Glfw.Windows.Callbacks.Mouse_Scroll);
        else
            Window.Disable_Callback (Glfw.Windows.Callbacks.Mouse_Position);
            Window.Disable_Callback (Glfw.Windows.Callbacks.Mouse_Enter);
            Window.Disable_Callback (Glfw.Windows.Callbacks.Mouse_Button);
            Window.Disable_Callback (Glfw.Windows.Callbacks.Mouse_Scroll);
        end if;
    end;

    --  ------------------------------------------------------------------------

    procedure Print_GL_Array3 (Name : String; anArray : GL.Types.Singles.Vector3_Array) is
        use GL.Types;
    begin
        Put_Line (Name & ": ");
        for Index in anArray'First .. anArray'Last loop
            Print_Vector3 (Name, anArray (Index));
        end loop;
        New_Line;
    end Print_GL_Array3;

    --  ------------------------------------------------------------------------

    procedure Print_GL_Array4 (Name : String; anArray : GL.Types.Singles.Vector4_Array) is
        use GL.Types;
    begin
        Put_Line (Name & ": ");
        for Index in anArray'First .. anArray'Last loop
            Print_Vector4 (Name, anArray (Index));
        end loop;
        New_Line;
    end Print_GL_Array4;

    --  ------------------------------------------------------------------------

    procedure Print_Array6 (Name : String; anArray : Maths.Vector6_Array) is
        use GL.Types;
    begin
        Put_Line (Name & ": ");
        for Index in anArray'First .. anArray'Last loop
            Print_Vector6 (Name, anArray (Index));
        end loop;
        New_Line;
    end Print_Array6;

    --  ---------------------------------------------------------------

    procedure Print_GL_Int_Array (Name : String; anArray : GL.Types.Int_Array) is
        use GL.Types;
    begin
        Put_Line (Name & ": ");
        for Index in anArray'First .. anArray'Last loop
            Put_line (Int'Image (Index) & ":  " & Int'Image (anArray (Index)));
        end loop;
        New_Line;
    end Print_GL_Int_Array;

    --  ------------------------------------------------------------------------

    procedure Print_Matrix (Name    : String;
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

    procedure Print_Matrix (Name    : String;
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

    procedure Print_Vector (Name : String; aVector : GL.Types.Singles.Vector2) is
    begin
       Print_Vector2 (Name, aVector);
    end Print_Vector;

    --  ------------------------------------------------------------------------

    procedure Print_Vector (Name : String; aVector : GL.Types.Singles.Vector3) is
    begin
       Print_Vector3 (Name, aVector);
    end Print_Vector;

    --  ------------------------------------------------------------------------

    procedure Print_Vector (Name : String; aVector : GL.Types.Singles.Vector4) is
    begin
       Print_Vector4 (Name, aVector);
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
        when others =>
            Put_Line ("An exceptiom occurred in Show_Shader_Program_Data.");
            raise;

    end Show_Shader_Program_Data;

    --  ------------------------------------------------------------------------

    procedure Show_Shader_Info_Log (aProgram : gl.Objects.Programs.Program) is
        use GL.Objects;
        Shaders_List        : Shaders.Lists.List := Programs.Attached_Shaders (aProgram);
        List_Cursor         : Shaders.Lists.Cursor := Shaders_List.First;
        Shader1             : Shaders.Shader := Shaders.Lists.Element (List_Cursor);
        Shader_Count        : Positive := 1;
    begin
        Put_Line ("Shader: " & Positive'Image (Shader_Count) & " log:");
        while Shaders.Lists.Has_Next (List_Cursor)  loop
            List_Cursor := Shaders.Lists.Next (List_Cursor);
            declare
                ShaderN  : Shaders.Shader := Shaders.Lists.Element (List_Cursor);
            begin
                Shader_Count := Shader_Count + 1;
                Put_Line ("Shader: " & Positive'Image (Shader_Count) & " log:");
                declare
                    Shader_Log : String := GL.Objects.Shaders.Info_Log (ShaderN);
                begin
                    Put_Line (Shader_Log);
                end;
            end;
        end loop;
        New_Line;
    exception
        when others =>
            Put_Line ("An exceptiom occurred in Show_Shader_Info_Log.");
            raise;

    end Show_Shader_Info_Log;

    --  ------------------------------------------------------------------------
end Utilities;
