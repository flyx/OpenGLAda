
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Buffers;
with GL.Toggles;
with GL.Errors;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Rasterization;
with GL.Toggles;
with GL.Types;
with GL.Types.Colors;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Shaders_Program;
with Utilities;

package body Run is
    package Math_Functions is new Ada.Numerics.Generic_Elementary_Functions (GL.Types.Single);

    procedure Render_Dot (Current_Time : Glfw.Seconds);
    function Startup (Main_Window : Window_Types.tWindow) return Boolean;

    Rendering_Program  : GL.Objects.Programs.Program;
    Vertex_Array       :  GL.Objects.Vertex_Arrays.Vertex_Array_Object;

    --  ------------------------------------------------------------------------

    procedure Main_Loop (Main_Window :  in out Window_Types.tWindow) is
        use Glfw.Input;
        Running : Boolean := True;
    begin
        if Startup (Main_Window) then
            while Running loop
                Render_Dot (Glfw.Time);
                glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
                glfw.Input.Poll_Events;
                Running := Running and not
                  (Main_Window.Key_State (glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
                Running := Running and not Main_Window.Should_Close;
            end loop;
        end if;

        Vertex_Array.Delete_Id;
        Rendering_Program.Delete_Id;
    end Main_Loop;

    --  ------------------------------------------------------------------------

    procedure Render_Dot (Current_Time : Glfw.Seconds) is
        use GL.Types;
        use Math_Functions;

        Back_Colour : GL.Types.Colors.Color := (0.5 * (1.0 + Sin (Single (Current_Time))),
                                                0.5 * (1.0 + Cos (Single (Current_Time))), 0.0, 1.0);
    begin
        GL.Buffers.Clear ((True, False, False, True));
        GL.Buffers.Set_Color_Clear_Value (Back_Colour);

        GL.Objects.Programs.Use_Program (Rendering_Program);
        GL.Objects.Vertex_Arrays.Draw_Arrays (Points, 0, 1);

    exception
        when anError : Constraint_Error =>
            Put ("Render returned constraint error: ");
            Put_Line (Exception_Information (anError));

        when anError : GL.Errors.Invalid_Operation_Error =>
            Put_Line ("Render returned an invalid operation error: ");
            Put_Line (Exception_Information (anError));

        when anError :  others =>
            Put_Line ("An exceptiom occurred in Render.");
            Put_Line (Exception_Information (anError));

    end Render_Dot;

    --  ------------------------------------------------------------------------

    function Startup (Main_Window : Window_Types.tWindow) return Boolean is
        OK : Boolean := Shaders_Program.Make_Shader_Program (Main_Window, Rendering_Program);
    begin
        if OK then
            gl.Toggles.Enable (gl.Toggles.Depth_Test);
            gl.Buffers.Set_Depth_Function (gl.Types.Less);
            Vertex_Array.Initialize_Id;
            Vertex_Array.Bind;
            -- Point size set in vertex shader
            gl.Toggles.Enable (gl.Toggles.Vertex_Program_Point_Size);
        else
            Put_Line ("Shader program linking failed.");
            Put_Line ("Log:");
            Put_Line (Rendering_Program.Info_Log);
        end if;
        Utilities.Show_Shader_Program_Data (Rendering_Program);
        return OK;
    end Startup;

    --  ------------------------------------------------------------------------


end Run;
