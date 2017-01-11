
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Culling;
with GL.Errors;
with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Shaders.Lists;
with GL.Toggles;
with GL.Types;
with GL.Types.Colors;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Shaders_Program;
with Utilities;

package body Run is

    package Math_Functions is new Ada.Numerics.Generic_Elementary_Functions (GL.Types.Single);
    subtype tVec4f is GL.Types.Singles.Vector4;

    Rendering_Program : GL.Objects.Programs.Program;
    Vertex_Array      : GL.Objects.Vertex_Arrays.Vertex_Array_Object;

    procedure Render_Triangle (Current_Time : Glfw.Seconds);
    function Setup_Graphic return Boolean;

    --  ------------------------------------------------------------------------

    procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is
        use Glfw.Input;
        Running : Boolean := True;
    begin
        if Setup_Graphic then
            while Running loop
                Render_Triangle (Glfw.Time);
                Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
                Glfw.Input.Poll_Events;
                Running := Running and not (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
                Running := Running and not Main_Window.Should_Close;
            end loop;
        end if;
        --  Deleting the following two lines causes a PROGRAM_ERROR
        Vertex_Array.Delete_Id;
        Rendering_Program.Delete_Id;
    end Main_Loop;

    --  ------------------------------------------------------------------------

    procedure Render_Triangle (Current_Time : Glfw.Seconds) is
        use Math_Functions;
        use GL.Types;
        Back_Colour : GL.Types.Colors.Color := (0.5 * (1.0 + Sin (Single (Current_Time))),
                                                0.5 * (1.0 + Cos (Single (Current_Time))), 0.0, 1.0);
        colour      : tVec4f  := (0.8 * (1.0 + Sin (Single (Current_Time))),
                                  0.4 * (1.0 + Cos (Single (Current_Time))), 0.4, 1.0);
        offset      : tVec4f := (0.5 * Sin (Single (Current_Time)),
                                 0.5 * Cos (Single (Current_Time)), 0.0, 0.0);
    begin
        GL.Buffers.Clear ((True, False, False, True));
        GL.Buffers.Set_Color_Clear_Value (Back_Colour);

        GL.Objects.Programs.Use_Program (Rendering_Program);

        GL.Attributes.Set_Single (0, offset);
        GL.Attributes.Set_Single (1, colour);
        GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, 3);

    exception
        when anError : Constraint_Error =>
            Put ("Render_Triangle returned constraint error: ");
            Put_Line (Exception_Information (anError));

        when anError : GL.Errors.Invalid_Operation_Error =>
            Put_Line ("Render_Triangle returned an invalid operation error: ");
            Put_Line (Exception_Information (anError));

        when anError :  others =>
            Put_Line ("An exceptiom occurred in Render_Triangle.");
            Put_Line (Exception_Information (anError));

    end Render_Triangle;

    --  ------------------------------------------------------------------------

    function Setup_Graphic return Boolean is
        OK : Boolean := Shaders_Program.Make_Shader_Program (Rendering_Program);
    begin
        if OK then
            GL.Toggles.Enable (GL.Toggles.Depth_Test);
            GL.Toggles.Enable (GL.Toggles.Cull_Face);
            GL.Culling.Set_Front_Face (GL.Types.Clockwise);
            GL.Culling.Set_Cull_Face (GL.Culling.Back);
            GL.Buffers.Set_Depth_Function (GL.Types.Less);
            Vertex_Array.Initialize_Id;
            Vertex_Array.Bind;
        end if;
        Utilities.Show_Shader_Program_Data (Rendering_Program);
        return OK;
    end Setup_Graphic;

    --  ------------------------------------------------------------------------

end Run;
