
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Types.Colors;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Utilities;

    --  ------------------------------------------------------------------------

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

    procedure Render is
        Back_Colour : GL.Types.Colors.Color := (1.0, 1.0, 1.0, 1.0);
    begin
        Utilities.Clear_Background_Colour_And_Depth (Back_Colour);

    exception
        when others =>
            Put_Line ("An exception occurred in Render.");
            raise;
    end Render;

    --  ------------------------------------------------------------------------

    use Glfw.Input;
    Running : Boolean := True;
begin
    while Running loop
        Render;
        glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
        glfw.Input.Poll_Events;
        Running := Running and not
            (Main_Window.Key_State (glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
        Running := Running and not Main_Window.Should_Close;
    end loop;
exception
    when others =>
            Put_Line ("An exception occurred in Main_Loop.");
            raise;
end Main_Loop;
