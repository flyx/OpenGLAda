
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Buffers;
--  with GL.Errors;
with GL.Types;
with GL.Types.Colors;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Utilities;

    --  ------------------------------------------------------------------------

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

    procedure Render is
        use GL.Types;
        Back_Colour : GL.Types.Colors.Color := (1.0 , 1.0 , 1.0, 1.0);
    begin
        Utilities.Clear_Background_Colour_And_Depth (Back_Colour);

    exception
        when others =>
            Put_Line ("An exceptiom occurred in Render.");
            raise;
    end Render;

    --  ------------------------------------------------------------------------

    procedure Setup_Graphic is
    begin
       null;
    end Setup_Graphic;

    --  ------------------------------------------------------------------------

    use Glfw.Input;
    Running : Boolean := True;
begin
    Setup_Graphic;
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
            Put_Line ("An exceptiom occurred in Main_Loop.");
            raise;
end Main_Loop;
