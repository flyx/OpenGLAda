
with Glfw.Input;
with glfw.Input.Keys;
with glfw.Windows.Context;

with Render;

package body Run is

    procedure Main_Loop(Main_Window         :  in out Window_Types.tWindow;
                        Rendering_Program   : GL.Objects.Programs.Program) is
        use Glfw.Input;
        Running  : Boolean := True;
    begin
        while Running loop
            Render.Render(Rendering_Program, glfw.Time);
            glfw.Windows.Context.Swap_Buffers(Main_Window'Access);
            glfw.Input.Poll_Events;
            Running := Running and not (Main_Window.Key_State(glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
            Running := Running and not Main_Window.Should_Close;
        end loop;
    end Main_Loop;

end Run;
