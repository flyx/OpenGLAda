with System;

with GL.Objects.Programs;

with Glfw;

package Render is

    procedure Render(Program : GL.Objects.Programs.Program; Current_Time : Glfw.Seconds);

end Render;
