-- Program A_Dot
-- Author Roger Mc Murtrie
-- Created 16 December 2016

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Programs;
with GL.Objects.Vertex_Arrays;

with glfw;

with Initialization;
with Run;
with Utilities;
with Window_Types;

procedure A_Dot is
    Main_Window : Window_Types.tWindow;
begin
    glfw.Init;
    Initialization.Initialize (Main_Window);
    Run.Main_Loop (Main_Window);
    glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Application returned constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exceptiom occurred in Application.");
        Put_Line (Exception_Information (anError));
end A_Dot;
