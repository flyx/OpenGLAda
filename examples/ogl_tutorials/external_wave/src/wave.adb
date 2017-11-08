--  Program Hello_Textures
--  Author Roger Mc Murtrie
--  Created 2 June 2017
--  Based on OGL_Tutorials external wave.c
--  modified, in particular, to use GLSL programmed shaders
--  Quads, now deprecated, have been changed to triangle pairs

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Wave is
    Main_Window  : Glfw.Windows.Window;
    Window_Title : constant String := "Wave Simulation";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Wave returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exceptiom occurred in Wave.");
        Put_Line (Exception_Information (anError));

end Wave;
