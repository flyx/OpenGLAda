-- Particle Display
-- Author Roger Mc Murtrie
-- Created 23 October 2019

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Particle_Display is
    Main_Window : Glfw.Windows.Window;
    Window_Title : constant String := "OpenGL SuperBible Particles Example";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Particle_Display returned constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exceptiom occurred in Particle_Display.");
        Put_Line (Exception_Information (anError));
end Particle_Display;
