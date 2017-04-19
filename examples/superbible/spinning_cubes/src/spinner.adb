--  Program Spinning Cubes
--  Based on OpenGL SuperBible spinnycube.cpp
--  Author Roger Mc Murtrie
--  Created 12 February 2017

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Spinner is
    Main_Window :  Glfw.Windows.Window;
begin
    Glfw.Init;
    Initialize (Main_Window, "OpenGL SuperBible Example - Spinning Cubes");
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Spinner returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Spinner.");
        Put_Line (Exception_Information (anError));
end Spinner;
