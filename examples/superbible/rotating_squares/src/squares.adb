--  Program Rotating Squares
--  Adapted from Spinning Cube listings of
--  OpenGL SuperBible Chapter 5, Using Uniforms To Transform Geometry
--  and OpenGL SuperBible code spinnycube.cpp
--  Author Roger Mc Murtrie
--  Created 26 March 2017

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Squares is
    Main_Window :  Glfw.Windows.Window;
begin
    Glfw.Init;
    Initialize (Main_Window, "OpenGL Example - Rotating Squares");
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Squares returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Squares.");
        Put_Line (Exception_Information (anError));
end Squares;
