--  Program Colour Vertices
--  Author Roger Mc Murtrie
--  Created 17 December 2016

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Colour_Vertices is
    Main_Window   : aliased Glfw.Windows.Window;
begin
    Glfw.Init;
    Initialize (Main_Window, "OpenGL SuperBible Vertex Colouring Example");
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Colour_Vertices returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exceptiom occurred in Colour_Vertices.");
        Put_Line (Exception_Information (anError));
end Colour_Vertices;
