--  Program Simple_Geometry (including tesselation)
--  Author Roger Mc Murtrie
--  Created 16 December 2016

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;

with Initialize;
with Main_Loop;
with Glfw.Windows;

procedure Simple_Geometry is
    Main_Window : Glfw.Windows.Window;
    Window_Title : String := "OpenGL SuperBible Simple Tesselation Example";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Simple_Geometry returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exceptiom occurred in Simple_Geometry.");
        Put_Line (Exception_Information (anError));
end Simple_Geometry;
