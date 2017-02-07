--  Program Simple_Tessalation
--  Author Roger Mc Murtrie
--  Created 16 December 2016

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Simple_Tesselation is
    Main_Window : aliased Glfw.Windows.Window;
    Window_Title : constant String
      := "OpenGL SuperBible Simple Tesselation Example";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Simple_Tesselation returned constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError : others =>
        Put_Line ("An exceptiom occurred in Simple_Tesselation.");
        Put_Line (Exception_Information (anError));
end Simple_Tesselation;
