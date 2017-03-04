--  Program Trivial
--  Author Roger Mc Murtrie
--  Created 13 Febuary 2017
--  Based on Durian Software's
--  An intro to modern OpenGL. Chapter 2: Hello World
--  Extremely trivial. Just displays a window.

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Trivial is
    Main_Window : Glfw.Windows.Window;
    Window_Title : constant String := "Durian Software - Hello";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Trivial returned constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Trivial.");
        Put_Line (Exception_Information (anError));
end Trivial;
