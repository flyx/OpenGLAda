-- Program Simple Texturing
-- Author Roger Mc Murtrie
-- Created 6 September 2017

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Simple_Texturing is
    Main_Window : Glfw.Windows.Window;
    Window_Title : constant String := "OpenGL SuperBible - Simple Texturing";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Simple_Texturing returned constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exceptiom occurred in Simple_Texturing.");
        Put_Line (Exception_Information (anError));
end Simple_Texturing;
