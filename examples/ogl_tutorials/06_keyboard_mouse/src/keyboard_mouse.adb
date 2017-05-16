
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Keyboard_Mouse is
    Main_Window : Glfw.Windows.Window;
    Window_Title : String := "Tutorial 6 - Keyboard_Mouse Cube";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;
exception
    when anError : Constraint_Error =>
        Put ("Tutorial_6 returned constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Tutorial_6.");
        Put_Line (Exception_Information (anError));
end Keyboard_Mouse;
