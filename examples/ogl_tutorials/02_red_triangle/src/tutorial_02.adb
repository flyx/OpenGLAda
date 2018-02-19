
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Tutorial_02 is
    Main_Window  : Glfw.Windows.Window;
    Window_Title : constant String := "Tutorial 2 - Red Triangle";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Tutorial_02 returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exceptiom occurred in Tutorial_02.");
        Put_Line (Exception_Information (anError));

end Tutorial_02;
