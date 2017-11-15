
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Textured_Cube is
    Main_Window  : Glfw.Windows.Window;
    Window_Title : constant String := "Tutorial 5 - Textured Cube";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;
exception
    when anError : Constraint_Error =>
        Put ("Tutorial_5 returned constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Tutorial_5.");
        Put_Line (Exception_Information (anError));
end Textured_Cube;
