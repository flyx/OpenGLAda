--  Draw_Triangles implements Redbook example 03-drawcommands from OGLPG-9th-Edition
--  Author: R Mc Murtrie
--  5th March 1918

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Draw_Triangles is
    Main_Window  : Glfw.Windows.Window;
    Window_Title : constant String := "Drawing Commands";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Draw_Triangles returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Draw_Triangles.");
        Put_Line (Exception_Information (anError));

end Draw_Triangles;
