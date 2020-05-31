
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;
with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Draw_Transform is
    Main_Window  : Glfw.Windows.Window;
    Window_Title : constant String := "Red Book Tutorial 10 - Draw Transform Buffer";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Draw_Transform returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Draw_Transform.");
        Put_Line (Exception_Information (anError));

end Draw_Transform;
