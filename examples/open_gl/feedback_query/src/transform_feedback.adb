--  Transform Feedback
--  Author Roger Mc Murtrie
--  Created 12 November 2019
--  Based on https://open.gl/feedback

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Windows;

with Initialize;
with Display;

procedure Transform_Feedback is
    Main_Window  : Glfw.Windows.Window;
    Window_Title : constant String := "Transform Feedback";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Display;
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Transform_Feedback returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Transform_Feedback.");
        Put_Line (Exception_Information (anError));
end Transform_Feedback;
