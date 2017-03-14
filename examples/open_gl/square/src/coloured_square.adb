-- Coloured Square
-- Author Roger Mc Murtrie
-- Created 22 Febuary 2017
-- Based on https://open.gl/drawing Drawing Polygons

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Coloured_Square is
    Main_Window : Glfw.Windows.Window;
    Window_Title : constant String := "Coloured Square";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Coloured_Square returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Coloured_Square.");
        Put_Line (Exception_Information (anError));
end Coloured_Square;
