--  Element Buffers
--  Author Roger Mc Murtrie
--  Created 16 Febuary 2017
--  Based on https://open.gl/drawing Drawing Polygons

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Element_Buffers is
    Main_Window  : Glfw.Windows.Window;
    Window_Title : constant String := "Element_Buffers";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Element_Buffers returned a constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exception occurred in Element_Buffers.");
        Put_Line (Exception_Information (anError));
end Element_Buffers;
