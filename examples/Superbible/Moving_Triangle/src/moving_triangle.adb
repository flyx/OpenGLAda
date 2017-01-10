--  Program Moving_Triangle
--  Author Roger Mc Murtrie
--  Created 16 December 2016

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw;

with Initialization;
with Run;
with Window_Types;

procedure Moving_Triangle is
    Main_Window : Window_Types.tWindow;
    Window_Title : String := "OpenGL SuperBible Moving Triangle Example";
begin
    Glfw.Init;
    Initialization.Initialize (Main_Window, Window_Title);
    Run.Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Moving_Triangle returned constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exceptiom occurred in Moving_Triangle.");
        Put_Line (Exception_Information (anError));

end Moving_Triangle;
