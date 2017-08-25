--  Program Hello_Character
--  Author Roger Mc Murtrie
--  Created 25 August 2017
--  Based on Durian Software's
--  An intro to modern OpenGL. Chapter 2.1: Buffers and Textures

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Hello_Character is
    Main_Window : Glfw.Windows.Window;
    Window_Title : constant String := "Single Freetype Character Example";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Hello_Character returned constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exceptiom occurred in Hello_Character.");
        Put_Line (Exception_Information (anError));
end Hello_Character;
