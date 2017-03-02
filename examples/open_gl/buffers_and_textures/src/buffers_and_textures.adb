--  Program Buffers_And_Textures
--  Author Roger Mc Murtrie
--  Created 13 February 2017
--  Based on https://open.gl/textures

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Windows;

with Initialize;
with Main_Loop;

procedure Buffers_And_Textures is
    Main_Window  : Glfw.Windows.Window;
    Window_Title : constant String := "Open GL - Textures Example";
begin
    Glfw.Init;
    Initialize (Main_Window, Window_Title);
    Main_Loop (Main_Window);
    Glfw.Shutdown;

exception
    when anError : Constraint_Error =>
        Put ("Buffers_And_Textures returned constraint error: ");
        Put_Line (Exception_Information (anError));

    when anError :  others =>
        Put_Line ("An exceptiom occurred in Buffers_And_Textures.");
        Put_Line (Exception_Information (anError));
end Buffers_And_Textures;
