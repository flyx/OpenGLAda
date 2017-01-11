
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Errors;

with Glfw;
with Glfw.Input.Mouse;
with Glfw.Windows;
with Glfw.Windows.Context;
with Glfw.Windows.Hints;

with Utilities;

package body Initialization is

    procedure Set_Window_Hints;

    --  ------------------------------------------------------------------------

    procedure Enable_Callbacks (thisWindow : in out Glfw.Windows.Window) is
    begin
        thisWindow.Enable_Callback (Glfw.Windows.Callbacks.Size);
        thisWindow.Enable_Callback (Glfw.Windows.Callbacks.Key);
        thisWindow.Enable_Callback (Glfw.Windows.Callbacks.Mouse_Button);
        thisWindow.Enable_Callback (Glfw.Windows.Callbacks.Mouse_Scroll);
    end Enable_Callbacks;

    --  ------------------------------------------------------------------------

    procedure Initialize (Main_Window  : in out Glfw.Windows.Window;
                          Window_Title : String) is

        Window_Width  : constant Glfw.Size := 800;
        Window_Height : constant Glfw.Size := 600;
        Cursor        : Glfw.Input.Mouse.Cursor_Mode := Glfw.Input.Mouse.Hidden;
    begin
        Set_Window_Hints;
        Main_Window.Init (Window_Width, Window_Height, Window_Title);
        Glfw.Windows.Context.Make_Current (Main_Window'Access);
        Enable_Callbacks (Main_Window);
        Main_Window.Set_Cursor_Mode (Cursor);
        Utilities.Show_GL_Data;

    exception
        when anError : Constraint_Error =>
            Put ("Initialize returned constraint error: ");
            Put_Line (Exception_Information (anError));

        when anError : GL.Errors.Invalid_Operation_Error =>
            Put_Line ("Initialize returned an invalid operation error: ");
            Put_Line (Exception_Information (anError));

        when anError :  others =>
            Put_Line ("An exceptiom occurred in Initialize.");
            Put_Line (Exception_Information (anError));

    end Initialize;

    --  ------------------------------------------------------------------------

    procedure Set_Window_Hints is
        Min_Major_Version : Integer := 3;
        Minor_Version     : Integer := 2;
    begin
        Glfw.Windows.Hints.Set_Minimum_OpenGL_Version (Min_Major_Version, Minor_Version);
        Glfw.Windows.Hints.Set_Forward_Compat (True);
        Glfw.Windows.Hints.Set_Profile (Glfw.Windows.Context.Core_Profile);
        Glfw.Windows.Hints.Set_Debug_Context (True);
        --  Set samples to 16 before taking screen shots.
        Glfw.Windows.Hints.Set_Samples (4);

    end Set_Window_Hints;

    --  ------------------------------------------------------------------------

end Initialization;
