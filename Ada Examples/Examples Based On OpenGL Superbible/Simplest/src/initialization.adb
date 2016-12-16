
with Interfaces.C;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with gl.Context;
with gl.Buffers;
with GL.Errors;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Shaders.Lists;
with GL.Toggles;
with GL.Types; use  GL.Types;

with Glfw.Input.Mouse;
with glfw.Windows;
with glfw.Windows.Context;
with glfw.Windows.Hints;

with Shaders_Program;
with Utilities;

package body Initialization is

    procedure Set_Window_Hints;
    function Startup(Main_Window : Window_Types.tWindow;
                     Rendering_Program : out GL.Objects.Programs.Program;
                     Vertex_Array              : in out GL.Objects.Vertex_Arrays.Vertex_Array_Object) return Boolean;

    --  -----------------------------------------------------------------------------------------------------------------------------

    procedure Enable_Callbacks(thisWindow : in out Window_Types.tWindow) is
    begin
        thisWindow.Enable_Callback(glfw.Windows.Callbacks.Size);
        thisWindow.Enable_Callback(glfw.Windows.Callbacks.Key);
        thisWindow.Enable_Callback(glfw.Windows.Callbacks.Mouse_Button);
        thisWindow.Enable_Callback(glfw.Windows.Callbacks.Mouse_Scroll);
    end Enable_Callbacks;

    --  -----------------------------------------------------------------------------------------------------------------------------

   function Initialize(Main_Window : in out Window_Types.tWindow;
                     Rendering_Program : out GL.Objects.Programs.Program;
                       Vertex_Array            : in out GL.Objects.Vertex_Arrays.Vertex_Array_Object)
                       return Boolean is
        -- sb7.init:
        Window_Width       : Interfaces.C.Int := 800;
        Window_Height      : Interfaces.C.Int := 600;
        Cursor                    : Glfw.Input.Mouse.Cursor_Mode := Glfw.Input.Mouse.Hidden;
        Can_Start               : Boolean := False;
    begin
        Set_Window_Hints;
        -- sb7 appears to be based on glfw version 2.
        Main_Window.Init(Window_Width, Window_Height, "OpenGL SuperBible Example");
        glfw.Windows.Context.Make_Current(Main_Window'Access);
        Enable_Callbacks(Main_Window);

        gl.Toggles.Enable(gl.Toggles.Depth_Test);          -- glEnable(GL_DEPTH_TEST)
        gl.Buffers.Set_Depth_Function(gl.Types.Less);    -- glDepthFunc(GL_LESS)
        Main_Window.Set_Cursor_Mode(Cursor);
        Utilities.Show_GL_Data;

        Can_Start := Startup(Main_Window, Rendering_Program, Vertex_Array);
        return Can_Start;

 exception
      when anError : Constraint_Error =>
         Put("Initialize returned constraint error: ");
         Put_Line(Exception_Information(anError));
        return Can_Start;

      when anError : GL.Errors.Invalid_Operation_Error =>
         Put_Line("Initialize returned an invalid operation error: ");
         Put_Line(Exception_Information(anError));
        return Can_Start;

      when anError :  others =>
         Put_Line("An exceptiom occurred in Initialize.");
         Put_Line(Exception_Information(anError));
        return Can_Start;

    end Initialize;

    --  ------------------------------------------------------------------------------------------------------------------------

   procedure Set_Window_Hints is
      Min_Major_Version    : Integer := 3;
      Minor_Version            : Integer := 2;
   begin
        glfw.Windows.Hints.Set_Minimum_OpenGL_Version(Min_Major_Version, Minor_Version);
        glfw.Windows.Hints.Set_Forward_Compat(True);
        glfw.Windows.Hints.Set_Profile(glfw.Windows.Context.Core_Profile);
        glfw.Windows.Hints.Set_Debug_Context(True);
        -- Set samples to 16 before taking screen shots.
        glfw.Windows.Hints.Set_Samples(4);

    end Set_Window_Hints;

    --  ------------------------------------------------------------------------------------------------------------------------

    function Startup(Main_Window : Window_Types.tWindow;
                     Rendering_Program   : out GL.Objects.Programs.Program;
                     Vertex_Array              : in out GL.Objects.Vertex_Arrays.Vertex_Array_Object)
                     return Boolean is

        OK  : Boolean := Shaders_Program.Make_Shader_Program(Main_Window, Rendering_Program);
    begin
        if OK then
           Vertex_Array.Initialize_Id;
           Vertex_Array.Bind;
            gl.Toggles.Enable(gl.Toggles.Vertex_Program_Point_Size);
        else
           Put_Line("Shader program linking failed.");
           Put_Line("Log:");
           Put_Line(Rendering_Program.Info_Log);
        end if;
        Utilities.Show_Shader_Program_Data(Rendering_Program);
        return OK;
    end Startup;

    --  -----------------------------------------------------------------------------------------------------------------------------


end Initialization;
