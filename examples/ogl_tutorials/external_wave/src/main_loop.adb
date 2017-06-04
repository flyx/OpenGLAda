
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Shaders;
with GL.Objects.Vertex_Arrays;
with GL.Rasterization;
with GL.Types;
with GL.Types.Colors;
with GL.Uniforms;
with GL.Window;

with Glfw;
with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Input.Mouse;
with Glfw.Windows.Context;

with Maths; use Maths;
with Program_Loader;
with Utilities;

with Control_Wave;
with Vertex_Data;

procedure Main_Loop (Main_Window :  in out Glfw.Windows.Window) is

    Vertex_Array    : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    Elements_Buffer : GL.Objects.Buffers.Buffer;
    Vertices_Buffer : GL.Objects.Buffers.Buffer;
    Render_Program  : GL.Objects.Programs.Program;
    MVP_Matrix_ID   : GL.Uniforms.Uniform;
    MVP_Matrix      : GL.Types.Singles.Matrix4 := GL.Types.Singles.Identity4;
    Pressure        : Vertex_Data.Grid_Array;
    Vel_X           : Vertex_Data.Grid_Array;
    Vel_Y           : Vertex_Data.Grid_Array;
    Last_Time       : GL.Types.Single;
    dt              : GL.Types.Single := 0.0;
    Running         : Boolean := True;

    --  ------------------------------------------------------------------------

    procedure Set_MVP_Matrix (Window         : in out Glfw.Windows.Window;
                              Render_Program : GL.Objects.Programs.Program);

    --  ------------------------------------------------------------------------

    procedure Render (Window : in out Glfw.Windows.Window) is
        use GL.Types;
        use GL.Objects.Buffers;

        Black         : GL.Types.Colors.Color := (0.0, 0.0, 0.0, 0.0);
        Window_Width  : Glfw.Size;
        Window_Height : Glfw.Size;
        Now           : Single := Single (Glfw.Time);
        dt_Total      : Single := Now - Last_Time;
    begin
        Window.Get_Framebuffer_Size (Window_Width, Window_Height);
        GL.Window.Set_Viewport (0, 0, Int (Window_Width),
                                GL.Types.Int (Window_Height));
        Utilities.Clear_Background_Colour_And_Depth (Black);
        Last_Time := Now;

        while dt_Total > 0.0 loop
            if dt_Total > Vertex_Data.Max_dt then
                dt := Vertex_Data.Max_dt;
            else
                dt := dt_Total;
            end if;
            dt_Total := dt_Total - dt;
            Vertex_Data.Calculate_Grid (Pressure, Vel_X, Vel_Y, dt);
        end loop;

        GL.Objects.Programs.Use_Program (Render_Program);

        Control_Wave.Check_Input (Window);
        Set_MVP_Matrix (Window, Render_Program);
        GL.Uniforms.Set_Single (MVP_Matrix_ID, MVP_Matrix);

        Vertices_Buffer.Initialize_Id;
        Array_Buffer.Bind (Vertices_Buffer);
        Utilities.Load_Vector6_Buffer (Array_Buffer,
                                       Vertex_Data.Vertex_Buffer_Data, Static_Draw);

        Elements_Buffer.Initialize_Id;
        Element_Array_Buffer.Bind (Elements_Buffer);
        Vertex_Data.Load_Element_Buffer (Element_Array_Buffer,
                                         Vertex_Data.Quad_Element_Array, Static_Draw);

        GL.Attributes.Enable_Vertex_Attrib_Array (0);
        GL.Attributes.Set_Vertex_Attrib_Pointer (Index  => 0, Count  => 3,
                                                 Kind   => Single_Type,
                                                 Stride => 0, Offset => 0);

        GL.Attributes.Enable_Vertex_Attrib_Array (1);
        GL.Attributes.Set_Vertex_Attrib_Pointer (1, 2, Single_Type, 3, 0);

        Put_Line ("OK 3 in Render.");
        Draw_Elements (GL.Types.Quads, GL.Types.Size (4 * Vertex_Data.Num_Quads), UInt_Type);
        Put_Line ("OK 4 in Render.");

        GL.Attributes.Disable_Vertex_Attrib_Array (0);
        GL.Attributes.Disable_Vertex_Attrib_Array (1);

    exception
        when  others =>
            Put_Line ("An exceptiom occurred in Render.");
            raise;
    end Render;

    --  ------------------------------------------------------------------------

    procedure Run (Window : in out Glfw.Windows.Window) is
        use Glfw.Input;
    begin
        while Running loop
            Render (Main_Window);
            Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
            Glfw.Input.Poll_Events;
            Running := Running and not
              (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
            Running := Running and not Main_Window.Should_Close;
        end loop;
    end Run;

    --  ------------------------------------------------------------------------

    procedure Set_MVP_Matrix (Window         : in out Glfw.Windows.Window;
                              Render_Program : GL.Objects.Programs.Program) is
        use GL.Types;
        use GL.Types.Singles;
        use Maths;

        Alpha             : Degree;
        Beta              : Degree;
        Zoom              : Single;
    begin
        MVP_Matrix_ID := GL.Objects.Programs.Uniform_Location
          (Render_Program, "MVP_Matrix");
        Control_Wave.Get_Settings (Alpha, Beta, Zoom);
--          MVP_Matrix :=  Maths.Translation_Matrix ((0.0, 0.0, -Zoom)) * Singles.Identity4;
--          MVP_Matrix :=  Maths.Rotation_Matrix (Beta, (1.0, 0.0, 0.0)) * MVP_Matrix;
--          MVP_Matrix :=  Maths.Rotation_Matrix (Alpha, (0.0, 0.0, 1.0)) * MVP_Matrix;

    exception
        when others =>
            Put_Line ("An exception occurred in Set_MVP_Matrix.");
            raise;
    end Set_MVP_Matrix;

    --  ------------------------------------------------------------------------

    procedure Setup (Window : in out Glfw.Windows.Window) is
        use GL.Objects.Buffers;
        use GL.Objects.Shaders;
        use GL.Types;
        use Glfw.Input;
        use Program_Loader;

        Window_Width  : Glfw.Size;
        Window_Height : Glfw.Size;
    begin
        Window.Set_Input_Toggle (Sticky_Keys, True);
        Window.Set_Cursor_Mode (Mouse.Disabled);
        Glfw.Windows.Context.Set_Swap_Interval (1);
        Glfw.Input.Poll_Events;

        Window'Access.Get_Size (Window_Width, Window_Height);
        Window'Access.Set_Cursor_Pos (Mouse.Coordinate (0.5 * Single (Window_Width)),
                                      Mouse.Coordinate (0.5 * Single (Window_Height)));
        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;

        GL.Rasterization.Set_Point_Size (2.0);

        Control_Wave.Get_Data (Pressure, Vel_X, Vel_Y);
        Vertex_Data.Initialize_Vertices;
        Vertex_Data.Initialize_Grid (Pressure, Vel_X, Vel_Y);
        Vertex_Data.Adjust_Grid (Pressure);
        Last_Time := Single (Glfw.Time) - 0.01;

        Render_Program := Program_From
          ((Src ("src/shaders/simple_vertex_shader.glsl", Vertex_Shader),
           Src ("src/shaders/simple_fragment_shader.glsl", Fragment_Shader)));

        Utilities.Enable_Mouse_Callbacks (Window, True);
        Window.Enable_Callback (Glfw.Windows.Callbacks.Char);
        Window.Enable_Callback (Glfw.Windows.Callbacks.Position);
        Window.Enable_Callback (Glfw.Windows.Callbacks.Mouse_Scroll);
        Window.Enable_Callback (Glfw.Windows.Callbacks.Framebuffer_Size);

    exception
        when others =>
            Put_Line ("An exceptiom occurred in Setup.");
            raise;
    end Setup;

    --  ------------------------------------------------------------------------

begin
    Setup (Main_Window);
    Run (Main_Window);

    Vertex_Array.Delete_Id;
    Elements_Buffer.Delete_Id;
    Render_Program.Delete_Id;

exception
    when anError :  others =>
        Put_Line ("An exceptiom occurred in Main_Loop.");
        raise;
end Main_Loop;
