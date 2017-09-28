
--  Based on libgasandbox.draw.h and draw.cpp

with Ada.Containers.Vectors;
with Ada.Numerics;
with Ada.Exceptions; use Ada.Exceptions;

with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Types; use GL.Types;
with GL.Types.Colors;
with GL.Uniforms;

with Glfw.Windows;

with Maths;
with GA_Maths;
with Utilities;

with E3GA;
with E3GA_Utilities;
with Geosphere;

package body GA_Draw is

    package Draw_State_Package is new Ada.Containers.Vectors (positive, Draw_Mode);
    type Draw_State_Vector is new Draw_State_Package.Vector with null record;

    Palet            : Colour_Palet;
    G_Draw_State     : Draw_State;
    M_Draw_Mode      : Draw_State_Vector;
    --  M_Sphere         : Geosphere.Geosphere_S;
    --  M_Sphere_GL_List : GL.Types.UInt;

    procedure Example_MVP_Matrix (Window     : in out Glfw.Windows.Window;
                              Render_Program : GL.Objects.Programs.Program;
                              Scale          : float := 1.0) is
        use GL.Types;
        use GL.Types.Singles;
        use Maths;

        MVP_Matrix_ID     : GL.Uniforms.Uniform;
        --  Camera position, Look_At and Up are world coordinates.
        Camera_Position   : Vector3 := (4.0, 3.0, -3.0);
        Look_At           : Vector3 := (0.0, 0.0, 0.0);
        Up                : Vector3 := (0.0, 1.0, 0.0);
        --  The Model_Matrix operates in world coordinates.
        Model_Matrix      : Matrix4 := Singles.Identity4;
        MVP_Matrix        : Matrix4 := Singles.Identity4;
        --  The Projection_Matrix projects the camera view in camera coordinates
        --  onto the camera view's Near plane
        Projection_Matrix : Matrix4;
        --  The View_Matrix transforms the world_cordinates of the world view
        --  into view (camera) coordinates.
        View_Matrix       : Matrix4;
        Window_Width      : Glfw.Size;
        Window_Height     : Glfw.Size;
    begin
        Window.Get_Framebuffer_Size (Window_Width, Window_Height);
        MVP_Matrix_ID := GL.Objects.Programs.Uniform_Location
          (Render_Program, "MVP_5");

        Init_Lookat_Transform (Camera_Position, Look_At, Up, View_Matrix);
        Init_Perspective_Transform (45.0, Single (Window_Width),
                                          Single (Window_Height),
                                    0.1, 100.0, Projection_Matrix);
       MVP_Matrix :=  Projection_Matrix * View_Matrix * Model_Matrix;
    end Example_MVP_Matrix;

    --  ------------------------------------------------------------------------
    --  Draw_Bivector corresponds to draw.draw_Bivector of draw.cpp
    --  The parameter names correspond of those in draw.h!
    procedure Draw_Bivector (Render_Program : GL.Objects.Programs.Program;
                             Base, Factor_1, Factor_2 : GA_Maths.Vector;
                             Scale  : float := 1.0;
                             Method : Bivector_Method_Type;
                             Colour : Color := (1.0, 1.0, 1.0, 1.0)) is
        use GA_Maths;
        use GL.Types.Singles;
        GL_Scale   : Single := Single (Scale);
      --  L          : boolean;
        Rotor_Step : float := 2.0 * Ada.Numerics.Pi / 64.0;
      --  X          : float;
      --  Y          : float;
        Cords      : Array_3D := GA_Maths.Get_Coords (Base);
        Translate  : Vector3 := (Single (Get_Coord_1 (Base)),
                      Single (Get_Coord_2 (Base)), Single (Get_Coord_3 (Base)));
        MVP_Matrix : Matrix4 := Singles.Identity4;
        Rotor      : E2GA.Rotor;
    begin
            MVP_Matrix := GL.Types.Singles.Identity4 * GL.Types.Singles.Identity4;
        if E3GA.Norm_E2 (Base) /= 0.0  then
            MVP_Matrix := Maths.Translation_Matrix (Translate) * GL.Types.Singles.Identity4;
        end if;
        if  Method /= Draw_Bivector_Parallelogram and then
              Method /= Draw_Bivector_Parallelogram_No_Vectors then
            MVP_Matrix := Maths.Scaling_Matrix ((GL_Scale, GL_Scale, GL_Scale)) * MVP_Matrix;

        end if;
    exception
        when anError :  others =>
            Put_Line ("An exception occurred in Draw_Object.Draw_Bivector.");
            raise;
    end Draw_Bivector;

    --  ----------------------------------------------------------------------

    procedure Draw_Bivector (Render_Program : GL.Objects.Programs.Program;
                         BV : E2GA.Bivector;
                         Colour : GL.Types.Colors.Color := (1.0, 1.0, 1.0, 1.0);
                         Scale : float := 1.0) is
        use GL.Objects.Buffers;
        use GL.Types.Colors;
--          Vertices : GL.Types.Singles.Vector2_Array (1 .. 4) :=
--                            ((0.0, 0.0),
--                            (Single (V2.Coord_1), Single (V2.Coord_2)),
--                            (Single (V3.Coord_1), Single (V3.Coord_2)),
--                            (Single (V4.Coord_1), Single (V4.Coord_2)));
--          Z               : Single;
--          Rotor           : E2GA.Rotor;
--          Roti            : E2GA.Rotor;
--          Colour_Location : GL.Uniforms.Uniform;
        Vertex_Buffer : Buffer;
    begin
            Vertex_Buffer.Initialize_Id;
            Array_Buffer.Bind (Vertex_Buffer);
--              Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices, Static_Draw);
--              GL.Attributes.Enable_Vertex_Attrib_Array (0);
--              GL.Attributes.Set_Vertex_Attrib_Pointer (0, 2, Single_Type, 0, 0);
--
--              Colour_Location := GL.Objects.Programs.Uniform_Location
--                (Render_Program, "vector_colour");
--              GL.Uniforms.Set_Single (Colour_Location, Colour (R), Colour (G), Colour (B));
--
--              GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => Lines,
--                                                    First => 0,
--                                                    Count => 4 * 2);
--              GL.Attributes.Disable_Vertex_Attrib_Array (0);

    exception
        when anError :  others =>
            Put_Line ("An exception occurred in Draw_Object.Draw_Bivector.");
            raise;
    end Draw_Bivector;

    --  ----------------------------------------------------------------------

    procedure Draw_Multivector (Render_Program : GL.Objects.Programs.Program;
                         MV : E2GA.Multivector;
                         Colour : GL.Types.Colors.Color := (1.0, 1.0, 1.0, 1.0);
                         Scale : float := 1.0) is
     --   L          : boolean;
        Rotor_Step : float := 2.0 * Ada.Numerics.Pi / 64.0;
     --   X          : float;
     --   Y          : float;
        MVP_Matrix : Singles.Matrix4 := Singles.Identity4;
        Rotor      : E2GA.Rotor;

    begin
        null;
--          if E3GA.Norm_E2 (MV).M_C1 = 0.0  then
--              Maths.Translation_Matrix (Get_Coord_1 (Base), Get_Coord_2 (Base), Get_Coord_3 (Base)) * MVP_Matrix;
--          end if;
    exception
        when anError :  others =>
            Put_Line ("An exception occurred in Draw_Object.Draw_Multivector.");
            raise;
    end Draw_Multivector;

    --  ----------------------------------------------------------------------

    procedure Draw_Quad (Render_Program : GL.Objects.Programs.Program;
                         V2, V3, V4 : GA_Maths.Vector_2D;
                         Colour : GL.Types.Colors.Color := (1.0, 1.0, 1.0, 1.0);
                         Scale : float := 1.0) is
        use GL.Objects.Buffers;
        use GL.Types.Colors;
        use GA_Maths;

        Vertices : GL.Types.Singles.Vector2_Array (1 .. 6) :=
                          ((0.0, 0.0),
                          (Single (Get_Coord_1 (V2)), Single (Get_Coord_2 (V2))),
                          (Single (Get_Coord_1 (V4)), Single (Get_Coord_2 (V4))),
                          (Single (Get_Coord_1 (V2)), Single (Get_Coord_2 (V2))),
                          (Single (Get_Coord_1 (V3)), Single (Get_Coord_2 (V3))),
                          (Single (Get_Coord_1 (V4)), Single (Get_Coord_2 (V4))));
    --    Z               : Single;
        Rotor           : E2GA.Rotor;
        Roti            : E2GA.Rotor;
        Colour_Location : GL.Uniforms.Uniform;
        Vertex_Buffer : Buffer;
    begin
            Vertex_Buffer.Initialize_Id;
            Array_Buffer.Bind (Vertex_Buffer);
            Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices, Static_Draw);
            GL.Attributes.Enable_Vertex_Attrib_Array (0);
            GL.Attributes.Set_Vertex_Attrib_Pointer (0, 2, Single_Type, 0, 0);

            Colour_Location := GL.Objects.Programs.Uniform_Location
              (Render_Program, "vector_colour");
            GL.Uniforms.Set_Single (Colour_Location, Colour (R), Colour (G), Colour (B));

            GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => Triangles,
                                                  First => 0,
                                                  Count => 6 * 2);
            GL.Attributes.Disable_Vertex_Attrib_Array (0);

    exception
        when anError :  others =>
            Put_Line ("An exception occurred in Draw_Object.Draw_Quad.");
            raise;
    end Draw_Quad;

    --  ------------------------------------------------------------------------

    procedure Draw_Vector (Render_Program : GL.Objects.Programs.Program;
                           Tail, Direction : GA_Maths.Vector_2D;
                           Colour : GL.Types.Colors.Color := (1.0, 1.0, 1.0, 1.0);
                           Scale : float := 1.0) is
        use GL.Objects.Buffers;
        use GL.Types.Colors;
        use GA_Maths;
        GL_Tail       : Singles.Vector2 := (Single (Get_Coord_1 (Tail)),
                                            Single (Get_Coord_2 (Tail)));
        GL_Dir        : Singles.Vector2  := (Single (Get_Coord_1 (Direction)),
                                             Single (Get_Coord_2 (Direction)));
--          Z             : Single;
        Rotor         : E2GA.Rotor;
        Roti          : E2GA.Rotor;
        Colour_Location : GL.Uniforms.Uniform;
        Vertices      : Singles.Vector2_Array (1 .. 2);
        Vertex_Buffer : Buffer;
    begin
        if Scale /= 0.0 then
            Vertices := ((0.0, 0.0),
                         (0.98 * GL_Dir (GL.X), 0.98 * GL_Dir (GL.Y)));

            Vertex_Buffer.Initialize_Id;
            Array_Buffer.Bind (Vertex_Buffer);
            Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices, Static_Draw);
            GL.Attributes.Enable_Vertex_Attrib_Array (0);
            GL.Attributes.Set_Vertex_Attrib_Pointer (0, 2, Single_Type, 0, 0);

            Colour_Location := GL.Objects.Programs.Uniform_Location
              (Render_Program, "vector_colour");
            GL.Uniforms.Set_Single (Colour_Location, Colour (R), Colour (G), Colour (B));

            GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => Lines,
                                                  First => 0,
                                                  Count => 2 * 2);
            GL.Attributes.Disable_Vertex_Attrib_Array (0);
        end if;
    exception
        when anError :  others =>
            Put_Line ("An exception occurred in Draw_Object.Draw_Vector.");
            raise;
    end Draw_Vector;

    --  ------------------------------------------------------------------------

    function Get_Draw_Mode return Draw_Mode is
    begin
        return M_Draw_Mode.Last_Element;
    end Get_Draw_Mode;

    --  ------------------------------------------------------------------------

    procedure Set_Foreground_Colour (Fore_Colour : Color) is
    begin
        Palet.Foreground_Colour := Fore_Colour;
    end Set_Foreground_Colour;

    --  ------------------------------------------------------------------------

    procedure Set_Background_Colour (Back_Colour : Color) is
    begin
        Palet.Background_Colour := Back_Colour;
    end Set_Background_Colour;

    --  ------------------------------------------------------------------------

    procedure Set_MVP_Matrix (Window         : in out Glfw.Windows.Window;
                              Render_Program : GL.Objects.Programs.Program;
                              Direction      : E2GA.Rotor;
                              Scale          : float := 1.0) is
        use GL.Types;
        use GL.Types.Singles;
        use Maths;
        use GA_Maths.Float_Functions;

        MVP_Matrix_ID     : GL.Uniforms.Uniform;
        MVP_Matrix        : GL.Types.Singles.Matrix4 := Singles.Identity4;
        GL_Scale          : Single := Single (Scale);
        Rotor             : E2GA.Rotor;
        Shift             : Singles.Vector3 :=
                              (Single (Direction.M_C1), Single (Direction.M_C2), 0.0);
        Scale_Factor1     : Single := Single (1.2 / Scale);
        Scale_Factor2     : Single := 1.1 * Single (Sqrt (Scale));
        Scale_Factor      : Singles.Vector3 := (Scale_Factor1, Scale_Factor1, Scale_Factor1);

        Window_Width      : Glfw.Size;
        Window_Height     : Glfw.Size;
    begin
        Window.Get_Framebuffer_Size (Window_Width, Window_Height);
        MVP_Matrix_ID := GL.Objects.Programs.Uniform_Location
          (Render_Program, "MVP_Matrix");
        --  Translate to head of vector
        MVP_Matrix := Translation_Matrix (Shift) * MVP_Matrix;
        if Scale > 1.2 then
--              Rotor := E3GA_Utilities.Rotor_Vector_To_Vector (E3GA.e3, E3GA.Unit_E (Direction));
            MVP_Matrix := Scaling_Matrix (Scale_Factor) * MVP_Matrix;
            Scale_Factor := (Scale_Factor2, Scale_Factor2, Scale_Factor2);
            MVP_Matrix := Scaling_Matrix (Scale_Factor) * MVP_Matrix;
      --      MVP_Matrix := Rotation_Matrix (Angle : Degree; Axis : Singles.Vector3) * MVP_Matrix;
        end if;
exception
        when others =>
            Put_Line ("An exception occurred in Set_MVP_Matrix.");
            raise;
    end Set_MVP_Matrix;

    --  ------------------------------------------------------------------------

    procedure Set_Draw_Mode (Mode : Draw_Mode) is
    begin
        M_Draw_Mode.Append (Mode);
    end Set_Draw_Mode;

    --  ------------------------------------------------------------------------

    procedure Set_Ol_Colour (Ol_Colour : Color) is
    begin
        Palet.Ol_Colour := Ol_Colour;
    end Set_Ol_Colour;

    --  ------------------------------------------------------------------------

    procedure Set_Point_Size (Point_Size : float) is
    begin
        G_Draw_State.Point_Size := Point_Size;
    end Set_Point_Size;

    --  ------------------------------------------------------------------------

end GA_Draw;
