
--  Based on libgasandbox.draw.h and draw.cpp

with Ada.Containers.Vectors;
with Ada.Numerics;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL;
with GL.Attributes;
with GL.Culling;
with GL.Immediate;
with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Toggles;
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
with GL_Util;

package body GA_Draw is

    package Draw_State_Package is new Ada.Containers.Vectors (positive, Draw_Mode);
    type Draw_State_Vector is new Draw_State_Package.Vector with null record;

    Palet                : Colour_Palet;
    G_Draw_State         : Draw_State;
    M_Draw_Mode          : Draw_State_Vector;
    --  M_Sphere         : Geosphere.Geosphere_S;
    --  M_Sphere_GL_List : GL.Types.UInt;
    Vertex_Array_Object  : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
    MV_Matrix_ID         : GL.Uniforms.Uniform;
    Model_View_Matrix    : GL.Types.Singles.Matrix4;
    Projection_Matrix_ID : GL.Uniforms.Uniform;
    Projection_Matrix    : GL.Types.Singles.Matrix4;
    Colour_Location      : GL.Uniforms.Uniform;

    --  ------------------------------------------------------------------------

    --  Draw_Bivector corresponds to draw.draw_Bivector of draw.cpp
    --  The parameter names correspond of those in draw.h!
    procedure Draw_Bivector (Render_Program                       : GL.Objects.Programs.Program;
                             Model_View_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4;
                             Base, Factor_1, Factor_2             : GA_Maths.Vector;
                             Scale                                : GL.Types.Single;
                             Method                               : Bivector_Method_Type;
                             Colour                               : Color := (1.0, 1.0, 1.0, 1.0)) is
        use GA_Maths;
        use GL.Types.Singles;
        --  L          : boolean;
        Rotor_Step : float := 2.0 * Ada.Numerics.Pi / 64.0;
        --  X          : GL.Types.Single;
        --  Y          : GL.Types.Single;
        Cords      : Array_3D := GA_Maths.Get_Coords (Base);
        Translate  : Vector3 := (Single (Get_Coord_1 (Base)),
                                 Single (Get_Coord_2 (Base)), Single (Get_Coord_3 (Base)));
        MVP_Matrix : Matrix4 := Singles.Identity4;
        Rotor      : E2GA.Rotor;
    begin
        GL.Objects.Programs.Use_Program (Render_Program);
        Vertex_Array_Object.Initialize_Id;
        Vertex_Array_Object.Bind;

        MVP_Matrix := GL.Types.Singles.Identity4 * GL.Types.Singles.Identity4;
        if E3GA.Norm_E2 (Base) /= 0.0  then
            MVP_Matrix := Maths.Translation_Matrix (Translate) * GL.Types.Singles.Identity4;
        end if;
        if  Method /= Draw_Bivector_Parallelogram and then
          Method /= Draw_Bivector_Parallelogram_No_Vectors then
            MVP_Matrix := Maths.Scaling_Matrix ((Scale, Scale, Scale)) * MVP_Matrix;

        end if;
    exception
        when anError :  others =>
            Put_Line ("An exception occurred in Draw_Object.Draw_Bivector.");
            raise;
    end Draw_Bivector;

    --  ----------------------------------------------------------------------

    procedure Draw_Bivector (Render_Program                       : GL.Objects.Programs.Program;
                             Model_View_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4;
                             BV                                   : E2GA.Bivector;
                             Colour                               : GL.Types.Colors.Color := (1.0, 1.0, 1.0, 1.0);
                             Scale                                : GL.Types.Single) is
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
        Vertex_Buffer : Buffer;
    begin
        GL.Objects.Programs.Use_Program (Render_Program);
        Vertex_Array_Object.Initialize_Id;
        Vertex_Array_Object.Bind;
        Vertex_Buffer.Initialize_Id;
        Array_Buffer.Bind (Vertex_Buffer);
        --              Utilities.Load_To_Buffer (Array_Buffer, Vertices, Static_Draw);
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
        --              GL.Attributes.Disable_Vertex_Attrib_Array (0)

    exception
        when anError :  others =>
            Put_Line ("An exception occurred in Draw_Object.Draw_Bivector.");
            raise;
    end Draw_Bivector;

    --  ----------------------------------------------------------------------

    --      procedure Draw_Multivector (Render_Program : GL.Objects.Programs.Program;
    --               Model_View_Matrix, Projection_Matrix : GL.Types.Singles.Matrix4;
    --               MV             : E2GA.Multivector;
    --               Colour         : GL.Types.Colors.Color := (1.0, 1.0, 1.0, 1.0);
    --               Scale          : GL.Types.Single) is
    --      --   L          : boolean;
    --          Rotor_Step : float := 2.0 * Ada.Numerics.Pi / 64.0;
    --          --   X          : float;
    --          --   Y          : float;
    --          MVP_Matrix : Singles.Matrix4 := Singles.Identity4;
    --          Rotor      : E2GA.Rotor;
    --
    --      begin
--
--      GL.Objects.Programs.Use_Program (Render_Program);
--      Vertex_Array_Object.Initialize_Id;
--      Vertex_Array_Object.Bind;
    --          --          if E3GA.Norm_E2 (MV).M_C1 = 0.0  then
    --          --              Maths.Translation_Matrix (Get_Coord_1 (Base),
    --          --                Get_Coord_2 (Base), Get_Coord_3 (Base)) * MVP_Matrix;
    --          --          end if;
    --      exception
    --          when anError :  others =>
    --              Put_Line ("An exception occurred in Draw_Object.Draw_Multivector.");
    --              raise;
    --      end Draw_Multivector;

    --  ----------------------------------------------------------------------

    procedure Draw_Base (Render_Program    : GL.Objects.Programs.Program;
                         Scale             : Gl.Types.Single) is

        use GL.Objects.Buffers;
        use GL.Types.Singles;
        use GA_Maths.Float_Functions;

        GL_e1           : Vector3 := GL_Util.To_GL (E3GA.e1);
        GL_e2           : Vector3 := GL_Util.To_GL (E3GA.e2);
        GL_e3           : Vector3 := GL_Util.To_GL (E3GA.e3);
        Z               : float := 0.0;
        Num_Steps       : constant int := 32;
        Rotor_Step      : constant float := 2.0 * Ada.Numerics.Pi / float (Num_Steps);
        Vertex_Buffer   : GL.Objects.Buffers.Buffer;
        Fan             : Singles.Vector3_Array (1 .. Num_Steps + 1);
    begin
        Vertex_Buffer.Initialize_Id;
        Array_Buffer.Bind (Vertex_Buffer);
        Fan (1) := (0.0, 0.0, -0.25);
        for Count in 2 .. Num_Steps + 1 loop
            Fan (Count) := (Single (0.1 * Cos (Z)), Single (0.1 * Sin (Z)), -0.25);
            Z := Z + Rotor_Step;
        end loop;

        Utilities.Load_Vertex_Buffer (Array_Buffer, Fan, Static_Draw);

        GL.Uniforms.Set_Single (MV_Matrix_ID, Model_View_Matrix);
        GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

        GL.Attributes.Enable_Vertex_Attrib_Array (0);
        GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);

        GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => Triangle_Fan,
                                              First => 0,
                                              Count => Num_Steps);
        GL.Attributes.Disable_Vertex_Attrib_Array (0);

    exception
        when anError :  others =>
            Put_Line ("An exception occurred in GA_Draw.Draw_Base.");
            raise;
    end Draw_Base;

    --  ------------------------------------------------------------------------

    procedure Draw_Cone (Render_Program    : GL.Objects.Programs.Program;
                         Scale             : Gl.Types.Single) is

        use GL.Objects.Buffers;
        use GL.Types.Singles;
        use GA_Maths.Float_Functions;

        GL_e1           : Vector3 := GL_Util.To_GL (E3GA.e1);
        GL_e2           : Vector3 := GL_Util.To_GL (E3GA.e2);
        GL_e3           : Vector3 := GL_Util.To_GL (E3GA.e3);
        Z               : float := 0.0;
        Num_Steps       : constant int := 256;
        Rotor_Step      : constant float := 2.0 * Ada.Numerics.Pi / float (Num_Steps);
        Vertex_Buffer   : GL.Objects.Buffers.Buffer;
        Fan             : Singles.Vector3_Array (1 .. Num_Steps);
    begin
        Vertex_Buffer.Initialize_Id;
        Array_Buffer.Bind (Vertex_Buffer);
        Fan (1) := (0.0, 0.0, 0.0);
        for Count in 2 .. Num_Steps loop
            Fan (Count) := (Single (0.1 * Cos (Z)), Single (0.1 * Sin (Z)), -0.25);
            Z := Z + Rotor_Step;
        end loop;

        Utilities.Load_Vertex_Buffer (Array_Buffer, Fan, Static_Draw);

        GL.Uniforms.Set_Single (MV_Matrix_ID, Model_View_Matrix);
        GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

        GL.Attributes.Enable_Vertex_Attrib_Array (0);
        GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);

        GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => Triangle_Fan,
                                              First => 0,
                                              Count => Num_Steps);
        GL.Attributes.Disable_Vertex_Attrib_Array (0);

    exception
        when anError :  others =>
            Put_Line ("An exception occurred in GA_Draw.Draw_Cone.");
            raise;
    end Draw_Cone;

    --  ------------------------------------------------------------------------

    procedure Draw_Line (Render_Program    : GL.Objects.Programs.Program;
                         Tail, Direction   : GA_Maths.Vector;
                         Colour            : GL.Types.Colors.Color;
                         Scale             : Gl.Types.Single) is

        use GL.Objects.Buffers;
        use GL.Toggles;
        use GL.Types.Colors;
        use GL.Types.Singles;

        GL_Dir                 : Vector3 := GL_Util.To_GL (Direction);
        GL_e1                  : Vector3 := GL_Util.To_GL (E3GA.e1);
        GL_e2                  : Vector3 := GL_Util.To_GL (E3GA.e2);
        GL_e3                  : Vector3 := GL_Util.To_GL (E3GA.e3);
        Dir_e1                 : Single := Single (Dot_Product (GL_Dir, GL_e1));
        Dir_e2                 : Single := Single (Dot_Product (GL_Dir, GL_e2));
        Dir_e3                 : Single := Single (Dot_Product (GL_Dir, GL_e3));
        Vertex_Buffer          : GL.Objects.Buffers.Buffer;
        Vertices               : Singles.Vector3_Array (1 .. 2);
    begin

        Vertex_Buffer.Initialize_Id;
        Array_Buffer.Bind (Vertex_Buffer);
        Utilities.Load_Vertex_Buffer (Array_Buffer, Vertices, Static_Draw);

        GL.Uniforms.Set_Single (Colour_Location, Colour (R), Colour (G), Colour (B));
        GL.Uniforms.Set_Single (MV_Matrix_ID, Model_View_Matrix);
        GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

        GL.Attributes.Set_Vertex_Attrib_Pointer (0, 3, Single_Type, 0, 0);
        GL.Attributes.Enable_Vertex_Attrib_Array (0);

        GL.Objects.Vertex_Arrays.Draw_Arrays (Mode  => Lines,
                                              First => 0,
                                              Count => 1 * 3);
        GL.Attributes.Disable_Vertex_Attrib_Array (0);

    exception
        when anError :  others =>
            Put_Line ("An exception occurred in GA_Draw.Draw_Line.");
            raise;
    end Draw_Line;

    --  ------------------------------------------------------------------------

    procedure Draw_Vector (Render_Program          : GL.Objects.Programs.Program;
                           MV_Matrix, Proj_Matrix  : GL.Types.Singles.Matrix4;
                           Tail, Direction         : GA_Maths.Vector;
                           Colour                  : GL.Types.Colors.Color;
                           Scale                   : Gl.Types.Single) is
        use GL.Culling;
        use GL.Objects.Buffers;
        use GL.Toggles;
        use GL.Types.Colors;
        use GL.Types.Singles;
        use GA_Maths;
        use GA_Maths.Float_Functions;

        GL_Tail         : Vector3 := GL_Util.To_GL (Tail);
        GL_Dir          : Vector3 := GL_Util.To_GL (Direction);

        Z               : Single := 0.0;
        Dir_e1          : Single := Single (Dot_Product (Direction, E3GA.e1));
        Dir_e2          : Single := Single (Dot_Product (Direction, E3GA.e2));
        Dir_e3          : Single := Single (Dot_Product (Direction, E3GA.e3));
        Tail_e1         : Single := Single (Dot_Product (Tail, E3GA.e1));
        Tail_e2         : Single := Single (Dot_Product (Tail, E3GA.e2));
        Tail_e3         : Single := Single (Dot_Product (Tail, E3GA.e3));
        Scale_Factor1   : Single := Single (1.2 / Scale);
        Scale_Factor2   : Single := 1.1 * Single (Sqrt (float (Scale)));
        Scale_Factor1_V : Singles.Vector3 := (Scale_Factor1, Scale_Factor1, Scale_Factor1);
        Scale_Factor2_V : Singles.Vector3 := (Scale_Factor2, Scale_Factor2, Scale_Factor2);
        aRotor          : E3GA.Rotor;
        Saved_Cull_Face : Face_Selector := Cull_Face;
    begin
        if Scale /= 0.0 then

            GL.Objects.Programs.Use_Program (Render_Program);
            Vertex_Array_Object.Initialize_Id;
            Vertex_Array_Object.Bind;

            MV_Matrix_ID := GL.Objects.Programs.Uniform_Location
              (Render_Program, "MV_Matrix");
            Projection_Matrix_ID := GL.Objects.Programs.Uniform_Location
              (Render_Program, "Proj_Matrix");
            GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

            Colour_Location := GL.Objects.Programs.Uniform_Location
              (Render_Program, "vector_colour");
            Model_View_Matrix := MV_Matrix;
            Projection_Matrix := Proj_Matrix;

            if E3GA.Norm_e2 (Tail) /= 0.0 then
                Model_View_Matrix := Maths.Translation_Matrix
                  ((Tail_e1, Tail_e2, Tail_e3)) * Model_View_Matrix;
            end if;
            Draw_Line (Render_Program, Tail, Direction, Colour, Scale);

            --  rotate e3 to vector direction
            Model_View_Matrix := GL.Types.Singles.Identity4;
            aRotor := E3GA_Utilities.Rotor_Vector_To_Vector (E3GA.e3, E3GA.Unit_e (Direction));
            GL_Util.Rotor_GL_Multiply (aRotor, Model_View_Matrix);

            Model_View_Matrix := MV_Matrix * Model_View_Matrix;

            --  Translate to head of vector
            if E3GA.Norm_e2 (Tail) /= 0.0 then
                Model_View_Matrix := Maths.Translation_Matrix
                  ((Tail_e1, Tail_e2, Tail_e3)) * Model_View_Matrix;
            end if;
            Model_View_Matrix := Maths.Translation_Matrix (Scale * GL_Dir) * Model_View_Matrix;
            Enable (Cull_Face);
            Set_Front_Face (GL.Types.Clockwise);
            Set_Cull_Face (Front);

            Draw_Cone (Render_Program, Scale);
            Draw_Base (Render_Program, Scale);
            Set_Cull_Face (Saved_Cull_Face);
        end if;
    exception
        when anError :  others =>
            Put_Line ("An exception occurred in GA_Draw.Draw_Vector.");
            raise;
    end Draw_Vector;

    --  -----------------------------------------------------------------------

    function Get_Draw_Mode return Draw_Mode is
    begin
        return M_Draw_Mode.Last_Element;
    end Get_Draw_Mode;

    --  ------------------------------------------------------------------------

    procedure Set_Background_Colour (Back_Colour : Color) is
    begin
        Palet.Background_Colour := Back_Colour;
    end Set_Background_Colour;

    --  ------------------------------------------------------------------------

    procedure Set_Draw_Mode (Mode : Draw_Mode) is
    begin
        M_Draw_Mode.Append (Mode);
    end Set_Draw_Mode;

    --  ------------------------------------------------------------------------

    procedure Set_Foreground_Colour (Fore_Colour : Color) is
    begin
        Palet.Foreground_Colour := Fore_Colour;
    end Set_Foreground_Colour;

    --  ------------------------------------------------------------------------

    procedure Set_MVP_Matrix (Window         : in out Glfw.Windows.Window;
                              Render_Program : GL.Objects.Programs.Program;
                              Direction      : E2GA.Rotor;
                              Scale          : GL.Types.Single := 1.0) is
        use GL.Types;
        use GL.Types.Singles;
        use Maths;
        use GA_Maths.Float_Functions;

        MVP_Matrix_ID     : GL.Uniforms.Uniform;
        MVP_Matrix        : GL.Types.Singles.Matrix4 := Singles.Identity4;
        Rotor             : E2GA.Rotor;
        Shift             : Singles.Vector3 :=
                              (Single (Direction.M_C1), Single (Direction.M_C2), 0.0);
        Scale_Factor1     : Single := Single (1.2 / Scale);
        Scale_Factor2     : Single := 1.1 * Single (Sqrt (float (Scale)));
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

    procedure Set_Ol_Colour (Ol_Colour : Color) is
    begin
        Palet.Ol_Colour := Ol_Colour;
    end Set_Ol_Colour;

    --  ------------------------------------------------------------------------

    procedure Set_Point_Size (Point_Size : GL.Types.Single) is
    begin
        G_Draw_State.Point_Size := Point_Size;
    end Set_Point_Size;

    --  ------------------------------------------------------------------------

end GA_Draw;
