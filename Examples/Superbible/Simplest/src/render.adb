
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO; use  Ada.Text_IO;

with GL.Algebra;
with GL.Buffers;
with GL.Errors;
with GL.Objects.Vertex_Arrays;
with GL.Types; use GL.Types;
with GL.Types.Colors;

package body Render is
    package Math_Functions is new Ada.Numerics.Generic_Elementary_Functions (Single);

    procedure Render(Program : GL.Objects.Programs.Program; Current_Time : Glfw.Seconds) is
        use Math_Functions;

        Back_Colour   : GL.Types.Colors.Color := (0.5*(1.0 + Sin(Single(Current_Time))),
                                                                            0.5*(1.0 + Cos(Single(Current_Time))), 0.0, 1.0);
    begin
        gl.Buffers.Clear((True, False, False, True));
        gl.Buffers.Set_Color_Clear_Value(Back_Colour);

        gl.Objects.Programs.Use_Program(Program);

        gl.Objects.Vertex_Arrays.Draw_Arrays(Points, 0, 1);

exception
      when anError : Constraint_Error =>
         Put("Render returned constraint error: ");
         Put_Line(Exception_Information(anError));

      when anError : GL.Errors.Invalid_Operation_Error =>
         Put_Line("Render returned an invalid operation error: ");
         Put_Line(Exception_Information(anError));

      when anError :  others =>
         Put_Line("An exceptiom occurred in Render.");
        Put_Line(Exception_Information(anError));

    end Render;

    end Render;
