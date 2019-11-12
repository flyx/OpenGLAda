--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"


--  private with GL.Low_Level.Enums;

package GL.Objects.Queries is
   pragma Preelaborate;

   type Query_Target is (Time_Elapsed, Samples_Passed, Any_Samples_Passed,
                         Transform_Feedback_Primitives_Written);

   procedure Invalidate (Target : in out Query_Target);

   type Query is new GL_Object with private;

   Default_Query : constant Query;
private

   type Query is new GL_Object with null record;

   overriding
   procedure Internal_Create_Id (Object : Query; Id : out UInt);

   overriding
   procedure Internal_Release_Id (Object : Query; Id : UInt);

   Default_Query : constant Query :=
                     Query'(Ada.Finalization.Controlled with Reference =>
                                              Reference_To_Null_Object'Access);

   for Query_Target use (Time_Elapsed                          => 16#88BF#,
                         Samples_Passed                        => 16#8914#,
                         Any_Samples_Passed                    => 16#8C2F#,
                         Transform_Feedback_Primitives_Written => 16#8C88#);
   for Query_Target'Size use Low_Level.Enum'Size;

end GL.Objects.Queries;
