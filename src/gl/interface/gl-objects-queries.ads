--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.Low_Level.Enums;

package GL.Objects.Queries is
   pragma Preelaborate;

   procedure Invalidate (Target : in out GL.Low_Level.Enums.Query_Param);

   type Query_Object is new GL_Object with private;

   Default_Query : constant Query_Object;

private

   type Query_Object is new GL_Object with null record;

   overriding
   procedure Internal_Create_Id (Object : Query_Object; Id : out UInt);

   overriding
   procedure Internal_Release_Id (Object : Query_Object; Id : UInt);

   Default_Query : constant Query_Object :=
                     Query_Object'(Ada.Finalization.Controlled with Reference =>
                                              Reference_To_Null_Object'Access);
end GL.Objects.Queries;
