--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

package GL.Objects.Queries is
   pragma Preelaborate;

   type Query_Object is new GL_Object with private;

   Default_Query : constant Query_Object;

private

   type Query_Object is new GL_Object with null record;

   overriding
   procedure Internal_Create_Id (Object : Query_Object; Id : out UInt);

   overriding
   procedure Internal_Release_Id (Object : Query_Object; Id : UInt);

   function Is_Query (Query : UInt) return Boolean;

   Default_Query : constant Query_Object :=
                     Query_Object'(Ada.Finalization.Controlled with Reference =>
                                              Reference_To_Null_Object'Access);
end GL.Objects.Queries;
