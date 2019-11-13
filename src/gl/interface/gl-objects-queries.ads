--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.Low_Level.Enums;

package GL.Objects.Queries is
   pragma Preelaborate;

   type Query_Object is new GL_Object with private;

   Default_Query : constant Query_Object;

   procedure Begin_Query (Target : GL.Low_Level.Enums.Query_Param;
                          Object : GL.Objects.Queries.Query_Object);

   procedure End_Query (Target : GL.Low_Level.Enums.Query_Param);

   procedure Begin_Query_Indexed (Target : GL.Low_Level.Enums.Query_Param;
                                  Index : UInt; Object : GL.Objects.Queries.Query_Object);

   procedure End_Query_Indexed (Target : GL.Low_Level.Enums.Query_Param;
                                Index : UInt);

   procedure Query_Counter (Object : GL.Objects.Queries.Query_Object;
                            Target : Low_Level.Enums.Query_Param);

   procedure Get_Query_Object (Object : GL.Objects.Queries.Query_Object;
                               Pname : GL.Low_Level.Enums.Query_Results;
                               Params : out Int_Array);

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
