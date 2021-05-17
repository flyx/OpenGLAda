--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.API;

package body GL.Objects.Queries is

    procedure Begin_Query (Target : GL.Low_Level.Enums.Query_Param;
                          Object : Query_Object) is
   begin
      API.Begin_Query (Target, Object.Reference.GL_Id);
      Raise_Exception_On_OpenGL_Error;
   end Begin_Query;

   procedure End_Query (Target : GL.Low_Level.Enums.Query_Param) is
   begin
      API.End_Query (Target);
      Raise_Exception_On_OpenGL_Error;
   end End_Query;

    procedure Begin_Query_Indexed (Target : GL.Low_Level.Enums.Query_Param;
                                   Index : UInt; Object : Query_Object) is
   begin
      API.Begin_Query_Indexed (Target, Index, Object.Reference.GL_Id);
      Raise_Exception_On_OpenGL_Error;
   end Begin_Query_Indexed;

   procedure End_Query_Indexed (Target : GL.Low_Level.Enums.Query_Param;
                                Index : UInt) is
   begin
      API.End_Query_Indexed (Target, Index);
      Raise_Exception_On_OpenGL_Error;
   end End_Query_Indexed;

   procedure Get_Query_Object
     (Object : Query_Object; Pname : GL.Low_Level.Enums.Query_Results;
      Params : out UInt) is
   begin
      API.Get_Query_Object (Object.Reference.GL_Id, Pname, Params);
      Raise_Exception_On_OpenGL_Error;
   end Get_Query_Object;

   overriding
   procedure Internal_Create_Id (Object : Query_Object; Id : out UInt) is
      pragma Unreferenced (Object);
   begin
      API.Gen_Queries (1, Id);
      Raise_Exception_On_OpenGL_Error;
   end Internal_Create_Id;

   overriding
   procedure Internal_Release_Id (Object : Query_Object; Id : UInt) is
      pragma Unreferenced (Object);
   begin
      API.Delete_Queries (1, (1 => Id));
      Raise_Exception_On_OpenGL_Error;
   end Internal_Release_Id;

   function Is_Query (Query :  Query_Object) return Boolean is
   begin
      return API.Is_Query (Raw_Id (Query));
   end Is_Query;

   procedure Query_Counter (Object : Query_Object;
                            Target : Low_Level.Enums.Query_Param) is
   begin
      API.Query_Counter (Object.Reference.GL_Id, Target);
      Raise_Exception_On_OpenGL_Error;
   end Query_Counter;

end GL.Objects.Queries;
