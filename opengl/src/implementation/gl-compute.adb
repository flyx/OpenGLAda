--  part of OpenGLAda, (c) 2022 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.Enums.Getter;
with GL.API;

package body GL.Compute is
   function Max_Compute_Work_Group_Count (Index : Index_Type) return Int is
      Value : aliased Int;
   begin
      API.Get_Integer_Indexed
        (GL.Enums.Getter.Max_Compute_Work_Group_Count,
         Index_Type'Pos (Index),
         Value'Access);
      Raise_Exception_On_OpenGL_Error;
      return Value;
   end Max_Compute_Work_Group_Count;

   function Max_Compute_Work_Group_Size (Index : Index_Type) return Int is
      Value : aliased Int;
   begin
      API.Get_Integer_Indexed
        (GL.Enums.Getter.Max_Compute_Work_Group_Size,
         Index_Type'Pos (Index),
         Value'Access);
      Raise_Exception_On_OpenGL_Error;
      return Value;
   end Max_Compute_Work_Group_Size;

   function Max_Compute_Work_Group_Invocations return Int is
      Value : aliased Int;
   begin
      API.Get_Integer
        (GL.Enums.Getter.Max_Compute_Work_Group_Invocations,
         Value'Access);
      Raise_Exception_On_OpenGL_Error;
      return Value;
   end Max_Compute_Work_Group_Invocations;

   procedure Dispatch_Compute
     (Num_Groups_X, Num_Groups_Y, Num_Groups_Z : UInt) is
   begin
      API.Dispatch_Compute (Num_Groups_X, Num_Groups_Y, Num_Groups_Z);
      Raise_Exception_On_OpenGL_Error;
   end Dispatch_Compute;
end GL.Compute;


