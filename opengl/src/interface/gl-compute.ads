--  part of OpenGLAda, (c) 2022 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.Types;

package GL.Compute is
   pragma Preelaborate;

   use GL.Types;

   type Index_Type is (X, Y, Z);

   function Max_Compute_Work_Group_Count (Index : Index_Type) return Int;
   function Max_Compute_Work_Group_Size (Index : Index_Type) return Int;
   function Max_Compute_Work_Group_Invocations return Int;

   procedure Dispatch_Compute
     (Num_Groups_X, Num_Groups_Y, Num_Groups_Z : UInt);

private
   for Index_Type use (X => 0, Y => 1, Z => 2);
end GL.Compute;

