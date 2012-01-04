with GL.CGL;
with System;

procedure CGL_Test1 is
   type Dummy_Refs_Array is array (Positive range <>) of System.Address;

   --  just to test if the cgl functions are correctly imported
   Dummy_Refs : constant Dummy_Refs_Array :=
     (GL.CGL.CGLChoosePixelFormat'Address, GL.CGL.CGLDestroyPixelFormat'Address,
      GL.CGL.CGLDescribePixelFormat'Address);

begin
   null;
end CGL_Test1;
