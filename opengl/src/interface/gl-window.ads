--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.Types;

package GL.Window is
   use GL.Types;

   procedure Set_Viewport (X, Y : Int; Width, Height : Size);
   procedure Get_Viewport (X, Y : out Int; Width, Height : out Size);

   procedure Set_Depth_Range (Near, Far : Double);
   procedure Get_Depth_Range (Near, Far : out Double);

end GL.Window;
