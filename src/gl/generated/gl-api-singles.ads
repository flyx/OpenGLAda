-- Autogenerated by Generate, do not edit
package GL.API.Singles is
   pragma Preelaborate;
   use GL.Types;
   procedure Get_Light_Position (Name : Enums.Light_Name; Pname : Enums.Light_Param; Target : in out Types.Singles.Vector4);
   pragma Import (StdCall, Get_Light_Position, "glGetLightfv");
   procedure Get_Light_Direction (Name : Enums.Light_Name; Pname : Enums.Light_Param; Target : in out Types.Singles.Vector3);
   pragma Import (StdCall, Get_Light_Direction, "glGetLightfv");
   procedure Light_Position (Name : Enums.Light_Name; Pname : Enums.Light_Param; Param : Types.Singles.Vector4);
   pragma Import (StdCall, Light_Position, "glLightfv");
   procedure Light_Direction (Name : Enums.Light_Name; Pname : Enums.Light_Param; Param : Types.Singles.Vector3);
   pragma Import (StdCall, Light_Direction, "glLightfv");
   Uniform1 : T33;
   Uniform1v : T34;
   Uniform2 : T35;
   Uniform2v : T36;
   Uniform3 : T37;
   Uniform3v : T38;
   Uniform4 : T39;
   Uniform4v : T40;
   Uniform_Matrix2 : T41;
   Uniform_Matrix3 : T42;
   Uniform_Matrix4 : T43;
   Vertex_Attrib1 : T44;
   Vertex_Attrib2 : T45;
   Vertex_Attrib2v : T46;
   Vertex_Attrib3 : T47;
   Vertex_Attrib3v : T48;
   Vertex_Attrib4 : T49;
   Vertex_Attrib4v : T50;
end GL.API.Singles;