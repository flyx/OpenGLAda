spec GL.API.Singles is
   procedure Get_Light_Position
     (Name   : Enums.Light_Name; Pname : Enums.Light_Param;
      Target : in out Types.Singles.Vector4) is Static ("glGetLightfv");
   procedure Get_Light_Direction
     (Name   : Enums.Light_Name; Pname : Enums.Light_Param;
      Target : in out Types.Singles.Vector3) is Static ("glGetLightfv");
   procedure Light_Position
     (Name  : Enums.Light_Name; Pname : Enums.Light_Param;
      Param : Types.Singles.Vector4) is Static ("glLightfv");
   procedure Light_Direction
     (Name  : Enums.Light_Name; Pname : Enums.Light_Param;
      Param : Types.Singles.Vector3) is Static ("glLightfv");
   procedure Uniform1 (Location : Uniforms.Uniform; Value : Single) is
     Dynamic ("glUniform1f");
   procedure Uniform1v (Location : Uniforms.Uniform; Count : Size;
                        Value : Single_Array) is Dynamic ("glUniform1fv");
   procedure Uniform2 (Location : Uniforms.Uniform; V0, V1 : Single) is
     Dynamic ("glUniform2f");
   procedure Uniform2v (Location : Uniforms.Uniform; Count : Size;
                        Value : Types.Singles.Vector2_Array) is
     Dynamic ("glUniform2fv");
   procedure Uniform3 (Location : Uniforms.Uniform; V0, V1, V2 : Single) is
     Dynamic ("glUniform3f");
   procedure Uniform3v (Location : Uniforms.Uniform; Count : Size;
                        Value : Types.Singles.Vector3_Array) is
     Dynamic ("glUniform3fv");
   procedure Uniform4 (Location : Uniforms.Uniform; V0, V1, V2, V3 : Single) is
     Dynamic ("glUniform4f");
   procedure Uniform4v (Location : Uniforms.Uniform; Count : Size;
                        Value : Types.Singles.Vector4_Array) is
     Dynamic ("glUniform4fv");
   procedure Uniform_Matrix2 (Location : Uniforms.Uniform; Count : Size;
                              Transpose : Low_Level.Bool;
                              Value : Types.Singles.Matrix2_Array) is
     Dynamic ("glUniformMatrix2fv");
   procedure Uniform_Matrix3 (Location : Uniforms.Uniform; Count : Size;
                              Transpose : Low_Level.Bool;
                              Value : Types.Singles.Matrix3_Array) is
     Dynamic ("glUniformMatrix3fv");
   procedure Uniform_Matrix4 (Location : Uniforms.Uniform; Count : Size;
                              Transpose : Low_Level.Bool;
                              Value : Types.Singles.Matrix4_Array) is
     Dynamic ("glUniformMatrix4fv");
   procedure Vertex_Attrib1 (Index : Attributes.Attribute; Value : Single) is
     Dynamic ("glVertexAttrib1f");
   procedure Vertex_Attrib2 (Index : Attributes.Attribute; V0, V1 : Single) is
     Dynamic ("glVertexAttrib2f");
   procedure Vertex_Attrib2v (Index : Attributes.Attribute;
                              Value : Types.Singles.Vector2) is
     Dynamic ("glVertexAttrib2fv");
   procedure Vertex_Attrib3 (Index : Attributes.Attribute; V0, V1, V2 : Single)
     is Dynamic ("glVertexAttrib3f");
   procedure Vertex_Attrib3v (Index : Attributes.Attribute;
                              Value : Types.Singles.Vector3) is
     Dynamic ("glVertexAttrib3fv");
   procedure Vertex_Attrib4 (Index : Attributes.Attribute;
                             V0, V1, V2, V3 : Single) is
     Dynamic ("glVertexAttrib4f");
   procedure Vertex_Attrib4v (Index : Attributes.Attribute;
                              Value : Types.Singles.Vector4) is
     Dynamic ("glVertexAttrib4fv");
end GL.API.Singles;
