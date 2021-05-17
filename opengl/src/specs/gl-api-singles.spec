spec GL.API.Singles is
   procedure Uniform1 (Location : Uniforms.Uniform; Value : Single) with
     Dynamic => "glUniform1f",
     Wrapper => "GL.Uniforms.Set_Single";
   procedure Uniform1v (Location : Uniforms.Uniform; Count : Size;
                        Value : Single_Array) with
     Dynamic => "glUniform1fv",
     Wrapper => "GL.Uniforms.Set_Single";
   procedure Uniform2 (Location : Uniforms.Uniform; V0, V1 : Single) with
     Dynamic => "glUniform2f",
     Wrapper => "GL.Uniforms.Set_Single";
   procedure Uniform2v (Location : Uniforms.Uniform; Count : Size;
                        Value : Types.Singles.Vector2_Array) with
     Dynamic => "glUniform2fv",
     Wrapper => "GL.Uniforms.Set_Single";
   procedure Uniform3 (Location : Uniforms.Uniform; V0, V1, V2 : Single) with
     Dynamic => "glUniform3f",
     Wrapper => "GL.Uniforms.Set_Single";
   procedure Uniform3v (Location : Uniforms.Uniform; Count : Size;
                        Value : Types.Singles.Vector3_Array) with
     Dynamic => "glUniform3fv",
     Wrapper => "GL.Uniforms.Set_Single";
   procedure Uniform4 (Location : Uniforms.Uniform; V0, V1, V2, V3 : Single)
     with Dynamic => "glUniform4f",
          Wrapper => "GL.Uniforms.Set_Single";
   procedure Uniform4v (Location : Uniforms.Uniform; Count : Size;
                        Value : Types.Singles.Vector4_Array) with
     Dynamic => "glUniform4fv",
     Wrapper => "GL.Uniforms.Set_Single";
   procedure Uniform_Matrix2 (Location : Uniforms.Uniform; Count : Size;
                              Transpose : Low_Level.Bool;
                              Value : Types.Singles.Matrix2_Array) with
     Dynamic => "glUniformMatrix2fv",
     Wrapper => "GL.Uniforms.Set_Single";
   procedure Uniform_Matrix3 (Location : Uniforms.Uniform; Count : Size;
                              Transpose : Low_Level.Bool;
                              Value : Types.Singles.Matrix3_Array) with
     Dynamic => "glUniformMatrix3fv",
     Wrapper => "GL.Uniforms.Set_Single";
   procedure Uniform_Matrix4 (Location : Uniforms.Uniform; Count : Size;
                              Transpose : Low_Level.Bool;
                              Value : Types.Singles.Matrix4_Array) with
     Dynamic => "glUniformMatrix4fv",
     Wrapper => "GL.Uniforms.Set_Single";
   procedure Vertex_Attrib1 (Index : Attributes.Attribute; Value : Single) with
     Dynamic => "glVertexAttrib1f",
     Wrapper => "GL.Attributes.Set_Single";
   procedure Vertex_Attrib2 (Index : Attributes.Attribute; V0, V1 : Single)
     with Dynamic => "glVertexAttrib2f",
          Wrapper => "GL.Attributes.Set_Single";
   procedure Vertex_Attrib2v (Index : Attributes.Attribute;
                              Value : Types.Singles.Vector2) with
     Dynamic => "glVertexAttrib2fv",
     Wrapper => "GL.Attributes.Set_Single";
   procedure Vertex_Attrib3 (Index : Attributes.Attribute; V0, V1, V2 : Single)
     with Dynamic => "glVertexAttrib3f",
          Wrapper => "GL.Attributes.Set_Single";
   procedure Vertex_Attrib3v (Index : Attributes.Attribute;
                              Value : Types.Singles.Vector3) with
     Dynamic => "glVertexAttrib3fv",
     Wrapper => "GL.Attributes.Set_Single";
   procedure Vertex_Attrib4 (Index : Attributes.Attribute;
                             V0, V1, V2, V3 : Single) with
     Dynamic => "glVertexAttrib4f",
     Wrapper => "GL.Attributes.Set_Single";
   procedure Vertex_Attrib4v (Index : Attributes.Attribute;
                              Value : Types.Singles.Vector4) with
     Dynamic => "glVertexAttrib4fv",
     Wrapper => "GL.Attributes.Set_Single";
end GL.API.Singles;
