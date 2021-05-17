spec GL.API.UInts is
   procedure Uniform1 (Location : Uniforms.Uniform; Value : UInt) with
     Dynamic => "glUniform1ui",
     Wrapper => "GL.Uniforms.Set_UInt";
   procedure Uniform1v (Location : Uniforms.Uniform; Count : Size;
                        Value : UInt_Array) with
     Dynamic => "glUniform1uiv",
     Wrapper => "GL.Uniforms.Set_UInt";
   procedure Uniform2 (Location : Uniforms.Uniform; V0, V1 : UInt) with
     Dynamic => "glUniform2ui",
     Wrapper => "GL.Uniforms.Set_UInt";
   procedure Uniform2v (Location : Uniforms.Uniform; Count : Size;
                        Value : UInts.Vector2_Array) with
     Dynamic => "glUniform2uiv",
     Wrapper => "GL.Uniforms.Set_UInt";
   procedure Uniform3 (Location : Uniforms.Uniform; V0, V1, V2 : UInt) with
     Dynamic => "glUniform3ui",
     Wrapper => "GL.Uniforms.Set_UInt";
   procedure Uniform3v (Location : Uniforms.Uniform; Count : Size;
                        Value : UInts.Vector3_Array) with
     Dynamic => "glUniform3uiv",
     Wrapper => "GL.Uniforms.Set_UInt";
   procedure Uniform4 (Location : Uniforms.Uniform; V0, V1, V2, V3 : UInt) with
     Dynamic => "glUniform4ui",
     Wrapper => "GL.Uniforms.Set_UInt";
   procedure Uniform4v (Location : Uniforms.Uniform; Count : Size;
                        Value : UInts.Vector4_Array) with
     Dynamic => "glUniform4uiv",
     Wrapper => "GL.Uniforms.Set_UInt";
   procedure Uniform_Matrix2 (Location : Uniforms.Uniform; Count : Size;
                              Transpose : Low_Level.Bool;
                              Value : UInts.Matrix2_Array) with
     Dynamic => "glUniformMatrix2uiv",
     Wrapper => "GL.Uniforms.Set_UInt";
   procedure Uniform_Matrix3 (Location : Uniforms.Uniform; Count : Size;
                              Transpose : Low_Level.Bool;
                              Value : UInts.Matrix3_Array) with
     Dynamic => "glUniformMatrix3uiv",
     Wrapper => "GL.Uniforms.Set_UInt";
   procedure Uniform_Matrix4 (Location : Uniforms.Uniform; Count : Size;
                              Transpose :Low_Level.Bool;
                              Value : UInts.Matrix4_Array) with
     Dynamic => "glUniformMatrix4uiv",
     Wrapper => "GL.Uniforms.Set_UInt";
   procedure Vertex_Attrib1 (Index : Attributes.Attribute; Value : UInt) with
     Dynamic => "glVertexAttribI1ui",
     Wrapper => "GL.Attributes.Set_UInt";
   procedure Vertex_Attrib2 (Index : Attributes.Attribute; V0, V1 : UInt) with
     Dynamic => "glVertexAttribI2ui",
     Wrapper => "GL.Attributes.Set_UInt";
   procedure Vertex_Attrib2v (Index : Attributes.Attribute;
                              Value : UInts.Vector2) with
     Dynamic => "glVertexAttribI2uiv",
     Wrapper => "GL.Attributes.Set_UInt";
   procedure Vertex_Attrib3 (Index : Attributes.Attribute; V0, V1, V2 : UInt)
     with Dynamic => "glVertexAttribI3ui",
          Wrapper => "GL.Attributes.Set_UInt";
   procedure Vertex_Attrib3v (Index : Attributes.Attribute;
                              Value : UInts.Vector3) with
     Dynamic => "glVertexAttribI3uiv",
     Wrapper => "GL.Attributes.Set_UInt";
   procedure Vertex_Attrib4 (Index : Attributes.Attribute;
                             V0, V1, V2, V3 : UInt) with
     Dynamic => "glVertexAttribI4ui",
     Wrapper => "GL.Attributes.Set_UInt";
   procedure Vertex_Attrib4v (Index : Attributes.Attribute;
                              Value : UInts.Vector4) with
     Dynamic => "glVertexAttrib4Iuiv",
     Wrapper => "GL.Attributes.Set_UInt";
end GL.API.UInts;
