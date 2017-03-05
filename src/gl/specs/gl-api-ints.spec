spec GL.API.Ints is
   procedure Uniform1 (Location : Uniforms.Uniform; Value : Int) with
     Dynamic => "glUniform1i",
     Wrapper => "GL.Uniforms.Set_Int";
   procedure Uniform1v (Location : Uniforms.Uniform; Count : Size;
                        Value : Int_Array) with
     Dynamic => "glUniform1iv",
     Wrapper => "GL.Uniforms.Set_Int";
   procedure Uniform2 (Location : Uniforms.Uniform; V0, V1 : Int) with
     Dynamic => "glUniform2i",
     Wrapper => "GL.Uniforms.Set_Int";
   procedure Uniform2v (Location : Uniforms.Uniform; Count : Size;
                        Value : Types.Ints.Vector2_Array) with
     Dynamic => "glUniform2iv",
     Wrapper => "GL.Uniforms.Set_Int";
   procedure Uniform3 (Location : Uniforms.Uniform; V0, V1, V2 : Int) with
     Dynamic => "glUniform3i",
     Wrapper => "GL.Uniforms.Set_Int";
   procedure Uniform3v (Location : Uniforms.Uniform; Count : Size;
                        Value : Types.Ints.Vector3_Array) with
     Dynamic => "glUniform3iv",
     Wrapper => "GL.Uniforms.Set_Int";
   procedure Uniform4 (Location : Uniforms.Uniform; V0, V1, V2, V3 : Int) with
     Dynamic => "glUniform4i",
     Wrapper => "GL.Uniforms.Set_Int";
   procedure Uniform4v (Location : Uniforms.Uniform; Count : Size;
                        Valeu : Types.Ints.Vector4_Array) with
     Dynamic => "glUniform4iv",
     Wrapper => "GL.Uniforms.Set_Int";
   procedure Uniform_Matrix2 (Location : Uniforms.Uniform; Count : Size;
                              Transpose : Low_Level.Bool;
                              Value : Types.Ints.Matrix2_Array) with
     Dynamic => "glUniformMatrix2iv",
     Wrapper => "GL.Uniforms.Set_Int";
   procedure Uniform_Matrix3 (Location : Uniforms.Uniform; Count : Size;
                              Transpose : Low_Level.Bool;
                              Value : Types.Ints.Matrix3_Array) with
     Dynamic => "glUniformMatrix3iv",
     Wrapper => "GL.Uniforms.Set_Int";
   procedure Uniform_Matrix4 (Location : Uniforms.Uniform; Count : Size;
                              Transpose : Low_Level.Bool;
                              Value : Types.Ints.Matrix4_Array) with
     Dynamic => "glUniformMatrix4iv",
     Wrapper => "GL.Uniforms.Set_Int";
   procedure Vertex_Attrib1 (Index : Attributes.Attribute; Value : Int) with
     Dynamic => "glVertexAttribI1i",
     Wrapper => "GL.Attributes.Set_Int";
   procedure Vertex_Attrib2 (Index : Attributes.Attribute; V0, V1 : Int) with
     Dynamic => "glVertexAttribI2i",
     Wrapper => "GL.Attributes.Set_Int";
   procedure Vertex_Attrib2v (Index : Attributes.Attribute;
                              Value : Types.Ints.Vector2) with
     Dynamic => "glVertexAttribI2iv",
     Wrapper => "GL.Attributes.Set_Int";
   procedure Vertex_Attrib3 (Index : Attributes.Attribute; V0, V1, V2 : Int)
     with Dynamic => "glVertexAttribI3i",
          Wrapper => "GL.Attributes.Set_Int";
   procedure Vertex_Attrib3v (Index : Attributes.Attribute;
                              Value : Types.Ints.Vector3) with
     Dynamic => "glVertexAttribI3iv",
     Wrapper => "GL.Attributes.Set_Int";
   procedure Vertex_Attrib4 (Index : Attributes.Attribute;
                             V0, V1, V2, V3 : Int) with
     Dynamic => "glVertexAttribI4i",
     Wrapper => "GL.Attributes.Set_Int";
   procedure Vertex_Attrib4v (Index : Attributes.Attribute;
                              Value : Types.Ints.Vector4) with
     Dynamic => "glVertexAttrib4Iiv",
     Wrapper => "GL.Attributes.Set_Int";
end GL.API.Ints;
