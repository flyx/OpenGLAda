spec GL.API.UInts is
   pragma Preelaborate;
   use GL.Types.UInts;

   procedure Uniform1 (Location : Uniforms.Uniform; Value : UInt) is
     Dynamic ("glUniform1ui");
   procedure Uniform1v (Location : Uniforms.Uniform; Count : Size;
                        Value : UInt_Array) is Dynamic ("glUniform1uiv");
   procedure Uniform2 (Location : Uniforms.Uniform; V0, V1 : UInt) is
     Dynamic ("glUniform2ui");
   procedure Uniform2v (Location : Uniforms.Uniform; Count : Size;
                        Value : Vector2_Array) is Dynamic ("glUniform2uiv");
   procedure Uniform3 (Location : Uniforms.Uniform; V0, V1, V2 : UInt) is
     Dynamic ("glUniform3ui");
   procedure Uniform3v (Location : Uniforms.Uniform; Count : Size;
                        Value : Vector3_Array) is Dynamic ("glUniform3uiv");
   procedure Uniform4 (Location : Uniforms.Uniform; V0, V1, V2, V3 : UInt) is
     Dynamic ("glUniform4ui");
   procedure Uniform4v (Location : Uniforms.Uniform; Count : Size;
                        Value : Vector4_Array) is Dynamic ("glUniform4uiv");
   procedure Uniform_Matrix2 (Location : Uniforms.Uniform; Count : Size;
                              Transpose : Low_Level.Bool;
                              Value : Matrix2_Array) is
     Dynamic ("glUniformMatrix2uiv");
   procedure Uniform_Matrix3 (Location : Uniforms.Uniform; Count : Size;
                              Transpose : Low_Level.Bool;
                              Value : Matrix3_Array) is
     Dynamic ("glUniformMatrix3uiv");
   procedure Uniform_Matrix4 (Location : Uniforms.Uniform; Count : Size;
                              Transpose :Low_Level.Bool;
                              Value : Matrix4_Array) is
     Dynamic ("glUniformMatrix4uiv");
   procedure Vertex_Attrib1 (Index : Attributes.Attribute; Value : UInt) is
     Dynamic ("glVertexAttribI1ui");
   procedure Vertex_Attrib2 (Index : Attributes.Attribute; V0, V1 : UInt) is
     Dynamic ("glVertexAttribI2ui");
   procedure Vertex_Attrib2v (Index : Attributes.Attribute; Value : Vector2) is
     Dynamic ("glVertexAttribI2uiv");
   procedure Vertex_Attrib3 (Index : Attributes.Attribute; V0, V1, V2 : UInt) is
     Dynamic ("glVertexAttribI3ui");
   procedure Vertex_Attrib3v (Index : Attributes.Attribute; Value : Vector3) is
     Dynamic ("glVertexAttribI3uiv");
   procedure Vertex_Attrib4 (Index : Attributes.Attribute;
                             V0, V1, V2, V3 : UInt) is
     Dynamic ("glVertexAttribI4ui");
   procedure Vertex_Attrib4v (Index : Attributes.Attribute; Value : Vector4) is
     Dynamic ("glVertexAttrib4Iuiv");
end GL.API.UInts;
