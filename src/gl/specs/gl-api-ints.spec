spec GL.API.Ints is
   pragma Preelaborate;
   use GL.Types.Ints;

   procedure Get_Light_Position (Name   : Enums.Light_Name;
                                 Pname  : Enums.Light_Param;
                                 Target : in out Vector4) is
     Static ("glGetLightiv");
   procedure Light_Position
     (Name  : Enums.Light_Name; Pname : Enums.Light_Param;
      Param : Vector4) is Static("glLightiv");
   procedure Uniform1 (Location : Uniforms.Uniform; Value : Int) is
     Dynamic ("glUniform1i");
   procedure Uniform1v (Location : Uniforms.Uniform; Count : Size;
                        Value : Int_Array) is Dynamic ("glUniform1iv");
   procedure Uniform2 (Location : Uniforms.Uniform; V0, V1 : Int) is
     Dynamic ("glUniform2i");
   procedure Uniform2v (Location : Uniforms.Uniform; Count : Size;
                        Value : Vector2_Array) is Dynamic ("glUniform2iv");
   procedure Uniform3 (Location : Uniforms.Uniform; V0, V1, V2 : Int) is
     Dynamic ("glUniform3i");
   procedure Uniform3v (Location : Uniforms.Uniform; Count : Size;
                        Value : Vector3_Array) is Dynamic ("glUniform3iv");
   procedure Uniform4 (Location : Uniforms.Uniform; V0, V1, V2, V3 : Int) is
     Dynamic ("glUniform4i");
   procedure Uniform4v (Location : Uniforms.Uniform; Count : Size;
                        Valeu : Vector4_Array) is Dynamic ("glUniform4iv");
   procedure Uniform_Matrix2 (Location : Uniforms.Uniform; Count : Size;
                              Transpose : Low_Level.Bool;
                              Value : Matrix2_Array) is
     Dynamic ("glUniformMatrix2iv");
   procedure Uniform_Matrix3 (Location : Uniforms.Uniform; Count : Size;
                              Transpose : Low_Level.Bool;
                              Value : Matrix3_Array) is
     Dynamic ("glUniformMatrix3iv");
   procedure Uniform_Matrix4 (Location : Uniforms.Uniform; Count : Size;
                              Transpose : Low_Level.Bool;
                              Value : Matrix4_Array) is
     Dynamic ("glUniformMatrix4iv");
   procedure Vertex_Attrib1 (Index : Attributes.Attribute; Value : Int) is
     Dynamic ("glVertexAttribI1i");
   procedure Vertex_Attrib2 (Index : Attributes.Attribute; V0, V1 : Int) is
     Dynamic ("glVertexAttribI2i");
   procedure Vertex_Attrib2v (Index : Attributes.Attribute; Value : Vector2) is
     Dynamic ("glVertexAttribI2iv");
   procedure Vertex_Attrib3 (Index : Attributes.Attribute; V0, V1, V2 : Int) is
     Dynamic ("glVertexAttribI3i");
   procedure Vertex_Attrib3v (Index : Attributes.Attribute; Value : Vector3) is
     Dynamic ("glVertexAttribI3iv");
   procedure Vertex_Attrib4 (Index : Attributes.Attribute;
                             V0, V1, V2, V3 : Int) is
     Dynamic ("glVertexAttribI4i");
   procedure Vertex_Attrib4v (Index : Attributes.Attribute; Value : Vector4) is
     Dynamic ("glVertexAttrib4Iiv");
end GL.API.Ints;
