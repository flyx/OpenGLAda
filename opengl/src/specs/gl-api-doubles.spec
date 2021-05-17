spec GL.API.Doubles is
   procedure Vertex_Attrib1 (Index : Attributes.Attribute; Value : Double) with
     Dynamic => "glVertexAttribL1d",
     Wrapper => "GL.Attributes.Set_Double";
   procedure Vertex_Attrib2 (Index : Attributes.Attribute; V0, V1 : Double) with
     Dynamic => "glVertexAttribL2d",
     Wrapper => "GL.Attributes.Set_Double";
   procedure Vertex_Attrib2v (Index : Attributes.Attribute;
                              Value : Types.Doubles.Vector2) with
     Dynamic => "glVertexAttribL2dv",
     Wrapper => "GL.Attributes.Set_Double";
   procedure Vertex_Attrib3 (Index : Attributes.Attribute; V0, V1, V2 : Double)
     with Dynamic => "glVertexAttribL3d",
          Wrapper => "GL.Attributes.Set_Double";
   procedure Vertex_Attrib3v (Index : Attributes.Attribute;
                              Value : Types.Doubles.Vector3) with
     Dynamic => "glVertexAttribL3dv",
     Wrapper => "GL.Attributes.Set_Double";
   procedure Vertex_Attrib4 (Index : Attributes.Attribute;
                             V0, V1, V2, V3 : Double) with
     Dynamic => "glVertexAttribL4d",
     Wrapper => "GL.Attributes.Set_Double";
   procedure Vertex_Attrib4v (Index : Attributes.Attribute;
                              Value : Types.Doubles.Vector4) with
     Dynamic => "glVertexAttribL4dv",
     Wrapper => "GL.Attributes.Set_Double";
end GL.API.Doubles;