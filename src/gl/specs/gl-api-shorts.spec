spec GL.API.Shorts is
   procedure Vertex_Attrib1 (Index : Attributes.Attribute; Value : Short) with
     Dynamic => "glVertexAttrib1s",
     Wrapper => "GL.Attributes.Set_Short";
   procedure Vertex_Attrib2 (Index : Attributes.Attribute; V0, V1 : Short) with
     Dynamic => "glVertexAttrib2s",
     Wrapper => "GL.Attributes.Set_Short";
   procedure Vertex_Attrib2v (Index : Attributes.Attribute;
                              Value : Shorts.Vector2) with
     Dynamic => "glVertexAttrib2sv",
     Wrapper => "GL.Attributes.Set_Short";
   procedure Vertex_Attrib3 (Index : Attributes.Attribute; V0, V1, V2 : Short)
     with Dynamic => "glVertexAttrib3s",
          Wrapper => "GL.Attributes.Set_Short";
   procedure Vertex_Attrib3v (Index : Attributes.Attribute;
                              Value : Shorts.Vector3) with
     Dynamic => "glVertexAttrib3sv",
     Wrapper => "GL.Attributes.Set_Short";
   procedure Vertex_Attrib4 (Index : Attributes.Attribute;
                             V0, V1, V2, V3 : Short) with
     Dynamic => "glVertexAttrib4s",
     Wrapper => "GL.Attributes.Set_Short";
   procedure Vertex_Attrib4v (Index : Attributes.Attribute;
                              Value : Shorts.Vector4) with
     Dynamic => "glVertexAttrib4sv",
     Wrapper => "GL.Attributes.Set_Short";
end GL.API.Shorts;