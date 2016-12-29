spec GL.API.Shorts is
   pragma Preelaborate;
   use GL.Types.Shorts;

   procedure Vertex_Attrib1 (Index : Attributes.Attribute; Value : Short) is
     Dynamic ("glVertexAttrib1s");
   procedure Vertex_Attrib2 (Index : Attributes.Attribute; V0, V1 : Short) is
     Dynamic ("glVertexAttrib2s");
   procedure Vertex_Attrib2v (Index : Attributes.Attribute; Value : Vector2) is
     Dynamic ("glVertexAttrib2sv");
   procedure Vertex_Attrib3 (Index : Attributes.Attribute; V0, V1, V2 : Short)
     is Dynamic ("glVertexAttrib3s");
   procedure Vertex_Attrib3v (Index : Attributes.Attribute; Value : Vector3) is
     Dynamic ("glVertexAttrib3sv");
   procedure Vertex_Attrib4 (Index : Attributes.Attribute;
                             V0, V1, V2, V3 : Short) is
     Dynamic ("glVertexAttrib4s");
   procedure Vertex_Attrib4v (Index : Attributes.Attribute; Value : Vector4) is
     Dynamic ("glVertexAttrib4sv");
end GL.API.Shorts;