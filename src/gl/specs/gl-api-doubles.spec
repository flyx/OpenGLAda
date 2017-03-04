spec GL.API.Doubles is
   procedure Raster_Pos4 (Value : Types.Doubles.Vector4) is
     Static ("glRasterPos4dv");
   procedure Raster_Pos3 (Value : Types.Doubles.Vector3) is
     Static ("glRasterPos3dv");
   procedure Raster_Pos2 (Value : Types.Doubles.Vector2) is
     Static ("glRasterPos2dv");

   procedure Vertex_Attrib1 (Index : Attributes.Attribute; Value : Double) is
     Dynamic ("glVertexAttribL1d");
   procedure Vertex_Attrib2 (Index : Attributes.Attribute; V0, V1 : Double) is
     Dynamic ("glVertexAttribL2d");
   procedure Vertex_Attrib2v (Index : Attributes.Attribute;
                              Value : Types.Doubles.Vector2) is
     Dynamic ("glVertexAttribL2dv");
   procedure Vertex_Attrib3 (Index : Attributes.Attribute; V0, V1, V2 : Double)
     is Dynamic ("glVertexAttribL3d");
   procedure Vertex_Attrib3v (Index : Attributes.Attribute;
                              Value : Types.Doubles.Vector3) is
     Dynamic ("glVertexAttribL3dv");
   procedure Vertex_Attrib4 (Index : Attributes.Attribute;
                             V0, V1, V2, V3 : Double) is
     Dynamic ("glVertexAttribL4d");
   procedure Vertex_Attrib4v (Index : Attributes.Attribute;
                              Value : Types.Doubles.Vector4) is
     Dynamic ("glVertexAttribL4dv");
end GL.API.Doubles;