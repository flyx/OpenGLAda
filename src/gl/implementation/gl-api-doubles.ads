--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <contact@flyx.org>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

package GL.API.Doubles is
   pragma Preelaborate;
   
   use GL.Types.Doubles;
   
   procedure Load_Matrix (Value : Matrix4);
   pragma Import (Convention => StdCall, Entity => Load_Matrix,
                  External_Name => "glLoadMatrixd");

   procedure Mult_Matrix (Factor : Matrix4);
   pragma Import (Convention => StdCall, Entity => Mult_Matrix,
                  External_Name => "glMultMatrixd");
   
   procedure Vertex4 (Value : Vector4);
   pragma Import (Convention => StdCall, Entity => Vertex4,
                  External_Name => "glVertex4dv");

   procedure Vertex3 (Value : Vector3);
   pragma Import (Convention => StdCall, Entity => Vertex3,
                  External_Name => "glVertex3dv");

   procedure Vertex2 (Value : Vector2);
   pragma Import (Convention => StdCall, Entity => Vertex2,
                  External_Name => "glVertex2dv");
   
   procedure Normal (Value : Vector3);
   pragma Import (Convention => StdCall, Entity => Normal,
                  External_Name => "glNormal3dv");
   
   procedure Tex_Coord4 (Value : Vector4);
   pragma Import (Convention => StdCall, Entity => Tex_Coord4,
                  External_Name => "glTexCoord4dv");

   procedure Tex_Coord3 (Value : Vector3);
   pragma Import (Convention => StdCall, Entity => Tex_Coord3,
                  External_Name => "glTexCoord3dv");

   procedure Tex_Coord2 (Value : Vector2);
   pragma Import (Convention => StdCall, Entity => Tex_Coord2,
                  External_Name => "glTexCoord2dv");
   
   procedure Raster_Pos4 (Value : Vector4);
   pragma Import (Convention => StdCall, Entity => Raster_Pos4,
                  External_Name => "glRasterPos4dv");
   
   procedure Raster_Pos3 (Value : Vector3);
   pragma Import (Convention => StdCall, Entity => Raster_Pos3,
                  External_Name => "glRasterPos3dv");
   
   procedure Raster_Pos2 (Value : Vector2);
   pragma Import (Convention => StdCall, Entity => Raster_Pos2,
                  External_Name => "glRasterPos2dv");
   
   procedure Vertex_Attrib1 is new Loader.Procedure_With_2_Params
     ("glVertexAttribL1d", Attributes.Attribute, Double);
   
   procedure Vertex_Attrib2 is new Loader.Procedure_With_3_Params
     ("glVertexAttribL2d", Attributes.Attribute, Double, Double);
   
   procedure Vertex_Attrib2v is new Loader.Procedure_With_2_Params
     ("glVertexAttribL2dv", Attributes.Attribute, Vector2);
   
   procedure Vertex_Attrib3 is new Loader.Procedure_With_4_Params
     ("glVertexAttribL3d", Attributes.Attribute, Double, Double, Double);
   
   procedure Vertex_Attrib3v is new Loader.Procedure_With_2_Params
     ("glVertexAttribL3dv", Attributes.Attribute, Vector3);
   
   procedure Vertex_Attrib4 is new Loader.Procedure_With_5_Params
     ("glVertexAttribL4d", Attributes.Attribute, Double, Double, Double, Double);
   
   procedure Vertex_Attrib4v is new Loader.Procedure_With_2_Params
     ("glVertexAttribL4dv", Attributes.Attribute, Vector4);
end GL.API.Doubles;
