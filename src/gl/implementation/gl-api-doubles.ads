--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <flyx@isobeef.org>
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
end GL.API.Doubles;