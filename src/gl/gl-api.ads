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

with GL.Enums;
with GL.Enums.Getter;
with GL.Low_Level;
with GL.Matrices;
with GL.Immediate;
with GL.Vectors;
with GL.Colors;
with GL.Normals;
with GL.Buffers;

private package GL.API is

   function Get_Error return Enums.Error_Code;
   pragma Import (Convention => StdCall, Entity => Get_Error,
                  External_Name => "glGetError");

   procedure Flush;
   pragma Import (Convention => StdCall, Entity => Flush,
                  External_Name => "glFlush");

   -----------------------------------------------------------------------------
   --                           Parameter Getters                             --
   -----------------------------------------------------------------------------

   procedure Get_Boolean (Name   : Enums.Getter.Parameter;
                          Target : access Low_Level.Bool);
   pragma Import (Convention => StdCall, Entity => Get_Boolean,
                  External_Name => "glGetBooleanv");

   procedure Get_Double (Name   : Enums.Getter.Parameter;
                         Target : access Low_Level.Double);
   pragma Import (Convention => StdCall, Entity => Get_Double,
                  External_Name => "glGetDoublev");

   procedure Get_Integer (Name   : Enums.Getter.Parameter;
                          Target : access C.int);
   pragma Import (Convention => StdCall, Entity => Get_Integer,
                  External_Name => "glGetIntegerv");

   -----------------------------------------------------------------------------
   --            Matrix stack API (deprecated as of OpenGL 3.0)               --
   -----------------------------------------------------------------------------

   procedure Matrix_Mode (Mode : Enums.Matrix_Mode);
   pragma Import (Convention => StdCall, Entity => Matrix_Mode,
                  External_Name => "glMatrixMode");

   procedure Frustum (Left, Right, Bottom, Top, zNear, zFar : Low_Level.Double);
   pragma Import (Convention => StdCall, Entity => Frustum,
                  External_Name => "glFrustum");

   procedure Ortho (Left, Right, Bottom, Top, zNear, zFar : Low_Level.Double);
   pragma Import (Convention => StdCall, Entity => Ortho,
                  External_Name => "glOrtho");

   procedure Load_Identity;
   pragma Import (Convention => StdCall, Entity => Load_Identity,
                  External_Name => "glLoadIdentity");

   procedure Load_Matrix (Value : Matrices.Matrix);
   pragma Import (Convention => StdCall, Entity => Load_Matrix,
                  External_Name => "glLoadMatrixd");

   procedure Mult_Matrix (Factor : Matrices.Matrix);
   pragma Import (Convention => StdCall, Entity => Mult_Matrix,
                  External_Name => "glMultMatrixd");

   procedure Push_Matrix;
   pragma Import (Convention => StdCall, Entity => Push_Matrix,
                  External_Name => "glPushMatrix");

   procedure Pop_Matrix;
   pragma Import (Convention => StdCall, Entity => Pop_Matrix,
                  External_Name => "glPopMatrix");

   procedure Rotate (Angle, X, Y, Z : Low_Level.Double);
   pragma Import (Convention => StdCall, Entity => Rotate,
                  External_Name => "glRotated");

   procedure Scale (X, Y, Z : Low_Level.Double);
   pragma Import (Convention => StdCall, Entity => Scale,
                  External_Name => "glScaled");

   procedure Translate (X, Y, Z : Low_Level.Double);
   pragma Import (Convention => StdCall, Entity => Translate,
                  External_Name => "glTranslated");

   -----------------------------------------------------------------------------
   --              Immediate API (deprecated as of OpenGL 3.0)                --
   -----------------------------------------------------------------------------

   procedure GL_Begin (Mode : Immediate.Connection_Mode);
   pragma Import (Convention => StdCall, Entity => GL_Begin,
                  External_Name => "glBegin");

   procedure GL_End;
   pragma Import (Convention => StdCall, Entity => GL_End,
                  External_Name => "glEnd");

   procedure Vertex (Value : Vectors.Vector);
   pragma Import (Convention => StdCall, Entity => Vertex,
                  External_Name => "glVertex4dv");

   procedure Color (Value : Colors.Color);
   pragma Import (Convention => StdCall, Entity => Color,
                  External_Name => "glColor4dv");

   procedure Secondary_Color (Value : Colors.Basic_Color);
   pragma Import (Convention => StdCall, Entity => Secondary_Color,
                  External_Name => "glSecondaryColor3dv");

   procedure Fog_Coord (Distance : Low_Level.Double);
   pragma Import (Convention => StdCall, Entity => Fog_Coord,
                  External_Name => "glFogCoordd");

   procedure Normal (Value : Normals.Normal);
   pragma Import (Convention => StdCall, Entity => Normal,
                  External_Name => "glNormal3dv");

   procedure Tex_Coord (Value : Vectors.Vector);
   pragma Import (Convention => StdCall, Entity => Tex_Coord,
                  External_Name => "glTexCoord4dv");
                  
   -----------------------------------------------------------------------------
   --                                Buffers                                  --
   -----------------------------------------------------------------------------
   
   procedure Clear (Bits : Low_Level.Bitfield);
   pragma Import (Convention => StdCall, Entity => Clear,
                  External_Name => "glClear");
   
   procedure Draw_Buffer (Mode : Buffers.Color_Buffer_Selector);
   pragma Import (Convention => StdCall, Entity => Draw_Buffer,
                  External_Name => "glDrawBuffer");

end GL.API;
