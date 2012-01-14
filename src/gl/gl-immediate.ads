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

with Ada.Finalization;
with GL.Low_Level;
with GL.Colors;
with GL.Vectors;
with GL.Normals;

-- This package provides functions to directly insert vertices, colors, normals
-- etc. into the pipeline. Note that these functions have been deprecated with
-- OpenGL 3, you should use VBOs instead.
package GL.Immediate is

   type Connection_Mode is (Points, Lines, Line_Loop, Line_Strip, Triangles,
                            Triangle_Strip, Triangle_Fan, Quads, Quad_Strip,
                            Polygon);

   type Input_Token (<>) is new Ada.Finalization.Limited_Controlled with private;

   overriding procedure Finalize (Token : in out Input_Token);

   function Start (Mode : Connection_Mode) return Input_Token;

   procedure Add_Vertex (Token : Input_Token; Vertex : Vectors.Vector);

   procedure Set_Color (Value : Colors.Color);
   function Current_Color return Colors.Color;

   procedure Set_Fog_Distance (Value : Real);
   function Current_Fog_Distance return Real;

   procedure Set_Normal (Value : Normals.Normal);
   function Current_Normal return Normals.Normal;

   procedure Set_Texture_Coordinates (Value : Vectors.Vector);
   function Current_Texture_Coordinates return Vectors.Vector;


private
   type Input_Token (Mode : Connection_Mode) is
     new Ada.Finalization.Limited_Controlled with null record;

   for Connection_Mode use (Points         => 16#0000#,
                            Lines          => 16#0001#,
                            Line_Loop      => 16#0002#,
                            Line_Strip     => 16#0003#,
                            Triangles      => 16#0004#,
                            Triangle_Strip => 16#0005#,
                            Triangle_Fan   => 16#0006#,
                            Quads          => 16#0007#,
                            Quad_Strip     => 16#0008#,
                            Polygon        => 16#0009#);
   for Connection_Mode'Size use Low_Level.Enum'Size;

end GL.Immediate;
