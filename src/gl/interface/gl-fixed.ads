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

with GL.Types;

private with GL.Low_Level;

-- Fixed function pipeline. Deprecated in OpenGL 3.0.
package GL.Fixed is
   pragma Preelaborate;
   
   use GL.Types;
   
   subtype Vertex_Length is Positive range 2 .. 4;
   
   type Client_Side_Capability is (Vertex_Array, Normal_Array, Color_Array,
                                   Index_Array, Texture_Coord_Array,
                                   Edge_Flag_Array, Fog_Coord_Array,
                                   Secondary_Color_Array);
   
   procedure Set_Vertex_Pointer (Length : Vertex_Length; Stride, Offset : Size);
   procedure Set_Color_Pointer  (Stride, Offset : Size);
   
   procedure Enable (Capability : Client_Side_Capability);
   procedure Disable (Capability : Client_Side_Capability);
   
private
   for Client_Side_Capability use (Vertex_Array          => 16#8074#,
                                   Normal_Array          => 16#8075#, 
                                   Color_Array           => 16#8076#,
                                   Index_Array           => 16#8077#,
                                   Texture_Coord_Array   => 16#8078#,
                                   Edge_Flag_Array       => 16#8079#,
                                   Fog_Coord_Array       => 16#8457#,
                                   Secondary_Color_Array => 16#845E#);
   for Client_Side_Capability'Size use Low_Level.Enum'Size;
end GL.Fixed;
