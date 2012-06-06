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

package GL.Objects.Vertex_Arrays is
   pragma Preelaborate;
   
   type Vertex_Array_Object is new GL_Object with private;
   
   procedure Bind (Object : Vertex_Array_Object);
   
   procedure Draw_Arrays (Mode : Connection_Mode; First, Count : Natural);
   
   -- bind this object to unbind the current array object.
   Null_Array_Object : constant Vertex_Array_Object;
private
   type Vertex_Array_Object is new GL_Object with null record;
   
   procedure Create_Id (Object : in out Vertex_Array_Object);
   
   procedure Delete_Id (Object : in out Vertex_Array_Object);
   
   Null_Array_Object : constant Vertex_Array_Object
     := Vertex_Array_Object'(Ada.Finalization.Controlled with
        Reference => null);
   
end GL.Objects.Vertex_Arrays;