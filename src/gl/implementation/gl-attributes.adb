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

with GL.API;
with GL.Low_Level;

package body GL.Attributes is
   procedure Set_Vertex_Attrib_Pointer (Index  : Attribute;
                                        Size   : Component_Count;
                                        Kind   : Numeric_Type;
                                        Stride, Offset : Natural) is
   begin
      case Kind is
         when Single_Type => 
            API.Vertex_Attrib_Pointer (Index, Size, Kind, Low_Level.False,
                                       Low_Level.SizeI (Stride),
                                       Int (Offset));
         when Double_Type =>
            API.Vertex_AttribL_Pointer (Index, Size, Kind,
                                        Low_Level.SizeI (Stride),
                                        Int (Offset));
         when others =>
            API.Vertex_AttribI_Pointer (Index, Size, Kind,
                                        Low_Level.SizeI (Stride),
                                        Int (Offset));
      end case;
      Check_OpenGL_Error;
   end Set_Vertex_Attrib_Pointer;

   procedure Enable_Vertex_Attrib_Array  (Index : Attribute) renames
     API.Enable_Vertex_Attrib_Array;
   
   procedure Disable_Vertex_Attrib_Array (Index : Attribute) renames
     API.Disable_Vertex_Attrib_Array;

end GL.Attributes;