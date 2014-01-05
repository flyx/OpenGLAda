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

with Interfaces.C.Pointers;

package GL.Types.Colors is
   pragma Preelaborate;
      
   type Color_Index is (R, G, B, A);
   subtype Basic_Color_Index is Color_Index range R .. B;

   subtype Component is Single range 0.0 .. 1.0;

   type Color is array (Color_Index) of aliased Component;
   type Basic_Color is array (Basic_Color_Index) of Component;
   
   pragma Convention (C, Color);
   pragma Convention (C, Basic_Color);
   
   type Color_Array is array (Size range <>) of aliased Color;
   type Basic_Color_Array is array (Size range <>) of aliased Basic_Color;
   
   package Color_Pointers is new Interfaces.C.Pointers
     (Size, Color, Color_Array, Color'(others => 0.0));
   package Basic_Color_Pointers is new Interfaces.C.Pointers
     (Size, Basic_Color, Basic_Color_Array, Basic_Color'(others => 0.0));
end GL.Types.Colors;
