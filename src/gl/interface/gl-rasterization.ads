--------------------------------------------------------------------------------
-- Copyright (c) 2014, Felix Krause <contact@flyx.org>
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

package GL.Rasterization is
   pragma Preelaborate;

   use GL.Types;

   subtype Line_Width_Range is Singles.Vector2;

   procedure Set_Line_Width (Value : Single);
   function Line_Width return Single;

   function Aliased_Line_Width_Range return Line_Width_Range;
   function Smooth_Line_Width_Range  return Line_Width_Range;
   function Smooth_Line_Width_Granularity return Single;

end GL.Rasterization;
