--------------------------------------------------------------------------------
-- Copyright (c) 2013, Felix Krause <contact@flyx.org>
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

private with GL.Low_Level;

package GL.Culling is
   pragma Preelaborate;

   type Orientation is (Clockwise, Counter_Clockwise);

   type Face_Selector is (Front, Back, Front_And_Back);

   procedure Set_Front_Face (Face : Orientation);
   function Front_Face return Orientation;

   procedure Set_Cull_Face (Selector : Face_Selector);
   function Cull_Face return Face_Selector;

private
   for Orientation use (Clockwise => 16#0900#, Counter_Clockwise => 16#0901#);
   for Orientation'Size use Low_Level.Enum'Size;

   for Face_Selector use (Front          => 16#0404#,
                          Back           => 16#0405#,
                          Front_And_Back => 16#0408#);
   for Face_Selector'Size use Low_Level.Enum'Size;

   pragma Convention (StdCall, Set_Cull_Face);
   pragma Convention (StdCall, Set_Front_Face);
end GL.Culling;
