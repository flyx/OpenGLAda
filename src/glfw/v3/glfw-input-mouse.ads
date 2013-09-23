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

package Glfw.Input.Mouse is

   type Button is new Interfaces.C.int range 0 .. 7;

   type Enter_Action is (Leaving, Entering);

   type Cursor_Mode is (Normal, Hidden, Disabled);

   Left_Button   : constant := 0;
   Right_Button  : constant := 1;
   Middle_Button : constant := 2;

   subtype Coordinate is Interfaces.C.double;
   subtype Scroll_Offset is Interfaces.C.double;

private
   for Button'Size use Interfaces.C.int'Size;

   for Enter_Action use (Leaving  => 0,
                         Entering => 1);
   for Enter_Action'Size use C.int'Size;

   for Cursor_Mode use (Normal   => 16#34001#,
                        Hidden   => 16#34002#,
                        Disabled => 16#34003#);
   for Cursor_Mode'Size use Interfaces.C.int'Size;
end Glfw.Input.Mouse;
