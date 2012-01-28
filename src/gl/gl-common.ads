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

with GL.Low_Level;

package GL.Common is
   
   type Compare_Function is (Never, Less, Equal, LEqual, Greater, Not_Equal,
                             GEqual, Always);
   
private

   for Compare_Function use (Never     => 16#0200#,
                             Less      => 16#0201#,
                             Equal     => 16#0202#,
                             LEqual    => 16#0203#,
                             Greater   => 16#0204#,
                             Not_Equal => 16#0205#,
                             GEqual    => 16#0206#,
                             Always    => 16#0207#);
   for Compare_Function'Size use Low_Level.Enum'Size;
   
end GL.Common;