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

with Ada.Strings.Unbounded;

with GL.Types;

package GL.Context is
   pragma Preelaborate;

   use GL.Types;

   type String_List is array (Positive range <>) of
     Ada.Strings.Unbounded.Unbounded_String;

   Null_String_List : constant String_List := (2 .. 1 => <>);

   -- these two require OpenGL 3:
   function Major_Version return Int;
   function Minor_Version return Int;

   -- legacy (deprecated in OpenGL 3)
   function Version_String return String;

   function Vendor return String;

   function Renderer return String;

   -- uses OpenGL 3 interface if available, otherwise old interface
   function Extensions return String_List;
   function Has_Extension (Name : String) return Boolean;

   function Primary_Shading_Language_Version return String;

   -- available since OpenGL 4.3:
   function Supported_Shading_Language_Versions return String_List;
   function Supports_Shading_Language_Version (Name : String) return Boolean;
end GL.Context;
