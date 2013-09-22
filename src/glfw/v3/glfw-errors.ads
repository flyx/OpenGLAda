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

package Glfw.Errors is
   type Kind is (Not_Initialized,
                 No_Current_Context,
                 Invalid_Enum,
                 Invalid_Value,
                 Out_Of_Memory,
                 API_Unavailable,
                 Version_Unavailable,
                 Platform_Error,
                 Format_Unavailable);
   for Kind use (Not_Initialized => 16#00010001#,
                 No_Current_Context  => 16#00010002#,
                 Invalid_Enum        => 16#00010003#,
                 Invalid_Value       => 16#00010004#,
                 Out_Of_Memory       => 16#00010005#,
                 API_Unavailable     => 16#00010006#,
                 Version_Unavailable => 16#00010007#,
                 Platform_Error      => 16#00010008#,
                 Format_Unavailable  => 16#00010009#);
   for Kind'Size use Interfaces.C.int'Size;

   type Callback is access procedure (Error : Kind; Description : String);

   procedure Set_Callback (Handler : Callback);
end Glfw.Errors;
