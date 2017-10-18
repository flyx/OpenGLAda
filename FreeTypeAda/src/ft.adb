--------------------------------------------------------------------------------
-- Copyright (c) 2017, Felix Krause <contact@flyx.org>
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

with FT.Errors;
with FT.API;

package body FT is
   use type Errors.Error_Code;
   use type Library_Ptr;

   procedure Init (Object : in out Library_Reference) is
   begin
      --  make sure a possibly existing reference gets properly disposed of.
      Finalize (Object);
      declare
         Code : constant Errors.Error_Code :=
           API.FT_Init_FreeType (Object.Data'Unchecked_Access);
      begin
         if Code /= Errors.Ok then
            raise FreeType_Exception with "FT.Initialize failed" &
              Errors.Description (Code);
         end if;
      end;
   end Init;

   function Initialized (Object : Library_Reference) return Boolean is
   begin
      return Object.Data /= System.Null_Address;
   end Initialized;

   procedure Adjust (Object : in out Library_Reference) is
   begin
      if Object.Data /= System.Null_Address then
         if API.FT_Reference_Library (Object.Data) /= Errors.Ok then
            --  we cannot raise an exception from inside Adjust; however, since
            --  we checked that the pointer is not null, there should be no
            --  error.
            null;
         end if;
      end if;
   end Adjust;

   procedure Finalize (Object : in out Library_Reference) is
      Ptr : constant Library_Ptr := Object.Data;
   begin
      Object.Data := System.Null_Address;
      if Ptr /= System.Null_Address then
         if API.FT_Done_Library (Ptr) /= Errors.Ok then
            --  we cannot raise an exception from inside Finalize; however,
            --  since we checked that the pointer is not null, there should be
            --  no error.
            null;
         end if;
      end if;
   end Finalize;
end FT;
