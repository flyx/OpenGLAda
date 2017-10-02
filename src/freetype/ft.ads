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

with Ada.Finalization;

with System;

with Interfaces.C;

package FT is
   pragma Preelaborate;

   --  reference-counted smart pointer
   type Library_Reference is new Ada.Finalization.Controlled with private;

   subtype Fixed is Interfaces.C.long;
   subtype ULong is Interfaces.C.unsigned_long;

   FreeType_Exception : exception;

   --  instantiates a new library object and makes the given reference point to
   --  that object.
   procedure Init (Object : in out Library_Reference);

   --  true iff the reference points to a valid library object (i.e. has been
   --  initialized).
   function Initialized (Object : Library_Reference) return Boolean;

   --  you may call this manually if you want to make the given reference
   --  uninitialized. the actual object it pointed to will only be deallocated
   --  if the reference count reaches zero.
   --
   --  post-condition : Object.Initialized = False
   overriding procedure Finalize (Object : in out Library_Reference);
private
   subtype Library_Ptr is System.Address;

   type Library_Reference is new Ada.Finalization.Controlled with record
      Data : Library_Ptr;
   end record;

   overriding procedure Adjust (Object : in out Library_Reference);
end FT;
