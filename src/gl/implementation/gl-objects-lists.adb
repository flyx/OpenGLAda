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

package body GL.Objects.Lists is

   function Create (Raw : UInt_Array) return List is
   begin
      return List'(Count => Raw'Length, Contents => Raw);
   end Create;

   function First (Object : List) return Cursor is
   begin
      if Object.Count = 0 then
         return No_Element;
      else
         return Cursor'(Object => Object'Unchecked_Access, Index => 1);
      end if;
   end First;

   function Last (Object : List) return Cursor is
   begin
      if Object.Count = 0 then
         return No_Element;
      else
         return Cursor'(Object => Object'Unchecked_Access,
                        Index  => Object.Contents'Length);
      end if;
   end Last;

   function Next (Current : Cursor) return Cursor is
   begin
      if Current = No_Element then
         raise Constraint_Error;
      elsif Current.Index = Current.Object.Contents'Length then
         return No_Element;
      else
         return Cursor'(Current.Object, Current.Index + 1);
      end if;
   end Next;

   function Previous (Current : Cursor) return Cursor is
   begin
      if Current = No_Element then
         raise Constraint_Error;
      elsif Current.Index = 1 then
         return No_Element;
      else
         return Cursor'(Current.Object, Current.Index - 1);
      end if;
   end Previous;

   function Has_Next (Current : Cursor) return Boolean is
   begin
      return Current /= No_Element and then
        Current.Index /= Current.Object.Contents'Length;
   end Has_Next;

   function Has_Previous (Current : Cursor) return Boolean is
   begin
      return Current /= No_Element and then Current.Index /= 1;
   end Has_Previous;

   function Element (Current : Cursor) return Object_Type is
   begin
      if Current = No_Element then
         raise Constraint_Error;
      else
         return Generate_From_Id (Current.Object.Contents (Current.Index));
      end if;
   end Element;

end GL.Objects.Lists;
