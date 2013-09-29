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

with Ada.Unchecked_Deallocation;

package body GL.Objects is

   overriding procedure Initialize (Object : in out GL_Object) is
   begin
      Object.Reference := new GL_Object_Reference'(GL_Id          => 0,
                                                  Reference_Count => 1,
                                                  Initialized     => False);
   end Initialize;

   overriding procedure Adjust (Object : in out GL_Object) is
   begin
      if Object.Reference /= null then
         Object.Reference.Reference_Count := Object.Reference.Reference_Count + 1;
      end if;
   end Adjust;

   overriding procedure Finalize (Object : in out GL_Object) is
      procedure Free is new Ada.Unchecked_Deallocation
         (Object => GL_Object_Reference, Name => GL_Object_Reference_Access);
   begin
      if Object.Reference /= null then
         Object.Reference.Reference_Count := Object.Reference.Reference_Count - 1;
         if Object.Reference.Reference_Count = 0 then
            if Object.Reference.Initialized then
               Delete_Id (GL_Object'Class (Object));
            end if;
            Free (Object.Reference);
         end if;
      end if;
   end Finalize;
   
   function Initialized (Object : GL_Object) return Boolean is
   begin
      return Object.Reference.Initialized;
   end Initialized;

   function Raw_Id (Object : GL_Object) return UInt is
   begin
      return Object.Reference.GL_Id;
   end Raw_Id;
   
   procedure Set_Raw_Id (Object : GL_Object; Id : UInt) is
   begin
      Object.Reference.GL_Id := Id;
   end Set_Raw_Id;

   function "=" (Left, Right : GL_Object) return Boolean is
   begin
      return Left.Reference = Right.Reference;
   end "=";

end GL.Objects;
