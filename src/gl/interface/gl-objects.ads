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

with Ada.Finalization;

with GL.Types;

private with GL.Low_Level;

package GL.Objects is
   pragma Preelaborate;

   use GL.Types;

   type Access_Kind is (Read_Only, Write_Only, Read_Write);

   type GL_Object is abstract new Ada.Finalization.Controlled with private;

   -- creates the object in OpenGL memory.
   overriding procedure Initialize (Object : in out GL_Object);

   -- Increases reference count.
   overriding procedure Adjust (Object : in out GL_Object);

   -- Decreases reference count. Destroys texture when it reaches zero.
   overriding procedure Finalize (Object : in out GL_Object);

   -- Create an OpenGL ID for this object. This has to be done before
   -- the object is used in any way. After calling this procedure,
   -- Initialized will be true.
   procedure Initialize_Id (Object : in out GL_Object) is abstract;

   -- Deletes the ID of an object. After calling this procedure,
   -- Initialized will be false.
   procedure Delete_Id (Object : in out GL_Object) is abstract;

   -- Check whether the object is set up to be used with OpenGL
   -- (i.e. whether Initialize_Id has been called on the object).
   function Initialized (Object : GL_Object) return Boolean;

   -- This getter is provided for low-level access. Its primary use is to
   -- interact with other C interfaces (e.g. OpenCL)
   function Raw_Id (Object : GL_Object) return UInt;
   -- Setter for low-level access
   procedure Set_Raw_Id (Object : GL_Object; Id : UInt);

   function "=" (Left, Right : GL_Object) return Boolean;

   No_Object_Bound_Exception : exception;
private

   for Access_Kind use (Read_Only  => 16#88B8#,
                        Write_Only => 16#88B9#,
                        Read_Write => 16#88BA#);
   for Access_Kind'Size use Low_Level.Enum'Size;

   type GL_Object_Reference is record
      GL_Id           : UInt;
      Reference_Count : Natural;
      Initialized     : Boolean := True;
   end record;

   type GL_Object_Reference_Access is access all GL_Object_Reference;

   type GL_Object is abstract new Ada.Finalization.Controlled with record
      Reference : GL_Object_Reference_Access;
   end record;

   pragma Inline (Raw_Id);
   pragma Inline (Set_Raw_Id);

end GL.Objects;
