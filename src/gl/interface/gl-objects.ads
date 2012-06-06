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

with GL.Types;

package GL.Objects is
   pragma Preelaborate;
   
   use GL.Types;
   
   type GL_Object is abstract new Ada.Finalization.Controlled with private;
         
   -- creates the object in OpenGL memory.
   overriding procedure Initialize (Object : in out GL_Object);
      
   -- Increases reference count.
   overriding procedure Adjust (Object : in out GL_Object);
   
   -- Decreases reference count. Destroys texture when it reaches zero.
   overriding procedure Finalize (Object : in out GL_Object);
   
   -- This getter is provided for low-level access. Its primary use is to
   -- interact with other C interfaces (e.g. OpenCL)
   function Raw_Id (Object : GL_Object) return UInt;
   
   function "=" (Left, Right : GL_Object) return Boolean;
   
   No_Object_Bound_Exception : exception;
private
   
   -- This method should be overridden by child classes to create
   -- IDs for the OpenGL object they implement
   procedure Create_Id (Object : in out GL_Object) is null;
   
   -- See above - this method should delete the object-specific id
   procedure Delete_Id (Object : in out GL_Object) is null;
   
   type GL_Object_Reference is record
      GL_Id           : UInt;
      Reference_Count : Natural;
   end record;
   
   type GL_Object_Reference_Access is access all GL_Object_Reference;
   
   type GL_Object is abstract new Ada.Finalization.Controlled with record
      Reference : GL_Object_Reference_Access;
   end record;
   
   -- cannot be constant because code expects to be able to modify the
   -- Reference value
   Null_Object_Reference : aliased GL_Object_Reference
     := GL_Object_Reference'(GL_Id => 0, Reference_Count => 0);
   
end GL.Objects;