--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Ada.Finalization;

with GL.Types;

private with GL.Low_Level;

package GL.Objects is
   pragma Preelaborate;

   use GL.Types;

   type Access_Kind is (Read_Only, Write_Only, Read_Write);

   type GL_Object is abstract tagged private;

   -- Create an OpenGL object for this reference. This has to be done before
   -- the object is used in any way. After calling this procedure,
   -- Initialized will be true.
   procedure Initialize_Id (Object : in out GL_Object);

   -- Removes the reference to the backing OpenGL object from this reference.
   -- the OpenGL object will only be deleted if this was the last reference.
   procedure Clear (Object : in out GL_Object);

   -- Check whether the object is set up to be used with OpenGL
   -- (i.e. whether Initialize_Id has been called on the object).
   function Initialized (Object : GL_Object) return Boolean;

   -- This getter is provided for low-level access. Its primary use is to
   -- interact with other C interfaces (e.g. OpenCL)
   function Raw_Id (Object : GL_Object) return UInt;

   -- Setter for low-level access. This decreases the reference count of the
   -- previously referred OpenGL object.
   procedure Set_Raw_Id (Object : in out GL_Object; Id : UInt;
                         Owned : Boolean := True);

   function "=" (Left, Right : GL_Object) return Boolean;

   -- abstract subprograms are required to be visible. however, these
   -- subprograms are not intended to be called from ouside!

   -- Create new OpenGL object of the corresponding type and return its ID
   procedure Internal_Create_Id
     (Object : GL_Object; Id : out UInt) is abstract;

   -- Tell OpenGL the object referred to be the given ID is no longer needed
   procedure Internal_Release_Id
     (Object : GL_Object; Id : UInt) is abstract;

   No_Object_Bound_Exception : exception;
private
   for Access_Kind use (Read_Only  => 16#88B8#,
                        Write_Only => 16#88B9#,
                        Read_Write => 16#88BA#);
   for Access_Kind'Size use Low_Level.Enum'Size;

   type GL_Object_Reference;
   type GL_Object_Reference_Access is access all GL_Object_Reference;

   type GL_Object_Reference is record
      GL_Id           : UInt;
      Reference_Count : Natural;
      Is_Owner        : Boolean;
   end record;

   type GL_Object is abstract new Ada.Finalization.Controlled with record
      Reference : GL_Object_Reference_Access := null;
   end record;

   -- Increases reference count.
   overriding procedure Adjust (Object : in out GL_Object);

   -- Decreases reference count. Destroys texture when it reaches zero.
   overriding procedure Finalize (Object : in out GL_Object);

   pragma Inline (Raw_Id);
   pragma Inline (Set_Raw_Id);

   Reference_To_Null_Object : aliased GL_Object_Reference :=
     (GL_Id => 0, Reference_Count => 0, Is_Owner => False);
end GL.Objects;
