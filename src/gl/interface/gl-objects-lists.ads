--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

generic
   type Object_Type (<>) is new GL_Object with private;
   with function Generate_From_Id (Id : UInt) return Object_Type;
package GL.Objects.Lists is
   pragma Preelaborate;

   type List (<>) is tagged private;

   type Cursor is private;

   No_Element : constant Cursor;

   function Create (Raw : UInt_Array) return List;

   function First (Object : List) return Cursor;
   function Last  (Object : List) return Cursor;

   function Next     (Current : Cursor) return Cursor;
   function Previous (Current : Cursor) return Cursor;

   function Has_Next     (Current : Cursor) return Boolean;
   function Has_Previous (Current : Cursor) return Boolean;

   function Element (Current : Cursor) return Object_Type;

private
   type List (Count : Size) is tagged record
      Contents : UInt_Array (1 .. Count);
   end record;

   type List_Access is access constant List;

   type Cursor is record
      Object : List_Access;
      Index  : Size;
   end record;

   No_Element : constant Cursor := Cursor'(null, 0);

end GL.Objects.Lists;
