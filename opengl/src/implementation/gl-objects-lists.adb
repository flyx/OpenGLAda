--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

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
