--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Ada.Unchecked_Deallocation;

package body GL.Objects is

   procedure Initialize_Id (Object : in out GL_Object) is
      New_Id : UInt;
   begin
      -- may raise exception; therefore we call it before actually making
      -- changes to the holder.
      GL_Object'Class (Object).Internal_Create_Id (New_Id);

      Object.Clear;
      Object.Reference :=
        new GL_Object_Reference'(GL_Id => New_Id, Reference_Count => 1,
                                 Is_Owner => True);
   end Initialize_Id;

   overriding procedure Adjust (Object : in out GL_Object) is
   begin
      if Object.Reference /= null and then
        Object.Reference.Reference_Count > 0 then
         Object.Reference.Reference_Count :=
           Object.Reference.Reference_Count + 1;
      end if;
   end Adjust;

   overriding procedure Finalize (Object : in out GL_Object) is
      procedure Free is new Ada.Unchecked_Deallocation
         (Object => GL_Object_Reference, Name => GL_Object_Reference_Access);
      Reference : GL_Object_Reference_Access := Object.Reference;
   begin
      Object.Reference := null;
      if Reference /= null and then Reference.Reference_Count > 0 then
         --  Reference_Count = 0 means that the holder recides in global memory
         Reference.Reference_Count := Reference.Reference_Count - 1;
         if Reference.Reference_Count = 0 then
            if Reference.Is_Owner then
               begin
                  GL_Object'Class (Object).Internal_Release_Id
                    (Reference.GL_Id);
               exception
                  when others =>
                     --  cannot let this escape as we're in a Finalize call and
                     --  thus that error cannot be properly catched. Chances are
                     --  that if the destructor fails, the context already has
                     --  vanished and thus we do not need to worry about
                     --  anything.
                     null;
               end;
            end if;
            Free (Reference);
         end if;
      end if;
   end Finalize;

   function Initialized (Object : GL_Object) return Boolean is
   begin
      return Object.Reference /= null;
   end Initialized;

   function Raw_Id (Object : GL_Object) return UInt is
   begin
      return Object.Reference.GL_Id;
   end Raw_Id;

   procedure Set_Raw_Id (Object : in out GL_Object; Id : UInt;
                         Owned : Boolean := True) is
   begin
      Object.Finalize;
      --  must create a new holder object for this ID. therefore, we are
      --  dropping the reference to the old ID.

      Object.Reference :=
        new GL_Object_Reference'(GL_Id => Id, Reference_Count => 1,
                                 Is_Owner => Owned);
   end Set_Raw_Id;

   function "=" (Left, Right : GL_Object) return Boolean is
   begin
      return Left.Reference = Right.Reference;
   end "=";

   procedure Clear (Object : in out GL_Object) renames Finalize;

end GL.Objects;
