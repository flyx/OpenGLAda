--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Ada.Unchecked_Deallocation;

package body GL.Objects is

   overriding procedure Initialize (Object : in out GL_Object) is
   begin
      Object.Reference :=
        new GL_Object_Reference'(GL_Id => 0, Reference_Count => 1,
                                 Initialized => Uninitialized,
                                 Destructor => null);
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
      Reference : GL_Object_Reference_Access := Object.Reference;
   begin
      Object.Reference := null;
      if Reference /= null then
         Reference.Reference_Count := Reference.Reference_Count - 1;
         if Reference.Reference_Count = 0 then
            if Reference.Initialized = Allocated then
               begin
                  Reference.Destructor (Reference);
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
      return Object.Reference.Initialized /= Uninitialized;
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

   procedure Delete_Id (Object : in out GL_Object) is
   begin
      if Object.Reference /= null and then Object.Reference.Destructor /= null
        then
         Object.Reference.Destructor (Object.Reference);
      end if;
   end Delete_Id;

end GL.Objects;
