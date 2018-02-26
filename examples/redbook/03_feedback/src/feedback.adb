
with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;

package body Feedback is
   use GL.Types;


   procedure Begin_Transform_Feedback (Primitive_Mode : GL.Types.Connection_Mode) is
   begin
      Transform_Feedback_API.Begin_Transform_Feedback (Primitive_Mode);
   end Begin_Transform_Feedback;

   --  -------------------------------------------------------------------------

   procedure End_Transform_Feedback is
   begin
      Transform_Feedback_API.End_Transform_Feedback;
   end End_Transform_Feedback;

   --  -------------------------------------------------------------------------

     procedure Get_Transform_Feedback_Varying
     (Program :  GL.Objects.Programs.Program;
      Index, Buffer_Size, Length, V_Length : Integer;
      V_Type : GL.Objects.Programs.Buffer_Mode; Name : String) is
   begin
      Transform_Feedback_API.Get_Transform_Feedback_Varying
        (Program, Int (Index), Size (Buffer_Size), Size (Length),
         Size (V_Length), V_Type, Name);

   exception
      when  others =>
         Put_Line ("An exception occurred in Feedback.Get_Transform_Feedback_Varying.");
         raise;
   end Get_Transform_Feedback_Varying;

   --  -------------------------------------------------------------------------

--     procedure Texture_Buffer (Target : GL.Objects.Buffers.Buffer_Target;
--                               Format : GL.Pixels.Internal_Format;
--                               Object : GL.Objects.Buffers.Buffer'Class) is
--     begin
--        Transform_Feedback_API.Tex_Buffer (Target, Format, Object);
--     end Texture_Buffer;

   --  -------------------------------------------------------------------------

   procedure Transform_Feedback_Varyings
     (Program :  GL.Objects.Programs.Program;
      Count : Integer; Varyings : Transform_Feedback_API.Varyings_Array;
      Buffer_Mode : Transform_Feedback_API.Transform_Buffer_Mode) is
   begin
--        Transform_Feedback_API.Transform_Feedback_Varyings
--          (Program, Size (Count), Varyings, Buffer_Mode);
        null;
   exception
      when  others =>
         Put_Line ("An exception occurred in Feedback.Transform_Feedback_Varyings.");
         raise;
   end Transform_Feedback_Varyings;

   --  -------------------------------------------------------------------------

end Feedback;
