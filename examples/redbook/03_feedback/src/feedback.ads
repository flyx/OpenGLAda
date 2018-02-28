
with GL.Objects.Programs;
with GL.Types;

with Transform_Feedback_API;

package Feedback  is

   procedure Begin_Transform_Feedback (Primitive_Mode : GL.Types.Connection_Mode);
   procedure End_Transform_Feedback;
   procedure Get_Transform_Feedback_Varying
     (Program :  GL.Objects.Programs.Program;
      Index, Buffer_Size, Length, V_Length : Integer;
      V_Type : GL.Objects.Programs.Buffer_Mode; Name : String);
   procedure Transform_Feedback_Varyings
     (Program :  GL.Objects.Programs.Program;
      Count : Integer; Varyings : Transform_Feedback_API.Varyings_Array;
      Buffer_Mode : GL.Objects.Programs.Buffer_Mode);

end Feedback;
