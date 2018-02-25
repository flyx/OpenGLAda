
with GL.Objects.Programs;

with Transform_Feedback_API;

package Feedback  is

   procedure Get_Transform_Feedback_Varying
     (Program :  GL.Objects.Programs.Program;
      Index, Buffer_Size, Length, V_Length : Integer;
      V_Type : GL.Objects.Programs.Buffer_Mode; Name : String);
--     procedure Texture_Buffer (Target : GL.Objects.Buffers.Buffer_Target;
--                               Format : GL.Pixels.Internal_Format;
--                               Object : GL.Objects.Buffers.Buffer'Class);
   procedure Transform_Feedback_Varyings
     (Program :  GL.Objects.Programs.Program;
      Count : Integer; Varyings : Transform_Feedback_API.Varyings_Array;
      Buffer_Mode : Transform_Feedback_API.Transform_Buffer_Mode);

end Feedback;
