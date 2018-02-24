
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Pixels;
with GL.Types; use  GL.Types;
with Ada.Strings.Unbounded;

package Transform_Feedback_API is


   type Transform_Buffer_Mode is (GL_Interleaved_Attribs, GL_Separate_Attribs);
   for Transform_Buffer_Mode use
     (GL_Interleaved_Attribs => 16#8C8C#,
      GL_Separate_Attribs    => 16#8C8D#);
   --  glTransformFeedbackVaryings (GLuint program, GLsizei count,
   --     const GLchar* const *varyings, GLenum bufferMode);
   --  glGetTransformFeedbackVarying (GLuint program, GLuint index,
   --     GLsizei bufSize, GLsizei *length, GLsizei *size, GLenum *type, GLchar *name);

   type Varyings_Array is array (Integer range <>) of Ada.Strings.Unbounded.Unbounded_String;

   procedure Get_Transform_Feedback_Varying
     (Program :  GL.Objects.Programs.Program;
      Index : Int; Buffer_Size, Length, V_Length : Size;
      V_Type : GL.Objects.Programs.Buffer_Mode; Name : String);
   pragma Import (StdCall, Get_Transform_Feedback_Varying,
                  "glGetTransformFeedbackVarying");

   procedure Tex_Buffer (Target : GL.Objects.Buffers.Buffer_Target;
                             Format : GL.Pixels.Internal_Format;
                             Object : GL.Objects.Buffers.Buffer'Class);
   pragma Import (StdCall, Tex_Buffer, "glTexBuffer");

   procedure Transform_Feedback_Varyings
     (Program :  GL.Objects.Programs.Program;
      Count : Size; Varyings : Varyings_Array;
      Buffer_Mode : Transform_Buffer_Mode);
   pragma Import (StdCall, Transform_Feedback_Varyings,
                  "glTransformFeedbackVaryings");

end Transform_Feedback_API;
