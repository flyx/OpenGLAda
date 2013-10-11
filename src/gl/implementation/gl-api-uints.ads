--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <contact@flyx.org>
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

with GL.Uniforms;

package GL.API.UInts is
   pragma Preelaborate;

   use GL.Types.UInts;

   procedure Uniform1 is new Loader.Procedure_With_2_Params
     ("glUniform1ui", Uniforms.Uniform, UInt);

   procedure Uniform1v is new Loader.Procedure_With_3_Params
     ("glUniform1uiv", Uniforms.Uniform, Size, UInt_Array);

   procedure Uniform2 is new Loader.Procedure_With_3_Params
     ("glUniform2ui", Uniforms.Uniform, UInt, UInt);

   procedure Uniform2v is new Loader.Procedure_With_3_Params
     ("glUniform2uiv", Uniforms.Uniform, Size, Vector2_Array);

   procedure Uniform3 is new Loader.Procedure_With_4_Params
     ("glUniform3ui", Uniforms.Uniform, UInt, UInt, UInt);

   procedure Uniform3v is new Loader.Procedure_With_3_Params
     ("glUniform3uiv", Uniforms.Uniform, Size, Vector3_Array);

   procedure Uniform4 is new Loader.Procedure_With_5_Params
     ("glUniform4ui", Uniforms.Uniform, UInt, UInt, UInt, UInt);

   procedure Uniform4v is new Loader.Procedure_With_3_Params
     ("glUniform4uiv", Uniforms.Uniform, Size, Vector4_Array);    

   procedure Uniform_Matrix2 is new Loader.Procedure_With_4_Params
     ("glUniformMatrix2uiv", Uniforms.Uniform, Size, Low_Level.Bool,
      Matrix2_Array);

   procedure Uniform_Matrix3 is new Loader.Procedure_With_4_Params
     ("glUniformMatrix3uiv", Uniforms.Uniform, Size, Low_Level.Bool,
      Matrix3_Array);

   procedure Uniform_Matrix4 is new Loader.Procedure_With_4_Params
     ("glUniformMatrix4uiv", Uniforms.Uniform, Size, Low_Level.Bool,
      Matrix4_Array);
   
   procedure Vertex_Attrib1 is new Loader.Procedure_With_2_Params
     ("glVertexAttribI1ui", Attributes.Attribute, UInt);
   
   procedure Vertex_Attrib2 is new Loader.Procedure_With_3_Params
     ("glVertexAttribI2ui", Attributes.Attribute, UInt, UInt);
   
   procedure Vertex_Attrib2v is new Loader.Procedure_With_2_Params
     ("glVertexAttribI2uiv", Attributes.Attribute, Vector2);
   
   procedure Vertex_Attrib3 is new Loader.Procedure_With_4_Params
     ("glVertexAttribI3ui", Attributes.Attribute, UInt, UInt, UInt);
   
   procedure Vertex_Attrib3v is new Loader.Procedure_With_2_Params
     ("glVertexAttribI3uiv", Attributes.Attribute, Vector3);
   
   procedure Vertex_Attrib4 is new Loader.Procedure_With_5_Params
     ("glVertexAttribI4ui", Attributes.Attribute, UInt, UInt, UInt, UInt);
   
   procedure Vertex_Attrib4v is new Loader.Procedure_With_2_Params
     ("glVertexAttrib4Iuiv", Attributes.Attribute, Vector4);
end GL.API.UInts;
