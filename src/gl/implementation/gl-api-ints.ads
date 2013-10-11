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

package GL.API.Ints is
   pragma Preelaborate;
   
   use GL.Types.Ints;
   
   procedure Get_Light_Position (Name   : Enums.Light_Name;
                                 Pname  : Enums.Light_Param;
                                 Target : in out Vector4);
   pragma Import (Convention => StdCall, Entity => Get_Light_Position,
                  External_Name => "glGetLightiv");
   
   procedure Light_Position (Name  : Enums.Light_Name; Pname : Enums.Light_Param;
                             Param : Vector4);
   pragma Import (Convention => StdCall, Entity => Light_Position,
                  External_Name => "glLightiv");
   
   procedure Uniform1 is new Loader.Procedure_With_2_Params
     ("glUniform1i", Uniforms.Uniform, Int);
   
   procedure Uniform1v is new Loader.Procedure_With_3_Params
     ("glUniform1iv", Uniforms.Uniform, Size, Int_Array);
   
   procedure Uniform2 is new Loader.Procedure_With_3_Params
     ("glUniform2i", Uniforms.Uniform, Int, Int);
   
   procedure Uniform2v is new Loader.Procedure_With_3_Params
     ("glUniform2iv", Uniforms.Uniform, Size, Vector2_Array);
   
   procedure Uniform3 is new Loader.Procedure_With_4_Params
     ("glUniform3i", Uniforms.Uniform, Int, Int, Int);
   
   procedure Uniform3v is new Loader.Procedure_With_3_Params
     ("glUniform3iv", Uniforms.Uniform, Size, Vector3_Array);
   
   procedure Uniform4 is new Loader.Procedure_With_5_Params
     ("glUniform4i", Uniforms.Uniform, Int, Int, Int, Int);
   
   procedure Uniform4v is new Loader.Procedure_With_3_Params
     ("glUniform4iv", Uniforms.Uniform, Size, Vector4_Array);    
   
   procedure Uniform_Matrix2 is new Loader.Procedure_With_4_Params
     ("glUniformMatrix2iv", Uniforms.Uniform, Size, Low_Level.Bool,
      Matrix2_Array);
   
   procedure Uniform_Matrix3 is new Loader.Procedure_With_4_Params
     ("glUniformMatrix3iv", Uniforms.Uniform, Size, Low_Level.Bool,
      Matrix3_Array);
   
   procedure Uniform_Matrix4 is new Loader.Procedure_With_4_Params
     ("glUniformMatrix4iv", Uniforms.Uniform, Size, Low_Level.Bool,
      Matrix4_Array);
   
   procedure Vertex_Attrib1 is new Loader.Procedure_With_2_Params
     ("glVertexAttribI1i", Attributes.Attribute, Int);
   
   procedure Vertex_Attrib2 is new Loader.Procedure_With_3_Params
     ("glVertexAttribI2i", Attributes.Attribute, Int, Int);
   
   procedure Vertex_Attrib2v is new Loader.Procedure_With_2_Params
     ("glVertexAttribI2iv", Attributes.Attribute, Vector2);
   
   procedure Vertex_Attrib3 is new Loader.Procedure_With_4_Params
     ("glVertexAttribI3i", Attributes.Attribute, Int, Int, Int);
   
   procedure Vertex_Attrib3v is new Loader.Procedure_With_2_Params
     ("glVertexAttribI3iv", Attributes.Attribute, Vector3);
   
   procedure Vertex_Attrib4 is new Loader.Procedure_With_5_Params
     ("glVertexAttribI4i", Attributes.Attribute, Int, Int, Int, Int);
   
   procedure Vertex_Attrib4v is new Loader.Procedure_With_2_Params
     ("glVertexAttrib4Iiv", Attributes.Attribute, Vector4);
end GL.API.Ints;
