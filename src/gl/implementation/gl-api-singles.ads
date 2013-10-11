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

with GL.Attributes;
with GL.Uniforms;

package GL.API.Singles is
   pragma Preelaborate;
   
   use GL.Types.Singles;
   
   procedure Get_Light_Position (Name   : Enums.Light_Name;
                                 Pname  : Enums.Light_Param;
                                 Target : in out Vector4);
   pragma Import (Convention => StdCall, Entity => Get_Light_Position,
                  External_Name => "glGetLightfv");
   
   procedure Get_Light_Direction (Name   : Enums.Light_Name;
                                  Pname  : Enums.Light_Param;
                                  Target : in out Vector3);
   pragma Import (Convention => StdCall, Entity => Get_Light_Direction,
                  External_Name => "glGetLightfv");
   
   procedure Light_Position (Name  : Enums.Light_Name; Pname : Enums.Light_Param;
                             Param : Vector4);
   pragma Import (Convention => StdCall, Entity => Light_Position,
                  External_Name => "glLightfv");
   
   procedure Light_Direction (Name  : Enums.Light_Name; Pname : Enums.Light_Param;
                              Param : Vector3);
   pragma Import (Convention => StdCall, Entity => Light_Direction,
                  External_Name => "glLightfv");
   
   procedure Uniform1 is new Loader.Procedure_With_2_Params
     ("glUniform1f", Uniforms.Uniform, Single);
   
   procedure Uniform1v is new Loader.Procedure_With_3_Params
     ("glUniform1fv", Uniforms.Uniform, Size, Single_Array);
   
   procedure Uniform2 is new Loader.Procedure_With_3_Params
     ("glUniform2f", Uniforms.Uniform, Single, Single);
   
   procedure Uniform2v is new Loader.Procedure_With_3_Params
     ("glUniform2fv", Uniforms.Uniform, Size, Vector2_Array);
   
   procedure Uniform3 is new Loader.Procedure_With_4_Params
     ("glUniform3f", Uniforms.Uniform, Single, Single, Single);
   
   procedure Uniform3v is new Loader.Procedure_With_3_Params
     ("glUniform3fv", Uniforms.Uniform, Size, Vector3_Array);
   
   procedure Uniform4 is new Loader.Procedure_With_5_Params
     ("glUniform4f", Uniforms.Uniform, Single, Single, Single, Single);
   
   procedure Uniform4v is new Loader.Procedure_With_3_Params
     ("glUniform4fv", Uniforms.Uniform, Size, Vector4_Array);    
   
   procedure Uniform_Matrix2 is new Loader.Procedure_With_4_Params
     ("glUniformMatrix2fv", Uniforms.Uniform, Size, Low_Level.Bool,
      Matrix2_Array);
   
   procedure Uniform_Matrix3 is new Loader.Procedure_With_4_Params
     ("glUniformMatrix3fv", Uniforms.Uniform, Size, Low_Level.Bool,
      Matrix3_Array);
   
   procedure Uniform_Matrix4 is new Loader.Procedure_With_4_Params
     ("glUniformMatrix4fv", Uniforms.Uniform, Size, Low_Level.Bool,
      Matrix4_Array);

   procedure Vertex_Attrib1 is new Loader.Procedure_With_2_Params
     ("glVertexAttrib1f", Attributes.Attribute, Single);

   procedure Vertex_Attrib2 is new Loader.Procedure_With_3_Params
     ("glVertexAttrib2f", Attributes.Attribute, Single, Single);
   
   procedure Vertex_Attrib2v is new Loader.Procedure_With_2_Params
     ("glVertexAttrib2fv", Attributes.Attribute, Vector2);
   
   procedure Vertex_Attrib3 is new Loader.Procedure_With_4_Params
     ("glVertexAttrib3f", Attributes.Attribute, Single, Single, Single);
   
   procedure Vertex_Attrib3v is new Loader.Procedure_With_2_Params
     ("glVertexAttrib3fv", Attributes.Attribute, Vector3);
   
   procedure Vertex_Attrib4 is new Loader.Procedure_With_5_Params
     ("glVertexAttrib4f", Attributes.Attribute, Single, Single, Single, Single);
   
   procedure Vertex_Attrib4v is new Loader.Procedure_With_2_Params
     ("glVertexAttrib4fv", Attributes.Attribute, Vector4);
end GL.API.Singles;
