--------------------------------------------------------------------------------
-- Copyright (c) 2013, Felix Krause <contact@flyx.org>
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

private with GL.Low_Level;

package GL.Objects.Shaders is
   pragma Preelaborate;
   
   type Shader_Type is (Fragment_Shader, Vertex_Shader, Geometry_Shader,
                        Tess_Evaluation_Shader, Tess_Control_Shader);
   
   type Shader (Kind : Shader_Type) is new GL_Object with private;
   
   procedure Set_Source (Subject : Shader; Source : String);
   function Source (Subject : Shader) return String;
   
   procedure Compile (Subject : Shader);
   
   procedure Release_Shader_Compiler;
   
   function Compile_Status (Subject : Shader) return Boolean;
   
   function Info_Log (Subject : Shader) return String;
   
   overriding
   procedure Initialize_Id (Object : in out Shader);
   
   overriding
   procedure Delete_Id (Object : in out Shader);
   
   -- low-level
   function Create_From_Id (Id : UInt) return Shader;
private
   type Shader (Kind : Shader_Type) is new GL_Object with null record;

   for Shader_Type use (Fragment_Shader        => 16#8B30#,
                        Vertex_Shader          => 16#8B31#,
                        Geometry_Shader        => 16#8DD9#,
                        Tess_Evaluation_Shader => 16#8E87#,
                        Tess_Control_Shader    => 16#8E88#);
   for Shader_Type'Size use Low_Level.Enum'Size;
   
end GL.Objects.Shaders;
