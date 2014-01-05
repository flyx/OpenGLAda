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
with GL.Objects.Shaders.Lists;
with GL.Uniforms;

package GL.Objects.Programs is
   pragma Preelaborate;
   
   subtype Subroutine_Index_Type is UInt;
   subtype Uniform_Location_Type is Int range -1 .. Int'Last;
   
   Invalid_Index : constant Subroutine_Index_Type;
   
   type Program is new GL_Object with private;
      
   procedure Attach (Subject : Program; Shader : Shaders.Shader);
   
   procedure Link (Subject : Program);
   
   function Link_Status (Subject : Program) return Boolean;
   
   function Info_Log (Subject : Program) return String;
   
   function Active_Subroutines (Object : Program; Shader : Shaders.Shader_Type)
                                return Size;
   function Active_Subroutine_Uniforms (Object : Program;
                                        Shader : Shaders.Shader_Type) 
                                        return Size;
   function Active_Subroutine_Uniform_Locations (Object : Program;
                                                 Shader : Shaders.Shader_Type)
                                                 return Size;
   function Active_Subroutine_Uniform_Max_Length (Object : Program;
                                                  Shader : Shaders.Shader_Type)
                                                  return Size;
   function Active_Subroutine_Max_Length (Object : Program;
                                          Shader : Shaders.Shader_Type)
                                          return Size;
   
   function Subroutine_Index (Object : Program; Shader : Shaders.Shader_Type;
                              Name   : String)
                              return Subroutine_Index_Type;
   function Subroutine_Uniform_Locations (Object : Program;
                                          Shader : Shaders.Shader_Type;
                                          Name   : String) 
                                          return Uniform_Location_Type;
   
   procedure Use_Program (Subject : Program);
   
   function Uniform_Location (Subject : Program; Name : String)
     return Uniforms.Uniform;
   
   procedure Bind_Attrib_Location (Subject : Program;
                                   Index : Attributes.Attribute;
                                   Name : String);
   function Attrib_Location (Subject : Program; Name : String)
     return Attributes.Attribute;
   
   function Attached_Shaders (Object : Program) return Shaders.Lists.List;
   
   overriding
   procedure Initialize_Id (Object : in out Program);
   
   overriding
   procedure Delete_Id (Object : in out Program);
private
   Invalid_Index : constant Subroutine_Index_Type := 16#FFFFFFFF#;
   
   type Program is new GL_Object with null record;
end GL.Objects.Programs;
