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

with GL.API;
with GL.Enums;

with Interfaces.C.Strings;

package body GL.Objects.Programs is
   procedure Attach(Subject : Program; Shader : Shaders.Shader) is
   begin
      API.Attach_Shader (Subject.Reference.GL_Id, Shader.Raw_Id);
      Raise_Exception_On_OpenGL_Error;
   end Attach;

   procedure Link (Subject : Program) is
   begin
      API.Link_Program (Subject.Reference.GL_Id);
      Raise_Exception_On_OpenGL_Error;
   end Link;

   function Link_Status (Subject : Program) return Boolean is
      Status_Value : Int := 0;
   begin
      API.Get_Program_Param (Subject.Reference.GL_Id, Enums.Link_Status,
                             Status_Value);
      Raise_Exception_On_OpenGL_Error;
      return Status_Value /= 0;
   end Link_Status;

   function Info_Log (Subject : Program) return String is
      Log_Length : Int := 0;
   begin
      API.Get_Program_Param (Subject.Reference.GL_Id, Enums.Info_Log_Length,
                             Log_Length);
      Raise_Exception_On_OpenGL_Error;
      -- Returned length includes null termination character
      Log_Length := Log_Length - 1;
      declare
         Info_Log : String (1 .. Integer (Log_Length));
         -- do not care that string does not get initialized
         pragma Warnings (Off, Info_Log);
         C_Info_Log : C.Strings.chars_ptr := C.Strings.New_String (Info_Log);
         Actual_Length : Size := 0;
      begin
         API.Get_Program_Info_Log (Subject.Reference.GL_Id,
                                   Log_Length + 1,
                                   Actual_Length, C_Info_Log);
         Raise_Exception_On_OpenGL_Error;
         Info_Log := C.Strings.Value (C_Info_Log, C.size_t (Actual_Length));
         C.Strings.Free (C_Info_Log);
         return Info_Log;
      end;
   end Info_Log;
   
   function Active_Subroutines (Object : Program; Shader : Shaders.Shader_Type)
                                return Size is
      Ret : Size := 0;
   begin
      API.Get_Program_Stage (Object.Reference.GL_Id, Shader,
                             Enums.Active_Subroutines, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Active_Subroutines;
   
   function Active_Subroutine_Uniforms (Object : Program;
                                        Shader : Shaders.Shader_Type) 
                                        return Size is
      Ret : Size := 0;
   begin
      API.Get_Program_Stage (Object.Reference.GL_Id, Shader,
                             Enums.Active_Subroutine_Uniforms, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Active_Subroutine_Uniforms;
   
   function Active_Subroutine_Uniform_Locations (Object : Program;
                                                 Shader : Shaders.Shader_Type)
                                                 return Size is
      Ret : Size := 0;
   begin
      API.Get_Program_Stage (Object.Reference.GL_Id, Shader,
                             Enums.Active_Subroutine_Uniform_Locations, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Active_Subroutine_Uniform_Locations;
      
   function Active_Subroutine_Uniform_Max_Length (Object : Program;
                                                  Shader : Shaders.Shader_Type)
                                                  return Size is
      Ret : Size := 0;
   begin
      API.Get_Program_Stage (Object.Reference.GL_Id, Shader,
                             Enums.Active_Subroutine_Uniform_Max_Length, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Active_Subroutine_Uniform_Max_Length;
   
   function Active_Subroutine_Max_Length (Object : Program;
                                          Shader : Shaders.Shader_Type)
                                          return Size is
      Ret : Size := 0;
   begin
      API.Get_Program_Stage (Object.Reference.GL_Id, Shader,
                             Enums.Active_Subroutine_Max_Length, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Active_Subroutine_Max_Length;
   
   function Subroutine_Index (Object : Program; Shader : Shaders.Shader_Type;
                              Name   : String)
                              return Subroutine_Index_Type is
      C_String : constant Interfaces.C.char_array
        := Interfaces.C.To_C (Name);
   begin
      return Index : constant Subroutine_Index_Type := API.Get_Subroutine_Index
        (Object.Reference.GL_Id, Shader, C_String) do
         Raise_Exception_On_OpenGL_Error;
      end return;
   end Subroutine_Index;
   
   function Subroutine_Uniform_Locations (Object : Program;
                                          Shader : Shaders.Shader_Type;
                                          Name   : String) 
                                          return Uniform_Location_Type is
      C_String : constant Interfaces.C.char_array
        := Interfaces.C.To_C (Name);
   begin
      return Index : constant Uniform_Location_Type
        := API.Get_Subroutine_Uniform_Location
        (Object.Reference.GL_Id, Shader, C_String) do
         Raise_Exception_On_OpenGL_Error;
      end return;
   end Subroutine_Uniform_Locations;
      
   procedure Use_Program (Subject : Program) is
   begin
      API.Use_Program (Subject.Reference.GL_Id);
      Raise_Exception_On_OpenGL_Error;
   end Use_Program;

   procedure Initialize_Id (Object : in out Program) is
   begin
      Object.Reference.GL_Id := API.Create_Program;
      Object.Reference.Initialized := True;
   end Initialize_Id;

   procedure Delete_Id (Object : in out Program) is
   begin
      API.Delete_Program (Object.Reference.GL_Id);
      Raise_Exception_On_OpenGL_Error;
      Object.Reference.GL_Id := 0;
      Object.Reference.Initialized := False;
   end Delete_Id;
   
   function Uniform_Location (Subject : Program; Name : String)
     return Uniforms.Uniform is
      Result : constant Uniforms.Uniform := API.Get_Uniform_Location
        (Subject.Reference.GL_Id, Interfaces.C.To_C (Name));
   begin
      Raise_Exception_On_OpenGL_Error;
      return Result;
   end Uniform_Location;
   
   procedure Bind_Attrib_Location (Subject : Program;
                                   Index : Attributes.Attribute;
                                   Name : String) is
   begin
      API.Bind_Attrib_Location (Subject.Reference.GL_Id, Index,
                               Interfaces.C.To_C (Name));
      Raise_Exception_On_OpenGL_Error;
   end Bind_Attrib_Location;
      
   function Attrib_Location (Subject : Program; Name : String)
     return Attributes.Attribute is
      Location : constant Attributes.Attribute := API.Get_Attrib_Location
        (Subject.Reference.GL_Id, Interfaces.C.To_C (Name));
   begin
      Raise_Exception_On_OpenGL_Error;
      return Location;
   end Attrib_Location;
   
   function Attached_Shaders (Object : Program) return Shaders.Lists.List is
      Shader_Count : aliased Int := 0;
   begin
      API.Get_Program_Param (Object.Reference.GL_Id, Enums.Attached_Shaders,
                             Shader_Count);
      Raise_Exception_On_OpenGL_Error;
      
      return List : constant Shaders.Lists.List := Shaders.Lists.Create
        (API.Get_Attached_Shaders (Object.Reference.GL_Id,
                                   Size (Shader_Count))) do
         Raise_Exception_On_OpenGL_Error;
      end return;
   end Attached_Shaders;
      
end GL.Objects.Programs;
