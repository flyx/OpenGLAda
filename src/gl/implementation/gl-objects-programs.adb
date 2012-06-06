--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <flyx@isobeef.org>
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
with GL.Low_Level;

with Interfaces.C.Strings;

package body GL.Objects.Programs is
   procedure Attach(Subject : Program; Shader : Shaders.Shader) is
   begin
      API.Attach_Shader (Subject.Reference.GL_Id, Shader.Raw_Id);
      Check_OpenGL_Error;
   end Attach;

   procedure Link (Subject : Program) is
   begin
      API.Link_Program (Subject.Reference.GL_Id);
      Check_OpenGL_Error;
   end Link;

   function Link_Status (Subject : Program) return Boolean is
      Status_Value : Int := 0;
   begin
      API.Get_Program_Param (Subject.Reference.GL_Id, Enums.Link_Status,
                             Status_Value);
      return Status_Value /= 0;
   end Link_Status;

   function Info_Log (Subject : Program) return String is
      Log_Length : Int := 0;
   begin
      API.Get_Program_Param (Subject.Reference.GL_Id, Enums.Info_Log_Length,
                             Log_Length);
      -- Returned length includes null termination character
      Log_Length := Log_Length - 1;
      declare
         Info_Log : String (1 .. Integer (Log_Length));
         -- do not care that string does not get initialized
         pragma Warnings (Off, Info_Log);
         C_Info_Log : C.Strings.chars_ptr := C.Strings.New_String (Info_Log);
         Actual_Length : Low_Level.SizeI := 0;
      begin
         API.Get_Program_Info_Log (Subject.Reference.GL_Id,
                                   Low_Level.SizeI (Log_Length + 1),
                                   Actual_Length, C_Info_Log);
         Info_Log := C.Strings.Value (C_Info_Log, C.size_t (Actual_Length));
         C.Strings.Free (C_Info_Log);
         return Info_Log;
      end;
   end Info_Log;
   
   procedure Use_Program (Subject : Program) is
   begin
      API.Use_Program (Subject.Reference.GL_Id);
   end Use_Program;

   procedure Create_Id (Object : in out Program) is
   begin
      Object.Reference.GL_Id := API.Create_Program;
   end Create_Id;

   procedure Delete_Id (Object : in out Program) is
   begin
      API.Delete_Program (Object.Reference.GL_Id);
   end Delete_Id;
   
   function Uniform_Location (Subject : Program; Name : String)
     return Uniforms.Uniform is
      C_Name : C.Strings.chars_ptr := C.Strings.New_String (Name);
      Result : Uniforms.Uniform :=
        API.Get_Uniform_Location (Subject.Reference.GL_Id, C_Name);
   begin
      C.Strings.Free (C_Name);
      Check_OpenGL_Error;
      return Result;
   end Uniform_Location;
   
   procedure Bind_Attrib_Location (Subject : Program; Index : UInt;
                                   Name : String) is
      C_Name : C.Strings.chars_ptr := C.Strings.New_String (Name);
   begin
      API.Bind_Attrib_Location (Subject.Reference.GL_Id, Index, C_Name);
      C.Strings.Free (C_Name);
      Check_OpenGL_Error;
   end Bind_Attrib_Location;
      
   function Attrib_Location (Subject : Program; Name : String) return UInt is
      C_Name   : C.Strings.chars_ptr := C.Strings.New_String (Name);
      Location : UInt := API.Get_Attrib_Location (Subject.Reference.GL_Id,
                                                  C_Name);
   begin
      C.Strings.Free (C_Name);
      Check_OpenGL_Error;
      return Location;
   end Attrib_Location;
      
end GL.Objects.Programs;