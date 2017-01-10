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

with Interfaces.C.Strings;

with GL.API;
with GL.Enums;

package body GL.Objects.Shaders is

   procedure Set_Source (Subject : Shader; Source : String) is
      C_Source : constant Low_Level.CharPtr_Array
        := (1 => C.Strings.New_String (Source));
      Lengths : constant Low_Level.Int_Array
        := (1 => Source'Length);
   begin
      API.Shader_Source (Subject.Reference.GL_Id, 1, C_Source, Lengths);
      Raise_Exception_On_OpenGL_Error;
   end Set_Source;

   function Source (Subject : Shader) return String is
      Source_Length : Size := 0;
   begin
      API.Get_Shader_Param (Subject.Reference.GL_Id,
                            Enums.Shader_Source_Length, Source_Length);
      Raise_Exception_On_OpenGL_Error;
      if Source_Length = 0 then
         return "";
      else
         declare
            Shader_Source : String (1 .. Integer (Source_Length));
         begin
            API.Get_Shader_Source (Subject.Reference.GL_Id, Source_Length,
                                 Source_Length, Shader_Source);
            Raise_Exception_On_OpenGL_Error;
            return Shader_Source (1 .. Integer (Source_Length));
         end;
      end if;
   end Source;

   procedure Compile (Subject : Shader) is
   begin
      API.Compile_Shader (Subject.Reference.GL_Id);
      Raise_Exception_On_OpenGL_Error;
   end Compile;

   procedure Release_Shader_Compiler is
   begin
      API.Release_Shader_Compiler.all;
   end Release_Shader_Compiler;

   function Compile_Status (Subject : Shader) return Boolean is
      Value : Int := 0;
   begin
      API.Get_Shader_Param (Subject.Reference.GL_Id, Enums.Compile_Status,
                            Value);
      Raise_Exception_On_OpenGL_Error;
      return Value /= 0;
   end Compile_Status;

   function Info_Log (Subject : Shader) return String is
      Log_Length : Size := 0;
   begin
      API.Get_Shader_Param (Subject.Reference.GL_Id,
                            Enums.Info_Log_Length, Log_Length);
      Raise_Exception_On_OpenGL_Error;
      if Log_Length = 0 then
        return "";
      else
         declare
            Info_Log : String (1 .. Integer (Log_Length));
         begin
            API.Get_Shader_Info_Log (Subject.Reference.GL_Id, Log_Length,
                                     Log_Length, Info_Log);
            Raise_Exception_On_OpenGL_Error;
            return Info_Log (1 .. Integer (Log_Length));
         end;
      end if;
   end Info_Log;

   procedure Destructor (Reference : not null GL_Object_Reference_Access) is
   begin
      API.Delete_Shader (Reference.GL_Id);
      Raise_Exception_On_OpenGL_Error;
      Reference.GL_Id := 0;
      Reference.Initialized := Uninitialized;
   end Destructor;

   procedure Initialize_Id (Object : in out Shader) is
   begin
      Object.Reference.GL_Id := API.Create_Shader (Object.Kind);
      Raise_Exception_On_OpenGL_Error;
      Object.Reference.Initialized := Allocated;
      Object.Reference.Destructor := Destructor'Access;
   end Initialize_Id;

   function Create_From_Id (Id : UInt) return Shader is
      Kind : Shader_Type := Shader_Type'First;
   begin
      API.Get_Shader_Type (Id, Enums.Shader_Type, Kind);
      Raise_Exception_On_OpenGL_Error;
      return Object : Shader (Kind) do
         Object.Reference.GL_Id := Id;
         Object.Reference.Initialized := External;
      end return;
   end Create_From_Id;

end GL.Objects.Shaders;
