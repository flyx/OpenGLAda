--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.API;
with GL.Enums;

package body GL.Objects.Shaders is

   procedure Set_Source (Subject : Shader; Source : String) is
      C_Source : C.char_array := C.To_C (Source);
   begin
      API.Shader_Source (Subject.Reference.GL_Id, 1,
                         (1 => C_Source (0)'Unchecked_Access),
                         (1 => Source'Length));
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

   overriding
   procedure Internal_Create_Id (Object : Shader; Id : out UInt) is
   begin
      Id := API.Create_Shader (Object.Kind);
      Raise_Exception_On_OpenGL_Error;
   end Internal_Create_Id;

   overriding
   procedure Internal_Release_Id (Object : Shader; Id : UInt) is
      pragma Unreferenced (Object);
   begin
      API.Delete_Shader (Id);
      Raise_Exception_On_OpenGL_Error;
   end Internal_Release_Id;

   function Create_From_Id (Id : UInt) return Shader is
      Kind : Shader_Type := Shader_Type'First;
   begin
      API.Get_Shader_Type (Id, Enums.Shader_Type, Kind);
      Raise_Exception_On_OpenGL_Error;
      return Object : Shader (Kind) do
         Object.Set_Raw_Id (Id);
      end return;
   end Create_From_Id;

end GL.Objects.Shaders;
