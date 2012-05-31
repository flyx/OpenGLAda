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

with Interfaces.C.Strings;

with GL.API;
with GL.Enums;


package body GL.Objects.Shaders is
   procedure Set_Source (Subject : Shader; Source : String) is
      C_Source : Low_Level.CharPtr_Array
        := (1 => C.Strings.New_String (Source));
      Lengths : Low_Level.Int_Array
        := (1 => Source'Length);
   begin
      API.Shader_Source (Subject.Reference.GL_Id, 1, C_Source, Lengths);
      Check_OpenGL_Error;
   end Set_Source;
   
   function Source (Subject : Shader) return String is
      Source_Length : Int := 0;
   begin
      API.Get_Shader_Param (Subject.Reference.GL_Id,
                            Enums.Shader_Source_Length, Source_Length);
      declare
         Shader_Source : String (1 .. Integer (Source_Length))
           := (others => ' ');
         C_Shader_Source : C.Strings.chars_ptr
           := C.Strings.New_String (Shader_Source);
         Actual_Length : Low_Level.SizeI;
      begin
         API.Get_Shader_Source (Subject.Reference.GL_Id,
                                Low_Level.SizeI (Source_Length),
                                Actual_Length, C_Shader_Source);
         return C.Strings.Value (C_Shader_Source, C.size_t (Actual_Length));
      end;
   end Source;

   procedure Compile (Subject : Shader) is
   begin
      API.Compile_Shader (Subject.Reference.GL_Id);
   end Compile;

   function Compile_Status (Subject : Shader) return Boolean is
      Value : Int := 0;
   begin
      API.Get_Shader_Param (Subject.Reference.GL_Id, Enums.Compile_Status,
                            Value);
      return Value /= 0;
   end Compile_Status;

   function Info_Log (Subject : Shader) return String is
      Log_Length : Int := 0;
   begin
      API.Get_Shader_Param (Subject.Reference.GL_Id,
                            Enums.Info_Log_Length, Log_Length);
      declare
         Info_Log : String (1 .. Integer (Log_Length))
           := (others => ' ');
         C_Info_Log : C.Strings.chars_ptr
           := C.Strings.New_String (Info_Log);
         Actual_Length : Low_Level.SizeI;
      begin
         API.Get_Shader_Info_Log (Subject.Reference.GL_Id,
                           Low_Level.SizeI (Log_Length),
                           Actual_Length, C_Info_Log);
         return C.Strings.Value (C_Info_Log, C.size_t (Actual_Length));
      end;
   end Info_Log;
   
   procedure Create_Id (Object : in out Shader) is
   begin
      Object.Reference.GL_Id := API.Create_Shader (Object.Kind);
   end Create_Id;

   procedure Delete_Id (Object : in out Shader) is
   begin
      API.Delete_Shader (Object.Reference.GL_Id);
   end Delete_Id;
   
end GL.Objects.Shaders;