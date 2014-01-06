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
      declare
         Shader_Source : String (1 .. Integer (Source_Length));
         -- do not care that we do not assign a value.
         pragma Warnings (Off, Shader_Source);
         C_Shader_Source : C.Strings.chars_ptr
           := C.Strings.New_String (Shader_Source);
         Actual_Length : Size;
      begin
         API.Get_Shader_Source (Subject.Reference.GL_Id, Source_Length,
                                Actual_Length, C_Shader_Source);
         Shader_Source
           := C.Strings.Value (C_Shader_Source, C.size_t (Actual_Length));
         C.Strings.Free (C_Shader_Source);
         return Shader_Source;
      end;
   end Source;

   procedure Compile (Subject : Shader) is
   begin
      API.Compile_Shader (Subject.Reference.GL_Id);
   end Compile;

   procedure Release_Shader_Compiler renames API.Release_Shader_Compiler;
   
   function Compile_Status (Subject : Shader) return Boolean is
      Value : Int := 0;
   begin
      API.Get_Shader_Param (Subject.Reference.GL_Id, Enums.Compile_Status,
                            Value);
      return Value /= 0;
   end Compile_Status;

   function Info_Log (Subject : Shader) return String is
      Log_Length : Size := 0;
   begin
      API.Get_Shader_Param (Subject.Reference.GL_Id,
                            Enums.Info_Log_Length, Log_Length);
      -- Returned length includes null termination character
      Log_Length := Log_Length - 1;
      declare
         Info_Log : String (1 .. Integer (Log_Length));
         pragma Warnings (Off, Info_Log);
         C_Info_Log : C.Strings.chars_ptr
           := C.Strings.New_String (Info_Log);
         Actual_Length : Size;
      begin
         API.Get_Shader_Info_Log (Subject.Reference.GL_Id,Log_Length + 1,
                           Actual_Length, C_Info_Log);
         if (Int (Actual_Length) /= Log_Length) then
            raise Constraint_Error with "Expected info log length of" &
                                        Log_Length'Img & ", actually got" &
                                        Actual_Length'Img & ".";
         end if;
         Info_Log := C.Strings.Value (C_Info_Log, C.size_t (Actual_Length));
         C.Strings.Free (C_Info_Log);
         return Info_Log;
      end;
   end Info_Log;
   
   procedure Initialize_Id (Object : in out Shader) is
   begin
      Object.Reference.GL_Id := API.Create_Shader (Object.Kind);
      Object.Reference.Initialized := True;
   end Initialize_Id;

   procedure Delete_Id (Object : in out Shader) is
   begin
      API.Delete_Shader (Object.Reference.GL_Id);
      Object.Reference.GL_Id := 0;
      Object.Reference.Initialized := False;
   end Delete_Id;
   
   function Create_From_Id (Id : UInt) return Shader is
      Kind : Shader_Type := Shader_Type'First;
   begin
      API.Get_Shader_Type (Id, Enums.Shader_Type, Kind);
      Raise_Exception_On_OpenGL_Error;
      return Object : Shader (Kind) do
         Object.Reference.GL_Id := Id;
         Object.Reference.Initialized := True;
      end return;
   end Create_From_Id;
   
end GL.Objects.Shaders;
