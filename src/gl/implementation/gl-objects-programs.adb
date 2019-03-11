--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Ada.Unchecked_Conversion;
with Interfaces.C;

with GL.API;
with GL.Enums;

package body GL.Objects.Programs is
   procedure Attach (Subject : Program; Shader : Shaders.Shader) is
   begin
      API.Attach_Shader (Subject.Reference.GL_Id, Shader.Raw_Id);
      Raise_Exception_On_OpenGL_Error;
   end Attach;

   procedure Link (Subject : Program) is
   begin
      API.Link_Program (Subject.Reference.GL_Id);
      Raise_Exception_On_OpenGL_Error;
   end Link;

   function Program_Bool_Param (Subject : Program; Param : Enums.Program_Param)
                                return Boolean is
      Value : Int := 0;
   begin
      API.Get_Program_Param (Subject.Reference.GL_Id, Param, Value);
      Raise_Exception_On_OpenGL_Error;
      return Value /= 0;
   end Program_Bool_Param;
   pragma Inline (Program_Bool_Param);

   function Link_Status (Subject : Program) return Boolean is
   begin
      return Program_Bool_Param (Subject, Enums.Link_Status);
   end Link_Status;

   function Delete_Status (Subject : Program) return Boolean is
   begin
      return Program_Bool_Param (Subject, Enums.Delete_Status);
   end Delete_Status;

   function Validate_Status (Subject : Program) return Boolean is
   begin
      return Program_Bool_Param (Subject, Enums.Validate_Status);
   end Validate_Status;

   function Info_Log (Subject : Program) return String is
      Log_Length : Size := 0;
   begin
      API.Get_Program_Param (Subject.Reference.GL_Id, Enums.Info_Log_Length,
                             Log_Length);
      Raise_Exception_On_OpenGL_Error;
      if Log_Length = 0 then
         return "";
      else
         declare
            Info_Log : String (1 .. Integer (Log_Length));
         begin
            API.Get_Program_Info_Log (Subject.Reference.GL_Id, Log_Length,
                                      Log_Length, Info_Log);
            Raise_Exception_On_OpenGL_Error;
            return Info_Log (1 .. Integer (Log_Length));
         end;
      end if;
   end Info_Log;

   function Program_Size_Param (Subject : Program; Param : Enums.Program_Param)
                                return Size is
      Ret : Size := 0;
   begin
      API.Get_Program_Param (Subject.Reference.GL_Id, Param, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Program_Size_Param;
   pragma Inline (Program_Size_Param);

   function Active_Attributes (Subject : Program) return Size is
   begin
      return Program_Size_Param (Subject, Enums.Active_Attributes);
   end Active_Attributes;

   function Active_Attribute_Max_Length (Subject : Program) return Size is
   begin
      return Program_Size_Param (Subject, Enums.Active_Attribute_Max_Length);
   end Active_Attribute_Max_Length;

   function Active_Uniform_Blocks (Subject : Program) return Size is
   begin
      return Program_Size_Param (Subject, Enums.Active_Uniform_Blocks);
   end Active_Uniform_Blocks;

   function Active_Uniform_Block_Max_Name_Length (Subject : Program)
                                                  return Size is
   begin
      return Program_Size_Param
        (Subject, Enums.Active_Uniform_Block_Max_Name_Length);
   end Active_Uniform_Block_Max_Name_Length;

   function Active_Uniforms (Subject : Program) return Size is
   begin
      return Program_Size_Param (Subject, Enums.Active_Uniforms);
   end Active_Uniforms;

   function Active_Uniform_Max_Length (Subject : Program) return Size is
   begin
      return Program_Size_Param (Subject, Enums.Active_Uniform_Max_Length);
   end Active_Uniform_Max_Length;

   function Tess_Control_Output_Vertices (Subject : Program) return Size is
   begin
      return Program_Size_Param (Subject, Enums.Tess_Control_Output_Vertices);
   end Tess_Control_Output_Vertices;

   function Program_Int_Param (Subject : Program; Param : Enums.Program_Param)
                               return Int is
      Ret : Int := 0;
   begin
      API.Get_Program_Param (Subject.Reference.GL_Id, Param, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Program_Int_Param;
   pragma Inline (Program_Int_Param);

   function Tess_Gen_Mode (Subject : Program)
                           return Tessellation_Primitive_Mode is
      function To_Primitive_Mode is
        new Ada.Unchecked_Conversion (Int, Tessellation_Primitive_Mode);
   begin
      return To_Primitive_Mode (Program_Int_Param (Subject, Enums.Tess_Gen_Mode));
   end Tess_Gen_Mode;

   function Tess_Gen_Point_Mode (Subject : Program) return Boolean is
   begin
      return Program_Bool_Param (Subject, Enums.Tess_Gen_Point_Mode);
   end Tess_Gen_Point_Mode;

   function Tess_Gen_Spacing (Subject : Program) return Tessellation_Spacing is
      function To_Spacing is
        new Ada.Unchecked_Conversion (Int, Tessellation_Spacing);
   begin
      return To_Spacing (Program_Int_Param (Subject, Enums.Tess_Gen_Spacing));
   end Tess_Gen_Spacing;

   function Tess_Gen_Vertex_Order (Subject : Program) return Orientation is
      function To_Vertex_Order is
        new Ada.Unchecked_Conversion (Int, Orientation);
   begin
      return To_Vertex_Order
        (Program_Int_Param (Subject, Enums.Tess_Gen_Vertex_Order));
   end Tess_Gen_Vertex_Order;

   function Transform_Feedback_Buffer_Mode (Object : Program)
                                            return Buffer_Mode is
      Program_Buffer_Mode_Param : constant Buffer_Mode :=
                                    Buffer_Mode'Enum_Val
                                      (Program_Int_Param (Object, Enums.Transform_Feedback_Buffer_Mode));
   begin
      return Program_Buffer_Mode_Param;
   end Transform_Feedback_Buffer_Mode;

   function Transform_Feedback_Varyings_Size (Object : Program) return Size is
   begin
      return Program_Size_Param (Object, Enums.Transform_Feedback_Varyings);
   end Transform_Feedback_Varyings_Size;

   function Transform_Feedback_Varying_Max_Length (Object : Program)
                                                   return Size is
   begin
      return Program_Size_Param
        (Object, Enums.Transform_Feedback_Varying_Max_Length);
   end Transform_Feedback_Varying_Max_Length;

   procedure Begin_Transform_Feedback (Primitive_Mode : Connection_Mode) is
   begin
      API.Begin_Transform_Feedback (Primitive_Mode);
      Raise_Exception_On_OpenGL_Error;
   end Begin_Transform_Feedback;

   procedure End_Transform_Feedback is
   begin
      API.End_Transform_Feedback.all;
      Raise_Exception_On_OpenGL_Error;
   end End_Transform_Feedback;

   procedure Get_Transform_Feedback_Varying
     (Object :  Program; Index : Int; Length, V_Length : out Size;
      V_Type : out Buffer_Mode; Name : out String) is
      use Interfaces.C;
      Buffer_Size : constant Size := Name'Length + 1;
      C_Name      : Interfaces.C.char_array (1 .. size_t (Buffer_Size + 1));
   begin
      if Buffer_Size > 1 then
         V_Length := Transform_Feedback_Varyings_Size (Object);
         if  V_Length > 0 and then
           Transform_Feedback_Varying_Max_Length (Object) > 0 then
            API.Get_Transform_Feedback_Varying
              (Object.Reference.GL_Id, Index, Buffer_Size,
               Length, V_Length, V_Type, C_Name);
            Raise_Exception_On_OpenGL_Error;
            Name (Name'First .. Name'First + Integer (Length) - 1) :=
              Interfaces.C.To_Ada (C_Name);
         else
            raise Constraint_Error with
              "Get_Transform_Feedback_Varying, Max_Length or V_Length is 0";
         end if;
      else
         raise Constraint_Error with
           "Get_Transform_Feedback_Varying, String must have at least one character";
      end if;
   end Get_Transform_Feedback_Varying;

   procedure Transform_Feedback_Varyings
     (Object :  Program; Varyings : String; Mode : Buffer_Mode) is
      use type Interfaces.C.size_t;
      use type Interfaces.C.char;

      Parameter_Buffer : Interfaces.C.char_array :=
                           Interfaces.C.To_C (Varyings);
      Pointer_Array    : Low_Level.Char_Access_Array
        (1 .. Parameter_Buffer'Length / 2);
      -- cannot be longer than this if every name has a length of at least 1
      Pointer_Count    : Size := 0;
      Recent_Was_Comma : Boolean := True;
   begin
      for Pos in Parameter_Buffer'First .. Parameter_Buffer'Last - 1 loop
         if Parameter_Buffer (Pos) = ',' then
            if Recent_Was_Comma then
               raise Constraint_Error with
                 "Get_Transform_Feedback_Varying, every varying name must have at least one character";
            end if;
            Parameter_Buffer (Pos) := Interfaces.C.nul;
            Recent_Was_Comma := True;
         elsif Recent_Was_Comma then
            Pointer_Count := Pointer_Count + 1;
            Pointer_Array (Pointer_Count) :=
              Parameter_Buffer (Pos)'Unchecked_Access;
            Recent_Was_Comma := False;
         end if;
      end loop;
      API.Transform_Feedback_Varyings
        (Object.Reference.GL_Id, Pointer_Count, Pointer_Array, Mode);
      Raise_Exception_On_OpenGL_Error;
   end Transform_Feedback_Varyings;

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

   procedure Validate (Subject : Program) is
   begin
      API.Validate_Program (Subject.Reference.GL_Id);
      Raise_Exception_On_OpenGL_Error;
   end Validate;

   procedure Use_Program (Subject : Program) is
   begin
      API.Use_Program (Subject.Reference.GL_Id);
      Raise_Exception_On_OpenGL_Error;
   end Use_Program;

   overriding
   procedure Internal_Create_Id (Object : Program; Id : out UInt) is
      pragma Unreferenced (Object);
   begin
      Id := API.Create_Program.all;
      Raise_Exception_On_OpenGL_Error;
   end Internal_Create_Id;

   overriding
   procedure Internal_Release_Id (Object : Program; Id : UInt) is
      pragma Unreferenced (Object);
   begin
      API.Delete_Program (Id);
      Raise_Exception_On_OpenGL_Error;
   end Internal_Release_Id;

   function Uniform_Location (Subject : Program; Name : String)
                              return Uniforms.Uniform is
      Result : constant Uniforms.Uniform := API.Get_Uniform_Location
        (Subject.Reference.GL_Id, Interfaces.C.To_C (Name));
   begin
      Raise_Exception_On_OpenGL_Error;
      return Result;
   end Uniform_Location;

   procedure Bind_Attrib_Location (Subject : Program;
                                   Index   : Attributes.Attribute;
                                   Name    : String) is
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
      declare
         Raw_List : UInt_Array (1 .. Shader_Count);
      begin
         API.Get_Attached_Shaders (Object.Reference.GL_Id,
                                   Size (Shader_Count), Shader_Count, Raw_List);
         Raise_Exception_On_OpenGL_Error;
         return Shaders.Lists.Create (Raw_List (1 .. Shader_Count));
      end;
   end Attached_Shaders;

   procedure Bind_Frag_Data_Location
     (Object : Program; Color_Number : Buffers.Draw_Buffer_Index;
      Name   : String) is
   begin
      API.Bind_Frag_Data_Location
        (Object.Reference.GL_Id, Color_Number, C.To_C (Name));
      Raise_Exception_On_OpenGL_Error;
   end Bind_Frag_Data_Location;

   function Frag_Data_Location (Object : Program; Name : String)
                                return Buffers.Draw_Buffer_Index is
      Ret : constant Int := API.Get_Frag_Data_Location
        (Object.Reference.GL_Id, C.To_C (Name));
   begin
      Raise_Exception_On_OpenGL_Error;
      if Ret < 0 then
         raise Unknown_Variable_Name with Name;
      else
         return Buffers.Draw_Buffer_Index (Ret);
      end if;
   end Frag_Data_Location;

end GL.Objects.Programs;
